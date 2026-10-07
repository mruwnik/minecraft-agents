(ns engine.flee-test
  "A flight is one whole attempt: retreat runs until the mobs stop chasing (follow range, out of line, no way), leaves
  :threat entries in body memory and tells the agent when one mob keeps chasing; respond-to-hostile never yields."
  (:require [cljs.test :refer [deftest is async]]
            [engine.ctx :as ctx]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.result :as r]
            [jobs.lib.cost :as cost]
            [jobs.lib.reach :as reach]
            [jobs.lib.threats :as threats]
            [jobs.survival.retreat :as retreat]))

(def ms-per-call
  "Game time one primitive call takes in these tests: a flight ends by time (out of line, its bound), so the clock moves
  with the body's acts."
  1000)

(def big-floor [-200 -12 20 12])

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor (merge {:floor big-floor} world))
        now (tu/act-clock clock p ms-per-call)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn recording-parent
  "A parent that calls job once per round with args; every status it answers goes to calls, its result to out."
  [calls out job args]
  {:check (constantly true)
   :round (fn ^:async recording-round [c]
            (let [r (await (ctx/call-child c :kid job args))]
              (swap! calls conj r)
              (when (= :done r) (reset! out (ctx/child-result c :kid)))
              r))})

(defn ^:async run-job!
  "Run job with args under a recording parent until the list is empty (at most 5 ticks).
  {:eng :p :seen :calls :out}."
  [s job args]
  (let [calls (atom [])
        out (atom nil)
        eng (assoc (:eng s) :jobs (assoc (:jobs (:eng s)) 'recording-parent (recording-parent calls out job args)))]
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i 5) (seq (:list (core/state eng))))
        (await (core/tick! eng))
        (recur (inc i))))
    (assoc s :eng eng :calls @calls :out @out)))

(defn zombie [id x opts] (merge {:id id :uuid (str "u" id) :name "zombie" :kind "hostile" :pos {:x x :y 64 :z 0}} opts))

(defn body-pos [p] (fake/vec-pos (:pos (fake/self p))))

(defn mob-pos [p id] (:pos (first (filter #(= id (:id %)) (fake/entities p)))))

(defn threat-entries [eng] (mapv :data (mem/entries (mem/view (:store eng)) :threat)))

(defn chased-events [seen] (filterv #(= :hostile.chased (:kind %)) @seen))

;; ------------------------------------------------------------------ the table and the rule

(deftest follow-ranges-are-vanilla
  (is (= [35 16 16 64] (mapv threats/follow-range ["zombie" "skeleton" "something_new" "enderman"]))))

(deftest a-mob-stops-chasing-past-its-range-out-of-line-or-with-no-way
  (let [now 10000 lost-ms 4000
        chasing {:mob "zombie" :distance 10 :in-line? true :way? true}]
    (is (nil? (retreat/stopped-chasing chasing 0 now lost-ms)) "in range, in line, a way: chasing")
    (is (= :gone (retreat/stopped-chasing nil 0 now lost-ms)))
    (is (= :far (retreat/stopped-chasing (assoc chasing :distance 36) now now lost-ms)))
    (is (= :lost (retreat/stopped-chasing (assoc chasing :in-line? false) 5000 now lost-ms)))
    (is (nil? (retreat/stopped-chasing (assoc chasing :in-line? false) 7000 now lost-ms)) "out of line 3 s: still")
    (is (= :closed (retreat/stopped-chasing (assoc chasing :way? false) now now lost-ms)))
    (is (= :far (retreat/stopped-chasing {:mob "skeleton" :distance 17 :in-line? true} now now lost-ms)))))

(deftest decide-moved-to-combat-unchanged
  (is (= :fight (cost/decide {:health 20 :damage 10 :creeper? false :reserve 4})))
  (is (= :flee (cost/decide {:health 20 :damage 17 :creeper? false :reserve 4})))
  (is (= :flee (cost/decide {:health 20 :damage 0 :creeper? true :reserve 4}))))

;; ------------------------------------------------------------------ whole flights

(deftest retreat-flees-until-the-zombie-gives-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng calls out]} (await (run-job! (setup {:entities [(zombie 7 5 {:chase {:speed 0.7}})]})
                                                         'jobs.survival.retreat {}))]
          (is (= [:done] calls) "one call: the whole flight in one round")
          (is (= :done (:status out)))
          (is (= :far (:ended out)))
          (is (> (fake/dist (body-pos p) (mob-pos p 7)) 35) "beyond the zombie's follow range")
          (is (= [{:mob "zombie" :id 7 :uuid "u7" :ended :far}]
                 (mapv #(select-keys % [:mob :id :uuid :ended]) (threat-entries eng))) "one :threat entry")
          (is (= {:cap 20 :ttl 300000} (mem/policy (mem/view (:store eng)) :threat))))))))

(deftest flight-steps-walk-through-a-go-to-child-that-never-escalates
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(zombie 7 5 {:chase {:speed 0.7}})]})
              real (get registry/jobs 'jobs.movement.go-to)
              args (atom [])
              wrapped (assoc real :round (fn ^:async spy [c] (swap! args conj (:args c)) (await ((:round real) c))))
              s (assoc-in s [:eng :jobs 'jobs.movement.go-to] wrapped)
              {:keys [p out]} (await (run-job! s 'jobs.survival.retreat {}))]
          (is (= :far (:ended out)))
          (is (pos? (count @args)) "every flight step is a go-to call")
          (is (every? #(false? (:escalate %)) @args) "a flee never digs or pillars")
          (is (> (fake/dist (body-pos p) (mob-pos p 7)) 35)))))))

(deftest retreat-eats-one-bite-per-flee-step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (run-job! (setup {:self {:food 2} :inventory [{:name "bread" :count 6}]
                                                   :entities [(zombie 7 5 {:chase {:speed 0.7}})]})
                                           'jobs.survival.retreat {}))
              names (mapv #(.-name %) (.-calls (.-world p)))
              eats (keep-indexed (fn [i n] (when (= "eat" n) i)) names)]
          (is (>= (count eats) 2) "it eats while fleeing")
          (is (every? (fn [[a b]] (some (complement #{"eat" "equip"}) (subvec names (inc a) b))) (partition 2 1 eats))
              "a walk between every two bites"))))))

(deftest retreat-ends-when-the-zombie-is-out-of-line
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p calls out]} (await (run-job! (setup {:entities [(zombie 7 5 {:visible false :chase {:speed 0.7}})]})
                                                     'jobs.survival.retreat {}))]
          (is (= [:done] calls))
          (is (= :lost (:ended out)))
          (is (< (fake/dist (body-pos p) (mob-pos p 7)) 35) "lost before the follow range"))))))

(deftest a-flight-has-no-time-limit
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(zombie 7 5 {:chase {:speed 1}})]})
              {:keys [p clock]} s
              world (.-world p)
              calls #(.-length (.-calls world))]
          (doseq [name ["steer" "moveTo" "wait" "attack" "place" "dig" "equip" "eat"]]
            (.override world name (fn ^:async f [token a impl]
                                    (when (= 5 (calls)) (swap! clock + 200000))
                                    (when (> (calls) 400) (swap! (fake/state p) assoc :entities []))
                                    (await (impl token a)))))
          (let [{:keys [out]} (await (run-job! s 'jobs.survival.retreat {:no-gain-steps 3}))]
            (is (> (calls) 400) "the chase went on past the old 180 s bound, until the zombie left")
            (is (not= :still-chased (:reason out)))))))))

(deftest a-chase-that-gains-no-distance-escalates-and-a-gain-resets-it
  (let [step (fn [mem gap] (retreat/note-gap mem gap))]
    (is (= {:best-gap 5 :since-gain 0 :sweeps 0} (step {:sweeps 1} 5)) "first gap is a gain")
    (is (= {:best-gap 5 :since-gain 2 :sweeps 1} (-> {:best-gap 5 :since-gain 1 :sweeps 1} (step 5.5))) "under a block: no gain")
    (is (= {:best-gap 7 :since-gain 0 :sweeps 0} (step {:best-gap 5 :since-gain 2 :sweeps 1} 7)) "a gain resets both")
    (is (retreat/no-gain? {:since-gain 3} 3))
    (is (not (retreat/no-gain? {:since-gain 2} 3)))))

(deftest a-step-with-no-hostile-near-is-no-gap-sample
  (is (= {:best-gap 9 :since-gain 0 :sweeps 1} (retreat/note-gap {:best-gap 9 :since-gain 4 :sweeps 1} nil))
      "the flight is succeeding: no-gain count resets, best gap and sweeps stay")
  (is (= {:since-gain 0} (retreat/note-gap {} nil)) "no best gap is invented from nothing")
  (is (= {:best-gap 6 :since-gain 0 :sweeps 0} (retreat/note-gap (retreat/note-gap {} nil) 6))))

(deftest the-step-gap-is-the-nearest-hostile-however-they-are-ordered
  (let [p (tu/fake-on-floor {:floor big-floor :entities [(zombie 1 20 {}) (zombie 2 6 {})]})
        hostiles (vec (reach/known-hostiles p 30 {:ranged-radius 30}))]
    (is (= 2 (count hostiles)))
    (is (= 6 (retreat/nearest-gap p hostiles)))
    (is (= 6 (retreat/nearest-gap p (vec (reverse hostiles)))))
    (is (nil? (retreat/nearest-gap p [])))))

(def sweeper
  "A job that runs retreat's blocked! until it ends the job."
  {:check (constantly true)
   :round (fn ^:async sweeper-round [c]
            (loop [n 1]
              (let [r (await (retreat/blocked! c "test"))]
                (if (= :again r) (recur (inc n)) r))))})

(deftest blocked-twice-in-a-row-stops-cannot-escape
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (assoc-in (setup {}) [:eng :jobs 'sweeper] sweeper)]
          (core/submit! eng '(sweeper) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "two failed sweeps end the job")
          (is (= 1 (count (filter #(= :retreat_blocked (:kind %)) @seen))) "warned once"))))))

(deftest every-option-failing-in-two-sweeps-in-a-row-is-cannot-escape
  (is (not (retreat/cannot-escape? (retreat/count-sweep {}))))
  (is (retreat/cannot-escape? (retreat/count-sweep (retreat/count-sweep {})))))

(deftest respond-to-hostile-never-continues
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [calls eng]} (await (run-job! (setup {:entities [(zombie 7 5 {:chase {:speed 0.7}})]})
                                                   'jobs.survival.respond-to-hostile {}))]
          (is (= [:done] calls) "one call: flight and all")
          (is (= [] (:list (core/state eng)))))))))

(defn ^:async flight!
  "Put a zombie (id 7, uuid) 5 blocks from the body out of line and run one retreat to its end."
  [s uuid]
  (let [[x _ _] (body-pos (:p s))]
    (swap! (fake/state (:p s)) assoc :entities [])
    (fake/add-entity! (:p s) (zombie 7 (+ x 5) {:uuid uuid :visible false}))
    (await (run-job! s 'jobs.survival.retreat {}))))

(deftest one-mob-chasing-three-times-tells-the-agent-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen clock] :as s} (setup {})]
          (await (flight! s "a"))
          (await (flight! s "a"))
          (is (= 0 (count (chased-events seen))) "two flights: nothing yet")
          (await (flight! s "b"))
          (is (= 0 (count (chased-events seen))) "id 7 reused by another mob (uuid b) does not count")
          (await (flight! s "a"))
          (is (= [{:mob "zombie" :id 7 :flights 3}] (mapv #(select-keys % [:mob :id :flights]) (chased-events seen)))
              "the third flight from one mob: one warning")
          (swap! clock + (* 6 60 1000))
          (await (flight! s "a"))
          (await (flight! s "a"))
          (is (= 1 (count (chased-events seen))) "flights past the ttl do not count"))))))

(deftest a-heard-only-chaser-leaves-a-direction-and-band-never-an-exact-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen] :as s} (setup {})]
          (await (flight! s "a"))
          (await (flight! s "a"))
          (await (flight! s "a"))
          (let [entry (first (threat-entries (:eng s)))
                chased (first (chased-events seen))]
            (is (not (contains? entry :pos)) "heard only: no exact :pos in the :threat memory")
            (is (every? entry [:direction :band :from]))
            (is (not (contains? chased :pos)) "nor in the :hostile.chased event")
            (is (every? chased [:direction :band]))))))))

(deftest a-seen-chaser-leaves-its-place
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng] :as s} (setup {:entities [(zombie 7 5 {:visible true :chase {:speed 0.7}})]})
              out (await (run-job! s 'jobs.survival.retreat {}))]
          (is (number? (:x (:pos (first (threat-entries (:eng out)))))) "seen: the :pos is kept"))))))

(deftest rough-pos-is-the-band-away-toward-the-direction
  (let [from {:x 10 :y 64 :z 10}]
    (is (= {:x 14 :y 64 :z 10} (update (threats/rough-pos {:direction :east :band :near :from from}) :x js/Math.round)))
    (is (= {:x 10 :y 64 :z -6} (update (threats/rough-pos {:direction :north :band :far :from from}) :x js/Math.round)))
    (is (= {:x 1 :y 2 :z 3} (threats/rough-pos {:pos {:x 1 :y 2 :z 3}})))
    (is (nil? (threats/rough-pos {:mob "zombie"})))))

;; ------------------------------------------------------------------ hiding holds in the round

(defn key-of [x y z] (str x "," y "," z))

(def dead-end
  "Stone round a 1-wide tunnel along x on z 0, feet and head height, x 0..8, closed behind the body at x -1."
  (let [open (set (for [x (range 0 9) y [64 65]] [x y 0]))]
    (into {} (for [x (range -3 10) y (range 62 68) z [-1 0 1] :when (not (open [x y z]))]
               [(key-of x y z) "stone"]))))

(defn holds [seen reason] (filterv #(and (= :holding (:kind %)) (= reason (some-> (:reason %) name))) @seen))

(deftest a-sealed-in-retreat-holds-in-the-round-until-the-zombie-leaves
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup {:floor [-3 -1 9 1] :blocks dead-end
                                             :inventory [{:name "cobblestone" :count 20}]
                                             :entities [(zombie 7 4 {})]})
              _ (js/setTimeout #(swap! (fake/state p) assoc :entities []) 400)
              {:keys [calls out eng]} (await (run-job! s 'jobs.survival.retreat {}))]
          (is (= [:done] calls) "sealed, hid and ended in one round")
          (is (= 1 (count (holds seen "hiding"))) "a declared hold while hidden")
          (is (= :hidden (:ended out)))
          (is (= [] (:list (core/state eng)))))))))

;; ------------------------------------------------------------------ what the flight senses, resume, the round's bound

(deftest the-flight-judges-chasers-from-what-the-body-knows
  (let [p (tu/fake-on-floor {:floor big-floor :entities [(zombie 7 30 {})]})
        per (perception/create (fake-raw/create p) {:now (constantly 1000000)})
        wrapped (perception/wrap p per)]
    (is (= [7] (mapv #(.-id %) (retreat/known-chasers p))) "no perception: every tracked mob")
    (is (= [] (mapv #(.-id %) (retreat/known-chasers wrapped))) "30 blocks off, unseen and unheard: not known")))

(deftest the-cornered-body-weighs-only-the-hostiles-it-knows
  (let [p (tu/fake-on-floor {:floor big-floor :entities [(zombie 7 30 {})]})
        per (perception/create (fake-raw/create p) {:now (constantly 1000000)})
        wrapped (perception/wrap p per)
        ids #(mapv (fn [e] (.-id e)) %)]
    (is (= [7] (ids (retreat/near-known p #{} 40 nil))) "no perception: every tracked mob")
    (is (= [] (ids (retreat/near-known wrapped #{} 40 nil))) "unseen and unheard: not weighed")
    (is (= [] (vec (retreat/hostile-cells wrapped 40))) "no cell is kept clear for an unknown mob")
    (is (= [] (ids (retreat/near-known p #{7} 40 nil))) "a corpse is skipped")))

(deftest a-flight-resumed-after-a-gap-starts-its-clocks-afresh
  (let [mem {:flight-start 0 :last-step 1000 :chasers {7 {:id 7 :seen-t 1000}}}]
    (is (= mem (retreat/resume-flight mem 3000)) "a short gap: as it was")
    (is (= {:flight-start 100000 :last-step 100000 :chasers {7 {:id 7 :seen-t 100000}}}
           (retreat/resume-flight mem 100000))
        "a long gap: the flight starts now and every chaser was just seen")))

(deftest respond-to-hostile-stops-after-three-calls-that-change-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(zombie 7 5 {:chase false})]})
              clock (:clock s)
              declining {:check (constantly true)
                         :round (fn ^:async idle-flight [c] (swap! clock + 1000) :done)}
              s (assoc-in s [:eng :jobs 'jobs.survival.retreat] declining)
              {:keys [calls out]} (await (run-job! s 'jobs.survival.respond-to-hostile {}))]
          (is (= [:done] calls))
          (is (= {:status :stopped :reason :no_response} (select-keys out [:status :reason]))))))))

(deftest respond-to-hostile-is-done-when-the-danger-is-gone-after-a-long-hide
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(zombie 7 5 {:chase false})]})
              clock (:clock s)
              p (:p s)
              hiding {:check (constantly true)
                      :round (fn ^:async long-hide [c]
                               (swap! clock + 400000)
                               (swap! (fake/state p) assoc :entities [])
                               (r/finish! c {:fled [7] :ended :hidden}))}
              s (assoc-in s [:eng :jobs 'jobs.survival.retreat] hiding)
              {:keys [calls out]} (await (run-job! s 'jobs.survival.respond-to-hostile {}))]
          (is (= [:done] calls))
          (is (not= :stopped (:status out))))))))

(deftest a-heard-mob-keeps-cells-clear-at-its-rough-spot-not-its-exact-one
  (let [p0 (tu/fake-on-floor {:floor big-floor})
        [bx] (body-pos p0)
        p (tu/fake-on-floor {:floor big-floor :entities [(zombie 7 (+ bx 10) {:visible false})]})
        cells (retreat/hostile-cells p 40)]
    (is (contains? (set (map :x cells)) (js/Math.floor (+ bx 16))) "heard 10 blocks east (far band): 16 east")
    (is (not-any? #(= (js/Math.floor (+ bx 10)) (:x %)) cells) "the exact place is not kept")))
