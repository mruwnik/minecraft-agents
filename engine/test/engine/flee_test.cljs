(ns engine.flee-test
  "A flight is one whole attempt: retreat runs until the mobs stop chasing (follow range, out of line, no way), leaves
  :threat entries in body memory and tells the agent when one mob keeps chasing; respond-to-hostile never yields."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.ctx :as ctx]
            [engine.registry :as registry]
            [engine.flight-job :as fj]
            [engine.settings :as settings]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.movement.go-to.result :as end]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.survival.respond-flight :as retreat]
            [jobs.survival.respond-to-hostile :as respond]
            [jobs.survival.respond.refuge :as refuge]
            [jobs.survival.hide-out :as hide-out]
            [jobs.survival.respond.options :as cornered]
            [jobs.survival.respond.cover :as cover]
            [jobs.survival.respond.state :as respond.state]
            [jobs.lib.seal :as seal]
            [jobs.survival.respond.goal :as respond.goal]
            [jobs.lib.flee-walk :as flee-walk]))

(def ms-per-call
  "Game time one primitive call takes in these tests: a flight ends by time (out of line, its bound), so the clock moves
  with the body's acts."
  1000)

(def big-floor [-200 -12 20 12])

(defn setup
  "An engine over a fake on big-floor; world's :ms-per-call (default ms-per-call) is the game time one call takes."
  [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor (merge {:floor big-floor} (dissoc world :ms-per-call))))
        now (tu/act-clock clock p (:ms-per-call world ms-per-call))
        eng (core/create {:primitives p :jobs fj/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn recording-parent
  "A parent that calls job once per round with args; each round's :continue or :done (ended) goes to calls, its outcome to out."
  [calls out job args]
  (let [rec (tu/recording-parent job args out)]
    (tu/job (fn ^:async recording-round [c]
              (let [r (await ((:round rec) c))]
                (swap! calls conj (if (= :continue r) :continue :done))
                r)))))

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

(deftest a-mob-stops-chasing-past-its-range-out-of-line-or-with-no-way
  (let [now 10000 lost-ms 4000
        chasing {:mob "zombie" :distance 10 :in-line? true :way? true}]
    (is (nil? (respond.state/stopped-chasing chasing 0 now lost-ms)) "in range, in line, a way: chasing")
    (is (= :gone (respond.state/stopped-chasing nil 0 now lost-ms)))
    (is (= :far (respond.state/stopped-chasing (assoc chasing :distance 36) now now lost-ms)))
    (is (= :lost (respond.state/stopped-chasing (assoc chasing :in-line? false) 5000 now lost-ms)))
    (is (nil? (respond.state/stopped-chasing (assoc chasing :in-line? false) 7000 now lost-ms)) "out of line 3 s: still")
    (is (= :closed (respond.state/stopped-chasing (assoc chasing :way? false) now now lost-ms)))
    (is (= :far (respond.state/stopped-chasing {:mob "skeleton" :distance 17 :in-line? true} now now lost-ms)))
    (is (nil? (respond.state/stopped-chasing (assoc chasing :still-ms 4000) now now lost-ms)) "still 4 s: may be waiting")
    (is (= :still (respond.state/stopped-chasing (assoc chasing :still-ms 6000) now now lost-ms)) "a melee mob that stands still is not chasing")
    (is (nil? (respond.state/stopped-chasing (assoc chasing :mob "skeleton" :ranged? true :distance 10 :still-ms 60000) now now lost-ms)) "a shooter stands still and shoots")))

(deftest a-low-body-keeps-fleeing-a-ranged-mob-that-lost-its-line-while-near
  (let [now 10000 lost-ms 4000
        witch {:mob "witch" :distance 6 :in-line? false :way? true :ranged? true}]
    (is (= :lost (respond.state/stopped-chasing witch 0 now lost-ms)) "a healthy body lets it go")
    (is (nil? (respond.state/stopped-chasing (assoc witch :low? true) 0 now lost-ms)) "a low one may not rest 6 blocks from it")
    (is (= :far (respond.state/stopped-chasing (assoc witch :low? true :distance 17) 0 now lost-ms)))
    (is (= :lost (respond.state/stopped-chasing {:mob "zombie" :distance 6 :in-line? false :way? true :low? true} 0 now lost-ms))
        "a melee mob out of line is still let go")))

;; ------------------------------------------------------------------ whole flights

(deftest a-body-shot-by-an-unseen-shooter-walks-away-from-the-side-the-arrow-came-from
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {})
              _ (mem/write! (:store (:eng s)) :hurt {:amount 3 :health 17 :damageType "arrow" :dir {:x 1 :z 0}} {:cap 50 :ttl 60000})
              {:keys [p out]} (await (run-job! s 'test.flight {}))]
          (is (= :done (:status out)))
          (is (< (first (body-pos p)) -2) "no mob is known, yet the body went west, away from the shooter in the east"))))))

(deftest respond-to-hostile-flees-a-shot-from-an-unseen-shooter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {})
              _ (mem/write! (:store (:eng s)) :hurt {:amount 3 :health 17 :damageType "arrow" :dir {:x 1 :z 0}} {:cap 50 :ttl 60000})
              {:keys [p]} (await (run-job! s 'jobs.survival.respond-to-hostile {}))]
          (is (< (first (body-pos p)) -2) "with no mob known the reflex still leaves the line of fire"))))))

(deftest retreat-flees-until-the-zombie-gives-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng calls out]} (await (run-job! (setup {:entities [(zombie 7 5 {:chase {:speed 0.7}})]})
                                                         'test.flight {}))]
          (is (= [:done] calls) "one call: the whole flight in one round")
          (is (= :done (:status out)))
          (is (= :far (:ended out)))
          (is (> (fake/dist (body-pos p) (mob-pos p 7)) 35) "beyond the zombie's follow range")
          (is (= [{:mob "zombie" :id 7 :uuid "u7" :ended :far}]
                 (mapv #(select-keys % [:mob :id :uuid :ended]) (threat-entries eng))) "one :threat entry")
          (is (= {:cap 20 :ttl 300000} (mem/policy (mem/view (:store eng)) :threat))))))))

(deftest a-flee-end-over-an-unseen-floor-is-avoided
  (let [p (tu/fake-on-floor {:blocks {"3,63,0" "sand"}})
        _ (tu/set-sensed! p (fn [pos] #js {:unknown true :pos pos}))
        seen (tu/seeing-all (tu/fake-on-floor {:blocks {"3,63,0" "sand"}}))
        end {:x 3 :y 64 :z 0}]
    (is (respond.state/unseen-floor-end? p end) "nothing seen: the planner would not stand there")
    (is (not (respond.state/unseen-floor-end? seen end)))))

(deftest flight-steps-walk-through-a-go-to-child-that-never-escalates
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(zombie 7 5 {:chase {:speed 0.7}})]})
              real (get registry/jobs 'jobs.movement.go-to)
              args (atom [])
              wrapped (assoc real :round (fn ^:async spy [c] (swap! args conj (:args c)) (await ((:round real) c))))
              s (assoc-in s [:eng :jobs 'jobs.movement.go-to] wrapped)
              {:keys [p out]} (await (run-job! s 'test.flight {}))]
          (is (= :far (:ended out)))
          (is (pos? (count @args)) "every flight step is a go-to call")
          (is (every? #(not-any? #{:pillar :stair :clear-path :dig-out} (:ways %)) @args) "a flee never digs or pillars")
          (is (every? #(= 16 (get-in % [:settings :jobs.lib.walk.world/y-margin])) @args) "a flee plans no route far below or above itself and its goal")
          (is (every? #(and (= 1 (get-in % [:spend :tries])) (false? (get-in % [:settings :jobs.movement.go-to.tuning/look-round]))) @args)
              "a step that gains nothing ends at once and never looks round")
          (is (> (fake/dist (body-pos p) (mob-pos p 7)) 35)))))))

(def pen-blocks
  "A 5x5 pen (x, z in -2..2) walled by a two-high fence ring on stone with an open gate at 0,64,3 in its south side."
  (into {} (for [x (range -3 4) z (range -3 4) y [63 64 65]
                 :when (and (or (= 3 (js/Math.abs x)) (= 3 (js/Math.abs z))) (not= [0 3 65] [x z y]))]
             [(str x "," y "," z) (cond (= 63 y) "stone" (and (= [0 3] [x z]) (= 64 y)) "oak_fence_gate" :else "oak_fence")])))

(deftest a-gate-the-flight-shut-on-a-zombie-stays-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:blocks pen-blocks :states {"0,64,3" {:open true}} :self {:pos {:x 0.5 :y 64 :z 1.5}}
                        :entities [(zombie 7 0 {:pos {:x 0.5 :y 64 :z 6.5} :chase {:speed 0.7}})]})
              {:keys [p seen]} (await (run-job! s 'test.flight {}))]
          (is (= 1 (count (filterv #(= :retreat.door-shut (:kind %)) @seen))) "the flight shut the gate")
          (is (false? (get-in @(fake/state p) [:states [0 64 3] :open])) "and nothing opened it again"))))))

(deftest a-flee-step-says-where-it-goes-and-from-what
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (run-job! (setup {:entities [(zombie 7 5 {:chase {:speed 0.7}})]}) 'test.flight {}))
              e (first (filterv #(= :retreat.target (:kind %)) @seen))]
          (is (some? e) "every flee step logs its target")
          (is (vector? (:target e)))
          (is (= ["zombie 7"] (:from e)) "the chasers the step runs from"))))))

(deftest a-flight-keeps-clear-of-a-creeper-it-lost-seconds-ago
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(zombie 7 -5 {:chase {:speed 0.7}})
                                   {:id 9 :uuid "u9" :name "creeper" :kind "hostile" :visible false :pos {:x 8 :y 64 :z 0}}]})
              _ (mem/write! (:store (:eng s)) :threat {:mob "creeper" :id 9 :uuid "u9" :key "u9" :pos {:x 8 :y 64 :z 0} :ended :lost}
                            threats/threat-policy)
              _ (swap! (:clock s) + 10000)
              {:keys [seen]} (await (run-job! s 'test.flight {}))
              target (:target (first (filterv #(= :retreat.target (:kind %)) @seen)))]
          (is (some? target))
          (is (> (js/Math.abs (- (nth target 2) 0)) 3) "sideways: not east towards the creeper")
          (is (< (first target) 4) "and not towards it"))))))

(deftest a-flee-step-does-not-end-nearer-a-hostile-that-is-not-chasing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (run-job! (setup {:entities [(zombie 7 -8 {:chase {:speed 0.7}}) (zombie 8 18 {})]}) 'test.flight {}))
              target (:target (first (filterv #(= :retreat.target (:kind %)) @seen)))]
          (is (some? target))
          (is (< (first target) 3) "straight away from the chaser is east, towards the other zombie: the step turns aside"))))))

(deftest a-flight-ignores-the-spot-of-a-creeper-the-client-no-longer-lists
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(zombie 7 -5 {:chase {:speed 0.7}})]})
              _ (mem/write! (:store (:eng s)) :threat {:mob "creeper" :id 9 :uuid "u9" :key "u9" :pos {:x 8 :y 64 :z 0} :ended :lost}
                            threats/threat-policy)
              _ (swap! (:clock s) + 10000)
              {:keys [seen]} (await (run-job! s 'test.flight {}))
              target (:target (first (filterv #(= :retreat.target (:kind %)) @seen)))]
          (is (some? target))
          (is (>= (first target) 4) "straight away from the zombie: the blown-up creeper's spot does not lean the way"))))))

(deftest retreat-eats-one-bite-per-flee-step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (run-job! (setup {:self {:food 2} :inventory [{:name "bread" :count 6}]
                                                   :entities [(zombie 7 5 {:chase {:speed 0.7}})]})
                                           'test.flight {}))
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
                                                     'test.flight {}))]
          (is (= [:done] calls))
          (is (= :lost (:ended out)))
          (is (< (fake/dist (body-pos p) (mob-pos p 7)) 35) "lost before the follow range"))))))

(deftest the-flight-time-spend-outlasts-the-longest-hide
  (is (> (settings/get respond/settings ::respond/job-time-s) (hide-out/hide-max-s))))

(deftest a-flight-runs-past-the-old-180-s-bound-until-the-zombie-leaves
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ms 5000
              s (setup {:entities [(zombie 7 5 {:chase {:speed 1}})] :ms-per-call ms})
              {:keys [p]} s
              world (.-world p)
              elapsed-s #(/ (* ms (.-length (.-calls world))) 1000)]
          (doseq [name ["steer" "moveTo" "wait" "attack" "place" "dig" "equip" "eat"]]
            (.override world name (fn ^:async f [token a impl]
                                    (when (> (elapsed-s) 180) (swap! (fake/state p) assoc :entities []))
                                    (await (impl token a)))))
          (let [{:keys [out]} (await (run-job! s 'test.flight {:settings {:jobs.survival.respond.state/no-gain-steps 3}}))]
            (is (> (elapsed-s) 180) "the chase went on past the old 180 s bound, until the zombie left")
            (is (not= :still-chased (:reason out)))))))))

(deftest a-flight-that-keeps-taking-hits-from-a-melee-mob-turns-and-fights-an-armed-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:inventory [{:name "stone_sword" :count 1}] :entities [(zombie 7 2 {:chase {:speed 1}})]})
              {:keys [p]} s
              world (.-world p)]
          (.override world "steer" (fn ^:async f [token a impl]
                                     (swap! (fake/state p) update-in [:self :health] - 2)
                                     (when (> (.-length (.-calls world)) 60) (swap! (fake/state p) assoc :entities []))
                                     (await (impl token a))))
          (await (run-job! s 'test.flight {}))
          (is (pos? (count (filter #(= "attack" (.-name %)) (.-calls world)))) "outrun by a zombie that hits it, it hits back"))))))

(deftest a-flee-leg-whose-search-runs-out-with-a-hostile-adjacent-fights-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:inventory [{:name "stone_sword" :count 1}] :entities [(zombie 7 1 {})]})
              {:keys [p clock]} s
              world (.-world p)
              t0 @clock
              searches (atom 0)
              searches-at-attack (atom nil)
              first-attack (atom nil)]
          (.override world "attack" (fn ^:async f [token a impl]
                                      (swap! searches-at-attack #(or % @searches))
                                      (swap! first-attack #(or % (+ @clock (* ms-per-call (.-length (.-calls world))))))
                                      (await (impl token a))))
          (with-redefs [near/walk-round! (fn [c _ _ _]
                                           (swap! searches inc)
                                           (swap! clock + 1000)
                                           (swap! (fake/state p) update-in [:self :health] - 2)
                                           (js/Promise.resolve {:result {:status :searching :fresh false} :status "searching"
                                                                :from (:pos c) :to (:pos c)}))]
            (await (run-job! s 'test.flight {})))
          (is (some? @first-attack) "the body hit back")
          (is (<= @searches-at-attack 1) "a hostile in reach bounds the leg to one search round, so the body does not stand taking hits")
          (is (<= (- @first-attack t0) (* 1000 (+ 4 (cornered/close-leg-s)))) "after one short leg (and the calls to equip and look), not after standing searching"))))))

(deftest a-flee-leg-is-short-when-the-body-was-just-hurt-and-no-hostile-is-in-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(zombie 7 5 {})]})
              {:keys [p]} s
              searches (atom 0)]
          (mem/write! (:store (:eng s)) :hurt {:amount 2 :health 10 :attacker {:name "spider"}} {:cap 50 :ttl 60000})
          (with-redefs [near/walk-round! (fn [c _ _ _]
                                           (swap! searches inc)
                                           (swap! (fake/state p) assoc :entities [])
                                           (swap! (:clock s) + 1000)
                                           (js/Promise.resolve {:result {:status :searching :fresh false} :status "searching"
                                                                :from (:pos c) :to (:pos c)}))]
            (await (run-job! s 'test.flight {})))
          (is (<= @searches 2) "a body hurt a moment ago, with nothing seen in reach, does not stand searching a whole leg"))))))

(deftest a-body-hit-a-moment-ago-with-no-hostile-in-sight-looks-again-before-the-response-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(zombie 7 5 {})]})
              paces (atom 0)]
          (mem/write! (:store (:eng s)) :hurt {:amount 5 :health 10 :attacker {:name "zombie"}} {:cap 50 :ttl 60000})
          (with-redefs [pace/pace! (fn [] (swap! paces inc) (js/Promise.resolve nil))
                        respond/respond (fn [_ _] (swap! (fake/state (:p s)) assoc :entities []) (js/Promise.resolve nil))]
            (await (run-job! s 'jobs.survival.respond-to-hostile {})))
          (is (>= @paces 2) "a hit within the hit window keeps the response looking, not one blink look"))))))

(defn ^:async searches-in-flee-leg
  "Runs a flight whose search never finishes (1 s a round) and returns the search rounds, the longest a go-to leg searched (ms) and the attacks. act: called with
  the fake state atom at each search."
  [setup-opts args act]
  (let [s (setup setup-opts)
        {:keys [p]} s
        world (.-world p)
        searches (atom 0)
        longest (atom 0)]
    (with-redefs [near/walk-round! (fn [c _ _ _]
                                     (swap! searches inc)
                                     (swap! (:clock s) + 1000)
                                     (when (.endsWith (:id c) "/step/run/walk") (swap! longest max (* 1000 (- (get-in (ctx/frozen-call c) [:spend :job-time]) (get-in (ctx/left c) [:spend :job-time])))))
                                     (act (fake/state p) @searches)
                                     (js/Promise.resolve {:result {:status :searching :fresh false} :status "searching"
                                                          :from (:pos c) :to (:pos c)}))]
      (await (run-job! s 'test.flight args)))
    {:searches @searches :longest-ms @longest :attacks (count (filter #(= "attack" (.-name %)) (.-calls world)))}))

(defn clear-after-6 [st n] (when (>= n 6) (swap! st assoc :entities [])))

(defn first-search-then-clear
  "A search act that runs first-act at the first search and clear-after-6 at the others."
  [first-act]
  (fn [st n] ((get {1 first-act} n clear-after-6) st n)))

(defn close-in [st _] (swap! st update :entities (fn [[e]] [(assoc-in e [:pos 0] 1)])))
(defn hurt [st _] (swap! st update-in [:self :health] - 2))

(deftest a-flee-leg-is-short-with-a-close-hitter-within-the-close-radius-creeper-included
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [e [(zombie 7 6 {}) (zombie 7 8 {}) (zombie 7 6 {:name "creeper"})]]
          (let [{:keys [longest-ms]} (await (searches-in-flee-leg {:entities [e]} {} clear-after-6))]
            (is (<= longest-ms 2000) (str "a hitter at " (:x (:pos e)) " bounds the leg to about one search round: " (:name e) " " longest-ms))))))))

(deftest a-hitter-beyond-the-close-radius-leaves-the-leg-long
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [longest-ms]} (await (searches-in-flee-leg {:entities [(zombie 7 11 {})]} {:settings {:jobs.lib.danger/hostile-radius 14}} clear-after-6))]
          (is (>= longest-ms 4000) "a mob 11 blocks off is not close: the leg searches on"))))))

(deftest the-close-radius-is-a-setting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [longest-ms]} (await (settings/with-settings {:jobs.survival.respond.options/close-radius 4}
                                          #(searches-in-flee-leg {:entities [(zombie 7 6 {})]} {} clear-after-6)))]
          (is (>= longest-ms 4000) "a zombie at 6 is outside a close radius of 4"))))))

(deftest an-armed-body-whose-search-runs-while-the-zombie-closes-in-fights
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [attacks]} (await (searches-in-flee-leg {:inventory [{:name "stone_sword" :count 1}] :entities [(zombie 7 11 {})]} {:settings {:jobs.lib.danger/hostile-radius 14}} (first-search-then-clear (fn [st n] (hurt st n) (close-in st n)))))]
          (is (pos? attacks) "armed with a sword, zombie adjacent, search running: it hits back"))))))

(deftest a-chase-that-gains-no-distance-escalates-and-a-gain-resets-it
  (let [step (fn [mem gap] (respond.state/note-gap mem gap))]
    (is (= {:best-gap 5 :since-gain 0 :sweeps 0} (step {:sweeps 1} 5)) "first gap is a gain")
    (is (= {:best-gap 5 :since-gain 2 :sweeps 1} (-> {:best-gap 5 :since-gain 1 :sweeps 1} (step 5.5))) "under a block: no gain")
    (is (= {:best-gap 7 :since-gain 0 :sweeps 0} (step {:best-gap 5 :since-gain 2 :sweeps 1} 7)) "a gain resets both")
    (is (respond.state/no-gain? {:since-gain 3} 3))
    (is (not (respond.state/no-gain? {:since-gain 2} 3)))))

(deftest a-step-with-no-hostile-near-is-no-gap-sample
  (is (= {:best-gap 9 :since-gain 0 :sweeps 1} (respond.state/note-gap {:best-gap 9 :since-gain 4 :sweeps 1} nil))
      "the flight is succeeding: no-gain count resets, best gap and sweeps stay")
  (is (= {:since-gain 0} (respond.state/note-gap {} nil)) "no best gap is invented from nothing")
  (is (= {:best-gap 6 :since-gain 0 :sweeps 0} (respond.state/note-gap (respond.state/note-gap {} nil) 6))))

(deftest the-step-gap-is-the-nearest-hostile-however-they-are-ordered
  (let [p (tu/fake-on-floor {:floor big-floor :entities [(zombie 1 20 {}) (zombie 2 6 {})]})
        hostiles (vec (danger-q/known-hostiles p 30 {:ranged-radius 30}))]
    (is (= 2 (count hostiles)))
    (is (= 6 (respond.state/nearest-gap p hostiles)))
    (is (= 6 (respond.state/nearest-gap p (vec (reverse hostiles)))))
    (is (nil? (respond.state/nearest-gap p [])))))

(declare holds)

(def sweeper
  "A contract job that runs retreat's blocked! until it ends the job."
  {:round (fn ^:async sweeper-round [c]
            (loop [n 1]
              (let [r (await (cornered/blocked! c "test"))]
                (if (= :again r) (recur (inc n)) r))))})

(deftest blocked-twice-in-a-row-stops-cannot-escape
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen out]} (await (run-job! (assoc-in (setup {}) [:eng :jobs 'sweeper] sweeper) 'sweeper {}))]
          (is (= [] (:list (core/state eng))) "two failed sweeps end the job")
          (is (= :stopped (:status out)))
          (is (= :cannot-escape (:reason out)) "the stop reason reaches the caller")
          (is (= 1 (count (filter #(= :retreat_blocked (:kind %)) @seen))) "warned once"))))))

(deftest blocked-holds-a-moment-unless-a-creeper-is-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label mob holds?] [["a zombie" (zombie 7 5 {}) true]
                                    ["a creeper" {:id 8 :uuid "u8" :name "creeper" :kind "hostile" :pos {:x 5 :y 64 :z 0}} false]]]
          (let [{:keys [seen]} (await (run-job! (assoc-in (setup {:entities [mob]}) [:eng :jobs 'sweeper] sweeper) 'sweeper {}))]
            (is (= holds? (boolean (seq (holds seen "cornered")))) label)))))))

(defn backer
  "A job that backs off once from the zombie and keeps what back-off! answered and whether the option stays tried."
  [out]
  {:defaults {:checks {:safe? '(always) :fit? '(always)}}
   :round (fn ^:async backer-round [c]
            (let [threat (first (danger-q/known-hostiles (:primitives c) 30 {:ranged-radius 30}))
                  r (await (cornered/back-off! c (some->> threat (danger-q/mob-pos (:primitives c)))))]
              (reset! out {:r r :tried (respond.state/tried? c :back-off)})
              (ctx/end c :done nil)))})

(deftest back-off-steps-away-and-fails-when-the-walk-does
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label go-to want] [["walks" {} {:r :again :tried false}]
                                    ["gives up" {'jobs.movement.go-to {:ways (:ways (get registry/jobs 'jobs.movement.go-to))
                                                                       :round (fn ^:async no-leg [c] (ctx/end c :nothing-possible :unreachable {:arrived false}))}}
                                     {:r nil :tried true}]]]
          (let [out (atom nil)
                s (setup {:entities [(zombie 7 2 {})]})
                s (update-in (assoc-in s [:eng :jobs 'backer] (backer out)) [:eng :jobs] merge go-to)]
            (await (run-job! s 'backer {}))
            (is (= want @out) (str "go-to " label))))))))

(defn fighter
  "A job that runs a fight whose attack child declines, after a sweep already counted and with the earlier call's per-mob hits as the prior; keeps the memory's sweeps."
  [out before]
  {:defaults {:checks {:safe? '(always) :fit? '(always)}}
   :round (fn ^:async fighter-round [c]
            (ctx/update-mem! c assoc :sweeps 1 :fight-prior {:struck (into {} (for [[id n] before] [id {:name "zombie" :hits n}]))})
            (reset! out [(await (cornered/fight! c [] false)) (:sweeps (ctx/mem c))])
            (ctx/end c :done nil))})

(deftest a-fight-that-landed-new-hits-resets-the-sweeps
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[before after want] [[{} {1 2} 0] [{1 2} {1 2} 1] [{1 2} {1 3} 0]]]
          (let [out (atom nil)
                s (setup {:entities [(zombie 7 2 {})]})
                s (update-in (assoc-in s [:eng :jobs 'fighter] (fighter out before)) [:eng :jobs] merge
                             {'jobs.combat.attack {:round (fn ^:async declined [c] (ctx/end c :nothing-possible :unreachable {:struck (into {} (for [[id n] after] [id {:name "zombie" :hits n}]))}))}})]
            (await (run-job! s 'fighter {}))
            (is (= [nil want] @out) (str before " -> " after))))))))

(deftest a-fight-resumed-after-flee-steps-measures-its-leash-from-where-the-body-now-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen (atom :unset)
              fighter {:defaults {:checks {:safe? '(always) :fit? '(always)}}
                       :round (fn ^:async f [c]
                                (ctx/update-mem! c assoc-in [:children :cornered] {:start {:x 0 :y 64 :z 0}})
                                (await (cornered/fight! c [] false))
                                (ctx/end c :done nil))}
              s (setup {:entities [(zombie 7 2 {})]})
              s (update-in (assoc-in s [:eng :jobs 'fighter] fighter) [:eng :jobs] merge
                           {'jobs.combat.attack {:round (fn ^:async look [c] (reset! seen (:start (ctx/mem c))) :continue)}})]
          (await (run-job! s 'fighter {}))
          (is (nil? @seen) "the stale start of the fight before the flee steps is dropped"))))))

(defn stepper
  "A job that takes one flee step to a fixed cell and keeps the refused ends the flight remembers."
  [out]
  {:defaults {:checks {:safe? '(always) :fit? '(always)}}
   :round (fn ^:async stepper-round [c]
            (await (cornered/leg-step! c :step {:x 7 :y 64 :z 2} 0 5))
            (reset! out (:refused (ctx/mem c)))
            (ctx/end c :done nil))})

(deftest a-flee-end-go-to-refuses-as-not-standable-is-remembered-for-the-flight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[why want] [[:goal-not-standable #{[7 64 2]}] [:no-progress nil]]]
          (let [out (atom :unset)
                s (setup {:entities [(zombie 7 2 {})]})
                s (update-in (assoc-in s [:eng :jobs 'stepper] (stepper out)) [:eng :jobs] merge
                             {'jobs.movement.go-to {:ways (:ways (get registry/jobs 'jobs.movement.go-to))
                                                    :round (fn ^:async refuse [c] (ctx/end c :nothing-possible :unreachable {:arrived false :why why}))}})]
            (await (run-job! s 'stepper {}))
            (is (= want @out) (str "go-to ends " why))))))))

(deftest every-option-failing-in-two-sweeps-in-a-row-is-cannot-escape
  (is (not (respond.state/cannot-escape? (respond.state/count-sweep {}))))
  (is (respond.state/cannot-escape? (respond.state/count-sweep (respond.state/count-sweep {})))))

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
    (await (run-job! s 'test.flight {}))))

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
              out (await (run-job! s 'test.flight {}))]
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
              {:keys [calls out eng]} (await (run-job! s 'test.flight {}))]
          (is (= [:done] calls) "sealed, hid and ended in one round")
          (is (seq (holds seen "hiding")) "a declared hold (a pause child) while hidden")
          (is (= :hidden (:ended out)))
          (is (= [] (:list (core/state eng)))))))))

(deftest a-chaser-beyond-the-radius-that-keeps-its-distance-ends-the-flight-keeps-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:floor [-8 -8 8 8] :entities [(zombie 7 4 {:pos {:x 4 :y 64 :z 4}})]})
              {:keys [calls out]} (await (run-job! s 'test.flight {:settings {:jobs.survival.respond.state/still-s 3600}}))]
          (is (= [:done] calls) "the flight ends, it does not dance for ever")
          (is (= :keeps-off (:ended out)))
          (is (re-find #"still following" (:text out))))))))

(def long-tunnel
  "Stone round a 1-wide tunnel along x on z 0, feet and head height, x 0..20, closed behind the body at x -1."
  (let [open (set (for [x (range 0 21) y [64 65]] [x y 0]))]
    (into {} (for [x (range -3 23) y (range 62 68) z [-1 0 1] :when (not (open [x y z]))]
               [(key-of x y z) "stone"]))))

(deftest a-walled-in-body-with-a-chaser-beyond-the-radius-ends-the-flight-keeps-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (setup {:floor [-3 -1 23 1] :blocks long-tunnel :entities [(zombie 7 8 {})]})
              world (.-world p)
              _ (doseq [name ["steer" "moveTo" "wait" "attack" "place" "dig" "equip" "eat"]]
                  (.override world name (fn ^:async f [token a impl]
                                          (swap! (fake/state p) update :entities (partial mapv #(assoc % :pos [15 64 0])))
                                          (await (impl token a)))))
              {:keys [calls out]} (await (run-job! s 'test.flight {:settings {:jobs.survival.respond.state/still-s 3600}}))]
          (is (= [:done] calls) "no way away, nothing within :radius: the flight ends")
          (is (= :keeps-off (:ended out))))))))

;; ------------------------------------------------------------------ what the flight senses, resume, the round's bound

(deftest the-flight-judges-chasers-from-what-the-body-knows
  (let [p (tu/fake-on-floor {:floor big-floor :entities [(zombie 7 30 {})]})
        per (perception/create (fake-raw/create p) {:now (constantly 1000000)})
        wrapped (perception/wrap p per)]
    (is (= [7] (mapv #(.-id %) (respond.state/known-chasers p))) "no perception: every tracked mob")
    (is (= [] (mapv #(.-id %) (respond.state/known-chasers wrapped))) "30 blocks off, unseen and unheard: not known")))

(deftest the-cornered-body-weighs-only-the-hostiles-it-knows
  (let [p (tu/fake-on-floor {:floor big-floor :entities [(zombie 7 30 {})]})
        per (perception/create (fake-raw/create p) {:now (constantly 1000000)})
        wrapped (perception/wrap p per)
        ids #(mapv (fn [e] (.-id e)) %)]
    (is (= [7] (ids (respond.state/near-known p #{} 40 nil))) "no perception: every tracked mob")
    (is (= [] (ids (respond.state/near-known wrapped #{} 40 nil))) "unseen and unheard: not weighed")
    (is (= [] (vec (seal/hostile-cells wrapped 40))) "no cell is kept clear for an unknown mob")
    (is (= [] (ids (respond.state/near-known p #{7} 40 nil))) "a corpse is skipped")))

(deftest a-flight-resumed-after-a-gap-starts-its-clocks-afresh
  (let [mem {:flight-start 0 :last-step 1000 :chasers {7 {:id 7 :seen-t 1000}}}]
    (is (= mem (respond.state/resume-flight mem 3000)) "a short gap: as it was")
    (is (= {:flight-start 100000 :last-step 100000 :chasers {7 {:id 7 :seen-t 100000}}}
           (respond.state/resume-flight mem 100000))
        "a long gap: the flight starts now and every chaser was just seen")))

(defn ^:async with-flee
  "Run (f) with the flight (respond-flight/flee!) replaced by stub, a fn of the ctx giving an ending or :continue."
  [stub f]
  (let [flee retreat/flee!]
    (set! retreat/flee! (fn [c] (js/Promise.resolve (stub c))))
    (try (await (f))
         (finally (set! retreat/flee! flee)))))

(deftest respond-to-hostile-stops-after-three-calls-that-change-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(zombie 7 5 {:chase false})]})
              clock (:clock s)
              {:keys [calls out]} (await (with-flee (fn idle-flight [c] (swap! clock + 1000) (ctx/end c :done nil))
                                                    #(run-job! s 'jobs.survival.respond-to-hostile {})))]
          (is (= [:done] calls))
          (is (= {:status :stopped :reason :no-response} (select-keys out [:status :reason])))
          (is (= :nothing-possible (get-in out [:ending :kind]))))))))

(deftest respond-to-hostile-stops-retreat-failed-with-the-flights-ending-as-because
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(zombie 7 5 {:chase false})]})
              {:keys [out]} (await (with-flee (fn no-escape [c] (ctx/end c :nothing-possible :cannot-escape))
                                              #(run-job! s 'jobs.survival.respond-to-hostile {})))]
          (is (= {:status :stopped :reason :retreat-failed} (select-keys out [:status :reason])))
          (is (= :nothing-possible (get-in out [:ending :kind])))
          (is (= :cannot-escape (get-in out [:ending :data :flight-ended])) "the flight's ending is the because"))))))

(deftest respond-to-hostile-passes-the-kind-of-a-flight-that-ends-impossible-or-limit-reached
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [kind [:impossible :limit-reached]]
          (let [s (setup {:entities [(zombie 7 5 {:chase false})]})
                {:keys [out]} (await (with-flee (fn ends [c] (ctx/end c kind :stub))
                                                #(run-job! s 'jobs.survival.respond-to-hostile {})))]
            (is (= kind (get-in out [:ending :kind])))))))))

(deftest respond-to-hostile-is-done-when-the-danger-is-gone-after-a-long-hide
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(zombie 7 5 {:chase false})]})
              clock (:clock s)
              p (:p s)
              {:keys [calls out]} (await (with-flee (fn long-hide [c]
                                                      (swap! clock + 400000)
                                                      (swap! (fake/state p) assoc :entities [])
                                                      (ctx/end c :done nil {:fled [7] :ended :hidden}))
                                                    #(run-job! s 'jobs.survival.respond-to-hostile {})))]
          (is (= [:done] calls))
          (is (not= :stopped (:status out))))))))

(deftest a-heard-mob-keeps-cells-clear-at-its-rough-spot-not-its-exact-one
  (let [p0 (tu/fake-on-floor {:floor big-floor})
        [bx] (body-pos p0)
        p (tu/fake-on-floor {:floor big-floor :entities [(zombie 7 (+ bx 10) {:visible false})]})
        cells (seal/hostile-cells p 40)]
    (is (contains? (set (map :x cells)) (js/Math.floor (+ bx 16))) "heard 10 blocks east (far band): 16 east")
    (is (not-any? #(= (js/Math.floor (+ bx 10)) (:x %)) cells) "the exact place is not kept")))

;; ------------------------------------------------------------------ a flight reads what the body sees

(defn setup-sensed
  "setup over perception (nothing seen yet, nothing remembered): the body at the origin looking east, level."
  [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        raw (tu/fake-on-floor (merge {:floor big-floor :yaw 270 :pitch 0} world))
        p (perception/wrap raw (perception/create (fake-raw/create raw) {:now (fn [] @clock)}))
        now (tu/act-clock clock p ms-per-call)
        eng (core/create {:primitives p :jobs fj/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    {:eng eng :p p :raw raw :seen seen :clock clock}))

(defn targeter
  "A job that picks one flight target (cover/flight-target!) away from the hostiles it knows, kept in out."
  [out]
  {:round (fn ^:async targeter-round [c]
            (when-let [g (:goal (:args c))] (ctx/update-mem! c assoc :goal g))
            (let [threats (danger-q/known-hostiles (:primitives c) 30 {:ranged-radius 30})]
              (reset! out (await (cover/flight-target! c threats)))
              (ctx/end c :done nil)))})

(deftest a-flight-in-an-open-field-turns-to-see-its-way-and-flees-far
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [raw seen out]} (await (run-job! (setup-sensed {:entities [(zombie 7 5 {:chase {:speed 0.7}})]})
                                                      'test.flight {}))
              names (set (map #(.-name %) (.-calls (.-world raw))))]
          (is (contains? #{:far :lost} (:ended out)) (str "out of its range, or out of sight and hearing: " (pr-str out)))
          (is (> (fake/dist (body-pos raw) (mob-pos raw 7)) 16) "beyond hearing")
          (is (< (first (body-pos raw)) -30) "fled far west, away from the zombie")
          (is (not (contains? names "attack")) "a chased body in the open never fights")
          (is (empty? (filter #(= :retreat_blocked (:kind %)) @seen)) "never cornered"))))))

(deftest a-flight-target-behind-the-body-is-looked-at-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [out (atom nil)
              s (setup-sensed {:entities [(zombie 7 5 {})]})
              s (assoc-in s [:eng :jobs 'targeter] (targeter out))]
          (await (run-job! s 'targeter {:step 6}))
          (is (= {:x -6 :y 64 :z 0} @out) "straight west, away from the zombie"))))))

(deftest a-flight-with-a-goal-behind-the-chaser-goes-round-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [run (fn ^:async run [args]
                    (let [out (atom nil)
                          s (assoc-in (setup-sensed {:entities [(zombie 7 5 {})]}) [:eng :jobs 'targeter] (targeter out))]
                      (await (run-job! s 'targeter args))
                      @out))
              away (await (run {:step 6}))
              round (await (run {:step 6 :goal {:x 10 :y 64 :z 16}}))]
          (is (= {:x -6 :y 64 :z 0} away) "no goal: straight away")
          (is (pos? (:z round)) (str "the goal lies past the zombie on the +z side: the flight turns that way " (pr-str round)))
          (is (<= (:x round) 0) "and still gains on the zombie"))))))

(defn first-target [seen] (:target (first (filterv #(= :retreat.target (:kind %)) @seen))))

(deftest a-flight-never-walks-to-a-goal-past-its-chaser
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (run-job! (setup {:entities [(zombie 7 6 {:chase {:speed 0.7}})]})
                                              'test.flight {:goal {:x 20 :y 64 :z 0}}))
              [x] (first-target seen)]
          (is (< x 0) (str "the goal lies past the zombie: away from it instead " (pr-str (first-target seen)))))))))

(deftest a-flight-walks-to-a-goal-on-the-side-away-from-its-chaser
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (run-job! (setup {:entities [(zombie 7 6 {:chase {:speed 0.7}})]})
                                              'test.flight {:goal {:x -20 :y 64 :z 0}}))]
          (is (= [-20 64 0] (first-target seen))))))))

(deftest a-flight-that-reaches-its-goal-flees-on-from-there-and-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out seen]} (await (run-job! (setup {:entities [(zombie 7 5 {:chase {:speed 0.7}})]})
                                                  'test.flight {:goal {:x 20 :y 64 :z 12}}))]
          (is (= :done (:status out)) (pr-str out))
          (is (< (count (filterv #(= :retreat.target (:kind %)) @seen)) 200) "no walking back and forth between the goal and the way away"))))))

;; ------------------------------------------------------------------ the goal leg keeps its height

(defn scripted-go-to
  "s with a go-to that walks nowhere: each call's args go to calls and the call ends with the result
  (script state-atom n) gives, n the count of calls so far; a result with :leg true moves the body 1.5 blocks toward the
  goal (the mobs with it) and ends as the engine ends a call whose seconds ran out."
  [s calls script]
  (let [real (get registry/jobs 'jobs.movement.go-to)
        st (fake/state (or (:raw s) (:p s)))]
    (assoc-in s [:eng :jobs 'jobs.movement.go-to]
              (assoc real :round (fn ^:async stub [c]
                                   (swap! calls conj (:args c))
                                   (let [r (script st (count @calls))]
                                     (if-not (:leg r)
                                       (end/finish! c r)
                                       (let [to (:pos (:args c))
                                             x (nth (:pos (:self @st)) 0)]
                                         (swap! st update-in [:self :pos] assoc 0 (+ x (* 1.5 (js/Math.sign (- (:x to) x)))))
                                         (swap! st update :entities (fn [es] (mapv #(update-in % [:pos 0] + (* 1.5 (js/Math.sign (- (:x to) x)))) es)))
                                         (ctx/end c :limit-reached {:reason :unreachable :why :search-spent :line :job-time} {:arrived false})))))))))

(defn stub-go-to
  "scripted-go-to of one step: (move state-atom) runs, the hostiles vanish (so the flight ends after one step) and the
  call ends as a leg that got nearer."
  [s calls move]
  (scripted-go-to s calls (fn [st _]
                            (move st)
                            (swap! st assoc :entities [])
                            {:arrived false :leg true})))

(defn events-of [seen kind] (filterv #(= kind (:kind %)) @seen))

(def seen-stair
  "Two stair floors west of the body with a stone wall behind them, so rays end there and the open cells over the floors are seen."
  (into {(key-of -4 64 0) "stone" (key-of -5 65 0) "stone"}
        (for [y (range 63 72) z (range -3 4)] [(key-of -9 y z) "stone"])))

(deftest a-buried-flight-takes-its-own-stair-before-a-goal-far-above
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              s (setup-sensed {:entities [(zombie 7 5 {})] :yaw 90 :pitch -25 :blocks seen-stair})
              _ (swap! (fake/state (:raw s)) assoc :light-default [0 14])
              _ (mem/write! (:store (:eng s)) :stair-way {:dim "overworld" :floors [[-4 64 0] [-5 65 0]]})
              _ (perception/pass! (aget (:p s) "perception") {:effort 1 :reuse? true})
              {:keys [seen]} (await (run-job! (stub-go-to s calls identity) 'test.flight {:goal {:x -40 :y 114 :z 0}}))]
          (is (= [-5 66 0] (first-target seen)) "the stair it cut, not a leg to a goal 50 blocks above through the caves"))))))

(deftest a-stand-behind-a-wall-is-not-seen-so-no-way-up-leads-to-it
  (let [wall (into {} (for [y (range 62 68) z (range -3 4)] [(key-of 5 y z) "stone"]))
        {:keys [p]} (setup-sensed {:blocks wall})]
    (perception/pass! (aget p "perception") {:effort 1 :reuse? true})
    (is (cover/seen-stand? p [3 63 0]) "open ground in front of the wall")
    (is (not (cover/seen-stand? p [7 63 0])) "the same stand behind it was never in view")))

(deftest a-goal-leg-that-takes-the-body-away-from-the-goals-height-ends-the-goal-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              s (setup {:entities [(zombie 7 6 {})]})
              dive (fn [st] (swap! st update-in [:self :pos] (fn [[x y z]] [(- x 4) (- y 5) z])))
              {:keys [seen]} (await (run-job! (stub-go-to s calls dive) 'test.flight {:goal {:x -20 :y 64 :z 0}}))]
          (is (= [-20 64 0] (first-target seen)) "the first leg aims at the goal")
          (is (= 1 (count (events-of seen :retreat.goal-off)))
              "a leg that ends nearer in x/z but 5 blocks under the goal's height is no way to it"))))))

(deftest a-flight-does-not-walk-to-a-goal-under-rock-below-its-descent-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              s (setup {:blocks {(key-of -20 63 0) "stone"} :entities [(zombie 7 6 {})]})
              {:keys [seen]} (await (run-job! (stub-go-to s calls identity) 'test.flight {:goal {:x -20 :y 54 :z 0}}))]
          (is (some? (first-target seen)))
          (is (not= [-20 54 0] (first-target seen)) "a goal 10 down a roofed shaft is not walked to"))))))

(deftest a-goal-leg-never-ends-within-keep-away-of-a-hostile
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              s (setup {:entities [(zombie 7 5 {}) (zombie 8 0 {:pos {:x -6 :y 64 :z 3}})]})
              {:keys [seen]} (await (run-job! (stub-go-to s calls identity) 'test.flight {:goal {:x -6 :y 64 :z 0}}))]
          (is (not= [-6 64 0] (first-target seen)) "a second hostile stands 3 blocks from the goal"))))))

;; ------------------------------------------------------------------ a goal must be safe to lean to

(def here {:x 0.5 :y 64 :z 0.5})

(deftest a-goal-is-judged-in-three-dimensions-against-every-known-hostile-and-a-safer-home
  (doseq [[why goal opts] [[nil {:x -20 :y 64 :z 0} {:hostiles [{:x 6 :y 64 :z 0}]}]
                           [:steep {:x 3 :y 90 :z 0} {:hostiles [{:x 4 :y 64 :z 0}]}]
                           [:steep {:x 3 :y 40 :z 0} {}]
                           [nil {:x -20 :y 72 :z 0} {}]
                           [:under-rock {:x -40 :y 84 :z 0} {:buried? true}]
                           [nil {:x -40 :y 84 :z 0} {:buried? false}]
                           [:hostile-there {:x -20 :y 64 :z 0} {:hostiles [{:x -22 :y 64 :z 5}]}]
                           [:past-hostile {:x 20 :y 64 :z 0} {:hostiles [{:x 6 :y 64 :z 0}]}]
                           [:home-safer {:x 10 :y 64 :z 30} {:hostiles [{:x 6 :y 64 :z 0}] :home {:x -12 :y 64 :z 0}}]
                           [nil {:x -14 :y 64 :z 3} {:hostiles [{:x 6 :y 64 :z 0}] :home {:x -12 :y 64 :z 0}}]]]
    (is (= why (respond.goal/verdict here goal opts)) (pr-str goal opts))))

(deftest a-goal-a-step-toward-which-nears-a-creeper-in-blast-reach-is-no-goal
  (let [goal {:x 9 :y 64 :z 20}]
    (is (= :past-hostile (respond.goal/verdict here goal {:blasts [{:x 3 :y 64 :z 1}]}))
        "the creeper 3 blocks off lies 53 degrees off the line to the goal: a tangent that lets it close in")
    (is (nil? (respond.goal/verdict here goal {:blasts [{:x -3 :y 64 :z 1}]})) "a creeper behind the body")
    (is (nil? (respond.goal/verdict here goal {:hostiles [{:x 3 :y 64 :z 1}]})) "any other mob keeps the 45 degree cone")))

(deftest a-home-is-safer-only-when-the-body-is-nearer-it-than-every-hostile-and-none-is-on-the-way
  (let [home {:x -12 :y 64 :z 0}]
    (is (= home (respond.goal/safe-home here [{:x 6 :y 64 :z 0}] [home])))
    (is (nil? (respond.goal/safe-home here [{:x -10 :y 64 :z 8}] [home])) "the hostile gets there first")
    (is (nil? (respond.goal/safe-home here [{:x -5 :y 64 :z 0}] [home])) "the hostile stands on the way")
    (is (nil? (respond.goal/safe-home here [{:x 6 :y 64 :z 0}] [])) "no home known")))

(deftest a-home-is-dropped-for-the-reason-it-is-no-safe-run
  (let [home {:x -12 :y 64 :z 0}]
    (is (nil? (respond.goal/home-why here [{:x 6 :y 64 :z 0}] home)))
    (is (= :hostile-nearer (respond.goal/home-why here [{:x -10 :y 64 :z 10}] home)))
    (is (= :past-hostile (respond.goal/home-why here [{:x -5 :y 64 :z 0}] home)))
    (is (nil? (respond.goal/home-why here [{:x 6 :y 64 :z 0}] nil)) "no home: nothing to explain")))

(defn choice [seen] (first (events-of seen :retreat.choice)))

(deftest the-choice-says-why-a-home-is-no-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              s (setup {:entities [(zombie 7 6 {})]})
              _ (mem/write! (:store (:eng s)) :home {:pos {:x 12 :y 64 :z 0}} {:cap 1 :ttl :forever})
              {:keys [seen]} (await (run-job! (stub-go-to s calls identity) 'test.flight {}))
              e (choice seen)]
          (is (= [[12 64 0] :past-hostile] [(:home e) (:home-why e)]) (pr-str e)))))))

(deftest a-flight-spends-no-leg-on-a-goal-straight-above-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              s (setup {:entities [(zombie 7 4 {})]})
              {:keys [seen]} (await (run-job! (stub-go-to s calls identity) 'test.flight {:goal {:x 3 :y 90 :z 0}}))]
          (is (some? (first-target seen)))
          (is (not= [3 90 0] (first-target seen)) "26 up and 3 across is no way out")
          (is (= :steep (:goal-why (choice seen))) (pr-str (choice seen))))))))

(deftest a-home-the-body-reaches-first-wins-over-the-goal-of-the-walk-it-was-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              s (setup {:entities [(zombie 7 6 {})]})
              _ (mem/write! (:store (:eng s)) :home {:pos {:x -12 :y 64 :z 0}} {:cap 1 :ttl :forever})
              {:keys [seen]} (await (run-job! (stub-go-to s calls identity) 'test.flight {:goal {:x 10 :y 64 :z 30}}))
              [x] (first-target seen)
              e (choice seen)]
          (is (neg? x) (str "west, to the home side, not to the errand's goal: " (pr-str (first-target seen))))
          (is (= [:home-safer [10 64 30] [-12 64 0]] [(:goal-why e) (:goal e) (:lean e)]) (pr-str e))
          (is (= ["zombie 7 [6 64 0]"] (:hostiles e)) "the choice names the hostiles and where they were"))))))

(deftest a-flight-does-not-walk-to-a-goal-beside-a-place-it-fled-a-mob-from
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              s (setup {:entities [(zombie 7 6 {}) {:id 99 :uuid "u99" :name "skeleton" :kind "hostile" :visible false :pos {:x -22 :y 64 :z 5}}]})
              _ (mem/write! (:store (:eng s)) :threat {:id 99 :mob "skeleton" :pos {:x -22 :y 64 :z 5} :ended :lost} threats/threat-policy)
              {:keys [seen]} (await (run-job! (stub-go-to s calls identity) 'test.flight {:goal {:x -20 :y 64 :z 0}}))]
          (is (not= [-20 64 0] (first-target seen)))
          (is (= :hostile-there (:goal-why (choice seen))) (pr-str (choice seen))))))))

;; ------------------------------------------------------------------ the flight avoids where the body died and dark ground

(defn looking-targeter
  "targeter that looks all round first, so the ground about is seen (and its light known)."
  [out]
  (update (targeter out) :round (fn [round] (fn ^:async looking-round [c] (await (look/look-around! c)) (await (round c))))))

(defn ^:async target-after
  "The cell cover/flight-target! picks for a zombie to the east, with the :died spots and settings given."
  [{:keys [light light-default time]} died settings-map]
  (let [out (atom nil)
        s (assoc-in (setup-sensed (cond-> {:entities [(zombie 7 5 {})]} time (assoc :time time))) [:eng :jobs 'targeter] (looking-targeter out))]
    (when light-default (swap! (fake/state (:raw s)) assoc :light light :light-default light-default))
    (doseq [pos died] (mem/write! (:store (:eng s)) :died {:pos pos} {:cap 4 :ttl :forever}))
    (await (settings/with-settings settings-map #(run-job! s 'targeter {:step 6})))
    @out))

(deftest a-verdict-drops-a-goal-near-a-place-the-body-died
  (let [spot {:x -22 :y 64 :z 3}]
    (is (= :died-there (respond.goal/verdict here {:x -20 :y 64 :z 0} {:died [spot]})))
    (is (nil? (respond.goal/verdict here {:x -20 :y 64 :z 0} {:died [{:x -22 :y 64 :z 40}]})) "a death far from the goal")))

(deftest a-flight-does-not-run-toward-a-place-the-body-died
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spot {:x -12 :y 64 :z 0}
              out (await (target-after {} [spot] {}))
              plain (await (target-after {} [] {}))
              off (await (target-after {} [spot] {:jobs.survival.respond.state/died-radius 0}))]
          (is (= {:x -6 :y 64 :z 0} plain) "no death: straight west")
          (is (> (u/dist out spot) 12) (str "the straight way ends within the radius of the death: " (pr-str out)))
          (is (= {:x -6 :y 64 :z 0} off) "radius 0 turns it off"))))))

(deftest a-flight-prefers-lit-ground-to-dark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [lit (into {} (for [x (range -12 4) z (range -12 13) y [63 64 65] :when (> (js/Math.abs z) 1)] [[x y z] [0 8]]))
              world {:time 18000 :light-default [15 0] :light lit}
              out (await (target-after world [] {}))
              off (await (target-after world [] {:jobs.survival.respond.state/prefer-lit? false}))
              day (await (target-after (assoc world :time 1000) [] {}))
              day-dark (await (target-after (assoc world :time 1000 :light-default [0 0]) [] {}))]
          (is (neg? (:z out)) (str "west is dark, north-west is lit: " (pr-str out)))
          (is (= {:x -6 :y 64 :z 0} off) "prefer-lit? false: straight west")
          (is (= {:x -6 :y 64 :z 0} day) "by day with every seen cell lit there is no reason to turn")
          (is (neg? (:z day-dark)) (str "dark is a light check, not the clock: dark seen ground by day is left out too " (pr-str day-dark))))))))

;; ------------------------------------------------------------------ a goal given up comes back

(def west-goal {:x -20 :y 64 :z 0})

(defn goal-legs [calls] (filterv #(= west-goal (:pos %)) @calls))

(deftest a-goal-given-up-is-walked-to-again-after-the-retry-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[retry-s legs] [[30 2] [100000 1]]]
          (let [calls (atom [])
                s (setup {:entities [(zombie 7 6 {})]})
                script (fn [st n]
                         (swap! (:clock s) + 10000)
                         (swap! st update :entities #(vec (take (- 8 n) %)))
                         {:arrived false :leg (and (> n 1) (not= west-goal (:pos (last @calls))))})]
            (await (settings/with-settings {:jobs.survival.respond.goal/retry-s retry-s}
                     #(run-job! (scripted-go-to s calls script) 'test.flight {:goal west-goal})))
            (is (= west-goal (:pos (first @calls))) "the first leg, which fails, goes to the goal")
            (is (= legs (count (goal-legs calls))) (str "retry-s " retry-s ": " (mapv :pos @calls)))))))))

(deftest a-flight-soon-after-one-that-gave-its-goal-up-skips-that-goal-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              s (setup {:entities [(zombie 7 6 {})]})
              st (fake/state (:p s))
              mobs (:entities @st)
              s (scripted-go-to s calls (fn [st _] (swap! st assoc :entities []) {:arrived false :leg false}))
              s (await (run-job! s 'test.flight {:goal west-goal}))
              _ (swap! st assoc :entities mobs)
              {:keys [seen]} (await (run-job! s 'test.flight {:goal west-goal}))]
          (is (= 2 (count @calls)) "one step a flight")
          (is (= 1 (count (goal-legs calls))) "the second flight does not spend a leg on the goal the first gave up")
          (is (= :given-up (:goal-why (last (events-of seen :retreat.choice))))))))))

(def walker
  "A listed job whose memory holds an open go-to walk to (10 64 16), as one cut on its way."
  {:round (fn ^:async walker-round [c]
            (ctx/update-mem! c assoc :children {:go {:open 1 :goal {:x 10 :y 64 :z 16}}})
            :continue)})

(deftest the-hostile-reflex-passes-on-the-goal-of-the-walk-it-interrupted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(zombie 7 5 {:chase {:speed 0.7}})]})
              s (assoc-in s [:eng :jobs 'walker] walker)
              _ (core/submit! (:eng s) '(walker) {})
              _ (await (core/tick! (:eng s)))
              {:keys [seen]} (await (run-job! s 'jobs.survival.respond-to-hostile {}))]
          (is (= [10 64 16] (first-target seen))))))))

(def tunnel-cells (set (for [x (range 0 9) y [64 65]] [x y 0])))

(deftest a-flight-target-never-reads-unseen-rock-as-a-way
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [out (atom nil)
              rock (into {} (for [x (range -6 10) y (range 63 68) z [-1 0 1]
                                  :when (not (or (tunnel-cells [x y z]) (and (<= -4 x -2) (#{64 65} y) (zero? z))))]
                              [(key-of x y z) "stone"]))
              s (setup-sensed {:blocks rock :entities [(zombie 7 5 {})]})
              s (assoc-in s [:eng :jobs 'targeter] (targeter out))]
          (await (run-job! s 'targeter {:step 6}))
          (is (nil? @out) (str "the cave behind its rock wall is no way: " (pr-str @out))))))))

(deftest a-buried-flight-takes-the-stair-the-body-cut-instead-of-a-hop-across-the-cave
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [out (atom nil)
              s (setup-sensed {:entities [(zombie 7 5 {})] :yaw 90 :pitch -25 :blocks seen-stair})
              _ (swap! (fake/state (:raw s)) assoc :light-default [0 14])
              _ (mem/write! (:store (:eng s)) :stair-way {:dim "overworld" :floors [[-4 64 0] [-5 65 0]]})
              _ (perception/pass! (aget (:p s) "perception") {:effort 1 :reuse? true})
              s (assoc-in s [:eng :jobs 'targeter] (targeter out))]
          (await (run-job! s 'targeter {:step 6}))
          (is (= {:x -5 :y 66 :z 0} @out) "the highest stair cell, away from the zombie, not a 6 block hop"))))))

(def rock-box
  "Stone z -3..3, x -12..12, y 62..68 round a 1-wide tunnel along x at z 0 (x -10..9, feet and head height)."
  (let [open (set (for [x (range -10 10) y [64 65]] [x y 0]))]
    (into {} (for [x (range -12 13) y (range 62 69) z (range -3 4) :when (not (open [x y z]))]
                [(key-of x y z) "stone"]))))

(defn buried-setup [world]
  (let [s (setup (merge {:blocks rock-box :inventory [{:name "stone_pickaxe" :count 1}]} world))]
    (swap! (fake/state (:p s)) assoc :light-default [0 0])
    s))

(defn dug [p] (filterv #(= "dig" (.-name %)) (.-calls (.-world p))))

(deftest a-goal-leg-under-rock-plans-no-deeper-than-the-descent-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom [])
              s (buried-setup {:inventory [] :entities [(zombie 7 6 {})]})]
          (await (run-job! (stub-go-to s calls identity) 'test.flight {:goal {:x -9 :y 64 :z 0}}))
          (is (= {:x -9 :y 64 :z 0} (:pos (first @calls))) "the leg goes to the goal")
          (is (= (flee-walk/max-descent) (get-in (first @calls) [:settings :jobs.lib.walk.world/y-margin])) "and its plan may not dip under the descent floor"))))))

(deftest a-buried-flight-with-no-stair-digs-a-pocket-before-any-hop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (run-job! (buried-setup {:entities [(zombie 7 6 {})]}) 'test.flight {}))
              names (mapv #(.-name %) (.-calls (.-world p)))]
          (is (= "dig" (first (filter #{"dig" "steer"} names))) "a pocket, not a hop down the tunnel"))))))

(deftest a-failed-pocket-is-not-tried-again-in-the-same-flight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (buried-setup {:entities [(zombie 7 6 {})]})
              digs (atom 0)
              _ (.override (.-world (:p s)) "dig" (fn ^:async f [_ _ _] (swap! digs inc) #js {:status "failed"}))
              job {:round (fn ^:async again [c]
                            (dotimes [_ 3]
                              (ctx/update-mem! c dissoc :tried)
                              (await (cover/dig-pocket! c)))
                            (ctx/end c :done nil))}
              s (assoc-in s [:eng :jobs 'again] job)]
          (await (run-job! s 'again {}))
          (is (= 1 @digs) "one dig attempt for the whole flight"))))))

(defn looks [raw] (count (filter #(= "look" (.-name %)) (.-calls (.-world raw)))))

(deftest a-flight-in-a-seen-tunnel-looks-once-per-direction-not-every-step
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [counts (atom [])
              raw-atom (atom nil)
              job {:round (fn ^:async twice [c]
                            (let [threats (danger-q/known-hostiles (:primitives c) 30 {:ranged-radius 30})]
                              (dotimes [_ 3]
                                (await (cover/flight-target! c threats))
                                (swap! counts conj (looks @raw-atom))
                                (fake/swap-self! @raw-atom update :pos #(update % 0 inc)))
                              (ctx/end c :done nil)))}
              s (setup-sensed {:blocks long-tunnel :entities [(zombie 7 3 {})]})
              _ (reset! raw-atom (:raw s))
              _ (fake/swap-self! (:raw s) update :pos assoc 0 8.5)
              s (assoc-in s [:eng :jobs 'twice] job)]
          (await (run-job! s 'twice {:step 6}))
          (is (pos? (first @counts)) "the first step looks at the walls")
          (is (= 1 (count (distinct @counts))) (str "later steps look at nothing again: " @counts)))))))

(deftest a-night-open-field-flight-with-unlit-cells-still-flees-far
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [raw seen out]} (await (run-job! (setup-sensed {:light-default [0 0] :entities [(zombie 7 5 {:chase {:speed 0.7}})]})
                                                      'test.flight {}))
              names (set (map #(.-name %) (.-calls (.-world raw))))]
          (is (< (first (body-pos raw)) -30) (str "fled far west: " (pr-str out)))
          (is (not (contains? names "attack")) "never fights")
          (is (empty? (filter #(= :retreat_blocked (:kind %)) @seen))))))))

(deftest a-silent-creeper-behind-the-fleeing-body-is-looked-back-at-before-the-flight-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [creeper {:id 9 :uuid "u9" :name "creeper" :kind "hostile" :pos {:x 5 :y 64 :z 0}
                       :chase {:speed 0.7 :follow 16}}
              {:keys [raw out]} (await (run-job! (setup-sensed {:entities [creeper]}) 'test.flight {}))]
          (is (= :done (:status out)) (pr-str out))
          (is (> (fake/dist (body-pos raw) (mob-pos raw 9)) 16)
              (str "the flight ends only once the creeper really stopped chasing, not while it follows unseen: "
                   (pr-str out) " body " (body-pos raw) " creeper " (mob-pos raw 9))))))))

(deftest short-flights-of-one-chain-add-up-from-the-chains-first-place
  (let [start {:x 0 :y 64 :z 0}]
    (is (= start (respond.state/chain-origin {:data {:from start}} {:x 10 :y 64 :z 0}))
        "a flight soon after another measures from where the chain began")
    (is (= {:x 10 :y 64 :z 0} (respond.state/chain-origin nil {:x 10 :y 64 :z 0}))
        "no earlier flight: from where this one began")))

(deftest short-flights-in-a-row-tell-the-agent-once-their-sum-is-far
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:entities [(zombie 7 4 {:chase {:speed 0.7 :follow 6}})]})
              fled (fn [r] (filterv #(= :hostile.fled (:kind %)) @(:seen r)))
              r1 (await (settings/with-settings {:jobs.survival.respond.state/far-flight 40}
                          #(run-job! s 'test.flight {})))
              n1 (count (fled r1))
              bp (body-pos (:p s))
              _ (swap! (fake/state (:p s)) update :entities (fn [es] (filterv #(not= 7 (:id %)) es)))
              _ (fake/add-entity! (:p s) (zombie 7 (+ (first bp) 4) {:chase {:speed 0.7 :follow 6}}))
              r2 (await (settings/with-settings {:jobs.survival.respond.state/far-flight 40}
                          #(run-job! r1 'test.flight {})))]
          (is (= 0 n1) "the first flight alone is under far-flight")
          (is (= 1 (count (fled r2))) "the second adds up past it, measured from the chain's start")
          (is (> (:distance (first (fled r2))) 40))
          (is (seq (mem/entries (mem/view (:store (:eng r2))) :displaced)) "the night job is told the body was driven off"))))))

(deftest a-far-flight-tells-the-agent-where-the-body-went-and-why
  (is (nil? (respond.state/fled-notice {:x 0 :y 64 :z 0} {:x -10 :y 64 :z 0} [{:mob "zombie" :id 7}] :far 16))
      "a short flight is no news")
  (is (nil? (respond.state/fled-notice nil {:x -10 :y 64 :z 0} [] :far 16)) "no start place: no news")
  (is (= {:from [0 64 0] :to [-40 64 3] :distance 40 :mobs ["zombie 7"] :ended :far
          :text "fled 40 blocks from zombie 7: [0 64 0] -> [-40 64 3]; it stopped chasing (far)"}
         (respond.state/fled-notice {:x 0.4 :y 64 :z 0.2} {:x -39.6 :y 64 :z 3.4} [{:mob "zombie" :id 7}] :far 16)))
  (are [ended tail] (= (str "fled 40 blocks from zombie 7: [0 64 0] -> [-40 64 3]; " tail)
                       (:text (respond.state/fled-notice {:x 0 :y 64 :z 0} {:x -40 :y 64 :z 3} [{:mob "zombie" :id 7}] ended 16)))
    :hidden "the body is hidden from it (hidden)"
    :keeps-off "it kept up, no ground was gained (keeps-off)"
    :cannot-escape "no way of escaping was left (cannot-escape)")
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (run-job! (setup {:entities [(zombie 7 5 {:chase {:speed 0.7}})]})
                                              'test.flight {}))
              fled (filterv #(= :hostile.fled (:kind %)) @seen)]
          (is (= 1 (count fled)) "one notice for the whole flight")
          (is (= :warn (:level (first fled))))
          (is (= ["zombie 7"] (:mobs (first fled))))
          (is (> (:distance (first fled)) 30)))))))



(deftest a-body-in-a-dead-end-backs-off-a-creeper-and-never-steps-towards-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (setup {:floor [-3 -1 9 1] :blocks dead-end :self {:pos {:x 3.5 :y 64 :z 0.5}}
                                        :entities [{:id 9 :uuid "u9" :name "creeper" :kind "hostile" :pos {:x 6.5 :y 64 :z 0.5} :chase {:speed 0.7}}]})
              xs (atom [])
              world (.-world p)
              _ (doseq [name ["steer" "moveTo" "wait" "attack" "place" "dig" "equip" "eat"]]
                  (.override world name (fn ^:async f [token a impl]
                                          (swap! xs conj (:x (body-pos p)))
                                          (await (impl token a)))))
              _ (await (run-job! s 'test.flight {}))
              _ (swap! xs conj (:x (body-pos p)))]
          (is (<= (apply max @xs) 3.6) (str "never east, towards the creeper: " (pr-str @xs)))
          (is (< (:x (body-pos p)) 3) "backed off west as far as the dead end allows"))))))

(deftest a-body-at-the-end-of-a-dead-end-with-a-creeper-beside-it-stays-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (setup {:floor [-3 -1 9 1] :blocks dead-end :self {:pos {:x 0.5 :y 64 :z 0.5}}
                                        :entities [{:id 9 :uuid "u9" :name "creeper" :kind "hostile" :pos {:x 2.5 :y 64 :z 0.5}}]})
              xs (atom [])
              world (.-world p)
              _ (doseq [name ["steer" "moveTo" "wait" "attack" "place" "dig" "equip" "eat"]]
                  (.override world name (fn ^:async f [token a impl]
                                          (swap! xs conj (:x (body-pos p)))
                                          (await (impl token a)))))
              _ (await (run-job! s 'test.flight {}))
              _ (swap! xs conj (:x (body-pos p)))]
          (is (<= (apply max @xs) 0.6) (str "never east, towards the creeper: " (pr-str @xs)))
          (is (<= (:x (body-pos p)) 0.6) "stays at the end"))))))
