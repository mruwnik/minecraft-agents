(ns engine.dusk-home-test
  "The :dusk-home trigger: before night, a body whose bed or home is a walk away leaves for it in time."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [engine.job-api :as job-api]
            [engine.shelter-support :as st]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [engine.hut-shelter-test :refer [hut-world]]
            [engine.test-util :as tu]
            [jobs.movement.go-to.cut :as gcut]
            [jobs.survival.go-home :as go-home]
            [triggers.survival.dusk-home :as dh]))

(defn holds?
  "The trigger over a fake body at (0 64 0) at time-of-day t, with places [[kind x z] ..] remembered."
  [t places & [over]]
  (let [store (mem/open (tu/tmp-dir) {:now (constantly 1000)})]
    (doseq [[kind x z y] places]
      (mem/write! store kind {:pos {:x x :y (or y 64) :z z}} mem/place-policy))
    (dh/dusk-home (tu/seeing-all (tu/fake-on-floor (merge {:floor [-4 -4 4 4] :time t :self {:pos {:x 0.5 :y 64 :z 0.5}}} over)))
                  (mem/view store) {})))

(deftest leaves-when-the-walk-takes-as-long-as-the-day-has-left
  (are [t places expected] (= expected (holds? t places))
    1000 [[:home 100 0]] false
    11000 [[:home 100 0]] true
    11000 [[:bed 100 0]] true
    11000 [[:home 20 0]] false
    11000 [[:home 150 0]] true
    11000 [[:home 300 0]] false
    11000 [[:home 900 0]] false
    11000 [] false))

(deftest it-does-not-fire-at-night-or-once-there
  (are [t places expected] (= expected (holds? t places))
    13000 [[:home 100 0]] false
    11000 [[:home 2 0]] false))

(def hut
  "A closed stone hut around the body at (0 64 0): 3x3 inside, roof at y 67."
  (apply dissoc (tu/box -2 64 -2 2 67 2 "stone") (keys (tu/box -1 64 -1 1 66 1 "stone"))))

(deftest a-body-underground-or-shut-in-a-hut-has-no-reason-to-go-home
  (are [blocks] (false? (holds? 11000 [[:home 100 0]] {:blocks blocks}))
    {"0,66,0" "stone"}
    hut))

(deftest a-body-under-a-lone-overhead-block-still-goes-home
  (is (true? (holds? 11000 [[:home 100 0]] {:floor [-20 -20 20 20] :blocks {"0,67,0" "stone"}}))))

(def stair-home-world
  "A roof over the body at x 0 and a home at x 60, y 70, up a stair from x 55: the body is in the ground (6 under home), with blocks to wall in."
  {:time 11500 :floor [-20 -20 120 20] :inventory [{:name "cobblestone" :count 64}]
   :blocks (into {"0,67,0" "stone"} (for [k (range 1 7)] [(str (+ 54 k) "," (+ 63 k) ",0") "stone"]))})

(defn ^:async go-home-from-the-ground
  "[blocks the body ends from the home at x, why go-home gave up, the jobs its :next names] with the home at x y (in world, default stair-home-world)."
  [x y args & [world]]
  (let [{:keys [eng p seen]} (st/setup (or world stair-home-world))]
    (mem/write! (:store eng) :home {:pos {:x x :y y :z 0}} mem/place-policy)
    (core/submit! eng (tu/order (list 'jobs.survival.go-home args)) {})
    (await (tu/run-until-empty eng 30))
    [(js/Math.abs (- x (.-x (.-pos (.self p))))) (some #(when (#{:completed :stopped} (:kind %)) (:reason %)) @seen)
     (some #(when (#{:completed :stopped} (:kind %)) (mapv :job (:next %))) @seen)]))

(deftest go-home-in-the-ground-stays-when-the-quoted-walk-there-and-back-costs-more-than-sheltering-here
  ;; the trigger fires for a buried body too; go-home, called with :commute?, prices go-to's own quote
  (is (true? (holds? 11500 [[:home 60 0 70]] stair-home-world)))
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far (await (go-home-from-the-ground 60 70 {:commute? true}))
              near (await (go-home-from-the-ground 6 64 {:commute? true}))
              plain (await (go-home-from-the-ground 60 70 {}))]
          (is (> (first far) 50) "the far home is not walked to")
          (is (= :shelter-cheaper (second far)))
          (is (= ["jobs.survival.night"] (nth far 2)) "the night job the reflex runs, not go-home again")
          (is (< (first near) 3) "a close home is walked to")
          (is (< (first plain) 3) "without :commute? the call walks home"))))))

(deftest go-home-in-the-ground-walks-when-nothing-can-shelter-the-body-where-it-stands
  ;; the stay is priced by shut-in's quote (no blocks, obsidian underfoot: no way), not a flat 12 s of sheltering
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (-> stair-home-world (assoc :inventory []) (update :blocks merge (tu/box -1 62 -1 1 63 1 "obsidian")))]
          (is (< (first (await (go-home-from-the-ground 60 70 {:commute? true} world))) 3) "the far home is walked to"))))))

(def lit-tunnel-world
  "The stair-home-world sealed in a stone shell, every cell lit by block light (nothing spawns anywhere the body can
  walk), with a dirt plug across the tunnel: the way out costs digging, which a worth of 0 never buys."
  (let [inner (tu/box -1 64 -1 62 73 1 "stone")]
    (-> stair-home-world
        (assoc :light {} :light-default [15 15])
        (update :blocks #(merge (apply dissoc (tu/box -2 64 -2 63 74 2 "stone") (keys inner)) (dissoc % "0,67,0") (tu/box 30 64 -1 30 73 1 "dirt"))))))

(deftest stuck-underground-climbs-out-of-a-lit-tunnel-at-its-own-worth
  ;; every neighbour cell seen and lit: the spot spawns nothing, so being stuck is the loss, not the night's spawns
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup lit-tunnel-world)
              _ (tu/seeing-all p)]
          (mem/write! (:store eng) :home {:pos {:x 60 :y 70 :z 0}} mem/place-policy)
          (core/submit! eng (tu/at-own-worth eng '(jobs.survival.go-home {:limit 1000})) {})
          (await (tu/run-until-empty eng 30))
          (is (< (js/Math.abs (- 60 (.-x (.-pos (.self p))))) 3) "the home up the stair is reached"))))))

(deftest go-home-walks-to-the-home-and-waits-with-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time st/noon :floor [-30 -30 30 30]})
              waiting (fn [world know]
                        (let [{:keys [eng]} (st/setup world)]
                          (when know (mem/write! (:store eng) :home {:pos {:x 12 :y 64 :z 0}} mem/place-policy))
                          (let [id (core/submit! eng (tu/order '(jobs.survival.go-home {})) {})]
                            (core/tick! eng)
                            (:reason (:waiting (job-api/summary eng id))))))]
          (is (= :no-home (waiting {:time st/noon} false)))
          (mem/write! (:store eng) :home {:pos {:x 12 :y 64 :z 0}} mem/place-policy)
          (core/submit! eng (tu/order '(jobs.survival.go-home {})) {})
          (await (tu/run-until-empty eng 30))
          (is (< (js/Math.abs (- 12 (.-x (.-pos (.self p))))) 3) "the body stands within 2 of the home"))))))

(deftest go-home-beyond-its-limit-waits-too-far
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup {:time st/noon :floor [-30 -30 30 30]})]
          (mem/write! (:store eng) :home {:pos {:x 300 :y 64 :z 0}} mem/place-policy)
          (do (core/submit! eng (tu/order '(jobs.survival.go-home {})) {})
            (await (tu/run-until-empty eng 10))
            (is (= [:too-far] (distinct (map :reason (filter #(= :waiting (:kind %)) @seen)))))
            (is (= 1 (count (:list (core/state eng)))) "the order waits in the list, once")))))))

(deftest go-home-arriving-at-the-bed-records-it-as-home
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (st/setup {:time st/noon :floor [-30 -30 30 30]})]
          (mem/write! (:store eng) :bed {:pos {:x 12 :y 64 :z 0}} mem/place-policy)
          (core/submit! eng (tu/order '(jobs.survival.go-home {})) {})
          (await (tu/run-until-empty eng 30))
          (is (= {:x 12 :y 64 :z 0} (mem/place (mem/view (:store eng)) :home))))))))

(deftest the-home-due-setting-retunes-the-trigger
  (is (true? (holds? 11000 [[:home 150 0]])))
  (settings/with-settings {:jobs.lib.shelter/home-due {:margin-s 0 :slack 0 :arrive 4}}
    #(is (false? (holds? 11000 [[:home 150 0]])))))

(defn sent-back-parent
  "A parent whose go-to child to x 12 was left open in this session after a cut found the body at x 9 (seed: the go-to's
  memory), the walk returned as it ends or waits."
  [seed]
  {:round (fn ^:async sent-back-round [c]
            (when (nil? (get-in (ctx/mem c) [:children :walk]))
              (ctx/update-mem! c assoc-in [:children :walk] (merge {:args {:pos [12 64 0]} :children {}} seed)))
            (let [r (await (child/call-walk* c :walk {:pos [12 64 0]}))]
              (if (= :continue r) r (ctx/end c :done nil))))})

(defn ^:async walk-out-at-dusk
  "[x the body ends at, the walk's wait, its seconds, the :sent-back entry] of a go-to from x 0 to x 12 at dusk, the
  home at x 2, no trigger armed; cut-at: the x where a cut found the walk, the body back home since, else nil."
  [cut-at]
  (let [{:keys [eng p]} (st/setup {:time 12000 :floor [-30 -30 30 30]})
        seed (when cut-at
               (let [d (js/Math.abs (- 12 cut-at))]
                 {:open (gcut/session-id) :goal {:x 12 :y 64 :z 0} :closest d :ref d :last-dist d :prev {:x cut-at :y 64 :z 0.5}}))
        eng (assoc eng :jobs (assoc (:jobs eng) 'sent-back-parent (sent-back-parent seed)))]
    (mem/write! (:store eng) :home {:pos {:x 2 :y 64 :z 0}} mem/place-policy)
    (let [id (core/submit! eng (tu/order '(sent-back-parent)) {})]
      (await (st/tick-n eng 20))
      [(:x (st/pos-of p)) (select-keys (core/waiting eng id) [:reason :wait]) (:wait-s (core/waiting eng id))
       (:data (mem/latest (mem/view (:store eng)) :sent-back))])))

(deftest a-walk-sent-back-home-at-dusk-waits-for-the-morning
  ;; the dusk-home trigger cut the walk at x 9 and walked the body home: walked again, it is cut again, so the job and
  ;; the trigger would hand the body to and fro; the walk learns it from its own cut, not from the trigger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[x w wait-s sent] (await (walk-out-at-dusk 9.5))]
          (is (< x 1) "the body stays home")
          (is (= {:reason :risky :wait :dawn} w))
          (is (> wait-s 500) "the night's 546 s come after the 27 s to nightfall")
          (is (= {:place :home :to {:x 0 :y 64 :z 0}} (select-keys sent [:place :to]))))
        (let [[x w _ sent] (await (walk-out-at-dusk -3.5))]
          ;; the way out of home first led away from the goal: home is nearer it than where the cut found the body
          (is (< x 1) "sent back nearer the goal, the body stays home")
          (is (= {:reason :risky :wait :dawn} w))
          (is (= :home (:place sent))))
        (let [[x _ _ sent] (await (walk-out-at-dusk nil))]
          (is (> x 10) "never sent back, the walk goes")
          (is (nil? sent)))))))

(def bed-hut
  "hut-shelter-test's shut-door hut (room x 4..6, z -1..1) with a bed in its north-west corner."
  {"4,64,-1" "red_bed"})

(deftest home-is-the-bed-s-closed-room-not-a-radius-round-it
  ;; outside the north wall 3 blocks from the bed is not home: go-home ended there and work beside it was cut every 2 s
  (are [pos places expected] (= expected (holds? 12000 places (assoc (hut-world pos bed-hut) :time 12000)))
    {:x 4 :y 64 :z -4} [[:bed 4 -1]] true
    {:x 5 :y 64 :z 0} [[:bed 4 -1]] false
    {:x 4 :y 64 :z -4} [[:bed 4 -6]] false))

(deftest go-home-walks-into-the-bed-s-room
  ;; from the north, within 2 of the bed is the far side of the wall; home is through the door, in the room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup (assoc (hut-world {:x 4 :y 64 :z -6} bed-hut) :time st/noon))]
          (mem/write! (:store eng) :bed {:pos {:x 4 :y 64 :z -1}} mem/place-policy)
          (core/submit! eng (tu/order '(jobs.survival.go-home {})) {})
          (await (tu/run-until-empty eng 30))
          (let [{:keys [x z]} (st/pos-of p)]
            (is (and (<= 4 x 7) (<= -1 z 2)) (str "inside the hut, at " x " " z))))))))

(deftest a-go-home-round-that-finds-home-past-its-limit-ends-too-far
  ;; Brindle S2: moved ~1100 blocks off mid-walk, go-home walked on toward the home it was given 160 blocks for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [store (mem/open (tu/tmp-dir) {:now (constantly 1000)})
              _ (mem/write! store :home {:pos {:x 90 :y 64 :z 0}} mem/place-policy)
              c {:primitives (tu/fake-on-floor {:floor [200 -5 230 5] :self {:pos {:x 220.5 :y 64 :z 0.5}}})
                 :args {:limit 100} :view (fn [] (mem/view store))}
              walks (atom 0)
              r (with-redefs [child/call-walk* (fn [& _] (swap! walks inc) (js/Promise.resolve :continue))
                              ctx/end (fn ([_ kind cause] [kind cause]) ([_ kind cause _] [kind cause]))]
                  (await (go-home/round c)))]
          (is (= [:condition-failed :too-far] r))
          (is (zero? @walks) "no step on toward the home"))))))
