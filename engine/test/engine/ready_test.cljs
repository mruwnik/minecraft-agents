(ns engine.ready-test
  "jobs.lib.ready: the safe? and fit? checkers a job binds, as statuses over the shelter, the light and the mobs."
  (:require [cljs.test :refer [deftest is are]]
            [engine.hut-shelter-test :as hut]
            [engine.registry :as registry]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [jobs.debug.walk-plan :as walk-plan]
            [jobs.explore.look :as look]
            [jobs.explore.search :as search]
            [jobs.movement.look-around :as look-around]
            [jobs.access.stair :as stair]
            [jobs.blocks.fill-cells :as fill-cells]
            [jobs.blocks.place :as place]
            [jobs.lib.ready :as ready]
            [jobs.lib.world :as known]
            [jobs.strip.site :as strip-site]
            [jobs.movement.boat-drive :as boat-drive]
            [jobs.movement.follow :as follow]
            [jobs.movement.linger-near :as linger-near]
            [jobs.movement.mount :as mount]
            [jobs.items.kitchen :as kitchen]
            [jobs.items.pick-up-loose :as pick-up-loose]
            [jobs.items.smelt :as smelt]
            [jobs.storage.deposit :as deposit]
            [jobs.storage.kit :as storage-kit]
            [jobs.storage.withdraw :as withdraw]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.danger :as danger]
            [jobs.maintenance.shut-doors :as shut-doors]
            [jobs.survival.eat :as eat]
            [jobs.survival.get-food :as get-food]
            [jobs.survival.log-out :as log-out]
            [jobs.survival.recover-drops :as recover-drops]
            [jobs.survival.set-spawn :as set-spawn]
            [jobs.survival.restore-broken :as restore-broken]
            [jobs.time.noop :as noop]
            [jobs.time.wait-for-day :as wait-for-day]
            [jobs.time.wait-for-dusk :as wait-for-dusk]))

(defn ctx-of
  "A job's ctx over a fake body built from world (a fresh job id: the check cache is per job)."
  [world]
  (let [p (tu/seeing-all (tu/fake (merge {:floor tu/walk-floor} world)))]
    {:id (str (random-uuid)) :slots [] :engine {:warned (atom #{})} :primitives p :view (fn [] {:now 0 :data {}})}))

(def inside {:x 5 :y 64 :z 0})
(def outside {:x 15 :y 64 :z 0})
(def barn-roof (tu/box 14 67 -2 16 67 2 "oak_planks"))
(def night-hut (merge (hut/hut-world inside {}) {:time st/night}))
(def open-night {:time st/night :self {:pos {:x 0 :y 64 :z 0}}})
(def tunnel {:time st/night :self {:pos {:x 0 :y 64 :z 0}} :blocks (tu/box 0 66 -1 6 66 1 "stone")})

(defn lit
  "(f) with every cell's light read as block and sky (the fake world has no light data)."
  [block sky f]
  (with-redefs [sh-sense/block-light-at (constantly block) sh-sense/sky-light-at (constantly sky)] (f)))

(defn kind [check world] (first (check (ctx-of world))))
(defn reason [check world] (:reason (nth (check (ctx-of world)) 2)))

(deftest safe-at-open-ground-at-night-waits-for-dawn
  (let [check (ready/safe-at (constantly outside))
        s (lit 0 15 #(check (ctx-of open-night)))]
    (is (= :later (first s)))
    (is (= #{:dawn} (:lowered-by (nth s 2))))
    (is (re-find #"15 64 0" (second s)) "names the place")
    (is (re-find #"dawn" (second s)) "and what helps")))

(deftest safe-at-a-sheltered-place-is-ok-and-a-walk-out-of-the-hut-is-not
  (are [world to expected] (= expected (kind (ready/safe-at (constantly to)) world))
    night-hut {:x 6 :y 64 :z 0} :ok
    night-hut outside :later
    (assoc night-hut :blocks (merge (:blocks night-hut) barn-roof)) outside :later
    tunnel {:x 4 :y 64 :z 0} :ok
    (assoc open-night :time st/noon) {:x 4 :y 64 :z 0} :look))

(deftest safe-at-open-ground-by-day-is-ok-where-the-light-is-seen
  (is (= :ok (lit 0 15 #(kind (ready/safe-at (constantly outside)) (assoc open-night :time st/noon))))))

(deftest safe-here-by-night-is-ok-in-the-hut
  (is (= :ok (kind ready/safe-here-by-night night-hut))))

(deftest safe-here-by-night-outside-at-night-is-later
  (is (= :later (lit 0 15 #(kind ready/safe-here-by-night open-night)))))

(deftest safe-if-lit-waits-at-night-above-ground-even-from-a-hut
  (are [world expected] (= expected (kind ready/safe-if-lit world))
    night-hut :later
    tunnel :ok
    (assoc open-night :time st/noon) :ok))

(deftest a-job-that-goes-to-its-place-binds-safe-at-that-place
  (are [check args] (= :later (lit 0 15 #(first (check (assoc (ctx-of open-night) :args args)))))
    deposit/safe? {:chest outside}
    withdraw/safe? {:chest outside}
    storage-kit/safe? {:chest outside}
    smelt/safe? {:furnace outside}
    kitchen/safe? {:furnace outside}
    pick-up-loose/safe? {}
    set-spawn/safe? {:bed [15 64 0]}))

;; plain safe-at counts darkness by day too (it broke set-spawn); the debug probe wraps it, so it is not caught
(deftest no-job-binds-plain-safe-at
  (is (empty? (for [[j d] registry/jobs :when (::ready/plain (meta (:safe? d)))] j))))

(def zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}})
(def wall (tu/box 2 64 -2 2 66 2 "stone"))

(deftest threatened-is-later-for-a-reachable-zombie-and-ok-behind-a-wall
  (are [world expected] (= expected (kind ready/threatened world))
    {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie]} :later
    {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie] :blocks (merge wall (tu/box 2 67 -2 4 67 2 "stone") (tu/box 3 64 -2 4 66 -2 "stone") (tu/box 3 64 2 4 66 2 "stone") (tu/box 4 64 -2 4 66 2 "stone"))} :ok
    {:time st/noon :self {:pos {:x 0 :y 64 :z 0}}} :ok))

(deftest a-look-reads-without-moving-and-does-not-wait-for-a-zombie
  (is (= :ok (first (look/safe? (ctx-of {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie]}))))))

(deftest a-look-around-only-turns-and-does-not-wait-for-a-zombie
  (is (= :ok (first (look-around/safe? (ctx-of {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie]}))))))

(deftest pick-up-loose-waits-while-a-zombie-is-near
  (is (= :later (first (pick-up-loose/safe? (ctx-of {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie]}))))))

(def skeleton {:id 8 :name "skeleton" :kind "hostile" :pos {:x 12 :y 64 :z 0}})
(def at-origin {:time st/noon :self {:pos {:x 0 :y 64 :z 0}}})

(defn with-entry
  "ctx-of world, its engine holding a hostile-near register entry with args."
  [world args]
  (assoc-in (ctx-of world) [:engine :state] (atom {:register [{:id :hostile-near :trigger :hostile-near :args args}]})))

(deftest threatened-reads-the-hostile-near-entrys-args-and-the-triggers-options
  (are [world args expected] (= expected (first (ready/threatened (with-entry (merge at-origin world) args))))
    {:entities [zombie]} {} :later
    {:entities [zombie]} {:radius 2} :ok
    {:entities [(assoc skeleton :pos {:x 0 :y 64 :z 20})]} {} :ok
    {:entities [skeleton]} {} :later
    {:entities [skeleton]} {:ranged-radius 5} :ok))

(deftest threatened-keeps-the-triggers-visible-only-option
  (let [far (assoc zombie :pos {:x 5 :y 64 :z 0})
        behind (tu/box 2 64 0 3 66 0 "stone")]
    (are [args expected] (= expected (first (ready/threatened (with-entry (merge at-origin {:entities [far] :blocks behind}) args))))
      {} :ok
      {:visible-only false} :later)))

(deftest threatened-is-read-once-per-window
  (let [c (ctx-of (merge at-origin {:entities [zombie]}))
        asked (atom 0)]
    (with-redefs [danger/hostile-near? (fn [& _] (swap! asked inc) true)]
      (ready/threatened c)
      (ready/threatened c))
    (is (= 1 @asked))))

(deftest safe-at-adds-the-threat-to-the-exposure
  (is (= :later (kind (ready/safe-at (constantly {:x 1 :y 64 :z 0}))
                      {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie]}))))

(deftest fit-is-later-at-5-hp
  (are [hp expected] (= expected (kind ready/fit? {:self {:pos {:x 0 :y 64 :z 0} :health hp}}))
    5 :later
    20 :ok))

(deftest ok-is-always-ok
  (is (= :ok (kind ready/ok open-night))))

(deftest an-exposure-over-the-limit-is-judged-by-what-lowers-it
  (are [block sky world expected] (= expected (lit block sky #(kind (ready/safe-at (constantly outside)) world)))
    8 15 open-night :ok
    0 15 open-night :later
    0 0 (assoc open-night :time st/noon) :never))


(deftest an-unseen-cell-asks-for-a-look-at-it
  (let [s ((ready/safe-at (constantly outside)) (ctx-of open-night))]
    (is (= :look (first s)))
    (is (= [[15 64 0]] (:look-at (nth s 2))))))

(def escapes
  '[respond-to-hostile fight-back block-arrow-gap unwedge breathe extinguish night dig-niche retreat plug-in dig-in])

(def healers '[bed-down sleep go-home])

(deftest escapes-bind-ok-and-the-night-walkers-bind-threatened
  (doseq [j escapes :let [d (get registry/jobs (symbol (str "jobs.survival." j)))]]
    (is (= [ready/ok ready/ok] ((juxt :safe? :fit?) d)) (str j)))
  (doseq [j healers :let [d (get registry/jobs (symbol (str "jobs.survival." j)))]]
    (is (= [ready/threatened ready/ok] ((juxt :safe? :fit?) d)) (str j))))

(defn bound
  "The kind of a job's checker over world with args."
  [checker world args]
  (first (checker (assoc (ctx-of world) :args args))))

(deftest sweep-c-jobs-bind-the-checker-their-audit-row-names
  (are [checker world args expected] (= expected (lit 0 15 #(bound checker world args)))
    search/safe? open-night {} :later
    follow/safe? open-night {} :later
    boat-drive/safe? open-night {:pos [15 64 0]} :later
    boat-drive/safe? (assoc open-night :time st/noon) {:pos [15 64 0]} :ok
    walk-plan/safe? open-night {:to [15 64 0]} :later
    linger-near/safe? open-night {:pos [15 64 0]} :later
    mount/safe? open-night {} :later
    mount/safe? (assoc open-night :time st/noon) {} :ok
    search/fit? {:self {:pos {:x 0 :y 64 :z 0} :health 5}} {} :later
    follow/fit? {:self {:pos {:x 0 :y 64 :z 0} :health 5}} {} :later
    linger-near/fit? {:self {:pos {:x 0 :y 64 :z 0} :health 5}} {} :later
    mount/fit? {:self {:pos {:x 0 :y 64 :z 0} :health 5}} {} :ok))

(deftest safe-at-by-night-waits-for-an-unseen-place-only-at-night
  (are [world expected] (= expected (kind (ready/safe-at-by-night (constantly outside)) world))
    (assoc open-night :time st/noon) :ok
    night-hut :later))

(deftest safe-at-by-night-judges-the-goal-of-a-buried-body
  (are [goal expected] (= expected (kind (ready/safe-at-by-night (constantly goal)) tunnel))
    {:x 4 :y 64 :z 0} :ok
    nil :ok
    outside :later))

(deftest safe-at-by-night-judges-a-column-goal-at-its-surface
  (is (= :never (with-redefs [sh-sense/block-light-at (constantly 0)] (kind (ready/safe-at-by-night (constantly {:x 4 :z 0})) tunnel)))))

(deftest explore-search-underground-at-night-is-ok
  (is (= :ok (bound search/safe? tunnel {}))))

(deftest the-rest-of-survival-binds-its-checkers
  (let [by-day-hostile {:time st/noon :self {:pos {:x 0 :y 64 :z 0}} :entities [zombie]}]
    (are [check world expected] (= expected (kind check world))
      restore-broken/safe? open-night :later
      restore-broken/safe? (assoc open-night :time st/noon) :ok
      restore-broken/fit? {:self {:pos {:x 0 :y 64 :z 0} :health 5}} :later
      get-food/safe? by-day-hostile :ok
      get-food/safe? open-night :ok
      recover-drops/safe? by-day-hostile :ok
      recover-drops/safe? open-night :ok
      get-food/fit? {:self {:pos {:x 0 :y 64 :z 0} :health 5}} :ok
      shut-doors/safe? by-day-hostile :later
      shut-doors/safe? open-night :ok)))

(deftest jobs-with-nothing-to-check-are-ok-at-night-with-a-hostile-and-hurt
  (let [world (assoc open-night :entities [zombie] :self {:pos {:x 0 :y 64 :z 0} :health 5})]
    (doseq [check [log-out/safe? log-out/fit? eat/safe? eat/fit? noop/safe? noop/fit? wait-for-day/safe? wait-for-day/fit?
                   wait-for-dusk/safe? wait-for-dusk/fit? shut-doors/fit?]]
      (is (= :ok (kind check world))))))

(deftest the-work-jobs-bind-their-place-by-night
  (are [world job args expected] (= expected (lit 0 15 #(first ((:safe? job) (assoc (ctx-of world) :args args)))))
    open-night {:safe? place/safe?} {:pos [15 64 0]} :later
    night-hut {:safe? place/safe?} {:pos [6 64 0]} :ok
    open-night {:safe? fill-cells/safe?} {:cells [[15 64 0]]} :later
    night-hut {:safe? fill-cells/safe?} {:cells [[6 64 0]]} :ok
    open-night {:safe? stair/safe?} {:from {:x 0 :y 64 :z 0}} :later
    night-hut {:safe? stair/safe?} {} :ok))

(defn jobs-under
  "The registry's jobs in the namespaces under the prefixes, but find-spot (which roams)."
  [& prefixes]
  (for [j (sort (keys registry/jobs))
        :when (some #(.startsWith (str j) %) prefixes)
        :when (not= j 'jobs.farm.find-spot)]
    j))

(def work-jobs (jobs-under "jobs.farm." "jobs.build."))

(defn safe-of [job] (:safe? (get registry/jobs job)))

(def plans
  {"far" {:id "far" :cells [{:pos [15 64 0] :part "p"}]}
   "near" {:id "near" :cells [{:pos [6 64 0] :part "p"}]}
   "pen-far" {:id "pen-far" :cells [] :plan {:parts [{:id "p" :box [[14 64 -1] [16 64 1]] :herd {:mob "cow"}}]}}
   "pen-near" {:id "pen-near" :cells [] :plan {:parts [{:id "p" :box [[5 64 -1] [7 64 1]] :herd {:mob "cow"}}]}}})

(def box-at (fn [x] {:min {:x (dec x) :y 64 :z -1} :max {:x (inc x) :y 64 :z 1}}))
(def corners-at (fn [x] {:from {:x (dec x) :y 64 :z -1} :to {:x (inc x) :y 64 :z 1}}))

(deftest work-jobs-bind-the-place-their-args-name-by-night
  (with-redefs [known/plan (fn [_ id] (get plans id))
                strip-site/site (constantly {:at [15 64 0]})]
    (are [job args world expected] (= expected (lit 0 15 #(bound (safe-of job) world args)))
      'jobs.animals.cull {:box (box-at 15)} night-hut :later
      'jobs.animals.cull {:box (box-at 5)} night-hut :ok
      'jobs.animals.cull {:center {:x 15 :z 0}} night-hut :later
      'jobs.animals.tend {:box (box-at 15)} night-hut :later
      'jobs.animals.tend {:box (box-at 5)} night-hut :ok
      'jobs.animals.tend {:plan "pen-far" :part "p"} night-hut :later
      'jobs.animals.tend {:plan "pen-near" :part "p"} night-hut :ok
      'jobs.animals.pen-check {:box (box-at 15)} night-hut :later
      'jobs.animals.pen-check {:plan "far"} night-hut :later
      'jobs.animals.pen-check {:plan "near"} night-hut :ok
      'jobs.animals.pen-check {:at [15 64 0]} night-hut :later
      'jobs.animals.pen-check {:at [6 64 0] :box (box-at 15)} night-hut :ok
      'jobs.apiary.guard {:box (corners-at 15)} night-hut :later
      'jobs.apiary.maintain {:box (corners-at 5)} night-hut :ok
      'jobs.apiary.harvest {:center [15 64 0]} night-hut :later
      'jobs.village.maintain {:plan "far"} night-hut :later
      'jobs.village.maintain {:plan "near"} night-hut :ok
      'jobs.village.maintain {:plan "gone"} night-hut :ok
      'jobs.strip.site {:site "t" :base "b"} night-hut :later
      'jobs.food.fish {:pos [15 64 0]} night-hut :later
      'jobs.food.fish {:pos [6 64 0]} night-hut :ok)))

(deftest farm-and-build-jobs-bind-the-place-of-their-args
  (doseq [job work-jobs]
    (are [args world expected] (= expected (lit 0 15 #(bound (safe-of job) world args)))
      {:at [15 64 0]} night-hut :later
      {:at [6 64 0]} night-hut :ok)))

(deftest roaming-jobs-wait-for-dawn-above-ground
  (are [job args] (= :later (lit 0 15 #(bound (safe-of job) night-hut args)))
    'jobs.farm.find-spot {}
    'jobs.food.fish {}))

(deftest jobs-that-go-to-a-place-keep-its-exposure-for-exposed-checks
  (doseq [job (concat work-jobs (jobs-under "jobs.animals." "jobs.apiary." "jobs.village." "jobs.strip." "jobs.food.")
                      '[jobs.farm.find-spot])]
    (is (contains? (meta (safe-of job)) ::ready/exposure) (str job))))
