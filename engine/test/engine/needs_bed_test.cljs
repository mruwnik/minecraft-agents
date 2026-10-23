(ns engine.needs-bed-test
  "The :needs-bed trigger: a body overdue for sleep with no bed gets one."
  (:require [cljs.test :refer [are deftest is]]
            [engine.memory :as mem]
            [engine.notes :as notes]
            [engine.test-util :as tu]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.world-files :as world]
            [triggers.survival.needs-bed :as nb]))

(def zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 8 :y 64 :z 0}})

(defn run
  "f over a fake world (night? and inv given), a view of the memory entries [[kind data age-days] ..] written age-days ago,
  the world's plans {id plan} and notes (another body's, live), and the listed jobs [[id label args contract] ..], queued in that order (a contract: started)."
  [f {:keys [night inv entries time entities blocks plans jobs] seen-notes :notes :or {entries []}}]
  (let [clock (atom 0)
        store (mem/open (tu/tmp-dir) {:now #(deref clock)})
        noted (notes/open {:world-dir (tu/tmp-dir) :body "Other" :emit (fn [_])})]
    (doseq [[kind data age] entries]
      (reset! clock (- (* 10 sh-clock/ms-per-day) (* age sh-clock/ms-per-day)))
      (mem/write! store kind data {:cap 8 :ttl (* 20 sh-clock/ms-per-day)}))
    (reset! clock (* 10 sh-clock/ms-per-day))
    (notes/add! noted @clock (mapv #(assoc % :by "Other" :t 1 :until 9e15) seen-notes))
    (doseq [[id label args contract] jobs]
      (swap! clock inc)
      (mem/create-job! store id label (cond-> args (not= "jobs.time.noop" label) (update :spend #(or % {:job-time 60}))))
      (when contract (mem/update-job! store id [] assoc :contract contract)))
    (f (tu/seeing-all (tu/fake-on-floor (cond-> {:floor [-10 -10 10 10] :inventory (or inv []) :entities entities :blocks blocks :time (or time 9000)} night (assoc :time 18000))))
       (mem/view store) {} (assoc (world/of-data (or plans {}) {}) :notes noted) (set (map first jobs)))))

(defn holds? [opts] (run nb/needs-bed opts))

(defn why [opts] (run (fn [p view & _] (nb/why p view)) opts))

(def slept-long-ago [[:slept {:pos {:x 0 :y 64 :z 0}} 4]])

(deftest an-overdue-body-with-no-bed-by-day-needs-one
  (is (true? (holds? {:entries slept-long-ago}))))

(deftest a-body-that-never-slept-counts-from-its-respawn-or-its-first-memory
  (is (true? (holds? {:entries [[:looked {} 4]]})))
  (is (true? (holds? {:entries [[:looked {} 4] [:respawned {} 3.5]]})))
  (is (false? (holds? {:entries [[:looked {} 4] [:respawned {} 1]]})))
  (is (false? (holds? {:entries [[:looked {} 4] [:deaths {:death-t (- (* 10 sh-clock/ms-per-day) sh-clock/ms-per-day)} 1]]}))))

(deftest a-rested-body-or-one-with-no-memory-does-not
  (is (false? (holds? {:entries [[:slept {:pos {:x 0 :y 64 :z 0}} 1] [:looked {} 4]]})))
  (is (false? (holds? {}))))

(deftest early-in-the-day-the-body-s-own-work-goes-first
  (is (false? (holds? {:entries slept-long-ago :time 3000})))
  (is (true? (holds? {:entries slept-long-ago :time (- sh-clock/dusk-tick 5000)})))
  (is (false? (holds? {:entries slept-long-ago :time sh-clock/dusk-tick}))))

(deftest at-night-the-night-job-sleeps-so-no-errand
  (is (false? (holds? {:entries slept-long-ago :night true}))))

(deftest first-light-or-a-known-hostile-hold-the-errand
  (is (false? (holds? {:entries slept-long-ago :time 400})))
  (is (false? (holds? {:entries slept-long-ago :entities [zombie]}))))

(deftest a-body-under-rock-does-not-leave-its-work-for-a-bed
  (is (false? (holds? {:entries slept-long-ago :blocks {"0,66,0" "stone"}}))))

(deftest a-carried-or-remembered-bed-needs-no-errand
  (is (false? (holds? {:entries slept-long-ago :inv [{:name "red_bed" :count 1}]})))
  (is (false? (holds? {:entries (conj slept-long-ago [:bed {:pos {:x 5 :y 64 :z 0}} 2]) :blocks {"5,64,0" "red_bed"}})))
  (is (true? (holds? {:entries (conj slept-long-ago [:bed {:pos {:x 5 :y 64 :z 0}} 2]) :blocks {"5,64,0" "air"}})) "a remembered bed seen gone is no bed"))

(def rested [[:slept {:pos {:x 0 :y 64 :z 0}} 1]])
(def spawn-bed-at-5 [:spawn-bed {:pos {:x 5 :y 64 :z 0}} 0.5])

(deftest a-rested-body-whose-respawn-bed-is-reset-or-lost-needs-a-bed
  (is (true? (holds? {:entries (conj rested spawn-bed-at-5 [:spawn-reset {} 0.2])})))
  (is (true? (holds? {:entries (conj rested spawn-bed-at-5 [:spawn-lost {:pos {:x 5 :y 64 :z 0}} 0.2])})))
  (is (false? (holds? {:entries (conj rested [:spawn-reset {} 0.8] spawn-bed-at-5) :blocks {"5,64,0" "red_bed"}}))))

(deftest a-respawn-bed-seen-gone-needs-a-bed-even-with-the-bed-place-remembered
  (let [entries (conj rested spawn-bed-at-5 [:bed {:pos {:x 5 :y 64 :z 0}} 0.5])]
    (is (true? (holds? {:entries entries :blocks {"5,64,0" "air"}})))
    (is (false? (holds? {:entries entries :blocks {"5,64,0" "red_bed"}})))))

(deftest a-standing-respawn-bed-counts-as-a-bed-though-the-bed-place-is-another
  (let [far-bed [:bed {:pos {:x 500 :y 64 :z 500}} 2]
        entries (conj slept-long-ago spawn-bed-at-5 far-bed)]
    (is (false? (holds? {:entries entries :blocks {"5,64,0" "red_bed"}})))
    (is (true? (holds? {:entries entries :blocks {"5,64,0" "air"}})))))

(deftest an-overdue-body-with-a-far-respawn-bed-still-needs-one
  (is (true? (holds? {:entries (conj slept-long-ago [:spawn-bed {:pos {:x 500 :y 64 :z 500}} 2])}))))

(deftest the-reason-it-fires-is-named
  (is (= {:why :unslept :days 4} (why {:entries slept-long-ago})))
  (is (= {:why :spawn-lost :pos {:x 5 :y 64 :z 0}}
         (why {:entries (conj rested spawn-bed-at-5 [:spawn-reset {} 0.2])})))
  (is (nil? (why {:entries rested}))))

(def hut {:id "hut" :parts [{:id "house" :cells [[3 64 3]] :want "oak_planks"}
                            {:id "bed" :cells [[4 64 3]] :want {:block "bed" :facing :east :part :foot}}]})
(def red-hut {:id "red-hut" :parts [{:id "bed" :cells [[4 64 3]] :want "red_bed[facing=east,part=foot]"}]})

(deftest a-listed-job-that-brings-a-bed-is-waited-for
  (are [jobs expected] (= expected (holds? {:entries slept-long-ago :plans {"hut" hut "red-hut" red-hut} :jobs jobs}))
    [["j1" "jobs.build.from-plan" {:plan "hut"}]] false
    [["j1" "jobs.build.from-plan" {:plan "hut" :part "bed"}]] false
    [["j1" "jobs.build.from-plan" {:plan "hut" :part "house"}]] true
    [["j1" "jobs.build.from-plan" {:plan "red-hut"}]] false
    [["j1" "jobs.items.obtain" {:any-of ["white_bed" "red_bed"]}]] false
    [["j1" "jobs.items.get-kit" {}]] true
    [] true))

(deftest a-bed-maker-that-starts-after-dusk-is-not-waited-for
  ;; at tick 9000 dusk is 177 s off
  (are [ahead expected] (= expected (holds? {:entries slept-long-ago :plans {"hut" hut} :jobs [ahead ["j2" "jobs.build.from-plan" {:plan "hut"}]]}))
    ["j1" "jobs.time.noop" {} {:call {:spend {:job-time 600}} :ran-ms 0}] true
    ["j1" "jobs.time.noop" {} {:call {:spend {:job-time 600}} :ran-ms 550000}] false
    ["j1" "jobs.time.noop" {:spend {:job-time 30}}] false
    ["j1" "jobs.time.noop" {}] true)
  (is (false? (holds? {:entries slept-long-ago :plans {"hut" hut}
                       :jobs [["j1" "jobs.build.from-plan" {:plan "hut"}] ["j2" "jobs.time.noop" {}]]}))
      "a job queued after the maker does not hold it up"))

(defn searched-for [what r]
  (for [x (range (- r) (inc r) 16) z (range (- r) (inc r) 16)] {:kind :searched :what [what] :pos [x 64 z] :r 24}))

(def sheep {:id 9 :name "sheep" :kind "passive" :pos {:x 6 :y 64 :z 0}})

(deftest land-searched-for-sheep-in-vain-holds-the-errand-till-wool-or-a-sheep-or-new-ground
  (are [opts expected] (= expected (holds? (merge {:entries slept-long-ago} opts)))
    {} true
    {:notes (searched-for "sheep" 32)} true
    {:notes (searched-for "sheep" 128)} false
    {:notes (searched-for "cow" 128)} true
    {:notes (map #(update % :pos (fn [[x y z]] [(+ x 1000) y z])) (searched-for "sheep" 128))} true
    {:notes (searched-for "sheep" 128) :entities [sheep]} true
    {:notes (searched-for "sheep" 128) :entities [(assoc sheep :baby true)]} false
    {:notes (searched-for "sheep" 128) :inv [{:name "red_wool" :count 3}]} true
    {:notes (searched-for "sheep" 128) :inv [{:name "white_wool" :count 2} {:name "string" :count 4}]} true
    {:notes (searched-for "sheep" 128) :inv [{:name "white_wool" :count 2} {:name "red_wool" :count 1}]} false))
