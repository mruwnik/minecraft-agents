(ns engine.explore-search-test
  "jobs.explore.search against the fake world, with a notes store over a temp world folder."
  (:require [cljs.test :refer [deftest is are async]]
            ["fs" :as fs]
            ["path" :as path]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.events :as events]
            [engine.expr :as expr]
            [engine.memory :as mem]
            [engine.notes :as notes]
            [engine.registry :as registry]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.threats :as threats]
            [jobs.lib.world-files :as world]
            [jobs.lib.child :as child]
            [jobs.explore.search :as search]))

(def ground
  "Grass at y 63 under every column within r of the origin (r a little past the search's :max-distance): every leg the
  patterns choose from the origin stands there, and the planner has a floor to walk over between them."
  (memoize (fn [r] (tu/box (- r) 63 (- r) r 63 r "grass_block"))))

(defn setup
  "An engine over the fake world spec (ground of radius ground-r added, default 64) with a notes store for body Fake in a temp world folder; a :wait act moves the clock by its ms."
  [spec & [ground-r]]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        dir (tu/tmp-dir)
        p (tu/fake (update (dissoc spec :cone? :zones) :blocks #(merge (ground (or ground-r 64)) %)))
        _ (.override (.-world p) "wait" (fn [token a impl] (swap! clock + (.-ms a)) (impl token a)))
        p (if (:cone? spec)
            (let [w (perception/wrap p (perception/create (fake-raw/create p) {:radius 16 :ray-deg 2}))]
              (.setOwner p "Fake")
              w)
            (tu/seeing-all p))
        store (notes/open {:world-dir dir :body "Fake" :emit (fn [_])})
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (assoc (world/of-data {} {} (:zones spec [])) :notes store)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock :dir dir :store store}))

(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))

(defn ^:async run-until-done
  "Tick 700 ms apart until the list is empty or n ticks have run."
  [{:keys [eng clock] :as s} n]
  (loop [i 0]
    (when (and (< i n) (not (finished? s)))
      (swap! clock + 700)
      (await (core/tick! eng))
      (recur (inc i))))
  s)

(defn ^:async search!
  [args spec & [n before]]
  (let [s (setup spec (some-> (:max-distance args) (+ 8)))]
    (when before (before s))
    (core/submit! (:eng s) (list 'jobs.explore.search args) {})
    (await (run-until-done s (or n 400)))))

(defn event-of [{:keys [seen]} kind] (first (filter #(= kind (:kind %)) @seen)))
(defn walks
  "The targets of the walks go-to made, in order (each walk writes a :moved entry)."
  [{:keys [eng]}]
  (mapv (comp :target :data) (mem/entries (mem/view (:store eng)) :moved)))
(defn file-notes [{:keys [dir]}]
  (:value (notes/parse-file (fs/readFileSync (path/join dir "notes" "Fake.edn") "utf8"))))
(defn write-other! [{:keys [dir]} body ns]
  (fs/mkdirSync (path/join dir "notes") #js {:recursive true})
  (fs/writeFileSync (path/join dir "notes" (str body ".edn")) (notes/render body ns)))

(def diamond {"45,64,3" "diamond_block"})

;; ------------------------------------------------------------------ pure

(deftest the-spiral-rings-the-origin-at-spacing-within-the-distance
  (is (= [[-16 -16] [0 -16] [16 -16] [16 0] [16 16] [0 16] [-16 16] [-16 0]]
         (search/spiral-points [0 0] 16 23)))
  (is (= [[0 -16] [16 0] [0 16] [-16 0]] (search/spiral-points [0 0] 16 20)))
  (is (= 24 (count (search/spiral-points [100 100] 16 46)))))

(deftest outward-goes-ahead-first-then-to-the-sides
  (is (= [[0 -16] [8 -12] [-8 -12] [12 -4] [-12 -4] [16 0] [-16 0]]
         (search/outward-points [0.5 0.5] :north 16)))
  (is (= [16 0] (first (search/outward-points [0 0] :east 16)))))

(defn column [m] (fn [[x y z]] (if (contains? m :unloaded) nil (get m [x y z] "air"))))

(deftest a-leg-stands-on-the-ground-not-the-canopy-and-never-in-the-unknown
  (are [m expected] (= expected (search/stand-cell (column m) 5 5 64))
    {[5 63 5] "grass_block"} {:y 64}
    {[5 70 5] "stone" [5 69 5] "stone"} {:y 71}
    {[5 66 5] "oak_leaves" [5 63 5] "dirt"} {:y 64}
    {[5 63 5] "water"} {:fail :wet}
    {[5 63 5] "dirt" [5 64 5] "oak_log" [5 65 5] "oak_log" [5 66 5] "oak_leaves"} {:fail :no-surface}
    {[5 43 5] "dirt"} {:y 44}
    {[5 66 5] "jungle_leaves" [5 30 5] "dirt"} {:y 31}
    {[5 66 5] "jungle_leaves" [5 20 5] "dirt"} {:fail :no-surface}
    {} {:fail :no-surface}
    {:unloaded true} {:fail :not-loaded}))

(deftest a-column-out-of-view-is-unseen-and-the-search-steps-toward-it
  (let [seen (fn [[x y z]] (cond (> x 8) :unseen (= y 63) "grass_block" :else "air"))]
    (is (= {:fail :unseen} (search/stand-cell seen 20 0 64)))
    (is (= [6 64 0] (search/step-toward seen [0 64 0] 20 0)) "12 is out of view, so the 6 block step")
    (is (= [6 64 0] (search/step-toward (fn [_] :unseen) [0 64 0] 20 0)) "all unseen: 6 blocks at feet height")
    (is (nil? (search/step-toward (fn [_] :unseen) [0 64 0] 3 0)))))

(deftest ore-beyond-the-view-is-walked-toward-and-found-after-looking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 48} {:blocks {"40,64,0" "diamond_block"} :cone? true}))
              e (event-of s :search.done)]
          (is (= nil (event-of s :search.not-found)))
          (is (= [{:what "diamond_block" :pos [40 64 0]}] (:found e)))
          (is (seq (walks s))))))))

;; ------------------------------------------------------------------ the job

(deftest a-target-in-sight-is-found-without-walking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block"} {:blocks {"10,64,0" "diamond_block"}}))]
          (is (finished? s))
          (is (= :found (:reason (event-of s :search.done))))
          (is (= [{:what "diamond_block" :pos [10 64 0]}] (:found (event-of s :search.done))))
          (is (= [] (walks s))))))))

(def two-diamonds {"10,64,0" "diamond_block" "0,64,12" "diamond_block"})

(deftest a-target-in-an-avoided-column-is-not-found
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :avoid [[10 0]]} {:blocks two-diamonds}))]
          (is (= [[0 64 12]] (map :pos (:found (event-of s :search.done))))))))))

(deftest a-target-in-an-avoided-box-is-not-found
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :avoid-boxes [[[8 -3] [14 3]]]} {:blocks two-diamonds}))]
          (is (= [[0 64 12]] (map :pos (:found (event-of s :search.done))))))))))

(deftest a-target-in-a-foreign-zone-is-found-only-without-takeable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [vault {:name "vault" :min [8 60 -2] :max [12 70 2] :owner "Miles"}
              spec {:blocks two-diamonds :zones [vault]}
              any (await (search! {:target "diamond_block"} spec))
              own (await (search! {:target "diamond_block" :takeable true} spec))]
          (is (= [[10 64 0]] (map :pos (:found (event-of any :search.done)))))
          (is (= [[0 64 12]] (map :pos (:found (event-of own :search.done))))))))))

(deftest a-target-out-of-sight-is-walked-to-found-and-noted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block"} {:blocks diamond}))
              ns (file-notes s)]
          (is (finished? s))
          (is (= :found (:reason (event-of s :search.done))))
          (is (= [[45 64 3]] (map :pos (:found (event-of s :search.done)))))
          (is (pos? (count (walks s))))
          (is (some #(= [:seen "diamond_block" [45 64 3] "Fake"] ((juxt :kind :what :pos :by) %)) ns))
          (is (some #(= [:searched ["diamond_block"]] ((juxt :kind :what) %)) ns))
          (is (nil? (event-of s :search.not-found))))))))

(deftest the-whole-search-is-one-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:blocks diamond})]
          (core/submit! (:eng s) (list 'jobs.explore.search {:target "diamond_block"}) {})
          (swap! (:clock s) + 700)
          (await (core/tick! (:eng s)))
          (is (finished? s) "legs walked and the ore found in one round")
          (is (= :found (:reason (event-of s :search.done)))))))))

(deftest an-entity-target-is-found-and-noted-by-its-uuid
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target ["cow" "pig"]}
                                {:entities [{:id 3 :uuid "u3" :name "cow" :kind "passive" :pos {:x 30.5 :y 64 :z 0.5}}]}))]
          (is (= [{:what "cow" :pos [30 64 0] :id "u3"}] (:found (event-of s :search.done))))
          (is (some #(= [:seen "cow" "u3"] ((juxt :kind :what :id) %)) (file-notes s))))))))

(deftest nothing-within-the-distance-ends-not-found-with-the-coverage
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20} {:blocks diamond}))
              e (event-of s :search.not-found)]
          (is (finished? s))
          (is (= :not-found (:reason e)))
          (is (= :distance (:why e)))
          (is (= 4 (get-in e [:coverage :legs])))
          (is (= 5 (get-in e [:coverage :scans])))
          (is (<= 14 (get-in e [:coverage :farthest]) 16) "the body stops within 2 of the leg 16 out (go-to ends on any cell within its range, so 14 or 15)")
          (is (= [] (:found e))))))))

(deftest the-leg-and-time-bounds-end-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [legs (await (search! {:target "diamond_block" :max-legs 2} {}))
              timed (await (search! {:target "diamond_block" :timeout-s 2} {} nil
                                    (fn [{:keys [p clock]}]
                                      (.override (.-world p) "look"
                                                 (fn ^:async f [token args impl]
                                                   (swap! clock + 1000)
                                                   (await (impl token args)))))))]
          (is (= [:legs 2] ((juxt :why #(get-in % [:coverage :legs])) (event-of legs :search.not-found))))
          (is (= :time (:why (event-of timed :search.not-found)))))))))

(deftest ground-another-body-searched-for-these-targets-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [marks (for [[x z] (search/spiral-points [0 0] 16 23)]
                      {:kind :searched :what ["diamond_block"] :pos [x 64 z] :r 24 :by "Other" :t 1 :until 9e15})
              s (await (search! {:target "diamond_block" :max-distance 23} {} 400 #(write-other! % "Other" marks)))
              e (event-of s :search.not-found)]
          (is (= [] (walks s)))
          (is (= 8 (get-in e [:coverage :skipped])))
          (is (= :distance (:why e))))))))

(deftest an-outward-search-goes-on-past-searched-ground-ahead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [marks (for [[x z] (search/outward-points [0.5 0.5] :north 16)]
                      {:kind :searched :what ["diamond_block"] :pos [x 64 z] :r 3 :by "Other" :t 1 :until 9e15})
              s (await (search! {:target "diamond_block" :pattern :outward :heading :north :max-distance 64} {} 400 #(write-other! % "Other" marks)))
              e (event-of s :search.not-found)]
          (is (pos? (count (walks s))) "the body walked on instead of ending at once")
          (is (every? #(< (:z %) -16) (walks s)) "past the searched legs, further north")
          (is (pos? (get-in e [:coverage :legs]))))))))

(deftest ground-searched-for-other-names-is-not-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [marks [{:kind :searched :what ["oak_log"] :pos [0 64 -16] :r 24 :by "Other" :t 1 :until 9e15}]
              s (await (search! {:target "diamond_block" :max-distance 20} {} 400 #(write-other! % "Other" marks)))]
          (is (= 4 (count (walks s)))))))))

(deftest a-target-another-body-noted-counts-unless-told-not-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen [{:kind :seen :what "diamond_block" :pos [45 64 3] :by "Other" :t 1 :until 9e15}]
              s (await (search! {:target "diamond_block"} {:cone? true :blocks diamond} 400 #(write-other! % "Other" seen)))
              own (await (search! {:target "diamond_block" :use-notes false :max-distance 20} {:cone? true :blocks diamond} 400
                                  #(write-other! % "Other" seen)))]
          (is (= [{:what "diamond_block" :pos [45 64 3] :noted true :by "Other"}] (:found (event-of s :search.done))))
          (is (= [] (walks s)))
          (is (= :not-found (:reason (event-of own :search.not-found)))))))))

(deftest a-noted-animal-in-the-bodys-own-zone-is-no-find-one-outside-is-and-the-text-gives-where
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [note (fn [pos] [{:kind :seen :what "cow" :id "u1" :pos pos :by "Other" :t 1 :until 9e15}])
              farm {:name "farm" :owner "Fake" :min [0 60 0] :max [10 70 10]}
              run! (fn ^:async f [pos] (await (search! {:target "cow" :max-distance 20 :max-legs 0} {:zones [farm]} 20
                                                       #(write-other! % "Other" (note pos)))))
              penned (await (run! [3 64 3]))
              wild (await (run! [12 64 12]))]
          (is (= :not-found (:reason (event-of penned :search.not-found))) "the penned cow is not counted")
          (is (re-find #"12,64,12" (str (:text (event-of wild :search.done)))) "the text says where"))))))

(deftest legs-that-cannot-be-stood-on-or-walked-are-recorded-and-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20}
                                {:unloaded ["0,63,-16"] :unreachable ["16,64,0"]}))
              failed (get-in (event-of s :search.not-found) [:coverage :failed])]
          (is (= #{{:pos [0 64 -16] :reason :not-loaded} {:pos [16 64 0] :reason :unreachable}} (set failed)))
          (is (not-any? #(= {:x 0 :z -16} (select-keys % [:x :z])) (walks s)) "an unloaded column is never walked to"))))))

(def ring-1-cells (mapv (fn [[x z]] (str x ",63," z)) (search/spiral-points [0 0] 16 23)))

(deftest a-search-started-before-the-ground-around-loaded-waits-for-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {:unloaded ring-1-cells} 28)]
          (core/submit! (:eng s) '(jobs.explore.search {:target "diamond_block" :max-distance 20}) {})
          (dotimes [_ 8] (swap! (:clock s) + 700) (await (core/tick! (:eng s))))
          (is (not (finished? s)) "still waiting for the chunks")
          (is (= [] (walks s)))
          (swap! (fake/state (:p s)) assoc :unloaded #{})
          (await (run-until-done s 400))
          (is (= 4 (count (walks s))))
          (is (= :distance (:why (event-of s :search.not-found)))))))))

(deftest ground-that-never-loads-ends-it-not-loaded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20} {:unloaded ring-1-cells}))
              e (event-of s :search.not-found)]
          (is (= :not-loaded (:why e)))
          (is (<= (get-in e [:coverage :scans]) 17) "a look every 2 s while waiting, not every tick")
          (is (= 4 (count (get-in e [:coverage :failed]))))
          (is (= [] (walks s))))))))

;; a leg whose goal lies by a mob the body would flee is not walked: go-to gives it up (:goal-danger :end), it is recorded
;; :goal-dangerous and does not count as a stuck walk
(deftest a-leg-by-a-dangerous-mob-is-recorded-and-not-walked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zombie {:key "z1" :name "zombie" :pos {:x 17 :y 64 :z 1}}
              s (with-redefs [threats/sensed-mobs (constantly [zombie])]
                  (await (search! {:target "diamond_block" :max-distance 20} {})))
              e (event-of s :search.not-found)]
          (is (= :distance (:why e)) (pr-str e))
          (is (some #{{:pos [16 64 0] :reason :goal-dangerous}} (get-in e [:coverage :failed])) (pr-str (:coverage e)))
          (is (not-any? #(= {:x 16 :z 0} (select-keys % [:x :z])) (walks s)) "the leg by the zombie is never walked to")
          (is (= 3 (count (walks s)))))))))

;; a step toward an unseen column whose step point lies by a dangerous mob is given up once: that column is tried, the
;; search goes on with others and never gives the same step up again
(deftest a-dangerous-step-toward-an-unseen-column-is-given-up-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zombie {:key "z1" :name "zombie" :pos {:x 1 :y 64 :z -7}}
              far? (fn [pos] (or (> (js/Math.abs (.-x pos)) 8) (> (js/Math.abs (.-z pos)) 8)))
              blind-far (fn [{:keys [p]}]
                          (let [at (.-sensedAt p)]
                            (aset p "sensedAt" (fn [pos] (if (far? pos) #js {:unknown true} (at pos))))))
              s (with-redefs [threats/sensed-mobs (constantly [zombie])]
                  (await (search! {:target "diamond_block" :max-distance 20} {} 80 blind-far)))
              given-up (filterv #(= :goal-dangerous (:reason %)) (get-in (or (event-of s :search.not-found) (event-of s :search.done)) [:coverage :failed]))]
          (is (finished? s) "the search ends")
          (is (= [{:pos [0 64 -6] :reason :goal-dangerous}] given-up) (pr-str (event-of s :search.not-found) (walks s)))
          (is (not-any? #(= {:x 0 :z -6} (select-keys % [:x :z])) (walks s)) "the step by the zombie is never walked to"))))))

(deftest a-step-toward-an-unseen-column-that-fails-is-not-repeated
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [blind-far (fn [{:keys [p]}]
                          (let [at (.-sensedAt p)]
                            (aset p "sensedAt" (fn [pos] (if (or (> (js/Math.abs (.-x pos)) 8) (> (js/Math.abs (.-z pos)) 8)) #js {:unknown true} (at pos))))))
              s (await (search! {:target "diamond_block" :max-distance 20}
                                {:self {:pos [0 100 0]} :blocks (tu/box -2 99 -2 8 99 2 "stone")} 60 blind-far))
              e (event-of s :search.not-found)]
          (is (finished? s) "the search ends within 60 rounds, not at the time bound")
          (is (not= :time (:why e)))
          (let [ps (map :pos (get-in e [:coverage :failed]))]
            (is (= (count ps) (count (set ps))) "a column whose step failed is not stepped toward again")))))))

(deftest the-last-leg-before-stuck-digs-out-of-a-sealed-pocket
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [room (apply dissoc (tu/box -3 64 -3 3 66 3 "dirt") (for [x (range -2 3) z (range -2 3) y [64 65]] (str x "," y "," z)))
              s (await (search! {:target "diamond_block" :max-distance 23} {:blocks room} 200))]
          (is (some #(= :go-to.escalated (:kind %)) @(:seen s))))))))

(deftest an-interrupted-walk-is-not-a-verdict-on-its-leg
  (are [res expected] (= expected (search/interrupted? res))
    {:reason :cut-again} true
    {:reason :unreachable :why :moved-while-searching} true
    {:reason :unreachable :why :goal-not-standable} false
    {:arrived true} false))

(deftest time-a-leg-spends-waiting-for-day-in-a-shelter-does-not-count-toward-the-timeout
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom 0)
              real child/call-walk-reason*
              night (fn ^:async night-walk [c slot args]
                      (if (< (swap! calls inc) 150)
                        [:continue :night-sheltered]
                        (await (real c slot args))))
              s (with-redefs [child/call-walk-reason* night]
                  (await (search! {:target "diamond_block" :max-distance 20 :timeout-s 60} {} 400)))
              e (event-of s :search.not-found)]
          (is (finished? s))
          (is (not= :time (:why e)) (:text e)))))))

(deftest a-plain-search-still-ends-at-the-timeout
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (with-redefs [child/call-walk-reason* (fn [_ _ _] (js/Promise.resolve [:continue nil]))]
                  (await (search! {:target "diamond_block" :max-distance 20 :timeout-s 60} {} 400)))
              e (event-of s :search.not-found)]
          (is (= :time (:why e)))
          (is (re-find #":spend :time" (:text e))))))))

(deftest three-failed-legs-in-a-row-end-it-stuck
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 23} {:unreachable (mapv (fn [[x z]] (str x ",64," z)) (search/spiral-points [0 0] 16 23))}))
              e (event-of s :search.not-found)]
          (is (= :stuck (:why e)))
          (is (= [[-16 64 -16] [16 64 -16] [16 64 16]] (mapv :pos (get-in e [:coverage :failed])))
              "each next leg lies another way: one within 60 degrees of a failed leg's way is held back"))))))

;; live (Juniper, card e51d1fe9): three legs east all failed at the same rim and the search ended stuck; a leg that lies
;; the way a leg just failed from here waits until a leg arrives, so the next leg goes another way
(deftest a-leg-the-way-a-leg-just-failed-waits-for-one-that-arrives
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 23} {:unreachable ["-16,64,-16" "0,64,-16" "16,64,-16"]}))
              e (event-of s :search.not-found)]
          (is (= :distance (:why e)) (pr-str e))
          (is (= #{[-16 64 -16] [0 64 -16] [16 64 -16]} (set (map :pos (get-in e [:coverage :failed])))) "the held leg is walked later, after an arrival"))))))

;; only held legs left (all candidates lie the way the one failed leg went): they are tried, so the search ends stuck
;; only after its failures in a row
(deftest only-held-legs-left-are-tried-after-one-failed-leg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [marks (for [[x z] [[16 -16] [16 0] [16 16] [0 16] [-16 16]]]
                      {:kind :searched :what ["diamond_block"] :pos [x 64 z] :r 3 :by "Other" :t 1 :until 9e15})
              s (await (search! {:target "diamond_block" :max-distance 23} {:unreachable ["-16,64,-16"]} 400 #(write-other! % "Other" marks)))
              e (event-of s :search.not-found)]
          (is (= :distance (:why e)) (pr-str e))
          (is (some #(= {:x 0 :z -16} (select-keys % [:x :z])) (walks s)) "the held legs are walked after the one failure"))))))

(deftest outward-walks-along-the-heading
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :pattern :outward :heading :east :max-distance 40} {}))]
          (is (= [16 30] (take 2 (map :x (walks s)))) "the second leg is 16 further than where the body stopped, 2 short of the first")
          (is (every? #(<= (js/Math.hypot (:x %) (:z %)) 40) (walks s)) "no leg beyond :max-distance")
          (is (= :distance (:why (event-of s :search.not-found)))))))))

(deftest an-unknown-pattern-or-heading-is-refused-at-submit
  (is (re-find #":pattern must be one of :outward, :spiral, got :zigzag"
               (expr/problem registry/jobs '(jobs.explore.search {:target "diamond_block" :pattern :zigzag}))))
  (is (re-find #":heading must be one of :east, :north, :south, :west, got :up"
               (expr/problem registry/jobs '(jobs.explore.search {:target "diamond_block" :pattern :outward :heading :up})))))

(deftest a-call-without-a-target-waits-at-its-start
  (with-redefs [ctx/frozen-call (constantly nil)]
    (is (= :no-target (search/no-target {:args {:target nil}})))
    (is (= :no-target (search/no-target {:args {:target []}})))
    (is (nil? (search/no-target {:args {:target "diamond_block"}}))))
  (with-redefs [ctx/frozen-call (constantly {:least 1})]
    (is (nil? (search/no-target {:args {:target nil}})) "a started call runs on")))

(deftest ore-the-body-has-not-seen-is-neither-found-nor-noted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20} {:blocks {"10,64,0" "diamond_block"}}
                                400 (fn [{:keys [p]}] (tu/blind p))))
              e (event-of s :search.not-found)]
          (is (= [] (:found e)))
          (is (not-any? #(= :seen (:kind %)) (file-notes s))))))))

(deftest a-scan-turns-its-head-so-ore-behind-the-body-is-found
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20 :max-legs 0}
                                {:blocks {"-4,64,0" "diamond_block"} :cone? true}))
              e (event-of s :search.done)]
          (is (= 1 (count (:found e))))
          (is (some #(= :seen (:kind %)) (file-notes s))))))))

(deftest a-scan-notes-ore-it-last-saw-until-a-look-corrects-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20 :max-legs 0}
                                {:blocks {"0,64,5" "diamond_block"} :cone? true}
                                400 (fn [{:keys [p]}]
                                      ;; mined after the body's last look, before sense reads memory: the body cannot know
                                      (let [seen-blocks (aget p "seenBlocks")]
                                        (aset p "seenBlocks" (fn [q]
                                                               (fake/set-block! p {:x 0 :y 64 :z 5} "air")
                                                               (seen-blocks q)))))))]
          (is (some? (event-of s :search.done)) "no sight pass corrected it: memory still says ore")
          (is (some #(= :seen (:kind %)) (file-notes s))))))))

(deftest a-name-with-many-sightings-does-not-fill-the-scan-cap-for-the-others
  (let [p (tu/seeing-all (tu/fake {:blocks (merge (tu/box 1 64 1 10 64 7 "hay_block") {"14,64,0" "diamond_block"})}))
        seen (set (map :what (search/sense p ["hay_block" "diamond_block"] 20)))]
    (is (= #{"hay_block" "diamond_block"} seen))))

(deftest spacing-and-distance-are-typed-and-bounded
  (is (= '(a/int-in 4 64) (:spec (:spacing search/args))))
  (is (= '(a/int-in 1 128) (:spec (:max-distance search/args)))))

(deftest one-choice-reads-a-bounded-number-of-stand-cells-and-says-more-is-left
  (let [reads (atom 0)
        p (tu/fake {})
        counting (js/Proxy. p #js {:get (fn [t k] (if (= "blockAt" k)
                                                   (fn [pos] (swap! reads inc) (.blockAt t pos))
                                                   (let [v (aget t k)] (if (fn? v) (.bind v t) v))))})
        c {:args {:pattern :spiral :spacing 4 :max-distance 128} :primitives counting}
        r (search/choose-leg c {:origin [0 64 0]} ["diamond_block"] [])]
    (is (nil? (:leg r)))
    (is (:more r))
    (is (< @reads 5000))))

(deftest a-search-over-ground-with-no-surface-takes-several-steps-and-ends-not-found-distance
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup {} 0)]
          (core/submit! (:eng s) '(jobs.explore.search {:target "diamond_block" :max-distance 40 :spacing 4}) {})
          (await (run-until-done s 400))
          (let [e (event-of s :search.not-found)]
            (is (<= (get-in e [:coverage :scans]) 2) "a step that only goes on choosing points does not look again")
            (is (= :distance (:why e)))
            (is (> (count (get-in e [:coverage :failed])) 100))))))))

(def sealed-room
  "Stone x 2..30, z 8..24, y 64..70 with a 3x3x2 room x 15..17, z 15..17 (the body in it) and ore buried at 22,64,16 and 16,64,21."
  (merge (tu/box 2 64 8 30 70 24 "stone") (tu/box 15 64 15 17 65 17 "air")
         {"22,64,16" "iron_ore" "16,64,21" "iron_ore"}))

(deftest buried-ore-is-not-found-from-inside-a-sealed-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "iron_ore" :count 1 :max-distance 8 :spacing 4 :scan-radius 16 :timeout-s 40}
                                {:blocks sealed-room :cone? true :self {:pos {:x 16.5 :y 64 :z 16.5}}} 60))]
          (is (nil? (event-of s :search.done)) (pr-str (:found (event-of s :search.done)))))))))

(deftest a-note-of-a-block-the-body-now-sees-as-something-else-is-not-a-finding
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen [{:kind :seen :what "diamond_block" :pos [0 64 5] :by "Other" :t 1 :until 9e15}]
              s (await (search! {:target "diamond_block" :max-distance 20 :max-legs 0} {:cone? true} 60 #(write-other! % "Other" seen)))]
          (is (nil? (event-of s :search.done)) "the cell is in view and holds air: the note is out of date")
          (is (= :not-found (:reason (event-of s :search.not-found)))))))))

;; a body in water does not look round (it sinks and drowns while it does): it walks on, and a leg ending in water fails
(deftest a-body-in-water-does-not-look-round-where-it-is
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (search! {:target "diamond_block" :max-distance 20} {:self {:pos [0.5 64 0.5] :inWater true}}))
              e (event-of s :search.not-found)]
          (is (finished? s))
          (is (zero? (get-in e [:coverage :scans])) (pr-str e)))))))
