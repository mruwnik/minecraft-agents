(ns engine.shafts-test
  "jobs.lib.shafts: finding a mineshaft the body may use, and the compact branch memory."
  (:require [cljs.test :refer [deftest is are]]
            [engine.memory :as mem]
            [jobs.lib.shafts :as shafts]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]))

(defn shaft-plan
  "A mineshaft plan by owner: stair from top [x y z] heading :east down to y 60."
  [id by top]
  (-> {:id id :kind :mineshaft :parts [{:id "stair" :stair {:from top :heading :east :to-y 60} :want :clear}]}
      (shape/with-author by)))

(def start {:x 0 :y 64 :z 0})

(defn ctx
  "A job ctx over a world of plans and zones with an in-RAM body memory."
  [plans zones]
  (let [a (atom mem/empty-data)]
    {:primitives #js {:self (fn [] #js {:username "Fake"})}
     :args {}
     :engine {:world (ew/of-data plans {} zones)}
     :view (fn [] {:data @a :now 0})
     :remember (fn [kind data policy] (swap! a mem/add-entry kind {:t 0 :data data} policy))}))

(defn ids [shafts] (mapv :id shafts))

(deftest lookup-gives-the-shafts-in-radius-nearest-first
  (let [c (ctx {"near" (shaft-plan "near" "Miles" [10 64 0])
                "mid" (shaft-plan "mid" "Fake" [40 64 0])
                "far" (shaft-plan "far" "Miles" [300 64 0])
                "hut" {:id "hut" :parts [{:id "p" :cells [[1 64 1]] :want "stone"}]}}
               [])
        r (shafts/lookup c start {})]
    (is (= ["near" "mid"] (ids (:usable r))))
    (is (= [] (:refused r)))
    (is (= ["near" "mid" "far"] (ids (:usable (shafts/lookup c start {:radius 400})))))
    (is (= ["near"] (ids (:usable (shafts/lookup c start {:radius 20})))))))

(def theirs {:name "theirs" :min [5 50 -5] :max [15 70 5] :owner "Miles"})
(def mine {:name "mine" :min [5 50 -5] :max [15 70 5] :owner "Fake"})

(deftest a-shaft-in-another-zone-is-refused-unless-overridden
  (are [zones foreign? usable refused] (let [r (shafts/lookup (ctx {"s" (shaft-plan "s" "Miles" [10 64 0])} zones) start {:foreign-zone? foreign?})]
                                          (= [usable refused] [(ids (:usable r)) (mapv #(get-in % [:verdict :reason]) (:refused r))]))
    [theirs] false [] [:zone]
    [theirs] true ["s"] []
    [mine] false ["s"] []
    [] false ["s"] []
    nil false [] [:no-zones]))

(deftest another-plans-footprint-over-the-top-cell-refuses-but-the-shafts-own-does-not
  (let [hut {:id "hut" :parts [{:id "p" :cells [[10 64 0]] :want "stone"}]}
        c (ctx {"s" (shaft-plan "s" "Miles" [10 64 0]) "hut" hut} [])
        r (shafts/lookup c start {})]
    (is (= [] (ids (:usable r))))
    (is (= [:footprint] (mapv #(get-in % [:verdict :reason]) (:refused r))))
    (is (= ["s"] (ids (:usable (shafts/lookup (ctx {"s" (shaft-plan "s" "Miles" [10 64 0])} []) start {}))))
        "the shaft's own cells never refuse it")))

(def shaft {:id "s" :top [10 64 0] :bottom [14 60 0] :heading :east})

(deftest the-first-branch-runs-on-and-later-ones-leave-the-first-run-left-and-right
  (let [first-run {:from [14 60 0] :heading :east :length 30}]
    (are [done expected] (= expected (shafts/next-branch shaft {:n (count done) :run (:length (first done))} 3))
      [] {:from [14 60 0] :heading :east}
      [first-run] {:from [17 60 0] :heading :north}
      [first-run first-run] {:from [17 60 0] :heading :south}
      [first-run first-run first-run] {:from [20 60 0] :heading :north}
      [first-run first-run first-run first-run] {:from [20 60 0] :heading :south})
    (is (nil? (shafts/next-branch shaft {:n 1 :run 2} 3)) "a first run too short to carry a branch: used up")))

(deftest shaft-of-reads-top-and-bottom-from-the-stair
  (is (= {:id "s" :top [10 64 0] :bottom [14 60 0] :heading :east :to-y 60 :from [10 64 0]}
         (shafts/shaft-of (shaft-plan "s" "Miles" [10 64 0]))))
  (is (nil? (shafts/shaft-of {:id "x" :parts [{:id "p" :cells [[0 0 0]] :want "stone"}]}))))

(deftest branch-memory-keeps-points-not-blocks-and-stays-small
  (let [c (ctx {} [])]
    (dotimes [i 100]
      (shafts/record-branch! c "s" {:from [i 60 0] :heading :north :length (* 1000 (inc i))}))
    (shafts/record-branch! c "t" {:from [0 60 0] :heading :east :length 5})
    (let [kept (shafts/branches c "s")]
      (is (= 24 (count kept)) "only the newest are kept")
      (is (= [76 60 0] (:from (first kept))))
      (is (every? #(= #{:from :heading :length} (set (keys %))) kept) "a point, a heading and a length")
      (is (= 1 (count (shafts/branches c "t"))))
      (is (< (count (pr-str (shafts/shafts-memory c))) 2500) "the entry is small whatever the lengths"))))

(deftest branch-slots-stay-distinct-past-the-remembered-branches
  (let [c (ctx {} [])
        slots (vec (for [_ (range 30)
                         :let [slot (shafts/next-for c shaft)]]
                     (do (shafts/record-branch! c "s" (assoc slot :length 100))
                         slot)))]
    (is (= 30 (count (set slots))) "no slot is handed out twice")
    (is (= 30 (shafts/branch-count c "s")))))
