(ns engine.tend-test
  "jobs.animals.tend against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.test-util :as tu]
            [jobs.animals.tend :as tend]))

(def box {:min {:x 0 :y 60 :z -5} :max {:x 8 :y 70 :z 5}})

(defn spec [args] (list 'jobs.animals.tend (merge {:box box} args)))

(def cow-drops [{:name "beef" :count 1} {:name "leather" :count 1}])

(defn animal
  ([id name x] (animal id name x 0 {}))
  ([id name x z more] (merge {:id id :uuid (str "u" id) :name name :kind "passive" :pos {:x x :y 64 :z z}} more)))

(defn cow
  ([id x] (cow id x 0))
  ([id x z] (animal id "cow" x z {:drops cow-drops})))

(defn calf [id x] (animal id "cow" x 0 {:drops cow-drops :baby true}))

(defn wheat [n] {:name "wheat" :count n})

(defn drop-at
  "An item entity lying at x."
  [id name x]
  {:id id :name "item" :kind "item" :pos {:x x :y 64 :z 0} :item {:name name :count 1}})

(def leather-on-the-ground (drop-at 90 "leather" 4))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks 700 ms apart; the setup map."
  [args world n]
  (let [s (h/setup world)]
    (core/submit! (:eng s) (spec args) {})
    (dotimes [_ n]
      (swap! (:clock s) + 700)
      (await (core/tick! (:eng s))))
    s))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event [s] (first (events-of s :tend.done)))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn inv [{:keys [p]}] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn interacts [{:keys [p]}] (h/calls p "interact"))
(defn attacked [{:keys [p]}] (mapv #(.-id (.-args %)) (h/calls p "attack")))

(deftest two-adults-and-wheat-below-target-are-fed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3)]} 12))]
          (is (= ["wheat" "wheat"] (mapv #(.-item (.-args %)) (interacts s))))
          (is (empty? (attacked s)))
          (is (= 2 (get-in (done-event s) [:steps :breed :fed])))
          (is (true? (finished? s))))))))

(deftest seven-adults-above-target-four-are-culled-and-the-drops-picked-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory h/sword :entities (mapv #(cow % (+ 1 %)) (range 1 8))} 80))]
          (is (= 3 (count (set (attacked s)))))
          (is (= {:killed 3 :remaining 4 :reason :keep} (select-keys (get-in (done-event s) [:steps :cull]) [:killed :remaining :reason])))
          (is (= 3 (get (inv s) "beef")))
          (is (= 3 (get (inv s) "leather")))
          (is (true? (finished? s))))))))

(deftest the-floor-of-two-adults-is-kept
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:inventory h/sword :entities (mapv #(cow % (+ 1 %)) (range 1 4))} 60))]
          (is (= {:killed 1 :remaining 2 :reason :keep} (select-keys (get-in (done-event s) [:steps :cull]) [:killed :remaining :reason])))
          (is (= 2 (:adults (done-event s)))))))))

(deftest calves-count-toward-the-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory [(wheat 4)] :entities [(cow 1 2) (cow 2 3) (calf 3 2) (calf 4 2) leather-on-the-ground]} 12))]
          (is (empty? (interacts s)))
          (is (= {:skipped :at-target} (get-in (done-event s) [:steps :breed]))))))))

(deftest calves-and-surplus-adults-cull-to-the-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory h/sword :entities (into [(calf 6 2) (calf 7 2)] (map #(cow % (+ 1 %))) (range 1 6))} 80))]
          (is (= 3 (count (set (attacked s)))))
          (is (= 2 (:adults (done-event s))))
          (is (= 2 (:babies (done-event s)))))))))

(deftest without-food-the-breed-is-skipped-and-the-job-ends-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:entities [(cow 1 2) (cow 2 3) leather-on-the-ground]} 12))]
          (is (empty? (interacts s)))
          (is (= {:skipped :no-food} (get-in (done-event s) [:steps :breed])))
          (is (true? (finished? s))))))))

(deftest one-adult-cannot-breed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory [(wheat 4)] :entities [(cow 1 2) (calf 2 2) leather-on-the-ground]} 12))]
          (is (= {:skipped :too-few-adults} (get-in (done-event s) [:steps :breed]))))))))

(deftest items-inside-the-box-are-collected-and-those-outside-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:entities [(cow 1 2) (cow 2 3) (cow 3 4) (cow 4 5) (drop-at 90 "leather" 4) (drop-at 91 "beef" 7) (drop-at 92 "dirt" 12)]} 20))]
          (is (= {"leather" 1 "beef" 1} (inv s)))
          (is (= 2 (get-in (done-event s) [:steps :collect :collected])))
          (is (= #{92} (set (map #(.-id %) (filter #(= "item" (.-kind %)) (.-entities (.-state (.-world (:p s))))))))))))))

(defn sheep [id x] (animal id "sheep" x 0 {}))

(deftest sheep-are-sheared-with-shears
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "sheep" :target 2} {:inventory [{:name "shears" :count 1}] :entities [(sheep 1 2) (sheep 2 3)]} 20))]
          (is (= 2 (get (inv s) "white_wool")))
          (is (= 2 (get-in (done-event s) [:steps :shear :shorn])))
          (is (true? (finished? s))))))))

(deftest no-shears-no-shearing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "sheep" :target 2} {:entities [(sheep 1 2) (sheep 2 3) leather-on-the-ground]} 20))]
          (is (= {:skipped :no-shears} (get-in (done-event s) [:steps :shear]))))))))

(deftest cows-are-not-sheared
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 2} {:inventory [{:name "shears" :count 1}] :entities [(cow 1 2) (cow 2 3) leather-on-the-ground]} 20))]
          (is (= {:skipped :not-sheep} (get-in (done-event s) [:steps :shear]))))))))

(def chest {:x 10 :y 64 :z 0})

(def four-cows [(cow 1 2) (cow 2 3) (cow 3 4) (cow 4 5)])

(def carried [{:name "beef" :count 3} {:name "leather" :count 2} (wheat 5) {:name "iron_sword" :count 1}])

(defn chest-items [{:keys [p]}]
  (into {} (map (juxt :name :count)) (js->clj (.get (.. p -world -state -containers) "10,64,0") :keywordize-keys true)))

(deftest produce-goes-to-the-chest-and-food-and-tools-stay
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4 :chest chest} {:inventory carried :entities four-cows :containers {"10,64,0" []}} 20))]
          (is (= {"beef" 3 "leather" 2} (chest-items s)))
          (is (= {"wheat" 5 "iron_sword" 1} (inv s)))
          (is (= {:gave-up false} (select-keys (get-in (done-event s) [:steps :deposit]) [:gave-up])))
          (is (true? (finished? s))))))))

(deftest keep-leaves-some-produce-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4 :chest chest :keep {"beef" 1}} {:inventory carried :entities four-cows :containers {"10,64,0" []}} 20))]
          (is (= {"beef" 2 "leather" 2} (chest-items s)))
          (is (= 1 (get (inv s) "beef"))))))))

(deftest without-a-chest-the-produce-stays-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory carried :entities (conj four-cows leather-on-the-ground)} 20))]
          (is (= 3 (get (inv s) "beef")))
          (is (= {:skipped :no-chest} (get-in (done-event s) [:steps :deposit])))
          (is (true? (finished? s))))))))

(defn declines?
  "True when the job is submitted, the first tick does nothing and nothing is walked to, fed or attacked."
  [args entities]
  (let [{:keys [eng p] :as s} (h/setup {:inventory [(wheat 4) {:name "iron_sword" :count 1}] :entities entities})]
    (core/submit! eng (list 'jobs.animals.tend args) {})
    (and (nil? (core/tick! eng))
         (empty? (interacts s))
         (empty? (attacked s))
         (zero? (count (h/calls p "moveTo"))))))

(deftest the-check-declines-when-nothing-needs-doing
  (is (declines? {:box box :target 4} four-cows) "at target")
  (is (declines? {:box box :target 4} []) "nothing around, one adult too few to breed")
  (is (declines? {:target 4} [(cow 1 2)]) "no box")
  (is (declines? {:box box :target 2} [(animal 1 "pig" 2) (animal 2 "pig" 3) (animal 3 "pig" 4)]) "other kinds only")
  (is (declines? {:box box :target 4} (conj four-cows (cow 5 20) (cow 6 30) (cow 7 40))) "cows outside the box are not counted"))

(deftest a-full-run-hands-over-its-result-as-data
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} {:inventory h/sword :entities (mapv #(cow % (+ 1 %)) (range 1 6))} 60))
              r (done-event s)]
          (is (= {:mob "cow" :target 4 :adults 4 :babies 0} (select-keys r [:mob :target :adults :babies])))
          (is (= [:breed :cull :shear :collect :deposit] (vec (keys (:steps r)))))
          (is (= {:breed {:skipped :at-target} :shear {:skipped :not-sheep} :deposit {:skipped :no-chest}}
                 (select-keys (:steps r) [:breed :shear :deposit]))))))))

(deftest the-pure-pieces-decide-from-facts
  (are [expected inventory produce reserve] (= expected (tend/to-store inventory produce reserve))
    ["beef"] [{:name "beef" :count 2} {:name "wheat" :count 1}] ["beef" "leather"] {}
    [] [{:name "beef" :count 1}] ["beef"] {"beef" 1}
    ["beef"] [{:name "beef" :count 64} {:name "beef" :count 3}] ["beef"] {"beef" 66}
    [] [{:name "iron_sword" :count 1}] ["beef"] {})
  (are [expected facts] (= expected (:skip (tend/decide :breed {:mob "cow" :target 4} facts)))
    :at-target {:adults 3 :babies 1}
    :too-few-adults {:adults 1 :babies 0}
    :no-food {:adults 2 :babies 0 :food nil}
    nil {:adults 2 :babies 0 :food "wheat" :reach 5})
  (are [expected facts] (= expected (:skip (tend/decide :cull {:mob "cow" :target 4} facts)))
    :within-target {:adults 4 :babies 0}
    :within-target {:adults 2 :babies 3}
    nil {:adults 5 :babies 0}
    nil {:adults 5 :babies 2}))

(deftest the-check-declines-when-the-only-drops-lie-outside-the-box
  (is (declines? {:box box :target 4} (conj four-cows (drop-at 90 "leather" 9)))))
