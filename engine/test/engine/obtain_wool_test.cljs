(ns engine.obtain-wool-test
  "Wool as a gathered need of items.obtain: the colour the body carries most of, taken from sheep of that colour."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.registry :as registry]
            [engine.hostile-test :as h]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.items.obtain :as obtain]
            [jobs.items.obtain.gather :as gather]
            [jobs.lib.world-files :as ew]))

(defn sheep [id x color]
  {:id id :name "sheep" :kind "passive" :pos {:x x :y 64 :z 0} :color color
   :drops [{:name (str color "_wool") :count 1}]})

(def beds ["white_bed" "brown_bed" "black_bed"])

(defn carried [& pairs] (mapv (fn [[n c]] {:name n :count c}) (partition 2 pairs)))

(defn ^:async scenario [args world n]
  (let [s (h/setup world 50)]
    (core/submit! (:eng s) (list 'jobs.items.obtain args) {})
    (await (tu/run-ticks (:eng s) (:clock s) n 700))
    s))

(defn attacked [{:keys [p]}] (mapv #(.-id (.-args %)) (h/calls p "attack")))

(deftest a-bed-need-hunts-a-sheep-of-the-colour-carried-most
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (into h/sword (carried "white_wool" 2 "oak_planks" 3 "crafting_table" 1))
              s (await (scenario {:any-of beds :count 1 :ways #{:craft :smelt :gather}}
                                 {:inventory inv :entities [(sheep 1 3 "brown") (sheep 2 6 "white") (sheep 3 8 "white") (sheep 4 10 "white") (sheep 5 11 "white")]} 40))]
          (is (seq (attacked s)) "a sheep was attacked")
          (is (every? #{2 3 4 5} (attacked s)) "only white sheep")
          (let [e (first (filter #(= :obtain.gathering (:kind %)) @(:seen s)))]
            (is (= :info (:level e)))
            (is (= "white_wool" (:item e)) "the walk names what it is for")
            (is (= 'jobs.combat.hunt (:via e)))))))))

(deftest a-colour-with-no-sheep-in-range-falls-back-to-another-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (into h/sword (carried "white_wool" 2 "oak_planks" 3 "crafting_table" 1))
              s (await (scenario {:any-of beds :count 1 :ways #{:craft :smelt :gather}}
                                 {:inventory inv :entities [(sheep 1 3 "brown") (sheep 2 6 "brown") (sheep 4 10 "brown") (sheep 5 11 "brown")]} 40))
              e (first (filter #(= :obtain.wool-colour (:kind %)) @(:seen s)))]
          (is (seq (attacked s)))
          (is (every? #{1 2 4 5} (attacked s)))
          (is (= "white" (:wanted e)))
          (is (= "brown" (:taking e))))))))

;; hunt keeps the last 2 sheep of the colour alive (its :keep), so each colour in the tests has 4.

(deftest sheared-sheep-and-the-kept-pair-do-not-count-as-wool-sources
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (into h/sword (carried "white_wool" 2 "oak_planks" 3 "crafting_table" 1))
              shorn #(assoc (sheep %1 %2 "white") :sheared true)
              ;; white: 3 sheep, 1 sheared: 2 unsheared are the pair hunt keeps; brown has 4
              s (await (scenario {:any-of beds :count 1 :ways #{:craft :smelt :gather}}
                                 {:inventory inv :entities [(shorn 1 3) (sheep 2 6 "white") (sheep 3 8 "white")
                                                            (sheep 4 10 "brown") (sheep 5 11 "brown") (sheep 6 12 "brown") (sheep 7 13 "brown")]} 40))
              e (first (filter #(= :obtain.wool-colour (:kind %)) @(:seen s)))]
          (is (seq (attacked s)))
          (is (every? #{4 5 6 7} (attacked s)) "only brown sheep")
          (is (= "brown" (:taking e))))))))

(deftest obtain-puts-a-carried-table-down-in-an-urgent-section
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "wooden_pickaxe" :count 1}
                                 {:inventory (carried "oak_planks" 3 "stick" 2 "crafting_table" 1)} 20))]
          (is (= 1 (get (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self (:p s)))) "wooden_pickaxe")) "crafted")
          (is (= [[:craft :urgent]] (->> @(:seen s) (filter #(and (= :section (:kind %)) (:open %))) (map (juxt :section :holds)) distinct vec))))))))

(deftest wool-with-no-sheep-seen-waits-no-source-without-throwing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup-seeing {:inventory (carried "oak_planks" 3)} nil 700)]
          (core/submit! (:eng s) (list 'jobs.items.obtain {:item "white_wool" :count 3}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 3 700))
          (is (empty? (filter #(= :failed (:kind %)) @(:seen s))) "no round or check threw"))))))

(def shears-recipe {"shears" {:count 1 :needs {"iron_ingot" 2}}})

(defn wool-source [{:keys [seen]}] (first (filter #(= :obtain.wool-source (:kind %)) @seen)))
(defn wool-count [{:keys [p]}]
  (reduce + 0 (for [s (.-inventory (.self p)) :when (= "white_wool" (.-name s))] (.-count s))))

(deftest wool-is-sheared-with-shears-crafted-from-carried-iron-and-no-sheep-dies
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (into h/sword (carried "iron_ingot" 2))
              s (await (scenario {:item "white_wool" :count 2}
                                 {:inventory inv :recipes shears-recipe :entities [(sheep 1 3 "white") (sheep 2 6 "white")]} 60))]
          (is (empty? (attacked s)) "no sheep was killed")
          (is (<= 2 (wool-count s)) "wool carried")
          (is (re-find #"shear" (pr-str (wool-source s))) "the text says it sheared"))))))

(deftest wool-is-hunted-when-shears-are-neither-carried-nor-craftable-and-says-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [inv (into h/sword (carried "iron_ingot" 1))
              s (await (scenario {:item "white_wool" :count 1}
                                 {:inventory inv :entities [(sheep 1 3 "white") (sheep 2 6 "white") (sheep 3 8 "white") (sheep 4 10 "white")]} 40))]
          (is (seq (attacked s)) "a sheep was killed")
          (is (re-find #"kill|hunt" (pr-str (wool-source s)))))))))

(deftest kept-pair-is-not-killed-for-wool-when-shearing-is-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "white_wool" :count 1}
                                 {:inventory h/sword :entities [(sheep 1 3 "white") (sheep 2 6 "white")]} 40))]
          (is (empty? (attacked s)) "the last two adults live"))))))

(deftest wool-is-never-taken-from-penned-or-off-limits-sheep
  (async done
    (tu/run-async done
      (fn []
        (settings/with-settings {:jobs.lib.animal-spare/off-limits #{"sheep"}}
          (fn ^:async t []
            (let [inv (into h/sword (carried "oak_planks" 3 "crafting_table" 1))
                  pen [{:name "linden-west-pen" :owner "Fake" :min [0 60 -3] :max [20 70 3]}]
                  s (h/setup {:inventory inv :entities [(sheep 1 3 "white") (sheep 2 6 "white") (sheep 3 8 "white") (sheep 4 10 "white")]}
                             50 (ew/of-data {} {} pen))]
              (core/submit! (:eng s) '(jobs.items.obtain {:any-of ["white_bed"] :count 1 :ways #{:craft :smelt :gather}}) {})
              (await (tu/run-ticks (:eng s) (:clock s) 40 700))
              (is (empty? (attacked s)) "no sheep attacked"))))))))

(deftest a-hunt-that-brings-no-wool-is-not-run-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [runs (atom 0)
              hunt (assoc (get registry/jobs 'jobs.combat.hunt) :round (fn ^:async no-wool [c] (swap! runs inc) (ctx/end c :done nil {:got 0})))]
          (await (with-redefs [registry/jobs (assoc registry/jobs 'jobs.combat.hunt hunt)]
                   (scenario {:item "white_wool" :count 1}
                             {:inventory h/sword :entities [(sheep 1 3 "white") (sheep 2 6 "white") (sheep 3 8 "white")]} 40)))
          (is (= 1 @runs)))))))

(defn cow [id x] {:id id :name "cow" :kind "passive" :pos {:x x :y 64 :z 0} :drops [{:name "leather" :count 1}]})

(deftest leather-is-hunted-from-a-cow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:item "leather" :count 1}
                                 {:inventory h/sword :entities [(cow 1 3) (cow 2 6) (cow 3 8)]} 40))]
          (is (seq (attacked s)) "a cow was attacked"))))))

(deftest string-and-bone-are-hunted-from-a-seen-spider-and-skeleton
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[item mob] [["string" "spider"] ["bone" "skeleton"]]]
          (let [s (await (scenario {:item item :count 1}
                                   {:inventory h/sword :entities [{:id 1 :name mob :kind "hostile" :pos {:x 20 :y 64 :z 0} :drops [{:name item :count 1}]}]} 40))]
            (is (seq (attacked s)) item)))))))

(deftest a-hurt-or-unarmed-body-does-not-hunt-a-spider-for-string
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spider [{:id 1 :name "spider" :kind "hostile" :pos {:x 20 :y 64 :z 0} :drops [{:name "string" :count 1}]}]]
          (doseq [world [{:inventory h/sword :self {:health 4} :entities spider}
                         {:inventory [] :entities spider}]]
            (is (empty? (attacked (await (scenario {:item "string" :count 1} world 40)))) (pr-str (dissoc world :entities)))))))))

(deftest a-hunt-that-spared-animals-says-which-rule-spared-them
  (is (= "left alone: 3 pen" (gather/spared-why {:spared-by {:pen 3}})))
  (is (nil? (gather/spared-why {:killed 0}))))

(deftest leather-from-a-penned-cow-is-not-hunted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pen [{:name "linden-west-pen" :owner "Fake" :min [0 60 -3] :max [20 70 3]}]
              s (h/setup {:inventory h/sword :entities [(cow 1 3) (cow 2 6) (cow 3 8)]} 50 (ew/of-data {} {} pen))]
          (core/submit! (:eng s) '(jobs.items.obtain {:item "leather" :count 1}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 40 700))
          (is (empty? (attacked s)) "no cow attacked")
          (is (re-find #"left alone: 3 pen" (:why (first (filter #(= :stopped (:kind %)) @(:seen s))))) "the ending says the cows were spared"))))))
