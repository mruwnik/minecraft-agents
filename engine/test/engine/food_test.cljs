(ns engine.food-test
  "The eat and get-food jobs and the hungry trigger against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.triggers.hungry :as hungry]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn call-args [p name k] (mapv #(js->clj (aget (.-args %) k) :keywordize-keys true) (calls p name)))

(defn entries [eng kind] (mapv :data (mem/entries (mem/view (:store eng)) kind)))

(defn know-source! [eng pos kind]
  (mem/write! (:store eng) :food-source {:pos pos :kind kind} {:cap 5 :ttl :forever}))

(defn food [p] (.-food (.self p)))

;; ---------------------------------------------------------------- hungry

(deftest hungry-below-the-food-line-or-below-the-hurt-line-when-hurt
  (are [food health expected] (= expected (hungry/hungry? food health {}))
    5 20 true
    6 20 false
    13 19 true
    14 19 false
    13 20 false
    2 20 true))

(deftest hungry-thresholds-are-args
  (is (hungry/hungry? 9 20 {:food 10}))
  (is (not (hungry/hungry? 13 10 {:food-when-hurt 12}))))

(deftest hungry-trigger-reads-the-world
  (let [when-fn (:when (get triggers/all :hungry))
        holds? (fn [self] (when-fn (tu/fake {:self self}) {} {}))]
    (is (= :hungry (:name (get triggers/all :hungry))))
    (is (= '(jobs.survival.get-food) (:job (get triggers/all :hungry))))
    (is (holds? {:food 4}))
    (is (not (holds? {:food 18})))
    (is (holds? {:food 10 :health 12}))))

;; ---------------------------------------------------------------- eat

(deftest eat-picks-the-best-food-and-writes-fed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 4}
                                      :inventory [{:name "carrot" :count 2} {:name "cooked_beef" :count 1}
                                                  {:name "rotten_flesh" :count 3}]})]
          (core/submit! eng '(jobs.survival.eat) {})
          (await (core/tick! eng))
          (is (= ["cooked_beef"] (call-args p "equip" "item")))
          (is (= ["cooked_beef"] (call-args p "eat" "item")))
          (is (= [{:item "cooked_beef" :food 9}] (entries eng :fed)))
          (is (= 1 (count (:list (core/state eng)))) "food 9 is below 18, so it goes on"))))))

(deftest eat-goes-on-until-fed-or-out-of-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 4} :inventory [{:name "bread" :count 5}]})]
          (core/submit! eng '(jobs.survival.eat) {})
          (is (= 3 (await (run-until-empty eng 10))) "4, 9, 14, 19")
          (is (= 19 (food p)))
          (is (= 3 (count (entries eng :fed)))))))))

(deftest eat-is-done-when-the-last-food-is-eaten
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 2} :inventory [{:name "bread" :count 1}]})]
          (core/submit! eng '(jobs.survival.eat) {})
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))))
          (is (= 7 (food p))))))))

(deftest eat-declines-without-edible-food-or-when-fed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [world [{:self {:food 4} :inventory [{:name "rotten_flesh" :count 2} {:name "spider_eye" :count 1}
                                                    {:name "cobblestone" :count 9}]}
                       {:self {:food 18} :inventory [{:name "bread" :count 2}]}]]
          (let [{:keys [eng p]} (setup world)]
            (core/submit! eng '(jobs.survival.eat) {})
            (is (nil? (core/tick! eng)) "the check declines")
            (is (= [] (calls p "eat")))))))))

(deftest eat-takes-bad-food-only-when-allowed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 4} :inventory [{:name "rotten_flesh" :count 1}]})]
          (core/submit! eng '(jobs.survival.eat {:allow-bad true}) {})
          (await (core/tick! eng))
          (is (= ["rotten_flesh"] (call-args p "eat" "item"))))))))

(deftest eat-until-is-an-arg
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 4} :inventory [{:name "bread" :count 5}]})]
          (core/submit! eng '(jobs.survival.eat {:until 8}) {})
          (await (run-until-empty eng 10))
          (is (= 9 (food p))))))))

;; ---------------------------------------------------------------- get-food

(deftest get-food-eats-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 3} :inventory [{:name "bread" :count 5}]})]
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 10))
          (is (= 18 (food p)) "eats until fed, as the eat job does")
          (is (= [] (calls p "moveTo")))
          (is (= [] (calls p "attack"))))))))

(deftest get-food-declines-when-not-hungry
  (let [{:keys [eng]} (setup {:self {:food 12}})]
    (core/submit! eng '(jobs.survival.get-food) {})
    (is (nil? (core/tick! eng)))))

(def farm
  {:blocks {"10,64,0" "carrots" "11,64,0" "carrots" "12,64,0" "carrots" "10,63,0" "farmland"}
   :ages {"10,64,0" 7 "11,64,0" 3 "12,64,0" 7}
   :drops {:carrots "carrot"}})

(deftest get-food-harvests-mature-crops-of-a-known-farm
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (assoc farm :self {:food 0}))]
          (know-source! eng {:x 11 :y 64 :z 0} :farm)
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 40))
          (is (= [] (:list (core/state eng))))
          (is (= [{:x 10 :y 64 :z 0} {:x 12 :y 64 :z 0}] (call-args p "dig" "pos"))
              "only the mature ones, the young carrots are left")
          (is (= 2 (count (calls p "eat"))))
          (is (= 10 (food p)))
          (is (not-any? #(= :food.none (:kind %)) @seen)))))))

(deftest get-food-ignores-a-source-beyond-its-radius
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 0}})]
          (know-source! eng {:x 500 :y 64 :z 0} :farm)
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 10))
          (is (= [] (calls p "moveTo"))))))))

(deftest get-food-takes-food-from-a-known-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 0}
                                      :containers {"20,64,0" [{:name "cobblestone" :count 30} {:name "bread" :count 4}
                                                              {:name "carrot" :count 9}]}})]
          (know-source! eng {:x 20 :y 64 :z 0} :chest)
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 20))
          (is (= ["bread"] (call-args p "transfer" "item")) "the best food in the chest, not the stone")
          (is (= ["withdraw"] (call-args p "transfer" "direction")))
          (is (= 20 (food p)) "then eats it"))))))

(def cow
  {:id 7 :name "cow" :kind "passive" :pos {:x 5 :y 64 :z 0} :health 20 :drops [{:name "beef" :count 1}]})

(deftest get-food-hunts-an-animal-when-no-source-is-known
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {:self {:food 2} :entities [cow]})]
          (core/submit! eng '(jobs.survival.get-food {:attack-gap-ms 0}) {})
          (await (run-until-empty eng 40))
          (is (= [] (:list (core/state eng))))
          (is (= [7 7 7 7] (call-args p "attack" "id")) "20 health, 5 a swing")
          (is (= ["beef"] (call-args p "eat" "item")))
          (is (= 7 (food p)))
          (is (not-any? #(= :food.none (:kind %)) @seen)))))))

(deftest get-food-does-not-swing-faster-than-the-gap
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock]} (setup {:self {:food 2} :entities [cow]})]
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (is (= 1 (count (calls p "attack"))) "the clock has not moved")
          (swap! clock + 1000)
          (await (core/tick! eng))
          (is (= 2 (count (calls p "attack")))))))))

(deftest get-food-forages-a-berry-bush-when-there-are-no-animals
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 1}
                                      :blocks {"6,64,0" "sweet_berry_bush"} :ages {"6,64,0" 3}
                                      :drops {:sweet_berry_bush "sweet_berries"}})]
          (core/submit! eng '(jobs.survival.get-food) {})
          (await (run-until-empty eng 20))
          (is (= [{:x 6 :y 64 :z 0}] (call-args p "dig" "pos")))
          (is (= ["sweet_berries"] (call-args p "eat" "item"))))))))

(deftest get-food-says-so-once-per-cooldown-when-nothing-is-found
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup {:self {:food 1}})
              none #(filterv (fn [e] (= :food.none (:kind e))) @seen)
              run! (fn ^:async run! []
                     (core/submit! eng '(jobs.survival.get-food) {})
                     (await (run-until-empty eng 10)))]
          (know-source! eng {:x 300 :y 64 :z 0} :farm)
          (await (run!))
          (is (= [] (:list (core/state eng))) "done, so the list resumes work")
          (is (= 1 (count (none))))
          (is (= :warn (:level (first (none)))))
          (is (re-find #"farm" (:text (first (none)))) "names the nearest known source")
          (is (= 1 (count (entries eng :hungry))))
          (swap! clock + 60000)
          (await (run!))
          (is (= 1 (count (none))) "inside the cooldown it stays quiet")
          (swap! clock + 600000)
          (await (run!))
          (is (= 2 (count (none))) "after the cooldown it says so again"))))))
