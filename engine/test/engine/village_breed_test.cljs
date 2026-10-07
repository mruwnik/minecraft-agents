(ns engine.village-breed-test
  "jobs.village.breed against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.hostile-test :as h]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(def job 'jobs.village.breed)

(defn villager
  "A villager at x along the row the body stands on; :baby true for a child."
  [id x & [extra]]
  (merge {:id id :name "villager" :kind "passive" :uuid (str "v-" id) :pos {:x x :y 64 :z 0}} extra))

(def bread [{:name "bread" :count 8}])

(defn feeding
  "Override toss: the drop is gone and taken by its target; after the births-th toss a baby villager appears
  (births 0: never)."
  [p births]
  (let [n (atom 0)]
    (.override (.-world p) "toss"
               (fn ^:async f [token a impl]
                 (let [r (await (impl token a))]
                   (when (= "tossed" (.-status r))
                     (swap! (fake/state p) update :entities #(filterv (fn [e] (not= "item" (:kind e))) %))
                     (set! (.-takenBy r) (clj->js {"v-1" (.-count r) "v-2" (.-count r) "v-3" (.-count r)}))
                     (when (= births (swap! n inc))
                       (fake/add-entity! p (villager 9 6 {:baby true}))))
                   r)))))

(defn ^:async breed
  "Run the job until it ends (at most n ticks); its result is in :out."
  [world args n prepare]
  (let [s (h/setup world)
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async r [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc (:eng s) :jobs (assoc (:jobs (:eng s)) 'recording-parent parent))]
    (prepare (:p s))
    (core/submit! eng '(recording-parent) {})
    (loop [i 0]
      (when (and (< i n) (seq (:list (core/state eng))))
        (swap! (:clock s) + 700)
        (await (core/tick! eng))
        (recur (inc i))))
    (assoc s :out out)))

(defn tosses [p] (h/calls p "toss"))
(defn gave? [{:keys [seen]} kind] (some #(= kind (:kind %)) @seen))
(def pair [(villager 1 3) (villager 2 5)])
(def args {:target 3 :wait-s 3})

(deftest check-wants-a-target-of-two-or-more
  (are [a ok] (= ok (boolean ((:check (get registry/jobs job)) {:args a :primitives nil :mem (constantly {})})))
    {:target nil} false
    {:target 1} false
    {:target 3} true))

(deftest a-met-target-is-done-without-feeding
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (breed {:entities (conj pair (villager 3 6 {:baby true})) :inventory bread} args 20 identity))]
          (is (= {:population 3 :target 3 :births 0} (select-keys @out [:population :target :births])))
          (is (nil? (:status @out)))
          (is (empty? (tosses p))))))))

(deftest two-adults-are-fed-three-bread-each-and-a-birth-completes-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (breed {:entities pair :inventory bread} args 80 #(feeding % 2)))]
          (is (= {:population 3 :target 3 :births 1} (select-keys @out [:population :target :births])))
          (is (nil? (:status @out)))
          (is (= 2 (count (tosses p))))
          (is (= ["bread" "bread"] (map #(.-item (.-args %)) (tosses p))))
          (is (= [3 3] (map #(.-count (.-args %)) (tosses p))))
          (is (gave? s :breed.done)))))))

(deftest nobody-breeding-ends-no-births
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (breed {:entities pair :inventory bread} args 120 #(feeding % 0)))]
          (is (= "no-births" (:reason @out)))
          (is (= :stopped (:status @out)))
          (is (= 0 (:births @out)))
          (is (gave? s :breed.gave-up)))))))

(deftest fewer-than-two-adults-is-no-pair
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [es [[(villager 1 3)] [(villager 1 3) (villager 2 5 {:baby true})] []]]
          (let [{:keys [p out]} (await (breed {:entities es :inventory bread} args 30 identity))]
            (is (= "no-pair" (:reason @out)))
            (is (empty? (tosses p)))))))))

(deftest no-food-and-no-fetch-yields-without-tossing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (breed {:entities pair} (assoc args :fetch false) 15 #(feeding % 2)))]
          (is (= :not-done @out))
          (is (empty? (tosses p))))))))
