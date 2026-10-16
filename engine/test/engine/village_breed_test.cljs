(ns engine.village-breed-test
  "jobs.village.breed against the fake world."
  (:require [engine.lit-support :as lit]
            [cljs.test :refer [deftest is are async use-fixtures]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.hostile-test :as h]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(use-fixtures :once lit/fixture)

(def job 'jobs.village.breed)

(defn villager
  "A villager at x along the row the body stands on; :baby true for a child."
  [id x & [extra]]
  (merge {:id id :name "villager" :kind "passive" :uuid (str "v-" id) :pos {:x x :y 64 :z 0}} extra))

(def bread [{:name "bread" :count 8}])

(defn feeding
  "Override toss: the drop is gone and taken by its target (taken false: left lying, nobody takes it); after the
  births-th toss entity arrives (a baby villager by default; births 0: never)."
  [p births & {:keys [entity taken after] :or {entity (villager 9 6 {:baby true}) taken true}}]
  (let [n (atom 0)]
    (.override (.-world p) "toss"
               (fn ^:async f [token a impl]
                 (let [r (await (impl token a))]
                   (when (and taken (= "tossed" (.-status r)))
                     (swap! (fake/state p) update :entities #(filterv (fn [e] (not= "item" (:kind e))) %))
                     (set! (.-takenBy r) (clj->js {"v-1" (.-count r) "v-2" (.-count r) "v-3" (.-count r)}))
                     (when (= births (swap! n inc))
                       (fake/add-entity! p entity))
                     (when after (after @n)))
                   r)))))

(defn start
  "An engine over a fake world (or the given p) on dir, holding a parent that runs the job as a child and keeps its
  result in :out."
  [{:keys [world p dir clock]} args]
  (let [clock (or clock (atom 1000000))
        [seen sink] (tu/legacy-capture-sink)
        p (or p (tu/fake-on-floor (merge {:floor h/flight-floor} world)))
        now (tu/act-clock clock p 0)
        out (atom :not-done)
        parent (tu/recording-parent job args out)
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent) :triggers {}
                          :dir (or dir (tu/tmp-dir)) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    {:eng eng :p p :seen seen :clock clock :out out}))

(defn ^:async run!
  "Tick the engine at most n times, while its list holds a job and stop? (when given) is false."
  [{:keys [eng clock]} n & [stop?]]
  (loop [i 0]
    (when (and (< i n) (seq (:list (core/state eng))) (not (and stop? (stop?))))
      (swap! clock + 700)
      (await (core/tick! eng))
      (recur (inc i)))))

(defn ^:async breed
  "Run the job until it ends (at most n ticks); its result is in :out."
  [world args n prepare]
  (let [p (tu/fake-on-floor (merge {:floor h/flight-floor} world))
        _ (prepare p)
        s (start {:p p} args)]
    (core/submit! (:eng s) '(recording-parent) {})
    (await (run! s n))
    s))

(defn tosses [p] (h/calls p "toss"))
(defn gave? [{:keys [seen]} kind] (some #(= kind (:kind %)) @seen))
(def pair [(villager 1 3) (villager 2 5)])
(def args {:target 3 :wait-s 3})

(deftest a-met-target-is-done-without-feeding
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (breed {:entities (conj pair (villager 3 6 {:baby true})) :inventory bread} args 20 identity))]
          (is (= :done (:status @out)) "already met: done with no data, before any round")
          (is (empty? (tosses p))))))))

(deftest two-adults-are-fed-three-bread-each-and-a-birth-completes-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (breed {:entities pair :inventory bread} args 80 #(feeding % 2)))]
          (is (= {:population 3 :target 3 :births 1} (select-keys @out [:population :target :births])))
          (is (= :done (:status @out)))
          (is (= 2 (count (tosses p))))
          (is (= ["bread" "bread"] (map #(.-item (.-args %)) (tosses p))))
          (is (= [3 3] (map #(.-count (.-args %)) (tosses p))))
          (is (gave? s :completed)))))))

(deftest nobody-breeding-ends-no-births
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out] :as s} (await (breed {:entities pair :inventory bread} args 120 #(feeding % 0)))]
          (is (= :no-births (:reason @out)))
          (is (= :stopped (:status @out)))
          (is (= 0 (:births @out)))
          (is (not (gave? s :breed.gave-up)) "the ending carries it"))))))

(deftest fewer-than-two-adults-or-no-food-never-starts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[es inv a] [[[(villager 1 3)] bread args]
                            [[(villager 1 3) (villager 2 5 {:baby true})] bread args]
                            [[] bread args]]]
          (let [{:keys [p out]} (await (breed {:entities es :inventory inv} a 30 identity))]
            (is (= [:stopped :no-pair] ((juxt :status :reason) @out)) "after one look the child ends condition-failed :no-pair; the job never starts")
            (is (empty? (tosses p)))))
        (let [{:keys [p out]} (await (breed {:entities pair} (assoc args :fetch false) 30 identity))]
          (is (= [:condition-failed :no-food] ((juxt (comp :kind :ending) :reason) @out)) "no food is a condition: the job never starts")
          (is (empty? (tosses p))))))))

(deftest an-adult-walking-into-view-is-not-a-birth
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (breed {:entities pair :inventory bread} {:target 4 :wait-s 3} 80
                                          #(feeding % 2 :entity (villager 9 6))))]
          (is (= 0 (:births @out)))
          (is (= :no-births (:reason @out))))))))

(deftest food-the-villager-does-not-take-is-a-fruitless-attempt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (breed {:entities (mapv #(villager % (+ 2 %)) (range 1 7)) :inventory [{:name "bread" :count 40}]}
                                            {:target 9 :wait-s 3} 400 #(feeding % 0 :taken false)))]
          (is (= :no-births (:reason @out)))
          (is (= 0 (:births @out)))
          (is (pos? (count (tosses p)))))))))

(deftest no-food-to-fetch-stops-no-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (breed {:entities pair} args 200 identity))]
          (is (= :no-food (:reason @out)))
          (is (= :stopped (:status @out))))))))

(deftest three-fruitless-attempts-stop-no-births
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (breed {:entities (mapv #(villager % (+ 2 %)) (range 1 9)) :inventory [{:name "bread" :count 40}]}
                                               {:target 12 :wait-s 1} 600 #(feeding % 0)))]
          (is (= :no-births (:reason @out))))))))

(deftest a-restart-mid-wait-keeps-the-baby-baseline
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [dir (tu/tmp-dir)
              p (tu/fake-on-floor (merge {:floor h/flight-floor} {:entities pair :inventory bread}))
              eng (atom nil)
              cut (atom false)
              _ (feeding p 0 :after #(when (= 2 %) (reset! cut true) (core/shutdown! @eng) (throw (core/cut-error))))
              s (start {:p p :dir dir} args)]
          (reset! eng (:eng s))
          (core/submit! (:eng s) '(recording-parent) {})
          (await (run! s 40 #(deref cut)))
          (is (= :not-done @(:out s)))
          (fake/add-entity! p (villager 9 6 {:baby true}))
          (let [again (start {:p p :dir dir :clock (:clock s)} args)]
            (await (run! again 40))
            (is (= 1 (:births @(:out again))) "the baby born while it was down still counts")))))))
