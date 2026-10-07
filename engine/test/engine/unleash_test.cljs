(ns engine.unleash-test
  "jobs.animals.unleash against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.fake :as fake]
            [engine.test-util :as tu]))

(defn animal [id x & [more]]
  (merge {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}} more))

(defn led [id x & [more]] (animal id x (merge {:leashed true :leashedToMe true} more)))
(defn tied [id x & [more]] (animal id x (merge {:leashed true :leashHolder 50} more)))
(def knot {:id 50 :name "leash_knot" :kind "other" :pos {:x 6 :y 64 :z 0}})

(defn ^:async run-ticks
  [{:keys [eng clock]} n step]
  (dotimes [_ n]
    (swap! clock + step)
    (await (core/tick! eng))))

(defn ^:async scenario
  "Submit (jobs.animals.unleash args) in a world; run n ticks 700 ms apart; the setup map."
  [args world n]
  (let [s (h/setup world)]
    (core/submit! (:eng s) (list 'jobs.animals.unleash args) {})
    (await (run-ticks s n 700))
    s))

(defn done-event [{:keys [seen]}] (first (filter #(= :unleash.done (:kind %)) @seen)))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn calls-of [{:keys [p]} name] (h/calls p name))
(defn count-of [{:keys [p]} item]
  (reduce + (map :count (filter #(= item (:name %)) (js->clj (.-inventory (.self p)) :keywordize-keys true)))))
(defn still-leashed [{:keys [p]}]
  (mapv :id (filter #(true? (:leashed %)) (fake/entities p))))
(defn lead-items [{:keys [p]}]
  (count (filter #(and (= "item" (:name %)) (= "lead" (get-in % [:item :name]))) (fake/entities p))))

(deftest takes-the-lead-off-and-picks-it-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:entities [(led 1 3)]} 6))]
          (is (finished? s))
          (is (= :unleashed (:reason (done-event s))))
          (is (= ["u1"] (:freed (done-event s))))
          (is (empty? (still-leashed s)))
          (is (= 1 (count-of s "lead")))
          (is (= 1 (:collected (done-event s))))
          (is (= 1 (:leads (done-event s))))
          (is (empty? (events-of s :unleash.gave-up))))))))

(deftest one-call-frees-and-collects
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:entities [(led 1 3)]} 1))]
          (is (finished? s))
          (is (= :unleashed (:reason (done-event s))))
          (is (= 1 (count-of s "lead"))))))))

(deftest a-lead-picked-up-at-once-counts-as-collected
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory [{:name "lead" :count 2}] :entities [(led 1 3 {:pickup true})]} 6))]
          (is (finished? s))
          (is (= 0 (lead-items s)) "nothing lay on the ground")
          (is (= 3 (count-of s "lead")))
          (is (= 1 (:collected (done-event s))))
          (is (= 3 (:leads (done-event s)))))))))

(deftest waits-for-the-lead-to-show-before-picking-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (h/setup {:entities [(led 1 3)]})
              order (atom [])]
          (doseq [name ["interact" "wait" "collect"]]
            (.override (.-world p) name (fn [token args impl] (swap! order conj name) (impl token args))))
          (core/submit! eng '(jobs.animals.unleash {}) {})
          (await (run-ticks s 8 700))
          (is (= ["interact" "wait" "collect"] (vec (distinct @order)))))))))

(deftest an-animal-key-limits-the-job-to-that-animal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:animal "u2"} {:entities [(led 1 3) (led 2 4)]} 6))]
          (is (finished? s))
          (is (= ["u2"] (:freed (done-event s))))
          (is (= [1] (still-leashed s)))
          (is (= 1 (count-of s "lead"))))))))

(deftest only-the-named-mob-is-freed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "cow"} {:entities [(led 1 3 {:name "pig"}) (led 2 4)]} 6))]
          (is (finished? s))
          (is (= ["u2"] (:freed (done-event s))))
          (is (= [1] (still-leashed s))))))))

(deftest unties-an-animal-from-a-fence-knot
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:entities [knot (tied 1 7)]} 8))]
          (is (finished? s))
          (is (= :unleashed (:reason (done-event s))))
          (is (empty? (still-leashed s)))
          (is (= 1 (count-of s "lead")))
          (is (= [50 1] (mapv #(.-id (.-args %)) (calls-of s "interact"))) "the knot is clicked first, then the animal it handed over")
          (is (empty? (filter #(= 50 (:id %)) (fake/entities (:p s)))) "and it is gone afterwards"))))))

(deftest ends-without-freeing-anything
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label world reason]
                [["nothing on a lead" {:entities [(animal 1 3)]} :none]
                 ["no animals" {:entities []} :none]
                 ["on a lead out of radius" {:entities [(led 1 30)]} :none]]]
          (let [s (await (scenario {} world 4))]
            (is (finished? s) label)
            (is (= reason (:reason (done-event s))) label)
            (is (empty? (calls-of s "interact")) label)
            (is (= [:unleash.gave-up] (mapv :kind (events-of s :unleash.gave-up))) label)))))))

(deftest collect-false-leaves-the-lead-on-the-ground
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:collect false} {:entities [(led 1 3)]} 6))]
          (is (finished? s))
          (is (= :unleashed (:reason (done-event s))))
          (is (empty? (calls-of s "collect")))
          (is (= 0 (count-of s "lead")))
          (is (= 1 (lead-items s))))))))

(deftest gives-up-on-an-unreachable-animal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:entities [(led 1 6)] :unreachable ["6,64,0"]} 5))]
          (is (finished? s))
          (is (= :unreachable (:reason (done-event s))))
          (is (empty? (calls-of s "interact")))
          (is (= [1] (still-leashed s))))))))

(deftest a-click-that-leaves-the-animal-on-its-lead-is-not-success
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (h/setup {:entities [(led 1 3)]})]
          (.override (.-world p) "interact" (fn [_token _args _impl] #js {:status "no-effect" :consumed 0 :worn 0 :love false :leash nil :changed #js {}}))
          (core/submit! eng '(jobs.animals.unleash {}) {})
          (await (run-ticks s 8 700))
          (is (finished? s))
          (is (not= :unleashed (:reason (done-event s))))
          (is (= [1] (still-leashed s))))))))

(deftest a-knot-that-cannot-be-seen-gives-up-without-a-click
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:entities [(tied 1 4)]} 5))]
          (is (finished? s))
          (is (= :refused (:reason (done-event s))))
          (is (empty? (calls-of s "interact"))))))))

(deftest an-animal-freed-a-moment-after-the-click-counts
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (h/setup {:entities [(led 1 3)]})
              waits (atom 0)]
          (.override (.-world p) "interact" (fn [_token _args _impl] #js {:status "no-effect" :consumed 0 :worn 0 :love false :leash nil :changed #js {}}))
          (.override (.-world p) "wait"
                     (fn [token args impl]
                       (when (= 2 (swap! waits inc))
                         (swap! (fake/state p) update :entities (partial mapv #(assoc % :leashed false :leashed-to-me false))))
                       (impl token args)))
          (core/submit! eng '(jobs.animals.unleash {}) {})
          (await (run-ticks s 8 700))
          (is (finished? s))
          (is (= :unleashed (:reason (done-event s))))
          (is (<= 2 @waits)))))))

(deftest unleash-follows-zones-and-claims
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra freed declined]
                [["Fake" {} ["u1" "u2"] []]
                 ["Miles" {} ["u2"] [:refused]]
                 ["Miles" {:ignore-zones? true} ["u1" "u2"] []]
                 [nil {} [] [:no-zones]]]]
          (let [s (h/setup {:entities [(led 1 3) (led 2 1)]} 0 (h/zone-store 3 owner))]
            (core/submit! (:eng s) (list 'jobs.animals.unleash extra) {})
            (await (run-ticks s 12 700))
            (is (= freed (vec (sort (:freed (done-event s))))) (pr-str owner extra))
            (is (= declined (mapv :reason (events-of s :unleash.declined))) (pr-str owner extra))))))))
