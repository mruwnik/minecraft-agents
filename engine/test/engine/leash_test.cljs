(ns engine.leash-test
  "jobs.animals.leash against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.test-util :as tu]))

(defn cow [id x & [more]]
  (merge {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}} more))

(def lead [{:name "lead" :count 2}])

(defn ^:async run-ticks
  [{:keys [eng clock]} n step]
  (dotimes [_ n]
    (swap! clock + step)
    (await (core/tick! eng))))

(defn ^:async scenario
  "Submit (jobs.animals.leash args) in a world; run n ticks 700 ms apart; the setup map."
  [args world n]
  (let [s (h/setup world)]
    (core/submit! (:eng s) (list 'jobs.animals.leash (merge {:mob "cow"} args)) {})
    (await (run-ticks s n 700))
    s))

(defn done-event [{:keys [seen]}] (first (filter #(= :leash.done (:kind %)) @seen)))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn calls-of [{:keys [p]} name] (h/calls p name))
(defn count-of [{:keys [p]} item]
  (reduce + (map :count (filter #(= item (:name %)) (js->clj (.-inventory (.self p)) :keywordize-keys true)))))
(defn on-lead [{:keys [p]}]
  (mapv #(.-id %) (filter #(true? (.-leashedToMe %)) (.. p -world -state -entities))))

(deftest leashes-the-nearest-cow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 5) (cow 2 3)]} 4))]
          (is (finished? s))
          (is (= :leashed (:reason (done-event s))))
          (is (= "u2" (:animal (done-event s))))
          (is (= [2] (on-lead s)))
          (is (= 1 (count-of s "lead")))
          (is (empty? (events-of s :leash.gave-up))))))))

(deftest a-cow-already-on-a-lead-is-left-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 2 {:leashed true :leashHolder 99}) (cow 2 4)]} 4))]
          (is (finished? s))
          (is (= :leashed (:reason (done-event s))))
          (is (= [2] (on-lead s))))))))

(deftest skips-the-animals-it-is-told-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:skip ["u2"]} {:inventory lead :entities [(cow 1 5) (cow 2 3)]} 4))]
          (is (= "u1" (:animal (done-event s))))
          (is (= [1] (on-lead s))))
        (let [s (await (scenario {:skip ["u1" "u2"]} {:inventory lead :entities [(cow 1 5) (cow 2 3)]} 4))]
          (is (= :none (:reason (done-event s))))
          (is (empty? (on-lead s))))))))

(deftest ends-without-leashing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label world reason]
                [["no lead" {:entities [(cow 1 2)]} :no-lead]
                 ["no cow" {:inventory lead :entities [{:id 1 :uuid "u1" :name "pig" :kind "passive" :pos {:x 2 :y 64 :z 0}}]} :none]
                 ["cow out of radius" {:inventory lead :entities [(cow 1 30)]} :none]
                 ["all on leads" {:inventory lead :entities [(cow 1 2 {:leashed true :leashHolder 99})]} :all-leashed]]]
          (let [s (await (scenario {} world 4))]
            (is (finished? s) label)
            (is (= reason (:reason (done-event s))) label)
            (is (empty? (calls-of s "interact")) label)
            (is (= [:leash.gave-up] (mapv :kind (events-of s :leash.gave-up))) label)))))))

(deftest gives-up-on-an-unreachable-cow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 6)] :unreachable ["6,64,0"]} 5))]
          (is (finished? s))
          (is (= :unreachable (:reason (done-event s))))
          (is (empty? (calls-of s "interact")))
          (is (empty? (on-lead s))))))))

(deftest a-use-the-server-ignores-is-not-a-leash
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 2 {:accepts false})]} 5))]
          (is (finished? s))
          (is (= :refused (:reason (done-event s))))
          (is (= 2 (count-of s "lead")))
          (is (empty? (on-lead s))))))))

(deftest a-use-that-reports-a-lead-but-leaves-the-cow-free-is-not-success
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (h/setup {:inventory lead :entities [(cow 1 2)]})]
          (.override (.-world p) "interact"
                     (fn [token args impl]
                       (let [r (impl token args)]
                         (doseq [e (.. p -world -state -entities)] (set! (.-leashedToMe e) false))
                         r)))
          (core/submit! eng '(jobs.animals.leash {:mob "cow"}) {})
          (await (run-ticks s 6 700))
          (is (finished? s))
          (is (not= :leashed (:reason (done-event s)))))))))

(deftest a-cow-that-leaves-before-the-lead-does-not-stall-the-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (h/setup {:inventory lead :entities [(cow 1 2) (cow 2 4)]})]
          (.override (.-world p) "interact"
                     (fn [token args impl]
                       (let [es (.. p -world -state -entities)]
                         (when (= 1 (.-id args))
                           (.splice es 0 1))
                         (impl token args))))
          (core/submit! eng '(jobs.animals.leash {:mob "cow"}) {})
          (await (run-ticks s 8 700))
          (is (finished? s))
          (is (= :leashed (:reason (done-event s))))
          (is (= [2] (on-lead s))))))))
