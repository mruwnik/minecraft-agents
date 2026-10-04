(ns engine.lead-to-test
  "jobs.animals.lead-to against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.test-util :as tu]))

(defn cow [id x & [more]]
  (merge {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}} more))

(def lead [{:name "lead" :count 1}])
(def fence {"31,64,0" "oak_fence"})
(def goal {:x 30 :y 64 :z 0})

(defn ^:async run-ticks
  [{:keys [eng clock]} n step]
  (dotimes [_ n]
    (swap! clock + step)
    (await (core/tick! eng))))

(defn ^:async submit
  "Submit (jobs.animals.lead-to args) in the setup s; run n ticks 700 ms apart."
  [s args n]
  (core/submit! (:eng s) (list 'jobs.animals.lead-to (merge {:mob "cow" :pos goal} args)) {})
  (await (run-ticks s n 700))
  s)

(defn ^:async scenario [args world n]
  (await (submit (h/setup world) args n)))

(defn done-event [{:keys [seen]}] (first (filter #(= :lead-to.done (:kind %)) @seen)))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn calls-of [{:keys [p]} name] (h/calls p name))
(defn count-of [{:keys [p]} item]
  (reduce + (map :count (filter #(= item (:name %)) (js->clj (.-inventory (.self p)) :keywordize-keys true)))))
(defn cow-of [{:keys [p]} id] (first (filter #(= id (.-id %)) (.. p -world -state -entities))))

(deftest leads-the-cow-to-the-spot-and-lets-it-go
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3)]} 12))
              c (cow-of s 1)]
          (is (finished? s))
          (is (= :unleashed (:reason (done-event s))))
          (is (= "u1" (:animal (done-event s))))
          (is (< (js/Math.abs (- 30 (.-x (.-pos c)))) 4) "the cow stands at the spot")
          (is (not (true? (.-leashed c))))
          (is (= 1 (count-of s "lead")) "the lead is back in the inventory")
          (is (empty? (events-of s :lead-to.gave-up))))))))

(deftest ties-the-cow-to-the-named-fence
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:fence {:x 31 :y 64 :z 0}} {:inventory lead :blocks fence :entities [(cow 1 3)]} 12))
              c (cow-of s 1)]
          (is (finished? s))
          (is (= :tied (:reason (done-event s))))
          (is (true? (.-leashed c)))
          (is (not (true? (.-leashedToMe c))) "held by the post, not by the body")
          (is (= 0 (count-of s "lead")) "the lead stays on the cow")
          (is (empty? (events-of s :lead-to.gave-up))))))))

(deftest declines-before-touching-anything
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label args world reason]
                [["no lead" {} {:entities [(cow 1 3)]} :no-lead]
                 ["no cow" {} {:inventory lead :entities []} :none]
                 ["no fence at the named cell" {:fence {:x 31 :y 64 :z 0}} {:inventory lead :entities [(cow 1 3)]} :no-fence]
                 ["a block that is not a fence" {:fence {:x 31 :y 64 :z 0}} {:inventory lead :blocks {"31,64,0" "stone"} :entities [(cow 1 3)]} :no-fence]]]
          (let [s (await (scenario args world 5))]
            (is (finished? s) label)
            (is (= reason (:reason (done-event s))) label)
            (is (empty? (calls-of s "moveTo")) label)
            (is (= [:lead-to.gave-up] (mapv :kind (events-of s :lead-to.gave-up))) label)))))))

(deftest a-lead-that-breaks-on-the-way-is-reported-not-succeeded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3 {:snaps true})]} 12))
              c (cow-of s 1)]
          (is (finished? s))
          (is (= :lead-broke (:reason (done-event s))))
          (is (= "u1" (:animal (done-event s))))
          (is (not (true? (.-leashed c))))
          (is (empty? (calls-of s "useOn")) "no tie was tried")
          (is (= [:lead-to.gave-up] (mapv :kind (events-of s :lead-to.gave-up)))))))))

(deftest a-cow-that-is-gone-after-the-walk-is-lost
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p] :as s} (h/setup {:inventory lead :entities [(cow 1 3)]})]
          (.override (.-world p) "moveTo"
                     (fn [token args impl]
                       (let [r (impl token args)]
                         (.splice (.. p -world -state -entities) 0 1)
                         r)))
          (await (submit s {} 12))
          (is (finished? s))
          (is (= :lost (:reason (done-event s)))))))))

(deftest an-unreachable-spot-ends-with-the-cow-still-on-the-lead
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory lead :entities [(cow 1 3)] :unreachable ["30,64,0"]} 14))
              c (cow-of s 1)]
          (is (finished? s))
          (is (= :unreachable (:reason (done-event s))))
          (is (true? (.-leashedToMe c)))
          (is (true? (:still-led (done-event s)))))))))
