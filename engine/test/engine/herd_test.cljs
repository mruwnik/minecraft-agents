(ns engine.herd-test
  "jobs.animals.herd against the fake world: a 5x5 pen (fence ring x 10..16, z 0..6) with a gate in its west wall."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.test-util :as tu]))

(def job 'jobs.animals.herd)

(def gate {:x 10 :y 64 :z 3})
(def gate-key "10,64,3")
(def box {:min {:x 11 :y 64 :z 1} :max {:x 15 :y 64 :z 5}})

(def ground
  (into {} (for [x (range -10 30) z (range -10 16)] [(str x ",63," z) "stone"])))

(def fence-ring
  (into {} (for [x (range 10 17) z (range 0 7) :when (or (#{10 16} x) (#{0 6} z))] [(str x ",64," z) "oak_fence"])))

(defn cow [id x z & [more]]
  (merge {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z z}} more))

(def wheat {:name "wheat" :count 4})

(defn leads
  "n leads and the cows' food."
  [n]
  [{:name "lead" :count n} wheat])

(defn world
  "The pen with its gate shut, the body west of it; more blocks and states merged over, other world keys passed on."
  [{:keys [entities inventory blocks states] :as w}]
  (merge w
         {:self {:pos {:x 2 :y 64 :z 3}}
          :inventory (or inventory (leads 2))
          :entities (or entities [])
          :blocks (merge ground fence-ring {gate-key "oak_fence_gate"} blocks)
          :states (merge {gate-key {:open false}} states)}))

(defn ^:async run-ticks
  [{:keys [eng clock]} n]
  (dotimes [_ n]
    (swap! clock + 700)
    (await (core/tick! eng))))

(defn submit! [s args]
  (core/submit! (:eng s) (list job (merge {:mob "cow" :box box} args)) {})
  s)

(defn ^:async scenario [args w n]
  (let [s (submit! (h/setup (world w)) args)]
    (await (run-ticks s n))
    s))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event [s] (first (events-of s :herd.done)))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn calls-of [{:keys [p]} name] (h/calls p name))
(defn entities-of [{:keys [p]}] (.. p -world -state -entities))
(defn cow-of [s id] (first (filter #(= id (.-id %)) (entities-of s))))
(defn in-pen? [c] (let [{:keys [x z]} (js->clj (.-pos c) :keywordize-keys true)] (and (<= 11 x 15) (<= 1 z 5))))
(defn on-lead [s] (mapv #(.-id %) (filter #(true? (.-leashedToMe %)) (entities-of s))))
(defn gate-open? [{:keys [p]}] (true? (some-> (.blockAt p (clj->js gate)) .-properties .-open)))
(defn self-x [{:keys [p]}] (.. p self -pos -x))
(defn count-of [{:keys [p]} item]
  (reduce + (map :count (filter #(= item (:name %)) (js->clj (.-inventory (.self p)) :keywordize-keys true)))))
(defn clicked-ids [s] (set (map #(.. % -args -id) (calls-of s "interact"))))
(defn held [{:keys [p]}] (.-held (.self p)))

(deftest brings-only-the-missing-ones-and-shuts-the-gate-behind-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 3}
                                 {:entities [(cow 9 13 3) (cow 1 4 3) (cow 2 6 4) (cow 3 -8 12)]}
                                 40))
              e (done-event s)]
          (is (finished? s))
          (is (= :brought (:reason e)))
          (is (= 3 (:inside e)))
          (is (= #{"u1" "u2"} (set (:brought e))))
          (is (every? in-pen? [(cow-of s 1) (cow-of s 2) (cow-of s 9)]))
          (is (not (in-pen? (cow-of s 3))) "the third cow outside was not wanted")
          (is (not (contains? (clicked-ids s) 9)) "the cow already inside is never touched")
          (is (empty? (on-lead s)))
          (is (not (gate-open? s)) "the gate is shut")
          (is (< (self-x s) 10) "the body ends outside the pen")
          (is (= 2 (count-of s "lead")) "both leads are back")
          (is (= 4 (count-of s "wheat")) "the lure costs no food")
          (is (nil? (held s)) "the food is put away")
          (is (empty? (events-of s :herd.gave-up))))))))

(deftest the-pens-own-animals-stay-in-while-the-gate-stands-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 3}
                                 {:entities [(cow 8 11 3) (cow 9 11 2) (cow 1 4 3)]}
                                 40))
              e (done-event s)]
          (is (= :brought (:reason e)))
          (is (every? in-pen? [(cow-of s 8) (cow-of s 9) (cow-of s 1)]))
          (is (not (gate-open? s))))))))

(deftest a-gate-standing-open-at-the-start-is-used-and-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:entities [(cow 1 4 3)] :states {gate-key {:open true}}} 40))]
          (is (= :brought (:reason (done-event s))))
          (is (not (gate-open? s))))))))

(deftest a-full-pen-declines-without-acting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 2} {:entities [(cow 8 12 2) (cow 9 13 3) (cow 1 4 3)]} 5))]
          (is (nil? (done-event s)))
          (is (empty? (calls-of s "moveTo")))
          (is (empty? (calls-of s "interact"))))))))

(deftest babies-are-not-fetched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:entities [(cow 1 3 3 {:baby true}) (cow 2 6 3)]} 40))]
          (is (= :brought (:reason (done-event s))))
          (is (= ["u2"] (:brought (done-event s))))
          (is (not (in-pen? (cow-of s 1)))))))))

(deftest fewer-than-wanted-ends-short-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 3} {:inventory (leads 3) :entities [(cow 1 4 3)]} 40))
              e (done-event s)]
          (is (finished? s))
          (is (= :short (:reason e)))
          (is (= 1 (:inside e)))
          (is (= ["u1"] (:brought e)))
          (is (not (gate-open? s)))
          (is (= 1 (count (events-of s :herd.gave-up)))))))))

(deftest gives-up-before-touching-the-gate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label w reason]
                [["no lead" {:inventory [wheat] :entities [(cow 1 4 3)]} :no-lead]
                 ["no food" {:inventory [{:name "lead" :count 2}] :entities [(cow 1 4 3)]} :no-food]
                 ["no cow outside" {:entities [(cow 9 13 3)]} :none]
                 ["a gap in the fence" {:entities [(cow 1 4 3)] :blocks {"16,64,3" "air"}} :leaky]
                 ["no gate" {:entities [(cow 1 4 3)] :blocks {gate-key "oak_fence"}} :no-gate]
                 ["only a corner gate" {:entities [(cow 1 4 3)] :blocks {gate-key "oak_fence" "10,64,0" "oak_fence_gate"}} :no-gate]]]
          (let [s (await (scenario {:target 2} w 20))]
            (is (finished? s) label)
            (is (= reason (:reason (done-event s))) label)
            (is (empty? (calls-of s "useOn")) label)
            (is (empty? (on-lead s)) label)
            (is (= 1 (count (events-of s :herd.gave-up))) label)))))))

(deftest led-animals-are-let-go-when-the-gate-cannot-be-reached
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:entities [(cow 1 4 3)] :unreachable ["9,64,3"]} 30))]
          (is (finished? s))
          (is (= :unreachable (:reason (done-event s))))
          (is (empty? (on-lead s)) "never left tethered to the body")
          (is (= 2 (count-of s "lead")))
          (is (empty? (calls-of s "useOn")))
          (is (= 1 (count (events-of s :herd.gave-up)))))))))

(deftest a-gate-that-will-not-open-is-given-up-and-the-animals-let-go
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:entities [(cow 1 4 3)] :states {gate-key {:open false :locked true}}} 30))]
          (is (finished? s))
          (is (= :gate-stuck (:reason (done-event s))))
          (is (empty? (on-lead s)))
          (is (not (gate-open? s)))
          (is (= 1 (count (events-of s :herd.gave-up)))))))))

(deftest a-lead-that-keeps-breaking-is-given-up-after-one-retry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 1} {:entities [(cow 1 4 3 {:snaps true})]} 40))
              e (done-event s)]
          (is (finished? s))
          (is (= :lost (:reason e)))
          (is (= {"u1" :lead-broke} (:given-up e)))
          (is (= 2 (count (filter #(= "lead" (.. % -args -item)) (calls-of s "interact")))) "leashed twice, not more")
          (is (empty? (calls-of s "useOn")) "the gate is never opened")
          (is (= 1 (count (events-of s :herd.gave-up)))))))))

(deftest leads-dropped-mid-way-are-picked-up-and-the-animals-leashed-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (submit! (h/setup (world {:entities [(cow 1 4 3) (cow 2 6 4)]})) {:target 2})]
          (loop [n 0]
            (when (and (< n 20) (< (count (on-lead s)) 2))
              (await (run-ticks s 1))
              (recur (inc n))))
          (is (= 2 (count (on-lead s))) "both on the lead before the cut")
          ;; what a log-out does: the leads come off and lie on the ground
          (doseq [c (filter #(true? (.-leashedToMe %)) (entities-of s))]
            (set! (.-leashed c) false)
            (set! (.-leashedToMe c) false)
            (.push (entities-of s) (clj->js {:id (+ 100 (.-id c)) :name "item" :kind "item" :pos (js->clj (.-pos c) :keywordize-keys true)
                                             :item {:name "lead" :count 1}})))
          (await (run-ticks s 40))
          (is (finished? s))
          (is (= :brought (:reason (done-event s))))
          (is (every? in-pen? [(cow-of s 1) (cow-of s 2)]))
          (is (not (gate-open? s)))
          (is (= 2 (count-of s "lead"))))))))
