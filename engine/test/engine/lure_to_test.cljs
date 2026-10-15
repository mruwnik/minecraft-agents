(ns engine.lure-to-test
  "jobs.animals.lure-to against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.hostile-test :as h]
            [engine.test-util :as tu]
            [jobs.animals.escort :as escort]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.world-files :as ew]))

(defn cow [id x & [more]]
  (merge {:id id :uuid (str "u" id) :name "cow" :kind "passive" :pos {:x x :y 64 :z 0}} more))

(def wheat [{:name "wheat" :count 4}])
(def goal {:x 30 :y 64 :z 0})

(defn ^:async scenario [args world n]
  (let [{:keys [eng clock] :as s} (h/setup (merge {:floor tu/walk-floor} (dissoc world :seeing)) 50)]
    (when (:seeing world) (tu/seeing-all (:p s)))
    (core/submit! eng (list 'jobs.animals.lure-to (merge {:mob "cow" :pos goal} args)) {})
    (dotimes [_ n]
      (swap! clock + 700)
      (await (core/tick! eng)))
    s))

(defn done-event [{:keys [seen]}] (first (filter #(= :lure-to.done (:kind %)) @seen)))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn stopped [{:keys [seen]}] (filterv #(= :stopped (:kind %)) @seen))
(defn cow-of [{:keys [p]} id] (first (filter #(= id (:id %)) (fake/entities p))))
(defn count-of [{:keys [p]} item]
  (reduce + (map :count (filter #(= item (:name %)) (js->clj (.-inventory (.self p)) :keywordize-keys true)))))

(deftest lures-the-cow-to-the-spot-and-puts-the-food-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory wheat :entities [(cow 1 3)]} 12))
              c (cow-of s 1)]
          (is (finished? s))
          (is (= :arrived (:reason (done-event s))))
          (is (= "u1" (:animal (done-event s))))
          (is (<= (js/Math.abs (- 30 (first (:pos c)))) 5) "the cow stands near the spot")
          (is (<= (js/Math.abs (- 30 (:x (:animal-at (done-event s))))) 5) "the result says where the animal stands")
          (is (true? (:near (done-event s))))
          (is (= 4 (count-of s "wheat")) "no food is used")
          (is (nil? (.-held (.self (:p s)))) "the food is put away at the end"))))))

(deftest a-full-inventory-still-ends-with-something-else-than-the-food-in-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [full (into wheat (map (fn [i] {:name (str "item_" i) :count 1}) (range 35)))
              s (await (scenario {} {:inventory full :entities [(cow 1 3)]} 12))]
          (is (= :arrived (:reason (done-event s))))
          (is (not= "wheat" (.-held (.self (:p s)))) "unequip found no free slot: another item is taken in hand"))))))

(deftest one-call-is-the-whole-attempt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory wheat :entities [(cow 1 3)]} 1))]
          (is (finished? s))
          (is (= :arrived (:reason (done-event s)))))))))

(deftest no-food-ends-no-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory [] :entities [(cow 1 3)]} 12))]
          (is (finished? s))
          (is (= :no-food (:reason (done-event s))))
          (is (= 1 (count (stopped s)))))))))

(deftest a-failed-food-fetch-names-the-fetchs-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory [] :entities [(cow 1 3)]} 12))
              e (first (stopped s))]
          (is (= :no-food (:reason e)) (pr-str e))
          (is (= :no-source (:why e)) "the fetch's cause comes up, not just :no-food"))))))

(deftest a-chicken-lure-with-no-seeds-breaks-grass-for-them-first
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [grass (into {} (for [x (range 1 6) z (range 1 4)] [(str x ",64," z) "short_grass"]))
              s (await (scenario {:mob "chicken"}
                                 {:inventory [] :seeing true :blocks grass :drops {"short_grass" "wheat_seeds"}
                                  :entities [{:id 1 :uuid "u1" :name "chicken" :kind "passive" :pos {:x 3 :y 64 :z 0}}]}
                                 60))]
          (is (finished? s))
          (is (not= :no-food (:reason (done-event s))) "seeds were got from the grass")
          (is (= :arrived (:reason (done-event s)))))))))

(deftest no-animal-ends-no-animal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory wheat :entities []} 12))]
          (is (= :no-animal (:reason (done-event s))))
          (is (= 1 (count (stopped s)))))))))

(deftest an-animal-on-a-lead-is-not-lured
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:inventory wheat :entities [(cow 1 3 {:leashed true})]} 12))]
          (is (= :no-animal (:reason (done-event s)))))))))

(deftest the-food-is-held-again-when-it-leaves-the-hand-mid-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (h/setup (merge {:floor tu/walk-floor} {:inventory wheat :entities [(cow 1 3)]}) 50)
              moves (atom 0)]
          (.override (.-world p) "steer"
                     (fn [token a impl]
                       (when (= 3 (swap! moves inc)) (swap! (fake/state p) assoc-in [:self :held] nil))
                       (impl token a)))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal}) {})
          (dotimes [_ 12] (swap! clock + 700) (await (core/tick! eng)))
          (is (<= 3 @moves) "the food was knocked out of the hand during the walk")
          (is (= :arrived (:reason (done-event s))))
          (is (empty? (filter #(= :lead-broke (:reason %)) (map :data @(:seen s))))))))))

(deftest food-gone-from-the-inventory-mid-walk-ends-no-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (h/setup (merge {:floor tu/walk-floor} {:inventory wheat :entities [(cow 1 3)]}) 50)
              moves (atom 0)]
          (.override (.-world p) "steer"
                     (fn [token a impl]
                       (when (= 3 (swap! moves inc)) (swap! (fake/state p) #(-> % (assoc :inventory []) (assoc-in [:self :held] nil))))
                       (impl token a)))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal}) {})
          (dotimes [_ 12] (swap! clock + 700) (await (core/tick! eng)))
          (is (<= 3 @moves))
          (is (= :no-food (:reason (done-event s)))))))))

(def end-reasons
  "Cheap stubs for the reasons a lure can end with: [reason args hook], the hook given the fake primitives and a steer counter."
  [[:no-hold {} (fn [p _] (.override (.-world p) "equip" (fn [_ _ _] (js/Promise.resolve #js {:status "equipped"}))))]
   [:lost {} (fn [p steers]
               (.override (.-world p) "steer"
                          (fn [token a impl]
                            (when (= 3 (swap! steers inc)) (swap! (fake/state p) assoc :entities []))
                            (impl token a))))]])

(deftest a-lure-that-cannot-go-on-ends-with-its-reason-and-stops
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[reason args hook] end-reasons]
          (let [{:keys [p eng clock] :as s} (h/setup (merge {:floor tu/walk-floor} {:inventory wheat :entities [(cow 1 3)]}) 50)]
            (hook p (atom 0))
            (core/submit! eng (list 'jobs.animals.lure-to (merge {:mob "cow" :pos goal} args)) {})
            (dotimes [_ 14] (swap! clock + 700) (await (core/tick! eng)))
            (is (finished? s) (name reason))
            (is (= reason (:reason (done-event s))) (name reason))
            (is (= 1 (count (stopped s))) (name reason))))))))

(deftest a-lure-near-its-budget-puts-the-food-away-and-ends-timeout
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (h/setup {:floor tu/walk-floor :inventory wheat :entities [(cow 1 3)]} 50)]
          (.override (.-world p) "steer" (fn [token a impl] (swap! clock + 25000) (impl token a)))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal :spend {:job-time 30}}) {})
          (dotimes [_ 14] (swap! clock + 700) (await (core/tick! eng)))
          (is (finished? s))
          (is (= :timeout (:reason (done-event s)))))))))

(deftest an-animal-near-the-body-takes-over-when-the-led-one-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (h/setup {:floor tu/walk-floor :inventory wheat :entities [(cow 1 3)]} 50)
              steers (atom 0)]
          (.override (.-world p) "steer"
                     (fn [token a impl]
                       (when (= 3 (swap! steers inc))
                         (swap! (fake/state p) assoc :entities [])
                         (fake/add-entity! p (cow 2 (inc (first (:pos (fake/self p)))))))
                       (impl token a)))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal}) {})
          (dotimes [_ 14] (swap! clock + 700) (await (core/tick! eng)))
          (is (finished? s))
          (is (= :arrived (:reason (done-event s))))
          (is (= "u2" (:animal (done-event s)))))))))

(deftest a-baby-is-named-when-it-is-the-only-animal-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [baby (await (scenario {} {:inventory wheat :entities [(cow 2 3 {:baby true})]} 12))]
          (is (= :no-animal (:reason (done-event baby))))
          (is (re-find #"babies" (:text (first (stopped baby))))))))))

(deftest a-lost-lure-reports-what-it-last-saw
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (h/setup {:floor tu/walk-floor :inventory wheat :entities [(cow 1 3)]} 50)
              steers (atom 0)]
          (.override (.-world p) "steer"
                     (fn [token a impl]
                       (when (= 3 (swap! steers inc)) (swap! (fake/state p) assoc :entities []))
                       (impl token a)))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal}) {})
          (dotimes [_ 14] (swap! clock + 700) (await (core/tick! eng)))
          (let [lost (:lost (done-event s))]
            (is (= "u1" (:tracked lost)))
            (is (some? (:last-seen lost)))
            (is (some? (:last-seen (:lost (first (stopped s))))) "the stopped result carries the last sighting")
            (is (number? (:last-seen-dist lost)))
            (is (nil? (:nearest lost)))))))))

(def pen-box {:min {:x 28 :y 64 :z -2} :max {:x 34 :y 64 :z 2}})
(defn in-pen-box? [c] (let [[x _ z] (:pos c)] (and (<= 28 x 35) (<= -2 z 3))))

(deftest an-animal-already-in-the-destination-box-is-not-the-one-lured
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:box pen-box :radius 12 :pos {:x 34 :y 64 :z 0} :range 1}
                                 {:self {:pos {:x 26 :y 64 :z 0}} :inventory wheat :entities [(cow 1 29) (cow 2 21)]} 20))]
          (is (= :arrived (:reason (done-event s))))
          (is (= "u2" (:animal (done-event s))) "the penned cow stays")
          (is (in-pen-box? (cow-of s 2))))))))

(deftest arrived-means-the-led-animal-is-in-the-destination-not-just-the-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (h/setup {:floor tu/walk-floor :self {:pos {:x 22 :y 64 :z 0}} :inventory wheat :entities [(cow 1 20)]} 50)
              hold! (fn [token a impl]
                      (.then (impl token a)
                             (fn [r]
                               (when (>= (.. p self -pos -x) 24) (swap! (fake/state p) update :entities (fn [es] (mapv #(assoc % :pos [26 64 0]) es))))
                               r)))]
          (doseq [act ["steer" "wait"]] (.override (.-world p) act hold!))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal :box pen-box}) {})
          (loop [i 0] (when (and (< i 150) (not (finished? s))) (swap! clock + 700) (await (core/tick! eng)) (recur (inc i))))
          (is (finished? s))
          (is (= :animal-outside (:reason (done-event s))) (str "the cow stalled outside the box " (pr-str (done-event s))))
          (is (number? (:distance (done-event s))) "how far the animal stands from :pos")
          (is (re-find #"outside" (:text (first (stopped s)))))
          (is (not (in-pen-box? (cow-of s 1)))))))))

(deftest an-animal-left-5-blocks-short-of-the-spot-has-not-arrived
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (h/setup {:floor tu/walk-floor :self {:pos {:x 22 :y 64 :z 0}} :inventory wheat :entities [(cow 1 20)]} 50)
              hold! (fn [token a impl]
                      (.then (impl token a)
                             (fn [r]
                               (when (>= (.. p self -pos -x) 24) (swap! (fake/state p) update :entities (fn [es] (mapv #(assoc % :pos [25 64 0]) es))))
                               r)))]
          (doseq [act ["steer" "wait"]] (.override (.-world p) act hold!))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal :range 1}) {})
          (loop [i 0] (when (and (< i 150) (not (finished? s))) (swap! clock + 700) (await (core/tick! eng)) (recur (inc i))))
          (is (finished? s))
          (is (= :animal-outside (:reason (done-event s))) (pr-str (done-event s)))
          (is (= 5 (js/Math.round (:distance (done-event s))))))))))

(deftest a-cow-out-of-sight-at-the-spot-is-lost-not-arrived
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (h/setup {:floor tu/walk-floor :self {:pos {:x 26 :y 64 :z 0}} :inventory wheat :entities [(cow 1 24)]} 50)
              vanish! (fn [token a impl]
                        (.then (impl token a)
                               (fn [r]
                                 (when (>= (.. p self -pos -x) 27) (swap! (fake/state p) assoc :entities []))
                                 r)))]
          (.override (.-world p) "steer" vanish!)
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal}) {})
          (loop [i 0] (when (and (< i 150) (not (finished? s))) (swap! clock + 700) (await (core/tick! eng)) (recur (inc i))))
          (is (finished? s))
          (is (= :lost (:reason (done-event s))) (pr-str (done-event s)))
          (is (some? (:last-seen (:lost (done-event s)))))
          (is (number? (:distance (done-event s))) "the distance of the last sighting from :pos")
          (is (= 1 (count (stopped s)))))))))

(def gate-plan
  {"pen" {:id "pen" :parts [{:id "gate" :cells [[27 64 0]] :want "oak_fence_gate"}]}})

(deftest an-animal-stalled-in-the-gate-cell-has-not-arrived
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (h/setup {:floor tu/walk-floor :self {:pos {:x 22 :y 64 :z 0}} :inventory wheat :entities [(cow 1 20)]}
                                                   50 (ew/of-data gate-plan {} []))
              stall! (fn [token a impl]
                       (.then (impl token a)
                              (fn [r]
                                (when (>= (.. p self -pos -x) 24) (swap! (fake/state p) update :entities (fn [es] (mapv #(assoc % :pos [27.5 64 0.5]) es))))
                                r)))]
          (doseq [act ["steer" "wait"]] (.override (.-world p) act stall!))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal}) {})
          (loop [i 0] (when (and (< i 150) (not (finished? s))) (swap! clock + 700) (await (core/tick! eng)) (recur (inc i))))
          (is (finished? s))
          (is (= :animal-outside (:reason (done-event s))) (pr-str (done-event s))))))))

;; ------------------------------------------------------------------ which animal

(def ring
  (merge (tu/box 3 64 -2 12 64 -2 "oak_fence") (tu/box 3 64 2 12 64 2 "oak_fence")
         (tu/box 3 64 -1 3 64 1 "oak_fence") (tu/box 12 64 -1 12 64 1 "oak_fence")))

(deftest an-animal-in-a-closed-pen-is-not-the-one-lured
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} {:self {:pos {:x 14 :y 64 :z 0}} :inventory wheat :blocks ring :entities [(cow 1 11) (cow 2 20)]} 20))]
          (is (= "u2" (:animal (done-event s))) "the penned cow stays")
          (is (< (first (:pos (cow-of s 1))) 12) "the penned cow is still inside the ring"))))))

(deftest the-penned-animal-can-be-lured-when-asked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:include-penned? true} {:self {:pos {:x 14 :y 64 :z 0}} :inventory wheat :blocks ring :entities [(cow 1 11) (cow 2 20)]} 40))]
          (is (= "u1" (:animal (done-event s)))))))))

(deftest the-animal-named-by-uuid-is-the-only-one-led
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:uuid "u2"} {:self {:pos {:x 22 :y 64 :z 0}} :inventory wheat :entities [(cow 1 24) (cow 2 18)]} 20))]
          (is (= :arrived (:reason (done-event s))))
          (is (= "u2" (:animal (done-event s)))))))))

(deftest a-named-animal-that-is-not-there-ends-no-animal-naming-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:uuid "u9"} {:inventory wheat :entities [(cow 1 3)]} 12))]
          (is (= :no-animal (:reason (done-event s))))
          (is (re-find #"u9" (:text (first (stopped s))))))))))

(deftest a-named-animal-that-is-gone-is-lost-not-replaced
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (h/setup {:floor tu/walk-floor :inventory wheat :entities [(cow 1 3) (cow 2 4)]} 50)
              steers (atom 0)]
          (.override (.-world p) "steer"
                     (fn [token a impl]
                       (when (= 3 (swap! steers inc)) (swap! (fake/state p) update :entities (fn [es] (filterv #(= 2 (:id %)) es))))
                       (impl token a)))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal :uuid "u1"}) {})
          (dotimes [_ 20] (swap! clock + 700) (await (core/tick! eng)))
          (is (= :lost (:reason (done-event s))))
          (is (= "u1" (:tracked (:lost (done-event s)))))
          (is (re-find #"u1" (:text (first (stopped s))))))))))

;; ------------------------------------------------------------------ an animal that stops following

(defn self-x [p] (first (:pos (fake/self p))))

(defn ^:async run-until-done [{:keys [eng clock] :as s} n]
  (loop [i 0]
    (when (and (< i n) (not (finished? s)))
      (swap! clock + 700)
      (await (core/tick! eng))
      (recur (inc i))))
  s)

(defn after-acts!
  "After every steer and wait, (f p) runs: the scripted behaviour of an animal that is not following."
  [p f]
  (doseq [act ["steer" "wait"]]
    (.override (.-world p) act (fn [token a impl] (.then (impl token a) (fn [r] (f p act) r))))))

(defn set-cow-x! [p x] (swap! (fake/state p) update :entities (fn [es] (mapv #(assoc % :pos [x 64 0.5]) es))))

(deftest a-cow-that-stays-behind-ends-animal-stalled-with-both-positions
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng] :as s} (h/setup {:floor tu/walk-floor :inventory wheat :entities [(cow 1 3)]} 50)]
          (after-acts! p (fn [p _] (set-cow-x! p 3.5)))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal}) {})
          (await (run-until-done s 60))
          (let [e (done-event s)]
            (is (= :animal-stalled (:reason e)))
            (is (< (self-x p) 16) "the body stopped where it stood instead of walking on")
            (is (= 3.5 (get-in e [:stalled :animal :x])))
            (is (number? (get-in e [:stalled :body :x])))
            (is (< 7 (get-in e [:stalled :distance])))
            (is (re-find #"stopped following" (:text (first (stopped s)))))))))))

(deftest the-wait-for-an-animal-shows-both-positions-and-the-distance
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng seen] :as s} (h/setup {:floor tu/walk-floor :inventory wheat :entities [(cow 1 3)]} 50)]
          (after-acts! p (fn [p _] (set-cow-x! p 3.5)))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal}) {})
          (await (run-until-done s 60))
          (let [w (first (filter #(= :escort.waiting (:kind %)) @seen))]
            (is (some? w))
            (is (number? (get-in w [:animal-pos :x])))
            (is (number? (get-in w [:body-pos :x])))
            (is (< 7 (:distance w)))))))))

(deftest a-cow-that-fell-far-behind-but-keeps-closing-is-waited-for-not-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng] :as s} (h/setup {:floor tu/walk-floor :inventory wheat :entities [(cow 1 3)]} 50)
              cow-x (atom nil)]
          (after-acts! p (fn [p act]
                           (when (and (nil? @cow-x) (<= 8 (self-x p))) (reset! cow-x (- (self-x p) 12)))
                           (when @cow-x
                             (when (= "wait" act) (swap! cow-x + 0.1))
                             (set-cow-x! p @cow-x))))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal}) {})
          (await (run-until-done s 400))
          (is (some? @cow-x) "the cow fell behind")
          (is (= :arrived (:reason (done-event s)))))))))

(deftest a-cow-out-of-sight-is-walked-back-toward-where-it-was-last-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng] :as s} (h/setup {:floor tu/walk-floor :inventory wheat :entities [(cow 1 3)]} 50)
              hid-at (atom nil)]
          (after-acts! p (fn [p act]
                           (when (nil? @hid-at) (set-cow-x! p 3.5))
                           (when (and (nil? @hid-at) (= "wait" act))
                             (reset! hid-at (self-x p))
                             (swap! (fake/state p) assoc :entities []))))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal}) {})
          (await (run-until-done s 120))
          (is (some? @hid-at) "the cow went out of sight")
          (is (= :lost (:reason (done-event s))))
          (is (< (self-x p) (- @hid-at 3)) "the body went back toward the last sighting")
          (is (some? (get-in (done-event s) [:lost :last-seen]))))))))

(deftest a-plan-search-that-is-still-running-never-sends-the-body-the-whole-way-in-one-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wall (tu/box 15 64 -29 15 66 28 "stone")
              budget wsearch/round-budget]
          (set! wsearch/round-budget 1)
          (try
            (let [s (await (scenario {:pos {:x 30 :y 64 :z 0} :range 3}
                                     {:floor [-30 -30 40 30] :blocks wall :inventory wheat :entities [(cow 1 2)]} 6))]
              (is (< 10 (count (tu/walked-to (:eng s)))) (pr-str (tu/walked-to (:eng s)))))
            (finally (set! wsearch/round-budget budget))))))))

(deftest the-leg-plan-may-open-gates-as-go-to-plans-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (h/setup {:floor tu/walk-floor :inventory wheat} 50)
              opts (atom nil)]
          (with-redefs [walk/plan-walk! (fn ([_ _ _ _ _] (js/Promise.resolve nil)) ([_ _ _ _ _ o] (reset! opts o) (js/Promise.resolve nil)))]
            (await (escort/leg-target {:primitives p} {:x 30 :y 64 :z 0} 2)))
          (is (contains? (:moves (:policy @opts)) :open) "a planned gate is a way, not a wall"))))))
