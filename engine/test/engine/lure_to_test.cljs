(ns engine.lure-to-test
  "jobs.animals.lure-to against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.expr :as expr]
            [engine.registry :as registry]
            [engine.fake :as fake]
            [engine.hostile-test :as h]
            [engine.hunt-test :as hunt-test]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.animals.escort :as escort]
            [jobs.animals.lure-to :as lure-to]
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

(defn done-event
  "The job's ending event (job.completed or job.stopped)."
  [{:keys [seen]}]
  (first (filter #(and (= :job (:source %)) (#{:completed :stopped} (:kind %))) @seen)))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn stopped [{:keys [seen]}] (filterv #(= :stopped (:kind %)) @seen))
(defn cow-of [{:keys [p]} id] (first (filter #(= id (:id %)) (fake/entities p))))
(defn count-of [{:keys [p]} item]
  (reduce + (map :count (filter #(= item (:name %)) (js->clj (.-inventory (.self p)) :keywordize-keys true)))))

(defn ctx-of
  "A ctx of lure-to in a seen world, args over its defaults, its memory m."
  [world args m]
  {:primitives (tu/seeing-all (tu/fake world)) :args (second (expr/leaf registry/jobs 'jobs.animals.lure-to (merge {:mob "cow" :pos goal} args)))
   :root "j" :slots [] :id "j" :engine {:warned (atom #{})} :emit (fn [& _]) :sym 'jobs.animals.lure-to
   :view (fn [] {:now 1000 :data {:entries {(mem/job-kind "j") [{:t 1000 :data m}]}}})})

(deftest done-is-ok-for-the-named-animal-already-at-the-spot-before-the-start
  (are [world args m expected] (= expected (tu/kind-reason (lure-to/done? (ctx-of world args m))))
    {:entities [(cow 1 30)]} {:uuid "u1"} {} [:ok :arrived]
    {:entities [(cow 1 3)]} {:uuid "u1"} {} [:later :luring]
    {:entities [(cow 1 30)]} {} {} [:later :luring]
    {:entities [(cow 1 30)]} {:uuid "u1"} {:animal "u1"} [:later :luring]))

(deftest the-animal-sighted-at-the-start-stays-a-candidate-after-it-wandered-past-the-radius
  (are [m expected] (= expected (mapv #(.-uuid %) (lure-to/free-adults (ctx-of {:self {:pos {:x 0 :y 64 :z 0}} :entities [(cow 1 12)]} {:radius 8} (merge {:origin {:x 0 :y 64 :z 0}} m)))))
    {} []
    {:sighting {:key "u1" :pos {:x 9 :y 64 :z 0} :at 1000}} ["u1"]
    {:sighting {:key "u2" :pos {:x 9 :y 64 :z 0} :at 1000}} []))

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
          (await (tu/run-ticks eng clock 12 700))
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
          (await (tu/run-ticks eng clock 12 700))
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
            (await (tu/run-ticks eng clock 14 700))
            (is (finished? s) (name reason))
            (is (= reason (:reason (done-event s))) (name reason))
            (is (= 1 (count (stopped s))) (name reason))))))))

(deftest a-lure-out-of-time-ends-on-the-engines-limit-and-its-cleanup-puts-the-food-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (h/setup {:floor tu/walk-floor :inventory wheat :entities [(cow 1 3)]} 50)]
          (.override (.-world p) "steer" (fn [token a impl] (swap! clock + 25000) (impl token a)))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal :spend {:job-time 30}}) {})
          (await (tu/run-ticks eng clock 14 700))
          (is (finished? s))
          (is (= [:job-time] (mapv :reason (stopped s))) "the engine's limit ends it")
          (is (nil? (.-held (.self p))) "the cleanup puts the food away"))))))

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
          (await (tu/run-ticks eng clock 14 700))
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
          (await (tu/run-ticks eng clock 14 700))
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

(deftest a-body-stopping-short-of-the-box-leads-on-until-the-animal-is-in-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng clock] :as s} (h/setup {:floor tu/walk-floor :self {:pos {:x 22 :y 64 :z 0}} :inventory wheat :entities [(cow 1 20)]} 50)
              ;; a tempted cow stops 2.4 blocks behind the food, so a body at the box edge leaves it outside
              trail! (fn [token a impl]
                       (.then (impl token a)
                              (fn [r]
                                (let [bx (.. p self -pos -x)]
                                  (when (>= bx 24) (swap! (fake/state p) update :entities (fn [es] (mapv #(assoc % :pos [(- bx 2.4) 64 0]) es)))))
                                r)))]
          (doseq [act ["steer" "wait"]] (.override (.-world p) act trail!))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal :box pen-box}) {})
          (loop [i 0] (when (and (< i 150) (not (finished? s))) (swap! clock + 700) (await (core/tick! eng)) (recur (inc i))))
          (is (= :arrived (:reason (done-event s))) (pr-str (done-event s)))
          (is (in-pen-box? (cow-of s 1))))))))

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
          (is (re-find #"last seen at .* blocks from the body" (:text (first (stopped s)))) (pr-str (:text (first (stopped s)))))
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
          (await (tu/run-ticks eng clock 20 700))
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

(deftest a-cow-swimming-in-slowly-is-not-given-up-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng] :as s} (h/setup {:floor tu/walk-floor :inventory wheat :entities [(cow 1 3)]} 50)
              cow-x (atom nil)]
          (after-acts! p (fn [p act]
                           (when (and (nil? @cow-x) (<= 8 (self-x p))) (reset! cow-x (- (self-x p) 12)))
                           (when @cow-x
                             (when (= "wait" act) (swap! cow-x + 0.0015))
                             (set-cow-x! p @cow-x))))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal}) {})
          (await (run-until-done s 60))
          (is (some? @cow-x) "the cow fell behind")
          (is (not= :animal-stalled (:reason (done-event s))) "a third of a block in 10 s is still closing in"))))))

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

(deftest the-animal-is-named-by-the-start-of-its-uuid
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:uuid "ab12"} {:self {:pos {:x 22 :y 64 :z 0}} :inventory wheat
                                               :entities [(cow 1 24 {:uuid "e60084c6-1111"}) (cow 2 18 {:uuid "ab12cd34-2222"})]} 20))]
          (is (= "ab12cd34-2222" (:animal (done-event s)))))))))

(deftest unreachable-inside-a-shut-pen-names-the-pen
  (let [c {:primitives (tu/fake {:floor tu/walk-floor :blocks ring}) :args {:mob "cow" :pos {:x 7 :y 64 :z 0}}}]
    (is (re-find #"closed pen" (lure-to/stop-text c :unreachable)))))

(deftest an-ambiguous-or-empty-uuid-start-is-refused-naming-the-matches
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[uuid pat] [["ab12" #"matches 2 animals"] ["" #"empty"]]]
          (let [s (await (scenario {:uuid uuid} {:inventory wheat
                                                 :entities [(cow 1 3 {:uuid "ab12-1111"}) (cow 2 4 {:uuid "ab12-2222"})]} 8))]
            (is (finished? s) (pr-str uuid))
            (is (= :stopped (:kind (done-event s))) (pr-str (done-event s)))
            (is (re-find pat (pr-str (done-event s))) (pr-str uuid))
            (is (empty? (tu/walked-to (:eng s))) "no animal is led")))))))

(def gate-lane
  "A fence across the lane at x 15 (floor under it) with a shut gate at z 0."
  (merge (tu/box 15 63 -10 15 63 10 "stone") (tu/box 15 64 -10 15 64 10 "oak_fence") {"15,64,0" "oak_fence_gate"}))

(defn gate-open? [{:keys [p]}]
  (:open (js->clj (.-properties (.blockAt p #js {:x 15 :y 64 :z 0})) :keywordize-keys true)))

(defn gate-world []
  {:floor tu/walk-floor :inventory wheat :entities [(cow 1 3)] :blocks gate-lane
   :states {"15,64,0" {:open false :facing "east"}}})

(defn ^:async tick-n [{:keys [clock eng]} n]
  (dotimes [_ n] (swap! clock + 700) (await (core/tick! eng))))

(defn watch-gate!
  "The atom holds whether the gate was ever open after a steer; (f) is called once the gate is open and the cow is through it."
  [{:keys [p] :as s} f]
  (let [seen-open (atom false)
        called (atom false)]
    (.override (.-world p) "steer"
               (fn [token a impl]
                 (.then (impl token a)
                        (fn [r]
                          (when (gate-open? s) (reset! seen-open true))
                          (when (and (gate-open? s) (< 17 (first (:pos (cow-of s 1)))) (not @called)) (reset! called true) (f))
                          r))))
    seen-open))

(deftest the-gate-the-legs-opened-is-shut-after-the-lure-arrived
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng] :as s} (h/setup (gate-world) 50)
              opened (watch-gate! s identity)]
          (tu/seeing-all p)
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal}) {})
          (await (tick-n s 40))
          (is (true? @opened) "the legs opened the gate")
          (is (= :arrived (:reason (done-event s))) (pr-str (done-event s)))
          (is (false? (gate-open? s)) "the cleanup shut the gate behind the cow and the body"))))))

(deftest the-gate-the-legs-opened-is-shut-after-a-cancel-and-another-jobs-gate-stays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (-> (gate-world) (update :blocks assoc "-20,64,0" "oak_fence_gate") (assoc :states {"15,64,0" {:open false :facing "east"} "-20,64,0" {:open true :facing "east"}}))
              {:keys [eng p] :as s} (h/setup world 50)
              store (:store eng)
              other-gate #(:open (js->clj (.-properties (.blockAt p #js {:x -20 :y 64 :z 0})) :keywordize-keys true))
              opened (watch-gate! s #(core/cancel! eng (first (:list (core/state eng)))))]
          (tu/seeing-all p)
          (swap! store update :data mem/add-entry :opened {:t ((:now @store)) :data {:cell {:x -20 :y 64 :z 0} :by "other-job/1" :shut? false}} nil)
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal}) {})
          (await (tick-n s 40))
          (is (true? @opened) "the legs opened the gate")
          (is (finished? s))
          (is (false? (gate-open? s)) "the cleanup shut it after the cancel")
          (is (true? (other-gate)) "another job's open gate is left")
          (is (nil? (.-held (.self p))) "the food is put away"))))))

(deftest unreachable-names-a-spot-with-no-standable-cell-and-the-ground-of-its-column
  (let [c (fn [y] {:primitives (tu/fake {:floor tu/walk-floor}) :args {:mob "cow" :pos {:x 7 :y y :z 0}}})]
    (is (re-find #"no standable cell at y70.*ground at y64" (lure-to/stop-text (c 70) :unreachable)))
    (is (not (re-find #"standable" (lure-to/stop-text (c 64) :unreachable))) "a standable spot is not blamed")))

(deftest a-cow-only-heard-is-stepped-toward-and-then-lured
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng] :as s} (h/setup {:floor tu/walk-floor :inventory wheat :entities [(cow 1 10)]} 50)]
          (hunt-test/hear-passives-until! p (fn [{:keys [x]}] (<= 3 x)))
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal :radius 24}) {})
          (await (run-until-done s 150))
          (is (= :arrived (:reason (done-event s)))))))))

(defn hide-until-seen-from!
  "The cow (x 12) is out of sight once the food is in hand, and in sight again once the body has looked from x or beyond."
  [p x]
  (let [looked? #(some (fn [call] (= "look" (.-name call))) (.-calls (.-world p)))
        cow-es (:entities @(fake/state p))
        hidden (atom false)
        sync! #(swap! (fake/state p) assoc :entities
                      (if (and @hidden (not (and (looked?) (<= x (self-x p))))) [] cow-es))]
    (.override (.-world p) "equip" (fn [token a impl] (.then (impl token a) (fn [r] (reset! hidden true) (sync!) r))))
    (.override (.-world p) "look" (fn [token a impl] (.then (impl token a) (fn [r] (sync!) r))))))

(deftest a-cow-lost-from-sight-on-the-way-in-is-looked-for-where-it-was-seen
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p eng] :as s} (h/setup {:floor tu/walk-floor :inventory wheat :entities [(cow 1 12)]} 50)]
          (hide-until-seen-from! p 8)
          (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal :radius 20}) {})
          (await (run-until-done s 150))
          (is (not= :no-animal (:reason (done-event s)))))))))

(deftest a-sighting-older-than-the-age-limit-is-not-walked-back-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (settings/with-settings {::lure-to/sighting-age-s 0}
          (fn ^:async f []
            (let [{:keys [p eng] :as s} (h/setup {:floor tu/walk-floor :inventory wheat :entities [(cow 1 12)]} 50)]
                  (hide-until-seen-from! p 8)
              (core/submit! eng (list 'jobs.animals.lure-to {:mob "cow" :pos goal :radius 20}) {})
              (await (run-until-done s 150))
              (is (= :no-animal (:reason (done-event s)))))))))))
