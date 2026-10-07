(ns engine.village-feed-test
  "jobs.village.feed against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.hostile-test :as h]
            [engine.perception :as perception]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.village.feed :as feed]))

(def job 'jobs.village.feed)

(defn villager
  "A villager entity at x along the row the body stands on."
  [x & [extra]]
  (merge {:id 1 :name "villager" :kind "passive" :uuid "v-1" :pos {:x x :y 64 :z 0}} extra))

(def bread [{:name "bread" :count 5}])

(defn taking
  "An override of toss that adds a pickup receipt: the drop is gone and who took it is named by (taker n) ({uuid n}); a
  taker of nil leaves the drop lying."
  [p taker]
  (.override (.-world p) "toss"
             (fn ^:async f [token a impl]
               (let [r (await (impl token a))
                     by (when (= "tossed" (.-status r)) (taker (.-count r)))]
                 (when (seq by)
                   (swap! (fake/state p) update :entities #(filterv (fn [e] (not= "item" (:kind e))) %)))
                 (set! (.-takenBy r) (clj->js (or by {})))
                 r))))

(defn ^:async feed
  "Run the job on a world until it ends (at most n ticks); prepare is called with the primitives first. The child's
  result is in :out."
  [world args n prepare]
  (let [s (h/setup world)
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
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

(defn inv [p] (reduce (fn [m i] (update m (.-name i) (fnil + 0) (.-count i))) {} (.-inventory (.self p))))
(defn kinds [{:keys [seen]}] (set (map :kind @seen)))
(defn tosses [p] (h/calls p "toss"))
(defn gave? [{:keys [seen]} kind] (some #(= kind (:kind %)) @seen))

(defn all-taken [uuid] (fn [n] {uuid n}))

(deftest check-wants-a-uuid-and-a-food-a-villager-takes
  (are [args ok] (= ok (boolean ((:check (get registry/jobs job)) {:args args :primitives nil :mem (constantly {})})))
    {:villager nil :item "bread"} false
    {:villager "v-1" :item "diamond"} false))

(deftest feeds-the-count-and-names-the-villager-in-the-receipt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (feed {:entities [(villager 6)] :inventory bread} {:villager "v-1" :count 2} 40
                                                 #(taking % (all-taken "v-1"))))]
          (is (= {:fed 2 :item "bread"} @out))
          (is (= {"bread" 3} (inv p)))
          (is (= 1 (count (tosses p))))
          (is (gave? s :feed.done)))))))

(deftest the-receipt-is-asked-for-with-the-toss
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (feed {:entities [(villager 2)] :inventory bread} {:villager "v-1"} 20 #(taking % (all-taken "v-1"))))]
          (is (pos? (.-watchS (.-args (first (tosses p)))))))))))

(deftest another-collector-does-not-count-and-three-refusals-stop-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[taker left] [[(fn [_] nil) {"bread" 5}] [(all-taken "someone-else") {}]]]
          (let [{:keys [p out] :as s} (await (feed {:entities [(villager 2)] :inventory bread} {:villager "v-1" :count 2} 60
                                                   #(taking % taker)))]
            (is (= {:fed 0 :item "bread" :status :stopped :reason "not-taken"} @out))
            (is (= 3 (count (tosses p))))
            (is (= left (inv p)) "a drop nobody took is collected back, one someone else took is gone")
            (is (gave? s :feed.gave-up))))))))

(deftest a-villager-who-is-not-there-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (feed {:inventory bread} {:villager "v-1"} 20 identity))]
          (is (= {:fed 0 :item nil :status :stopped :reason "gone"} @out))
          (is (empty? (tosses p))))))))

(deftest a-blocked-walk-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (feed {:entities [(villager 10)] :inventory bread :unreachable ["10,64,0"]}
                                           {:villager "v-1"} 60 #(taking % (all-taken "v-1"))))]
          (is (= "unreachable" (:reason @out)))
          (is (= 0 (:fed @out)))
          (is (empty? (tosses p))))))))

(deftest a-refused-toss-three-times-stops-with-its-status
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (feed {:entities [(villager 2)] :inventory bread} {:villager "v-1"} 40
                                           #(.override (.-world %) "toss" (fn ^:async f [_ _ _] #js {:status "no-item" :count 0}))))]
          (is (= {:fed 0 :item nil :status :stopped :reason "no-item"} (select-keys @out [:fed :item :status :reason])))
          (is (= 3 (count (tosses p)))))))))

(deftest a-drop-that-cannot-be-collected-ends-as-litter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (feed {:entities [(villager 2)] :inventory bread} {:villager "v-1"} 60
                                         (fn [p]
                                           (taking p (fn [_] nil))
                                           (.override (.-world p) "collect" (fn ^:async f [_ _ _] #js {:status "unreachable" :gained #js []})))))]
          (is (= "litter" (:reason @out))))))))

(deftest no-food-carried-with-fetch-off-feeds-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out]} (await (feed {:entities [(villager 2)]} {:villager "v-1" :fetch false} 20 identity))]
          (is (= :not-done @out) "declined at the check")
          (is (empty? (tosses p))))))))

(deftest food-is-fetched-from-a-seen-chest-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (h/setup-seeing {:blocks {"-2,64,3" "chest"} :containers {"-2,64,3" [{:name "bread" :count 10}]}
                                 :entities [(villager 2)]} nil)
              {:keys [eng p clock]} s]
          (taking p (all-taken "v-1"))
          (perception/pass! (aget p "perception"))
          (core/submit! eng (list job {:villager "v-1" :count 2}) {})
          (loop [i 0]
            (when (and (< i 120) (seq (:list (core/state eng))))
              (swap! clock + 700)
              (await (core/tick! eng))
              (recur (inc i))))
          (is (empty? (:list (core/state eng))))
          (is (gave? s :feed.done))
          (is (= 8 (some #(when (= "bread" (:name %)) (:count %)) (get-in @(fake/state p) [:containers [-2 64 3]])))))))))

(deftest clean-up-leaves-a-drop-that-was-lying-before-the-toss
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [other {:id 50 :name "item" :kind "item" :pos {:x 1 :y 64 :z 0} :item {:name "bread" :count 1}}
              {:keys [p out]} (await (feed {:entities [(villager 2) other] :inventory bread} {:villager "v-1" :count 2} 60
                                           #(taking % (fn [_] nil))))
              left (filter #(= "item" (:kind %)) (:entities @(fake/state p)))]
          (is (= "not-taken" (:reason @out)))
          (is (= {"bread" 5} (inv p)) "the body's own tosses came back")
          (is (= [50] (map :id left)) "another's drop stays where it lay"))))))

(deftest a-toss-cut-before-its-receipt-books-what-the-villager-took
  (are [before now lying took] (= took (feed/took {:before before} now (mapv (fn [n] {:count n}) lying)))
    5 3 [] 2          ; both thrown, nothing lying: taken
    5 3 [2] 0         ; both still lying: nothing taken
    5 3 [1] 1
    5 5 [] 0          ; the toss never happened
    5 2 [] 3))
