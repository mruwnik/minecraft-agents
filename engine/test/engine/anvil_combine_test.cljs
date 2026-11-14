(ns engine.anvil-combine-test
  "jobs.items.anvil-combine against the fake world's anvils."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def clock (atom 1000000))
(def spot "1,64,0")
(def pickaxe {:name "iron_pickaxe" :count 1})
(def mending {:name "enchanted_book" :count 1 :enchants [{:name "mending" :level 1}]})
(def sharpness {:name "enchanted_book" :count 1 :enchants [{:name "sharpness" :level 1}]})

(defn start [world prepare]
  (let [[seen sink] (tu/legacy-capture-sink)
        p (prepare (tu/seeing-all (tu/fake-on-floor world)))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (swap! clock + 700)
          (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async child-outcome [eng job-form n]
  (let [out (atom :not-done)
        parent (tu/recording-parent (first job-form) (second job-form) out)
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng (tu/order '(recording-parent {})) {})
    (await (run-until-empty eng n))
    @out))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn inv [p] (mapv #(cond-> {:name (.-name %) :count (.-count %)} (.-enchants %) (assoc :enchants (js->clj (.-enchants %) :keywordize-keys true)))
                    (.-inventory (.self p))))
(defn level [p] (.-level (.-experience (.self p))))
(defn block [p k] (get (js->clj (.-blocks (.-world p))) k))

(def anvil-world {:blocks {spot "anvil"} :inventory [pickaxe mending] :self {:experience {:level 5}}})

(defn ^:async combine!
  "Run the job over world with args; [result p seen]."
  ([world args] (combine! world args identity))
  ([world args prepare]
   (let [{:keys [eng p seen]} (start world prepare)
         result (await (child-outcome eng ['jobs.items.anvil-combine (merge {:item "iron_pickaxe" :enchant "mending"} args)] 60))]
     [result p seen])))

(deftest the-book-goes-on-the-item-at-a-seen-anvil
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p] (await (combine! anvil-world {}))]
          (is (= {:combined true :item "iron_pickaxe" :enchant "mending" :level-cost 2 :levels-spent 2 :xp-level 3 :text "put mending on iron_pickaxe"}
                 (select-keys r [:combined :item :enchant :level-cost :levels-spent :xp-level :text])))
          (is (= [{:name "iron_pickaxe" :count 1 :enchants [{:name "mending" :level 1}]}] (inv p)) "the book is used up")
          (is (= 3 (level p)))
          (is (= 1 (count (calls p "anvil")))))))))

(deftest a-chipped-anvil-out-of-reach-is-walked-to-and-the-other-book-stays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p] (await (combine! (assoc anvil-world :blocks {"12,64,0" "chipped_anvil"} :inventory [pickaxe sharpness mending]) {}))]
          (is (seq (tu/walk-calls p)))
          (is (true? (:combined r)))
          (is (= [sharpness] (filterv #(= "enchanted_book" (:name %)) (inv p)))))))))

(deftest a-goal-that-holds-already-uses-no-anvil
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p] (await (combine! (assoc anvil-world :inventory [(assoc pickaxe :enchants [{:name "mending" :level 1}])]) {}))]
          (is (true? (:already? r)))
          (is (empty? (calls p "anvil"))))))))

(def give-up-rows
  [["no book holds the enchantment" {:inventory [pickaxe sharpness]} {} :no-book]
   ["the anvil asks more levels than the body has" {:self {:experience {:level 1}}} {} :too-few-levels]
   ["no level at all" {:self {:experience {:level 0}}} {} :too-few-levels]
   ["the block given is no anvil" {:blocks {spot "crafting_table"}} {:anvil {:x 1 :y 64 :z 0}} :not-an-anvil]
   ["the anvil given is gone" {:blocks {}} {:anvil {:x 1 :y 64 :z 0}} :no-anvil]
   ["the item is not carried and may not be fetched" {:inventory [mending]} {:ways #{:carried}} :no-item]
   ["no anvil known or carried and none may be fetched" {:blocks {}} {:ways #{:carried}} :no-anvil]])

(deftest gives-up-with-a-reason-and-spends-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (tu/each-async
                give-up-rows
                (fn ^:async one [[label spec args reason]]
                  (let [world (merge anvil-world spec)
                        [r p] (await (combine! world args))]
                    (is (= reason (:reason r)) label)
                    (is (not (:combined r)) label)
                    (is (= (get-in world [:self :experience :level]) (level p)) label)))))))))

(deftest a-missing-anvil-is-fetched-when-the-ways-allow
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p] (await (combine! (assoc anvil-world :blocks {}) {}))]
          (is (= :need (:reason r)) "nothing to make an anvil from: the obtain child fails")
          (is (= "anvil" (get-in r [:ending :cause :item])))
          (is (empty? (calls p "anvil"))))))))

(deftest a-carried-anvil-is-put-down-used-and-dug-up-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p] (await (combine! (assoc anvil-world :blocks {} :inventory [pickaxe mending {:name "anvil" :count 1}]) {}))
              placed (first (calls p "place"))
              cell (js->clj (.-pos (.-args placed)) :keywordize-keys true)]
          (is (true? (:combined r)))
          (is (= {"anvil" 1} (:spent r)))
          (is (= "anvil" (.-item (.-args placed))))
          (is (= 1 (count (calls p "anvil"))))
          (is (some #(= "anvil" (:name %)) (inv p)) "dug up again and carried")
          (is (not= "anvil" (block p (str (:x cell) "," (:y cell) "," (:z cell))))))))))

(deftest an-anvil-that-broke-is-forgotten-and-the-next-one-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc anvil-world :blocks {spot "anvil" "-2,64,0" "anvil"})
              [r p] (await (combine! world {}
                                     (fn [p] (.override (.-world p) "anvil"
                                                        (let [n (atom 0)]
                                                          (fn ^:async f [token args impl]
                                                            (if (zero? (dec (swap! n inc)))
                                                              #js {:status "missing"}
                                                              (await (impl token args))))))
                                       p)))
              cells (mapv #(js->clj (.-pos (.-args %))) (calls p "anvil"))]
          (is (true? (:combined r)))
          (is (= 2 (count cells)))
          (is (apply not= cells) "the second visit is to the other anvil"))))))

(deftest a-visit-that-keeps-failing-ends-nothing-possible-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p] (await (combine! anvil-world {}
                                     (fn [p] (.override (.-world p) "anvil" (fn [_ _ _] #js {:status "failed" :reason "window-did-not-open"})) p)))]
          (is (= :anvil-failed (:reason r)))
          (is (= :nothing-possible (get-in r [:ending :kind])))
          (is (= "anvil gave up: anvil-failed: window-did-not-open" (:text r)))
          (is (seq (calls p "anvil"))))))))
