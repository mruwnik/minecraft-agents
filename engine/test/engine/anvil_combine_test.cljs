(ns engine.anvil-combine-test
  "jobs.items.anvil-combine against the fake world's anvils."
  (:require [cljs.test :refer [deftest is async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.items.anvil-combine :as ac]
            [jobs.items.obtain :as obtain]
            [jobs.lib.cost :as cost]
            [jobs.lib.ready :as ready]))

(def clock (atom 1000000))
(def spot "1,64,0")
(def pickaxe {:name "iron_pickaxe" :count 1})
(def mending {:name "enchanted_book" :count 1 :enchants [{:name "mending" :level 1}]})
(def sharpness {:name "enchanted_book" :count 1 :enchants [{:name "sharpness" :level 1}]})

(defn start [world prepare jobs]
  (let [[seen sink] (tu/legacy-capture-sink)
        p (prepare (tu/seeing-all (tu/fake-on-floor world)))
        eng (core/create {:primitives p :jobs (merge registry/jobs jobs) :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
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
(defn block [p {:keys [x y z]}] (some-> (.blockAt p #js {:x x :y y :z z}) .-name))

(def anvil-world {:blocks {spot "anvil"} :inventory [pickaxe mending] :self {:experience {:level 5}}})

(defn ^:async combine!
  "Run the job over world with args; [result p seen]."
  ([world args] (combine! world args identity))
  ([world args prepare] (combine! world args prepare nil))
  ([world args prepare jobs]
   (let [{:keys [eng p seen]} (start world prepare jobs)
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
   ["a mending book asks two levels, the body has one: refused before an anvil is fetched" {:blocks {} :self {:experience {:level 1}}} {} :too-few-levels]
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
          (is (not= "anvil" (block p cell))))))))

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

(defn slow-obtain
  "jobs.items.obtain whose round runs 100 s three times, then gives (give!) and ends done."
  [rounds give!]
  (assoc (get registry/jobs 'jobs.items.obtain)
         :can-start? (constantly (contract/ok :stub "a stub"))
         :quote-of (constantly {:uses {:job-time [1 1 1]}})
         :round (fn ^:async slow [c]
                  (if (< (swap! rounds inc) 4)
                    (do (swap! clock + 100000) :continue)
                    (do (give!) (ctx/end c :done nil {}))))))

(deftest a-fetch-that-runs-for-minutes-is-not-cut-by-the-visit-s-time
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [body (atom nil)
              rounds (atom 0)
              stub (slow-obtain rounds #(fake/add-item! @body "iron_pickaxe" 1))
              [r p] (await (combine! (assoc anvil-world :inventory [mending]) {} #(reset! body %) {'jobs.items.obtain stub}))]
          (is (= 4 @rounds) "the fetch ran 300 s, past the visit's 120 s")
          (is (true? (:combined r)) (pr-str r))
          (is (= 1 (count (calls p "anvil")))))))))

(deftest an-anvil-put-down-with-no-pickaxe-to-keep-its-drop-is-left-standing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[r p] (await (combine! (assoc anvil-world :blocks {} :inventory [{:name "iron_sword" :count 1} mending {:name "anvil" :count 1}])
                                     {:item "iron_sword" :ways #{:carried}}))
              placed (first (calls p "place"))
              cell (js->clj (.-pos (.-args placed)) :keywordize-keys true)]
          (is (true? (:combined r)))
          (is (= "anvil" (block p cell)) "not dug by hand, its drop lost"))))))

(deftest the-default-job-time-holds-an-anvil-made-from-nothing
  (let [t (ac/default-time {:item "iron_pickaxe" :enchant "mending"})]
    (is (< (:value (cost/goal-value [{:name "anvil" :count 1}])) t) "31 iron from nothing, not one unit's fetch")
    (is (<= (+ (obtain/default-time {:item "anvil"}) (obtain/default-time {:item "iron_pickaxe"})) t))
    (is (> 1e-6 (js/Math.abs (- t (obtain/default-time {:item "anvil"}) (ac/default-time {:item "iron_pickaxe" :enchant "mending" :anvil {:x 0 :y 64 :z 0}}))))
        "a given anvil is not fetched")))

(deftest the-quote-holds-the-price-of-each-fetch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [out (atom nil)
              priced (assoc (get registry/jobs 'jobs.items.obtain) :can-start? (constantly (contract/later :not-worth "a stub" :price 500)))
              asker {:round (fn ^:async ask [c]
                              (reset! out (await (ctx/quote c 'jobs.items.anvil-combine {:item "iron_pickaxe" :enchant "mending"})))
                              (ctx/end c :done nil))}
              {:keys [eng]} (start (assoc anvil-world :inventory [mending]) identity {'jobs.items.obtain priced 'asker asker})]
          (core/submit! eng (tu/order '(asker {})) {})
          (await (run-until-empty eng 5))
          (is (<= 500 (first (get-in @out [:uses :job-time]))) (pr-str @out)))))))

(deftest levels-short-of-the-book-are-earned-from-a-seen-ore-before-the-anvil
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [body (atom nil)
              dug (atom [])
              dig (assoc (get registry/jobs 'jobs.blocks.dig)
                         :can-start? (constantly (contract/ok :stub "a stub")) :safe? ready/ok :fit? ready/ok
                         :round (fn ^:async dig [c]
                                  (swap! dug conj (:pos (:args c)))
                                  (fake/remove-block! @body (:pos (:args c)))
                                  (fake/swap-self! @body assoc :experience {:level 2 :points 16 :progress 0})
                                  (ctx/end c :done nil {})))
              [r p] (await (combine! (assoc-in (assoc anvil-world :blocks {spot "anvil" "4,64,0" "lapis_ore" "5,64,0" "lapis_ore" "6,64,0" "lapis_ore"}) [:self :experience :level] 1)
                                     {} #(reset! body %) {'jobs.blocks.dig dig}))]
          (is (true? (:combined r)) (pr-str r))
          (is (= [[4 64 0]] (mapv (juxt :x :y :z) @dug)) "the 9 points short take three ores as priced; the nearest is dug first, and its pay was enough")
          (is (= 0 (level p)) "the two levels earned went on the book"))))))

(deftest a-fetch-may-walk-off-to-look-for-what-it-lacks
  (is (true? (get-in ac/args [:explore :default])))
  (is (true? (:explore (ac/fetch-opts {:args {:explore true}} "anvil"))))
  (is (false? (:explore (ac/fetch-opts {:args {:explore false}} "anvil")))))
