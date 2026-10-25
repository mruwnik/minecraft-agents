(ns engine.mine-trip-test
  "jobs.gather.mine, the trip around jobs.gather.dig-for: equip, call the dig, mend, walk back. The dig's own cases are engine.mine-a..e."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [jobs.gather.mine.cause :as cause]
            [jobs.lib.need :as need]
            [engine.mine-support :as m :refer [as-child rock-world inv cells]]
            [engine.test-util :as tu]))

(def sturdy-pickaxe [{:name "stone_pickaxe" :count 1 :durability 100 :max 131} {:name "torch" :count 10}])

(defn breaks-on-third-dig!
  "The third dig wipes the inventory: the pickaxe is gone."
  [s]
  (let [world (.-world (:p s))
        n (atom 0)]
    (.override world "dig"
               (fn ^:async f [token a impl]
                 (let [r (await (impl token a))]
                   (when (= 3 (swap! n inc))
                     (swap! (.. world -state) assoc :inventory []))
                   r)))))

(def ore-world (assoc (rock-world {"3,64,1" "iron_ore"}) :inventory sturdy-pickaxe))

(deftest a-pickaxe-that-breaks-mid-dig-is-fetched-and-the-dig-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s out fetch]} (await (as-child {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 8 :trip true}
                                                     ore-world "stone_pickaxe" 200 {:before breaks-on-third-dig!}))]
          (is (= 1 (count fetch)) "one pickaxe fetched, after the break")
          (is (= [:done :count] [(:kind out) (:reason (:data out))]) (pr-str out))
          (is (= 1 (get (inv s) "raw_iron"))))))))

(deftest a-trip-with-fetch-off-ends-with-the-digs-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s out fetch]} (await (as-child {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 8 :trip true :ways #{:carried}}
                                                     ore-world "stone_pickaxe" 200 {:before breaks-on-third-dig!}))]
          (is (empty? fetch))
          (is (= [:condition-failed :no-tool] [(:kind out) (contract/cause-key out)]) (pr-str out)))))))

(deftest a-trip-without-a-pickaxe-fetches-it-before-it-calls-the-dig
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s out fetch]} (await (as-child {:block "stone" :count 1 :trip true} {:blocks {"3,64,0" "stone"} :drops m/cobble} "stone_pickaxe" 40))]
          (is (= ["stone"] (mapv :block fetch)))
          (is (= [:done :count] [(:kind out) (:reason (:data out))])))))))

(deftest a-nearly-spent-last-pickaxe-ends-the-dig-no-tool-with-a-message
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out]} (await (as-child {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 8}
                                             (assoc (rock-world {}) :inventory [{:name "stone_pickaxe" :count 1 :durability 2 :max 131}]) nil 40))]
          (is (= [:condition-failed :no-tool :spent] [(:kind out) (contract/cause-key out) (get-in out [:data :why])]) (pr-str out))
          (is (re-find #"nearly spent.*jobs.gather.mine" (get-in out [:data :text]))))))))

(deftest a-walk-back-that-arrived-but-was-pushed-on-goes-on-digging
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [arrived-only {:round (fn ^:async go-to [c] (ctx/end c :done nil {:arrived true :in-water true}))}
              {:keys [out]} (await (as-child {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 8 :trip true}
                                             (assoc (rock-world {"3,64,1" "iron_ore"}) :inventory [])
                                             "stone_pickaxe" 200
                                             {:move-to [0.5 64 1.5] :jobs {'jobs.movement.go-to arrived-only}}))]
          (is (not= :not-back (:reason (:data out))) (pr-str out)))))))

(deftest a-trip-carrying-far-more-cobblestone-than-a-dig-needs-throws-the-rest-away-and-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s out]} (await (as-child {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 8 :trip true}
                                               (-> ore-world
                                                   (update :blocks #(apply dissoc % (for [x [-1 -2 -3] y [64 65]] (str x "," y ",0"))))
                                                   (assoc :inventory (into sturdy-pickaxe (repeat 5 {:name "cobblestone" :count 64}))))
                                               "stone_pickaxe" 400))]
          (is (= [:done :count] [(:kind out) (:reason (:data out))]) (pr-str out))
          (is (= 1 (get (inv s) "raw_iron")))
          (is (<= (get (inv s) "cobblestone" 0) 64) "a stack is kept, the rest thrown"))))))

(deftest cobblestone-the-dig-itself-brings-in-over-the-cap-is-thrown-away-between-digs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [s out]} (await (as-child {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 8 :trip true}
                                               (-> ore-world
                                                   (update :blocks #(apply dissoc % (for [x [-1 -2 -3] y [64 65]] (str x "," y ",0"))))
                                                   (assoc :inventory (into sturdy-pickaxe (repeat 2 {:name "cobblestone" :count 64}))))
                                               "stone_pickaxe" 400))]
          (is (= [:done :count] [(:kind out) (:reason (:data out))]) (pr-str out))
          (is (<= (get (inv s) "cobblestone" 0) 128) "128 carried at the start, the digs' cobblestone over the cap went"))))))

(deftest a-whole-iron-pickaxe-with-no-wood-for-a-stone-spare-digs-its-tunnel-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (m/scenario {:block "coal_ore" :count 1 :direction "east" :tunnel-length 4 :radius 8 :torches false :trip true}
                                   (assoc (rock-world {}) :inventory [{:name "iron_pickaxe" :count 1 :durability 250 :max-durability 250} {:name "cobblestone" :count 8}])
                                   300))
              e (m/done-event s)]
          (let [ev (first (m/events-of s :tool.spare-skipped))]
            (is (= ["stone_pickaxe" "iron_pickaxe" :not-worth] ((juxt :tool :item :reason) ev)) (pr-str ev))
            (is (< (:worth ev) (:price ev)) "the start price passed the wear saved")
            (let [evs (m/events-of s :tool.spare-skipped)]
              (is (= (count evs) (count (distinct (map (juxt :tool :item :price :worth) evs)))) "each decision is told once")))
          (is (= [] (into [] (comp (map (comp str :job)) (filter #(re-find #"/spare(/|$)" %)) (distinct)) @(:seen s))) "a spare dearer than the wear it saves is not looked for")
          (is (= [:tunnel-length 4] [(:stop e) (:length e)]) (pr-str (dissoc e :report))))))))

(deftest a-dear-spare-whose-start-check-only-waits-is-asked-again-not-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [orig need/item-status
              asked (atom 0)]
          (set! need/item-status (fn [& _] (swap! asked inc) [:later "waiting" {:reason :no-source}]))
          (let [s (try (await (m/scenario {:block "coal_ore" :count 1 :direction "east" :tunnel-length 4 :radius 8 :torches false :trip true}
                                          (assoc (rock-world {}) :inventory [{:name "iron_pickaxe" :count 1 :durability 250 :max-durability 250} {:name "cobblestone" :count 8}])
                                          300))
                       (finally (set! need/item-status orig)))]
            (is (pos? @asked) "the start check was asked")
            (is (nil? (:spare-tried (m/job-mem s))) "a wait that is not :not-worth does not give the spare up for the job")
            (is (empty? (m/events-of s :tool.spare-skipped)))))))))

(def iron-world (assoc (rock-world {}) :inventory [{:name "iron_pickaxe" :count 1 :durability 250 :max-durability 250} {:name "cobblestone" :count 8}]))

(defn ^:async run-dig!
  "Submit one more coal dig on s (a fresh job, same body) in direction dir; run it out."
  [s dir]
  (core/submit! (:eng s) (m/spec {:block "coal_ore" :count 1 :direction dir :tunnel-length 4 :radius 8 :torches false :trip true}) {})
  (await (m/run-ticks s 300)))

(defn ^:async with-price
  "Run (f) with the spare's start check answering not-worth at the price in the atom."
  [price f]
  (let [orig need/item-status]
    (set! need/item-status (fn [& _] [:never "too dear" {:reason :not-worth :price @price}]))
    (try (await (f)) (finally (set! need/item-status orig)))))

(deftest a-skipped-dear-spare-is-told-once-over-fresh-jobs-until-its-numbers-change-or-the-tool-breaks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [price (atom 100)
              s (m/start {:world iron-world})
              told #(count (m/events-of s :tool.spare-skipped))
              before (atom 0)
              grew? #(let [n (told)] (when (> n @before) (reset! before n) true))
              same? #(= @before (told))]
          (await (with-price price
                   (fn ^:async f []
                     (await (run-dig! s "east"))
                     (is (grew?) "first job tells it")
                     (await (run-dig! s "west"))
                     (is (same?) "a fresh job with the same numbers does not")
                     (reset! price 200)
                     (await (run-dig! s "north"))
                     (is (grew?) "a changed price tells it again")
                     (swap! (fake/state (:p s)) assoc :inventory [{:name "iron_pickaxe" :count 1 :durability 15 :max-durability 250} {:name "cobblestone" :count 8}])
                     (.override (.-world (:p s)) "dig"
                                (fn ^:async f [token a impl]
                                  (let [r (await (impl token a))]
                                    (swap! (fake/state (:p s)) assoc :inventory [{:name "stone_pickaxe" :count 1 :durability 131 :max-durability 131}])
                                    r)))
                     (await (run-dig! s "south"))
                     (is (= 1 (count (m/events-of s :tool.broke))) "the iron pickaxe broke")
                     (swap! (fake/state (:p s)) assoc :inventory [{:name "iron_pickaxe" :count 1 :durability 250 :max-durability 250} {:name "cobblestone" :count 8}])
                     (await (run-dig! s "east"))
                     (is (grew?) "after the break the same numbers are told afresh")))))))))

(deftest junk-over-the-cap-stops-a-shedding-dig-only-between-digs-not-while-it-collects-a-drop
  ;; a dig whose drops are still to collect goes on: a new call after the shed would not know the drop
  (let [p (tu/fake-on-floor {:inventory (into [{:name "stone_pickaxe" :count 1}] (repeat 4 {:name "cobblestone" :count 64}))})
        c {:primitives p :args {:block "iron_ore" :shed true} :view (fn [] {:data mem/empty-data :now 1000})}
        cause-with #(with-redefs [ctx/mem (constantly %)] (:reason (cause/stop-cause c)))]
    (is (= :surplus (cause-with {})))
    (is (nil? (cause-with {:collecting true})))))
