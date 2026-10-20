(ns engine.mine-trip-test
  "jobs.gather.mine, the trip around jobs.gather.dig-for: equip, call the dig, mend, walk back. The dig's own cases are engine.mine-a..e."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
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
        (let [{:keys [s out fetch]} (await (as-child {:block "iron_ore" :level false :count 1 :direction "east" :tunnel-length 8 :trip true :fetch false}
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
