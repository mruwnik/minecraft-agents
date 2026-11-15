(ns engine.ride-rail-test
  "jobs.movement.ride-rail against the fake world: a pushed minecart rolls to its :rollsTo, else stays where it is."
  (:require [cljs.test :refer [deftest is async]]
            [engine.build-from-plan-test :as b]
            [engine.contract :as contract]
            [engine.fake :as fake]
            [engine.farm-contract-test :refer [ending-of]]
            [engine.harvest-test :as h]
            [engine.library-test :as lt]
            [engine.rail-line-test :as rlt]
            [engine.test-util :as tu]
            [jobs.lib.vehicle :as vehicle]))

(def job 'jobs.movement.ride-rail)

(def plan (rlt/line-plan {}))

(defn cart [extra] (merge {:id 50 :uuid "u-50" :name "minecart" :kind "other" :health 1 :pos [0.5 64.0625 0.5]
                           :drops [{:name "minecart" :count 1}]} extra))

(defn ^:async ride! [blocks entities inventory & [setup]]
  (let [{:keys [eng p]} (b/start {:inventory inventory :blocks blocks :entities entities :self {:pos {:x 0 :y 64 :z 3}}}
                                 {"line" plan} [])
        _ (when setup (setup p))
        e (await (ending-of {:eng eng} job {:plan "line"} 400))]
    {:e e :p p}))

(defn self-x [p] (.. p self -pos -x))

(defn carts [p] (filterv #(= "minecart" (:name %)) (fake/entities p)))

(deftest a-cart-on-the-near-end-is-boarded-pushed-and-ridden-to-the-far-end
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [e p]} (await (ride! (rlt/built-world plan "stone") [(cart {:rollsTo [29.5 64.0625 0.5]})] []))]
          (is (= :done (:kind e)) (pr-str e))
          (is (= {:from [0 64 0] :to [29 64 0] :cart 50} (select-keys (:data e) [:from :to :cart])))
          (is (not (vehicle/mounted? p)) "got off")
          (is (< 28 (self-x p)) "the body is at the far end")
          (is (= 1 (count (carts p))) "a cart found on the rail is left at the far end"))))))

(deftest a-carried-cart-that-a-push-does-not-move-ends-stalled-and-is-taken-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [e p]} (await (ride! (rlt/built-world plan "stone") [] [{:name "minecart" :count 1}]))]
          (is (= [:nothing-possible :stalled] [(:kind e) (contract/cause-key e)]) (pr-str e))
          (is (= [0 64 0] (:pos (:data e))))
          (is (not (vehicle/mounted? p)))
          (is (empty? (carts p)) "the cart it put down is broken")
          (is (= 1 (get (lt/inv p) "minecart")) "and its item picked up"))))))

(deftest a-line-with-a-missing-rail-is-not-ridden
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [e p]} (await (ride! (dissoc (rlt/built-world plan "stone") (h/cell-key 12 64 0)) [(cart {})] []))]
          (is (= [:condition-failed :not-sound] [(:kind e) (contract/cause-key e)]) (pr-str e))
          (is (some #(= {:pos [12 64 0] :why :gap} %) (:breaks (:cause e))) (pr-str e))
          (is (not (vehicle/mounted? p))))))))

(deftest a-line-the-body-has-not-yet-seen-is-looked-at-before-it-is-judged
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [shown (atom false)
              unseen (fn [p]
                       (aset p "sensedAt" (fn [pos] (if @shown (.blockAt p pos) #js {:unknown true})))
                       (.override (.-world p) "look" (fn [token a impl] (reset! shown true) (impl token a))))
              {:keys [e]} (await (ride! (rlt/built-world plan "stone") [(cart {:rollsTo [29.5 64.0625 0.5]})] [] unseen))]
          (is (= :done (:kind e)) (pr-str e)))))))
