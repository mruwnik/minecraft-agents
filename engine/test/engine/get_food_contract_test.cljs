(ns engine.get-food-contract-test
  "jobs.survival.get-food as a contract job: it exports the contract, ends with a kind and a cause, and takes only the
  ways the call allows."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.get-food-rounds-test :as gr :refer [setup herd]]
            [engine.registry :as registry]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.survival.get-food :as get-food]
            [jobs.survival.get-food.cook :as cook]
            [jobs.survival.night.hold :as hold]))

(def food 'jobs.survival.get-food)

(defn ^:async ending-of
  "Run get-food with args as a ctx/call child of a recording parent on eng: the child's {:kind :cause :data}."
  [eng args]
  (let [out (atom nil)
        parent {:round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid food args))]
                           (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause :data])) (ctx/end c :done nil)))))}
        eng (assoc-in eng [:jobs 'recording-parent] parent)]
    (core/submit! eng '(recording-parent) {})
    (await (tu/run-until-empty eng 40))
    @out))

(defn kc [e] (select-keys e [:kind :cause]))

(deftest get-food-exports-the-contract
  (let [job (get registry/jobs food)]
    (is (fn? (:can-start? job)))
    (is (= [] (:target job)))
    (is (= [:bake :cook :source :collect :hunt :crops :fish :pen :field] (:ways job)))
    (is (fn? (:done? job)))
    (is (nil? (contract/quote-problem ((:quote-of job) {:args {}}))))
    (is (fn? cook/cook!) "the cooking block night's hold uses lives in get-food.cook")
    (is (fn? hold/cooking-in-shelter?))
    (is (fn? get-food/next-way!))))

(deftest fed-ends-done-with-the-food-level
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:food 3} :inventory [{:name "bread" :count 5}]})
              e (await (ending-of eng {}))]
          (is (= :done (:kind e)))
          (is (= 18 (get-in e [:data :food]))))))))

(deftest nothing-found-ends-nothing-possible-no-food
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:food 1}})
              e (await (ending-of eng {:explore false}))]
          (is (= {:kind :nothing-possible :cause :no-food} (kc e)))
          (is (re-find #"no food" (str (get-in e [:data :text])))))))))

(deftest an-unmet-stock-ends-nothing-possible-no-stock
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:food 20}})
              e (await (ending-of eng {:stock 8 :explore false}))]
          (is (= {:kind :nothing-possible :cause :no-stock} (kc e)))
          (is (= 8 (get-in e [:data :stock]))))))))

(deftest a-body-above-the-hungry-line-is-done-already
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:food 8 :health 20}})
              e (await (ending-of eng {}))]
          (is (= :done (:kind e))))))))

(deftest nothing-to-try-inside-the-cooldown-is-condition-failed-after-one-look
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 1}})]
          (mem/write! (:store eng) :hungry {:food 1} {:cap 10 :ttl 3600000})
          (let [e (await (ending-of eng {:explore false}))]
            (is (= :condition-failed (:kind e)))
            (is (= :no-food-found (contract/cause-key e)))
            (is (seq (.filter (.-calls (.-world p)) (fn [c] (= "look" (.-name c))))) "it looked round from here first")))))))

(deftest an-animal-seen-only-after-the-look-starts-the-job-inside-the-cooldown
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:self {:food 2}})
              look (.-look p)
              added (atom false)]
          (mem/write! (:store eng) :hungry {:food 2} {:cap 10 :ttl 3600000})
          (set! (.-look p) (fn [& args]
                             (when (compare-and-set! added false true) (run! #(fake/add-entity! p %) herd))
                             (.apply look p (to-array args))))
          (let [e (await (ending-of eng {:attack-gap-ms 0 :explore false}))]
            (is (= :done (:kind e)))
            (is (seq (gr/calls p "attack")))))))))

(deftest the-call-takes-only-the-ways-it-allows
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[ways bake-tried?] [[#{:hunt} false] [nil true]]]
          (let [{:keys [eng p]} (setup {:self {:food 3} :entities herd :inventory [{:name "wheat" :count 3}]})
                e (await (ending-of eng (cond-> {:attack-gap-ms 0} ways (assoc :ways ways))))]
            (is (= :done (:kind e)) (pr-str ways))
            (is (seq (gr/calls p "attack")) (pr-str ways))
            (is (= bake-tried? (boolean (seq (gr/entries eng :no-bake)))) (pr-str ways))))))))
