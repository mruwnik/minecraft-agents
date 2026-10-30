(ns engine.fetch-trip-food-test
  "jobs.survival.fetch-trip-food: get-food ends by its own goal; going on short is the holder's priced choice."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.get-food-rounds-test :refer [setup]]
            [engine.test-util :as tu]
            [jobs.survival.fetch-trip-food]))

(defn ^:async ending-of
  [eng args]
  (let [out (atom nil)
        parent {:round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid 'jobs.survival.fetch-trip-food args))]
                           (if (= :continue e) :continue (do (reset! out (select-keys e [:kind :cause :data :because])) (ctx/end c :done nil)))))}
        eng (assoc-in eng [:jobs 'recording-parent] parent)]
    (core/submit! eng '(recording-parent) {})
    (await (tu/run-until-empty eng 40))
    @out))

(deftest a-fetch-that-found-nothing-ends-with-get-foods-cause-unless-the-holder-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:food 20}})
              e (await (ending-of eng {:carry 8}))]
          (is (= [:nothing-possible :no-carry] [(:kind e) (:cause e)]) "0 of 8 points is not done"))))))

(deftest a-holder-that-goes-on-gets-done-short-with-the-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:food 20} :inventory [{:name "bread" :count 1}]})
              e (await (ending-of eng {:carry 8 :go-on? true}))]
          (is (= [:done true 5 :no-carry] [(:kind e) (get-in e [:data :short]) (get-in e [:data :have]) (get-in e [:data :cause])])))))))

(deftest a-pack-holding-the-carry-is-done-without-the-short-mark
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:self {:food 20} :inventory [{:name "bread" :count 5}]})
              e (await (ending-of eng {:carry 8}))]
          (is (= :done (:kind e)))
          (is (nil? (get-in e [:data :short]))))))))
