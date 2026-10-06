(ns engine.giveup-notice-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.job-api :as api]
            [engine.test-util :as tu]))

(defn ^:async emitted
  "The :gave-up event a job emits at level with fields, as collected by the sink."
  [level fields]
  (let [[seen sink] (tu/legacy-capture-sink)
        jobs {'say {:args {} :check (constantly true)
                    :round (fn [c] (ctx/emit! c :gave-up level fields) :done)}}
        eng (core/create {:primitives (tu/fake {}) :jobs jobs :triggers {} :dir (tu/tmp-dir)
                          :events (events/make {:stdout? false :sinks [sink]})})]
    (api/mutate! eng {:op :submit :request-id "r1" :generation-id (:generation-id (core/state eng))
                      :spec '(say)})
    (await (core/tick! eng))
    (core/shutdown! eng)
    (first (filter #(= :gave-up (:kind %)) @seen))))

(def cases
  [[:warn {} :notice]
   [:error {} :notice]
   [:warn {:attention :none} :none]
   [:error {:attention :required} :required]
   [:warn {:attention :notice} :notice]
   [:info {} nil]
   [:debug {} nil]
   [nil {} nil]
   [:info {:attention :notice} :notice]])

(deftest job-emits-carry-attention-by-level
  (async done
    (tu/run-async done
      (fn ^:async run []
        (doseq [[level fields expected] cases]
          (let [event (await (emitted level fields))]
            (is (= expected (:attention event)) (str "level " level " fields " fields))))))))

(deftest job-emits-keep-level-field
  (async done
    (tu/run-async done
      (fn ^:async run []
        (doseq [level [:warn :error :info nil]]
          (let [event (await (emitted level {:reason :x}))]
            (is (= :x (:reason event)))
            (is (= level (:level event)) (str "level " level))))))))
