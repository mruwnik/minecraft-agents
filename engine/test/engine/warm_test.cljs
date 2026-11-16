(ns engine.warm-test
  "jobs.lib.warm: the tables built before the first danger are the ones the danger path asks for."
  (:require [cljs.test :refer [async deftest is]]
            [engine.game :as game]
            [jobs.lib.cost.caught :as caught]
            [engine.settings :as settings]
            [jobs.lib.cost.table :as table]
            [jobs.lib.cost.value :as value]
            [jobs.lib.warm :as warm]))

(defn warm-now! [] (run! #(%) (warm/steps)))

(deftest the-warmed-item-table-is-the-one-item-value-asks-for
  (warm-now!)
  (is (identical? (table/table-for @game/version {} nil)
                  (table/table-for @game/version (table/normal-overrides nil) nil))
      "item-value with no overrides or basis asks the table warm! built"))

(deftest every-group-prior-reads-has-its-posterior-warmed
  (let [warmed (atom #{})
        asked (atom #{})
        record (fn [into] (fn [counts] (swap! into conj counts) {:points #js [] :blows 1 :shape 1}))]
    (with-redefs [caught/posterior (record warmed)] (warm-now!))
    (with-redefs [caught/posterior (record asked) caught/chance (constantly 0.5)]
      (doseq [which [::caught/flights ::caught/fights]
              mob ["zombie" "husk" "skeleton" "stray" "spider" "cave_spider" "creeper" "enderman"]]
        (caught/prior which mob 20)))
    (is (seq @asked))
    (is (= @asked @warmed) "the first danger's prices (caught/prior) never compute a posterior warm! left out")))

(deftest warm-up-gives-the-event-loop-a-turn-between-steps
  (async done
    (let [turns (atom 0)
          running (atom true)
          tick (fn tick [] (when @running (swap! turns inc) (js/setImmediate tick)))]
      (js/setImmediate tick)
      (-> (warm/warm!)
          (.then (fn [_]
                   (reset! running false)
                   (is (<= (dec (count (warm/steps))) @turns) "a timer runs between every two steps, so the body's keepalives and stall watch are not held for the whole warm-up")
                   (done)))))))

(deftest no-warm-step-builds-more-than-one-worth-table-pass
  (settings/with-settings {::table/craft-step 0.77 ::value/gather-prior-items 1.77}
    (fn []
      (let [built (map (fn [step] (let [n (.-n table/builds)] (step) (- (.-n table/builds) n))) (warm/steps))]
        (is (pos? (reduce + built)) "the settings give fresh tables, so the steps build some")
        (is (every? #(<= % 1) built) "a table pass is the most one turn holds, so the body's timers run between passes")))))

(deftest the-pass-memo-keeps-one-table-per-key
  (settings/with-settings {::table/craft-step 0.78 ::value/gather-prior-items 1.78}
    (fn []
      (warm-now!)
      (let [mine (filter (fn [[key _]] (= 0.78 (nth (peek key) 2))) @table/passes-held)]
        (is (seq mine))
        (is (every? (fn [[_ [k _]]] (= 2 k)) mine) "each key holds its latest pass only: pass 0 and 1 are inputs to the next, not kept beside it")))))
