(ns engine.composite-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.composite :as composite]
            [engine.ctx :as ctx]))

(defn stub
  "A ctx whose child slot :cN ends with (ends N): [status result]. Returns [ctx mem results calls]."
  [ends]
  (let [mem (atom {}) results (atom {}) calls (atom [])
        c {:view (constantly nil)
           :call-child (fn [slot _ _]
                         (swap! calls conj slot)
                         (let [[status result] (ends slot)]
                           (reset! results {slot result})
                           (js/Promise.resolve status)))
           :child-result (fn [slot] (get @results slot))
           :result (fn [data] (swap! results assoc :self data))
           :update-mem (fn [f args] (apply swap! mem f args))}]
    [(assoc c :mem-atom mem) mem results calls]))

(def kid [{:check (constantly true) :round (fn [_] :done)} {}])
(def refuses (constantly [:done {:status :stopped :reason :refused}]))

(defn round!
  "Starts def's round; ctx/mem is read in its synchronous part, before the child call."
  [def c]
  (with-redefs [ctx/mem (fn [_] {})]
    ((:round def) c)))

(deftest repeat-with-no-count-ends-when-its-child-refuses
  (async done
    (let [[c _ results calls] (stub refuses)
          d (composite/repeat-def kid nil)]
      (-> (round! d c)
          (.then (fn [r]
                   (is (= :done r))
                   (is (= {:status :stopped :reason :refused} (:self @results)))
                   (is (= [:c0] @calls))
                   (done)))))))

(deftest seq-stops-with-the-cause-of-a-refusing-step
  (async done
    (let [[c _ results] (stub refuses)
          d (composite/seq-def [kid kid])]
      (-> (round! d c)
          (.then (fn [r]
                   (is (= :done r))
                   (is (= {:status :stopped :reason :refused} (:self @results)))
                   (done)))))))

(deftest until-stops-when-its-child-refuses
  (async done
    (let [[c _ results] (stub refuses)
          d (composite/until-def [{:check (constantly false) :round (fn [_] :done)} {}] kid)]
      (-> (round! d c)
          (.then (fn [r]
                   (is (= :done r))
                   (is (= {:status :stopped :reason :refused} (:self @results)))
                   (done)))))))

(deftest any-skips-a-refusing-child-and-stops-when-all-refused
  (async done
    (let [[c mem results calls] (stub refuses)
          d (composite/any-def [kid kid])]
      (-> (round! d c)
          (.then (fn [r]
                   (is (= :continue r))
                   (is (= {:refused {:c0 {:status :stopped :reason :refused}}} @mem))
                   (let [c2 (assoc c :update-mem (fn [f args] (apply swap! mem f args)))]
                     (-> (with-redefs [ctx/mem (fn [_] @mem)] ((:round d) c2))
                         (.then (fn [r2]
                                  (is (= :continue r2))
                                  (is (= [:c0 :c1] @calls))
                                  (-> (with-redefs [ctx/mem (fn [_] @mem)] ((:round d) c2))
                                      (.then (fn [r3]
                                               (is (= :done r3))
                                               (is (= {:status :stopped :reason :refused} (:self @results)))
                                               (done))))))))))))))
