(ns engine.composite-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.composite :as composite]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.settings :as settings]
            [engine.test-util :as tu]))

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
(def impossible (constantly [:done {:status :stopped :reason :refused :ending {:kind :impossible :cause :refused}}]))

(defn round!
  "Starts def's round; ctx/mem is read in its synchronous part, before the child call."
  [def c]
  (with-redefs [ctx/mem (fn [_] {})]
    ((:round def) c)))

(deftest repeat-with-no-count-ends-when-its-child-is-impossible
  (async done
    (let [[c _ results calls] (stub impossible)
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

(deftest until-stops-when-its-child-is-impossible
  (async done
    (let [[c _ results] (stub impossible)
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

;; ------------------------------------------------------------------ repeat on the engine: rest and run again

(defn ending-kid
  "A contract job that ends with @ending ([kind cause]) and counts its runs in runs."
  [ending runs]
  {:conditions []
   :round (fn ^:async kid-round [c] (swap! runs inc) (let [[k cause] @ending] (ctx/end c k cause)))})

(defn repeat-setup [kind spec]
  (let [clock (atom 1000000)
        ending (atom [kind :nothing-to-do])
        runs (atom 0)
        eng (core/create {:primitives (tu/fake {}) :jobs {'kid (ending-kid ending runs) 'never {:check (constantly false) :round (fn [_] :done)}} :triggers {} :dir (tu/tmp-dir)
                          :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [] :now #(deref clock)})})]
    (core/submit! eng spec {})
    {:eng eng :clock clock :ending ending :runs runs}))

(defn ^:async run-s! [{:keys [eng clock]} s]
  (dotimes [_ (* 2 s)] (await (core/tick! eng)) (swap! clock + 500)))

(defn listed? [{:keys [eng]}] (boolean (seq (:list (core/state eng)))))

(defn rest-s [] (settings/get composite/settings ::composite/rest-s))

(defn ^:async rests-and-runs-again [spec kind]
  (let [s (repeat-setup kind spec)]
    (await (run-s! s 2))
    (is (= 1 @(:runs s)) (str kind ": one run, then it rests"))
    (is (listed? s) (str kind ": still listed"))
    (await (run-s! s (- (rest-s) 5)))
    (is (= 1 @(:runs s)) (str kind ": no run while it rests"))
    (await (run-s! s 10))
    (is (= 2 @(:runs s)) (str kind ": runs again after its rest"))
    (is (listed? s))))

(deftest repeat-rests-and-runs-again-after-a-failed-condition-or-nothing-possible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [kind [:condition-failed :nothing-possible :limit-reached]]
          (await (rests-and-runs-again '(repeat (kid)) kind)))
        (await (rests-and-runs-again '(until (never) (kid)) :condition-failed))))))

(deftest repeat-rests-longer-after-each-failed-run-in-a-row
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (repeat-setup :condition-failed '(repeat (kid)))]
          (await (run-s! s (+ 2 (rest-s) 5)))
          (is (= 2 @(:runs s)))
          (await (run-s! s (+ (rest-s) 5)))
          (is (= 2 @(:runs s)) "the second rest is twice the first")
          (reset! (:ending s) [:done nil])
          (await (run-s! s (+ (rest-s) 5)))
          (is (< 3 @(:runs s)) "a done run ends the rests"))))))

(deftest repeat-ends-when-its-child-is-impossible-or-cut-off
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [kind [:impossible :cut-off]]
          (let [s (repeat-setup kind '(repeat (kid)))]
            (await (run-s! s 2))
            (is (= 1 @(:runs s)))
            (is (not (listed? s)) (str kind " ends the repeat"))))))))

(deftest a-cancel-ends-a-resting-repeat
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (repeat-setup :condition-failed '(repeat (kid)))]
          (await (run-s! s 2))
          (core/cancel! (:eng s) (first (:list (core/state (:eng s)))))
          (await (run-s! s (* 3 (rest-s))))
          (is (not (listed? s)))
          (is (= 1 @(:runs s))))))))
