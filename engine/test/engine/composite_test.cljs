(ns engine.composite-test
  (:require [cljs.test :refer [deftest is async]]
            [engine.composite :as composite]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.settings :as settings]
            [engine.test-util :as tu]))

(defn stub
  "A ctx whose child slot :cN ends with (ends N), an ending or :continue (ctx/call). Returns [ctx mem calls]."
  [ends]
  (let [mem (atom {}) calls (atom [])
        c {:view (constantly nil)
           :slots []
           :call (fn [slot _ _]
                   (swap! calls conj slot)
                   (js/Promise.resolve (ends slot)))
           :update-mem (fn [f args] (apply swap! mem f args))}]
    [c mem calls]))

(def kid [(tu/job) {}])
(def refuses (constantly (contract/ending :condition-failed :refused nil)))
(def impossible (constantly (contract/ending :impossible :refused nil)))

(defn round!
  "Runs def's round with ctx/mem reading mem."
  [def c mem]
  (with-redefs [ctx/mem (fn [_] @mem)]
    ((:round def) c)))

(deftest repeat-with-no-count-ends-when-its-child-is-impossible
  (async done
    (let [[c mem calls] (stub impossible)
          d (composite/repeat-def kid nil)]
      (-> (round! d c mem)
          (.then (fn [r]
                   (is (= [:impossible :refused] [(:kind r) (:cause r)]))
                   (is (= :impossible (:kind (:because r))) "the child's ending is its because")
                   (is (= [:c0] @calls))
                   (done)))))))

(deftest seq-stops-with-the-ending-of-a-refusing-step
  (async done
    (let [[c mem] (stub (constantly (contract/ending :nothing-possible {:reason :no-path} nil)))
          d (composite/seq-def [kid kid])]
      (-> (round! d c mem)
          (.then (fn [r]
                   (is (= [:nothing-possible {:reason :no-path}] [(:kind r) (:cause r)]) "the child's kind and cause")
                   (done)))))))

(deftest seq-waits-without-calling-a-step-whose-check-fails
  (async done
    (let [[c mem calls] (stub refuses)
          d (composite/seq-def [[(tu/job [(constantly :not-ready)] nil) {}]])]
      (-> (round! d c mem)
          (.then (fn [r]
                   (is (= :continue r))
                   (is (= [] @calls))
                   (-> (round! d (assoc c :reflex :r) mem)
                       (.then (fn [r2]
                                (is (= [:condition-failed :not-ready] [(:kind r2) (:cause r2)])
                                    "a reflex job ends instead: it runs one round")
                                (done))))))))))

(deftest until-stops-when-its-child-is-impossible
  (async done
    (let [[c mem] (stub impossible)
          d (composite/until-def [(tu/job [(constantly :not-ready)] nil) {}] kid)]
      (-> (round! d c mem)
          (.then (fn [r]
                   (is (= :impossible (:kind r)))
                   (done)))))))

(deftest any-skips-a-refusing-child-and-stops-when-all-refused
  (async done
    (let [[c mem calls] (stub refuses)
          d (composite/any-def [kid kid])]
      (-> (round! d c mem)
          (.then (fn [r]
                   (is (= :continue r))
                   (is (= [:c0] (keys (:refused @mem))))
                   (-> (round! d c mem)
                       (.then (fn [r2]
                                (is (= :continue r2))
                                (is (= [:c0 :c1] @calls))
                                (-> (round! d c mem)
                                    (.then (fn [r3]
                                             (is (= [:condition-failed :refused] [(:kind r3) (:cause r3)]))
                                             (done)))))))))))))

(deftest any-ends-done-through-a-later-child-after-a-refusal
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[c mem calls] (stub {:c0 (contract/ending :condition-failed :refused nil) :c1 (contract/ending :done nil nil)})
              d (composite/any-def [kid kid])]
          (is (= :continue (await (round! d c mem))))
          (is (= :done (:kind (await (round! d c mem)))))
          (is (= [:c0 :c1] @calls)))))))

(deftest any-ends-as-the-last-refusal-in-time-among-many-children
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [ends (into {} (map (fn [i] [(composite/slot i) (contract/ending :condition-failed (keyword (str "r" i)) nil)])) (range 10))
              [c mem] (stub ends)
              d (composite/any-def (vec (repeat 10 kid)))]
          (dotimes [_ 10] (await (round! d c mem)))
          (is (= :r9 (:cause (await (round! d c mem))))))))))

(deftest every-combinator-waits-through-its-conditions-with-its-childs-reason
  (let [w {:reason :no-tree :near 3}
        waits (tu/job [(constantly w)] nil)
        never [(tu/job [(constantly :not-ready)] nil) {}]
        c {:view (constantly nil) :slots []}]
    (with-redefs [ctx/mem (constantly {})]
      (doseq [[label d] [[:seq (composite/seq-def [[waits {}]])] [:any (composite/any-def [[waits {}] [waits {}]])]
                         [:repeat (composite/repeat-def [waits {}] nil)] [:until (composite/until-def never [waits {}])]
                         [:nested (composite/seq-def [[(composite/repeat-def [waits {}] nil) {}]])]]]
        (is (nil? (:check d)) (str label ": no check export"))
        (is (= w (ctx/check-wait c d)) (str label ": the child's reason"))))))

(deftest combinators-pass-on-their-childs-impossible-and-look
  (let [never [{:conditions [[:fn (constantly (contract/impossible :never))]] :round (fn [c] (ctx/end c :done nil))} {}]
        peek [{:conditions [[:fn (constantly (contract/look-first :unseen))]] :round (fn [c] (ctx/end c :done nil))} {}]
        c {:view (constantly nil) :slots []}]
    (with-redefs [ctx/mem (constantly {})]
      (doseq [[label mk] [[:seq #(composite/seq-def [%])] [:repeat #(composite/repeat-def % nil)] [:until #(composite/until-def [(tu/job [(constantly :not-ready)] nil) {}] %)]
]]
        (is (= [:impossible :never] (contract/verdict c (:conditions (mk never)))) (str label ": impossible child"))
        (is (= [:look :unseen] (contract/verdict c (:conditions (mk peek)))) (str label ": look-first child"))))))

(deftest repeat-ends-impossible-when-its-child-never-can-start
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [never [{:conditions [[:fn (constantly (contract/impossible :never))]] :round (fn [c] (ctx/end c :done nil))} {}]
              [c mem] (stub {})
              r (await (round! (composite/repeat-def never nil) c mem))]
          (is (= [:impossible :never] [(:kind r) (:cause r)])))))))

;; ------------------------------------------------------------------ on the engine

(defn ending-kid
  "A contract job that ends with @ending ([kind cause]) and counts its runs in runs."
  [ending runs]
  {:conditions []
   :round (fn ^:async kid-round [c] (swap! runs inc) (let [[k cause] @ending] (ctx/end c k cause)))})

(defn repeat-setup
  ([kind spec] (repeat-setup kind spec []))
  ([kind spec sinks]
   (let [clock (atom 1000000)
         ending (atom [kind :nothing-to-do])
         runs (atom 0)
         eng (core/create {:primitives (tu/fake {}) :jobs {'kid (ending-kid ending runs) 'never (tu/job [(constantly :not-ready)] nil)} :triggers {} :dir (tu/tmp-dir)
                           :now #(deref clock)
                           :events (events/make {:body "Fake" :sinks sinks :now #(deref clock)})})]
     (core/submit! eng spec {})
     {:eng eng :clock clock :ending ending :runs runs})))

(defn ^:async run-s! [{:keys [eng clock]} s]
  (dotimes [_ (* 2 s)] (await (core/tick! eng)) (swap! clock + 500)))

(defn listed? [{:keys [eng]}] (boolean (seq (:list (core/state eng)))))

(deftest a-listed-seq-stops-with-its-childs-ending-kind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [spec ['(seq (kid)) '(seq (seq (kid)))]]
          (let [[seen sink] (tu/capture-sink)
                s (repeat-setup :nothing-possible spec [sink])]
            (await (run-s! s 2))
            (is (not (listed? s)))
            (is (= [{:kind :nothing-possible :cause :nothing-to-do}]
                   (->> @seen (filter #(= [:job :stopped] [(:source %) (:kind %)])) (mapv #(select-keys (:data %) [:kind :cause]))))
                (str spec))))))))

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

(deftest repeat-rests-while-its-childs-goal-already-holds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [spec ['(repeat (holds)) '(repeat (seq (holds) (holds))) '(repeat (any (holds) (holds)))]]
          (let [clock (atom 1000000)
                [seen sink] (tu/capture-sink)
                holds {:conditions [[:fn (constantly :not-needed)]] :round (fn [_] :continue)
                       :done? (constantly {:done? true :units :items :done 0 :left 0})}
                eng (core/create {:primitives (tu/fake {}) :jobs {'holds holds} :triggers {} :dir (tu/tmp-dir)
                                  :now #(deref clock) :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
                s {:eng eng :clock clock}
                ended #(count (filter (fn [e] (= [:job :child_ended :done] [(:source e) (:kind e) (:status (:data e))])) @seen))]
            (core/submit! eng spec {})
            (await (run-s! s 2))
            (let [n (ended)]
              (is (pos? n) (str spec ": ended done at once"))
              (await (run-s! s (- (rest-s) 5)))
              (is (= n (ended)) (str spec ": then it rests instead of spinning"))
              (is (listed? s))
              (await (run-s! s 10))
              (is (< n (ended)) (str spec ": asks again after its rest")))))))))
