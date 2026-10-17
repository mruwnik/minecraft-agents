(ns engine.declined-test
  "Ending-returning call helpers (jobs.lib.child, jobs.lib.blocks)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [jobs.lib.blocks :as b]
            [jobs.lib.child :as child]
            [jobs.lib.pen-entry :as pen-entry]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]))

;; ------------------------------------------------------------------ ending-returning call helpers (L1)

(defn ^:async in-parent
  "Run f (a promise-returning fn of the ctx) as the one round of a parent, with go-to replaced by go-to-def; resolves to
  what f resolved to."
  ([go-to-def f] (in-parent go-to-def f (atom [])))
  ([go-to-def f seen]
  (let [clock (atom 1000000)
        out (atom nil)
        parent {:round (fn ^:async r [c] (reset! out [(await (f c))]) (ctx/end c :done nil))}
        eng (core/create {:primitives (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :floor [-5 -5 5 5]})
                          :jobs (assoc registry/jobs 'parent parent 'jobs.movement.go-to go-to-def)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock) :world (world/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [(fn [e] (swap! seen conj (tu/legacy-event e)))] :now #(deref clock)})})]
    (core/submit! eng '(parent) {})
    (loop [n 0]
      (when (and (nil? @out) (< n 6))
        (swap! clock + 500)
        (await (core/tick! eng))
        (recur (inc n))))
    (first @out))))

(def walker
  {:ways [:walk :clear-seal]
   :round (fn ^:async w [c] (ctx/end c :done nil {:arrived true}))})

(def stranded
  {:ways [:walk :clear-seal]
   :round (fn ^:async w [c] (ctx/end c :nothing-possible :unreachable {:why :no-route}))})

(def contract-gave-up
  {:ways [:walk :clear-seal]
   :round (fn ^:async w [c] (ctx/end c :nothing-possible :tree-unreachable {:tried 2}))})

(def walks-twice
  (let [n (atom 0)]
    {:ways [:walk :clear-seal]
     :round (fn ^:async w [c]
              (if (< (swap! n inc) 3) :continue (ctx/end c :done nil {:arrived true})))}))

(deftest call-walk*-returns-the-go-to-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walk #(in-parent %1 (fn ^:async f [c] (await (child/call-walk* c :walk {:pos [1 65 0]}))))]
          (is (= {:kind :done :data {:arrived true}} (select-keys (await (walk walker)) [:kind :data])))
          (let [e (await (walk stranded))]
            (is (= [:nothing-possible :unreachable] ((juxt :kind :cause) e)))
            (is (= :no-route (get-in e [:data :why]))))
          (is (= [:nothing-possible :tree-unreachable {:tried 2}]
                 ((juxt :kind :cause :data) (await (walk contract-gave-up))))))))))

(deftest call-walk*-yields-continue-while-the-walk-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :continue (await (in-parent {:round (fn ^:async w [_] :continue)}
                                           (fn ^:async f [c] (await (child/call-walk* c :walk {:pos [1 65 0]}))))))
            "no ending yet")))))

(deftest run!*-calls-until-an-ending-and-walk-into!*-returns-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :done (:kind (await (in-parent walks-twice (fn ^:async f [c] (await (child/run!* c :kid 'jobs.movement.go-to {:pos [1 65 0]}))))))))
        (is (= :nothing-possible (:kind (await (in-parent contract-gave-up (fn ^:async f [c] (await (child/walk-into!* c :in [1 65 0]))))))))
        (is (= :continue (await (in-parent {:round (fn ^:async w [_] :continue)}
                                           (fn ^:async f [c] (await (child/run!* c :kid 'jobs.movement.go-to {:pos [1 65 0]} {:max-calls 2}))))))
            "max-calls gives the round back")))))

(deftest walk!*-is-arrived-or-the-childs-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :arrived (await (in-parent walker (fn ^:async f [c] (await (b/walk!* c [1 65 0])))))))
        (is (= [:nothing-possible :unreachable] ((juxt :kind :cause) (await (in-parent stranded (fn ^:async f [c] (await (b/walk!* c [1 65 0]))))))))
        (is (= [:nothing-possible :out-of-reach]
               ((juxt :kind :cause) (await (in-parent walker (fn ^:async f [c] (await (b/walk!* c [4 65 4]))))))) "arrived, still out of reach")))))

(deftest walk!*-keeps-out-of-pen-gates-and-warns-when-the-walk-fails
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [args (atom nil)
              seen (atom [])
              gate {:x 5 :y 64 :z 0}
              go-to (assoc stranded :round (fn ^:async w [c] (reset! args (:args c)) (ctx/end c :nothing-possible :unreachable)))]
          (with-redefs [pen-entry/pens! (fn ^:async p [_] [{:gates [gate] :animals 2}])]
            (is (= :nothing-possible (:kind (await (in-parent go-to (fn ^:async g [c] (await (b/walk!* c [9 64 0]))) seen))))))
          (is (= [gate] (:keep-out @args)))
          (is (some #(= :blocks.pen-kept-out (:kind %)) @seen)))))))

(deftest walk-into!*-asks-go-to-for-the-slots-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[slot expect] [[:walk {}] [:in {:y-margin child/entry-depth}] [:other {}]]]
          (let [args (atom nil)
                go-to (assoc walker :round (fn ^:async w [c] (reset! args (:args c)) (ctx/end c :done nil {:arrived true})))]
            (await (in-parent go-to (fn ^:async g [c] (await (child/walk-into!* c slot [1 65 0])))))
            (is (= (merge {:pos [1 65 0] :range 0 :ways #{:walk :clear-seal}} expect) (select-keys @args [:pos :range :ways :checks :y-margin])) (str slot))))))))

(deftest fruitless-rounds-count-per-slot-until-progress
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [0 1 2 0 1 0]
               (await (in-parent walker
                                 (fn ^:async f [c]
                                   (let [n #(child/fruitless c %)]
                                     [(n :a) (child/fruitless! c :a) (child/fruitless! c :a)
                                      (do (child/progress! c :a) (n :a))
                                      (child/fruitless! c :a) (n :b)]))))))))))

(deftest a-slot-is-exhausted-at-the-shared-max-fruitless
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [false true false]
               (await (in-parent walker
                                 (fn ^:async f [c]
                                   (dotimes [_ (dec (child/max-fruitless))] (child/fruitless! c :a))
                                   [(child/exhausted? c :a)
                                    (do (child/fruitless! c :a) (child/exhausted? c :a))
                                    (do (child/progress! c :a) (child/exhausted? c :a))])))))))))

(deftest a-per-call-max-and-the-give-up-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= [false true {:kind :nothing-possible :cause :no-births :data {:n 1} :because {:kind :nothing-possible :cause :x}}]
               (await (in-parent walker
                                 (fn ^:async f [c]
                                   (child/fruitless! c :a)
                                   [(child/exhausted? c :a 2)
                                    (child/exhausted? c :a 1)
                                    (do (child/fruitless! c :a {:kind :nothing-possible :cause :x :report {:uses {}}})
                                        (select-keys (child/give-up c :a :no-births {:n 1}) [:kind :cause :data :because]))])))))))))
