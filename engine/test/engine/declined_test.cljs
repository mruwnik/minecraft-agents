(ns engine.declined-test
  "jobs.lib.declined: a parent whose round returns a declined child's :declined is parked by its check until the
  child's check passes."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [jobs.lib.blocks :as b]
            [jobs.lib.child :as child]
            [jobs.lib.declined :as declined]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]))

(defn old-kid [need]
  {:check (fn [c] (if @need (ctx/wait c {:reason :need :what "thing"}) true))
   :round (fn ^:async kid-round [_] :done)})

(defn contract-kid [need]
  {:conditions [[:fn (fn [_] (when @need {:reason :need :what "thing"}))]]
   :round (fn ^:async kid-round [c] (ctx/end c :done nil))})

(defn setup
  ([] (setup old-kid))
  ([kid-of]
   (let [clock (atom 1000000)
         [seen sink] (tu/legacy-capture-sink)
         need (atom true)
         use-kid (atom true)
         kid (kid-of need)
         parent {:check (fn [c] (declined/check c))
                 :round (fn ^:async parent-round [c]
                          (declined/begin! c)
                          (if-not @use-kid
                            :continue
                            (let [r (await (declined/call-child! c :kid 'kid {}))]
                              (if (= :done r) :done r))))}
         p (tu/fake {:self {:pos {:x 0 :y 65 :z 0}}})
         eng (core/create {:primitives p :jobs (assoc registry/jobs 'kid kid 'parent parent)
                           :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock) :world (world/of-data {} {} [])
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
     (core/submit! eng '(parent) {})
     {:eng eng :clock clock :seen seen :need need :use-kid use-kid})))

(defn ^:async tick-n! [{:keys [eng clock]} n]
  (dotimes [_ n] (swap! clock + 500) (await (core/tick! eng))))

(defn rounds [{:keys [seen]}] (count (filter #(= :round_started (:kind %)) @seen)))

(defn ^:async parked-then-done [kid-of]
  (let [s (setup kid-of)]
    (await (tick-n! s 12))
    (is (= 1 (rounds s)) "one round meets the decline, then parked")
    (is (= [:need] (map :reason (filter #(= :waiting (:kind %)) @(:seen s)))) "the wait is told once")
    (reset! (:need s) false)
    (await (tick-n! s 3))
    (is (empty? (:list (core/state (:eng s)))) "done once the child's check passes")))

(deftest a-parent-of-a-declining-child-runs-one-round-then-none-and-resumes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (parked-then-done old-kid))
        (await (parked-then-done contract-kid))))))

(deftest a-round-that-calls-no-child-drops-the-booking
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (setup)]
          (await (tick-n! s 4))
          (reset! (:use-kid s) false)
          (reset! (:need s) false)
          (await (tick-n! s 2))
          (let [before (rounds s)]
            (reset! (:need s) true)
            (await (tick-n! s 4))
            (is (< before (rounds s)) "no stale booking parks the parent on the child's check")))))))

;; ------------------------------------------------------------------ ending-returning call helpers (L1)

(defn ^:async in-parent
  "Run f (a promise-returning fn of the ctx) as the one round of a parent, with go-to replaced by go-to-def; resolves to
  what f resolved to."
  [go-to-def f]
  (let [clock (atom 1000000)
        out (atom nil)
        parent {:check (fn [_] true)
                :round (fn ^:async r [c] (reset! out [(await (f c))]) :done)}
        eng (core/create {:primitives (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :floor [-5 -5 5 5]})
                          :jobs (assoc registry/jobs 'parent parent 'jobs.movement.go-to go-to-def)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock) :world (world/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [] :now #(deref clock)})})]
    (core/submit! eng '(parent) {})
    (loop [n 0]
      (when (and (nil? @out) (< n 6))
        (swap! clock + 500)
        (await (core/tick! eng))
        (recur (inc n))))
    (first @out)))

(def old-walker
  {:check (fn [_] true)
   :round (fn ^:async w [c] (ctx/result! c {:arrived true}) :done)})

(def old-stranded
  {:check (fn [_] true)
   :round (fn ^:async w [c] (ctx/result! c {:status :stopped :reason :unreachable :why :no-route}) :done)})

(def contract-gave-up
  {:conditions []
   :round (fn ^:async w [c] (ctx/end c :nothing-possible :tree-unreachable {:tried 2}))})

(def walks-twice
  (let [n (atom 0)]
    {:check (fn [_] true)
     :round (fn ^:async w [c]
              (if (< (swap! n inc) 3) :continue (do (ctx/result! c {:arrived true}) :done)))}))

(deftest call-walk*-returns-the-go-to-ending-of-an-old-or-a-contract-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [walk #(in-parent %1 (fn ^:async f [c] (await (child/call-walk* c :walk {:pos [1 65 0]}))))]
          (is (= {:kind :done :data {:arrived true}} (select-keys (await (walk old-walker)) [:kind :data])))
          (let [e (await (walk old-stranded))]
            (is (= [:condition-failed :unreachable] ((juxt :kind :cause) e)) "an old job's stopped reason is the cause")
            (is (= :no-route (get-in e [:data :why]))))
          (is (= [:nothing-possible :tree-unreachable {:tried 2}]
                 ((juxt :kind :cause :data) (await (walk contract-gave-up))))))))))

(deftest call-walk*-yields-continue-while-the-walk-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :continue (await (in-parent {:check (fn [_] true) :round (fn ^:async w [_] :continue)}
                                           (fn ^:async f [c] (await (child/call-walk* c :walk {:pos [1 65 0]}))))))
            "no ending yet")))))

(deftest run!*-calls-until-an-ending-and-walk-into!*-returns-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :done (:kind (await (in-parent walks-twice (fn ^:async f [c] (await (child/run!* c :kid 'jobs.movement.go-to {:pos [1 65 0]}))))))))
        (is (= :nothing-possible (:kind (await (in-parent contract-gave-up (fn ^:async f [c] (await (child/walk-into!* c :in [1 65 0]))))))))
        (is (= :continue (await (in-parent {:check (fn [_] true) :round (fn ^:async w [_] :continue)}
                                           (fn ^:async f [c] (await (child/run!* c :kid 'jobs.movement.go-to {:pos [1 65 0]} {:max-calls 2}))))))
            "max-calls gives the round back")))))

(deftest walk!*-is-arrived-or-the-childs-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= :arrived (await (in-parent old-walker (fn ^:async f [c] (await (b/walk!* c [1 65 0])))))))
        (is (= [:condition-failed :unreachable] ((juxt :kind :cause) (await (in-parent old-stranded (fn ^:async f [c] (await (b/walk!* c [1 65 0]))))))))
        (is (= [:nothing-possible :out-of-reach]
               ((juxt :kind :cause) (await (in-parent old-walker (fn ^:async f [c] (await (b/walk!* c [4 65 4]))))))) "arrived, still out of reach")))))

(deftest child-wait*-is-a-condition-failed-ending-while-the-check-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [need (atom true)
              wait (fn [] (in-parent (old-kid need) (fn ^:async f [c] (b/child-wait* c :kid 'jobs.movement.go-to {}))))]
          (is (= [:condition-failed {:reason :need :what "thing"}] ((juxt :kind :cause) (await (wait)))))
          (reset! need false)
          (is (nil? (await (wait))) "a passing check waits on nothing"))))))
