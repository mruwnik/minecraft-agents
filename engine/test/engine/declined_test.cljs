(ns engine.declined-test
  "Ending-returning call helpers (jobs.lib.child, jobs.lib.blocks)."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.settings :as settings]
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
  ([go-to-def f seen] (in-parent go-to-def f seen (atom 1000000)))
  ([go-to-def f seen clock] (in-parent go-to-def f seen clock (atom 0)))
  ([go-to-def f seen clock age]
  (let [out (atom nil)
        parent {:round (fn ^:async r [c] (reset! out [(await (f c))]) (ctx/end c :done nil))}
        prims (doto (tu/fake {:self {:pos {:x 0 :y 65 :z 0}} :floor [-5 -5 5 5]})
                (aset "worldAge" (fn [] @age)))
        eng (core/create {:primitives prims
                          :jobs (assoc registry/jobs 'parent parent 'jobs.movement.go-to go-to-def)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock) :world (world/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [(fn [e] (swap! seen conj (tu/legacy-event e)))] :now #(deref clock)})})]
    (core/submit! eng (tu/order '(parent)) {})
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
          (is (= [(assoc gate :factor ##Inf)] (:tolls @args)))
          (is (some #(= :blocks.pen-kept-out (:kind %)) @seen)))))))

(deftest walk-into!*-asks-go-to-for-the-slots-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[slot expect] [[:walk {}] [:in {:settings {:jobs.lib.walk.world/y-margin child/entry-depth}}] [:other {}]]]
          (let [args (atom nil)
                go-to (assoc walker :round (fn ^:async w [c] (reset! args (:args c)) (ctx/end c :done nil {:arrived true})))]
            (await (in-parent go-to (fn ^:async g [c] (await (child/walk-into!* c slot [1 65 0])))))
            (is (= (merge {:pos [1 65 0] :range 0 :ways #{:walk :clear-seal}} expect) (select-keys @args [:pos :range :ways :checks :settings])) (str slot))))))))

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

(defn round [s] {:kind :nothing-possible :cause :x :report {:uses {:wall-time s}}})

(deftest a-slot-is-exhausted-once-another-round-is-not-worth-its-price
  ;; at the order's 1200 s, :child-round's pooled .043/.136: after 1 fruitless round 379 s, after 2 45 s, after 3 24 s,
  ;; after 4 16.5 s; a round with no seconds, or whose child was declined, is priced at the kind's 17.1 s; a round
  ;; begun (child/begin!) is priced from there, the caller's own 300 s in it counted though its child took 5 ms;
  ;; :craft-refused's .01/1 at 3.677 s allows 3
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              age (atom 0)
              declined {:kind :condition-failed :cause :x :report {:uses {:wall-time 0.005}}}]
          (is (= {:none false :30s [false false true] :300s [false true] :unpriced [false false false true] :progress false
                  :declined [false false false true] :begun [false true] :refused [false false false true]}
                 (await (in-parent walker
                                   (fn ^:async f [c]
                                     (let [after (fn [slot e] (child/fruitless! c slot e) (child/exhausted? c slot))
                                           begun (fn [] (child/begin! c :e) (swap! age + (settings/ms->ticks 300000)) (after :e (round 0.005)))]
                                       {:none (child/exhausted? c :a)
                                        :30s (mapv (fn [_] (after :a (round 30))) (range 3))
                                        :300s (mapv (fn [_] (after :b (round 300))) (range 2))
                                        :unpriced (mapv (fn [_] (after :d nil)) (range 4))
                                        :declined (mapv (fn [_] (after :f declined)) (range 4))
                                        :begun (mapv (fn [_] (begun)) (range 2))
                                        :refused (mapv (fn [_] (child/fruitless! c :g) (child/exhausted? c :g :craft-refused)) (range 4))
                                        :progress (do (child/progress! c :a) (child/exhausted? c :a))}))
                                   (atom []) clock age)))))))))

(deftest a-round-is-priced-in-world-age-from-its-latest-begin
  ;; at the order's 1200 s a 30 s round leaves another worth it (:child-round's .043/.136: 379 s); a round of 3630 s does
  ;; not. A host pause moves the wall clock an hour and the world age none; a round begun twice runs from its second
  ;; begin! though the first one's 1000 s of age passed unended
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              age (atom 0)
              ticks #(settings/ms->ticks (* 1000 %))
              round-of (fn [c slot s] (swap! age + (ticks s)) (child/fruitless! c slot (round 0.005)) (child/exhausted? c slot))
              f (fn ^:async f [c]
                  {:paused (do (child/begin! c :p) (swap! clock + 3600000) (round-of c :p 30))
                   :begun-twice (do (child/begin! c :b) (swap! age + (ticks 1000)) (child/begin! c :b) (round-of c :b 30))})]
          (is (= {:paused false :begun-twice false} (await (in-parent walker f (atom []) clock age)))))))))

(deftest the-give-up-ending-carries-the-last-fruitless-one-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:kind :nothing-possible :cause :no-births :data {:n 1} :because {:kind :nothing-possible :cause :x}}
               (await (in-parent walker
                                 (fn ^:async f [c]
                                   (child/fruitless! c :a {:kind :nothing-possible :cause :x :report {:uses {}}})
                                   (child/fruitless! c :a {:kind :done :report {:uses {}}})
                                   (select-keys (child/give-up c :a :no-births {:n 1}) [:kind :cause :data :because]))))))))))
