(ns engine.give-test
  "jobs.items.give against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]))

(def job 'jobs.items.give)

(defn steve
  "A player entity named Steve at x along the row the body stands on."
  [x]
  {:id 5 :name "Steve" :kind "player" :pos {:x x :y 64 :z 0}})

(def bread {:inventory [{:name "bread" :count 5}]})

(defn setup
  "An engine over the fake world; a recording parent runs the job as its child
  and puts the child's result in :out when it finishes."
  [world args]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    {:eng eng :p p :clock clock :seen seen :out out}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn entities [p] (fake/entities p))
(defn inv [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn has-event? [{:keys [seen]} kind] (boolean (some #(= kind (:kind %)) @seen)))

(defn take-drops!
  "The player picks up every item entity lying in the world."
  [p]
  (swap! (fake/state p) update :entities #(filterv (fn [e] (not= "item" (:kind e))) %)))

(defn item-count [p] (count (filter #(= "item" (:kind %)) (entities p))))

(defn ^:async run-ticks
  "Tick up to n times, the clock moving step ms before each, until the job is gone.
  hook is called with the primitives and the tick index first. takes? has the
  player take the drops once the job has had a tick to see them."
  [{:keys [eng clock p]} n step takes? hook]
  (let [lying-before (atom false)]
    (loop [i 0]
      (when (and (< i n) (seq (:list (core/state eng))))
        (hook p i)
        (when (and takes? @lying-before) (take-drops! p))
        (reset! lying-before (pos? (item-count p)))
        (swap! clock + step)
        (await (core/tick! eng))
        (recur (inc i))))))

(defn ^:async give
  "Setup, run, return the setup map. takes? has the player take every drop; prep
  is called with the primitives first; hook before every tick with the primitives and the index."
  [world args n takes? & [prep hook]]
  (let [s (setup world args)]
    (when prep (prep (:p s)))
    (await (run-ticks s n 500 takes? (or hook (fn [_ _]))))
    s))

(defn late-drop
  "Override toss so the drop spawns late: the toss takes the items and the
  entity is held back in the atom."
  [p held]
  (.override (.-world p) "toss"
             (fn ^:async f [token a impl]
               (let [r (await (impl token a))]
                 (reset! held (last (entities p)))
                 (swap! (fake/state p) update :entities pop)
                 r))))

(defn appear-two-ticks-after-toss
  "A hook that puts the held drop into the world on the second tick after the toss."
  [held]
  (let [waited (atom 0)]
    (fn [p _]
      (when (seq (calls p "toss"))
        (when (and @held (= 2 (swap! waited inc)))
          (swap! (fake/state p) update :entities conj @held)
          (reset! held nil))))))

(deftest give-check-wants-strings
  (are [args ok] (= ok ((:check (get registry/jobs job)) {:args args}))
    {:player "Steve" :item "bread"} true
    {:player "Steve" :item nil} false
    {:player nil :item "bread"} false
    {:player 5 :item "bread"} false))

(deftest gives-the-count-to-a-player-ten-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args given left] [[{:player "Steve" :item "bread" :count 5} 5 {}]
                                   [{:player "Steve" :item "bread" :count 2} 2 {"bread" 3}]
                                   [{:player "Steve" :item "bread"} 5 {}]]]
          (let [{:keys [p out] :as s} (await (give (assoc bread :entities [(steve 10)]) args 12 true))]
            (is (= {:given given} @out))
            (is (= 1 (count (tu/walked-to (:eng s)))))
            (is (= 1 (count (calls p "toss"))))
            (is (= left (inv p)))
            (is (has-event? s :give.done))))))))

(deftest nothing-carried-gives-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (give {:entities [(steve 10)]} {:player "Steve" :item "bread"} 5 true))]
          (is (= {:given 0 :reason "no-item"} @out))
          (is (empty? (calls p "toss")))
          (is (has-event? s :give.no-item)))))))

(deftest a-player-who-is-never-there-is-gone-after-two-seconds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out clock] :as s} (await (give bread {:player "Steve" :item "bread"} 12 true))]
          (is (= {:given 0 :reason "gone"} @out))
          (is (<= 2000 (- @clock 1000000)))
          (is (empty? (calls p "toss")))
          (is (has-event? s :give.gone)))))))

(deftest three-blocked-walks-are-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (give (assoc bread :entities [(steve 10)] :unreachable ["10,64,0"])
                                                 {:player "Steve" :item "bread"} 12 true))]
          (is (= {:given 0 :reason "unreachable"} @out))
          (is (= 3 (count (tu/walked-to (:eng s)))))
          (is (empty? (calls p "toss")))
          (is (has-event? s :give.unreachable)))))))

(deftest a-drop-never-taken-is-collected-back-after-wait-s
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out clock] :as s} (await (give (assoc bread :entities [(steve 10)])
                                                      {:player "Steve" :item "bread" :count 5} 40 false))]
          (is (= {:given 0 :reason "not-taken" :returned 5} @out))
          (is (= {"bread" 5} (inv p)))
          (is (= 1 (count (calls p "collect"))))
          (is (<= 6000 (- @clock 1000000)))
          (is (has-event? s :give.returned)))))))

(deftest a-part-of-the-toss-coming-back-counts-what-was-given
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [done? (atom false)
              hook (fn [p _]
                     (when (and (not @done?) (seq (calls p "toss")))
                       (reset! done? true)
                       (take-drops! p)
                       (fake/add-item! p "bread" 2)))
              {:keys [out]} (await (give (assoc bread :entities [(steve 10)])
                                         {:player "Steve" :item "bread" :count 5} 40 false nil hook))]
          (is (= {:given 3 :reason "not-taken" :returned 2} @out)))))))

(deftest a-toss-that-keeps-failing-gives-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (give (assoc bread :entities [(steve 10)])
                                                {:player "Steve" :item "bread"} 20 true
                                                (fn [p] (.override (.-world p) "toss" (fn ^:async f [_ _ _] #js {:status "no-item" :count 0})))))]
          (is (= {:given 0 :reason "no-item"} @out))
          (is (= 3 (count (calls p "toss"))))
          (is (has-event? s :give.gave-up)))))))

(deftest a-drop-that-cannot-be-collected-ends-as-litter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (give (assoc bread :entities [(steve 10)])
                                                {:player "Steve" :item "bread" :count 5} 60 false
                                                (fn [p] (.override (.-world p) "collect" (fn ^:async f [_ _ _] #js {:status "unreachable" :gained #js []})))))]
          (is (= {:given 5 :reason "litter" :returned 0} @out))
          (is (= 3 (count (calls p "collect"))))
          (is (has-event? s :give.gave-up)))))))

(deftest a-drop-that-spawns-late-is-not-reported-as-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [held (atom nil)
              {:keys [p out] :as s} (await (give (assoc bread :entities [(steve 10)])
                                                {:player "Steve" :item "bread" :count 5} 60 false
                                                #(late-drop % held) (appear-two-ticks-after-toss held)))]
          (is (= {:given 0 :reason "not-taken" :returned 5} @out) "waited for the drop, then took it back")
          (is (= 1 (count (calls p "collect"))))
          (is (not (has-event? s :give.done))))))))

(deftest a-drop-that-never-appears-is-unconfirmed-after-three-seconds
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [held (atom nil)
              {:keys [out clock] :as s} (await (give (assoc bread :entities [(steve 10)])
                                                    {:player "Steve" :item "bread" :count 5} 30 false
                                                    #(late-drop % held)))]
          (is (= {:given 5 :reason "unconfirmed"} @out))
          (is (<= 3000 (- @clock 1000000)))
          (is (has-event? s :give.unconfirmed)))))))
