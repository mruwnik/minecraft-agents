(ns engine.give-test
  "jobs.items.give against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.settings :as settings]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.items.give :as give]))

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
        parent (tu/recording-parent job args out #(dissoc (tu/outcome %) :ending :status))
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/submit! eng '(recording-parent) {})
    {:eng eng :p p :clock clock :seen seen :out out}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn entities [p] (fake/entities p))
(defn inv [p] (reduce (fn [m i] (update m (.-name i) (fnil + 0) (.-count i))) {} (.-inventory (.self p))))
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

(deftest give-needs-a-player-and-an-item-name
  (are [args kind] (= kind (first (give/named {:args args})))
    {:to "Steve" :item "bread"} :ok
    {:to "Steve" :item nil} :never
    {:to nil :item "bread"} :never
    {:to 5 :item "bread"} :never))

(deftest gives-the-count-to-a-player-ten-away
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (tu/each-async
                [[{:to "Steve" :item "bread" :count 5} 5 {}]
                 [{:to "Steve" :item "bread" :count 2} 2 {"bread" 3}]
                 [{:to "Steve" :item "bread"} 5 {}]]
                (fn ^:async one [[args given left]]
                  (let [{:keys [p out] :as s} (await (give (assoc bread :entities [(steve 10)]) args 12 true))]
                    (is (= {:given given :text (str "gave " given)} @out))
                    (is (= 1 (count (tu/walked-to (:eng s)))))
                    (is (= 1 (count (calls p "toss"))))
                    (is (= left (inv p)))))))))))

(deftest nothing-carried-gives-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (give {:entities [(steve 10)]} {:to "Steve" :item "bread"} 5 true))]
          (is (= {:reason :no-item} @out) "fails its :no-item condition")
          (is (empty? (calls p "toss"))))))))

(deftest can-run-asks-the-item-and-the-player-until-the-toss
  (let [c (fn [world mem] (with-redefs [ctx/mem (constantly mem)]
                            (give/can-run? {:primitives (tu/fake-on-floor world) :args {:to "Steve" :item "bread"}})))
        reason (comp :reason last)]
    (is (= [:ok nil] [(first (c (assoc bread :entities [(steve 10)]) {})) nil]))
    (is (= [:later :gone] [(first (c bread {})) (reason (c bread {}))]))
    (is (= [:later :no-item] [(first (c {:entities [(steve 10)]} {})) (reason (c {:entities [(steve 10)]} {}))]))
    (is (= :ok (first (c {} {:tossed 2}))))
    (is (= :later (first (settings/scoped {::give/radius 5} #(c (assoc bread :entities [(steve 10)]) {})))) "the radius setting bounds the look for the player")))

(deftest three-blocked-walks-are-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (give (assoc bread :entities [(steve 10)] :unreachable ["10,64,0"])
                                                 {:to "Steve" :item "bread"} 12 true))]
          (is (= {:given 0 :reason :unreachable :text "cannot reach Steve"} @out))
          (is (= 3 (count (tu/walked-to (:eng s)))))
          (is (empty? (calls p "toss"))))))))

(deftest a-drop-never-taken-is-collected-back-after-wait-s
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out clock] :as s} (await (give (assoc bread :entities [(steve 10)])
                                                      {:to "Steve" :item "bread" :count 5} 40 false))]
          (is (= {:given 0 :reason :not-taken :returned 5 :text "gave 0 bread (not-taken)"} @out))
          (is (= {"bread" 5} (inv p)))
          (is (= 1 (count (calls p "collect"))))
          (is (some #(= "giving" (some-> (.-args %) .-why .-name)) (calls p "wait")))
          (is (<= 6000 (- @clock 1000000))))))))

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
                                         {:to "Steve" :item "bread" :count 5} 40 false nil hook))]
          (is (= {:given 3 :returned 2 :text "gave 3 bread"} @out)))))))

(deftest a-toss-that-keeps-failing-gives-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (give (assoc bread :entities [(steve 10)])
                                                {:to "Steve" :item "bread"} 20 true
                                                (fn [p] (.override (.-world p) "toss" (fn ^:async f [_ _ _] #js {:status "no-item" :count 0})))))]
          (is (= {:given 0 :reason :no-item :text "gave 0 bread (no-item)"} @out))
          (is (= 3 (count (calls p "toss")))))))))

(deftest a-drop-that-cannot-be-collected-ends-as-litter
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (give (assoc bread :entities [(steve 10)])
                                                {:to "Steve" :item "bread" :count 5} 60 false
                                                (fn [p] (.override (.-world p) "collect" (fn ^:async f [_ _ _] #js {:status "unreachable" :gained #js []})))))]
          (is (= {:given 5 :reason :litter :returned 0 :text "gave 5 bread (litter)"} @out))
          (is (= 3 (count (calls p "collect")))))))))

(deftest collects-that-fail-in-a-row-give-up-but-a-collect-between-resets-the-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [statuses (atom (cycle ["unreachable" "unreachable" "collected"]))
              {:keys [p out]} (await (give (assoc bread :entities [(steve 10)])
                                           {:to "Steve" :item "bread" :count 5} 14 false
                                           (fn [p] (.override (.-world p) "collect"
                                                              (fn ^:async f [_ _ _]
                                                                (let [st (first @statuses)]
                                                                  (swap! statuses rest)
                                                                  (when (> (count (calls p "collect")) 8) (take-drops! p))
                                                                  #js {:status st :gained (if (= "collected" st) #js [#js {:name "bread" :count 1}] #js [])}))))))]
          (is (= {:given 5 :returned 0 :text "gave 5 bread"} @out) "two fails, a collect, two fails do not end it as litter")
          (is (> (count (calls p "collect")) 4)))))))

(deftest a-drop-that-spawns-late-is-not-reported-as-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [held (atom nil)
              {:keys [p out] :as s} (await (give (assoc bread :entities [(steve 10)])
                                                {:to "Steve" :item "bread" :count 5} 60 false
                                                #(late-drop % held) (appear-two-ticks-after-toss held)))]
          (is (= {:given 0 :reason :not-taken :returned 5 :text "gave 0 bread (not-taken)"} @out) "waited for the drop, then took it back")
          (is (= 1 (count (calls p "collect")))))))))

(deftest a-drop-that-never-appears-is-unconfirmed-not-given-after-the-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [held (atom nil)
              {:keys [out clock] :as s} (await (give (assoc bread :entities [(steve 10)])
                                                    {:to "Steve" :item "bread" :count 5} 30 false
                                                    #(late-drop % held)))]
          (is (= {:given 0 :tossed 5 :reason :unconfirmed :text "never saw the drop of bread"} @out))
          (is (<= 6000 (- @clock 1000000))))))))

(defn stray-bread-and-player-takes-the-drop
  "A hook: the tick after the toss a bread entity far away appears, the tick after that the player takes the
  drops near them."
  []
  (let [ticks (atom 0)]
    (fn [p _]
      (when (seq (calls p "toss"))
        (let [n (swap! ticks inc)]
          (when (= 1 n)
            (swap! (fake/state p) update :entities conj
                   {:id 99 :name "item" :kind "item" :pos [25 64 0] :item {:name "bread" :count 3}}))
          (when (= 3 n)
            (swap! (fake/state p) update :entities
                   (fn [es] (filterv #(not (and (= "item" (:kind %)) (< (first (:pos %)) 15))) es)))))))))

(deftest an-unrelated-drop-far-from-the-toss-is-not-the-toss
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p out] :as s} (await (give (assoc bread :entities [(steve 10)])
                                                {:to "Steve" :item "bread" :count 5} 20 false
                                                nil (stray-bread-and-player-takes-the-drop)))]
          (is (= {:given 5 :text "gave 5"} @out) "the player took the toss; the stray bread is not it")
          (is (empty? (calls p "collect")) "the body does not walk to the stray"))))))

(deftest the-walk-and-the-toss-are-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (assoc bread :entities [(steve 10)]) {:to "Steve" :item "bread" :count 5})]
          (await (core/tick! eng))
          (is (= 1 (count (calls p "toss")))))))))

(deftest a-give-cut-while-the-drop-lies-takes-it-back-in-its-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup (assoc bread :entities [(steve 10)]) {:to "Steve" :item "bread" :count 5})
              cut (atom false)]
          (await (run-ticks s 40 500 false
                            (fn [p _]
                              (when (and (not @cut) (seq (calls p "toss")) (pos? (item-count p)))
                                (reset! cut true)
                                (core/cancel! eng (first (:list (core/state eng))))))))
          (is @cut "the cut came with the drop lying")
          (is (= {"bread" 5} (inv p)) "the cleanup took the drop back")
          (is (zero? (item-count p))))))))

(deftest a-give-cleanup-leaves-a-same-item-drop-larger-than-what-it-tossed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup (assoc bread :entities [(steve 10)]) {:to "Steve" :item "bread" :count 5})
              cut (atom false)]
          (await (run-ticks s 40 500 false
                            (fn [p _]
                              (when (and (not @cut) (seq (calls p "toss")) (pos? (item-count p)))
                                (reset! cut true)
                                (swap! (fake/state p) update :entities conj
                                       (assoc (first (filter #(= "item" (:kind %)) (entities p))) :id 900 :item {:name "bread" :count 8}))
                                (core/cancel! eng (first (:list (core/state eng))))))))
          (is @cut)
          (is (= {"bread" 5} (inv p)) "only the tossed count came back")
          (is (= [900] (map :id (filter #(= "item" (:kind %)) (entities p))))))))))
