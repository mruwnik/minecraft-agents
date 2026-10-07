(ns engine.pen-gate-test
  "The pen-gate trigger (triggers.animals.pen-gate) and jobs.animals.shut-gate against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.pen-gate :as pg]
            [triggers.animals.pen-gate :as trigger]
            [jobs.lib.world-files :as world]))

;; ------------------------------------------------------------------ plans

(def gate-want [:any "oak_fence_gate" "spruce_fence_gate"])

(defn pen-plan
  "A pen plan: a fence part and a gate part at gate-cell (a vector)."
  [id gate-cell]
  {:id id
   :parts [{:id "fence" :outline [[0 64 0] [4 64 4]] :want "oak_fence"}
           {:id "gate" :cells [gate-cell] :want gate-want}]})

(defn answers [plans]
  (let [w (world/of-data plans {})]
    (keep #(world/plan w %) (keys plans))))

(def pen-a (pen-plan "pen-a" [2 64 0]))
(def pen-b (pen-plan "pen-b" [20 64 0]))

;; ------------------------------------------------------------------ the gate index

(deftest gate-cells-are-the-cells-of-the-plans-that-want-a-gate
  (is (= {[2 64 0] "pen-a" [20 64 0] "pen-b"}
         (pg/gate-cells (answers {"pen-a" pen-a "pen-b" pen-b})))
      "several plans; fence cells are not gates"))

(deftest a-want-names-a-gate-as-a-name-a-choice-or-a-block-with-state
  (are [want gate?] (= gate? (boolean (pg/gate-want? want)))
    "oak_fence_gate" true
    [:any "oak_fence" "birch_fence_gate"] true
    {:block "oak_fence_gate" :facing "north"} true
    [:any {:block "oak_fence_gate" :facing "north"}] true
    "oak_fence" false
    :clear false
    {:crop "wheat"} false))

(deftest a-broken-plan-gives-no-gates
  (is (= {} (pg/gate-cells [{:id "x" :broken "bad"}]))))

(deftest the-index-follows-an-edited-plan-and-is-kept-between-reads
  (let [w (world/of-data {"pen-a" pen-a} {})
        first-read (pg/gate-index w)]
    (is (= {[2 64 0] "pen-a"} first-read))
    (is (identical? first-read (pg/gate-index w)) "no plan change: the same index, not rebuilt")
    (world/set-data! w {"pen-a" (pen-plan "pen-a" [3 64 4])} {})
    (is (= {[3 64 4] "pen-a"} (pg/gate-index w)) "the gate moved in the plan")
    (world/set-data! w {} {})
    (is (= {} (pg/gate-index w)) "plan removed: no gates")))

;; ------------------------------------------------------------------ the condition

(def gates {[2 64 0] "pen-a"})

(defn blocks-of [open?]
  (fn [{:keys [x y z]}]
    (when (= [x y z] [2 64 0]) #js {:name "oak_fence_gate" :properties #js {:open open?}})))

(deftest an-open-planned-gate-is-found-open-and-a-shut-or-other-block-is-not
  (are [block open?] (= open? (boolean (pg/open-gate? block)))
    #js {:name "oak_fence_gate" :properties #js {:open true}} true
    #js {:name "oak_fence_gate" :properties #js {:open "true"}} true
    #js {:name "oak_fence_gate" :properties #js {:open false}} false
    #js {:name "oak_fence_gate" :properties #js {}} false
    #js {:name "oak_fence" :properties #js {:open true}} false
    nil false))

(deftest open-gates-lists-the-open-planned-cells
  (is (= [[2 64 0]] (pg/open-cells (blocks-of true) (keys gates))))
  (is (= [] (pg/open-cells (blocks-of false) (keys gates))))
  (is (= [] (pg/open-cells (constantly nil) (keys gates))) "unloaded: not open"))

(deftest candidates-are-farther-than-min-dist-and-within-radius
  (are [self ok?] (= ok? (boolean (seq (pg/candidates self [[2 64 0]] {:radius 8 :min-dist 2}))))
    {:x 2.5 :y 64 :z 0.5} false   ; standing in the gate cell
    {:x 2.5 :y 64 :z 2.0} false   ; 1.5 away
    {:x 2.5 :y 64 :z 3.5} true    ; 3 away
    {:x 2.5 :y 64 :z 8.4} true    ; 7.9 away
    {:x 2.5 :y 64 :z 9.6} false   ; 9.1 away: out of the radius
    {:x 2.5 :y 80 :z 3.5} false)) ; far above

(deftest quiet-cells-are-the-gates-given-up-lately
  (let [data (-> mem/empty-data
                 (mem/add-entry :gate-gave-up {:t 1000 :data {:cell [2 64 0]}} nil)
                 (mem/add-entry :gate-gave-up {:t 500000 :data {:cell [20 64 0]}} nil))]
    (is (= #{[20 64 0]} (pg/quiet-cells {:data data :now 600000} 300000)))
    (is (= #{[2 64 0] [20 64 0]} (pg/quiet-cells {:data data :now 600000} 700000)))))

(deftest held-cells-are-the-gates-a-job-holds-open-on-purpose
  (let [data (mem/add-entry mem/empty-data :gate-held {:t 0 :data {:cell [2 64 0]}} nil)]
    (are [now held] (= held (pg/held-cells {:data data :now now} 30000))
      10000 #{[2 64 0]}
      29999 #{[2 64 0]}
      31000 #{})))

;; ------------------------------------------------------------------ the trigger

(def when-gate trigger/pen-gate)

(defn fake-at
  "A fake world: the gate of pen-a open or shut, the body at x z."
  [open? x z & [more]]
  (tu/seeing-all (tu/fake-on-floor (merge {:self {:pos {:x x :y 64 :z z}}
                   :blocks {"2,64,0" "oak_fence_gate"}
                   :states {"2,64,0" {:open open?}}}
                  more))))

(defn holds-over
  "Ask the trigger at each time in times; the answers."
  [p kn times & [data]]
  (let [args (:args (:pen-gate triggers/all))]
    (mapv #(boolean (when-gate p {:data (or data mem/empty-data) :now %} args kn)) times)))

(defn knowledge [& plans] (world/of-data (into {} (map (juxt :id identity)) plans) {}))

(def times [0 1000 2000 3000 4000 5000 6000])

(def f7 [false false false false false false false])
(def t7 [true true true true true true true])

(deftest the-trigger-fires-when-a-planned-gate-stands-open-with-the-body-away
  (is (= t7 (holds-over (fake-at true 2 5) (knowledge pen-a) times)) "open, body 5 away")
  (is (= f7 (holds-over (fake-at false 2 5) (knowledge pen-a) times)) "shut")
  (is (= f7 (holds-over (fake-at true 2 0) (knowledge pen-a) times)) "the body stands in the gate cell")
  (is (= f7 (holds-over (fake-at true 2 1) (knowledge pen-a) times)) "the body 1 block from it: a job at the gate")
  (is (= f7 (holds-over (fake-at true 2 20) (knowledge pen-a) times)) "too far away")
  (is (= f7 (holds-over (fake-at true 2 5) (knowledge) times)) "the gate is in no plan")
  (is (= f7 (holds-over (fake-at true 2 5) nil times)) "no world data at all"))

(deftest the-trigger-reads-the-gate-as-last-seen-never-through-a-wall
  (is (= f7 (holds-over (tu/blind (fake-at true 2 5)) (knowledge pen-a) times)) "never seen")
  (let [p (fake-at true 2 5)]
    (aset p "seenBlockAt" (fn [pos] #js {:name "oak_fence_gate" :properties #js {:open "false"} :pos pos :age-ms 5000}))
    (is (= f7 (holds-over p (knowledge pen-a) times)) "seen shut, opened unseen since: still shut")))

(deftest a-gate-a-walk-opened-is-the-walks-business-the-trigger-has-no-clock
  (let [data (mem/add-entry mem/empty-data :opened {:t 0 :data {:cell {:x 2 :y 64 :z 0} :by "j1" :shut? true}} nil)]
    (is (= f7 (holds-over (fake-at true 2 5) (knowledge pen-a) times data)) "an :opened entry: the shut-doors job's gate")
    (is (= t7 (holds-over (fake-at true 2 5) (knowledge pen-a) times)) "the same answer at every look, nothing kept between")))

(deftest a-gate-given-up-lately-does-not-fire-again
  (let [data (mem/add-entry mem/empty-data :gate-gave-up {:t 0 :data {:cell [2 64 0]}} nil)]
    (is (= f7 (holds-over (fake-at true 2 5) (knowledge pen-a) times data)))))

(deftest the-trigger-is-registered-with-the-job-that-shuts-the-gate
  (is (= trigger/pen-gate (:when (:pen-gate triggers/all))))
  (is (= '(jobs.animals.shut-gate) (:job (:pen-gate triggers/all))))
  (is (= :cooldown (:persistence (:pen-gate triggers/all)))))

;; ------------------------------------------------------------------ the job

(def clock (atom 1000000))

(defn start [spec plans & [extra-jobs]]
  (let [[seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor spec))
        w (world/of-data plans {})
        eng (core/create {:primitives p :jobs (merge registry/jobs extra-jobs) :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})
                          :world w})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-job
  "Submit the job with args and tick up to n times until the list is empty; {:eng :p :seen :ticks}."
  [spec plans args n & [extra-jobs]]
  (let [s (start spec plans extra-jobs)]
    (core/submit! (:eng s) (list 'jobs.animals.shut-gate args) {})
    (loop [i 0]
      (if (or (>= i n) (empty? (:list (core/state (:eng s)))))
        (assoc s :ticks i)
        (do (swap! clock + 700)
            (await (core/tick! (:eng s)))
            (recur (inc i)))))))

(defn gate-open? [p x y z]
  (let [b (.blockAt p #js {:x x :y y :z z})]
    (boolean (some-> b .-properties .-open))))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))

(def ring
  (into {} (for [x (range 0 5) z (range 0 5) :when (or (#{0 4} x) (#{0 4} z))] [(str x ",64," z) "oak_fence"])))

(def ground (into {} (for [x (range -5 30) z (range -5 30)] [(str x ",63," z) "stone"])))

(defn pen-world
  "The pen ring with its gate at 2,64,0 (open?), the body at x z, and a second gate of pen-b at 20,64,0 when more."
  [open? x z & [more]]
  (merge {:self {:pos {:x x :y 64 :z z}}
          :blocks (merge ground ring {"2,64,0" "oak_fence_gate"})
          :states {"2,64,0" {:open open?}}}
         more))

(deftest the-job-walks-up-and-shuts-an-open-planned-gate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-job (pen-world true 2 -7) {"pen-a" pen-a} {} 20))]
          (is (not (gate-open? (:p s) 2 64 0)))
          (is (empty? (:list (core/state (:eng s)))))
          (is (= 1 (:shut (first (events-of s :shut-gate.done)))) "one gate shut, reported"))))))

(defn waited-ms-before-click
  "The ms of the wait calls the body made before its first click on the gate."
  [s]
  (->> (.-calls (.-world (:p s))) (take-while #(not= "useOn" (.-name %))) (filter #(= "wait" (.-name %)))
       (map #(aget (.-args %) "ms")) (reduce + 0)))

(deftest a-gate-just-opened-is-given-its-grace-before-the-job-shuts-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-job (pen-world true 2 -7) {"pen-a" pen-a} {} 30))]
          (is (>= (waited-ms-before-click s) 4000) "without :plan the job waits :open-s (4 s) first")
          (is (not (gate-open? (:p s) 2 64 0))))
        (let [s (await (run-job (pen-world true 2 -7) {"pen-a" pen-a} {:open-s 0} 30))]
          (is (< (waited-ms-before-click s) 1000) ":open-s 0: no grace")
          (is (not (gate-open? (:p s) 2 64 0))))
        (let [s (await (run-job (pen-world true 2 -7) {"pen-a" pen-a} {:plan "pen-a"} 30))]
          (is (< (waited-ms-before-click s) 1000) "with :plan the caller asked: no grace"))))))

(deftest a-gate-a-player-shuts-in-the-grace-is-left-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-job (pen-world false 2 -7) {"pen-a" pen-a} {} 30))]
          (is (zero? (waited-ms-before-click s)) "nothing open: no wait")
          (is (= 0 (:shut (first (events-of s :shut-gate.done))))))))))

(deftest a-gate-shut-during-the-grace-wait-is-not-clicked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (start (pen-world true 2 -7) {"pen-a" pen-a} nil)]
          (.override (.-world p) "wait"
                     (fn [token args impl]
                       (swap! (fake/state p) assoc-in [:states [2 64 0] :open] false)
                       (impl token args)))
          (core/submit! eng (list 'jobs.animals.shut-gate {}) {})
          (loop [i 0]
            (when (and (< i 30) (seq (:list (core/state eng))))
              (swap! clock + 700)
              (await (core/tick! eng))
              (recur (inc i))))
          (is (pos? (count (filter #(= "wait" (.-name %)) (.-calls (.-world p))))) "it waited the grace")
          (is (not (gate-open? p 2 64 0)))
          (is (empty? (filter #(= "useOn" (.-name %)) (.-calls (.-world p)))) "no click on the gate the player shut")
          (is (= 0 (:shut (first (events-of s :shut-gate.done))))))))))

(deftest from-the-far-side-the-job-walks-round-the-ring-and-shuts-the-gate
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-job (pen-world true 2 7) {"pen-a" pen-a} {} 30))]
          (is (seq (mem/entries (mem/view (:store (:eng s))) :moved)) "it walked")
          (is (not (gate-open? (:p s) 2 64 0)))
          (is (= 1 (:shut (first (events-of s :shut-gate.done)))))
          (is (empty? (events-of s :shut-gate.gave-up))))))))

(deftest a-gate-already-shut-is-done-at-once-without-a-click
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-job (pen-world false 2 7) {"pen-a" pen-a} {} 20))]
          (is (<= (:ticks s) 2))
          (is (zero? (count (filter #(= "useOn" (.-name %)) (.-calls (.-world (:p s)))))))
          (is (= 0 (:shut (first (events-of s :shut-gate.done))))))))))

(deftest a-gate-in-no-plan-is-left-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-job (pen-world true 2 7) {} {} 20))]
          (is (gate-open? (:p s) 2 64 0)))))))

(deftest the-body-standing-in-the-gate-cell-does-not-shut-it-under-itself
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-job (pen-world true 2 0) {"pen-a" pen-a} {} 20))]
          (is (gate-open? (:p s) 2 64 0))
          (is (= [{:cell [2 64 0] :reason :standing-in}] (:left (first (events-of s :shut-gate.stopped)))))
          (is (empty? (events-of s :shut-gate.done)) "a gate left open is not done"))))))

(deftest with-a-plan-every-open-gate-of-that-plan-is-shut-and-other-plans-are-not
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [two (pen-plan "pen-a" [2 64 0])
              two (update two :parts conj {:id "gate2" :cells [[0 64 2]] :want gate-want})
              spec (pen-world true 2 -6 {:blocks (merge ground ring {"2,64,0" "oak_fence_gate" "0,64,2" "oak_fence_gate" "20,64,0" "oak_fence_gate"})
                                        :states {"2,64,0" {:open true} "0,64,2" {:open true} "20,64,0" {:open true}}})
              s (await (run-job spec {"pen-a" two "pen-b" pen-b} {:plan "pen-a"} 40))]
          (is (not (gate-open? (:p s) 2 64 0)))
          (is (not (gate-open? (:p s) 0 64 2)))
          (is (gate-open? (:p s) 20 64 0) "the other plan's gate stays open")
          (is (= 2 (:shut (first (events-of s :shut-gate.done))))))))))

(deftest one-run-shuts-all-gates-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [two (update (pen-plan "pen-a" [2 64 0]) :parts conj {:id "gate2" :cells [[0 64 2]] :want gate-want})
              spec (pen-world true 2 -6 {:blocks (merge ground ring {"2,64,0" "oak_fence_gate" "0,64,2" "oak_fence_gate"})
                                        :states {"2,64,0" {:open true} "0,64,2" {:open true}}})
              s (await (run-job spec {"pen-a" two} {:plan "pen-a"} 1))]
          (is (empty? (:list (core/state (:eng s)))) "one tick: the job ended")
          (is (not (gate-open? (:p s) 2 64 0)))
          (is (not (gate-open? (:p s) 0 64 2)))
          (is (= 2 (:shut (first (events-of s :shut-gate.done))))))))))

(deftest a-gate-given-up-ends-the-run-stopped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-job (pen-world true 2 7 {:unreachable ["2,64,0"]}) {"pen-a" pen-a} {} 1))]
          (is (empty? (:list (core/state (:eng s)))) "one tick: the job ended")
          (is (empty? (events-of s :shut-gate.done)))
          (is (= [[2 64 0]] (mapv :cell (:left (first (events-of s :shut-gate.stopped)))))))))))

(deftest a-cow-that-stays-in-the-gate-is-waited-out-not-pushed-and-the-run-ends-stopped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cow {:id 7 :name "cow" :kind "passive" :pos {:x 2.5 :y 64 :z 0.5}}
              s (await (run-job (pen-world true 2 -6 {:entities [cow]}) {"pen-a" pen-a} {} 1))]
          (is (gate-open? (:p s) 2 64 0) "the cow is not pushed")
          (is (zero? (count (filter #(= "useOn" (.-name %)) (.-calls (.-world (:p s)))))) "no click")
          (is (= [{:cell [2 64 0] :reason :animal-in-the-way}] (:left (first (events-of s :shut-gate.stopped))))))))))

(deftest without-a-plan-only-gates-within-the-radius-are-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec (pen-world true 2 -6 {:blocks (merge ground ring {"2,64,0" "oak_fence_gate" "20,64,0" "oak_fence_gate"})
                                        :states {"2,64,0" {:open true} "20,64,0" {:open true}}})
              s (await (run-job spec {"pen-a" pen-a "pen-b" pen-b} {} 40))]
          (is (not (gate-open? (:p s) 2 64 0)))
          (is (gate-open? (:p s) 20 64 0) "12 blocks off: beyond the radius"))))))

(deftest a-plan-that-is-missing-is-declined-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[plans reason] [[{} "no such plan"]]]
          (let [s (await (run-job (pen-world true 2 7) plans {:plan "pen-a"} 6))
                warns (events-of s :shut-gate.declined)]
            (is (= 1 (count warns)) reason)
            (is (re-find (re-pattern reason) (:text (first warns))))
            (is (gate-open? (:p s) 2 64 0))))))))

(deftest an-unreachable-gate-is-given-up-with-one-warn-and-remembered
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (start (pen-world true 2 7 {:unreachable ["2,64,0"]}) {"pen-a" pen-a})
              p (:p s)]
          (core/submit! (:eng s) '(jobs.animals.shut-gate) {})
          (dotimes [_ 12]
            (swap! clock + 700)
            (await (core/tick! (:eng s))))
          (is (gate-open? p 2 64 0))
          (is (= 1 (count (events-of s :shut-gate.gave-up))) "one warn, not one per try")
          (is (= [2 64 0] (:cell (first (events-of s :shut-gate.gave-up)))))
          (is (empty? (:list (core/state (:eng s)))) "the job ended")
          (is (= [[2 64 0]] (mapv (comp :cell :data) (mem/entries (mem/view (:store (:eng s))) :gate-gave-up)))))))))

(deftest the-registered-reflex-shuts-a-gate-left-open-beside-an-idle-body
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start (pen-world true 2 -5) {"pen-a" pen-a})]
          (core/register-reflex! eng {:trigger :pen-gate})
          (loop [i 0]
            (when (and (< i 30) (gate-open? p 2 64 0))
              (swap! clock + 700)
              (await (core/tick! eng))
              (recur (inc i))))
          (is (not (gate-open? p 2 64 0)))
          (dotimes [_ 3]
            (swap! clock + 700)
            (await (core/tick! eng)))
          (is (some #(and (= :reflex (:source %)) (= :fired (:kind %)) (= :pen-gate (:reflex %))) @seen))
          (is (some #(= :shut-gate.done (:kind %)) @seen))
          (is (empty? (:list (core/state eng))) "the reflex job left the list"))))))

(deftest a-gate-held-by-a-job-does-not-fire-until-the-entry-is-old
  (let [data (mem/add-entry mem/empty-data :gate-held {:t 0 :data {:cell [2 64 0]}} nil)
        ask (fn [ts] (holds-over (fake-at true 2 5) (knowledge pen-a) ts data))]
    (is (= f7 (ask times)) "entry 0-6 s old: left alone")
    (is (= t7 (ask (range 31000 38000 1000))) "entry 31 s old: fires")))

(defn gave-up-cells [s] (mapv (comp :cell :data) (mem/entries (mem/view (:store (:eng s))) :gate-gave-up)))

(deftest a-cow-that-stays-leaves-a-short-gave-up-entry-so-the-trigger-does-not-refire-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cow {:id 7 :name "cow" :kind "passive" :pos {:x 2.5 :y 64 :z 0.5}}
              s (await (run-job (pen-world true 2 -6 {:entities [cow]}) {"pen-a" pen-a} {} 1))]
          (is (= [[2 64 0]] (gave-up-cells s))))))))

(deftest a-walk-that-ends-continue-is-not-read-as-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stub {'jobs.movement.go-to {:check (constantly true) :args {} :round (fn ^:async r [_c] :continue)}}
              s (await (run-job (pen-world true 2 7) {"pen-a" pen-a} {} 1 stub))]
          (is (= [:walk-interrupted] (mapv :reason (:left (first (events-of s :shut-gate.stopped))))))
          (is (empty? (events-of s :shut-gate.gave-up)) "no warn: it is not a verdict on the gate")
          (is (= [[2 64 0]] (gave-up-cells s)) "a short entry"))))))

;; ------------------------------------------------------------------ gates not seen lately

(defn unseen-from-afar
  "p whose sensedAt reports the cell as unknown (aged out) until the body stands within near blocks of it; with
  read-only?: a gate that never shows."
  [p cell near never?]
  (let [sensed (.-sensedAt p)
        [cx cy cz] cell]
    (aset p "sensedAt"
          (fn [pos]
            (let [self (.-pos (.self p))
                  gate? (and (= cx (.-x pos)) (= cy (.-y pos)) (= cz (.-z pos)))
                  far? (> (js/Math.hypot (- (.-x self) cx) (- (.-z self) cz)) near)]
              (if (and gate? (or never? far?)) #js {:unknown true} (sensed pos)))))
    p))

(defn run-unseen
  "Run :plan pen-a over the open gate at 2,64,0 that reads unknown from afar."
  [never? n]
  (let [s (start (pen-world true 2 -9) {"pen-a" pen-a} nil)]
    (unseen-from-afar (:p s) [2 64 0] 4 never?)
    (core/submit! (:eng s) (list 'jobs.animals.shut-gate {:plan "pen-a"}) {})
    (js/Promise.resolve s)))

(deftest a-planned-gate-aged-out-is-walked-to-looked-at-and-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-unseen false 0))]
          (loop [i 0]
            (when (and (< i 40) (seq (:list (core/state (:eng s)))))
              (swap! clock + 700)
              (await (core/tick! (:eng s)))
              (recur (inc i))))
          (is (not (gate-open? (:p s) 2 64 0)) "the open gate was shut")
          (is (= 1 (:shut (first (events-of s :shut-gate.done))))))))))

(deftest a-planned-gate-that-never-shows-is-left-unseen-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (run-unseen true 0))]
          (loop [i 0]
            (when (and (< i 40) (seq (:list (core/state (:eng s)))))
              (swap! clock + 700)
              (await (core/tick! (:eng s)))
              (recur (inc i))))
          (is (empty? (events-of s :shut-gate.done)) "not done: the gate was never read")
          (is (= [{:cell [2 64 0] :reason :unseen}] (:left (first (events-of s :shut-gate.stopped))))))))))
