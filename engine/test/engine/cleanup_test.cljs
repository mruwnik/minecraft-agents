(ns engine.cleanup-test
  "jobs.access.cleanup: the step decision as a plain function, the standing trigger, then whole cleanups against the
  fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.access.ledger :as ledger]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.triggers.scaffold-left :as scaffold-left]
            [engine.world :as world]
            [jobs.access.cleanup :as cleanup]))

;; ------------------------------------------------------------------ the step

(defn lookup
  "Air everywhere but the given cells (alternating cell and name), stone under y 64."
  [& cells]
  (let [m (apply hash-map cells)]
    (fn [[_ y _ :as pos]] (get m pos (if (< y 64) "stone" "air")))))

(defn entry [cell & {:as more}]
  (merge {:cell cell :item "dirt" :before "air" :job "j99" :purpose :pillar :state :placed} more))

(defn pillar-cells [from n] (mapv #(update from 1 + %) (range n)))

(defn eye [[x y z]] [(+ x 0.5) (+ y 1.62) (+ z 0.5)])

(defn step [& {:as in}]
  (let [entries (:entries in)
        feet (:feet in [3 64 0])]
    (cleanup/next-step (merge {:feet feet :eye (eye feet) :block-at (lookup) :entries entries
                               :ledger (set (map :cell entries)) :zones [] :footprints {} :accept #{:fluid-adjacent}
                               :reach 4.5 :held {}}
                              (dissoc in :feet)))))

(defn column-world [cells] (apply lookup (mapcat (fn [c] [c "dirt"]) cells)))

(deftest on-top-of-its-pillar-it-digs-the-block-under-its-feet
  (let [cells (pillar-cells [0 64 0] 6)]
    (is (= {:step :dig :cell [0 69 0] :item "dirt"}
           (step :feet [0 70 0] :entries (mapv entry cells) :block-at (column-world cells))))))

(deftest cells-under-a-block-that-is-not-its-own-wait
  (let [cells (pillar-cells [0 64 0] 6)
        s (step :feet [0 70 0] :entries (mapv entry (butlast cells)) :block-at (column-world cells))]
    (is (= :finish (:step s)) "the top block is no ledger block: nothing under it is dug")
    (is (= #{:under-body} (set (map :reason (:open s)))))))

(deftest the-block-under-the-feet-is-dug-only-over-a-floor
  (are [below result] (= result ((juxt :step :cell) (step :feet [0 67 0] :entries [(entry [0 66 0])]
                                                          :block-at (lookup [0 66 0] "dirt" [0 65 0] below [0 64 0] "stone"))))
    "stone" [:dig [0 66 0]]
    "air" [:finish nil]
    "water" [:finish nil]
    "magma_block" [:finish nil])
  (is (= [{:cell [0 66 0] :item "dirt" :reason :no-floor-below :block "air"}]
         (:open (step :feet [0 67 0] :entries [(entry [0 66 0])] :block-at (lookup [0 66 0] "dirt" [0 64 0] "stone"))))))

(deftest beside-the-body-the-highest-in-reach-goes-first
  (let [cells [[0 64 0] [0 65 0] [1 64 0]]]
    (is (= [:dig [0 65 0]] ((juxt :step :cell) (step :entries (mapv entry cells) :block-at (column-world cells)))))))

(deftest out-of-reach-it-walks-to-the-nearest
  (let [cells [[12 64 0] [9 64 0]]]
    (is (= {:step :walk :cell [9 64 0]} (step :entries (mapv entry cells) :block-at (column-world cells))))))

(deftest a-cell-held-unreachable-is-not-walked-to-again
  (is (= {:step :finish :open [{:cell [9 64 0] :item "dirt" :reason :unreachable}]}
         (step :entries [(entry [9 64 0])] :block-at (column-world [[9 64 0]]) :held {[9 64 0] {:reason :unreachable}}))))

(deftest refusals-and-unaccepted-hazards-keep-the-entry-open
  (are [in reason] (= reason (:reason (first (:open (apply step (mapcat identity (merge {:entries [(entry [1 64 0])]
                                                                                        :block-at (lookup [1 64 0] "dirt")}
                                                                                       in)))))))
    {:zones [{:name "keep" :min [1 64 0] :max [1 64 0]}]} :zone
    {:footprints {[1 64 0] "farm"}} :footprint
    {:zones nil} :no-zones
    {:block-at (lookup [1 64 0] "dirt" [2 64 0] "lava")} :hazard
    {:block-at (constantly nil)} :not-loaded))

(deftest water-beside-is-accepted-by-default-lava-is-not
  (is (= :dig (:step (step :entries [(entry [1 64 0])] :block-at (lookup [1 64 0] "dirt" [1 65 0] "water")))))
  (is (= [:lava-adjacent] (:hazards (first (:open (step :entries [(entry [1 64 0])]
                                                        :block-at (lookup [1 64 0] "dirt" [1 65 0] "lava"))))))))

(deftest a-zone-that-allows-digging-is-no-refusal
  (is (= :dig (:step (step :entries [(entry [1 64 0])] :block-at (lookup [1 64 0] "dirt")
                           :zones [{:name "work" :min [0 60 0] :max [5 70 5] :allow #{:dig}}])))))

(deftest nothing-left-is-a-finish
  (is (= {:step :finish :open []} (step :entries []))))

;; ------------------------------------------------------------------ the trigger

(defn trigger-store [entries jobs]
  (let [s (mem/open (tu/tmp-dir) {:now (constantly 1000)})]
    (ledger/write! s entries)
    (doseq [id jobs] (mem/write! s (mem/job-kind id) {:args {} :children {}} {:cap 1 :ttl :forever}))
    s))

(defn fires? [p s zones]
  ((:when scaffold-left/trigger) p (mem/view s) {} (world/of-data {} {} zones)))

(deftest the-trigger-offers-cleanup-for-blocks-no-live-job-owns
  (let [p (tu/fake {:blocks {"0,64,0" "dirt"} :unloaded ["50,64,0"]})]
    (is (true? (fires? p (trigger-store [(entry [0 64 0])] []) [])))
    (is (false? (fires? p (trigger-store [(entry [0 64 0] :job "j4/pillar")] ["j4"]) [])) "the owner is still listed")
    (is (false? (fires? p (trigger-store [] []) [])) "nothing open")
    (is (false? (fires? p (trigger-store [(entry [0 64 0])] []) nil)) "no zone list")
    (is (false? (fires? p (trigger-store [(entry [50 64 0])] []) [])) "the cell is not loaded")
    (is (false? (fires? p (doto (trigger-store [(entry [0 64 0])] []) (ledger/hold! [[0 64 0]])) [])) "held")))

(deftest the-trigger-is-a-builtin
  (is (= scaffold-left/trigger (:scaffold-left triggers/all)))
  (is (= '(jobs.access.cleanup) (:job scaffold-left/trigger))))

;; ------------------------------------------------------------------ against the fake world

(def job 'jobs.access.cleanup)

(defn cell-key [[x y z]] (str x "," y "," z))

(defn floor
  "Stone at y 63 over x0..x1, z -2..2."
  [x0 x1]
  (into {} (for [x (range x0 (inc x1)) z (range -2 3)] [(cell-key [x 63 z]) "stone"])))

(defn state-of [p] (.. p -world -state))
(defn block [p cell] (.get (.-blocks (state-of p)) (cell-key cell)))
(defn feet [p] (let [pos (.-pos (.self p))] [(js/Math.floor (.-x pos)) (js/Math.floor (.-y pos)) (js/Math.floor (.-z pos))]))
(defn the-ledger [eng] (ledger/open-entries (mem/view (:store eng))))
(defn of-kind [seen kind] (filterv #(= kind (:kind %)) @seen))

(defn fall!
  "The fake has no gravity: drop the body onto the next block below."
  [p]
  (let [self (.-self (state-of p))
        pos (.-pos self)]
    (loop [y (.-y pos)]
      (if (or (<= y 0) (some? (block p [(js/Math.floor (.-x pos)) (dec y) (js/Math.floor (.-z pos))])))
        (set! (.-pos self) #js {:x (.-x pos) :y y :z (.-z pos)})
        (recur (dec y))))))

(defn record-digs!
  "Every dig records [cell feet-before] in digs and lets the body fall after it."
  [p digs]
  (.override (.-world p) "dig"
             (fn [token args impl]
               (let [pos (.-pos args)]
                 (swap! digs conj [[(.-x pos) (.-y pos) (.-z pos)] (feet p)])
                 (.then (impl token args) (fn [r] (fall! p) r))))))

(defn setup
  "An engine over the fake world with the ledger written into body memory; a recording parent (j1) runs cleanup with
  args as its child and keeps its result in :out. :make starts another engine on the same body."
  [{:keys [self blocks entries zones args] :or {zones []}}]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        dir (tu/tmp-dir)
        p (tu/fake {:self {:pos self} :blocks blocks :inventory []})
        out (atom :not-done)
        w (world/of-data {} {} zones)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job (or args {})))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        make (fn [] (core/create {:primitives p :jobs (assoc registry/jobs 'recording-parent parent)
                                  :triggers triggers/all :dir dir :now #(deref clock) :world w
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}))
        eng (make)
        digs (atom [])]
    (ledger/write! (:store eng) entries)
    (record-digs! p digs)
    ;; the fake's collect puts the body where the drop was spawned, mid-air for a pillar's; keep it where it stood
    (.override (.-world p) "collect"
               (fn [token args impl]
                 (let [pos (.-pos (.-self (state-of p)))]
                   (.then (impl token args) (fn [r] (set! (.-pos (.-self (state-of p))) pos) r)))))
    {:eng eng :make make :p p :clock clock :seen seen :out out :digs digs}))

(defn ^:async ticks [{:keys [clock]} eng n]
  (loop [i 0]
    (when (and (< i n) (seq (:list (core/state eng))))
      (swap! clock + 500)
      (await (core/tick! eng))
      (recur (inc i)))))

(defn pillar-blocks [cells] (into (merge (floor -2 12)) (map (fn [c] [(cell-key c) "dirt"])) cells))

(deftest a-six-high-pillar-is-dug-from-on-top-descending-with-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (pillar-cells [0 64 0] 6)
              {:keys [eng p out digs seen] :as s} (setup {:self {:x 0.5 :y 70 :z 0.5} :blocks (pillar-blocks cells)
                                                          :entries (mapv entry cells)})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 60))
          (is (= (mapv (fn [c] [c (update c 1 inc)]) (reverse cells)) @digs) "top-down, each from on top of it")
          (is (every? nil? (map #(block p %) cells)))
          (is (= [] (the-ledger eng)))
          (is (= (mapv (fn [c] {:cell c :item "dirt"}) (reverse cells)) (:removed @out)))
          (is (= [] (:open @out)))
          (is (= 6 (:collected @out)))
          (is (= 1 (count (of-kind seen :cleanup.done)))))))))

(deftest side-blocks-are-dug-from-the-ground-and-a-far-one-after-a-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[0 64 0] [0 65 0] [10 64 0]]
              {:keys [eng p out digs] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                                     :entries (mapv entry cells)})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 60))
          (is (= [[0 65 0] [0 64 0] [10 64 0]] (mapv first @digs)))
          (is (= [3 64 0] (second (first @digs))) "the first two from where it stood")
          (is (not-any? (fn [[cell f]] (= f (update cell 1 inc))) @digs) "never from on top of the block")
          (is (every? nil? (map #(block p %) cells)))
          (is (= [] (the-ledger eng)))
          (is (= 3 (count (:removed @out)))))))))

(deftest a-swapped-or-vanished-cell-is-never-dug-and-its-entry-dropped-with-a-note
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p out digs seen] :as s}
              (setup {:self {:x 3.5 :y 64 :z 0.5}
                      :blocks (merge (floor -2 5) {"0,64,0" "stone" "1,64,0" "dirt"})
                      :entries [(entry [0 64 0]) (entry [0 65 0]) (entry [1 64 0])]})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 30))
          (is (= [[1 64 0]] (mapv first @digs)))
          (is (= "stone" (block p [0 64 0])))
          (is (= [] (the-ledger eng)))
          (is (= [{:cell [0 64 0] :item "dirt" :found "stone"} {:cell [0 65 0] :item "dirt" :found "air"}] (:dropped @out)))
          (is (= [[0 64 0] [0 65 0]] (mapv :cell (of-kind seen :cleanup.dropped)))))))))

(deftest a-restart-between-the-intent-and-the-dig-digs-the-block-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[0 64 0] [0 65 0]]
              {:keys [eng make p out] :as s} (setup {:self {:x 2.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                                     :entries (mapv entry cells)})]
          (.hold (.-world p) "dig")
          (core/submit! eng '(recording-parent) {})
          (core/tick! eng)
          (await (js/Promise. (fn [ok] (js/setTimeout ok 10))))
          (is (= :removing (:state (ledger/entry-at (the-ledger eng) [0 65 0]))) "the intent was saved before the call")
          (core/shutdown! eng)
          (let [again (make)]
            (await (ticks s again 40))
            (is (every? nil? (map #(block p %) cells)))
            (is (= [] (the-ledger again)))
            (is (= [[0 65 0] [0 64 0]] (mapv :cell (:removed @out))))))))))

(deftest a-restart-after-the-dig-but-before-its-result-counts-it-removed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[0 64 0] [0 65 0]]
              {:keys [eng make p out digs] :as s} (setup {:self {:x 2.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                                          :entries (mapv entry cells)})]
          (.override (.-world p) "dig" (fn [token args impl] (impl token args) (js/Promise. (fn [_ _]))))
          (core/submit! eng '(recording-parent) {})
          (core/tick! eng)
          (await (js/Promise. (fn [ok] (js/setTimeout ok 10))))
          (core/shutdown! eng)
          (is (nil? (block p [0 65 0])))
          (is (= :removing (:state (ledger/entry-at (the-ledger eng) [0 65 0]))))
          (record-digs! p digs)
          (let [again (make)]
            (await (ticks s again 40))
            (is (= [[0 64 0]] (mapv first @digs)) "the dug block is not dug again")
            (is (= [] (the-ledger again)))
            (is (= #{[0 64 0] [0 65 0]} (set (map :cell (:removed @out)))))))))))

(deftest a-zone-refusal-keeps-the-entry-and-reports-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[0 64 0] [0 65 0]]
              {:keys [eng p out digs seen] :as s} (setup {:self {:x 2.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                                          :entries (mapv entry cells)
                                                          :zones [{:name "keep" :min [0 65 0] :max [0 65 0]}]})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 30))
          (is (= [[0 64 0]] (mapv first @digs)))
          (is (= "dirt" (block p [0 65 0])))
          (is (= [[0 65 0]] (mapv :cell (the-ledger eng))))
          (is (= [{:cell [0 65 0] :item "dirt" :reason :zone :zone "keep"}] (:open @out)))
          (is (= 1 (count (of-kind seen :cleanup.left))))
          (is (= #{[0 65 0]} (ledger/held-cells (mem/view (:store eng))))))))))

(deftest only-the-named-instance-and-its-children
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells [[0 64 0] [1 64 0]]
              {:keys [eng p out] :as s} (setup {:self {:x 3.5 :y 64 :z 0.5} :blocks (pillar-blocks cells)
                                                :entries [(entry [0 64 0] :job "j1/pillar") (entry [1 64 0] :job "j77")]
                                                :args {:job "j1"}})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 30))
          (is (nil? (block p [0 64 0])))
          (is (= "dirt" (block p [1 64 0])))
          (is (= [[1 64 0]] (mapv :cell (the-ledger eng))))
          (is (= [[0 64 0]] (mapv :cell (:removed @out)))))))))

(deftest a-column-over-a-hole-stops-where-the-floor-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [cells (pillar-cells [0 65 0] 3)
              {:keys [eng p out digs] :as s} (setup {:self {:x 0.5 :y 68 :z 0.5}
                                                     :blocks (dissoc (pillar-blocks cells) "0,63,0")
                                                     :entries (mapv entry cells)})]
          (core/submit! eng '(recording-parent) {})
          (await (ticks s eng 30))
          (is (= [[[0 67 0] [0 68 0]] [[0 66 0] [0 67 0]]] @digs))
          (is (= "dirt" (block p [0 65 0])))
          (is (= [{:cell [0 65 0] :item "dirt" :reason :no-floor-below :block "air"}] (:open @out))))))))

(deftest nothing-open-declines-without-a-round
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:self {:x 0.5 :y 64 :z 0.5} :blocks (floor -2 2) :entries []})]
          (core/submit! eng (list job) {})
          (await (ticks s eng 20))
          (is (= 1 (count (:list (core/state eng)))) "still listed, never run")
          (is (= 0 (.-length (.-calls (.-world p)))))
          (is (empty? (of-kind seen :cleanup.done))))))))

(deftest held-cells-are-not-offered-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:self {:x 2.5 :y 64 :z 0.5} :blocks (pillar-blocks [[0 64 0]])
                                            :entries [(entry [0 64 0])]
                                            :zones [{:name "keep" :min [0 64 0] :max [0 64 0]}]})]
          (core/submit! eng (list job) {})
          (await (ticks s eng 10))
          (is (empty? (:list (core/state eng))))
          (core/submit! eng (list job) {})
          (let [n (.-length (.-calls (.-world p)))]
            (await (ticks s eng 10))
            (is (= 1 (count (:list (core/state eng)))) "the second declines")
            (is (= n (.-length (.-calls (.-world p)))))))))))

(deftest no-zone-list-declines-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:self {:x 2.5 :y 64 :z 0.5} :blocks (pillar-blocks [[0 64 0]])
                                                 :entries [(entry [0 64 0])] :zones nil})]
          (core/submit! eng (list job) {})
          (await (ticks s eng 10))
          (is (= "dirt" (block p [0 64 0])))
          (is (= 1 (count (of-kind seen :cleanup.declined)))))))))
