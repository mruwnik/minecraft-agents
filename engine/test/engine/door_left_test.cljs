(ns engine.door-left-test
  "The door-left trigger (triggers.maintenance.door-left) and jobs.maintenance.shut-doors: a door or gate a walk opened and
  left open (the walk was cut or cancelled between the open and the shut) is shut again."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [jobs.lib.near :as near]
            [jobs.lib.pass :as pass]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box floor]]
            [engine.triggers :as triggers]
            [triggers.maintenance.door-left :as dl]))

(def start {:x 0 :y 64 :z 0})

(def gate-cell {:x 5 :y 64 :z 0})

(defn gate-world
  "A floor, a fence line across x 5 with a gate at 5,64,0 (open?), the body at x z."
  [open? x z & [more]]
  (merge-with merge
              {:self {:pos {:x x :y 64 :z z}}
               :blocks (merge (floor -2 -8 40 8) (assoc (box 5 64 -6 5 64 6 "oak_fence") "5,64,0" "oak_fence_gate"))
               :states {"5,64,0" {:open open? :facing "east"}}}
              more))

(defn open? [p {:keys [x y z]}] (true? (:open (js->clj (.-properties (.blockAt p #js {:x x :y y :z z})) :keywordize-keys true))))

(defn opened [eng] (mapv :data (mem/entries (mem/view (:store eng)) :opened)))

;; ---------------------------------------------------------------- the trigger

(defn view-with
  "A memory view at now with one :opened entry for the gate written at t (more merged into its data)."
  ([now t] (view-with now t {}))
  ([now t more]
   {:data (mem/add-entry mem/empty-data :opened {:t t :data (merge {:cell gate-cell :by "j1" :t t :shut? true} more)}
                         pass/opened-policy)
    :now now}))

(deftest the-trigger-holds-for-an-old-entry-of-an-open-block-near-a-body-out-of-its-column
  (are [open x z now holds note] (is (= holds (boolean (dl/door-left (tu/seeing-all (tu/fake (gate-world open x z))) (view-with now 0) (:args (:door-left triggers/all)) nil))) note)
    true 9 0 10000 true "10 s open, 4 blocks off"
    true 9 0 9999 false "not yet open-s"
    false 9 0 60000 false "shut again"
    true 5 0 60000 false "the body stands in the gate"
    true 30 0 60000 false "beyond the radius"))

(deftest the-trigger-reads-only-what-the-body-has-seen
  (let [args (:args (:door-left triggers/all))
        holds? #(boolean (dl/door-left % (view-with 60000 0) args nil))
        p (tu/seeing-all (tu/fake (gate-world true 9 0)))]
    (is (true? (holds? p)) "seen open")
    (is (false? (holds? (tu/blind (tu/fake (gate-world true 9 0))))) "never seen: not read through the wall")
    (aset p "seenBlockAt" (fn [pos] #js {:name "oak_fence_gate" :properties #js {:open "false"} :pos pos :age-ms 5000}))
    (is (false? (holds? p)) "last seen shut, opened unseen: the last seen state counts")))

(deftest a-block-a-walk-left-open-on-purpose-is-not-the-triggers-business
  (let [p (tu/seeing-all (tu/fake (gate-world true 9 0)))
        holds? #(dl/door-left p (view-with 60000 0 %) (:args (:door-left triggers/all)) nil)]
    (is (false? (holds? {:shut? false})) "a :leave-open walk's entry")
    (is (true? (holds? {})))))

(deftest the-trigger-is-registered-with-the-job-that-shuts-doors
  (is (= dl/door-left (:when (:door-left triggers/all))))
  (is (= '(jobs.maintenance.shut-doors) (:job (:door-left triggers/all)))))

;; ---------------------------------------------------------------- a walk cut in the gate

(defn walker [args]
  {:check (constantly true)
   :round (fn ^:async walk-round [c]
            (await (apply near/walk-near! c args))
            :done)})

(defn setup
  ([world] (setup world [{:x 10 :y 64 :z 0} 0]))
  ([world walk-args] (setup world walk-args {}))
  ([world walk-args extra-jobs]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake world))
        eng (core/create {:primitives p :jobs (assoc (merge registry/jobs extra-jobs) 'walker (walker walk-args))
                          :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :clock clock :seen seen})))

(defn cut-after-the-open!
  "When the gate has been opened, end the job with stop! (eng) as the walk's steer through it ends: the walker's next act,
  the click that would shut the gate, finds its round cut."
  [{:keys [eng p]} stop!]
  (.override (.-world p) "steer"
             (fn ^:async f [token args impl]
               (let [r (await (impl token args))]
                 (when (and (open? p gate-cell) (seq (opened eng))) (stop! eng))
                 r))))

(defn ^:async tick-until [{:keys [eng clock]} done? n]
  (loop [i 0]
    (when (and (< i n) (not (done?)))
      (swap! clock + 700)
      (await (core/tick! eng))
      (recur (inc i)))))

(deftest a-walk-cut-in-the-gate-leaves-it-open-and-the-reflex-shuts-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label stop!] [["cancelled" #(core/cancel! % "j1")]
                               ["cut by a job done now" #(core/do-now! % '(jobs.debug.notify {:text "x"}))]]]
          (let [{:keys [eng p] :as s} (setup (gate-world false 0 0))]
            (core/register-reflex! eng {:trigger :door-left})
            (cut-after-the-open! s (let [once (atom false)] #(when-not @once (reset! once true) (stop! %))))
            (core/submit! eng '(walker) {})
            (await (tick-until s #(open? p gate-cell) 10))
            (is (open? p gate-cell) (str label ": the cut walk left the gate open"))
            (is (= [gate-cell] (mapv :cell (opened eng))) label)
            (await (tick-until s #(not (open? p gate-cell)) 60))
            (is (not (open? p gate-cell)) (str label ": shut again"))
            (is (= [] (opened eng)) label)))))))

(defn remember-opened!
  "An :opened entry for the gate, as a walk of job j1 writes it before its click."
  [{:keys [eng clock]}]
  (mem/write! (:store eng) :opened {:cell gate-cell :by "j1" :t @clock} pass/opened-policy))

(deftest a-left-gate-out-of-reach-is-walked-back-to-and-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup (gate-world true 14 0))]
          (remember-opened! s)
          (core/submit! eng '(jobs.maintenance.shut-doors) {})
          (await (tick-until s #(empty? (:list (core/state eng))) 20))
          (is (not (open? p gate-cell)))
          (is (seq (tu/walked-to eng)) "it walked back")
          (is (= [] (opened eng)))
          (is (= 1 (:shut (first (filter #(= :shut-doors.done (:kind %)) @seen))))))))))

(deftest a-gate-the-walker-did-not-open-is-left-alone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup (gate-world true 8 0))]
          (core/submit! eng '(jobs.maintenance.shut-doors) {})
          (await (tick-until s #(empty? (:list (core/state eng))) 5))
          (is (open? p gate-cell))
          (is (= 0 (:shut (first (filter #(= :shut-doors.done (:kind %)) @seen))))))))))

(deftest an-unreachable-left-gate-is-given-up-with-one-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup (assoc (gate-world true 14 0) :unreachable ["5,64,0"]))]
          (remember-opened! s)
          (core/submit! eng '(jobs.maintenance.shut-doors) {})
          (await (tick-until s #(empty? (:list (core/state eng))) 30))
          (is (empty? (:list (core/state eng))) "the job ended")
          (is (open? p gate-cell))
          (is (= 1 (count (filter #(= :shut-doors.gave-up (:kind %)) @seen))))
          (is (= [] (opened eng)) "the entry is dropped, so the trigger does not fire for it again"))))))

(deftest a-walk-records-whether-it-will-shut-what-it-opens
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[doors shut?] [[:shut true] [:leave-open false]]]
          (let [{:keys [eng p] :as s} (setup (gate-world false 0 0) [{:x 10 :y 64 :z 0} 0 {:doors doors}])
                seen (atom [])]
            (.override (.-world p) "useOn"
                       (fn ^:async f [token args impl]
                         (swap! seen into (map :shut? (opened eng)))
                         (await (impl token args))))
            (core/submit! eng '(walker) {})
            (await (tick-until s #(empty? (:list (core/state eng))) 10))
            (is (= shut? (first @seen)) (str doors))))))))

(def gate2-cell {:x 5 :y 64 :z 4})

(defn two-gates-world [x z more]
  (gate-world true x z (merge-with merge {:blocks {"5,64,4" "oak_fence_gate"} :states {"5,64,4" {:open true :facing "east"}}} more)))

(defn remember-opened-at! [{:keys [eng clock]} cell]
  (mem/write! (:store eng) :opened {:cell cell :by "j1" :t @clock} pass/opened-policy))

(deftest one-run-shuts-every-left-block
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup (two-gates-world 9 2 {}))]
          (remember-opened-at! s gate-cell)
          (remember-opened-at! s gate2-cell)
          (core/submit! eng '(jobs.maintenance.shut-doors) {})
          (await (tick-until s #(empty? (:list (core/state eng))) 1))
          (is (empty? (:list (core/state eng))) "one tick: the job ended")
          (is (not (open? p gate-cell)))
          (is (not (open? p gate2-cell)))
          (is (= 2 (:shut (first (filter #(= :shut-doors.done (:kind %)) @seen))))))))))

(deftest a-block-given-up-ends-the-run-stopped-with-the-others-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup (two-gates-world 14 2 {:unreachable ["5,64,0"]}))]
          (remember-opened-at! s gate-cell)
          (remember-opened-at! s gate2-cell)
          (core/submit! eng '(jobs.maintenance.shut-doors) {})
          (await (tick-until s #(empty? (:list (core/state eng))) 3))
          (is (empty? (:list (core/state eng))))
          (is (open? p gate-cell))
          (is (not (open? p gate2-cell)))
          (is (empty? (filter #(= :shut-doors.done (:kind %)) @seen)) "partial is not done")
          (let [e (first (filter #(= :shut-doors.stopped (:kind %)) @seen))]
            (is (= 1 (:shut e)))
            (is (= [[5 64 0]] (mapv :cell (:left e))))))))))

(def continuing-go-to
  "A go-to whose every round ends :continue, as one waiting on its own child does."
  {'jobs.movement.go-to {:check (constantly true) :args {} :round (fn ^:async r [_c] :continue)}})

(deftest a-block-the-body-stands-in-stays-left-with-its-entry-for-a-later-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup (gate-world true 5 0))]
          (remember-opened! s)
          (core/submit! eng '(jobs.maintenance.shut-doors) {})
          (await (tick-until s #(empty? (:list (core/state eng))) 3))
          (is (open? p gate-cell))
          (is (= [[[5 64 0] :standing-in]] (mapv (juxt :cell :reason) (:left (first (filter #(= :shut-doors.stopped (:kind %)) @seen))))))
          (is (= [gate-cell] (mapv :cell (opened eng))) "the entry stays, so a later run shuts it"))))))

(deftest a-walk-that-ends-continue-is-not-read-as-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as s} (setup (gate-world true 14 0) [{:x 10 :y 64 :z 0} 0] continuing-go-to)]
          (remember-opened! s)
          (core/submit! eng '(jobs.maintenance.shut-doors) {})
          (await (tick-until s #(empty? (:list (core/state eng))) 3))
          (is (= [:walk-interrupted] (mapv :reason (:left (first (filter #(= :shut-doors.stopped (:kind %)) @seen))))))
          (is (= [gate-cell] (mapv :cell (opened eng))) "the entry stays for a later run")
          (is (empty? (filter #(and (= :warn (:level %)) (= :job (:source %)) (not= :stopped (:kind %))) @seen)) "the job adds no warn of its own for an interrupted walk")
          (is (= [@(:clock s)] (mapv :t (mem/entries (mem/view (:store eng)) :opened)))
              "the entry is stamped afresh, so the trigger waits open-s again"))))))
