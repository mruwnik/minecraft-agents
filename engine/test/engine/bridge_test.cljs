(ns engine.bridge-test
  "jobs.access.bridge: the step decision as a plain function, then whole rows against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [jobs.lib.ledger :as ledger]
            [engine.core :as core]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.world-files :as world]
            [jobs.access.bridge :as bridge]))

;; ------------------------------------------------------------------ the step

(def start [0 64 0])

(defn world
  "Air everywhere but the given cells (alternating cell and name), with stone under the start."
  [& cells]
  (let [m (merge {[0 63 0] "stone"} (apply hash-map cells))]
    (fn [pos] (get m pos "air"))))

(defn step [& {:as in}]
  (bridge/next-step (merge {:feet start :start start :heading :east :length 5 :block-at (world)
                            :carried {"dirt" 64} :item nil :zones [] :footprints #{} :ledger #{}}
                           in)))

(deftest the-first-block-goes-ahead-and-down-from-the-feet
  (is (= {:step :place :cell [1 63 0] :item "dirt"} (step)))
  (is (= {:step :place :cell [0 63 -1] :item "dirt"} (step :heading :north))))

(deftest a-solid-cell-ahead-is-stepped-onto-not-placed
  (is (= {:step :move :to [1 64 0]} (step :block-at (world [1 63 0] "dirt")))))

(deftest done-at-the-length
  (is (= {:step :done} (step :feet [5 64 0])))
  (is (= {:step :done} (step :feet [2 64 0] :length 2))))

(deftest the-next-block-follows-the-feet
  (is (= {:step :place :cell [3 63 0] :item "dirt"}
         (step :feet [2 64 0] :block-at (world [1 63 0] "dirt" [2 63 0] "dirt")))))

(deftest the-item-without-an-item-arg-is-dirt-then-cobblestone
  (are [carried item] (= item (:item (step :carried carried)))
    {"dirt" 1 "cobblestone" 5} "dirt"
    {"cobblestone" 5} "cobblestone")
  (is (= "oak_planks" (:item (step :item "oak_planks" :carried {"oak_planks" 1 "dirt" 9})))))

(deftest gives-up-with-a-reason
  (are [in reason] (= reason (:reason (apply step (mapcat identity in))))
    {:carried {}} :too-few-blocks
    {:item "stone" :carried {"dirt" 9}} :too-few-blocks
    {:block-at (world [0 63 0] "air")} :not-on-solid
    {:block-at (world [0 63 0] "water")} :not-on-solid
    {:block-at (world [1 65 0] "stone")} :blocked
    {:block-at (world [1 64 0] "stone")} :blocked
    {:block-at (world [1 63 0] nil)} :not-loaded
    {:zones nil} :no-zones
    {:zones [{:name "farm" :min [3 60 0] :max [3 70 0]}]} :zone
    {:footprints #{[4 63 0]}} :footprint
    {:feet [0 65 0]} :off-line
    {:feet [0 64 1]} :off-line
    {:feet [-1 64 0]} :off-line
    {:length 0} :bad-args
    {:length 200} :bad-args
    {:heading :up} :bad-args))

(deftest a-zone-anywhere-along-the-row-refuses-before-the-first-block
  (let [s (step :zones [{:name "keep-out" :min [4 60 -2] :max [4 70 2]}])]
    (is (= [:give-up :zone "keep-out" [4 63 0]] ((juxt :step :reason :zone :at) s)))))

(deftest a-zone-beyond-the-row-does-not-matter
  (is (= :place (:step (step :length 3 :zones [{:name "far" :min [4 60 -2] :max [9 70 2]}])))))

(deftest a-zone-that-allows-placing-is-no-refusal
  (is (= :place (:step (step :zones [{:name "work" :min [0 60 -2] :max [9 70 2] :allow #{:place}}])))))

;; ------------------------------------------------------------------ against the fake world

(def job 'jobs.access.bridge)

(def landing "Stone floor from x=6 on: a 5-wide gap at x 1..5." (into {} (for [x (range 6 9)] [(str x ",63,0") "stone"])))

(defn setup
  "An engine over a fake world: stone under the start and from x=6 on, a gap between; :make starts another engine on the
  same body."
  [{:keys [inventory blocks zones] :or {zones []}}]
  (let [clock (atom 1000000)
        w (world/of-data {} {} zones)
        dir (tu/tmp-dir)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake {:self {:pos {:x 0.5 :y 64 :z 0.5}}
                    :blocks (merge {"0,63,0" "stone"} landing blocks)
                    :inventory (or inventory [{:name "dirt" :count 64}])})
        make (fn [] (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir dir :now #(deref clock) :world w
                                  :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})}))]
    {:eng (make) :make make :p p :seen seen :clock clock}))

(defn ^:async ticks [{:keys [clock]} eng n]
  (loop [i 0]
    (when (< i n)
      (swap! clock + 500)
      (await (core/tick! eng))
      (recur (inc i)))))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn block [p cell] (get (:blocks @(fake/state p)) (vec cell)))
(defn feet [p] (let [pos (.-pos (.self p))] [(js/Math.floor (.-x pos)) (.-y pos) (js/Math.floor (.-z pos))]))
(defn the-ledger [eng] (ledger/open-entries (mem/view (:store eng))))
(defn of-kind [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn finished? [eng] (empty? (:list (core/state eng))))
(defn row [n] (mapv (fn [i] [(inc i) 63 0]) (range n)))

(deftest bridges-the-row-recording-every-block-before-it-is-placed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {})]
          (core/submit! eng (list job {:heading :east :length 5}) {})
          (await (ticks s eng 30))
          (is (finished? eng))
          (is (= [5 64 0] (feet p)))
          (is (= (repeat 5 "dirt") (mapv #(block p %) (row 5))))
          (is (= (mapv (fn [c] [c :placed "dirt" "air" :bridge]) (row 5))
                 (mapv (juxt :cell :state :item :before :purpose) (the-ledger eng))))
          (is (= 59 (.-count (first (.-inventory (.self p))))))
          (let [d (first (of-kind seen :bridge.done))]
            (is (= (row 5) (:placed d)))))))))

(deftest no-blocks-fetches-them-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:inventory []})]
          (core/submit! eng (list job {:heading :east :length 5}) {})
          (await (ticks s eng 4))
          (is (empty? (calls p "place")))
          (let [f (first (of-kind seen :fetch.started))]
            (is (= :need (:for f)))
            (is (= ["dirt" "cobblestone"] (:any-of (:args f))))
            (is (= 5 (:count (:args f))))))))))

(deftest running-short-part-way-fetches-the-rest-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as s} (setup {:inventory [{:name "dirt" :count 2}]})]
          (core/submit! eng (list job {:heading :east :length 5}) {})
          (await (ticks s eng 12))
          (is (= 3 (:count (:args (first (of-kind seen :fetch.started)))))))))))

(deftest too-few-blocks-at-the-start-waits-saying-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:inventory []})
              id (core/submit! eng (list job {:heading :east :length 5 :fetch false}) {})]
          (await (ticks s eng 3))
          (is (not (finished? eng)))
          (is (empty? (calls p "place")))
          (is (empty? (of-kind seen :fetch.started)))
          (is (= :too-few-blocks (:reason (core/waiting eng id)))))))))

(deftest running-short-part-way-builds-what-it-has-and-gives-up-saying-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:inventory [{:name "dirt" :count 2}]})]
          (core/submit! eng (list job {:heading :east :length 5 :fetch false}) {})
          (await (ticks s eng 30))
          (is (finished? eng))
          (is (= [2 64 0] (feet p)))
          (is (= 2 (count (the-ledger eng))))
          (let [g (first (of-kind seen :bridge.gave-up))]
            (is (= :too-few-blocks (:reason g)))
            (is (= (row 2) (:placed g)))))))))

(deftest a-zone-refuses-before-any-placement
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:zones [{:name "keep-out" :min [4 60 -2] :max [4 70 2]}]})]
          (core/submit! eng (list job {:heading :east :length 5}) {})
          (await (ticks s eng 5))
          (is (finished? eng))
          (is (empty? (calls p "place")))
          (is (empty? (the-ledger eng)))
          (is (= [:zone "keep-out"] ((juxt :reason :zone) (first (of-kind seen :bridge.gave-up))))))))))

(deftest a-ceiling-ahead-refuses-before-any-placement
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:blocks {"1,65,0" "stone"}})]
          (core/submit! eng (list job {:heading :east :length 5}) {})
          (await (ticks s eng 5))
          (is (finished? eng))
          (is (empty? (calls p "place")))
          (is (= :blocked (:reason (first (of-kind seen :bridge.gave-up))))))))))

(defn stall-at!
  "Override place so that its nth call never resolves (after placing the block when place?); the calls before are the
  real thing. An atom of the calls made."
  [p n place?]
  (let [k (atom 0)]
    (.override (.-world p) "place"
               (fn [token args impl]
                 (if (< (swap! k inc) n)
                   (impl token args)
                   (do (when place? (impl token args))
                       (js/Promise. (fn [_ _]))))))
    k))

(defn ^:async until-calls [k n]
  (loop [i 0]
    (when (and (< i 200) (< @k n))
      (await (js/Promise. (fn [ok] (js/setTimeout ok 10))))
      (recur (inc i)))))

(deftest a-cut-between-intent-and-placement-is-resumed-without-a-double-entry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng make p seen] :as s} (setup {})]
          (core/submit! eng (list job {:heading :east :length 5}) {})
          (let [k (stall-at! p 3 false)]
            (core/tick! eng)
            (await (until-calls k 3)))
          (is (= :intent (:state (ledger/entry-at (the-ledger eng) [3 63 0]))) "the intent was saved before the call")
          (core/shutdown! eng)
          (.override (.-world p) "place" nil)
          (let [again (make)]
            (await (ticks s again 30))
            (is (finished? again))
            (is (= [5 64 0] (feet p)))
            (is (= (mapv (fn [c] [c :placed]) (row 5)) (mapv (juxt :cell :state) (sort-by first (the-ledger again)))))
            (is (= (row 5) (:placed (first (of-kind seen :bridge.done)))))))))))

(deftest a-restart-after-the-block-went-in-but-before-it-was-confirmed-counts-it-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng make p seen] :as s} (setup {})]
          (core/submit! eng (list job {:heading :east :length 5}) {})
          (let [k (stall-at! p 3 true)]
            (core/tick! eng)
            (await (until-calls k 3)))
          (core/shutdown! eng)
          (.override (.-world p) "place" nil)
          (is (= "dirt" (block p [3 63 0])))
          (let [again (make)]
            (await (ticks s again 30))
            (is (finished? again))
            (is (= [5 64 0] (feet p)))
            (is (= 5 (count (the-ledger again))) "one entry per cell")
            (is (= 5 (count (calls p "place"))) "three before the restart (one stalled), two after")))))))

(deftest a-shove-off-the-line-walks-back-and-goes-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng make p] :as s} (setup {:blocks {"2,63,1" "stone"}})]
          (core/submit! eng (list job {:heading :east :length 5}) {})
          (let [k (stall-at! p 3 false)]
            (core/tick! eng)
            (await (until-calls k 3)))
          (core/shutdown! eng)
          (.override (.-world p) "place" nil)
          (swap! (fake/state p) assoc-in [:self :pos] [2.5 64 1.5])
          (let [again (make)]
            (await (ticks s again 30))
            (is (finished? again))
            (is (= [5 64 0] (feet p)))
            (is (= (repeat 5 "dirt") (mapv #(block p %) (row 5))))))))))

(deftest a-fall-off-the-row-gives-up-off-line
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng make p seen] :as s} (setup {})]
          (core/submit! eng (list job {:heading :east :length 5}) {})
          (let [k (stall-at! p 3 false)]
            (core/tick! eng)
            (await (until-calls k 3)))
          (core/shutdown! eng)
          (.override (.-world p) "place" nil)
          (swap! (fake/state p) #(-> % (assoc-in [:self :pos] [2.5 60 0.5]) (update :blocks dissoc [2 63 0])))
          (let [again (make)]
            (await (ticks s again 10))
            (is (finished? again))
            (is (= :off-line (:reason (first (of-kind seen :bridge.gave-up)))))))))))
