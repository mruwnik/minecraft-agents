(ns engine.pillar-test
  "jobs.access.pillar: the step decision as a plain function, then whole pillars against the fake world."
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
            [jobs.access.pillar :as pillar]))

;; ------------------------------------------------------------------ the step

(def base [0 64 0])

(defn world
  "Air everywhere but the given cells (alternating cell and name), with stone under the base."
  [& cells]
  (let [m (merge {[0 63 0] "stone"} (apply hash-map cells))]
    (fn [pos] (get m pos "air"))))

(defn step [& {:as in}]
  (pillar/next-step (merge {:feet base :base base :height 6 :block-at (world) :carried {"dirt" 64} :item nil
                            :zones [] :footprints #{} :ledger #{}}
                           in)))

(deftest the-first-block-goes-under-the-feet
  (is (= {:step :place :cell base :item "dirt"} (step))))

(deftest done-at-the-height
  (is (= {:step :done} (step :feet [0 70 0])))
  (is (= {:step :done} (step :feet [0 66 0] :height 2))))

(deftest the-item-without-an-item-arg-is-dirt-then-cobblestone
  (are [carried item] (= item (:item (step :carried carried :block-at (world))))
    {"dirt" 1 "cobblestone" 5} "dirt"
    {"cobblestone" 5} "cobblestone"
    {"oak_planks" 5 "cobblestone" 1} "cobblestone")
  (is (= "oak_planks" (:item (step :item "oak_planks" :carried {"oak_planks" 1 "dirt" 9})))))

(deftest gives-up-with-a-reason
  (are [in reason] (= reason (:reason (apply step (mapcat identity in))))
    {:carried {}} :too-few-blocks
    {:carried {"oak_planks" 9}} :too-few-blocks
    {:item "stone" :carried {"dirt" 9}} :too-few-blocks
    {:block-at (world [0 63 0] "air")} :not-on-solid
    {:block-at (world [0 63 0] "water")} :not-on-solid
    {:block-at (world [0 66 0] "stone")} :ceiling
    {:block-at (world [0 65 0] "oak_leaves")} :ceiling
    {:zones nil} :no-zones
    {:zones [{:name "farm" :min [0 60 0] :max [0 64 0]}]} :zone
    {:footprints #{[0 64 0]}} :footprint
    {:feet [1 65 0]} :off-column
    {:feet [0 63 0]} :off-column
    {:height 0} :bad-args
    {:height 200} :bad-args))

(deftest a-zone-anywhere-in-the-rest-of-the-column-refuses-before-the-first-block
  (let [s (step :zones [{:name "keep-out" :min [-2 67 -2] :max [2 67 2]}])]
    (is (= :give-up (:step s)))
    (is (= :zone (:reason s)))
    (is (= "keep-out" (:zone s)))
    (is (= [0 67 0] (:at s)))))

(deftest a-zone-above-the-pillar-top-does-not-matter
  (is (= :place (:step (step :height 3 :zones [{:name "sky" :min [0 67 0] :max [0 90 0]}])))))

(deftest a-zone-that-allows-placing-is-no-refusal
  (is (= :place (:step (step :zones [{:name "work" :min [0 60 0] :max [0 70 0] :allow #{:place}}])))))

(deftest the-ceiling-reason-names-the-cell
  (is (= {:step :give-up :reason :ceiling :at [0 66 0] :block "stone"} (step :block-at (world [0 66 0] "stone")))))

(deftest higher-up-the-pillar-the-checks-move-with-the-feet
  (is (= {:step :place :cell [0 66 0] :item "dirt"}
         (step :feet [0 66 0] :block-at (world [0 65 0] "dirt" [0 64 0] "dirt")))))

;; ------------------------------------------------------------------ against the fake world

(def job 'jobs.access.pillar)

(defn setup
  "An engine over a fake world with a stone floor under the base; :make starts another engine on the same body."
  [{:keys [inventory blocks zones] :or {zones []}}]
  (let [clock (atom 1000000)
        w (world/of-data {} {} zones)
        dir (tu/tmp-dir)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake {:self {:pos {:x 0 :y 64 :z 0}}
                    :blocks (merge {"0,63,0" "stone"} blocks)
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
(defn blocks-at [p cells] (mapv #(block p %) cells))
(defn feet [p] (let [pos (.-pos (.self p))] [(.-x pos) (.-y pos) (.-z pos)]))
(defn the-ledger [eng] (ledger/open-entries (mem/view (:store eng))))
(defn of-kind [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn finished? [eng] (empty? (:list (core/state eng))))
(defn column [from n] (mapv #(update from 1 + %) (range n)))

(deftest builds-the-full-height-one-block-a-round-each-recorded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {})]
          (core/submit! eng (list job {:height 6}) {})
          (await (ticks s eng 10))
          (is (finished? eng))
          (is (= [0 70 0] (feet p)))
          (is (= (repeat 6 "dirt") (blocks-at p (column base 6))))
          (is (= (repeat 6 1) (mapv #(.. % -args -count) (calls p "jumpPlace"))) "one block per call")
          (is (= (mapv (fn [c] [c :placed "dirt" "air" :pillar]) (column base 6))
                 (mapv (juxt :cell :state :item :before :purpose) (the-ledger eng))))
          (is (= 58 (.-count (first (.-inventory (.self p))))))
          (let [d (first (of-kind seen :pillar.done))]
            (is (= (column base 6) (:cells d)))
            (is (= 6 (:height d)))))))))

(deftest a-ceiling-stops-the-pillar-below-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:blocks {"0,68,0" "stone"}})]
          (core/submit! eng (list job {:height 6}) {})
          (await (ticks s eng 8))
          (is (finished? eng))
          (is (= [0 66 0] (feet p)) "two blocks: a third would put the head in the ceiling")
          (is (= 2 (count (the-ledger eng))))
          (let [g (first (of-kind seen :pillar.gave-up))]
            (is (= :ceiling (:reason g)))
            (is (= [0 68 0] (:at g)))
            (is (= (column base 2) (:cells g)))))))))

(deftest too-few-blocks-builds-what-it-has-and-then-waits-saying-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {:inventory [{:name "dirt" :count 2}]})
              id (core/submit! eng (list job {:height 6}) {})]
          (await (ticks s eng 8))
          (is (not (finished? eng)) "still listed: it waits for blocks")
          (is (= [0 66 0] (feet p)))
          (is (= 2 (count (the-ledger eng))))
          (is (= {:reason :too-few-blocks :short 4} (select-keys (core/waiting eng id) [:reason :short]))))))))

(deftest a-zone-refuses-before-any-placement
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:zones [{:name "keep-out" :min [-3 66 -3] :max [3 66 3]}]})]
          (core/submit! eng (list job {:height 6}) {})
          (await (ticks s eng 4))
          (is (finished? eng))
          (is (empty? (calls p "jumpPlace")))
          (is (empty? (the-ledger eng)))
          (is (= [:zone "keep-out"] ((juxt :reason :zone) (first (of-kind seen :pillar.gave-up))))))))))

(deftest no-zone-list-refuses-before-any-placement
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:zones nil})]
          (core/submit! eng (list job {:height 6}) {})
          (await (ticks s eng 4))
          (is (finished? eng))
          (is (empty? (calls p "jumpPlace")))
          (is (= :no-zones (:reason (first (of-kind seen :pillar.gave-up))))))))))

(deftest a-cut-between-intent-and-placement-is-resumed-without-a-double-entry
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng make p seen] :as s} (setup {})]
          (core/submit! eng (list job {:height 6}) {})
          (await (ticks s eng 2))
          (.hold (.-world p) "jumpPlace")
          (core/tick! eng)
          (await (js/Promise. (fn [ok] (js/setTimeout ok 10))))
          (is (= :intent (:state (ledger/entry-at (the-ledger eng) [0 66 0]))) "the intent was saved before the call")
          (core/shutdown! eng)
          (let [again (make)]
            (await (ticks s again 8))
            (is (finished? again))
            (is (= [0 70 0] (feet p)))
            (is (= (mapv (fn [c] [c :placed]) (column base 6)) (mapv (juxt :cell :state) (the-ledger again))))
            (is (= (column base 6) (:cells (first (of-kind seen :pillar.done)))))))))))

(deftest a-restart-after-the-block-went-in-but-before-it-was-confirmed-counts-it-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng make p seen] :as s} (setup {})]
          (core/submit! eng (list job {:height 6}) {})
          (await (ticks s eng 2))
          (.override (.-world p) "jumpPlace"
                     (fn [token args impl]
                       (impl token args)
                       (js/Promise. (fn [_ _]))))
          (core/tick! eng)
          (await (js/Promise. (fn [ok] (js/setTimeout ok 10))))
          (core/shutdown! eng)
          (.override (.-world p) "jumpPlace" nil)
          (is (= "dirt" (block p [0 66 0])))
          (is (= :intent (:state (ledger/entry-at (the-ledger eng) [0 66 0]))))
          (let [again (make)]
            (await (ticks s again 8))
            (is (finished? again))
            (is (= [0 70 0] (feet p)))
            (is (= 6 (count (calls p "jumpPlace"))) "three before the restart, three after")
            (is (= (mapv (fn [c] [c :placed]) (column base 6)) (mapv (juxt :cell :state) (the-ledger again))))
            (is (= 58 (.-count (first (.-inventory (.self p))))))
            (is (= (column base 6) (:cells (first (of-kind seen :pillar.done)))))))))))

(deftest three-failed-placements-in-a-row-give-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {})]
          (.override (.-world p) "jumpPlace"
                     (fn [_token _args _impl] (js/Promise.resolve #js {:status "failed" :placed 0 :reason "not-raised"})))
          (core/submit! eng (list job {:height 6}) {})
          (await (ticks s eng 6))
          (is (finished? eng))
          (is (= 3 (count (calls p "jumpPlace"))))
          (is (empty? (the-ledger eng)) "the intents of blocks that never went in are dropped")
          (let [g (first (of-kind seen :pillar.gave-up))]
            (is (= :place-failed (:reason g)))
            (is (= "not-raised" (:detail g)))))))))

(deftest a-job-that-sees-its-own-earlier-blocks-does-not-place-them-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (setup {})]
          (core/submit! eng (list job {:height 2}) {})
          (await (ticks s eng 5))
          (core/submit! eng (list job {:height 1}) {})
          (await (ticks s eng 5))
          (is (finished? eng))
          (is (= [0 67 0] (feet p)))
          (is (= 3 (count (the-ledger eng))) "a second pillar job on top adds its own entry"))))))

;; ------------------------------------------------------------------ knockback

(defn knock!
  "Override jumpPlace so that its first n calls fail with the body shoved to pos (and airborne when air?), as a hit
  mid-jump does; later calls are the real thing."
  [p n pos air?]
  (let [left (atom n)]
    (.override (.-world p) "jumpPlace"
               (fn [_token _args impl]
                 (if (pos? @left)
                   (do (swap! left dec)
                       (fake/swap-self! p assoc :pos pos :onGround (not air?))
                       (js/Promise.resolve #js {:status "failed" :placed 0
                                                :reason "place-failed: Server refused to place cobblestone: block is still air"}))
                   (impl _token _args))))))

(deftest knocked-off-the-column-it-walks-back-and-finishes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:blocks {"1,63,0" "stone"}})]
          (knock! p 4 [1 64 0] false)
          (core/submit! eng (list job {:height 3}) {})
          (await (ticks s eng 30))
          (is (finished? eng))
          (is (empty? (of-kind seen :pillar.gave-up)))
          (is (= [0 67 0] (feet p)))
          (is (= (repeat 3 "dirt") (blocks-at p (column base 3)))))))))

(deftest knocked-into-the-air-it-waits-to-land-before-judging-the-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {})]
          (knock! p 1 [0 65 0] true)
          (.override (.-world p) "wait"
                     (fn [_token _args _impl]
                       (fake/swap-self! p assoc :pos [0 64 0] :onGround true)
                       (js/Promise.resolve #js {:status "ok"})))
          (core/submit! eng (list job {:height 2}) {})
          (await (ticks s eng 12))
          (is (finished? eng))
          (is (empty? (of-kind seen :pillar.gave-up)))
          (is (= [0 66 0] (feet p))))))))

(deftest shoved-off-every-time-it-ends-failed-with-a-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup {:blocks {"1,63,0" "stone"}})]
          (knock! p 1000 [1 64 0] false)
          (core/submit! eng (list job {:height 3}) {})
          (await (ticks s eng 60))
          (is (finished? eng))
          (is (= :off-column (:reason (first (of-kind seen :pillar.gave-up))))))))))
