(ns engine.tidy-test
  "Tidying up after trespassing: a dig or place in another's zone is recorded in body memory, and
  jobs.survival.restore-broken puts the cells back when the body is safe."
  (:require [cljs.test :refer [deftest is async use-fixtures]]
            [engine.zones-survival-test :as zs]
            [engine.fake :as fake]
            [engine.blocks-dig-test :as bd]
            [engine.core :as core]
            [engine.events :as events]
            [engine.ctx :as ctx]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.tidy :as tidy]
            [jobs.survival.restore-broken :as restore-broken]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [triggers.survival.tidy-pending :as tidy-pending]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]))

(defn ^:async ticks! [eng n]
  (dotimes [_ n] (await (core/tick! eng))))

(defn setup
  "zs/setup with the fake's hitbox mode: the body stands at the middle of cells and the server refuses a block
  placed into its 0.6 x 1.8 hitbox. A block beside the cell at 0,65,0 gives place something to place against."
  [world zones]
  (zs/setup (-> world (assoc :bodyHitbox true) (update :blocks #(merge {"-1,65,0" "stone"} %))) zones))

(defn ended-runs
  "The :completed or :stopped events of the tidy runs that carry key k."
  [seen k]
  (filterv #(and (#{:completed :stopped} (:kind %)) (seq (get % k))) @seen))
(defn restored-runs [seen] (ended-runs seen :cells))
(defn not-restored-runs [seen] (mapv #(assoc % :cells (:not-restored %)) (ended-runs seen :not-restored)))

(defn tidy-entries [eng] (mapv :data (mem/entries (mem/view (:store eng)) :tidy)))

(defn seed! [eng entries]
  (doseq [e entries] (mem/write! (:store eng) :tidy e tidy/tidy-policy)))

(def dug {:cell [0 65 0] :action :dig :was "stone" :now "air" :zone "vault" :tries 0})
(def placed {:cell [0 65 0] :action :place :was "air" :now "cobblestone" :zone "vault" :tries 0})
(def stone (:inventory {:inventory [{:name "stone" :count 2}]}))
(def zombie {:id 7 :name "zombie" :kind "hostile" :pos {:x 3 :y 64 :z 0}})

(def floor (into {} (for [x (range -5 6) z (range -5 6)] [(str x ",63," z) "stone"])))

(def enclosed {"0,65,0" "stone" "0,66,0" "stone"})

(def enclosed-body
  "The body at 0,64,0 shut in by stone on every side, as a breathe trespass finds it; a floor at y 63 is under it only."
  {:blocks (merge {"1,66,0" "stone" "0,66,0" "stone" "0,63,0" "stone" "1,64,0" "stone" "-1,64,0" "stone" "0,64,1" "stone" "0,64,-1" "stone"
                            "0,65,0" "stone" "1,65,0" "stone" "-1,65,0" "stone" "0,65,1" "stone" "0,65,-1" "stone"})})

(deftest breathe-records-what-it-dug-in-a-foreign-zone-only
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[zones recorded] [[[(zs/whole-zone "Miles")] [{:cell [0 65 0] :action :dig :was "stone" :now "air" :zone "vault" :tries 0 :job "j1"}
                                                              {:cell [0 66 0] :action :dig :was "stone" :now "air" :zone "vault" :tries 0 :job "j1"}]]
                                  [[(zs/whole-zone "fake")] []]
                                  [[] []]
                                  [nil []]]]
          (let [{:keys [eng]} (setup {:blocks enclosed} zones)]
            (core/submit! eng '(jobs.survival.breathe) {})
            (await (core/tick! eng))
            (is (= recorded (tidy-entries eng)) (pr-str zones))))))))

(defn restore!
  "Run restore-broken on a world with seeded entries; [eng p seen] after the job ran."
  [world zones entries]
  (let [{:keys [eng] :as s} (setup world zones)]
    (seed! eng entries)
    (core/submit! eng '(jobs.survival.restore-broken) {})
    s))

(def aside {:self {:pos [2 64 0]}})

(deftest restore-places-the-dug-block-when-carried-and-safe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (restore! (merge aside {:inventory [{:name "stone" :count 2}]}) [(zs/whole-zone "Miles")] [dug])]
          (await (zs/run-until-empty eng 6))
          (is (= [{:x 0 :y 65 :z 0}] (mapv zs/arg-pos (zs/calls p "place"))))
          (is (= [] (zs/calls p "dig")) "digs nothing")
          (is (some #(and (= :child_started (:kind %)) (= :put (:slot %))) @seen) "the block goes back through a child job")
          (is (= [] (tidy-entries eng)))
          (is (= [[[0 65 0]]] (mapv :cells (restored-runs seen))))
          (is (= [] (not-restored-runs seen)) "nothing left, no warn"))))))

(deftest restore-ends-done-with-the-count-or-at-once-with-nothing-pending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (merge aside {:inventory [{:name "stone" :count 2}]}) [(zs/whole-zone "Miles")])]
          (seed! eng [dug])
          (let [r (await (tu/child-outcome eng 'jobs.survival.restore-broken {} 20))]
            (is (= {:status :done :restored 1 :cells [[0 65 0]]} r))))
        (let [{:keys [eng p seen]} (restore! aside [(zs/whole-zone "Miles")] [])]
          (await (zs/run-until-empty eng 6))
          (is (= [] (:list (core/state eng))) "nothing pending: done at once")
          (is (= [] (restored-runs seen)) "no report")
          (is (= [] (zs/calls p "place"))))))))

(deftest a-crop-dug-in-another-zone-is-put-back-as-its-seed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (zs/setup (merge aside {:blocks {"0,65,0" "potatoes" "0,64,0" "farmland"} :inventory []}) [(zs/whole-zone "Miles")])]
          (core/submit! eng '(jobs.blocks.dig {:pos {:x 0 :y 65 :z 0} :ignore-zones? true}) {})
          (await (zs/run-until-empty eng 6))
          (is (= [["potato"]] (mapv :any-of (tidy-entries eng))) "the entry names the seed that replants the crop")
          (fake/add-item! p "potato" 2)
          (core/register-reflex! eng {:trigger :tidy-pending})
          (await (ticks! eng 20))
          (is (= [{:x 0 :y 65 :z 0}] (mapv zs/arg-pos (zs/calls p "place"))) "the potato is sown in the harvested cell")
          (is (= [] (tidy-entries eng))))))))

(deftest restore-digs-a-placed-block-again
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (restore! {:blocks {"0,65,0" "cobblestone"}} [(zs/whole-zone "Miles")] [placed])]
          (await (zs/run-until-empty eng 6))
          (is (= [{:x 0 :y 65 :z 0}] (mapv zs/arg-pos (zs/calls p "dig"))))
          (is (= [] (zs/calls p "place")))
          (is (= [[[0 65 0]]] (mapv :cells (restored-runs seen)))))))))

(deftest restore-digging-a-placed-block-with-a-pickaxe-that-breaks-says-tool-broke-then-tool-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (restore! {:blocks {"0,65,0" "cobblestone"} :inventory [{:name "stone_pickaxe" :count 1}]}
                                             [(zs/whole-zone "Miles")] [placed])]
          (.override (.-world p) "dig"
                     (fn ^:async f [token args impl]
                       (let [r (await (impl token args))]
                         (swap! (fake/state p) assoc :inventory [])
                         r)))
          (await (zs/run-until-empty eng 6))
          (is (= 1 (count (zs/trespass seen :tool.broke))))
          (is (= 1 (count (zs/trespass seen :tool.none)))))))))

(deftest restore-skips-and-warns-what-it-cannot-put-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[world entries why kept]
                [[{:inventory []} [dug] :not-carried 1]
                 [{:inventory [{:name "stone" :count 2}] :blocks {"0,65,0" "dirt"}} [dug] :changed 0]]]
          (let [{:keys [eng p seen]} (restore! world [(zs/whole-zone "Miles")] entries)]
            (await (zs/run-until-empty eng 6))
            (is (= [] (zs/calls p "place")) (pr-str why))
            (is (= [] (zs/calls p "dig")) (pr-str why))
            (is (= kept (count (tidy-entries eng))) (pr-str why))
            (is (= [[{:cell [0 65 0] :was "stone" :why why}]] (mapv :cells (not-restored-runs seen))) (pr-str why))
            (is (= [] (restored-runs seen)) (pr-str why))))))))

(deftest restore-waits-while-hurt-or-with-a-hostile-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[world reason] [[{:inventory [{:name "stone" :count 2}] :self {:health 5}} :low-health]
                                [{:inventory [{:name "stone" :count 2}] :entities [zombie] :blocks floor} :hostile-near]]]
          (let [{:keys [eng p]} (restore! world [(zs/whole-zone "Miles")] [dug])
                id (first (:list (core/state eng)))]
            (await (zs/run-until-empty eng 6))
            (is (= reason (:reason (core/waiting eng id))) (pr-str reason))
            (is (= [] (zs/calls p "place")) (pr-str reason))
            (is (= [] (zs/calls p "dig")) (pr-str reason))
            (is (= 1 (count (tidy-entries eng))) (pr-str reason))))))))

(deftest a-put-back-that-waits-for-the-world-yields-the-run-and-costs-no-try
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (restore! (merge aside {:inventory [{:name "stone" :count 2}]}) [(zs/whole-zone "Miles")] [dug])
              orig ctx/call
              hostile-near {:kind :condition-failed :cause {:reason :hostile-near :text "a hostile is near"}}]
          (set! ctx/call (fn [c slot job & more] (if (= 'jobs.blocks.place job) (js/Promise.resolve hostile-near) (apply orig c slot job more))))
          (await (zs/run-until-empty eng 6))
          (set! ctx/call orig)
          (is (= [] (not-restored-runs seen)) "a wait for the world is no failed cell")
          (is (= [0] (mapv :tries (tidy-entries eng)))))))))

(deftest a-cell-replanted-by-its-harvester-counts-as-restored-and-the-message-counts-only-real-failures
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [crop (fn [x] {:cell [x 65 0] :action :dig :was "potatoes" :now "air" :zone "vault" :tries 0})
              carrot {:cell [9 65 0] :action :dig :was "carrots" :now "air" :zone "vault" :tries 0}
              {:keys [eng p seen]} (restore! {:inventory [] :blocks {"1,65,0" "potatoes" "2,65,0" "potatoes"}} [(zs/whole-zone "Miles")]
                                             [(crop 1) (crop 2) carrot])
              id (first (:list (core/state eng)))]
          (await (zs/run-until-empty eng 6))
          (is (= [] (zs/calls p "place")))
          (is (= [[{:cell [9 65 0] :was "carrots" :why :not-carried}]] (mapv :cells (not-restored-runs seen))))
          (is (= #{[1 65 0] [2 65 0]} (set (:cells (first (restored-runs seen))))) "the replanted cells are restored")
          (is (= "1 broken blocks not restored, 2 restored" (:text (first (zs/trespass seen :stopped)))))
          (is (= 1 (count (tidy-entries eng)))))))))

(def standing-in-the-cell {:floor [-5 -5 5 5] :inventory [{:name "stone" :count 2}]})

(deftest restore-steps-clear-of-the-cell-before-placing-in-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (restore! standing-in-the-cell [(zs/whole-zone "Miles")] [dug])]
          (await (zs/run-until-empty eng 20))
          (is (= [{:x 0 :y 65 :z 0}] (mapv zs/arg-pos (zs/calls p "place"))))
          (is (= [] (tidy-entries eng)))
          (is (= [[[0 65 0]]] (mapv :cells (restored-runs seen))))
          (is (not= [0 0] [(js/Math.floor (.-x (.-pos (.self p)))) (js/Math.floor (.-z (.-pos (.self p))))]) "the body left the cell"))))))

(deftest restore-keeps-a-cell-the-body-cannot-leave
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (restore! (-> enclosed-body (update :blocks dissoc "0,65,0") (assoc :inventory [{:name "stone" :count 2}])) [(zs/whole-zone "Miles")] [dug])]
          (await (zs/run-until-empty eng 20))
          (is (= [] (zs/calls p "place")) "never walls the body in")
          (is (= 1 (count (tidy-entries eng))) "kept for when the body is out")
          (is (= [[{:cell [0 65 0] :was "stone" :why :occupied}]] (mapv :cells (not-restored-runs seen)))))))))

(def with-hitbox {:bodyHitbox true :floor [-8 -8 8 8] :inventory [{:name "stone" :count 12}]})

(defn dug-cell [cell] {:cell cell :action :dig :was "stone" :now "air" :zone "vault" :tries 0})

(deftest restore-steps-clear-when-the-hitbox-overlaps-the-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (restore! (assoc-in with-hitbox [:self :pos] [0.709 64 0.5]) [(zs/whole-zone "Miles")] [(dug-cell [1 64 0])])]
          (await (zs/run-until-empty eng 20))
          (is (= [{:x 1 :y 64 :z 0}] (mapv zs/arg-pos (zs/calls p "place"))) "one place, after the step: the server refuses an overlapping one")
          (is (= [] (tidy-entries eng)))
          (is (= [[[1 64 0]]] (mapv :cells (restored-runs seen)))))))))

(deftest restore-puts-back-a-dug-box-the-body-stands-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [box (vec (for [x [-1 0 1] z [-1 0 1]] [x 64 z]))
              {:keys [eng p seen]} (restore! (assoc-in with-hitbox [:self :pos] [0.5 64 0.5]) [(zs/whole-zone "Miles")] (mapv dug-cell box))]
          (await (zs/run-until-empty eng 60))
          (is (= (set box) (set (map (comp (juxt :x :y :z) zs/arg-pos) (zs/calls p "place")))))
          (is (= 9 (count (zs/calls p "place"))) "each cell placed once, none refused")
          (is (= [] (tidy-entries eng)))
          (is (= [[]] (mapv #(vec (remove (set box) (:cells %))) (restored-runs seen)))))))))

(deftest restore-keeps-a-cell-the-hitbox-overlaps-and-the-body-cannot-leave
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (-> enclosed-body (update :blocks dissoc "0,65,0") (assoc :inventory [{:name "stone" :count 2}] :bodyHitbox true)
                        (assoc-in [:self :pos] [0.709 64 0.5]))
              {:keys [eng p seen]} (restore! world [(zs/whole-zone "Miles")] [dug])]
          (await (zs/run-until-empty eng 20))
          (is (= [] (zs/calls p "place")))
          (is (= 1 (count (tidy-entries eng))))
          (is (= [[{:cell [0 65 0] :was "stone" :why :occupied}]] (mapv :cells (not-restored-runs seen)))))))))

(defn cell-key [[x y z]] (str x "," y "," z))

(def walled-cube
  "A roofed 3x3 room (walls at 2 from the middle, roof at y 66) on a wide floor: the body at its middle cannot leave."
  (into {} (for [x (range -2 3) z (range -2 3) y [64 65 66]
                 :when (or (= y 66) (= 2 (max (js/Math.abs x) (js/Math.abs z))))]
             [(cell-key [x y z]) "stone"])))

(deftest restore-keeps-a-cell-when-the-only-clear-cells-are-outside-the-walls
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc with-hitbox :blocks walled-cube)
              {:keys [eng p seen]} (restore! (assoc-in world [:self :pos] [0 64 0]) [(zs/whole-zone "Miles")] [(dug-cell [0 64 0])])]
          (await (zs/run-until-empty eng 20))
          (is (nil? (restore-broken/clear-cell {:primitives p} (dug-cell [0 64 0]) [])) "no clear cell the body can walk to")
          (is (= [] (zs/calls p "place")))
          (is (= 1 (count (tidy-entries eng))))
          (is (= [[{:cell [0 64 0] :was "stone" :why :occupied}]] (mapv :cells (not-restored-runs seen)))))))))

(def dead-end
  "A one-wide tunnel x 2..4 at z 0 (feet y 64, roof y 66) with the body at its closed end x 4."
  (into {} (for [x (range 1 6) z [-1 0 1] y [64 65 66]
                 :when (or (= y 66) (not= z 0) (#{1 5} x))
                 :when (not (and (= y 66) (not (<= 2 x 4))) )]
             [(cell-key [x y z]) "stone"])))

(deftest restore-puts-back-the-cell-that-would-seal-another-last
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc-in (assoc with-hitbox :blocks dead-end) [:self :pos] [4 64 0])
              entries [(dug-cell [3 64 0]) (dug-cell [2 64 0])]
              {:keys [eng p seen]} (restore! world [(zs/whole-zone "Miles")] entries)]
          (await (zs/run-until-empty eng 40))
          (is (= [{:x 2 :y 64 :z 0} {:x 3 :y 64 :z 0}] (mapv zs/arg-pos (zs/calls p "place"))) "the far cell first, then the one next to the body")
          (is (= [] (tidy-entries eng)))
          (is (= [[[2 64 0] [3 64 0]]] (mapv :cells (restored-runs seen)))))))))

(deftest a-breathe-trespass-is-restored-after-the-body-moves-clear
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (assoc enclosed-body :floor [-5 -5 5 5] :inventory [{:name "stone" :count 2}])
                                             [(zs/whole-zone "Miles")])]
          (core/register-reflex! eng {:trigger :tidy-pending})
          (core/submit! eng '(jobs.survival.breathe) {})
          (await (ticks! eng 40))
          (is (= [{:x 0 :y 66 :z 0}] (mapv zs/arg-pos (zs/calls p "place"))) "gravity drops the body back to the shaft floor: the roof cell is clear of it and goes back, its head cell does not")
          (is (= 1 (count (tidy-entries eng))) "the cell at its head waits")
          (fake/swap-self! p assoc :pos [4 64 0])
          (await (ticks! eng 40))
          (is (= #{{:x 0 :y 65 :z 0} {:x 0 :y 66 :z 0}} (set (map zs/arg-pos (zs/calls p "place")))))
          (is (= [] (tidy-entries eng))))))))

(deftest a-walk-that-fails-is-a-try-and-the-cell-is-given-up-in-one-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (restore! (merge with-hitbox {:floor [-60 -8 60 8] :self {:pos [-50 64 0]}
                                                         :inventory [{:name "stone" :count 2}]})
                                      [(zs/whole-zone "Miles")] [(dug-cell [50 64 0])])]
          (.override (.-world p) "steer" (fn ^:async f [_ _ _] #js {:status "failed" :reason "no controls"}))
          (await (core/tick! eng))
          (is (= [[{:cell [50 64 0] :was "stone" :why :gave-up}]] (mapv :cells (not-restored-runs seen)))
              "each failed walk is a try; after max-tries the cell is given up, all in one run")
          (is (= [] (tidy-entries eng))))))))

(deftest unreachable-reads-the-one-reach-setting-for-trigger-and-job
  (let [p (tu/fake (assoc-in (assoc with-hitbox :blocks walled-cube) [:self :pos] [0 64 0]))
        far {:cell [5 64 0]}
        unreachable #(settings/with-settings {:jobs.lib.tidy/reach %} (fn [] (tidy/unreachable? p far)))]
    (is (unreachable 3) "farther than the reach, sealed in")
    (is (not (unreachable 10)) "within the reach, no walk needed")))

;; ------------------------------------------------------------------ the tidy-pending trigger

(def with-stone (merge aside {:inventory [{:name "stone" :count 2}]}))
(def with-stone-world with-stone)

(defn holds?
  "Whether the trigger holds on a fake world with the memory entries [[kind data] ...] written and the jobs live
  (default none)."
  ([world memory] (holds? world memory #{}))
  ([world memory live]
   (let [s (mem/open (tu/tmp-dir) {:now (constantly 1000)})]
     (doseq [[kind data] memory] (mem/write! s kind data tidy/tidy-policy))
     (tidy-pending/tidy-pending (tu/fake world) (mem/view s) tidy-pending/defaults nil live))))

(deftest the-trigger-ignores-entries-of-a-job-that-is-still-live
  (doseq [[live expected why] [[#{"j1"} false "its job is live"]
                               [#{"j2"} true "another job is live, its own has ended"]
                               [#{} true "its job has ended"]]]
    (is (= expected (holds? with-stone-world [[:tidy (assoc dug :job "j1")]] live)) why))
  (is (true? (holds? with-stone-world [[:tidy dug]] #{"j1"})) "an entry with no job id is never held back"))

(def reported [:tidy-reported {:cells [[0 65 0]]}])

(deftest the-trigger-holds-for-a-restorable-entry-when-safe-and-for-an-unreported-one
  (doseq [[world memory expected why]
          [[with-stone [[:tidy dug]] true "carried, cell as left, safe"]
           [with-stone [[:tidy dug] reported] true "still restorable after a report"]
           [{:blocks {"0,65,0" "cobblestone"}} [[:tidy placed]] true "a placed block is dug again, no item needed"]
           [{} [[:tidy dug]] true "not carried and not reported yet: the job warns once"]
           [{} [[:tidy dug] reported] false "not carried and reported: nothing changed"]
           [{:blocks {"0,65,0" "dirt"}} [[:tidy dug] reported] false "changed cell reported"]
           [{:blocks {"0,65,0" "dirt"}} [[:tidy dug]] true "changed cell is forgotten by one run"]
           [(assoc with-stone :entities [(assoc-in zombie [:pos :x] 5)] :blocks (into {} (for [x [2 3] y [64 65 66]] [(str x "," y ",0") "stone"]))) [[:tidy dug]] true "a walled-off hostile is no danger"]
           [with-stone [] false "no entries"]
           [with-stone [[:tidy (assoc dug :tries (tidy/max-tries))] reported] false "given up and reported"]
           [(assoc with-stone :unloaded ["0,65,0"]) [[:tidy dug]] false "cell not loaded"]]]
    (is (= expected (holds? world memory)) why)))

(deftest the-trigger-holds-for-a-loaded-cell-not-yet-seen
  (let [s (mem/open (tu/tmp-dir) {:now (constantly 1000)})]
    (mem/write! s :tidy dug tidy/tidy-policy)
    (is (true? (tidy-pending/tidy-pending (tu/seeing-after-look (tu/fake with-stone)) (mem/view s) tidy-pending/defaults nil #{})))))

(deftest an-unseen-cell-is-not-a-changed-one
  (let [p (tu/seeing-after-look (tu/fake with-stone))]
    (is (nil? (tidy/why-not p dug)) "not made out yet: the restore walks up and sees it")
    (is (= :changed (tidy/why-not (tu/fake (assoc with-stone :blocks {"0,65,0" "dirt"})) dug)) "seen and different")))

(deftest the-trigger-does-not-hold-while-airborne
  (is (false? (holds? (assoc-in with-stone [:self :onGround] false) [[:tidy dug]])) "mid-jump")
  (is (true? (holds? (assoc-in with-stone [:self :onGround] true) [[:tidy dug]])) "landed"))

(defn holds-at
  "tidy-pending on a fresh view of memory at time now-s seconds, the body at pos."
  [world memory now-s pos]
  (let [s (mem/open (tu/tmp-dir) {:now (constantly (* 1000 now-s))})]
    (doseq [[kind data] memory] (mem/write! s kind data tidy/tidy-policy))
    (tidy-pending/tidy-pending (tu/fake (assoc-in world [:self :pos] pos)) (mem/view s) tidy-pending/defaults nil #{})))

(defn tried
  "dug with a counted try at t-s seconds, from the body at [2 64 0] (can: whether it was restorable then)."
  ([t-s] (tried t-s true))
  ([t-s can] (assoc (update dug :tries inc) :tried {:t (* 1000 t-s) :x 2 :y 64 :z 0 :can can})))

(deftest an-attempted-cell-is-not-tried-again-until-the-body-moves-far-or-the-window-passes
  (let [at (fn [t pos] (holds-at with-stone [[:tidy (tried 1000)]] t pos))]
    (is (true? (holds-at with-stone [[:tidy dug]] 990 [2 64 0])) "never tried: held")
    (is (true? (holds-at with-stone [[:tidy (update dug :tries inc)]] 1000 [2 64 0])) "a try with no record: held")
    (is (false? (at 1000 [2 64 0])) "a try was counted: wait")
    (is (false? (at 1010 [2 64 0])) "same place, soon after")
    (is (false? (at 1010 [4 64 0])) "moved a little")
    (is (true? (at 1010 [40 64 0])) "moved far")
    (is (true? (at (+ 1000 (tidy-pending/retry-s)) [2 64 0])) "window passed")))

(deftest the-trigger-waits-for-entries-far-from-the-body
  (let [far (assoc dug :cell [130 65 0])
        at (fn [entry pos] (holds-at (assoc with-stone :unloaded []) [[:tidy entry]] 1000 pos))]
    (is (false? (at far [0 64 0])) "130 blocks away: wait")
    (is (true? (at far [120 64 0])) "body came near")
    (is (false? (at (assoc dug :cell [0 130 0]) [0 64 0])) "far above")
    (is (true? (at dug [0 64 0])) "near")))

(deftest the-trigger-answers-from-memory-alone-so-a-restart-changes-nothing
  (is (= (holds-at with-stone [[:tidy (tried 1000)]] 1010 [2 64 0])
         (holds-at with-stone [[:tidy (tried 1000)]] 1010 [2 64 0]))
      "same entries, same answer, whatever ran before")
  (is (false? (holds-at with-stone [[:tidy (tried 1000)]] 1010 [2 64 0])))
  (is (true? (holds-at with-stone [[:tidy (tried 1000 false)]] 1010 [2 64 0])) "restorable now, was not at the try: something changed"))

(deftest a-counted-try-is-stamped-on-the-entry-for-the-trigger
  (is (= (:tried (tried 5)) (tidy/tried-record {:x 2 :y 64 :z 0} 5000 true)))
  (is (= {:t 5000 :x 2 :y 64 :z 0 :can false} (tidy/tried-record {:x 2 :y 64 :z 0} 5000 false))))

(deftest a-new-cell-is-tried-at-once
  (is (false? (holds-at with-stone [[:tidy (tried 1000)]] 1000 [2 64 0])))
  (is (true? (holds-at with-stone [[:tidy (tried 1000)] [:tidy (assoc dug :cell [0 65 1])]] 1001 [2 64 0])) "other cell too"))

(deftest the-trigger-is-a-builtin-with-a-cooldown
  (is (= tidy-pending/tidy-pending (:when (:tidy-pending triggers/all))))
  (is (= '(jobs.survival.restore-broken) (:job (:tidy-pending triggers/all))))
  (is (= [:cooldown 10] ((juxt :persistence :cooldown-s) (:tidy-pending triggers/all)))))

(deftest a-trespass-is-restored-by-itself-through-the-trigger
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup with-stone [(zs/whole-zone "Miles")])]
          (seed! eng [dug])
          (core/register-reflex! eng {:trigger :tidy-pending})
          (await (ticks! eng 12))
          (is (= [{:x 0 :y 65 :z 0}] (mapv zs/arg-pos (zs/calls p "place"))))
          (is (= [] (tidy-entries eng)))
          (is (= [] (not-restored-runs seen))))))))

(deftest unrestorable-entries-warn-once-and-do-not-loop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup {} [(zs/whole-zone "Miles")])]
          (seed! eng [dug])
          (core/register-reflex! eng {:trigger :tidy-pending})
          (await (ticks! eng 40))
          (is (= [] (zs/calls p "place")))
          (is (= 1 (count (not-restored-runs seen))))
          (is (= [] (restored-runs seen)) "nothing restored, no info")
          (is (= 1 (count (tidy-entries eng))) "the entry is kept for later"))))))

(deftest a-reported-entry-is-restored-once-the-item-is-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup aside [(zs/whole-zone "Miles")])]
          (seed! eng [dug])
          (core/register-reflex! eng {:trigger :tidy-pending})
          (await (ticks! eng 20))
          (is (= 1 (count (not-restored-runs seen))))
          (fake/add-item! p "stone" 2)
          (await (ticks! eng 20))
          (is (= [{:x 0 :y 65 :z 0}] (mapv zs/arg-pos (zs/calls p "place"))))
          (is (= [] (tidy-entries eng))))))))

;; ------------------------------------------------------------------ after a backoff

(def unplaceable
  "A dug entry whose put-back the server refuses every time (seeds onto no farmland): restore-broken backs off on it."
  {:cell [1 65 0] :action :dig :was "wheat_seeds" :now "air" :zone "vault" :tries 0 :job "j0"})

(defn setup-with-backoff
  "zs/setup with the engine's default backoff on and the clock returned, so a test can wait the backoff out."
  [world zones]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:offlineScale 0.0001 :bodyHitbox true} world))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (ew/of-data {} {} zones)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async ticks-over!
  "n ticks, the clock moving ms after each."
  [eng clock n ms]
  (dotimes [_ n]
    (await (core/tick! eng))
    (swap! clock + ms)))

(defn firings [seen] (count (filter #(and (= :fired (:kind %)) (= :tidy-pending (:reflex %))) @seen)))

(defn ^:async backed-off!
  "A body by an unplaceable entry with the tidy-pending trigger as registered; returns the setup once the reflex ended."
  [world]
  (let [{:keys [eng seen] :as s} (setup-with-backoff (merge-with into aside {:blocks {"-1,65,0" "stone"}} world {:inventory [{:name "wheat_seeds" :count 4}]})
                                                     [(zs/whole-zone "Miles")])]
    (seed! eng [unplaceable])
    (core/register-reflex! eng {:trigger :tidy-pending})
    (await (ticks-over! eng (:clock s) 12 10))
    (is (= 1 (count (filter #(and (= :ended (:kind %)) (= :tidy-pending (:reflex %))) @seen)))
        "the first run ended once, the entry given up inside it")
    s))

(deftest after-a-backoff-a-new-entry-is-restored
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (await (backed-off! {:inventory [{:name "stone" :count 2}]}))]
          (seed! eng [(assoc dug :job "j9")])
          (await (ticks-over! eng clock 60 1000))
          (is (= [{:x 0 :y 65 :z 0}] (filterv #(= 0 (:x %)) (mapv zs/arg-pos (zs/calls p "place")))) "the new entry is put back")
          (is (= [] (tidy-entries eng)) "the given-up entry is forgotten too")
          (is (>= 3 (firings seen)) "no loop on the entry that cannot be placed"))))))

(deftest after-a-backoff-a-not-carried-entry-is-restored-once-the-item-is-picked-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock]} (await (backed-off! {}))]
          (seed! eng [(assoc dug :job "j9")])
          (await (ticks-over! eng clock 60 1000))
          (is (= [] (filterv #(= 0 (:x %)) (mapv zs/arg-pos (zs/calls p "place")))) "no stone carried")
          (is (= [#{{:cell [1 65 0] :was "wheat_seeds" :why :gave-up}} #{{:cell [0 65 0] :was "stone" :why :not-carried}}]
                 (mapv (comp set :cells) (not-restored-runs seen)))
              "one warn per run: the seeds given up, then the stone not carried")
          (let [n (firings seen)]
            (await (ticks-over! eng clock 60 1000))
            (is (= n (firings seen)) "nothing changed: no firing"))
          (fake/add-item! p "stone" 2)
          (await (ticks-over! eng clock 60 1000))
          (is (= [{:x 0 :y 65 :z 0}] (filterv #(= 0 (:x %)) (mapv zs/arg-pos (zs/calls p "place")))) "put back once carried")
          (is (= [] (tidy-entries eng))))))))

(def dirt-box {:area {:min {:x 1 :y 65 :z 1} :max {:x 2 :y 65 :z 2}}})
(def dirt-world
  "Four dirt cells at y 65 on a stone floor at y 64 (the ground the body walks on once they are dug, and steps out of the pit onto)."
  {:blocks (merge (tu/box -4 64 -4 6 64 6 "stone")
                  {"1,65,1" "dirt" "2,65,1" "dirt" "1,65,2" "dirt" "2,65,2" "dirt"})})
(def miles-zone {:name "farm" :min [1 60 1] :max [2 70 2] :owner "Miles"})

(defn ^:async clear-in-foreign-zone!
  "clear-cells with :ignore-zones? in Miles's zone, the tidy-pending reflex registered; [world-dug-while-running seen
  eng p] once the job and the restore have ended. Records how many places happened before the box job ended."
  [world]
  (let [{:keys [eng p seen]} (setup world [miles-zone])
        placed-while-live (atom 0)]
    (core/register-reflex! eng {:trigger :tidy-pending})
    (core/submit! eng (list 'jobs.blocks.clear-cells (assoc dirt-box :ignore-zones? true)) {})
    (loop [i 0]
      (when (< i 80)
        (let [live? (some #(= 'jobs.blocks.clear-cells (:def (get-in (core/state eng) [:instances %]))) (:list (core/state eng)))]
          (when live? (reset! placed-while-live (count (zs/calls p "place")))))
        (await (core/tick! eng))
        (recur (inc i))))
    {:placed-while-live @placed-while-live :seen seen :eng eng :p p}))

(deftest a-trespassing-clear-cells-is-not-undone-while-it-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [placed-while-live seen eng p]} (await (clear-in-foreign-zone! (assoc dirt-world :inventory [{:name "dirt" :count 4}])))]
          (is (= 0 placed-while-live) "nothing placed while clear-cells was live")
          (is (= 4 (count (zs/calls p "dig"))) "each dirt dug exactly once")
          (is (= 4 (count (zs/calls p "place"))) "restored once after the job ended")
          (is (= [] (tidy-entries eng)))
          (is (= [] (not-restored-runs seen))))))))

;; ------------------------------------------------------------------ a sealed body with entries outside

(def sealed-with-outside
  "The body sealed in the walled cube at its middle: its own cell waits (:occupied) and so does a cell 5 away outside the walls."
  [(dug-cell [0 64 0]) (dug-cell [5 64 0])])

(deftest a-sealed-body-reports-what-it-cannot-reach-once-and-does-not-loop
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc-in (assoc with-hitbox :blocks walled-cube) [:self :pos] [0 64 0])
              {:keys [eng p seen clock]} (setup-with-backoff world [(zs/whole-zone "Miles")])]
          (seed! eng sealed-with-outside)
          (core/register-reflex! eng {:trigger :tidy-pending})
          (await (ticks-over! eng clock 80 1000))
          (is (= [] (zs/calls p "place")) "nothing is placed")
          (is (= [] (filterv #(= :backoff (:outcome %)) (filter #(= :ended (:kind %)) @seen))) "no run backed off")
          (is (= 1 (firings seen)) "one run, no refiring every cycle")
          (is (= [[{:cell [0 64 0] :was "stone" :why :occupied} {:cell [5 64 0] :was "stone" :why :unreachable}]]
                 (mapv :cells (not-restored-runs seen)))
              "one warn naming both")
          (is (= 2 (count (tidy-entries eng))) "both entries wait"))))))

(deftest restore-keeps-the-doorway-that-is-the-bodys-only-way-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc-in (assoc with-hitbox :floor [-25 -25 25 25] :blocks (dissoc walled-cube "2,64,0" "2,65,0")) [:self :pos] [0 64 0])
              {:keys [eng p seen]} (restore! world [(zs/whole-zone "Miles")] [(dug-cell [2 64 0])])]
          (is (not (reach-room/enclosed? p)) "the hut has a doorway")
          (is (reach-room/enclosed? p #{[2 64 0]}) "filling it would shut the body in")
          (await (zs/run-until-empty eng 30))
          (is (= [] (zs/calls p "place")) "the doorway is not filled")
          (is (= 1 (count (tidy-entries eng))) "kept for when the body is out")
          (is (= [[{:cell [2 64 0] :was "stone" :why :seals}]] (mapv :cells (not-restored-runs seen)))))))))

(deftest restore-puts-back-a-cell-that-only-trims-a-room-sized-floor
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc-in (assoc with-hitbox :floor [-12 -20 12 20] :blocks {"2,66,0" "stone"}) [:self :pos] [0 64 0])
              {:keys [eng p]} (restore! world [(zs/whole-zone "Miles")] [(dug-cell [2 65 0])])]
          (is (not (reach-room/enclosed? p)) "the floor is room: exactly room-cells walkable cells")
          (is (reach-room/enclosed? p #{[2 65 0]}) "one cell less floods as closed at room-cells")
          (await (zs/run-until-empty eng 30))
          (is (= [{:x 2 :y 65 :z 0}] (mapv zs/arg-pos (zs/calls p "place"))) "the body keeps the rest of the floor"))))))

(def mid-shaft
  "A one-wide shaft x 0, z 0 walled by stone at y 65 and 66, the body standing at its feet y 65 on a block at y 64; the
  cell at y 67 above its head is an open escalation hole."
  (into {"0,64,0" "stone" "0,68,0" "stone"}
        (for [y [65 66] [dx dz] [[1 0] [-1 0] [0 1] [0 -1]]] [(cell-key [dx y dz]) "stone"])))

(def escalation-hole {:cell [0 67 0] :action :dig :was "stone" :now "air" :zone "vault" :tries 0 :escalation true :any-of ["stone"]})

(deftest restore-keeps-an-escalation-hole-while-the-body-is-shut-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc with-hitbox :floor [-25 -25 25 25] :blocks mid-shaft :self {:pos [0 65 0]})
              {:keys [eng p seen]} (restore! world [(zs/whole-zone "Miles")] [escalation-hole])]
          (is (reach-room/enclosed? p) "the body is shut in the shaft")
          (is (not (tidy/in-body? p [0 67 0])) "the hole is clear of the hitbox")
          (await (zs/run-until-empty eng 30))
          (is (= [] (zs/calls p "place")) "the hole is the body's way on: not filled")
          (is (= 1 (count (tidy-entries eng))) "kept for when the body is out")
          (is (= [[{:cell [0 67 0] :was "stone" :why :shut-in}]] (mapv :cells (not-restored-runs seen))))
          (fake/swap-self! p assoc :pos [2 64 1])
          (is (not (reach-room/enclosed? p)) "out on open ground")
          (is (nil? (tidy/why-not p escalation-hole)) "nothing holds the hole back once the body is out"))))))

(def own-hole
  "An escalation hole noted before holes were noted only in another's zone: no :zone, :claim or :plan."
  (dissoc escalation-hole :zone))

(deftest an-own-hole-noted-earlier-is-neither-held-for-nor-filled
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (false? (holds? with-stone [[:tidy (assoc own-hole :cell [0 65 0])]]))
            "the trigger does not hold for the body's own digging")
        (is (true? (holds? with-stone [[:tidy (assoc escalation-hole :cell [0 65 0])]]))
            "a hole in another's zone is held for")
        (let [{:keys [eng p seen]} (restore! with-stone [] [(assoc own-hole :cell [0 65 0])])]
          (await (zs/run-until-empty eng 6))
          (is (= [] (zs/calls p "place")) "the hole stays")
          (is (= [] (not-restored-runs seen)) "it is no tidy work: nothing to warn about"))))))

(deftest a-sealed-body-restores-the-cell-it-can-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc-in (assoc with-hitbox :blocks (dissoc walled-cube "2,64,0")) [:self :pos] [0 64 0])
              {:keys [eng p seen]} (restore! world [(zs/whole-zone "Miles")] [(dug-cell [0 64 0]) (dug-cell [2 64 0])])]
          (await (zs/run-until-empty eng 30))
          (is (= [{:x 2 :y 64 :z 0}] (mapv zs/arg-pos (zs/calls p "place"))) "the wall cell within reach is put back")
          (is (= [[[2 64 0]]] (mapv :cells (restored-runs seen))))
          (is (= [[{:cell [0 64 0] :was "stone" :why :occupied}]] (mapv :cells (not-restored-runs seen)))))))))

;; ------------------------------------------------------------------ the floor rule

(def torch-ring
  "Torches in the floor at y 63 on the ring two cells out around the body: the nearest cells clear of the dug cell."
  (into {} (for [x (range -2 3) z (range -2 3) :when (= 2 (max (js/Math.abs x) (js/Math.abs z)))] [(cell-key [x 63 z]) "torch"])))

(deftest clear-cell-never-chooses-a-cell-above-a-torch
  (let [p (tu/fake (assoc-in (assoc with-hitbox :blocks torch-ring) [:self :pos] [0 64 0]))]
    (is (nil? (restore-broken/clear-cell {:primitives p} (dug-cell [0 64 0]) [])) "the ring of torches is no floor to walk or stand on")))

(deftest standable-cell-is-the-rule-clear-cell-and-go-to-share
  (doseq [[below expected] [["stone" true] ["torch" false] ["lava" false] ["cactus" false] ["air" false] ["water" false]]]
    (is (= expected (reach/standable-cell? (tu/fake {:blocks {"0,63,0" below}}) {:x 0 :y 64 :z 0})) below)))

(deftest restore-puts-every-cell-back-in-one-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc-in (assoc with-hitbox :blocks dead-end) [:self :pos] [4 64 0])
              entries [(dug-cell [3 64 0]) (dug-cell [2 64 0])]
              {:keys [eng p]} (restore! world [(zs/whole-zone "Miles")] entries)]
          (await (core/tick! eng))
          (is (= [] (:list (core/state eng))) "one tick ends the run")
          (is (= 2 (count (zs/calls p "place"))))
          (is (= [] (tidy-entries eng))))))))

;; ------------------------------------------------------------------ cuts and the pass cap

(def open-floor-world (assoc with-hitbox :self {:pos [0 64 0]}))

(deftest a-cut-between-the-put-back-and-the-forget-does-not-place-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (restore! open-floor-world [(zs/whole-zone "Miles")] [(dug-cell [3 64 0])])]
          (.override (.-world p) "place"
                     (fn ^:async f [token args impl]
                       (let [r (await (impl token args))]
                         (core/cut! eng (core/holder eng) :test nil)
                         r)))
          (await (core/tick! eng))
          (is (= 1 (count (zs/calls p "place"))))
          (core/submit! eng '(jobs.survival.restore-broken) {})
          (await (zs/run-until-empty eng 6))
          (is (= 1 (count (zs/calls p "place"))) "the cell holds what the job left: no second place")
          (is (= [] (tidy-entries eng))))))))

(deftest a-walk-that-yields-is-not-a-try
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (tu/with-near-stub (fn ^:async f [_ _ _ _] :partial)
          (fn ^:async b []
            (let [{:keys [eng seen]} (restore! open-floor-world [(zs/whole-zone "Miles")] [(dug-cell [3 64 0])])]
              (dotimes [_ 6] (await (core/tick! eng)))
              (is (= [0] (mapv :tries (tidy-entries eng))) "yielding go-to costs no try")
              (is (= [] (not-restored-runs seen)) "the job yields, it is no ending and never :gave-up")))))))))

(deftest a-run-that-hits-the-pass-cap-with-cells-waiting-ends-stopped-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (with-redefs [restore-broken/max-passes (fn [] 2)]
          (let [{:keys [eng seen]} (restore! open-floor-world [(zs/whole-zone "Miles")]
                                             [(dug-cell [3 64 0]) (dug-cell [3 64 2]) (dug-cell [3 64 -2])])]
            (await (core/tick! eng))
            (is (= [] (:list (core/state eng))))
            (is (= :not-restored (:reason (first (zs/trespass seen :stopped)))) "not :done while a cell waits")
            (is (= 1 (count (tidy-entries eng))) "the waiting cell keeps its entry")
            (let [warns (not-restored-runs seen)]
              (is (= 1 (count warns)))
              (is (= [(:cell (first (tidy-entries eng)))] (mapv :cell (:cells (first warns)))) "the warn lists the unvisited cell"))))))))

;; ------------------------------------------------------------------ bounded work per pass

(defn holes
  "n escalation holes (dug stone pillars, now air) on the open floor 6 to 13 blocks east of the body."
  [n]
  (vec (for [i (range n)] (assoc escalation-hole :cell [(+ 6 (mod i 8)) 64 (- (quot i 8) 4)]))))

(defn decide-work
  "The walk searches and floods one restore-broken pass decision runs over n escalation holes, and its verdict."
  [n]
  (let [{:keys [p]} (setup (assoc with-hitbox :floor [-30 -30 30 30] :self {:pos [0 64 0]}) [(zs/whole-zone "Miles")])
        searches (atom 0)
        floods (atom 0)
        search reach/search
        flood reach-room/flood
        verdict (with-redefs [reach/search (fn [& xs] (swap! searches inc) (apply search xs))
                              reach-room/flood (fn [& xs] (swap! floods inc) (apply flood xs))]
                  (first (restore-broken/decide {:primitives p} (holes n))))]
    {:verdict verdict :searches @searches :floods @floods}))

(deftest one-pass-decision-does-bounded-work-however-many-entries-wait
  (let [few (decide-work 20)
        many (decide-work 48)]
    (is (= :restore (:verdict many)))
    (is (<= (:floods many) 3) "the body's enclosure is flooded once a pass, not once per escalation entry")
    (is (= (:searches few) (:searches many)) "walk searches do not grow with the number of entries")
    (is (= (:floods few) (:floods many)))))

(def two-fields
  "Two open fields (x -45..-7 and 9..45) joined by one roofed tunnel at z 0 through a stone wall (x -6..8, y 64 to 66),
  the wall holding sealed one-wide pockets at z -4, -2, 2 and 4 (x -3..5): each side is larger than a search's node
  budget, so a walk that cannot get through runs it out."
  (into {} (for [x (range -6 9) z (range -20 21) y [64 65 66]
                 :when (not (and (#{64 65} y) (= 0 z)))
                 :when (not (and (#{64 65} y) (#{-4 -2 2 4} z) (<= -3 x 5)))]
             [(cell-key [x y z]) "stone"])))

(def two-fields-world (assoc with-hitbox :floor [-45 -20 45 20] :blocks two-fields))

(defn tunnel-holes
  "Escalation holes along the tunnel of two-fields (from x -6), then in the far field."
  [tunnel far]
  (vec (concat (for [x (range -6 (+ -6 tunnel))] (assoc escalation-hole :cell [x 64 0]))
               (for [i (range far)] (assoc escalation-hole :cell [(+ 12 (mod i 6)) 64 (- (quot i 6) 2)])))))

(defn count-work
  "Run f counting the walk searches, the cells they expand and the block lookups it makes:
  {:result :searches :nodes :lookups}."
  [f]
  (let [searches (atom 0)
        nodes (atom 0)
        lookups (atom 0)
        search reach/search
        lookup reach/seen-lookup
        forward reach/forward
        backward reach/backward
        result (with-redefs [reach/search (fn [& xs] (swap! searches inc) (apply search xs))
                             reach/forward (fn [& xs] (swap! nodes inc) (apply forward xs))
                             reach/backward (fn [& xs] (swap! nodes inc) (apply backward xs))
                             reach/seen-lookup (fn ([p] (swap! lookups inc) (lookup p reach/kind-of :solid))
                                                 ([p kind] (swap! lookups inc) (lookup p kind :solid))
                                                 ([p kind unseen] (swap! lookups inc) (lookup p kind unseen)))]
                 (f))]
    {:result result :searches @searches :nodes @nodes :lookups @lookups}))

(deftest a-pass-decision-in-cramped-terrain-expands-a-bounded-number-of-cells
  (let [{:keys [p]} (setup (assoc two-fields-world :self {:pos [-10 64 0]}) [(zs/whole-zone "Miles")])
        {:keys [result nodes]} (count-work #(restore-broken/decide {:primitives p}
                                                                   (tunnel-holes 5 16)))]
    (is (= :restore (first result)) "a seal search that runs out of budget is no seal")
    (is (<= nodes 12000) (str nodes " cells expanded"))))

(deftest a-step-clear-cell-in-cramped-terrain-runs-a-bounded-number-of-searches
  (let [p (tu/fake (assoc two-fields-world :self {:pos [1 64 0]}))
        waiting (tunnel-holes 15 0)
        {:keys [result searches lookups]} (count-work #(restore-broken/clear-cell {:primitives p} (dug-cell [1 64 0]) waiting))]
    (is (nil? result) "no clear cell: the tunnel is reserved and the pockets are sealed")
    (is (<= searches 6) (str searches " walk searches"))
    (is (= 1 lookups) "one block lookup")))

(deftest a-step-clear-cell-is-never-one-the-body-has-not-seen
  (let [p (tu/fake (assoc with-hitbox :self {:pos [0 64 0]}))
        sensed (.-sensedAt p)
        _ (aset p "sensedAt" (fn [q] (if (neg? (.-x q)) #js {:unknown true} (sensed q))))
        stand (restore-broken/clear-cell {:primitives p} (dug-cell [0 64 0]) [])]
    (is (some? stand))
    (is (not (neg? (:x stand))) "the west half never seen: the nearest stand there is not read off the true world")))

(deftest filling-a-doorway-shuts-the-body-in-only-over-walls-it-has-seen
  (let [shell (merge (tu/box -1 64 -1 3 66 1 "stone"))
        room (apply dissoc shell (concat (for [x (range 0 3) y [64 65]] (str x "," y ",0")) ["3,64,0" "3,65,0"]))
        p (tu/fake {:blocks room :floor [-30 -30 30 30] :self {:pos [1 64 0]}})
        sensed (.-sensedAt p)
        seals? #(restore-broken/seals-body? {:primitives p} {:cell [3 64 0] :action :dig} (restore-broken/probe {:primitives p}))]
    (is (true? (seals?)) "every wall seen: the doorway is the only way out")
    (aset p "sensedAt" (fn [q] (if (#{[1 64 1] [1 65 1]} [(.-x q) (.-y q) (.-z q)]) #js {:unknown true} (sensed q))))
    (is (false? (seals?)) "a wall cell never looked at may be a gap: not shut in")))

(def stair-cells-plan
  {:id "hut" :parts [{:id "stair" :cells [[0 65 0] [0 66 0]] :want :clear}]})

(deftest a-dig-in-another-plans-footprint-is-tidy-work-but-not-in-a-mineshaft
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[kind plan-recorded] [[nil [:hut :hut]] [:mineshaft []]]]
          (let [{:keys [eng]} (zs/setup-with-plan {:blocks (merge enclosed {"0,63,0" "stone"})}
                                                  (cond-> (shape/with-author stair-cells-plan "Miles") kind (assoc :kind kind)))]
            (core/submit! eng '(jobs.survival.breathe) {})
            (await (core/tick! eng))
            (is (= (map #(some-> % name) plan-recorded) (map :plan (tidy-entries eng))) (str kind))))))))

(deftest decide-looks-past-the-first-window-for-a-cell-that-seals-no-other
  (let [{:keys [p]} (setup (assoc with-hitbox :floor [-30 -30 30 30] :self {:pos [0 64 0]}) [(zs/whole-zone "Miles")])
        c {:primitives p}
        waiting (holes 6)
        sixth (last waiting)]
    (with-redefs [restore-broken/seals-others? (fn [_ e _ _] (not= sixth e))]
      (is (= [:restore sixth] (restore-broken/decide c waiting))))))

(deftest decide-restores-the-cell-with-a-block-beside-it-before-one-hanging-in-the-air
  (let [{:keys [p]} (setup (assoc with-hitbox :floor [-30 -30 30 30] :self {:pos [0 64 0]}) [(zs/whole-zone "Miles")])
        lower (dug-cell [5 64 0])
        upper (dug-cell [5 65 0])]
    (is (= [:restore lower] (restore-broken/decide {:primitives p} [upper lower])) "the upper cell is farther but rests on the lower")))
