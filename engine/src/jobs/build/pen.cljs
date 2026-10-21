(ns jobs.build.pen
  (:require [engine.args :as a] [jobs.lib.need :as need]
            [engine.composite :as composite]
            [engine.contract :as contract]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.build.from-plan :as from-plan]
            [jobs.lib.pen :as pen]
            [jobs.lib.pen-patch :as pen-patch]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.sense :as sense]
            [jobs.lib.build.plan :as plan]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.util :as u]
            [jobs.lib.storage :as storage]
            [jobs.lib.world :as known]))

(def doc
  "Build the fence a plan wants (:plan, optionally only its :part) and prove it holds animals.
  The pen is the bounding box of the plan's cells that want a fence, a fence gate or a wall.
  A plan with :surface true is put on the ground the body has seen (a pen on a slope follows it).
  Phase 0 fells each trunk seen inside the box with a jobs.forestry.fell-tree child (:at its base log), then clears
  the logs and leaves left, up to the fence tops, with a jobs.blocks.clear-cells child (:only), so no leaf staircase
  leads over the fence; trees not seen yet are left to the check.
  Then a jobs.access.cleanup child takes up the body's own scaffold-ledger blocks in and around the box (a night shelter's cheap blocks stay after dawn; an animal steps from one onto a fence).
  Phase 1 runs jobs.build.from-plan on the plan. Its arg :accept passes through, and its rules
  (zones, other plans' footprints, materials, cuts) apply. Phase 2: a body still inside the box walks out through the
  gate (a jobs.movement.go-to child to the cell outside it) and jobs.maintenance.shut shuts it. Phase 3 checks the pen
  over that box with jobs.lib.pen (see jobs.animals.pen-check): a step out of the box is a leak.
  A leak left after the build (a hole in the plan, or a cell the build refused, gave up on or lacked material for) is
  patched in up to ::max-passes passes (each fills the listed leaks, then reads again; a pass that put nothing right is the last) unless the call's :ways (:build, :patch) leaves :patch out: a jobs.blocks.place child puts one fence of the plan's kind in each
  :gap cell and on top of each :climb or :perch-in cell, then the pen is checked again. Nothing else is placed that the plan does not ask for.
  Needs: the plan, a zone list (unless :ignore-zones?), and the blocks of the fence (fetched by the builder unless
  the call's ways leave out :chest :craft :gather). Returns, as the data of its ending, {:closed? :reason :cells :leaks :gates :built {:placed :missing
  :short :given-up :wrong :refused}}. :reason is nil when closed, else :leak, :unbounded, :unloaded or :no-start.
  The pen ends with the body outside and the gate shut.
  Endings: done when the pen holds; nothing-possible with the :reason as cause when it does not (the data's :leaks name {:pos :why} (:gap :open-gate :climb :perch-in :open :unloaded; at most 12), and :built what the
  build left: :refused, :given-up, :short); nothing-possible :inside when the body cannot get out of the finished
  pen. An ending of the builder's can-start? (no data: :no-items, :no-zones) is this job's; a builder that ended
  short or refused after its work (data) goes on to the check, which reports the shortage as leaks with :built. A
  shut-gate child that did not end done is named in the data as :shut.
  Start facts, all waits or refusals of can-start?:
  - {:reason :plan-trouble :why text}: a wait while the plan is missing or unreadable; never when it has no cells (in :part) or no fence cells.
  - {:reason :uneven :why text}: its ring steps more than 1 between neighbouring fences, or a gate is more than 1 above
    the ground of an inside neighbour (animals will not step down 2), unless :uneven? true.
  - {:reason :no-zones}: no zone list has been read (unless :ignore-zones?).
  - {:reason :no-items}: the builder's own (jobs.build.from-plan): nothing to place is carried or fetchable; also the
    ending (condition-failed) when its look round finds that, with nothing placed.
  A pen already sound with the body outside it is done at once with nothing placed. Nothing is rebuilt after a restart: the world is the memory.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world" :spec a/name? :default nil}
   :part {:doc "only the cells of this part" :spec a/name? :default nil}
   :accept {:doc "as jobs.build.from-plan" :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default [:fluid-adjacent]}
   :explore {:doc "as jobs.build.from-plan: walk off to look for blocks that are not in sight" :spec boolean? :default true}
   :uneven? {:doc "build although the ring steps more than 1 between neighbouring fences or the inside drops more than 1 at the gate" :spec boolean? :default false}
   :ignore-zones? :shared})

;; ------------------------------------------------------------------ the pen from the plan

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one pen build may take." :spec (a/int-in 1 nil)}
   ::max-passes {:default 3 :doc "Patch passes (patch, then read again) before the leaks that remain are reported." :spec (a/int-in 1 nil)}
   ::max-cells {:default pen/default-max-cells :doc "Most cells the pen check visits before it gives up with :unbounded." :spec (a/int-in 1 nil)}})
(defn max-cells [] (settings/get settings ::max-cells))

(defn gate? [want] (boolean (some #(str/ends-with? % "_fence_gate") (pen-read/want-blocks want))))

(def max-step "The most an animal steps up or down between neighbouring cells of a pen." 1)

(defn uneven
  "Where the pen is too uneven for animals: [{:why :step :at :to :step}] for neighbouring barrier columns whose
  lowest cells differ by more than max-step, [{:why :gate-drop :gate :inside [x z] :drop}] for a gate more than
  max-step above the seen ground (top-of x z y, nil when unknown) of an inside neighbour column, [{:why :gate-blocked
  :gate :outside [x z]}] for a gate whose outside neighbour has no room for an animal or the body (name-at x y z, the
  seen block name or nil: a trunk or wall there)."
  ([cells top-of] (uneven cells top-of (constantly nil)))
  ([cells top-of name-at]
  (let [low (reduce (fn [m {[x y z] :pos :keys [want]}]
                      (if (pen-read/barrier? want) (update m [x z] #(min y (or % y))) m))
                    {} cells)
        box (pen-read/barrier-box cells)
        inside? (fn [[x z]] (and (<= (:x (:min box)) x (:x (:max box))) (<= (:z (:min box)) z (:z (:max box)))))
        near (fn [[x z]] [[(inc x) z] [x (inc z)]])
        steps (for [[col y] (sort low) n (near col) :let [y2 (low n)] :when (and y2 (> (js/Math.abs (- y y2)) max-step))]
                {:why :step :at [(first col) y (second col)] :to [(first n) y2 (second n)] :step (js/Math.abs (- y y2))})
        drops (for [{[x y z] :pos :keys [want]} cells
                    :when (gate? want)
                    [nx nz :as n] [[(inc x) z] [(dec x) z] [x (inc z)] [x (dec z)]]
                    :when (and (not (low n)) (inside? n))
                    :let [top (top-of nx nz y)]
                    :when (and top (> (- y (inc top)) max-step))]
                {:why :gate-drop :gate [x y z] :inside n :drop (- y (inc top))})
        open? (fn [x y z] (let [n (name-at x y z)] (or (nil? n) (bs/passable? n))))
        free? (fn [x y z] (and (open? x y z) (open? x (inc y) z)))
        blocked (for [{[x y z] :pos :keys [want]} cells
                      :when (gate? want)
                      [nx nz :as n] [[(inc x) z] [(dec x) z] [x (inc z)] [x (dec z)]]
                      :when (and (not (low n)) (not (inside? n)) (not (free? nx y nz)) (not (free? nx (inc y) nz)))]
                  {:why :gate-blocked :gate [x y z] :outside n})]
    (vec (concat steps drops blocked)))))

(defn uneven-text [problems]
  (str "the pen is uneven (steps over " max-step "): "
       (str/join ", " (map (fn [{:keys [why at to step gate inside outside drop]}]
                             (case why
                               :step (str (pr-str at) " to " (pr-str to) " steps " step)
                               :gate-drop (str "gate " (pr-str gate) " drops " drop " to " (pr-str inside))
                               :gate-blocked (str "gate " (pr-str gate) " opens onto the blocked cell " (pr-str outside))))
                           (take 4 (sort-by #(- (or (:step %) (:drop %) 9)) problems))))))

(defn planned
  "{:cells judged} for the plan in the args, or {:trouble text}."
  [c]
  (let [{:keys [plan part]} (:args c)
        p (:primitives c)
        answer (plan/surfaced c (known/plan c plan))
        cells (when (and answer (not (:broken answer)))
                (plan/judged p answer part (set (keys (storage/carried-totals (u/inventory p)))) nil))
        trouble (or (plan/plan-trouble answer cells)
                    (when-not (pen-read/barrier-box cells) "the plan has no fence, wall or gate cells")
                    (when (and (nil? (known/zones c)) (not (:ignore-zones? (:args c)))) plan/no-zones))]
    (if-not trouble
      {:cells cells}
      {:trouble trouble :later? (and (not= plan/no-zones trouble) (known/unreadable? answer))})))

(defn pen-opts [c cells]
  {:box (pen-read/barrier-box cells) :max-cells (max-cells)})

(defn read-pen
  "The pen check over what the body sees or remembers (pen-read/read; the round's finish! looks first): an unseen cell is an :unloaded leak."
  [c cells]
  (pen-read/read (:primitives c) (pen-opts c cells)))

(defn build-args [c]
  (assoc (select-keys (:args c) [:plan :part :accept :explore :ignore-zones?]) :ways (need/pass-ways c)))

;; ------------------------------------------------------------------ conditions

(def target [:plan :part])

(defn judged-plan
  "planned, once per check-cache window."
  [c]
  (check-cache/cached c :planned #(planned c)))

(defn plan-problem
  "Later for a plan that is missing or unreadable, or no zone list read (the plan may be saved or fixed later); never
  for a readable plan without cells or fence cells."
  [c]
  (let [{:keys [trouble later?]} (judged-plan c)]
    (cond
      (nil? trouble) (contract/ok :plan "the plan can be built")
      (= plan/no-zones trouble) (contract/later :no-zones "no zone list has been read")
      later? (contract/later :plan-trouble trouble :why trouble)
      :else (contract/never :plan-trouble trouble :why trouble))))

(def uneven-problem
  "Later :uneven for a ring that steps over max-step or a gate that drops too far onto the seen ground, unless :uneven?
  (ground the body sees changes, so at the start only)."
  (checks/refusing
   :even "the ground is even"
   (fn [c]
     (let [{:keys [cells trouble]} (judged-plan c)]
       (when-not (or trouble (:uneven? (:args c)))
         (when-let [bad (seq (uneven cells (plan/top-fn c) (fn [x y z] (u/seen-name (:primitives c) {:x x :y y :z z}))))]
           {:reason :uneven :why (uneven-text bad)}))))))

(def ways (into [:build :patch] need/fetch-ways))
(def defaults {:spend {:job-time ::job-time-s}})
(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)

(def can-start? (cond/and plan-problem uneven-problem from-plan/no-items))
(def can-run? plan-problem)

(defn inside?
  "Whether the body's column is in the box (a little above or below it too)."
  [box self]
  (let [[x y z] (map #(int (js/Math.floor %)) [(:x self) (:y self) (:z self)])]
    (and (<= (:x (:min box)) x (:x (:max box)))
         (<= (:z (:min box)) z (:z (:max box)))
         (<= (- (:y (:min box)) 2) y (+ (:y (:max box)) 2)))))

(defn outside?
  "Whether the body stands outside the pen's box."
  [c cells]
  (not (inside? (pen-read/barrier-box cells) (u/self-pos c))))

(defn result-of
  "The data of the call's ending: the pen's summary, what was built and the shut-gate cause when it did not end done."
  [c answer]
  (cond-> (assoc answer :built (:built (ctx/mem c)))
    (:shut (ctx/mem c)) (assoc :shut (:shut (ctx/mem c)))))

(defn done?
  "Ok when nothing is owed, the pen holds and the body is outside it; else later, with the cells placed and still owed."
  [c]
  (let [{:keys [cells trouble]} (judged-plan c)
        plan (:plan (:args c))
        read (when-not trouble (read-pen c cells))
        answer (some-> read pen/summary)
        holds? (boolean (and (not trouble) (empty? (plan/owed cells)) (:closed? answer) (empty? (:remembered read)) (outside? c cells)
                             (or (:built (ctx/mem c)) (not (checks/running? c)))))]
    (checks/goal holds?
                 [:holds (str "pen of " plan " holds, " (:cells answer) " cells, placed " (get-in (ctx/mem c) [:built :placed] 0))]
                 [:owed (str "pen of " plan " is not finished")]
                 {:units :cells :done (get-in (ctx/mem c) [:built :placed] 0) :left (if trouble 0 (count (plan/owed cells)))
                  :result (when answer (result-of c answer))})))

(defn ^:async quote-of [c]
  (select-keys (await (ctx/quote c 'jobs.build.from-plan (build-args c))) [:uses :yields]))

;; ------------------------------------------------------------------ rounds

(defn patch-item
  "The fence or wall item of the plan's cells to patch with: the first barrier cell that is no gate, as allotted."
  [cells]
  (when-let [cell (first (filter #(and (pen-read/barrier? (:want %)) (not (gate? (:want %)))) cells))]
    (or (:item cell) (first (pen-read/want-blocks (:want cell))))))

(defn ^:async finish!
  "Read the pen (looking at the remembered cells in sight first), emit the outcome and end: done when it holds, else nothing-possible with the reason."
  [c cells]
  (let [plan (:plan (:args c))
        built (:built (ctx/mem c))
        answer (pen/summary (await (pen-read/read! c (pen-opts c cells))))
        result (result-of c answer)]
    (cond
      (:closed? answer)
      (ctx/end c :done nil (assoc result :text (str "pen of " plan " holds, " (:cells answer) " cells, placed " (:placed built))))
      (and (ctx/way? c :patch) (seq (pen-patch/patch-cells (:leaks answer))) (patch-item cells)
           (let [{:keys [pass pass-ok]} (ctx/mem c)] (pen-patch/another-pass? pass pass-ok (settings/get settings ::max-passes))))
      (do (ctx/update-mem! c #(assoc % :phase :patch :pass (inc (or (:pass %) 0)) :pass-ok 0 :patch-queue (pen-patch/patch-cells (:leaks answer))))
          :again)
      :else
      (ctx/end c :nothing-possible (:reason answer) result))))

(def tree-endings ["_log" "_wood" "_leaves"])

(defn trees-in
  "The seen log, wood and leaf cells inside the box of cells, from its lowest fence cell to one above its highest."
  [c cells]
  (let [box (pen-read/barrier-box cells)
        ys (map (comp second :pos) (filter #(pen-read/barrier? (:want %)) cells))]
    (for [x (range (:x (:min box)) (inc (:x (:max box))))
          z (range (:z (:min box)) (inc (:z (:max box))))
          y (range (apply min ys) (+ 2 (apply max ys)))
          :let [n (u/seen-name (:primitives c) {:x x :y y :z z})]
          :when (and n (some #(str/ends-with? n %) tree-endings))]
      [x y z])))

(defn trunk-bases
  "The lowest seen log of each log column among the tree cells (a log whose cell below is no log), not yet in felled."
  [c cells felled]
  (let [name-at (fn [x y z] (u/seen-name (:primitives c) {:x x :y y :z z}))
        log? #(some-> % (str/ends-with? "_log"))]
    (remove (fn [[x y z :as pos]] (or (contains? felled pos) (log? (name-at x (dec y) z))))
            (filter (fn [[x y z]] (log? (name-at x y z))) cells))))

(defn ^:async trees-step!
  "While trees stand in the box: one jobs.forestry.fell-tree child per trunk base (each tried once), then a clear-cells
  child over the box clears the logs and leaves left. Whatever they end with, on to the build (a tree that stays shows
  as a leak in the check)."
  [c cells]
  (let [trees (trees-in c cells)
        felled (set (:felled (ctx/mem c)))]
    (if (empty? trees)
      (do (ctx/update-mem! c assoc :phase :tidy) :again)
      (if-let [[x y z :as base] (first (trunk-bases c trees felled))]
        (let [r (await (ctx/call c (keyword (str "fell-" x "-" y "-" z)) 'jobs.forestry.fell-tree
                                 (merge (assoc (select-keys (:args c) [:ignore-zones?]) :ways (need/pass-ways c))
                                        {:at {:x x :y y :z z} :for-plan (:plan (:args c)) :replant? false})))]
          (if (= :continue r)
            :continue
            (do (ctx/update-mem! c update :felled (fnil conj []) base) :again)))
        (let [box (pen-read/barrier-box cells)
              ys (map (comp second :pos) cells)
              r (await (ctx/call c :trees 'jobs.blocks.clear-cells
                                 (merge (select-keys (:args c) [:ignore-zones?])
                                        {:area {:min {:x (:x (:min box)) :y (apply min ys) :z (:z (:min box))}
                                                 :max {:x (:x (:max box)) :y (+ 1 (apply max ys)) :z (:z (:max box))}}
                                         :only tree-endings})))]
          (if (= :continue r)
            :continue
            (do (ctx/update-mem! c assoc :phase :tidy) :again)))))))

(def tidy-margin "Blocks around the box within which the body's own leftover blocks are taken up before the build." 4)

(defn ^:async tidy-step!
  "Take up the body's own scaffold-ledger blocks (a night shelter's cheap blocks stay after dawn) in and around the box
  with a jobs.access.cleanup child, so no animal steps from one onto a fence. Whatever it ends with, on to the build."
  [c cells]
  (let [{:keys [min max]} (pen-read/barrier-box cells)
        ys (map (comp second :pos) cells)
        mid (fn [a b] (/ (+ a b 1) 2))
        r (await (ctx/call c :tidy 'jobs.access.cleanup
                           (merge (select-keys (:args c) [:ignore-zones?])
                                  {:of :all
                                   :near {:pos {:x (mid (:x min) (:x max)) :y (mid (apply js/Math.min ys) (apply js/Math.max ys)) :z (mid (:z min) (:z max))}
                                          :radius (+ tidy-margin (js/Math.hypot (- (:x max) (:x min)) (- (:z max) (:z min)) (- (apply js/Math.max ys) (apply js/Math.min ys))))}})))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c assoc :phase :build) :again))))

(defn ^:async patch-step!
  "One fence of the patch queue by a blocks.place child (its ending, placed or not, passes it on); then back to the check."
  [c cells]
  (if-let [[x y z] (first (:patch-queue (ctx/mem c)))]
    (let [r (await (pen-patch/place-fence! c [x y z] (patch-item cells) {:for-plan (:plan (:args c)) :ways (need/pass-ways c)}))]
      (if (= :continue r)
        :continue
        (do (ctx/update-mem! c #(-> % (update :patch-queue rest) (update :pass-ok (fnil + 0) (if (= :done (:kind r)) 1 0)))) :again)))
    (do (ctx/update-mem! c assoc :phase :check) :again)))

(defn ^:async build-step!
  "One round of the builder as the child; the phase moves to the exit when its round ended with its data (done, or it
  left cells refused, given up or short: the check reports them as leaks); an ending of its conditions, or of a limit, is
  this job's, and so is a :no-items with nothing placed (the look round found the fences short)."
  [c]
  (let [r (await (ctx/call c :build 'jobs.build.from-plan (build-args c)))]
    (cond
      (or (= :continue r) (= :cut-off (:kind r))) :continue
      (and (= :condition-failed (:kind r)) (= :no-items (get-in r [:cause :reason])) (zero? (get-in r [:data :placed] 0)))
      (composite/stop c r)
      (or (= :done (:kind r)) (and (:data r) (#{:nothing-possible :condition-failed} (:kind r)))) (do (ctx/update-mem! c assoc :phase :exit :built (:data r))
                                                 :again)
      :else (composite/stop c r))))

(defn exit-cell
  "The column {:x :z} just outside the box beside a gate of cells (its ground may lie below the gate), or nil without a gate on the box's edge."
  [box cells]
  (first (for [{[x y z] :pos :keys [want]} cells
               :when (gate? want)
               [nx nz] [[(inc x) z] [(dec x) z] [x (inc z)] [x (dec z)]]
               :when (not (inside? box {:x nx :y y :z nz}))]
           {:x nx :z nz})))

(defn ^:async exit-step!
  "While the body is inside the box: walk out beside the gate. Then on to shutting the gate, or the check if the body
  was never inside. Nothing-possible :inside when it cannot get out."
  [c cells]
  (let [box (pen-read/barrier-box cells)]
    (if-not (inside? box (u/self-pos c))
      (do (ctx/update-mem! c assoc :phase (if (:exited (ctx/mem c)) :shut :check))
          :again)
      (if-let [out (exit-cell box cells)]
        (let [r (await (child/call-walk* c :exit {:pos out :range 0 :ways #{:walk :clear-seal}}))]
          (cond
            (= :continue r) :continue
            (= :done (:kind r)) (do (ctx/update-mem! c assoc :exited true) :again)
            :else (ctx/end c :nothing-possible {:reason :inside :why "the walk out of the pen failed"} {:built (:built (ctx/mem c))})))
        (ctx/end c :nothing-possible {:reason :inside :why "the pen has no gate to leave by"} {:built (:built (ctx/mem c))})))))

(defn ^:async shut-step!
  "Shut the plan's gates the walk out left open, then on to the check."
  [c]
  (let [r (await (ctx/call c :shut 'jobs.maintenance.shut {:plan (:plan (:args c))}))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c #(cond-> (assoc % :phase :check)
                                (not= :done (:kind r)) (assoc :shut (contract/cause-key r))))
          :again))))

(defn ^:async step [c]
  (let [{:keys [cells trouble]} (planned c)
        phase (or (:phase (ctx/mem c)) :trees)]
    (cond
      trouble :continue
      (= :trees phase) (await (trees-step! c cells))
      (= :tidy phase) (await (tidy-step! c cells))
      (= :exit phase) (await (exit-step! c cells))
      (= :patch phase) (await (patch-step! c cells))
      (= :shut phase) (await (shut-step! c))
      (= :check phase) (do (await (look/survey! c)) (await (finish! c cells))) ; cells behind the body are not seen until it looks
      :else (await (build-step! c)))))

(defn ^:async round
  "The whole attempt: build, walk out, shut the gate, check the pen, in steps (pace/steps!); :continue only while a child waits."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
