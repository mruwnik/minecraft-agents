(ns jobs.survival.restore-broken
  (:require [engine.ctx :as ctx]
            [engine.jobs.reach :as reach]
            [engine.jobs.tidy :as tidy]
            [engine.jobs.util :as u]))

(def doc
  "Put back what a job broke in another's zone or claim (the :tidy entries engine.jobs.tidy writes).
  A dug block is placed again from an item of the same name carried. A placed block is dug again. It digs nothing else.
  Works only when the body is safe: health at least :min-health and no hostile within :danger-radius.
  Touches only a cell whose block is still what the job left (a changed cell is somebody's, left alone),
  with at most 3 tries per cell.
  A cell not restored for now (unsafe, item not carried) keeps its entry. The others are forgotten.
  Ends with one info tidy.restored (the cells put back, none if none were).
  If cells are left it also emits one warning tidy.not-restored {:cells [{:cell :was :why}]}.
  Memory: reads :tidy. Writes :tidy-reported, the cells still waiting, which the :tidy-pending trigger reads
  to warn once per set of cells.")

(def args
  {:min-health {:doc "least health to restore anything" :default 14}
   :danger-radius {:doc "a hostile this close postpones restoring" :default 8}
   :reach {:doc "walk to within this many blocks of a cell" :default 3}})

(declare skip!)

(def step-radius 4)

(defn halo-clear?
  "Whether a body standing at the middle of feet cell [x y z] (its 0.6 wide hitbox can reach into the neighbouring
  columns) is clear of every cell in reserved at feet and head height."
  [reserved [x y z]]
  (not-any? reserved (for [dx [-1 0 1] dz [-1 0 1] dy [0 1]] [(+ x dx) (+ y dy) (+ z dz)])))

(defn clear-cell
  "The nearest standable cell within step-radius of the body that the body can walk to, and where its hitbox is clear
  of the cell of entry e and of every entry of waiting (still to be restored), or nil."
  [c {:keys [cell]} waiting]
  (let [p (:primitives c)
        here (u/self-pos c)
        floor (fn [k] (js/Math.floor (k here)))
        [x y z] [(floor :x) (floor :y) (floor :z)]
        reserved (into #{cell} (map :cell) waiting)
        centre (fn [pos] (-> pos (update :x + 0.5) (update :z + 0.5)))
        cands (for [dx (range (- step-radius) (inc step-radius))
                    dz (range (- step-radius) (inc step-radius))
                    dy (range -1 3)
                    :let [pos {:x (+ x dx) :y (+ y dy) :z (+ z dz)}]
                    :when (halo-clear? reserved [(:x pos) (:y pos) (:z pos)])
                    :when (reach/standable-cell? p pos)]
                pos)]
    (->> cands
         (sort-by #(u/dist here (centre %)))
         (filter #(reach/walkable-way? p here %))
         first)))

(def max-others-checked 12)

(defn seals-others?
  "Whether putting back the dug block of e would shut the body off from another entry of waiting it can still reach
  now (the body must be able to walk next to a cell to place a block in it)."
  [c {:keys [cell action] :as e} waiting]
  (when (= :dig action)
    (let [p (:primitives c)
          here (u/self-pos c)
          others (->> waiting
                      (remove #(= cell (:cell %)))
                      (filter #(and (= :dig (:action %)) (nil? (tidy/why-not p %))))
                      (take max-others-checked))
          at (fn [o] (zipmap [:x :y :z] (:cell o)))]
      (boolean (some #(and (reach/walkable-way? p here (at %))
                           (not (reach/walkable-way? p here (at %) #{cell}))) others)))))

(defn check [c] (boolean (seq (tidy/entries c))))

(defn unsafe?
  "Whether the body should not be busy with other people's blocks now."
  [c args]
  (tidy/unsafe? (:primitives c) args))

(defn why-not
  "Why entry cannot be restored now (:changed :not-carried), or nil."
  [c e]
  (tidy/why-not (:primitives c) e))

(defn ^:async walk-near!
  "Walk to within reach of cell: :done, :continue (still walking) or :failed."
  [c cell reach]
  (let [r (await (ctx/call-child c :go 'jobs.movement.go-to {:pos (zipmap [:x :y :z] cell) :range reach :escalate false}))]
    (if (= :continue r) :continue (if (:arrived (ctx/child-result c :go)) :done :failed))))

(defn ^:async put-back!
  "Place the recorded block again, or dig the placed one. True when it worked."
  [c {:keys [cell action was]}]
  (let [pos (zipmap [:x :y :z] cell)
        res (await (if (= :dig action)
                     (ctx/act c :place (clj->js {:pos pos :item was}))
                     (ctx/act c :dig (clj->js {:pos pos}))))]
    (contains? #{"placed" "dug"} (.-status res))))

(defn ^:async restore-one!
  "One step towards putting entry e back: :continue while walking, else :next once it was tried."
  [c {:keys [cell tries] :as e} reach]
  (let [w (await (walk-near! c cell reach))]
    (when-not (= :continue w)
      (tidy/count-try! c cell)
      (if (and (= :done w) (await (put-back! c e)))
        (do (ctx/update-mem! c update :restored (fnil conj []) cell)
            (ctx/update-mem! c update :seen (fnil conj #{}) cell))
        (when (>= (inc tries) tidy/max-tries)
          (skip! c e :gave-up))))
    :continue))

(defn report!
  "Forget the cells that are settled (restored, changed, given up), emit the two events, end."
  [c]
  (let [{:keys [restored failed]} (ctx/mem c)]
    (doseq [cell restored] (tidy/forget-cell! c cell))
    (doseq [{:keys [cell why]} failed :when (#{:changed :gave-up} why)] (tidy/forget-cell! c cell))
    (when (seq restored)
      (ctx/emit! c :tidy.restored :info {:cells (vec restored) :text (str "restored " (count restored) " broken blocks")}))
    (when (seq failed)
      (ctx/emit! c :tidy.not-restored :warn {:cells (vec failed) :text (str (count failed) " broken blocks not restored")}))
    (ctx/remember! c :tidy-reported {:cells (mapv :cell (tidy/entries c))} tidy/reported-policy)
    :done))

(defn skip! [c {:keys [cell was]} why]
  (ctx/update-mem! c update :failed (fnil conj []) {:cell cell :was was :why why})
  (ctx/update-mem! c update :seen (fnil conj #{}) cell))

(defn ^:async step-clear!
  "The body's hitbox overlaps the cell of e: walk to a clear standable cell (:continue), or, with none or after a walk
  that did not clear it, skip e as :occupied (its entry stays for when the body is out). The cell is clear of every
  entry still waiting."
  [c e waiting]
  (let [target (when-not (:stepped (ctx/mem c)) (clear-cell c e waiting))]
    (if-not target
      (do (skip! c e :occupied) :continue)
      (let [r (await (ctx/call-child c :go 'jobs.movement.go-to {:pos target :range 0 :escalate false}))]
        (when-not (= :continue r) (ctx/update-mem! c assoc :stepped true))
        :continue))))

(defn ^:async round [c]
  (let [a (:args c)
        seen (:seen (ctx/mem c) #{})
        waiting (vec (remove #(contains? seen (:cell %)) (tidy/entries c)))
        e (or (first (filter #(= :occupied (why-not c %)) waiting))
              (first (remove #(seals-others? c % waiting) waiting))
              (first waiting))]
    (cond
      (nil? e) (report! c)
      (unsafe? c a) (do (skip! c e :unsafe) :continue)
      (= :occupied (why-not c e)) (await (step-clear! c e waiting))
      (why-not c e) (let [why (why-not c e)]
                      (skip! c e why)
                      :continue)
      (tidy/unreachable? (:primitives c) e (:reach a)) (do (skip! c e :unreachable) :continue)
      (>= (:tries e) tidy/max-tries) (do (skip! c e :gave-up) :continue)
      (seals-others? c e waiting) (do (skip! c e :seals) :continue)
      :else (await (restore-one! c e (:reach a))))))
