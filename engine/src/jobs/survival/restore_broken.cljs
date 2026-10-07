(ns jobs.survival.restore-broken
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach :as reach]
            [jobs.lib.result :as result]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(def doc
  "Put back what a job broke in another's zone or claim, and the holes a go-to escalation dug (the :tidy entries
  jobs.lib.tidy and jobs.movement.go-to write). A dug block is placed again from an item of the same name
  carried, or for an escalation hole what the block drops (cobblestone for stone). An escalation hole waits while
  the body is shut in (:shut-in), as it is the body's way on. A placed block is dug again. It digs nothing else.
  Works only when the body is safe: health at least :min-health and no hostile within :danger-radius.
  Touches only a cell whose block is still what the job left (a changed cell is somebody's, left alone),
  with at most 3 tries per cell.
  One run over every entry, paced between steps, a cell put back forgotten at once; a walk is one go-to call and
  a failed try is repeated until the cell gave up. A cell not restored for now (unsafe, item not carried) keeps its
  entry. The others are forgotten.
  Ends with one info tidy.restored (the cells put back, none if none were), :done when every cell was settled,
  else (also when max-passes ran out with cells unvisited) stopped :not-restored {:restored n :failed n}. Never :continue.
  If cells are left it also emits one warning tidy.not-restored {:cells [{:cell :was :why}]}.
  Memory: reads :tidy. Writes :tidy-reported, the cells still waiting, which the :tidy-pending trigger reads
  to warn once per set of cells.")

(a/defargs args
  {:min-health {:doc "least health to restore anything" :spec (a/num-in 0 20) :default 14}
   :danger-radius {:doc "a hostile this close postpones restoring" :spec (a/num-in 0 nil) :default 8}
   :reach {:doc "walk to within this many blocks of a cell" :spec (a/num-in 0 nil) :default 3}})

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

(defn seals-body?
  "Whether putting back the dug block of e would shut the body itself in: it has room to walk now and would not with
  the cell solid (its doorway, its stair)."
  [c {:keys [cell action]}]
  (and (= :dig action)
       (let [p (:primitives c)]
         (and (not (reach/enclosed? p)) (reach/enclosed? p #{(vec cell)})))))

(defn check [c]
  (or (boolean (seq (tidy/entries c)))
      (ctx/wait c {:reason :nothing-to-restore})))

(defn unsafe?
  "Whether the body should not be busy with other people's blocks now."
  [c args]
  (tidy/unsafe? (:primitives c) args))

(defn why-not
  "Why entry cannot be restored now (:changed :not-carried), or nil."
  [c e]
  (tidy/why-not (:primitives c) e))

(defn ^:async walk-to!
  "Walk to within reach of cell: true when it arrived, :continue when the walk yielded, false when it gave up."
  [c cell reach]
  (case (await (near/go-near! c (zipmap [:x :y :z] cell) reach {:escalate false :one-way :open}))
    :there true
    :partial :continue
    false))

(defn place-item
  "The carried item that puts back the dug cell: tidy/place-item of its entry (an escalation hole takes what the
  block drops), else was."
  [c cell was]
  (or (some->> (tidy/entries c) (filter #(= cell (:cell %))) first (tidy/place-item (:primitives c))) was))

(defn ^:async put-back!
  "Place the recorded block again, or dig the placed one. True when it worked."
  [c {:keys [cell action was] :as e}]
  (let [pos (zipmap [:x :y :z] cell)
        _ (when-not (= :dig action) (await (tools/equip-tool! c (:now e) {:fast true})))
        res (await (if (= :dig action)
                     (ctx/act c :place (clj->js {:pos pos :item (place-item c cell was)}))
                     (ctx/act c :dig (clj->js {:pos pos}))))
        _ (when-not (= :dig action) (await (tools/note-wear! c)))]
    (contains? #{"placed" "dug"} (.-status res))))

(defn ^:async restore-one!
  "One try at putting entry e back: a walk that yields (:continue) is no try (the next pass walks again); else the
  try is counted before the put-back (a cut run still used one), the cell is forgotten as soon as it is put back.
  A cell that failed is tried again on the next pass until it gave up."
  [c {:keys [cell tries] :as e} reach]
  (let [walked (await (walk-to! c cell reach))]
    (if (= :continue walked)
      :continue
      (do (tidy/count-try! c cell)
          (if (and walked (await (put-back! c e)))
            (do (tidy/forget-cell! c cell)
                (ctx/update-mem! c update :restored (fnil conj []) cell)
                (ctx/update-mem! c update :seen (fnil conj #{}) cell))
            (when (>= (inc tries) (tidy/max-tries))
              (skip! c e :gave-up)))))))

(defn report!
  "Forget the cells that are settled (changed, given up), emit the two events, end: :done, or stopped
  :not-restored while a cell was not put back (its entry kept unless it changed or was given up)."
  [c]
  (let [{:keys [restored failed seen]} (ctx/mem c)
        left (remove #(contains? (or seen #{}) (:cell %)) (tidy/entries c))
        unvisited (count left)
        not-restored (into (vec failed) (map (fn [{:keys [cell was]}] {:cell cell :was was :why :unvisited})) left)]
    (doseq [{:keys [cell why]} failed :when (#{:changed :gave-up} why)] (tidy/forget-cell! c cell))
    (when (seq restored)
      (ctx/emit! c :tidy.restored :info {:cells (vec restored) :text (str "restored " (count restored) " broken blocks")}))
    (when (seq not-restored)
      (ctx/emit! c :tidy.not-restored :warn {:cells not-restored :text (str (count not-restored) " broken blocks not restored")}))
    (ctx/remember! c :tidy-reported {:cells (mapv :cell (tidy/entries c))} tidy/reported-policy)
    (if (or (pos? unvisited) (seq (remove #(= :changed (:why %)) failed)))
      (result/stop! c :not-restored (str (+ unvisited (count failed)) " broken blocks not restored, " (count restored) " restored")
                    :restored (count restored) :failed (+ unvisited (count failed)))
      :done)))

(defn skip! [c {:keys [cell was]} why]
  (ctx/update-mem! c update :failed (fnil conj []) {:cell cell :was was :why why})
  (ctx/update-mem! c update :seen (fnil conj #{}) cell))

(defn ^:async step-clear!
  "The body's hitbox overlaps the cell of e: one go-to to a clear standable cell, or, with none or after a walk
  that did not clear it, skip e as :occupied (its entry stays for when the body is out). The cell is clear of every
  entry still waiting."
  [c e waiting]
  (let [{:keys [x y z]} (u/self-pos c)
        _ (await (dig-look/look-unknown! c (step-off/floors {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)} 2)))
        target (when-not (:stepped (ctx/mem c)) (clear-cell c e waiting))]
    (if-not target
      (skip! c e :occupied)
      (do (await (ctx/call-child c :go 'jobs.movement.go-to {:pos target :range 0 :escalate false}))
          (ctx/update-mem! c assoc :stepped true)))))

(def max-passes "Steps in one run before it stops." 100)

(defn ^:async pass!
  "One step over the next waiting entry: restore it, step clear of it, or skip it with a reason. Resolves :again,
  or :done when none is waiting."
  [c]
  (let [a (:args c)
        seen (:seen (ctx/mem c) #{})
        waiting (vec (remove #(contains? seen (:cell %)) (tidy/entries c)))
        e (or (first (filter #(= :occupied (why-not c %)) waiting))
              (first (remove #(seals-others? c % waiting) waiting))
              (first waiting))]
    (cond
      (nil? e) :done
      (unsafe? c a) (do (skip! c e :unsafe) :again)
      (= :occupied (why-not c e)) (do (await (step-clear! c e waiting)) :again)
      (why-not c e) (do (skip! c e (why-not c e)) :again)
      (tidy/unreachable? (:primitives c) e (:reach a)) (do (skip! c e :unreachable) :again)
      (>= (:tries e) (tidy/max-tries)) (do (skip! c e :gave-up) :again)
      (or (seals-body? c e) (seals-others? c e waiting)) (do (skip! c e :seals) :again)
      :else (do (await (restore-one! c e (:reach a))) :again))))

(defn ^:async round [c]
  (loop [i 0]
    (cond
      (not (ctx/alive? c)) :done
      (<= max-passes i) (report! c)
      (= :again (await (pass! c))) (do (await (pace/pace!)) (recur (inc i)))
      :else (report! c))))
