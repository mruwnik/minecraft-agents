(ns jobs.survival.restore-broken
  (:require [engine.ctx :as ctx]
            [engine.jobs.tidy :as tidy]
            [engine.jobs.util :as u]
            [jobs.survival.extinguish :as extinguish]))

(def doc
  "Put back what a job broke in another's zone or claim (the :tidy entries engine.jobs.tidy writes). A dug block is
  placed again from the item of the same name carried; a placed block is dug again. Only when the body is safe
  (health at least :min-health, no hostile within :danger-radius), only a cell whose block is still what the job left
  (a changed cell is somebody's, left alone), and at most 3 tries per cell. Digs nothing else. Ends with one info
  tidy.restored (the cells put back; none when none were) and, only when a cell is left, one warn tidy.not-restored {:cells [{:cell :was :why}]}; a cell that is
  not restored for now (unsafe, item not carried) keeps its entry, the others are forgotten. At its end it remembers the cells still waiting (:tidy-reported), which
  the trigger :tidy-pending reads to warn once per set of cells.")

(def args
  {:min-health {:doc "least health to restore anything" :default 14}
   :danger-radius {:doc "a hostile this close postpones restoring" :default 8}
   :reach {:doc "walk to within this many blocks of a cell" :default 3}})

(declare skip!)

(def step-radius 4)

(defn clear-cell
  "The nearest standable cell within step-radius of the body whose feet and head are not the cell of entry e, or nil."
  [c {:keys [cell]}]
  (let [p (:primitives c)
        here (u/self-pos c)
        floor (fn [k] (js/Math.floor (k here)))
        [x y z] [(floor :x) (floor :y) (floor :z)]
        cands (for [dx (range (- step-radius) (inc step-radius))
                    dz (range (- step-radius) (inc step-radius))
                    dy (range -1 3)
                    :let [pos {:x (+ x dx) :y (+ y dy) :z (+ z dz)}
                          feet [(:x pos) (:y pos) (:z pos)]]
                    :when (not (#{feet (update feet 1 inc)} cell))
                    :when (extinguish/standable? p pos)]
                pos)]
    (when (seq cands) (apply min-key #(u/dist here %) cands))))

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
  (let [r (await (ctx/call-child c :go 'jobs.movement.go-to {:pos (zipmap [:x :y :z] cell) :range reach}))]
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
  "The body stands in the cell of e: walk to a clear standable cell (:continue), or, with none or after a walk that
  did not clear it, skip e as :occupied (its entry stays for when the body is out)."
  [c e]
  (let [target (when-not (:stepped (ctx/mem c)) (clear-cell c e))]
    (if-not target
      (do (skip! c e :occupied) :continue)
      (let [r (await (ctx/call-child c :go 'jobs.movement.go-to {:pos target :range 0}))]
        (when-not (= :continue r) (ctx/update-mem! c assoc :stepped true))
        :continue))))

(defn ^:async round [c]
  (let [a (:args c)
        seen (:seen (ctx/mem c) #{})
        e (first (remove #(contains? seen (:cell %)) (tidy/entries c)))]
    (cond
      (nil? e) (report! c)
      (unsafe? c a) (do (skip! c e :unsafe) :continue)
      (= :occupied (why-not c e)) (await (step-clear! c e))
      (why-not c e) (let [why (why-not c e)]
                      (skip! c e why)
                      :continue)
      (>= (:tries e) tidy/max-tries) (do (skip! c e :gave-up) :continue)
      :else (await (restore-one! c e (:reach a))))))
