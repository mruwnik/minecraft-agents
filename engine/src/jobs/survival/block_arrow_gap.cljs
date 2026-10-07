(ns jobs.survival.block-arrow-gap
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.combat :as combat]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach :as reach]
            [jobs.lib.result :as r]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in :as dig-in]))

(def doc
  "Stop a ranged mob's arrows at a gap in the body's cover: a doorway, window or hole a skeleton (or the like) has a
  line of fire through. The body stays where it is and places blocks in the open cells near it, seen ones only, that cross the
  arrows' line (jobs.lib.reach/line-of-fire?: from the mob's eye to the body's eye and its middle), the cell nearest
  the body first, one that ends the line alone before one that blocks half of it. A mob that was only heard is
  taken at the rough spot its direction and band give (jobs.lib.reach/mob-pos). A door, gate or trapdoor standing
  open in the way is shut, not walled over (jobs.survival.dig-in/place-all!).
  Declines (waiting) with :no-ranged-danger when no ranged mob within :radius has a line of fire.
  With none of :blocks carried it fetches one (:fetch, default true; jobs.lib.fetch, jobs.items.obtain) before it stops :no-blocks.
  Ends done {:placed [cells]} once none has (also when the mob moved away meanwhile), else stopped :no-blocks (none
  carried that :blocks names, nothing fetched), :refused (every cell that would help is another's zone, claim or plan; :ignore-zones?
  lifts it), :no-gap (a line of fire stays and no open cell within :reach would end it), :place-failed or
  :no-progress (over max-steps rounds).
  Does not fight, flee or walk: respond-to-hostile and retreat own that.
  What it placed is what place-all! reports placed (the body trusts its own place, not a view that may lag).
  Filling a doorway can shut the body in: the blocks are the building blocks dig-in shelters with, and a shut-in body
  gets out as from its own shelter (go-to escalation digs a door, dig-in leave).
  Events: block-arrow-gap.closed (info), block_arrow_gap_failed (warning).")

(def args
  {:radius {:doc "ranged mobs within this many blocks count" :default 16}
   :reach {:doc "open cells within this many blocks of the body's feet may be filled" :default 3}
   :blocks {:doc "names of the blocks it may place" :default dig-in/shelter-blocks}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :fetch {:doc "get a missing block (jobs.lib.fetch): true, a set of kinds or a map of limits; false stops :no-blocks" :default true}})

(def max-steps "Rounds one call takes at most." 12)

(def ray-heights "Heights above the body's feet the arrows are aimed at." [reach/eye-height 0.9])

(defn ranged-dangers
  "The ranged mobs within radius with a line of fire to the body, nearest first."
  [c]
  (filterv combat/ranged? (reach/dangers (:primitives c) (:radius (:args c)) {:ranged-radius (:radius (:args c))} {})))

(defn clear-rays
  "How many of the two arrow rays from the mob at mob-pos to the body at body-pos cross no :solid cell of kind-at."
  [kind-at mob-pos body-pos]
  (let [from [(:x mob-pos) (+ (:y mob-pos) reach/eye-height) (:z mob-pos)]]
    (count (filter #(reach/ray-clear? kind-at from [(:x body-pos) (+ (:y body-pos) %) (:z body-pos)]) ray-heights))))

(defn candidates
  "The cells within `within` blocks of the feet cell, nearest the body first, bar its feet and head cells."
  [feet within]
  (let [{:keys [x y z]} feet]
    (->> (for [dx (range (- within) (inc within)) dz (range (- within) (inc within)) dy [-1 0 1 2 3]
               :when (and (<= (+ (* dx dx) (* dz dz)) (* within within)) (not (and (zero? dx) (zero? dz) (<= 0 dy 1))))]
           {:x (+ x dx) :y (+ y dy) :z (+ z dz)})
         (sort-by #(u/dist feet %)))))

(def faces [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn supported?
  "Whether a block can be placed at cell: one of its faces touches a solid block (solid-at? takes a cell; the job
  passes what the body has seen)."
  [solid-at? {:keys [x y z]}]
  (boolean (some (fn [[dx dy dz]] (solid-at? {:x (+ x dx) :y (+ y dy) :z (+ z dz)})) faces)))

(defn plug-cell
  "The open cell (arrow-kind :open of kind-at) in cells that cuts most of the mob's line of fire, the nearest to the
  body among equals, or nil when none cuts any. ok? (cell -> bool) leaves cells out."
  [kind-at cells mob-pos body-pos ok?]
  (let [now (clear-rays kind-at mob-pos body-pos)
        open (filter #(and (ok? %) (= :open (kind-at (:x %) (:y %) (:z %)))) cells)
        left (fn [cell] (clear-rays (fn [x y z] (if (and (= x (:x cell)) (= y (:y cell)) (= z (:z cell))) :solid (kind-at x y z)))
                                    mob-pos body-pos))
        scored (map (juxt identity left) open)
        best (first (sort-by second scored))]
    (when (and best (< (second best) now)) (first best))))

(defn seen-kind
  "A function (kind-at x y z): what the body has seen at that cell is to an arrow (:open or :solid), :unseen for a cell
  it never saw."
  [p]
  (fn [x y z]
    (let [b (look/seen-block p {:x x :y y :z z})]
      (if (or (nil? b) (:unknown b))
        :unseen
        (reach/arrow-kind-of #js {:name (:name b) :properties (clj->js (:properties b))})))))

(defn fail! [c reason text]
  (ctx/emit! c :block_arrow_gap_failed :warn {:reason reason :text text})
  (r/stop! c reason text))

(defn ^:async step
  "One cell: :done, :again, or :continue while it fetches a block."
  [c]
  (let [p (:primitives c)
        {:keys [blocks] within :reach} (:args c)
        dangers (ranged-dangers c)]
    (if (empty? dangers)
      (do (ctx/emit! c :block-arrow-gap.closed :info {:placed (vec (:placed (ctx/mem c))) :text "no arrow line to the body"})
          (r/finish! c {:placed (vec (:placed (ctx/mem c)))}))
      (let [kind-at (seen-kind p)
            in (access/rules-input c)
            skip (:skip (ctx/mem c) #{})
            seen-solid? #(let [b (look/seen-block p %)] (and b (not (:unknown b)) (sh/solid? (:name b))))
            allowed? #(and (not (contains? skip %)) (supported? seen-solid? %))
            permitted? #(not (access/trespass-refusal in :place %))
            mob (reach/mob-pos p (first dangers))
            body (u/pos-of (.-pos (.self p)))
            cells (candidates (sh/feet p) within)
            cell (plug-cell kind-at cells mob body #(and (allowed? %) (permitted? %)))]
        (cond
          (nil? (dig-in/pick c blocks)) (or (await (fetch/step! c 'jobs.survival.block-arrow-gap {:reason :need :any-of (vec blocks)}))
                                            (fail! c :no-blocks "no block to stop the arrows with"))
          (nil? cell) (if (plug-cell kind-at cells mob body allowed?)
                        (fail! c :refused "every cell that would stop the arrows is another's (zone, claim or plan)")
                        (fail! c :no-gap "no open cell within reach would stop the arrows"))
          :else (let [_ (fetch/settle! c)
                      status (await (dig-in/place-all! c blocks [cell]))]
                  (if (not= :ok status)
                    (fail! c :place-failed (str "cannot place at the gap: " status))
                    (do (ctx/update-mem! c update :skip (fnil conj #{}) cell)
                        :again))))))))

(defn check
  "A ranged mob with a line of fire to the body; a decline says why (ctx/wait): :no-ranged-danger."
  [c]
  (or (boolean (seq (ranged-dangers c))) (ctx/wait c {:reason :no-ranged-danger})))

(defn ^:async round
  "Fill the gap (see doc), one cell a step with a timer between."
  [c]
  (loop [i 0]
    (let [res (if (< i max-steps) (await (step c)) (fail! c :no-progress "the gap took too many steps"))]
      (if (= :again res)
        (do (await (pace/pace!)) (recur (inc i)))
        res))))
