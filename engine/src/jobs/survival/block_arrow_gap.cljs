(ns jobs.survival.block-arrow-gap
  (:require [jobs.lib.access.rules :as rules]
            [jobs.lib.ready :as ready]
            [jobs.lib.checks :as checks]
            [jobs.lib.choose :as choose]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.game :as game]
            [jobs.lib.blocks :as lb]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.combat :as combat]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.reach :as reach]
            [jobs.lib.seal :as seal]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.solid :as solid]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]))

(def doc
  "Stop a ranged mob's arrows at a gap in the body's cover: a doorway, window or hole a skeleton (or the like) has a
  line of fire through. The body stays where it is and places blocks in the open cells near it, seen ones only, that cross the
  arrows' line (jobs.lib.reach/line-of-fire?: from the mob's eye to the body's eye and its middle), the cell nearest
  the body first, one that ends the line alone before one that blocks half of it. A mob that was only heard is
  taken at the rough spot its direction and band give (jobs.lib.danger/mob-pos). A door, gate or trapdoor standing
  open in the way is shut, not walled over (jobs.survival.dig-in-cells/place-all!).
  Done at once when no ranged mob within :radius has a line of fire. With no cell to fill it looks around once (yielding
  :continue) before it ends :no-gap, for cells it has not seen yet.
  The blocks are the call's :use :blocks (default the carried shelter fillers, jobs.lib.choose/fillers).
  With none of them carried it fetches one (:fetch, default true; jobs.lib.need, jobs.items.obtain) before it ends :condition-failed :no-blocks.
  Ends :done with the data {:placed [cells]} once none has a line of fire (also when the mob moved away meanwhile), else
  :condition-failed :no-blocks (none carried, nothing fetched) or :refused (every cell that would help is another's zone,
  claim or plan; :ignore-zones? lifts it), or :nothing-possible :no-gap (a line of fire stays and no open cell within
  :reach would end it), :place-failed or :no-progress (over max-steps rounds).
  Does not fight, flee or walk: respond-to-hostile and retreat own that.
  What it placed is what place-all! reports placed (the body trusts its own place, not a view that may lag).
  Filling a doorway can shut the body in: the blocks are the building blocks dig-in shelters with, and a shut-in body
  gets out as from its own shelter (go-to escalation digs a door, dig-in leave).")

(a/defargs args
  {:radius {:doc "ranged mobs within this many blocks count" :spec (a/num-in 0 nil) :default 16}
   :reach {:doc "open cells within this many blocks of the body's feet may be filled" :spec (a/num-in 0 nil) :default 3}
   :ignore-zones? :shared
   :fetch {:doc "get a missing block (jobs.lib.need): true, a set of kinds or a map of limits; false stops :no-blocks" :spec need/option? :default true}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds stopping the arrows may take, a block fetch included." :spec (a/num-in 0 nil)}
   ::max-steps {:default 12 :doc "Rounds one call takes at most." :spec (a/int-in 0 nil)}})
(defn max-steps [] (settings/get settings ::max-steps))

(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s} :use {:blocks :shelter}})

(def meta {:category :phase :part-of '[jobs.survival.respond-to-hostile]})

(def safe? ready/ok)

(def fit? ready/ok)

(defn blocks
  "The blocks it may place: the call's :use :blocks, else (nothing carried when the call began) the shelter fillers."
  [c]
  (let [used (ctx/use c :blocks)]
    (if (seq used) used (choose/fillers :shelter))))

(def ray-heights "Heights above the body's feet the arrows are aimed at." [game/eye-height 0.9])

(defn ranged-dangers
  "The ranged mobs within radius with a line of fire to the body, nearest first."
  [c]
  (filterv combat/ranged? (danger-q/dangers (:primitives c) (:radius (:args c)) {:ranged-radius (:radius (:args c))} {})))

(defn clear-rays
  "How many of the two arrow rays from the mob at mob-pos to the body at body-pos cross no :solid cell of kind-at."
  [kind-at mob-pos body-pos]
  (let [from [(:x mob-pos) (+ (:y mob-pos) game/eye-height) (:z mob-pos)]]
    (count (filter #(reach/ray-clear? kind-at from [(:x body-pos) (+ (:y body-pos) %) (:z body-pos)]) ray-heights))))

(defn candidates
  "The cells within `within` blocks of the feet cell, nearest the body first, bar its feet and head cells."
  [feet within]
  (let [{:keys [x y z]} feet]
    (->> (for [dx (range (- within) (inc within)) dz (range (- within) (inc within)) dy [-1 0 1 2 3]
               :when (and (<= (+ (* dx dx) (* dz dz)) (* within within)) (not (and (zero? dx) (zero? dz) (<= 0 dy 1))))]
           {:x (+ x dx) :y (+ y dy) :z (+ z dz)})
         (sort-by #(u/dist feet %)))))

(def faces rules/neighbour-deltas)

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

(def failure-kinds {:no-blocks :condition-failed :refused :condition-failed})

(defn fail! [c reason text]
  (ctx/end c (get failure-kinds reason :nothing-possible) reason {:placed (vec (:placed (ctx/mem c))) :text text}))

(defn ^:async fetch-blocks!
  "None of blocks carried: get one through jobs.lib.need when :fetch allows (:again once got, :continue while it
  waits), else end :no-blocks, a failed fetch's ending as :because."
  [c blocks]
  (let [o (need/opts c 'jobs.survival.block-arrow-gap)
        e (when (contains? (:what o) :item) (await (need/blocks! c blocks 1 (select-keys o [:how :explore]))))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) :again
      :else (contract/with-because (fail! c :no-blocks "no block to stop the arrows with") e))))

(defn ^:async step
  "One cell: :done, :again, or :continue while it fetches a block."
  [c]
  (let [p (:primitives c)
        within (:reach (:args c))
        blocks (blocks c)
        dangers (ranged-dangers c)]
    (if (empty? dangers)
      (ctx/end c :done nil {:placed (vec (:placed (ctx/mem c)))})
      (let [kind-at (seen-kind p)
            in (access/rules-input c)
            skip (:skip (ctx/mem c) #{})
            seen-solid? #(let [b (look/seen-block p %)] (and b (not (:unknown b)) (solid/solid? (:name b))))
            allowed? #(and (not (contains? skip %)) (supported? seen-solid? %))
            permitted? #(not (access/trespass-refusal in :place %))
            mob (danger-q/mob-pos p (first dangers))
            body (u/pos-of (.-pos (.self p)))
            cells (candidates (sh-sense/feet p) within)
            cell (plug-cell kind-at cells mob body #(and (allowed? %) (permitted? %)))]
        (cond
          (nil? (lb/pick c blocks)) (await (fetch-blocks! c blocks))
          (and (nil? cell) (not (look/surveyed? c))) (await (look/look-around! c))
          (nil? cell) (if (plug-cell kind-at cells mob body allowed?)
                        (fail! c :refused "every cell that would stop the arrows is another's (zone, claim or plan)")
                        (fail! c :no-gap "no open cell within reach would stop the arrows"))
          :else (let [status (await (dig-cells/place-all! c blocks [cell]))]
                  (if (not= :ok status)
                    (fail! c :place-failed (str "cannot place at the gap: " status))
                    (do (ctx/update-mem! c update :skip (fnil conj #{}) cell)
                        :again))))))))


(defn done?
  "No ranged mob has a line of fire to the body; the cells placed so far."
  [c]
  (checks/goal (empty? (ranged-dangers c)) [:covered "no ranged mob has a line of fire"] [:in-fire "a ranged mob has a line of fire"]
               {:units :cells :done (count (:placed (ctx/mem c)))}))

(defn quote-of [_c] {:uses {:job-time [0 3 30]}})

(defn ^:async round
  "Fill the gap (see doc), one cell a step with a timer between."
  [c]
  (loop [i 0]
    (let [res (if (< i (max-steps)) (await (step c)) (fail! c :no-progress "the gap took too many steps"))]
      (if (= :again res)
        (do (await (pace/pace!)) (recur (inc i)))
        res))))
