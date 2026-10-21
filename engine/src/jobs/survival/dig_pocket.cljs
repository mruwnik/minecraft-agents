(ns jobs.survival.dig-pocket
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.dig-look :as dig-look]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-pit :as pit-quote]
            [jobs.survival.plug-way :as way]
            [jobs.survival.pit-cells :as pit]))

(def doc
  "A way of jobs.survival.shut-in: dig a pocket into a wall beside the body (feet and head cell, depth 1), step in and wall the way in
  with the dug blocks. One wall test: both cells natural, unprotected, not a falling block (sand, gravel) and harvested by a carried tool,
  nothing falling over the roof, rock all round but the way in, no fluid in or beside them, except the water of the way in when the body
  stands in water (it digs from there and the water that runs in drains once the way is shut). Needs a block to wall with, carried or
  dug. One call is a whole attempt: :done {:pos :way :pocket} once enclosed, at once when it already is; nothing-possible :failed
  (:reason) with no pocket that fits (:no-site), no tool for the wall (:no-tool), a dig that fails, a way in that cannot be walled, or a
  walk that fails. A pocket that lays open fluid or a cave is dropped (lava is filled first, :on-lava :seal).
  Where no side fits at the feet it first walks (go-to) to the nearest standing cell it has seen within the ::walk-radius setting (6, dy -3
  to 3) from where one does, not to a cell a hostile stands on, a wall that is no other's first, never one of the body's own plans.
  A wall no carried tool harvests: with ways beyond :carried (jobs.lib.need) it gets a tool first, and cuts bare-handed when none comes (slow, the drops
  lost); without, that wall is no site. The walls go to the scaffold ledger with :for (default :danger, ledger :retreat-plug, taken
  back by jobs.access.cleanup once no hostile is near; :night: ledger :shelter, left alone). Quote: time by the walk (##Inf when no side
  fits). Picked by jobs.survival.shut-in, which shuts the body in by the cheapest way.")

(a/defargs args
  {:ignore-zones? :shared
   :for {:doc "why the body shuts itself in: :danger (blocks are taken back by the cleanup once no hostile is near; a hostile in a cell fails it), :night (blocks stay for the night; a mob in a cell is waited out)" :spec #{:danger :night} :default :danger}
   :trespass? {:doc "false: a pocket whose walls are another's zone, claim or plan is no site (the caller tries again with true as a last resort)" :spec boolean? :default true}
   :on-lava {:doc ":seal: lava a dig lays open is filled with a carried block; :stop: the pocket is dropped and the lava left" :spec #{:seal :stop} :default :seal}})

(a/defargs settings
  {::walk-radius {:default 6 :doc "Blocks, to either side and 3 up or down, within which the body walks to a standing cell with a wall to pocket when none fits where it stands (0: never walks; at most 32, the body sees no farther and the scan reads every cell of the square)." :spec (a/num-in 0 32)}
   ::job-time-s {:default 60 :doc "Seconds a pocket attempt may take." :spec (a/num-in 0 nil)}
   ::walk-s-per-block {:default 0.3 :doc "Seconds a block of walk to a pocket's standing cell is guessed to take, for the quote." :spec (a/num-in 0 nil)}
   ::fetch-s {:default 60 :doc "Seconds fetching a tool is guessed to take, for the quote." :spec (a/num-in 0 nil)}})

(def target [])

(def ways need/fetch-ways)

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s} :ways #{:carried}})

(defn done? [c] (way/done? c :pocket))

(defn side-here? [c feet by-hand?] (boolean (pit/pocket-site c feet by-hand?)))

(defn stand-cell
  "The nearest cell within ::walk-radius (dy -3 to 3) that the body sees open (feet and head over a solid floor), holds no hostile
  and has a pocket side (pocket-site), one that digs no other's wall first; nil when none."
  [c feet by-hand?]
  (let [p (:primitives c)
        r (int (settings/get settings ::walk-radius))
        mobs (pit/hostiles c)
        in (access/rules-input c)
        ok? (fn [cell]
              (and (dig-cells/open-cell? p cell) (dig-cells/open-cell? p (update cell :y inc))
                   (dig-cells/known-solid? p (update cell :y dec)) (not (mobs cell))
                   (side-here? c cell by-hand?)))
        refused? (fn [cell] (some #(access/trespass-refusal in :dig %) (pit/pocket-site c cell by-hand?)))]
    (->> (for [dx (range (- r) (inc r)) dz (range (- r) (inc r)) dy [0 -1 1 -2 2 -3 3]
               :let [cell (assoc feet :x (+ (:x feet) dx) :y (+ (:y feet) dy) :z (+ (:z feet) dz))]
               :when (and (not= cell feet) (<= (+ (* dx dx) (* dz dz)) (* r r)))]
           cell)
         (sort-by #(u/dist feet %))
         (filter ok?)
         (reduce (fn [fallback cell] (if (refused? cell) (or fallback cell) (reduced cell))) nil))))

(defn fetch-tool?
  "Whether the call's ways let the job get a tool."
  [c]
  (some? (need/fetch-opts c)))

(defn site
  "Where the pocket is dug from: {:stand cell :fetch? true when no carried tool harvests its wall and one may be fetched}; with
  no site {:no-tool? true} when only a wall no carried tool harvests is left and no fetch is allowed, else {}."
  [c]
  (let [feet (sh-sense/feet (:primitives c))
        by-hand? (:by-hand (ctx/mem c))
        find (fn [hand?] (if (side-here? c feet hand?) {:stand feet} (when-let [stand (stand-cell c feet hand?)] {:stand stand})))]
    (or (find by-hand?)
        (when-let [s (and (not by-hand?) (find true))]
          (if (fetch-tool? c) (assoc s :fetch? true) {:no-tool? true}))
        {})))

(defn quote-of
  "Walk to the standing cell, the dig of the pocket's two cells by the best carried tool (the hand when a tool is to be fetched), one block to seal the way in."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        {:keys [stand fetch?]} (site c)
        cells (when stand (pit/pocket-site c stand (boolean fetch?)))
        seconds (if (and stand cells)
                  (+ (* (u/dist feet stand) (settings/get settings ::walk-s-per-block))
                     (/ (reduce + (map #(tools/clear-ms p (pit-quote/priced-name p %)) cells)) 1000)
                     (way/place-s c)
                     (way/zone-penalty c {:dig cells :plug [feet]})
                     (if fetch? (settings/get settings ::fetch-s) 0))
                  ##Inf)]
    (way/quote-of c seconds (settings/get settings ::job-time-s))))

(defn ^:async step!
  "A pocket dug once (pit-cells/pocket!: it steps in), then walls on its way in: :done, :again, or nil with none that fits
  or one that cannot be walled."
  [c]
  (if (:dug (ctx/mem c))
    (await (way/walls-step! c))
    (when (await (pit/pocket! c))
      (ctx/update-mem! c assoc :dug true)
      :again)))

(defn can-start?
  "A wall to pocket into that a carried tool harvests, or the ways let it get one."
  [c]
  (if (:no-tool? (site c))
    (contract/later :no-tool "no tool for the wall to pocket into")
    (contract/ok :tool "a tool for the wall, or none needed")))

(def safe? ready/ok)
(def fit? ready/ok)

(defn fail! [c reason text]
  (ctx/end c :nothing-possible reason {:text text :reason reason}))

(defn ^:async fetch!
  "The tool for the wall of cells (jobs.lib.need): :continue while it works, :again once it came; none came: the pocket is cut by
  hand from now on."
  [c cells]
  (let [p (:primitives c)
        o (need/fetch-opts c)
        block (some #(let [n (dig-look/rock-name p %)] (when-not (tools/can-harvest? p n) n)) cells)
        e (await (need/tool! c {:block block} o))]
    (cond (= :continue e) :continue
          (= :done (:kind e)) :again
          :else (do (ctx/update-mem! c assoc :by-hand true) :again))))

(defn ^:async walk!
  "The walk to the pocket's standing cell once: :again to go on, :continue while it works, or the ending of a walk that failed."
  [c stand]
  (let [e (await (ctx/call c :walk 'jobs.movement.go-to {:pos stand :range 0 :ways #{:walk :clear-seal} :floor {:health 1}}))]
    (cond (= :continue e) :continue
          (not= :done (:kind e)) (fail! c :walk-failed "could not walk to a wall to pocket")
          (not (side-here? c (sh-sense/feet (:primitives c)) (:by-hand (ctx/mem c)))) (fail! c :walk-failed "walked to a wall but no side fits there")
          :else :again)))

(defn ^:async approach!
  "Get to a standing cell where the pocket can be dug: nil once there, :again to go on, :continue while a walk or fetch waits, or the
  ending when there is no site or the walk fails."
  [c]
  (let [{:keys [stand fetch? no-tool?]} (site c)
        feet (sh-sense/feet (:primitives c))]
    (cond
      no-tool? (fail! c :no-tool "no tool for the wall to pocket into")
      (and (nil? stand) (not (look/surveyed? c))) (await (look/look-around! c))
      (nil? stand) (fail! c :no-site "no wall in sight that a pocket fits into")
      fetch? (await (fetch! c (pit/pocket-site c stand true)))
      (not= stand feet) (await (walk! c stand)))))

(defn ^:async round [c]
  (loop []
    (let [r (when-not (way/enclosed? c) (await (approach! c)))]
      (if (= :again r)
        (recur)
        (or r (await (way/run! c :pocket step!)))))))
