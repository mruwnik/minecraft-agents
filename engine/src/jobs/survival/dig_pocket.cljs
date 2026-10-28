(ns jobs.survival.dig-pocket
  (:require [jobs.lib.cost.health :as cost-health]
            [engine.args :as a]
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
            [jobs.survival.dig-pit-site :as pit-site]
            [jobs.survival.plug-way :as way]
            [jobs.survival.pit-cells :as pit]
            [jobs.survival.respond.state :as flight]))

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
  With :for :night on sand or gravel, where no wall is in sight, it first digs a shaft down by hand (a pit's zigzag, at most ::sink-depth blocks) until a pocket fits in the rock under the loose ground; sand or gravel it carries then seals the way in (a pocket whose floor holds it), cutting the rock by hand.
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
   ::fetch-s {:default 60 :doc "Seconds fetching a tool is guessed to take, for the quote." :spec (a/num-in 0 nil)}
   ::sink-depth {:default 8 :doc "Blocks down a night shaft in sand or gravel goes looking for rock to pocket into." :spec (a/int-in 1 nil)}
   ::sink-s {:default 40 :doc "Seconds a night shaft in sand is guessed to take, for the quote." :spec (a/num-in 0 nil)}})

(def target [])

(def ways need/fetch-ways)

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s} :ways #{:carried}})

(defn done? [c] (way/done? c :pocket))

(def stand-offsets
  "[dx dy dz] of the cells within walk radius r (dy -3 to 3) but the feet, nearest first."
  (memoize (fn [r]
             (->> (for [dx (range (- r) (inc r)) dz (range (- r) (inc r)) dy [0 -1 1 -2 2 -3 3]
                        :when (and (not= [0 0 0] [dx dy dz]) (<= (+ (* dx dx) (* dz dz)) (* r r)))]
                    [dx dy dz])
                  (sort-by (fn [[dx dy dz]] (js/Math.hypot dx dy dz)))
                  vec))))

(defn stand-cells
  "The cells within ::walk-radius (dy -3 to 3) of feet, nearest first, that the body sees open (feet and head over a solid floor)
  and hold no hostile: where a pocket may be dug from."
  [c feet]
  (let [p (:primitives c)
        mobs (pit/hostiles c)]
    (->> (stand-offsets (int (settings/get settings ::walk-radius)))
         (map (fn [[dx dy dz]] (assoc feet :x (+ (:x feet) dx) :y (+ (:y feet) dy) :z (+ (:z feet) dz))))
         (filter #(and (dig-cells/open-cell? p %) (dig-cells/open-cell? p (update % :y inc))
                       (dig-cells/known-solid? p (update % :y dec)) (not (mobs %)))))))

(defn fetch-tool?
  "Whether the call's ways let the job get a tool."
  [c]
  (some? (need/fetch-opts c)))

(defn sink-site?
  "Whether a night shaft may be dug (on) here: the feet stand on sand or gravel, whose walls would fall, or in the shaft this call began."
  [c]
  (let [p (:primitives c)]
    (and (= :night (:for (:args c)))
         (or (:sink (ctx/mem c))
             (contains? dig-look/loose-ground (u/seen-name p (update (sh-sense/feet p) :y dec)))))))

(defn site
  "Where the pocket is dug from: the feet, else the nearest of stand-cells with a pocket side (pit/pocket-site), one that digs
  no other's wall first: {:stand cell :fetch? true when no carried tool harvests its wall and one may be fetched}; with
  no site {:no-tool? true} when only a wall no carried tool harvests is left and no fetch is allowed, {:sink? true} when a night
  shaft down may find rock, else {}; :cells the pocket's (pit/pocket-site at :stand, by hand with :fetch?). One pass for
  both: a side a carried tool digs is one the hand digs."
  [c]
  (let [feet (sh-sense/feet (:primitives c))
        by-hand? (:by-hand (ctx/mem c))
        ;; the zone rules are read only once a side fits: on open ground none does
        in (delay (access/rules-input c))
        own-in (delay (access/rules-input c {:spare-own? true}))
        blocks (flight/blocks c)
        refused? (fn [cells] (some #(access/trespass-refusal @in :dig %) cells))
        sites (volatile! {})
        sides (fn [cell] (let [s (pit/pocket-sites c cell own-in blocks)]
                           (vswap! sites assoc cell s)
                           (when-let [hand (:hand s)] {:hand hand :carried (if by-hand? hand (:carried s))})))
        here (sides feet)
        ;; {kind [cell refused?]}: the first cell of each kind, one not refused replacing a refused one
        better (fn [found kind cell cells]
                 (let [[_ was-refused?] (found kind)]
                   (cond (nil? cells) found
                         (nil? (found kind)) (assoc found kind [cell (refused? cells)])
                         (and was-refused? (not (refused? cells))) (assoc found kind [cell false])
                         :else found)))
        found (if (:carried here)
                {:carried [feet false]}
                (reduce (fn [found cell]
                          (let [s (sides cell)
                                found (-> found (better :carried cell (:carried s)) (better :hand cell (:hand s)))]
                            (if (= false (second (:carried found))) (reduced found) found)))
                        (if (:hand here) {:hand [feet false]} {})
                        (stand-cells c feet)))]
    (cond
      (:carried found) (let [stand (first (:carried found))] {:stand stand :cells (:carried (@sites stand))})
      (and (:hand found) (not by-hand?)) (if (fetch-tool? c)
                                           (let [stand (first (:hand found))] {:stand stand :fetch? true :cells (:hand (@sites stand))})
                                           {:no-tool? true})
      (sink-site? c) {:sink? true}
      :else {})))

(defn plan
  "{:feet :stand :cells (the pocket's dug cells, nil with no site) :sink? :open-s (the walk, the dig and a tool fetch: seconds before the way in can be shut)}."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        {:keys [stand fetch? sink? cells]} (site c)]
    {:feet feet :stand stand :cells cells :sink? sink?
     :open-s (when cells
               (+ (* (u/dist feet stand) (settings/get settings ::walk-s-per-block))
                  (/ (reduce + (map #(tools/clear-ms p (pit-site/priced-name p %)) cells)) 1000)
                  (if fetch? (settings/get settings ::fetch-s) 0)))}))

(defn quote-of
  "Walk to the standing cell, the dig of the pocket's two cells by the best carried tool (the hand when a tool is to be fetched), one block to seal the way in.
  pl: its plan (default made here)."
  ([c] (quote-of c (plan c)))
  ([c {:keys [feet cells sink? open-s]}]
   (let [seconds (if cells
                   (+ open-s (way/place-s c) (way/zone-penalty c {:dig cells :plug [feet]}))
                   (if sink? (+ (settings/get settings ::sink-s) (way/place-s c)) ##Inf))]
     (way/quote-of c seconds (settings/get settings ::job-time-s)))))

(defn exposed-s
  "Seconds the body stays open to a shooter before the pocket shuts it in: the walk and the dig. ##Inf with no site. pl: its plan (default made here)."
  ([c] (exposed-s c (plan c)))
  ([c {:keys [cells sink? open-s]}]
   (cond
     cells open-s
     sink? (settings/get settings ::sink-s)
     :else ##Inf)))

(defn fails
  "The chance the pocket lays itself open through a cell round it the body never sensed (pit-cells/shell-fails); 0 with
  no site. pl: its plan (default made here)."
  ([c] (fails c (plan c)))
  ([c {:keys [stand cells]}]
   (if cells (pit/shell-fails (:primitives c) cells (set (into cells [stand (update stand :y inc)]))) 0)))

(defn cover
  "[quote exposed-s fails] from one plan."
  [c]
  (let [pl (plan c)] [(quote-of c pl) (exposed-s c pl) (fails c pl)]))

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
  (let [e (await (ctx/call c :walk 'jobs.movement.go-to {:pos stand :range 0 :ways #{:walk :clear-seal} :floor cost-health/spend-down}))]
    (cond (= :continue e) :continue
          (not= :done (:kind e)) (fail! c :walk-failed "could not walk to a wall to pocket")
          (not (pit/pocket-site c (sh-sense/feet (:primitives c)))) (fail! c :walk-failed "walked to a wall but no side fits there")
          :else :again)))

(defn sink-plan
  "The refuge pit-cells/pit-round! digs for a shaft down from feet: a zigzag over the first open side, ::sink-depth deep, with nothing to plug."
  [c feet]
  (let [p (:primitives c)
        side (first (filter (fn [[dx dz]] (dig-cells/open-cell? p (assoc feet :x (+ (:x feet) dx) :z (+ (:z feet) dz)))) dig-cells/sides))
        {:keys [steps]} (dig-cells/pit-steps feet side (settings/get settings ::sink-depth))]
    (when side {:kind :sink :sink true :anchor feet :steps steps :plugs [] :roof (update feet :y inc) :cells (pit/plan-cells {:steps steps})})))

(defn ^:async sink!
  "One step of the night shaft down (pit-cells/pit-round!, no roof to plug): :again, or the ending when it is refused, a dig fails or it is deep enough with no rock to pocket into."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        kept (:sink (ctx/mem c))
        plan (if (and kept (pit/pit-index kept feet)) kept (sink-plan c feet))]
    (ctx/update-mem! c assoc :loose-plug true :by-hand true :sink plan)
    (cond
      (nil? plan) (fail! c :no-site "no open side to start a shaft down")
      (= (count (:steps plan)) (pit/pit-index plan feet)) (fail! c :no-site "no rock to pocket into within the shaft's depth")
      :else
      (let [in (access/rules-input c)
            _ (access/trespass! c "shut-in" (some #(access/trespass-refusal in :dig %) (mapcat :dig (:steps plan))))
            r (await (pit/pit-round! c plan))]
        (if (:stop r)
          (fail! c (:stop r) (:text r))
          :again)))))

(defn ^:async approach!
  "Get to a standing cell where the pocket can be dug: nil once there, :again to go on, :continue while a walk or fetch waits, or the
  ending when there is no site or the walk fails."
  [c]
  (let [{:keys [stand fetch? no-tool? sink?]} (site c)
        feet (sh-sense/feet (:primitives c))]
    (cond
      no-tool? (fail! c :no-tool "no tool for the wall to pocket into")
      (and (nil? stand) (not (look/surveyed? c))) (await (look/look-around! c))
      (and (nil? stand) sink?) (await (sink! c))
      (nil? stand) (fail! c :no-site "no wall in sight that a pocket fits into")
      fetch? (await (fetch! c (pit/pocket-site c stand true)))
      (not= stand feet) (await (walk! c stand)))))

(defn ^:async round [c]
  (loop []
    (let [r (when-not (way/enclosed? c) (await (approach! c)))]
      (if (= :again r)
        (recur)
        (or r (await (way/run! c :pocket step!)))))))
