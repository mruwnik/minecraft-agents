(ns jobs.farm.tidy
  (:require [clojure.string :as str]
            [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.tools :as tools]
            [engine.jobs.util :as u]
            [plan.shape :as shape]))

(def doc
  "Dig the stray blocks over a plan of the body's world (:plan, optionally only its :part) and pick the drops
  up. Bodies only read plans; the plan does not say what a stray is, this job does. Over the cells the plan names:
  a cell it wants :clear holds a stray when anything is in it; a crop cell (want {:crop c}) holds one when it holds
  anything but that crop (weeds, saplings, leaves, stone, dirt ...); and the one cell of air above every crop cell
  that the plan does not itself name counts like a crop cell. NEVER dug, only listed: the planned crop itself (ripe
  or not) and a block the plan wants; water and lava; farmland where a crop cell or its air is wanted clear; a
  container or workstation (anything with an inventory), a bed, sign, banner or head, and any light, where the plan
  wants something else (result :kept {:pos :block :why :container|:owned|:light|:fluid}); a block in a cell that
  wants another block (farmland, water, a fence ...) and a crop of another kind in a crop cell (result :wrong
  {:pos :found :want}, warn tidy.wrong): fixing them is a build, till or plant job's work, and a cell that wants
  water is never dug, so the plan's own water is never drained. Cells nobody has loaded are no strays.
  Every dig goes through engine.access.rules/may-dig? with the zones and the footprints of all OTHER active plans,
  asked when the cell is chosen and again right before the dig. :footprint, :zone and a cell that cannot be dug are
  refused at once; a hazard the dig reports and :accept does not name defers the cell until nothing else is left,
  then it is tried :give-up times (the body may have moved) and refused as :hazard. A cell that cannot be walked to
  or reached :give-up times is refused as :unreachable. Refused cells are the result's :refused [{:pos :block
  :reason ...}] and the warn tidy.refused. The sweep goes top-down, nearest first, and ends by collecting the
  drops (jobs.forestry.collect-drops, everything within the field's extent plus 4 blocks, as a child). Ends with a
  result {:dug n :collected n :kept [...] :wrong [...] :refused [...]} and the info tidy.done; convergent: over a
  tidy field it digs nothing and says \"nothing to dig\". The check declines, with one tidy.declined warn naming
  the plan and the reason, while the plan is missing, unreadable or has no cells (in :part), and while
  no zone list has been read (nil zones never mean no zones).")

(def args
  {:plan {:doc "id of a plan of the body's world" :default nil}
   :part {:doc "only the cells of this part (and the air above its crop cells)" :default nil}
   :accept {:doc "dig hazards accepted: :fluid-adjacent (water beside; lava beside is :lava-adjacent and is not accepted by default), :falling-block, :under-feet"
            :default #{:fluid-adjacent}}
   :reach {:doc "cells whose centre is this close to the eye are dug without walking, in blocks" :default 4.2}
   :give-up {:doc "failed walks, failed digs or hazard-blocked tries after which a cell is refused" :default 3}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def eye-height 1.62)

;; ------------------------------------------------------------------ access

(defn access-world
  "The social half of the rules' input, read now (engine.jobs.access/zone-input): zones nil when the zone file was never
  read, claims, footprints of every plan but this one, the body's name, the clock and the job's :ignore-zones? arg."
  [c]
  (access/zone-input c {:except (:plan (:args c)) :ignore-zones? (:ignore-zones? (:args c))}))

;; ------------------------------------------------------------------ the rule, pure

(def fluids #{"water" "lava" "bubble_column"})

(def lights
  #"(^|_)(torch|lantern|candle)$|^(campfire|soul_campfire|end_rod|sea_lantern|glowstone|shroomlight|jack_o_lantern)$")

(def containers
  #{"chest" "trapped_chest" "ender_chest" "barrel" "hopper" "dispenser" "dropper" "furnace" "blast_furnace" "smoker"
    "crafting_table" "composter" "brewing_stand" "enchanting_table" "cauldron" "water_cauldron" "grindstone" "loom"
    "smithing_table" "stonecutter" "cartography_table" "fletching_table" "lectern" "bell" "jukebox" "note_block"
    "beacon" "conduit" "lodestone" "respawn_anchor" "spawner" "beehive" "bee_nest" "flower_pot" "decorated_pot"
    "anvil" "chipped_anvil" "damaged_anvil" "crafter" "chiseled_bookshelf"})

(def owned #"_bed$|_sign$|_banner$|_head$|_skull$|_shulker_box$|^shulker_box$")

(def crop-blocks
  #{"wheat" "carrots" "potatoes" "beetroots" "melon_stem" "pumpkin_stem" "attached_melon_stem" "attached_pumpkin_stem"
    "melon" "pumpkin" "sugar_cane" "bamboo" "bamboo_sapling" "sweet_berry_bush" "nether_wart" "cocoa" "torchflower_crop"
    "pitcher_crop"})

(defn keep-why
  "Why a block is never dug (:fluid :light :container :owned), or nil."
  [n]
  (cond
    (fluids n) :fluid
    (re-find lights n) :light
    (containers n) :container
    (or (re-find owned n) (str/ends-with? n "_shulker_box")) :owned))

(defn crop-want? [want] (and (map? want) (contains? want :crop)))
(defn tree-want? [want] (and (map? want) (contains? want :tree)))

(defn classify
  "What the world's block (nil: nobody has seen it) is in a cell with this want: nil (nothing for this job), :dig,
  :wrong (reported, never dug) or [:kept why]. A want is a plan want, or :headroom for the air above a crop cell."
  [want block]
  (let [n (:name block)]
    (cond
      (or (nil? block) (shape/air? n)) nil
      (tree-want? want) nil
      (and (crop-want? want) (contains? (shape/crop-names (:crop want)) n)) nil
      (or (= :clear want) (= :headroom want) (crop-want? want))
      (if-let [why (keep-why n)]
        [:kept why]
        (cond
          (= "farmland" n) :wrong
          (and (crop-blocks n) (= :headroom want)) nil
          (and (crop-blocks n) (crop-want? want)) :wrong
          :else :dig))
      :else (when (= :wrong (shape/judge want block)) :wrong))))

(defn work-cells
  "The cells the job looks at: the plan's cells (of part when given) [{:pos :want ...}] and the cell above each crop
  cell that no cell of the plan names, as {:pos :want :headroom}."
  [cells part]
  (let [claimed (set (map :pos cells))
        mine (filterv #(or (nil? part) (= part (:part %))) cells)
        heads (->> mine (filter #(crop-want? (:want %))) (map #(update (:pos %) 1 inc)) (remove claimed) distinct)]
    (into mine (map (fn [pos] {:pos pos :want :headroom})) heads)))

(defn want-text [want] (if (= :headroom want) "air" (shape/want-text want)))

(defn strays
  "{:dig [{:pos :block}] :kept [{:pos :block :why}] :wrong [{:pos :found :want}]} of the cells, read through block-at
  (a fn [pos] -> {:name n :state s} or nil)."
  [cells block-at]
  (reduce (fn [acc {:keys [pos want]}]
            (let [block (block-at pos)
                  verdict (classify want block)]
              (cond
                (nil? verdict) acc
                (= :dig verdict) (update acc :dig conj {:pos pos :block (:name block)})
                (= :wrong verdict) (update acc :wrong conj {:pos pos :found (:name block) :want (want-text want)})
                :else (update acc :kept conj {:pos pos :block (:name block) :why (second verdict)}))))
          {:dig [] :kept [] :wrong []}
          cells))

(defn hazard-reason
  "The reason a dig hazard is accepted by: lava beside is :lava-adjacent, kept apart from water."
  [{:keys [reason fluid]}]
  (if (and (= :fluid-adjacent reason) (= "lava" fluid)) :lava-adjacent reason))

(defn judge-verdict
  "What to do with the may-dig? verdict v: :dig, :skip (not loaded), [:refuse {:reason ...}] or [:hazard [reasons]]."
  [v accept]
  (cond
    (= :not-loaded (:reason v)) :skip
    (not (:ok v)) [:refuse (select-keys v [:reason :zone :claim :plan])]
    (rules/accepts? (update v :hazards (fn [hs] (mapv #(assoc % :reason (hazard-reason %)) hs))) accept) :dig
    :else [:hazard (mapv hazard-reason (:hazards v))]))

(defn plan-trouble
  "Why a plan answer cannot be worked (the cells in part), or nil."
  [answer part]
  (cond
    (nil? answer) "no such plan"
    (:broken answer) (str "the plan cannot be read: " (:broken answer))
    (not-any? #(or (nil? part) (= part (:part %))) (:cells answer)) "no cells"))

;; ------------------------------------------------------------------ reading the world

(defn world-block
  "The block at [x y z] in plan.shape's shape: nil when unloaded, else {:name n} with :state when it has properties."
  [p [x y z]]
  (when-let [b (.blockAt p #js {:x x :y y :z z})]
    (cond-> {:name (.-name b)}
      (.-properties b) (assoc :state (js->clj (.-properties b) :keywordize-keys true)))))

(defn feet-of [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn permit
  "The may-dig? verdict for pos now: zones and footprints read afresh, the body's feet where they are."
  [c pos]
  (let [p (:primitives c)]
    (rules/may-dig? (merge {:block-at (fn [cell] (:name (world-block p cell))) :cell pos :feet (feet-of c) :ledger #{}}
                           (access-world c)))))

(defn decide [c pos] (judge-verdict (permit c pos) (:accept (:args c))))

(defn eye-dist [body [x y z]]
  (u/dist {:x (:x body) :y (+ (:y body) eye-height) :z (:z body)} {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)}))

(defn in-reach? [c pos] (<= (eye-dist (u/self-pos c) pos) (:reach (:args c))))

(defn pos-map [[x y z]] {:x x :y y :z z})

;; ------------------------------------------------------------------ check

(defn planned
  "{:answer plan-answer} for the plan in the args, or {:trouble text} (warned once per reason)."
  [c]
  (let [{:keys [plan part]} (:args c)
        answer (ctx/plan c plan)
        trouble (or (plan-trouble answer part)
                    (let [{:keys [zones ignore-zones?]} (access-world c)]
                      (when (and (nil? zones) (not ignore-zones?)) "no zone list has been read")))]
    (if-not trouble
      {:answer answer}
      (do (ctx/warn-once! c [plan trouble] :tidy.declined
                          {:plan plan :part part :reason trouble
                           :text (str "tidy declines plan " plan (when part (str " part " part)) ": " trouble)})
          {:trouble trouble}))))

(defn check [c] (not (:trouble (planned c))))

;; ------------------------------------------------------------------ steps

(defn refuse
  "m with pos refused: {:pos :block ...why}."
  [m {:keys [pos block]} why]
  (-> m
      (update :fails dissoc pos)
      (update :refused (fnil conj []) (merge {:pos pos :block block} why))))

(defn count-fail
  "m with one more failure on the stray; refused with :reason why at the give-up-th."
  [m {:keys [pos] :as stray} why give-up]
  (let [n (inc (get-in m [:fails pos] 0))]
    (if (>= n give-up)
      (refuse m stray {:reason why})
      (assoc-in m [:fails pos] n))))

(defn ^:async dig-one!
  "Dig the stray after asking the access rules once more; a refusal, a failure or a dig that did nothing is booked."
  [c {:keys [pos block] :as stray}]
  (let [p (:primitives c)
        give-up (:give-up (:args c))
        d (decide c pos)]
    (cond
      (and (vector? d) (= :refuse (first d))) (ctx/update-mem! c refuse stray (second d))
      (not= :dig d) nil
      :else
      (let [tool (tools/best-tool (map :name (u/inventory p)) block)]
        (when (and tool (not= tool (.-held (.self p))))
          (await (ctx/act c :equip #js {:item tool :dest "hand"})))
        (let [status (.-status (await (ctx/act c :dig (clj->js {:pos (pos-map pos)}))))]
          (case status
            "dug" (ctx/update-mem! c #(-> % (update :dug (fnil inc 0)) (assoc :collect true)))
            "missing" nil
            "cannot" (ctx/update-mem! c refuse stray {:reason :cannot})
            "unreachable" (ctx/update-mem! c count-fail stray :unreachable give-up)
            (ctx/update-mem! c count-fail stray :failed give-up)))))))

(defn ^:async walk-to!
  "Walk to within 3 of the stray; a walk that is blocked, or ends out of reach, counts a failure."
  [c {:keys [pos] :as stray}]
  (let [give-up (:give-up (:args c))
        w (await (u/walk-near! c (pos-map pos) 3))]
    (when (= :blocked w)
      (ctx/update-mem! c count-fail stray :unreachable give-up))
    (when (and (= :there w) (not (in-reach? c pos)))
      (ctx/update-mem! c count-fail stray :unreachable give-up))
    :continue))

(defn split-by-decision
  "{:ready [strays] :deferred [[stray reasons]]} of the strays; a refused one is booked on the way, a stray whose
  chunk is gone is dropped from both."
  [c todo]
  (reduce (fn [acc stray]
            (let [d (decide c (:pos stray))]
              (cond
                (= :dig d) (update acc :ready conj stray)
                (= :skip d) acc
                (= :refuse (first d)) (do (ctx/update-mem! c refuse stray (second d)) acc)
                :else (update acc :deferred conj [stray (second d)]))))
          {:ready [] :deferred []}
          todo))

(defn nearest [c strays]
  (let [body (u/self-pos c)]
    (first (sort-by #(u/dist body (pos-map (:pos %))) strays))))

(defn ^:async work!
  "One step over the strays still to dig: dig what is in reach (top-down), else walk to the nearest ready one; with
  only hazard-blocked ones left, count a try on those in reach, else walk to the nearest."
  [c todo]
  (let [{:keys [ready deferred]} (split-by-decision c todo)
        near (->> ready
                  (filter #(in-reach? c (:pos %)))
                  (sort-by (juxt #(- (get (:pos %) 1)) #(u/dist (u/self-pos c) (pos-map (:pos %))))))]
    (cond
      (seq near) (do (loop [left near]
                       (when (seq left)
                         (await (dig-one! c (first left)))
                         (recur (rest left))))
                     :continue)
      (seq ready) (await (walk-to! c (nearest c ready)))
      (seq deferred) (let [stray (nearest c (map first deferred))
                           reasons (second (first (filter #(= (:pos stray) (:pos (first %))) deferred)))]
                       (if (in-reach? c (:pos stray))
                         (do (ctx/update-mem! c (fn [m]
                                                  (let [n (inc (get-in m [:fails (:pos stray)] 0))]
                                                    (if (>= n (:give-up (:args c)))
                                                      (refuse m stray {:reason :hazard :hazards reasons})
                                                      (assoc-in m [:fails (:pos stray)] n)))))
                             :continue)
                         (await (walk-to! c stray))))
      :else :continue)))

(defn field-radius
  "How far from the body drops can lie: the cells' extent plus 4, at most 48."
  [cells]
  (let [ps (map :pos cells)
        lo (apply map min ps)
        hi (apply map max ps)]
    (min 48 (+ 4 (js/Math.ceil (apply js/Math.hypot (map - hi lo)))))))

(defn ^:async collect! [c cells]
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius (field-radius cells)}))]
    (when (= :done r)
      (ctx/update-mem! c #(-> % (dissoc :collect) (assoc :collected (:collected (ctx/child-result c :collect) 0)))))
    :continue))

(defn finish! [c found]
  (let [m (ctx/mem c)
        plan (:plan (:args c))
        refused (vec (:refused m))
        result {:dug (:dug m 0) :collected (:collected m 0) :kept (:kept found) :wrong (:wrong found) :refused refused}]
    (when (seq refused)
      (ctx/emit! c :tidy.refused :warn {:plan plan :refused refused
                                        :text (str "tidy of " plan " refused " (count refused) " strays: "
                                                   (str/join ", " (map #(str (pr-str (:pos %)) " " (:block %) " " (name (:reason %))) refused)))}))
    (when (seq (:wrong found))
      (ctx/emit! c :tidy.wrong :warn {:plan plan :cells (:wrong found)
                                      :text (str "tidy of " plan " left " (count (:wrong found)) " wrong blocks: "
                                                 (str/join ", " (map #(str (pr-str (:pos %)) " " (:found %)) (:wrong found))))}))
    (ctx/emit! c :tidy.done :info {:plan plan :dug (:dug result) :collected (:collected result) :kept (count (:kept found))
                                   :wrong (count (:wrong found)) :refused (count refused)
                                   :text (str "tidy of " plan " done: "
                                              (if (and (zero? (:dug result)) (empty? refused)) "nothing to dig" (str "dug " (:dug result)))
                                              (when (seq refused) (str ", refused " (count refused))))})
    (ctx/result! c result)
    :done))

(defn ^:async round [c]
  (let [{:keys [answer trouble]} (planned c)]
    (if trouble
      :declined
      (let [cells (work-cells (:cells answer) (:part (:args c)))
            found (strays cells #(world-block (:primitives c) %))
            m (ctx/mem c)
            refused (set (map :pos (:refused m)))
            todo (remove #(refused (:pos %)) (:dig found))]
        (cond
          (seq todo) (await (work! c todo))
          (:collect m) (await (collect! c cells))
          :else (finish! c found))))))
