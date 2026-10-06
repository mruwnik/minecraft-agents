(ns jobs.forestry.prepare
  (:require [clojure.string :as str]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.forestry.trees :as forestry]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.access :as access]
            [jobs.farm.tidy :as tidy]
            [jobs.forestry.maintain :as maintain]
            [jobs.lib.world :as known]))

(def doc
  "Get the planting spots of a forest plan ready. A spot is a planned tree cell (want {:tree species}); the cell
  is where the sapling goes. The goal per cell: a sapling of the planned species can be planted and can grow there.
  Each round takes one step on the nearest cell that can still be improved:
  1. Dig the stray in the cell (grass, flowers, snow, leaves, stone: anything but air, the species' own sapling or
     log, or a thing never dug).
  2. Replace natural ground under the cell (stone, sand, gravel, cobblestone, deepslate ...) with carried dirt. It
     digs, then places. A hole this job dug is filled first, also after a restart.
  3. Plant the sapling when it is carried (jobs.forestry.plant-sapling as a child).
  Drops are collected (jobs.forestry.collect-drops) when the next step is on another cell or none is left.
  Bringing saplings and dirt is another job, e.g. (seq (jobs.storage.withdraw ...) (jobs.forestry.prepare ...)).
  Water in the cell is repaired. A source in the cell is filled with carried dirt (the dirt is then dug as a
  stray). A source beside it on its level is dammed first, since it would flood the dug cell back. A flow is
  traced upstream (at most 16 cells) and its source filled. The cell then gets 10 s to recede (the round returns
  :declined, nothing is polled). Water that cannot be traced, no dirt carried, or a refused source leaves the
  cell as :wet {:pos :why :source} (warn prepare.wet).
  Never dug, only reported:
  - a stray or natural ground that no carried tool harvests (stone with no pickaxe: slow, nothing drops): result
    :no-tool {:pos :block}, warn prepare.no-tool. The cell is taken up when a tool is carried.
  - in the cell: another species' sapling, any log but the species' own, lava, a container, bed, sign or
    light (result :wrong).
  - under the cell: anything but natural ground (planks, logs, wool, a chest, air, fluid, a cell the plan names)
    gives :no-soil with :why :other-block, :kept, :hollow, :fluid or :planned. Natural ground with no dirt
    carried gives :no-soil :no-dirt.
  - above the cell: nothing is dug. A block in the growth space that a tree cannot grow through (anything but
    air, leaves, saplings, plants, snow, vines) leaves the cell untouched and reported as :cramped
    {:pos :at :block}.
  The growth space is HEADROOM cells from the planned cell up, the cell included: oak 7, birch 8, spruce 9,
  jungle 13, acacia 10, dark_oak 10, pale_oak 10, cherry 9. These are approximate (about 70% sure of each).
  Light level and the neighbours of a 2x2 tree are not checked. :headroom {species n} overrides. A species not in
  the table is refused as :unsupported-species. The species' own sapling is left growing, its own log is
  maintain's business.
  Every dig and place is checked against zones and the footprints of all other active plans, when the cell is
  chosen and again before the act. A refusal, a dig hazard not in :accept, or three failed tries (walk, dig,
  place, plant) skips the cell for 10 minutes (body memory :forestry/prepare-skip). It is reported as :refused
  {:pos :reason} (warn prepare.refused).
  Waits (check) unless a step can be taken. A field where every cell is done, wrong, cramped, without soil or
  short of saplings is left alone, with one note per cell (prepare.wrong, prepare.no-soil, prepare.cramped,
  prepare.wet) and one per species short (prepare.short {:species :missing}). The job wakes when what it lacked
  is carried. A started job runs on to finish.
  The job declines (one prepare.declined warn) while the plan is missing, unreadable or has no tree cells (in
  :part), and while no zone list is loaded.
  Result: {:cleared :soiled :planted :dammed :short {species missing} :wrong :no-soil :no-tool :cramped :wet :refused},
  info prepare.done.")

(def args
  {:plan {:doc "id of a plan of the body's world; its tree cells are the planting spots" :default nil}
   :part {:doc "only the cells of this part" :default nil}
   :accept {:doc "dig hazards (jobs.lib.access.rules) taken: a set of :fluid-adjacent :falling-block :under-feet; lava beside is :lava-adjacent and never taken by default"
            :default #{:fluid-adjacent}}
   :headroom {:doc "{species cells} overriding the table of growth space above a planted cell (the cell included)" :default {}}
   :collect-radius {:doc "how far from where the body stands the drops are collected, in blocks" :default 8}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

;; ------------------------------------------------------------------ the rule, pure

(def headroom-table
  {"oak" 7 "birch" 8 "spruce" 9 "jungle" 13 "acacia" 10 "dark_oak" 10 "pale_oak" 10 "cherry" 9})

(defn headroom-of
  "Cells of growth space for species (the planted cell included): the override, else the table, nil for a species
  not supported."
  [species over]
  (or (get over species) (get headroom-table species)))

(def tree-plants
  #{"short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "snow" "vine" "glow_lichen" "dandelion" "poppy"
    "blue_orchid" "allium" "azure_bluet" "red_tulip" "orange_tulip" "white_tulip" "pink_tulip" "oxeye_daisy"
    "cornflower" "lily_of_the_valley" "torchflower" "sunflower" "lilac" "rose_bush" "peony" "pink_petals"
    "wildflowers" "leaf_litter" "short_dry_grass" "tall_dry_grass" "bush" "firefly_bush"})

(defn tree-free?
  "Whether a tree's growth may take the place of the block: air, leaves, saplings, plants, snow, vines."
  [n]
  (boolean (or (rules/air n) (str/ends-with? n "_leaves") (str/ends-with? n "_sapling") (tree-plants n))))

(def natural-names
  #{"stone" "cobblestone" "deepslate" "cobbled_deepslate" "andesite" "diorite" "granite" "tuff" "calcite" "sand"
    "red_sand" "gravel" "sandstone" "red_sandstone" "clay" "mud" "snow_block" "mycelium" "terracotta" "dirt_path"})

(defn natural-ground?
  "Whether the block is ground nobody built: replaced by dirt when it is under a planted cell."
  [n]
  (contains? natural-names n))

(def soil-preference ["dirt" "grass_block" "coarse_dirt" "podzol"])

(defn soil-item
  "The soil block of species' soil carried (carried: a set of names), dirt first, or nil."
  [carried species]
  (first (filter #(and (carried %) ((maintain/soil-for species) %)) soil-preference)))

(defn plant-like? [n]
  (boolean (re-find #"(_sapling|_propagule|_fungus)$" n)))

(defn own-cell
  "{:state ...} when the block in the planned cell settles it (unloaded, growing, grown, wrong), else nil."
  [p pos species]
  (let [n (u/block-name p pos)]
    (cond
      (nil? n) {:state :unloaded}
      (= n (forestry/sapling-of species)) {:state :growing}
      (= n (str species "_log")) {:state :grown}
      (or (plant-like? n) (forestry/log-name? n) (tidy/keep-why n)) {:state :wrong :found n})))

(defn growth-space
  "{:state :cramped :at :block} for the lowest block above the planted cell a tree cannot grow through, {:state
  :unloaded} when a cell of it is not loaded, else nil."
  [p pos species over]
  (some (fn [dy]
          (let [at (update pos :y + dy)
                n (u/block-name p at)]
            (cond
              (nil? n) {:state :unloaded}
              (not (tree-free? n)) {:state :cramped :at (maintain/cell-vec at) :block n})))
        (range 1 (headroom-of species over))))

(defn soil-state
  "{:state ...} for the ground under the planted cell: nil when it is soil the species takes, else :soil-dig (natural
  ground, dirt carried), :soil-place (the hole this job dug, dirt carried), :no-soil with :why, or :unloaded."
  [p pos species {:keys [planned holes carried]}]
  (let [under (maintain/down pos)
        n (u/block-name p under)
        dirt (soil-item carried species)
        no-dirt {:state :no-soil :why :no-dirt}]
    (cond
      (nil? n) {:state :unloaded}
      ((maintain/soil-for species) n) nil
      (and (contains? holes (maintain/cell-vec under)) (rules/air n)) (if dirt {:state :soil-place :item dirt} no-dirt)
      (planned (maintain/cell-vec under)) {:state :no-soil :why :planned}
      (rules/air n) {:state :no-soil :why :hollow}
      (rules/fluids n) {:state :no-soil :why :fluid}
      (tidy/keep-why n) {:state :no-soil :why :kept}
      (not (natural-ground? n)) {:state :no-soil :why :other-block}
      dirt {:state :soil-dig :block n :item dirt}
      :else no-dirt)))

(def max-upstream "How many cells upstream the walk to a source goes." 16)

(defn upstream-cells
  "The water cells the cell at level l is fed from, the best first: the cell above for falling water, else the
  neighbours on its level with a lower level (a falling one too, for level 1), lowest first."
  [level-at [x y z] l]
  (if (>= l 8)
    (filter level-at [[x (inc y) z]])
    (->> rules/neighbour-deltas
         (filter (fn [[_ dy _]] (zero? dy)))
         (keep (fn [[dx _ dz]]
                 (let [q [(+ x dx) y (+ z dz)]
                       ql (level-at q)]
                   (when (and ql (or (< ql l) (and (= l 1) (>= ql 8)))) [ql q]))))
         (sort-by first)
         (map second))))

(defn upstream
  "The [x y z] of the source reached by walking upstream from start, or nil: level-at maps an [x y z] to the level of
  the water there (0 source, 1-7 flowing, 8+ falling), nil for anything else; at most max-upstream steps, no cell twice."
  [level-at start]
  (loop [pos (vec start) n 0 seen #{}]
    (let [l (level-at pos)
          next-pos (when (and l (pos? l) (< n max-upstream) (not (seen pos))) (first (upstream-cells level-at pos l)))]
      (cond
        (nil? l) nil
        (zero? l) pos
        next-pos (recur next-pos (inc n) (conj seen pos))))))

(defn water-level
  "The level of the water at pos (a number), nil for any other block, unloaded, or water without a level."
  [p pos]
  (let [b (u/block-at p pos)]
    (when (= "water" (some-> b .-name))
      (let [l (some-> b .-properties .-level)]
        (when (number? l) l)))))

(defn wet-state
  "{:state ...} of a planned cell holding water: :fill (the source is in the cell and no source lies beside it),
  :dam (the source is at :target: upstream, or beside a source cell, which would flood back once dug), both with the
  :item to place and the :source, or :wet with :why :untraced / :no-dirt."
  [p pos species carried]
  (let [l (water-level p pos)
        cell (maintain/cell-vec pos)
        level-at (fn [[x y z]] (water-level p {:x x :y y :z z}))
        beside (when (and l (zero? l))
                 (let [[x y z] cell]
                   (->> rules/neighbour-deltas
                        (filter (fn [[_ dy _]] (zero? dy)))
                        (map (fn [[dx _ dz]] [(+ x dx) y (+ z dz)]))
                        (filter #(= 0 (level-at %)))
                        first)))
        source (cond (nil? l) nil
                     beside beside
                     (zero? l) cell
                     :else (upstream level-at cell))
        dirt (soil-item carried species)]
    (cond
      (nil? source) {:state :wet :why :untraced}
      (nil? dirt) {:state :wet :why :no-dirt :source source}
      (= source cell) {:state :fill :target pos :source source :item dirt}
      :else {:state :dam :target (zipmap [:x :y :z] source) :source source :item dirt})))

(declare assess-open)

(defn assess
  "{:state ...} of one planned cell: :unsupported, :unloaded, :growing, :grown, :wrong (:found), :cramped (:at :block),
  :fill / :dam / :wet (:why :source) for water in the cell, :clear (:block: a stray to dig), :soil-dig, :soil-place,
  :no-soil (:why), :no-tool (:block: a dig no carried tool harvests; snow, whose drop is a snowball, is dug anyway), :plant (:item) or :short."
  [p pos species world]
  (let [r (assess-open p pos species world)
        dug (when (#{:clear :soil-dig} (:state r)) (:block r))]
    (if (and dug (not= "snow" dug) (not (tools/can-harvest? p dug)))
      {:state :no-tool :block dug}
      r)))

(defn assess-open
  "assess before the tool rule."
  [p pos species {:keys [carried over] :as world}]
  (let [n (u/block-name p pos)
        sapling (forestry/sapling-of species)]
    (or (when-not (headroom-of species over) {:state :unsupported})
        (when (= "water" n) (wet-state p pos species carried))
        (own-cell p pos species)
        (growth-space p pos species over)
        (when-not (rules/air n) {:state :clear :block n})
        (soil-state p pos species world)
        (if (carried sapling) {:state :plant :item sapling} {:state :short}))))

(def steps #{:fill :dam :clear :soil-dig :soil-place :plant})

;; ------------------------------------------------------------------ reading the field

(def skip-kind :forestry/prepare-skip)

(def skip-policy {:cap 100 :ttl (* 10 60 1000)})

(defn skipped
  "{pos data} of the cells skipped for a while (body memory)."
  [c]
  (into {} (map (fn [e] [(:pos (:data e)) (:data e)])) (ctx/entries c skip-kind)))

(defn planned
  "{:trees {pos species} :planned #{[x y z]}} for the plan, or {:trouble text} (warned once per reason)."
  [c]
  (let [{:keys [plan part]} (:args c)
        answer (known/plan c plan)
        trees (maintain/tree-cells answer part)
        trouble (or (maintain/plan-trouble answer trees)
                    (when (and (nil? (known/zones c)) (not (:ignore-zones? (:args c)))) "no zone list"))]
    (if-not trouble
      {:trees trees :planned (set (map (comp vec :pos) (:cells answer)))}
      (do (ctx/warn-once! c [plan trouble] :prepare.declined
                          {:plan plan :part part :reason trouble
                           :text (str "prepare declines plan " plan (when part (str " part " part)) ": " trouble)})
          {:trouble trouble}))))

(declare place-verdict)

(defn settle-wet
  "The assessed cell with a water state checked against what is known now: a :fill / :dam the access rules refuse is
  :wet with the reason, and an untraced cell still receding from a dam is :receding."
  [c {:keys [state why pos] :as cell}]
  (let [v (when (#{:fill :dam} state) (place-verdict c (maintain/cell-vec (:target cell))))
        until (get (:recede (ctx/mem c)) pos)]
    (cond
      (vector? v) (assoc cell :state :wet :why (second v))
      (and (= :wet state) (= :untraced why) until (> until (ctx/now c))) (assoc cell :state :receding :until until)
      :else cell)))

(defn assessments
  "{pos {:pos :species :state ...}} of every planned tree cell, read now."
  [c {:keys [trees planned]}]
  (let [m (ctx/mem c)
        world {:planned planned
               :holes (set (:holes m))
               :carried (maintain/carried-names (:primitives c))
               :over (:headroom (:args c))}]
    (into {} (map (fn [[pos species]]
                    [pos (settle-wet c (assoc (assess (:primitives c) pos species world) :pos pos :species species))]))
          trees)))

(defn todo
  "The cells with a step to take, not skipped, nearest to the body first."
  [c states]
  (let [skip (skipped c)
        here (u/self-pos c)]
    (->> (vals states)
         (filter #(and (steps (:state %)) (not (contains? skip (:pos %)))))
         (sort-by #(u/dist here (:pos %))))))

(defn in-column?
  "Whether the body stands in the column of pos."
  [c pos]
  (let [[x _ z] (maintain/feet-cell c)]
    (and (= x (:x pos)) (= z (:z pos)))))

;; ------------------------------------------------------------------ notes

(defn note-cells!
  "Say, once each, what is wrong with a cell, and (when settled) which species are short of saplings."
  [c states settled?]
  (doseq [{:keys [pos species state found why at block source]} (sort-by (comp maintain/cell-vec :pos) (vals states))]
    (case state
      :wrong (ctx/warn-once! c [:wrong pos found] :prepare.wrong
                             {:pos pos :found found :species species
                              :text (str "prepare leaves " found " at " (pr-str (maintain/cell-vec pos)) " (the plan wants a " species " tree)")})
      :no-soil (ctx/warn-once! c [:no-soil pos why] :prepare.no-soil
                               {:pos pos :why why
                                :text (str "prepare cannot give " (pr-str (maintain/cell-vec pos)) " soil: " (name why))})
      :wet (ctx/warn-once! c [:wet pos why] :prepare.wet
                           {:pos pos :why why :water-source (some-> source vec)
                            :text (str "prepare leaves " (pr-str (maintain/cell-vec pos)) " wet: " (name why)
                                       (when source (str ", water source " (pr-str source))))})
      :no-tool (ctx/warn-once! c [:no-tool pos block] :prepare.no-tool
                               {:pos pos :block block
                                :text (str "prepare leaves " block " at " (pr-str (maintain/cell-vec pos)) ": no carried tool harvests it")})
      :cramped (ctx/warn-once! c [:cramped pos at] :prepare.cramped
                               {:pos pos :at at :block block
                                :text (str "prepare leaves " (pr-str (maintain/cell-vec pos)) ": " block " at " (pr-str at) " blocks the growth space")})
      nil))
  (when settled?
    (doseq [[species cells] (group-by :species (filter #(= :short (:state %)) (vals states)))]
      (ctx/warn-once! c [:short species (count cells)] :prepare.short
                      {:species species :missing (count cells)
                       :text (str "prepare has no " (forestry/sapling-of species) " for " (count cells) " cells")}))))

;; ------------------------------------------------------------------ check

(defn receding? [states] (boolean (some #(= :receding (:state %)) (vals states))))

(defn check
  "Passes while a step can be taken on a planned cell, and once the job has begun (its finishing round must run)
  unless a cell is still receding from a dam."
  [c]
  (let [field (planned c)]
    (boolean
     (and (not (:trouble field))
          (let [states (assessments c field)
                work (todo c states)]
            (note-cells! c states (empty? work))
            (or (seq work) (and (:begun (ctx/mem c)) (not (receding? states)))))))))

;; ------------------------------------------------------------------ access

(defn dig-verdict
  "What the access rules say of digging the cell pos: :ok, :wait (not loaded) or [:refuse reason]."
  [c pos]
  (let [d (tidy/decide c pos)]
    (cond
      (= :dig d) :ok
      (= :skip d) :wait
      (= :refuse (first d)) [:refuse (:reason (second d))]
      :else [:refuse (first (second d))])))

(defn place-verdict
  "What the access rules say of placing at the cell pos: :ok, :wait (not loaded, or the body in it) or [:refuse reason]."
  [c pos]
  (let [p (:primitives c)
        v (rules/may-place? (merge {:block-at (fn [[x y z]] (u/block-name p {:x x :y y :z z})) :cell pos
                                    :feet (maintain/feet-cell c) :ledger #{}}
                                   (tidy/access-world c)))]
    (cond
      (:ok v) :ok
      (#{:not-loaded :own-body} (:reason v)) :wait
      :else [:refuse (:reason v)])))

;; ------------------------------------------------------------------ steps

(defn skip!
  "Leave the cell for a while and say why, once."
  [c pos reason]
  (ctx/remember! c skip-kind {:pos pos :reason reason} skip-policy)
  (ctx/warn-once! c [:refused pos reason] :prepare.refused
                  {:pos pos :reason reason
                   :text (str "prepare leaves " (pr-str (maintain/cell-vec pos)) " alone: " (name reason))}))

(defn fail!
  "Count a failed try on the cell; skipped at the third. :continue."
  [c pos reason]
  (let [n (inc (get-in (ctx/mem c) [:fails pos] 0))]
    (ctx/update-mem! c assoc-in [:fails pos] n)
    (when (>= n u/max-failures)
      (skip! c pos reason))
    :continue))

(defn blocked!
  "Book a verdict that is not :ok against the planned cell: a refusal skips it, a wait counts a failure. :continue."
  [c cell v]
  (if (vector? v)
    (skip! c cell (second v))
    (fail! c cell :not-loaded))
  :continue)

(defn bump [m k] (update m k (fnil inc 0)))

(defn ^:async step-off!
  "Walk off the column of pos, which the body stands in (jobs.lib.step-off). :continue."
  [c pos]
  (let [r (await (step-off/step-off! c pos {:ok? (step-off/zone-ok (access/rules-input c))}))]
    (when (:unreachable r)
      (fail! c pos :unreachable))
    :continue))

(defn ^:async ready!
  "Walk within reach of target; with column? also off the column of the planned cell. nil when ready to act, else
  :continue."
  [c cell target column?]
  (let [w (await (near/walk-near! c target 3))]
    (cond
      (= :partial w) :continue
      (= :blocked w) (fail! c cell :unreachable)
      (and column? (in-column? c cell)) (await (step-off! c cell))
      :else nil)))

(defn ^:async equip-for!
  "Hold the carried tool that suits the block, when there is one."
  [c block]
  (tools/equip-for! c block))

(defn ^:async dig!
  "Dig the block at target (a cell the planned cell owes work on). on-dug is called with c when it went."
  [c cell target block column? on-dug]
  (let [v (dig-verdict c (maintain/cell-vec target))]
    (if (vector? v)
      (blocked! c cell v)
      (or (await (ready! c cell target column?))
          (let [v (dig-verdict c (maintain/cell-vec target))]
            (if (not= :ok v)
              (blocked! c cell v)
              (do (await (equip-for! c block))
                  (let [r (await (ctx/act c :dig (clj->js {:pos target})))
                        _ (await (tools/note-wear! c))]
                    (case (.-status r)
                      "dug" (on-dug c)
                      "missing" nil
                      "cannot" (skip! c cell :cannot)
                      "unreachable" (fail! c cell :unreachable)
                      (fail! c cell :failed))
                    :continue))))))))

(defn dug-stray [pos]
  (fn [c] (ctx/update-mem! c #(-> % (bump :cleared) (assoc :collect pos)))))

(defn dug-ground [pos under]
  (fn [c] (ctx/update-mem! c #(-> % (update :holes (fnil conj #{}) (maintain/cell-vec under)) (assoc :collect pos)))))

(defn ^:async soil-place!
  "Fill the hole under the planned cell with the carried soil. :continue."
  [c {:keys [pos item]}]
  (let [under (maintain/down pos)
        v (place-verdict c (maintain/cell-vec under))]
    (if (vector? v)
      (blocked! c pos v)
      (or (await (ready! c pos under true))
          (let [v (place-verdict c (maintain/cell-vec under))]
            (if (not= :ok v)
              (blocked! c pos v)
              (do (await (ctx/act c :equip (clj->js {:item item})))
                  (let [r (await (ctx/act c :place (clj->js {:pos under :item item})))]
                    (case (.-status r)
                      "placed" (ctx/update-mem! c #(-> % (bump :soiled) (update :holes disj (maintain/cell-vec under))))
                      ("no-item" "occupied") nil
                      (fail! c pos :failed))
                    :continue))))))))

(def recede-ms "How long a flow is given to recede after its source was dammed." 10000)

(defn ^:async place-source!
  "Place the carried soil into the water of a wet cell: the source in the cell (:fill), or the source elsewhere
  (:dam, after which the cell is receding for recede-ms). :continue."
  [c {:keys [pos target item state]}]
  (let [at (maintain/cell-vec target)
        fill? (= :fill state)
        v (place-verdict c at)]
    (if (vector? v)
      (blocked! c pos v)
      (or (when (in-column? c target) (await (step-off! c pos)))
          (await (ready! c pos target fill?))
          (let [v (place-verdict c at)]
            (if (not= :ok v)
              (blocked! c pos v)
              (do (await (ctx/act c :equip (clj->js {:item item})))
                  (let [r (await (ctx/act c :place (clj->js {:pos target :item item})))
                        dammed (fn [m] (cond-> (bump m :dammed)
                                         (not fill?) (update :recede assoc pos (+ (ctx/now c) recede-ms))))]
                    (case (.-status r)
                      "placed" (ctx/update-mem! c dammed)
                      ("no-item" "occupied") nil
                      (fail! c pos :failed))
                    :continue))))))))

(defn ^:async plant!
  "Plant the carried sapling in the planned cell with jobs.forestry.plant-sapling as a child. :continue."
  [c {:keys [pos species item]}]
  (let [v (place-verdict c (maintain/cell-vec pos))]
    (if (vector? v)
      (blocked! c pos v)
      (or (await (ready! c pos pos true))
          (let [v (place-verdict c (maintain/cell-vec pos))]
            (if (not= :ok v)
              (blocked! c pos v)
              (do (await (ctx/call-child c :plant 'jobs.forestry.plant-sapling
                                {:at pos :species species :for-plan (:plan (:args c))
                                 :ignore-zones? (:ignore-zones? (:args c))}))
                  (if (= item (u/block-name (:primitives c) pos))
                    (ctx/update-mem! c bump :planted)
                    (fail! c pos :failed))
                  :continue)))))))

(defn ^:async collect!
  "One collect-drops round while the drops of the digs are owed. :continue."
  [c]
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius (:collect-radius (:args c))}))]
    (when (= :done r) (ctx/update-mem! c dissoc :collect))
    :continue))

(defn ^:async act!
  "The step the cell is owed."
  [c {:keys [state pos block] :as cell}]
  (case state
    (:fill :dam) (await (place-source! c cell))
    :clear (await (dig! c pos pos block false (dug-stray pos)))
    :soil-dig (let [under (maintain/down pos)]
                (await (dig! c pos under block true (dug-ground pos under))))
    :soil-place (await (soil-place! c cell))
    :plant (await (plant! c cell))))

;; ------------------------------------------------------------------ result

(defn by-pos [entries] (vec (sort-by :pos entries)))

(defn state-list
  "The result entries of the cells in state, each built by (f cell), by position."
  [states state f]
  (by-pos (map f (filter #(= state (:state %)) (vals states)))))

(defn refused-list
  "The refusals: cells skipped in body memory (of this plan's cells) and cells of an unsupported species."
  [c states]
  (by-pos (concat (keep (fn [[pos data]]
                          (when (contains? states pos) {:pos (maintain/cell-vec pos) :reason (:reason data)}))
                        (skipped c))
                  (state-list states :unsupported (fn [{:keys [pos]}] {:pos (maintain/cell-vec pos) :reason :unsupported-species})))))

(defn finish!
  "Say what was done and be done."
  [c states]
  (let [m (ctx/mem c)
        missing (frequencies (map :species (filter #(= :short (:state %)) (vals states))))
        result {:cleared (:cleared m 0) :soiled (:soiled m 0) :planted (:planted m 0) :dammed (:dammed m 0)
                :short missing
                :wrong (state-list states :wrong (fn [{:keys [pos found species]}] {:pos (maintain/cell-vec pos) :found found :species species}))
                :no-soil (state-list states :no-soil (fn [{:keys [pos why]}] {:pos (maintain/cell-vec pos) :why why}))
                :no-tool (state-list states :no-tool (fn [{:keys [pos block]}] {:pos (maintain/cell-vec pos) :block block}))
                :cramped (state-list states :cramped (fn [{:keys [pos at block]}] {:pos (maintain/cell-vec pos) :at at :block block}))
                :wet (state-list states :wet (fn [{:keys [pos why source]}]
                                               (cond-> {:pos (maintain/cell-vec pos) :why why} source (assoc :source source))))
                :refused (refused-list c states)}]
    (ctx/emit! c :prepare.done :info
               (assoc result :text (str "prepare done: cleared " (:cleared result) ", soiled " (:soiled result)
                                        ", planted " (:planted result) ", short " (reduce + (vals missing))
                                        ", left " (+ (count (:wrong result)) (count (:no-soil result)) (count (:no-tool result)) (count (:cramped result))
                                                    (count (:wet result)) (count (:refused result))))))
    (ctx/result! c result)
    :done))

(defn ^:async round [c]
  (let [field (planned c)]
    (if (:trouble field)
      :declined
      (do
        (when-not (:begun (ctx/mem c))
          (ctx/update-mem! c assoc :begun true))
        (let [states (assessments c field)
              work (todo c states)
              target (first work)
              owed (:collect (ctx/mem c))]
          (note-cells! c states (empty? work))
          (cond
            (and owed (or (nil? target) (not= owed (:pos target)))) (await (collect! c))
            target (await (act! c target))
            (receding? states) :declined
            :else (finish! c states)))))))
