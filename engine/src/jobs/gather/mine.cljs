(ns jobs.gather.mine
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.tools :as tools]
            [engine.jobs.util :as u]
            [jobs.survival.dig-in :as dig-in]))

(def doc
  "Dig and pick up :count more of one block kind (:block) within :radius, then
  mend the pit under where it started. A pickaxe block (by engine.jobs.tools/tool-kind)
  with no *_pickaxe carried ends at once, before any write or dig: warn
  mine.no-tool with :tool \"pickaxe\", hand over {:got 0 :reason :no-tool :tool \"pickaxe\"}
  (shovel and axe blocks drop by hand, so those go on without the tool). Check: a phase is in memory, or :block
  is named and an exposed one is within :radius. An exposed block has at least
  one of its 6 face neighbours air or cave_air; it is skipped when a neighbour
  is lava, or water unless :wet. The first round writes the goal (carried +
  :count), the start cell and the ground (the solid cells of the 5x5 under the
  start at y-1 and y-2, empty when :mend is false) to job memory before any
  dig, so a cut or a restart still mends. Then, one step per round, in order:
  (1) a collecting flag runs the collect-drops child for the item within
  :collect-radius; (2) carrying the goal ends :count; (3) :max-failures
  failures end :gave-up (warn mine.gave-up); :dry-digs digs in a row after
  which the collect phase left the carried count of the item unchanged (a
  full inventory) end :no-drops (warn mine.gave-up with
  :reason :no-drops); (4) no target ends :wet when an
  exposed block was rejected only for water, else :none; (5) the nearest
  target (those over the ground snapshot only after all others, so the floor
  under the start is dug last) is walked to (within 3: blocked skips it and counts a failure, partial
  tries again, the third partial in a row skips it like blocked), the best carried tool of the kind (shovel, axe or pickaxe by
  block; the material decides) is equipped, and the block dug: dug resets the
  failures and starts collecting, missing does nothing, cannot (bedrock) skips
  it without a failure, anything else skips it and counts one. The mend phase
  fills every ground cell that is now air, cave_air or water with the first
  carried of dig-in's building blocks other than the item, the item itself last
  (when it is a building block or the block itself), lowest first, then nearest, never the body's feet
  or head cell; when only those are owed it jumpPlaces one block. Nothing to
  fill with warns mine.mend-short, six failed fills warn mine.mend-failed; both
  end the job. The item is :item, else the drop-item table, else the block
  name. When nothing is owed and the mend left fewer than the goal carried
  (it spent what the dig brought), the job goes back to the dig phase (ground
  kept, :resumes + 1, at most 2) while an exposed target off the ground remains
  in :radius; otherwise it ends :spent-on-mend, keeping the earlier reason in
  :dig-reason. Hands over {:got n :reason r} (plus :dig-reason and :resumes when
  set; info mine.done with :mended, the cells filled); :got is how many more are carried
  than at the start, at least 0.
  Zones and plans (engine.access.rules, through engine.jobs.access): a target must also be a dig the rules permit,
  with only :accept hazards, when it is chosen and again right before the dig. A cell inside a zone that does not
  allow :dig, or in an active plan's footprint, is never a target; one refused right before the dig is skipped
  without a failure (info mine.refused), as is one with a hazard not accepted. Every exposed block refused ends the
  dig phase :refused, or, before the first round, declines; either way one mine.declined warn per job names the zones
  and plans ({:reason :refused :zones :plans}). No zone list (zones.edn missing or never valid) declines the check
  with one mine.declined warn {:reason :no-zones}, also in the middle of the job; nothing is dug or placed then.")

(def args
  {:block {:doc "name of the block to mine (required)" :default nil}
   :item {:doc "the item the block drops; nil: the drop-item table, else the block name" :default nil}
   :count {:doc "how many more to carry than at the start" :default 8}
   :radius {:doc "blocks within this many blocks of the body count" :default 16}
   :wet {:doc "dig blocks that touch water" :default false}
   :mend {:doc "fill the ground under the start again afterwards" :default true}
   :collect-radius {:doc "how far around to collect drops after a dig" :default 6}
   :max-failures {:doc "failures in a row before giving up" :default 3}
   :dry-digs {:doc "digs in a row after which the carried count of the item did not rise before giving up (:no-drops)" :default 3}
   :accept {:doc "dig hazards of engine.access.rules taken (:fluid-adjacent :falling-block :under-feet); the lava and :wet rules above still hold"
            :default #{:fluid-adjacent :falling-block :under-feet}}})

(def ores
  {"coal_ore" "coal" "iron_ore" "raw_iron" "copper_ore" "raw_copper" "gold_ore" "raw_gold"
   "diamond_ore" "diamond" "redstone_ore" "redstone" "lapis_ore" "lapis_lazuli" "emerald_ore" "emerald"})

(def drop-item
  (merge {"stone" "cobblestone" "grass_block" "dirt" "deepslate" "cobbled_deepslate"
          "clay" "clay_ball" "snow_block" "snowball"}
         ores
         (into {} (map (fn [[k v]] [(str "deepslate_" k) v])) ores)))

(def reach 3)
(def mend-reach 4)
(def max-mend-failures 6)
(def air #{"air" "cave_air"})
(def not-solid #{"air" "cave_air" "water" "lava" "short_grass" "tall_grass"})
(def faces [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn item-name
  "The item the mined block drops."
  [{:keys [block item]}]
  (or item (get drop-item block) block))

(defn carried
  "Total of the item carried over all stacks."
  [c]
  (let [item (item-name (:args c))]
    (transduce (comp (filter #(= item (:name %))) (map :count)) + 0 (u/inventory (:primitives c)))))

(defn around [{:keys [x y z]} [dx dy dz]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})

(defn classify
  "How the cell of a block would be dug: :ok, :wet (only water stops it) or :no
  (buried, or lava beside it)."
  [c wet pos]
  (let [names (map #(u/block-name (:primitives c) (around pos %)) faces)]
    (cond
      (not-any? air names) :no
      (some #{"lava"} names) :no
      (and (some #{"water"} names) (not wet)) :wet
      :else :ok)))

(defn scan
  "{:targets [pos] nearest first, those over the ground snapshot after all
  others, :refused [verdict] of exposed blocks a zone or plan refuses, :wet?
  true when an unskipped block was rejected only for water}."
  [c]
  (let [{:keys [block radius wet accept]} (:args c)
        skipped (set (:skipped (ctx/mem c)))
        ground (into #{} (map :pos) (:ground (ctx/mem c)))
        here (u/self-pos c)
        cells (->> (array-seq (.blocks (:primitives c) #js {:radius radius :names #js [block] :max 128}))
                   (map #(u/pos-of (.-pos %)))
                   (remove skipped))
        graded (map (juxt identity #(classify c wet %)) cells)
        in (access/rules-input c)
        judged (->> graded
                    (filter #(= :ok (second %)))
                    (map (fn [[pos]] (let [v (access/may-dig? in pos)] [pos v (access/judge v accept)]))))]
    {:targets (->> judged (filter #(= :ok (nth % 2))) (map first) (sort-by (juxt #(if (ground %) 1 0) #(u/dist here %))) vec)
     :refused (into [] (comp (filter #(= :refused (nth % 2))) (map second)) judged)
     :wet? (boolean (some #(= :wet (second %)) graded))}))

(defn off-ground?
  "Whether a target is not one of the ground snapshot's cells."
  [c pos]
  (not-any? #(= pos (:pos %)) (:ground (ctx/mem c))))

(defn check [c]
  (cond
    (nil? (ctx/zones c)) (access/decline! c :mine.declined "mine" {:reason :no-zones})
    (:phase (ctx/mem c)) true
    (not (:block (:args c))) false
    :else (let [{:keys [targets refused]} (scan c)]
            (cond
              (seq targets) true
              (seq refused) (access/decline! c :mine.declined "mine" (assoc (access/refusal-fields refused) :reason :refused))
              :else false))))

(defn cell-of [pos] {:x (js/Math.floor (:x pos)) :y (js/Math.floor (:y pos)) :z (js/Math.floor (:z pos))})

(defn snapshot
  "The solid cells of the 5x5 under the start at y-1 and y-2, as {:pos :name}."
  [c start]
  (vec (for [dy [-1 -2] dx (range -2 3) dz (range -2 3)
             :let [pos (around start [dx dy dz])
                   name (u/block-name (:primitives c) pos)]
             :when (and name (not (not-solid name)))]
         {:pos pos :name name})))

(defn finish!
  "Emit the outcome, hand it to the parent and end the job."
  [c]
  (let [{:keys [goal reason mended dig-reason resumes]} (ctx/mem c)
        got (max 0 (- (carried c) (- goal (:count (:args c)))))
        why (cond-> {} dig-reason (assoc :dig-reason dig-reason) resumes (assoc :resumes resumes))]
    (ctx/emit! c :mine.done :info (merge {:got got :reason reason :mended (or mended 0)
                                          :text (str "mine done: " (name reason) ", got " got ", mended " (or mended 0))}
                                         why))
    (ctx/result! c (merge {:got got :reason reason} why))
    :done))

(defn to-mend!
  "End the dig phase with a reason: mend next, or finish when there is nothing to mend."
  [c reason]
  (ctx/update-mem! c assoc :phase :mend :reason reason :at-mend (carried c))
  (if (:mend (:args c))
    :continue
    (finish! c)))

(defn skip! [c pos] (ctx/update-mem! c update :skipped (fnil conj []) pos))

(defn ^:async equip! [c]
  (let [tool (tools/best-tool (map :name (u/inventory (:primitives c))) (:block (:args c)))]
    (when (and tool (not= tool (.-held (.self (:primitives c)))))
      (await (ctx/act c :equip (clj->js {:item tool :dest "hand"}))))))

(defn ^:async collect! [c]
  (let [{:keys [collect-radius]} (:args c)
        r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                 {:radius collect-radius :filter [(item-name (:args c))]}))]
    (when (= :done r)
      (let [now (carried c)]
        (ctx/update-mem! c (fn [m]
                             (-> m
                                 (dissoc :collecting)
                                 (assoc :dry (if (> now (:last-carried m 0)) 0 (inc (:dry m 0)))
                                        :last-carried now))))))
    :continue))

(defn refused!
  "The rules refused pos (verdict v) right before the dig, no failure: a hazard skips it; a zone or a plan keeps it
  out of the targets anyway, and counted among the refused."
  [c pos v verdict]
  (when (= :hazard verdict) (skip! c pos))
  (ctx/emit! c :mine.refused :info (merge {:pos pos :verdict verdict}
                                          (select-keys v [:reason :zone :plan :hazards])
                                          {:text (str "mine left " (access/cell pos) ": " (name verdict))})))

(defn ^:async dig! [c pos]
  (await (equip! c))
  (let [v (access/may-dig? (access/rules-input c) pos)
        verdict (access/judge v (:accept (:args c)))]
    (if (not= :ok verdict)
      (refused! c pos v verdict)
      (let [status (.-status (await (ctx/act c :dig (clj->js {:pos pos}))))]
        (cond
          (= "dug" status) (ctx/update-mem! c assoc :failures 0 :collecting true)
          (= "missing" status) nil
          (= "cannot" status) (skip! c pos)
          :else (do (skip! c pos) (ctx/update-mem! c update :failures (fnil inc 0))))))
    :continue))

(def max-partials 3)

(defn skip-failed!
  "Skip the target and count a failure."
  [c pos]
  (skip! c pos)
  (ctx/update-mem! c #(-> % (update :failures (fnil inc 0)) (dissoc :partials :partial-pos))))

(defn partial!
  "Count a partial walk toward pos (reset when the target changed); the third in a row skips it."
  [c pos]
  (let [m (ctx/mem c)
        n (if (= pos (:partial-pos m)) (inc (:partials m 0)) 1)]
    (if (>= n max-partials)
      (skip-failed! c pos)
      (ctx/update-mem! c assoc :partials n :partial-pos pos))
    :continue))

(defn ^:async dig-round! [c]
  (let [{:keys [goal failures dry]} (ctx/mem c)
        {:keys [max-failures wet dry-digs]} (:args c)
        {:keys [targets refused wet?]} (scan c)
        pos (first targets)]
    (cond
      (>= (carried c) goal) (to-mend! c :count)
      (>= (or dry 0) dry-digs) (do (ctx/emit! c :mine.gave-up :warn {:reason :no-drops :dry dry :text (str "mine gave up: " dry " digs brought nothing")})
                                   (to-mend! c :no-drops))
      (>= failures max-failures) (do (ctx/emit! c :mine.gave-up :warn {:failures failures :text (str "mine gave up after " failures " failures")})
                                     (to-mend! c :gave-up))
      (and (nil? pos) (seq refused))
      (do (access/decline! c :mine.declined "mine" (assoc (access/refusal-fields refused) :reason :refused))
          (to-mend! c :refused))
      (nil? pos) (to-mend! c (if (and wet? (not wet)) :wet :none))
      :else (let [walked (await (u/walk-near! c pos reach))]
              (cond
                (= :blocked walked) (do (skip-failed! c pos) :continue)
                (= :partial walked) (partial! c pos)
                :else (do (ctx/update-mem! c dissoc :partials :partial-pos)
                          (await (dig! c pos))))))))

;; ------------------------------------------------------------------ mend

(defn owed
  "The ground entries whose cell is now air, cave_air or water (unloaded cells are not owed)."
  [c]
  (filterv #(let [name (u/block-name (:primitives c) (:pos %))]
              (and name (or (air name) (= "water" name))))
           (:ground (ctx/mem c))))

(defn filler
  "The first carried block to fill with: dig-in's building blocks other than the
  mined item, then the item itself when it is a block (the mend must not eat the count)."
  [c]
  (let [{:keys [block] :as a} (:args c)
        item (item-name a)
        own (when (or (some #{item} dig-in/building-blocks) (= item block)) [item])]
    (dig-in/pick c (concat (remove #{item} dig-in/building-blocks) own))))

(defn mend-target
  "The owed cell to fill next: not the body's feet or head, lowest y first, then nearest."
  [c cells]
  (let [feet (cell-of (u/self-pos c))
        head (update feet :y inc)
        here (u/self-pos c)]
    (->> cells
         (map :pos)
         (remove #(or (= % feet) (= % head)))
         (sort-by (juxt :y #(u/dist here %)))
         first)))

(defn mend-fail! [c] (ctx/update-mem! c update :mend-failures (fnil inc 0)))

(defn ^:async place! [c pos item]
  (let [walked (if (> (u/dist (u/self-pos c) pos) mend-reach)
                 (await (u/walk-near! c pos reach))
                 :there)]
    (if (not= :there walked)
      (do (when (= :blocked walked) (mend-fail! c)) :continue)
      (let [status (.-status (await (ctx/act c :place (clj->js {:pos pos :item item}))))]
        (cond
          (= "placed" status) (ctx/update-mem! c #(-> % (assoc :mend-failures 0) (update :mended (fnil inc 0))))
          (= "occupied" status) (ctx/update-mem! c assoc :mend-failures 0)
          :else (mend-fail! c))
        :continue))))

(defn ^:async raise! [c item]
  (let [status (.-status (await (ctx/act c :jumpPlace (clj->js {:item item :count 1}))))]
    (if (contains? #{"done" "partial"} status)
      (ctx/update-mem! c update :mended (fnil inc 0))
      (mend-fail! c))
    :continue))

(defn cells-text [cells] (str/join " " (map #(str (:x (:pos %)) "," (:y (:pos %)) "," (:z (:pos %))) cells)))

(defn spent-on-mend?
  "Whether the mend ate into what the dig brought, leaving fewer than the goal carried."
  [c]
  (let [{:keys [goal at-mend]} (ctx/mem c)
        now (carried c)
        start (- goal (:count (:args c)))]
    (and (< now goal) (< now at-mend) (> at-mend start))))

(def max-resumes 2)

(defn mended!
  "Nothing is owed: dig on when the mend spent the count and targets off the ground remain, else finish."
  [c]
  (let [{:keys [resumes reason]} (ctx/mem c)
        more? (some #(off-ground? c %) (:targets (scan c)))]
    (cond
      (not (spent-on-mend? c)) (finish! c)
      (and more? (< (or resumes 0) max-resumes))
      (do (ctx/update-mem! c #(-> % (assoc :phase :dig :failures 0 :dry 0 :last-carried (carried c) :mend-failures 0)
                                  (dissoc :collecting :partials :partial-pos)
                                  (update :resumes (fnil inc 0))))
          :continue)
      :else (do (ctx/update-mem! c assoc :reason :spent-on-mend :dig-reason reason)
                (finish! c)))))

(defn ^:async mend-round! [c]
  (let [cells (owed c)
        item (when (seq cells) (filler c))
        pos (mend-target c cells)]
    (cond
      (empty? cells) (mended! c)
      (>= (:mend-failures (ctx/mem c) 0) max-mend-failures)
      (do (ctx/emit! c :mine.mend-failed :warn {:owed (mapv :pos cells) :text (str "mine could not mend: " (cells-text cells))})
          (finish! c))
      (nil? item)
      (do (ctx/emit! c :mine.mend-short :warn {:owed (mapv :pos cells) :text (str "mine has nothing to mend with: " (cells-text cells))})
          (finish! c))
      (some? pos) (await (place! c pos item))
      :else (await (raise! c item)))))

(defn no-tool?
  "Whether the block needs a pickaxe and none is carried (shovel and axe blocks drop by hand)."
  [c]
  (and (= "pickaxe" (tools/tool-kind (:block (:args c))))
       (not-any? #(str/ends-with? (:name %) "_pickaxe") (u/inventory (:primitives c)))))

(defn no-tool!
  "End at once, before any dig: warn and hand over {:got 0 :reason :no-tool :tool \"pickaxe\"}."
  [c]
  (ctx/emit! c :mine.no-tool :warn {:tool "pickaxe" :text (str "mine has no pickaxe for " (:block (:args c)))})
  (ctx/emit! c :mine.done :info {:got 0 :reason :no-tool :tool "pickaxe" :mended 0 :text "mine done: no-tool, got 0, mended 0"})
  (ctx/result! c {:got 0 :reason :no-tool :tool "pickaxe"})
  :done)

(defn ^:async round [c]
  (let [m (ctx/mem c)]
    (cond
      (and (nil? (:phase m)) (no-tool? c)) (no-tool! c)

      (nil? (:phase m))
      (let [now (carried c)
            start (cell-of (u/self-pos c))]
        (ctx/update-mem! c assoc
                         :goal (+ now (:count (:args c))) :start start :failures 0 :dry 0 :last-carried now
                         :ground (if (:mend (:args c)) (snapshot c start) [])
                         :phase :dig)
        :continue)

      (= :mend (:phase m)) (await (mend-round! c))
      (:collecting m) (await (collect! c))
      :else (await (dig-round! c)))))
