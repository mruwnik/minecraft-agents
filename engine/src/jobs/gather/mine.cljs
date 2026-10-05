(ns jobs.gather.mine
  (:require [engine.jobs.tidy :as tidy]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.gate :as gate]
            [engine.jobs.tools :as tools]
            [engine.access.rules :as rules]
            [engine.jobs.util :as u]
            [engine.path.near :as near]
            [engine.jobs.look :refer [cell-of headings heading-name facing glance! look-around!]]
            [jobs.survival.dig-in :as dig-in]))

(def doc
  "Mine like a player: dig out :count more of one block kind (:block) that the body has seen, strip-tunnelling at its
  own level to find more, then mend the pit under where it started.
  Targets come only from perception (the primitives' seenBlocks: what the body has seen, never x-ray): a remembered
  :block within :radius that is still there and has at least one of its 6 face neighbours air or cave_air (a block
  with no air face is never a target); it is skipped when a neighbour is lava, or water unless :wet. Without a
  perception the body sees nothing and only tunnels.
  A pickaxe block (by engine.jobs.tools/tool-kind) with no *_pickaxe carried ends at once, before any write or dig:
  warn mine.no-tool with :tool \"pickaxe\", hand over {:got 0 :reason :no-tool :tool \"pickaxe\"} (shovel and axe
  blocks drop by hand). Check: a phase is in memory, or :block is named, unless every seen target is refused (below).
  The first round writes the goal (carried + :count), the start cell, the tunnel's heading (:direction, else the way
  the body faces) and the ground (the solid cells of the 5x5 under the start at y-1 and y-2, empty when :mend is
  false) to job memory before any dig, so a cut or a restart still mends. Then, one step per round, in order:
  (1) a collecting flag runs the collect-drops child for the item within :collect-radius (walking to each drop until
  it is picked up; the count is the inventory's, never the dig's drop report), and drops of the item still lying
  there afterwards (out of reach) are reported once each, info mine.left-behind {:items [{:pos :count}]}, and listed
  in the result's :left; (2) carrying the goal ends :count; (3) :max-failures failures end :gave-up (warn
  mine.gave-up); :dry-digs digs in a row after which the carried count of the item did not rise end :no-drops (warn
  mine.gave-up with :reason :no-drops); (4) the nearest target (those over the ground snapshot only after all
  others, so the floor under the start is dug last) is walked to (within 3: blocked skips it and counts a failure,
  partial tries again, the third partial in a row skips it like blocked), the best carried tool of the kind is
  equipped and the block dug: dug resets the failures and starts collecting, missing does nothing, cannot (bedrock)
  skips it without a failure, anything else skips it and counts one; (5) with no target the body looks around from
  where it stands (each heading, level and down at the floor ahead, a sight pass after each look), once per cell
  and again after each dig, so a vein's next block comes into view (it also looks around once before the first
  target); (6) still none: the strip tunnel.
  The strip tunnel: a 1-wide 2-high straight run at the level the body stands on when it starts, along :direction
  (north, south, east, west or n/s/e/w; default the way the body faced when the job started), at most :tunnel-length
  blocks per job (0: no tunnel; then no target ends :wet when a seen block was rejected only for water, else :none).
  Each step stands on the last cell of the run, judges the next cell and the one over it (a fluid in it, lava beside
  it, or water beside it unless :wet; a floor that is solid; the zone and plan rules, with only :accept hazards),
  digs them head first (a cut of :block is collected like a target), looks ahead level and down at the new cells
  so perception records the walls, floor, roof and face they exposed, and steps in. Whatever ore comes into view is
  then an ordinary target and is dug before the next step (veins followed by the look-around). The run used up ends
  the dig phase :tunnel-length, any stop ends it :tunnel-stopped; either way one warn mine.tunnel-end {:reason
  (:tunnel-length, :lava, :water, :fluid, :no-floor, :not-loaded, :refused, :dig-failed, :walk-failed) :heading
  :length :at :next}, :at the offending cell (the lava beside the line), :next the line cell it stopped before. The
  result carries :tunnel {:origin :heading :steps :stop}; every job ends, after the mend, by walking back to the cell it started on (info mine.not-home when the walk does not arrive).
  The mend phase fills every ground cell that is now air, cave_air or water with the first carried of dig-in's
  building blocks other than the item, the item itself last (when it is a building block or the block itself),
  lowest first, then nearest, never the body's feet or head cell; when only those are owed it jumpPlaces one block.
  Nothing to fill with warns mine.mend-short, six failed fills warn mine.mend-failed; both end the job. The item is
  :item, else the drop-item table, else the block name. When nothing is owed and the mend left fewer than the goal
  carried (it spent what the dig brought), the body looks around once from where it stands, and the job goes back to
  the dig phase (ground kept, :resumes + 1, at most 2) while a seen target off the ground remains in :radius; otherwise it ends :spent-on-mend, keeping the earlier
  reason in :dig-reason. Hands over {:got n :reason r} (plus :dig-reason, :resumes, :tunnel and :left when set;
  info mine.done with :mended, the cells filled); :got is how many more are carried than at the start, at least 0.
  Zones and plans (engine.access.rules, through engine.jobs.access): a target must also be a dig the rules permit,
  with only :accept hazards, when it is chosen and again right before the dig. A cell inside a zone that does not
  allow :dig, or in an active plan's footprint, is never a target; one refused right before the dig is skipped
  without a failure (info mine.refused), as is one with a hazard not accepted. Seen blocks all refused: before the
  first round the check declines; in the job the tunnel goes on (or, with :tunnel-length 0, the dig phase ends
  :refused); either way one mine.declined warn per job names the zones and plans ({:reason :refused :zones :plans}).
  No zone list (zones.edn missing or never valid) declines the check with one mine.declined warn {:reason
  :no-zones}, also in the middle of the job; nothing is dug or placed then. The mend asks the zone rules too: an
  owed cell in a zone or claim of another owner that does not let others place is not filled (one mine.declined
  warn {:reason :refused}), and a mend with no cell left to fill ends as one with nothing owed.")

(def args
  {:block {:doc "name of the block to mine (required)" :default nil}
   :item {:doc "the item the block drops; nil: the drop-item table, else the block name" :default nil}
   :count {:doc "how many more to carry than at the start" :default 8}
   :radius {:doc "seen blocks within this many blocks of the body count" :default 16}
   :wet {:doc "dig blocks that touch water, and tunnel beside water" :default false}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :mend {:doc "fill the ground under the start again afterwards" :default true}
   :collect-radius {:doc "how far around to collect drops after a dig" :default 6}
   :max-failures {:doc "failures in a row before giving up" :default 3}
   :dry-digs {:doc "digs in a row after which the carried count of the item did not rise before giving up (:no-drops)" :default 3}
   :direction {:doc "the strip tunnel's heading: north, south, east or west (n/s/e/w); nil: the way the body faces when the job starts" :default nil}
   :tunnel-length {:doc "the most blocks the strip tunnel runs in this job, at the body's level; 0: no tunnel, seen blocks only" :default 32}
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
  "How the cell of a block would be dug: :ok, :wet (only water stops it), :buried (no air face) or :no (lava
  beside it)."
  [c wet pos]
  (let [names (map #(u/block-name (:primitives c) (around pos %)) faces)]
    (cond
      (not-any? air names) :buried
      (some #{"lava"} names) :no
      (and (some #{"water"} names) (not wet)) :wet
      :else :ok)))

(defn seen-cells
  "The cells perception remembers holding block within radius of the body, nearest first ([] without a perception)."
  [p block radius]
  (if-let [f (aget p "seenBlocks")]
    (mapv #(u/pos-of (.-pos %)) (array-seq (.call f p #js {:radius radius :names #js [block] :max 128})))
    []))

(defn scan
  "{:targets [pos] nearest first (the higher of two as near), those over the ground snapshot after all others, :refused [verdict] of blocks a
  zone or plan refuses, :wet? true when an unskipped block was rejected only for water}. Only seen blocks that are
  still there count."
  [c]
  (let [{:keys [block radius wet accept]} (:args c)
        p (:primitives c)
        skipped (set (:skipped (ctx/mem c)))
        ground (into #{} (map :pos) (:ground (ctx/mem c)))
        here (u/self-pos c)
        cells (->> (seen-cells p block radius)
                   (remove skipped)
                   (filter #(= block (u/block-name p %))))
        graded (map (juxt identity #(classify c wet %)) cells)
        in (access/rules-input c)
        judged (->> graded
                    (filter #(= :ok (second %)))
                    (map (fn [[pos]] (let [v (access/may-dig? in pos)] [pos v (access/judge v accept)]))))]
    {:targets (->> judged (filter #(= :ok (nth % 2))) (map first) (sort-by (juxt #(if (ground %) 1 0) #(u/dist here %) #(- (:y %)))) vec)
     :refused (into [] (comp (filter #(= :refused (nth % 2))) (map second)) judged)
     :wet? (boolean (some #(= :wet (second %)) graded))}))

(defn off-ground?
  "Whether a target is not one of the ground snapshot's cells."
  [c pos]
  (not-any? #(= pos (:pos %)) (:ground (ctx/mem c))))

(defn check [c]
  (cond
    (and (nil? (ctx/zones c)) (not (:ignore-zones? (:args c)))) (access/decline! c :mine.declined "mine" {:reason :no-zones})
    (:phase (ctx/mem c)) true
    (not (:block (:args c))) false
    :else (let [{:keys [targets refused]} (scan c)]
            (if (and (empty? targets) (seq refused))
              (access/decline! c :mine.declined "mine" (assoc (access/refusal-fields refused) :reason :refused))
              true))))


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
  (let [{:keys [goal reason mended dig-reason resumes tunnel left]} (ctx/mem c)
        got (max 0 (- (carried c) (- goal (:count (:args c)))))
        why (cond-> {} dig-reason (assoc :dig-reason dig-reason) resumes (assoc :resumes resumes)
              (pos? (:steps tunnel 0)) (assoc :tunnel (-> tunnel (select-keys [:origin :heading :steps :stop]) (update :origin access/cell)))
              (seq left) (assoc :left (mapv (fn [[pos n]] {:pos pos :count n}) left)))]
    (ctx/emit! c :mine.done :info (merge {:got got :reason reason :mended (or mended 0)
                                          :text (str "mine done: " (name reason) ", got " got ", mended " (or mended 0))}
                                         why))
    (ctx/result! c (merge {:got got :reason reason} why))
    :done))

(defn wrap-up!
  "The normal end: the body walks back to where it started (phase :home), whatever the run did, then finishes."
  [c]
  (ctx/update-mem! c assoc :phase :home)
  :continue)

(defn to-mend!
  "End the dig phase with a reason: mend next, or wrap up when there is nothing to mend."
  [c reason]
  (ctx/update-mem! c assoc :phase :mend :reason reason :at-mend (carried c))
  (if (:mend (:args c))
    :continue
    (wrap-up! c)))

(defn skip! [c pos] (ctx/update-mem! c update :skipped (fnil conj []) pos))

(defn ^:async equip!
  "Hold the best carried tool for block (the mined block when not given)."
  ([c] (equip! c (:block (:args c))))
  ([c block]
   (let [tool (tools/best-tool (map :name (u/inventory (:primitives c))) block)]
     (when (and tool (not= tool (.-held (.self (:primitives c)))))
       (await (ctx/act c :equip (clj->js {:item tool :dest "hand"})))))))

(defn left-behind
  "Drops of the item lying within :collect-radius, as {[x y z] count}."
  [c]
  (let [item (item-name (:args c))]
    (into {}
          (comp (filter #(= item (some-> (.-item %) .-name)))
                (map (fn [e] [(access/cell (cell-of (u/pos-of (.-pos e)))) (or (some-> (.-item e) .-count) 1)])))
          (array-seq (.entities (:primitives c) #js {:radius (:collect-radius (:args c)) :kind "item" :max 32})))))

(def pickup-grace-ms 400)

(defn ^:async note-left!
  "Report drops of the item collect-drops left lying (out of reach), once per cell. A drop still listed right after
  the collect may be one whose pickup packet has not arrived, so a drop counts only when it is still lying after a
  short wait."
  [c]
  (let [known (:left (ctx/mem c))
        fresh? (fn [[pos _]] (not (contains? known pos)))
        first-look (into {} (filter fresh?) (left-behind c))
        fresh (if (empty? first-look)
                first-look
                (do (await (ctx/act c :wait #js {:ms pickup-grace-ms}))
                    (into {} (filter (fn [[pos _]] (contains? first-look pos))) (left-behind c))))]
    (when (seq fresh)
      (ctx/emit! c :mine.left-behind :info {:items (mapv (fn [[pos n]] {:pos pos :count n}) fresh)
                                            :text (str "mine left drops it could not pick up at "
                                                       (str/join " " (map (fn [[pos _]] (str/join "," pos)) fresh)))})
      (ctx/update-mem! c update :left merge fresh))))

(defn ^:async collect! [c]
  (let [{:keys [collect-radius]} (:args c)
        r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                 {:radius collect-radius :filter [(item-name (:args c))]}))]
    (when (= :done r)
      (await (note-left! c))
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
                                          (select-keys v [:reason :zone :claim :owner :plan :hazards])
                                          {:text (str "mine left " (access/cell pos) ": " (name verdict))})))

(defn ^:async dig! [c pos]
  (await (equip! c))
  (let [v (access/may-dig? (access/rules-input c) pos)
        verdict (access/judge v (:accept (:args c)))]
    (if (not= :ok verdict)
      (refused! c pos v verdict)
      (let [status (.-status (await (tidy/dig! c pos)))]
        (cond
          (= "dug" status) (ctx/update-mem! c assoc :failures 0 :collecting true :looked :dug)
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

;; ------------------------------------------------------------------ the strip tunnel

(defn cut-hazard
  "Why the tunnel may not take cell pos (a fluid in it, lava or unwanted water beside it, not loaded), else nil:
  {:reason r :at cell}, the cell being the offending one (the lava beside pos, not pos itself)."
  [c pos]
  (let [own (u/block-name (:primitives c) pos)
        beside (map (fn [f] (let [cell (around pos f)] [cell (u/block-name (:primitives c) cell)])) faces)
        beside-of (fn [n] (some (fn [[cell nm]] (when (= n nm) cell)) beside))]
    (cond
      (nil? own) {:reason :not-loaded :at pos}
      (rules/fluids own) {:reason :fluid :at pos}
      (beside-of "lava") {:reason :lava :at (beside-of "lava")}
      (and (beside-of "water") (not (:wet (:args c)))) {:reason :water :at (beside-of "water")})))

(defn step-cell
  "The feet cell k blocks along the tunnel from its origin."
  [{:keys [origin heading]} k]
  (let [[dx dz] (headings heading)]
    (-> origin (update :x + (* k dx)) (update :z + (* k dz)))))

(defn tunnel-reason [stop] (if (= :tunnel-length stop) :tunnel-length :tunnel-stopped))

(defn tunnel-end!
  "The tunnel is done (stop: :tunnel-length or why it stopped at cell at): one warn mine.tunnel-end, then the mend."
  [c stop at & [next]]
  (let [{:keys [goal tunnel]} (ctx/mem c)
        {:keys [heading steps]} tunnel
        got (max 0 (- (carried c) (- goal (:count (:args c)))))]
    (ctx/emit! c :mine.tunnel-end :warn
               (cond-> {:reason stop :heading heading :length steps :got got
                        :text (str "mine's tunnel ended after " steps " blocks " heading ": "
                                   (if (= :tunnel-length stop) "its length is used up" (str "stopped, " (name stop)))
                                   (when at (str " at " (str/join "," (access/cell at))))
                                   "; got " got " of " (:count (:args c)))}
                 at (assoc :at (access/cell at))
                 next (assoc :next (access/cell next))))
    (ctx/update-mem! c assoc-in [:tunnel :stop] stop)
    (to-mend! c (tunnel-reason stop))))

(defn ^:async step-to!
  "Walk to the centre of cell pos: true when there."
  [c pos]
  (or (= (cell-of (u/self-pos c)) pos)
      (= "arrived" (.-status (await (ctx/act c :moveTo (clj->js {:pos {:x (+ (:x pos) 0.5) :y (:y pos) :z (+ (:z pos) 0.5)}
                                                                  :range 0})))))))

(defn ^:async cut!
  "Dig the cells in order (equip, rules, tidy): :ok when each is air afterwards, else :refused or :dig-failed. A cut
  of the mined block is collected like a target."
  [c cells]
  (loop [cells cells]
    (if-let [pos (first cells)]
      (let [block (u/block-name (:primitives c) pos)
            _ (await (equip! c block))
            v (access/may-dig? (access/rules-input c) pos)
            verdict (access/judge v (:accept (:args c)))
            status (when (= :ok verdict) (.-status (await (tidy/dig! c pos))))]
        (cond
          (not= :ok verdict) (do (refused! c pos v verdict) :refused)
          (not (#{"dug" "missing"} status)) :dig-failed
          :else (do (when (and (= "dug" status) (= block (:block (:args c))))
                      (ctx/update-mem! c assoc :collecting true))
                    (recur (rest cells)))))
      :ok)))

(defn ^:async tunnel-round!
  "One step of the strip tunnel: back onto its last cell, judge and cut the next two, look at them, step in."
  [c]
  (let [m (ctx/mem c)
        t (or (:tunnel m) {:origin (cell-of (u/self-pos c)) :heading (:heading m) :steps 0})
        _ (when-not (:tunnel m) (ctx/update-mem! c assoc :tunnel t))
        {:keys [steps stop heading]} t
        name-at #(u/block-name (:primitives c) %)
        from (step-cell t steps)
        next (step-cell t (inc steps))
        cut [(update next :y inc) next]
        hazard (some #(cut-hazard c %) cut)]
    (cond
      stop (to-mend! c (tunnel-reason stop))
      (>= steps (:tunnel-length (:args c))) (tunnel-end! c :tunnel-length nil)
      (not (await (step-to! c from))) (tunnel-end! c :walk-failed from)
      hazard (tunnel-end! c (:reason hazard) (:at hazard) next)
      (not (rules/solid-floor? name-at (update next :y dec))) (tunnel-end! c :no-floor next)
      :else (let [r (await (cut! c (remove #(air (name-at %)) cut)))]
              (if (not= :ok r)
                (tunnel-end! c r next)
                (do (await (glance! c [(headings heading)]))
                    (if (await (step-to! c next))
                      (do (ctx/update-mem! c #(-> % (update-in [:tunnel :steps] inc) (assoc :looked next)))
                          :continue)
                      (tunnel-end! c :walk-failed next))))))))

;; ------------------------------------------------------------------ the dig phase

(defn ^:async dig-round! [c]
  (let [{:keys [goal failures dry looked]} (ctx/mem c)
        {:keys [max-failures wet dry-digs tunnel-length]} (:args c)
        {:keys [targets refused wet?]} (scan c)
        pos (first targets)]
    (cond
      (>= (carried c) goal) (to-mend! c :count)
      (>= (or dry 0) dry-digs) (do (ctx/emit! c :mine.gave-up :warn {:reason :no-drops :dry dry :text (str "mine gave up: " dry " digs brought nothing")})
                                   (to-mend! c :no-drops))
      (>= failures max-failures) (do (ctx/emit! c :mine.gave-up :warn {:failures failures :text (str "mine gave up after " failures " failures")})
                                     (to-mend! c :gave-up))
      (nil? looked) (await (look-around! c))
      (some? pos) (let [walked (await (near/walk-near! c pos reach))]
                    (cond
                      (= :blocked walked) (do (skip-failed! c pos) :continue)
                      (= :partial walked) (partial! c pos)
                      :else (do (ctx/update-mem! c dissoc :partials :partial-pos)
                                (await (dig! c pos)))))
      (not= looked (cell-of (u/self-pos c))) (await (look-around! c))
      :else (do (when (seq refused)
                  (access/decline! c :mine.declined "mine" (assoc (access/refusal-fields refused) :reason :refused)))
                (cond
                  (pos? tunnel-length) (await (tunnel-round! c))
                  (seq refused) (to-mend! c :refused)
                  :else (to-mend! c (if (and wet? (not wet)) :wet :none)))))))

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

(defn mend-permitted
  "The owed cells ({:pos ..}) the job may place on (zones and claims; one mine.declined warn per job when refused)."
  [c cells]
  (let [ok (set (gate/allowed c :mine.declined "mine" :place (map :pos cells)))]
    (filterv #(ok (:pos %)) cells)))

(defn ^:async place! [c pos item]
  (let [walked (if (> (u/dist (u/self-pos c) pos) mend-reach)
                 (await (near/walk-near! c pos reach))
                 :there)]
    (if (not= :there walked)
      (do (when (= :blocked walked) (mend-fail! c)) :continue)
      (let [status (if (gate/allowed? c :mine.declined "mine" :place pos)
                     (.-status (await (ctx/act c :place (clj->js {:pos pos :item item}))))
                     "refused")]
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

(defn ^:async mended!
  "Nothing is owed. The mend spent the count: look around once from here, then dig on while targets off the ground
  remain (at most max-resumes times). Else finish."
  [c]
  (let [{:keys [resumes reason looked]} (ctx/mem c)
        more? (some #(off-ground? c %) (:targets (scan c)))]
    (cond
      (not (spent-on-mend? c)) (wrap-up! c)
      (not= looked (cell-of (u/self-pos c))) (await (look-around! c))
      (and more? (< (or resumes 0) max-resumes))
      (do (ctx/update-mem! c #(-> % (assoc :phase :dig :failures 0 :dry 0 :last-carried (carried c) :mend-failures 0)
                                  (dissoc :collecting :partials :partial-pos)
                                  (update :resumes (fnil inc 0))))
          :continue)
      :else (do (ctx/update-mem! c assoc :reason :spent-on-mend :dig-reason reason)
                (wrap-up! c)))))

(defn ^:async mend-round! [c]
  (let [cells (mend-permitted c (owed c))
        item (when (seq cells) (filler c))
        pos (mend-target c cells)]
    (cond
      (empty? cells) (await (mended! c))
      (>= (:mend-failures (ctx/mem c) 0) max-mend-failures)
      (do (ctx/emit! c :mine.mend-failed :warn {:owed (mapv :pos cells) :text (str "mine could not mend: " (cells-text cells))})
          (wrap-up! c))
      (nil? item)
      (do (ctx/emit! c :mine.mend-short :warn {:owed (mapv :pos cells) :text (str "mine has nothing to mend with: " (cells-text cells))})
          (wrap-up! c))
      (some? pos) (await (place! c pos item))
      :else (await (raise! c item)))))

(defn ^:async home-round!
  "The last step of every normal run: back to the cell it started on (moveTo), whatever the digging left
  open behind it; an info mine.not-home when the walk did not arrive. The job ends either way."
  [c]
  (let [{:keys [start]} (ctx/mem c)]
    (when-not (await (step-to! c start))
      (ctx/emit! c :mine.not-home :info {:to (access/cell start) :at (access/cell (cell-of (u/self-pos c)))
                                         :text (str "mine did not get back to " (str/join "," (access/cell start)))}))
    (finish! c)))

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
  (let [m (ctx/mem c)
        heading (heading-name (:direction (:args c)))]
    (cond
      (and (nil? (:phase m)) (no-tool? c)) (no-tool! c)

      (and (nil? (:phase m)) (:direction (:args c)) (nil? heading))
      (do (ctx/emit! c :mine.done :warn {:got 0 :reason :bad-direction :direction (:direction (:args c))
                                         :text (str "mine: no such direction " (pr-str (:direction (:args c))) "; north, south, east or west")})
          (ctx/result! c {:got 0 :reason :bad-direction})
          :done)

      (nil? (:phase m))
      (let [now (carried c)
            start (cell-of (u/self-pos c))]
        (ctx/update-mem! c assoc
                         :goal (+ now (:count (:args c))) :start start :failures 0 :dry 0 :last-carried now
                         :heading (or heading (facing (:primitives c)))
                         :ground (if (:mend (:args c)) (snapshot c start) [])
                         :phase :dig)
        :continue)

      (= :home (:phase m)) (await (home-round! c))
      (= :mend (:phase m)) (await (mend-round! c))
      (:collecting m) (await (collect! c))
      :else (await (dig-round! c)))))
