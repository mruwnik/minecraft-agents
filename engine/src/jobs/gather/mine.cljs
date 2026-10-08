(ns jobs.gather.mine
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.game :as game]
            [jobs.lib.blocks :as blocks]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.body :as body]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.gate :as gate]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.tools :as tools]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.trip :as trip]
            [jobs.lib.targets :as targets]
            [jobs.lib.watch :as watch]
            [jobs.lib.reach :as reach-lib]
            [jobs.lib.look :as look :refer [cell-of headings heading-name facing glance! look-around!]]
            [jobs.lib.breath :as breath]
            [jobs.lib.torch :as torch]
            [jobs.lib.placement :as placement]
            [jobs.lib.world :as known]))

(def doc
  "Mine like a player: dig out :count more of one block kind (:block) that the body has seen, strip-tunnelling
  to find more (for an ore at its ore level, :level), then mend the pit under where it started and walk back to the start cell.
  An ore's stone and deepslate kinds both count (:variants): {:block \"iron_ore\" :count 8} gets 8 raw_iron from iron_ore and
  deepslate_iron_ore.

  Targets come only from perception (the primitives' seenBlocks, never x-ray). A target is a remembered :block
  within :radius that is still there and has at least one of its 6 face neighbours seen as air or cave_air. It is
  skipped when a seen neighbour is lava, or water unless :wet; lava on top counts as seen (it drips); a face never seen is no hazard. Without perception the body sees nothing and only
  tunnels.

  Declines and ends early:
  - Health below jobs.lib.body/risky-min-health (6): the check waits {:reason :low-health}, also for a resumed job.
  - The head in water (jobs.lib.breath/head-under?) before a dig: no more digging, warn mine.flooded; before the first dig the job ends
    {:status :stopped :got 0 :reason :flooded}, later the dig phase ends :flooded and the body walks home.
  - A pickaxe block (jobs.lib.tools/tool-kind) with no *_pickaxe carried, at the start or later (the pick broke),
    runs jobs.items.get-tool as a child first (unless :fetch is false). Nothing got: before any dig it ends at once, warn mine.no-tool,
    {:status :stopped :got 0 :reason :no-tool :tool <kind needed: pickaxe, sword, shears>}; in the dig phase the mend and walk home follow with
    :reason :no-tool. Shovel and axe blocks drop by hand. Soil or wood dug with only a pickaxe held empties the
    hand first (no wear on the pickaxe).
  - The check passes when a phase is in memory, or :block is named, unless every seen target is refused.
  - No zone list (zones.edn missing or never valid) declines with one warn mine.declined {:reason :no-zones},
    also in the middle of the job. Nothing is dug or placed then.

  Before the first round it deposits at the known chest when the bag is fullish (jobs.lib.trip/ready!; :deposit-first? false skips it).

  The first round writes to job memory before any dig: the goal (carried + :count), the start cell, the
  tunnel heading (:heading, else :direction, else the way the body faces) and the ground (the solid cells of the 5x5 under the
  start at y-1 and y-2; empty when :mend is false). So a cut or restart still mends.

  One call runs these steps in order, again and again until the job ends (it yields with :continue only while a child or
  a walk waits on the world, and after 400 steps):
  1. Collecting: after a dig the body glances at the dug cell, the cell under it and unsensed cells further down (once per cell), then the collect-drops child gathers the item within :collect-radius. The count is the
     inventory's, never the dig's report. Drops of the item still lying afterwards (out of reach) are reported
     once each, info mine.left-behind {:items [{:pos :count}]}. The result's :left is re-checked after the walk
     home and keeps only drops still lying then.
  2. Carrying the goal ends :count.
  3. :max-failures failures end :gave-up (warn mine.gave-up). :dry-digs digs in a row after which the carried
     count did not rise end :no-drops (warn mine.gave-up).
  4. Dig the nearest target by walking (one bounded search, jobs.lib.targets, going on in the next step; a seen
     block out of every stand's reach is passed over; when the search finds none reachable, the nearest in a straight
     line is tried; targets over the ground snapshot come last, so the floor
     under the start is dug last, the block under the feet after all others; a block under the feet whose floor
     is unseen is dug from a cell beside it (the jobs.blocks.dig child), never from above, and skipped when there is
     none (info mine.refused, no failure); targets whose drop lies in a clear line from the eye come first, and a body
     whose line is blocked walks to within 1 when it can). Walk within 3: blocked skips the target and counts a failure, partial tries
     again and the third partial in a row skips it. The best carried tool is equipped. Dug resets the failures
     and starts collecting. Missing does nothing. Cannot (bedrock) skips without a failure. Anything else skips
     and counts one.
  5. With no target the body looks around from where it stands (each heading, level and down at the floor
     ahead), once per cell and after each dig, so a vein's next block comes into view. It also looks around
     once before the first target.
  6. Still none, the block stone-type (stone, cobblestone, deepslate, andesite, granite, diorite, tuff) and the
     tunnel not begun: a stair down (jobs.access.stair child, one step at a time, :fetch) along the heading, at most
     :descend-limit steps, soil dug by hand; each step looks round, so stone that comes into view is a target.
     The limit used up, or the stair stopping, ends :stopped :no-stone-found (warn mine.no-stone-found, result
     :descent {:steps :stop}); the mend and the walk home follow.
  6b. Still none, a tunnel level set (:level, else the ore's ::ore-levels entry, e.g. iron_ore 16; :level false: none) and the
     body above it, the tunnel owed and not begun: a stair down the same way (one step a round, :fetch passed on, a look round
     each step, a torch every :torch-interval steps) until the feet are at the level. The stair stopping ends the descent
     there (info mine.descent-stopped {:steps :stop :y :level}) and the tunnel runs at that height.
  7. Else the strip tunnel, at the level then stood on.

  Strip tunnel: a 1-wide 2-high straight run at the level the body stood on at the start, along :heading or :direction
  (north, south, east, west or n/s/e/w), at most :tunnel-length blocks per job; it starts past cells of the line already open (info
  mine.tunnel-skipped, also down a stair to a floor) and never digs a torch. 0 means no tunnel: then no
  target ends :wet when a seen block was rejected only for water, else :none. Each step stands on the last
  cell of the run and judges the next cell and the one over it: no fluid in it, no seen lava beside it, no seen water
  beside it unless :wet, a solid floor, the zone and plan rules with only :accept hazards. It digs them head
  first (a cut of :block is collected like a target), looks ahead level and down so perception records what
  the cut exposed, and steps in. A cut cell not air after the cut, or a walk stopped with a falling block (gravel, sand)
  in it, is cut again (the column above falls in turn), at most ::max-recuts times per cell, then :dig-failed. Ore that comes into view is then an ordinary target and is dug before the
  next step.

  The run used up ends the dig phase :tunnel-length, any stop ends it :tunnel-stopped. Either way one warn
  mine.tunnel-end {:y (the tunnel's feet height) :reason (:tunnel-length :lava :water :fluid :no-floor :not-loaded :refused :dig-failed
  :walk-failed) :heading :length :at :next :walk}. :at is the offending cell, :next the line cell it stopped before, :walk (walk-failed) the moveTo status, reason and the names seen at the cell and its floor.

  Torches: a dig of seen ore that takes the body 4+ blocks off the tunnel line hangs one torch there the same way
  (none within 4 of an earlier branch torch). The strip tunnel hangs a torch (jobs.access.tunnel/torch-at: a wall torch in the head cell, else a floor
  torch) behind the body at its first step, then every :torch-interval steps, and at the cut's end when none hangs
  within 4 blocks of it; 0 hangs none. Torches are the body's own and stay: nothing takes them back. Under 2
  carried with a coal or charcoal and a stick or logs or planks (sticks are crafted; :fetch is passed on), the torch craft is run as a child (4 torches). With no torch and
  nothing to craft from, one info mine.no-torches (per tunnel and per branch) and the dig goes on dark (it tries again at the next torch
  step, after more coal or sticks are picked up). A place the rules refuse, or that fails, or a branch dig straight above or below the line (:no-site), is an info
  mine.torch-left-out {:cell :reason}.

  Mend: fills every ground cell that is now air, cave_air or water with the first carried of dig-in's building
  blocks other than the item (the item last, when it is a building block), lowest first, then nearest, never the
  body's feet or head cell. When only those are owed it jumpPlaces one block. Nothing to fill with warns
  mine.mend-short. Six failed fills warn mine.mend-failed. Both end the job. A mend with no cell left to fill (also when every owed cell is refused)
  ends as nothing owed. The item is :item, else the
  drop-item table, else the block name. The mend asks the zone rules too: an owed cell in a zone or claim of
  another owner that does not let others place is not filled (one warn mine.declined {:reason :refused}).

  When nothing is owed and the mend left fewer than the goal carried (it spent what the dig brought), the body
  looks around once. The job goes back to the dig phase (ground kept, :resumes + 1, at most 2) while a seen
  target off the ground remains in :radius. Otherwise it ends :spent-on-mend, keeping the earlier reason in
  :dig-reason.

  Every job ends, after the mend, by a go-to walk back to the cell it started on, or onto a seen standable cell beside it when its
  floor is open or unseen (warn mine.not-home when the walk does not arrive; the job then ends {:status :stopped :reason :not-home}, the ore kept, :dig-reason the earlier reason).

  Zones and plans (jobs.lib.access.rules, through jobs.lib.access): a target must be a dig the rules permit,
  with only :accept hazards, when chosen and again right before the dig. A cell in a zone that bars :dig, or in
  an active plan's footprint, is never a target. One refused right before the dig is skipped without a failure
  (info mine.refused), as is one with a hazard not accepted. When all seen blocks are refused: before the
  first round the check declines. In the job the tunnel goes on (with :tunnel-length 0 the dig phase ends
  :refused). One warn mine.declined per job names the zones and plans ({:reason :refused :zones :plans}).

  Hands over {:got n :reason r} (with :status :stopped when :got is 0 or the walk home failed, :reason :not-home: never completed) plus
  :dig-reason, :resumes, :tunnel and :left when set; info mine.done with
  :mended, the cells filled; its text (and the result's text when :got is 0) names the stair's steps and the tunnel's length,
  heading and height (\"; stair down 48 steps; tunnel 32 blocks east at y 16\"); :descent {:steps :stop} is in the result after any descent. With reason :wet (also when a tunnel dug nothing and seen blocks were skipped for water), :wet-skipped counts the seen blocks left for water beside them. Its text says the reason, :got and :mended, and for a tunnel its length, heading, end cell
  and whether the body walked back. :got is how many more are carried than at the start, at least 0. :tunnel is
  {:origin :heading :steps :stop :end :back-at :walked-back?}, :end the cell it ended on before the walk back.")

(a/defargs args
  {:block {:doc "name of the block to mine (required)" :spec a/name? :default nil}
   :item {:doc "the item the block drops; nil: the drop-item table, else the block name" :spec a/item? :default nil}
   :count {:doc "how many more to carry than at the start" :spec (a/int-in 0 nil) :default 8}
   :radius {:doc "seen blocks within this many blocks of the body count" :spec (a/num-in 0 nil) :default 16}
   :wet {:doc "dig blocks that touch water, and tunnel beside water" :spec boolean? :default false}
   :fetch {:doc "get a pickaxe when none is carried (jobs.items.get-tool via jobs.lib.fetch limits): true, a set of kinds or a map of limits; false ends :no-tool" :spec fetch/option? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}
   :deposit-first? {:doc "deposit at the known chest first when the bag is fullish and the chest is near (jobs.lib.trip/ready!)" :spec boolean? :default true}
   :spare-own-builds {:doc "a cell of a plan this body made is never a target; false: it may be dug" :spec boolean? :default true}
   :mend {:doc "fill the ground under the start again afterwards" :spec boolean? :default true}
   :collect-radius {:doc "how far around to collect drops after a dig" :spec (a/num-in 0 nil) :default 6}
   :max-failures {:doc "failures in a row before giving up" :spec (a/int-in 1 nil) :default 3}
   :dry-digs {:doc "digs in a row after which the carried count of the item did not rise before giving up (:no-drops)" :spec (a/int-in 1 nil) :default 3}
   :heading {:doc "the strip tunnel's heading, over :direction: north, south, east or west (n/s/e/w), or a cell [x y z] (the dominant axis from the start toward it); nil: :direction" :spec (a/pred "a heading name or a cell [x y z]" #(or (and (or (string? %) (keyword? %)) (heading-name %)) (and (sequential? %) (= 3 (count %)) (every? number? %)))) :default nil}
   :direction {:doc "the strip tunnel's heading: north, south, east or west (n/s/e/w); nil: the way the body faces when the job starts" :spec (a/pred "north, south, east or west (n/s/e/w), a string or keyword" #(and (or (string? %) (keyword? %)) (heading-name %))) :default nil}
   :tunnel-length {:doc "the most blocks the strip tunnel runs in this job, at the body's level; 0: no tunnel, seen blocks only" :spec (a/int-in 0 nil) :default 32}
   :variants {:doc "an ore's stone and deepslate kinds both count as the block (iron_ore and deepslate_iron_ore, either named); false: only :block" :spec boolean? :default true}
   :level {:doc "the feet height the strip tunnel runs at (a whole number, or a cell [x y z] / {:x :y :z} whose y it takes): with no target seen the body first stairs down to it (jobs.access.stair) when higher; nil: the ore's level (setting ::ore-levels), none for other blocks; false: where the body stands"
           :spec (a/pred "a whole number, a cell [x y z] or {:x :y :z}, nil or false" #(or (nil? %) (false? %) (integer? %) (and (sequential? %) (= 3 (count %)) (integer? (second %))) (and (map? %) (integer? (:y %))))) :default nil}
   :descend-limit {:doc "the most steps of stair down through soil to find stone, when the block is stone-type and none is in sight; 0: never descend" :spec (a/int-in 0 nil) :default 12}
   :torch-interval {:doc "the strip tunnel hangs a torch every this many steps; 0: none" :spec (a/int-in 0 nil) :default 10}
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent :falling-block); the lava and :wet rules above still hold"
            :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent :falling-block}}})

(a/defargs settings
  {::reach {:default 3 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}
   ::mend-reach {:default 4 :doc "Reach while mending a cut, in blocks." :spec (a/int-in 0 nil)}
   ::max-mend-failures {:default 6 :doc "Failed mends before the job gives up." :spec (a/int-in 0 nil)}
   ::pickup-grace-ms {:default 400 :doc "How long the body waits for a drop to be picked up, in ms." :spec (a/int-in 0 nil)}
   ::end-torch-gap {:default 4 :doc "A cut's end gets a torch when the last hangs this many steps back or more." :spec (a/int-in 0 nil)}
   ::branch-gap {:default 4 :doc "A dig this far from the tunnel line (and from any torch hung on a branch) gets a torch of its own." :spec (a/int-in 0 nil)}
   ::torch-min {:default 2 :doc "A torch round crafts more when fewer torches than this are carried." :spec (a/int-in 0 nil)}
   ::torch-craft-count {:default 4 :doc "Torches one craft makes." :spec (a/int-in 1 nil)}
   ::max-resumes {:default 2 :doc "Resumes of a cut before the job gives up." :spec (a/int-in 0 nil)}
   ::max-steps {:default 400 :doc "Pieces of work of one call before it gives the round back with :continue." :spec (a/int-in 0 nil)}
   ::max-recuts {:default 12 :doc "Cuts again of one tunnel cell that a falling block (gravel, sand) refilled before the tunnel stops :dig-failed." :spec (a/int-in 0 nil)}
   ::ore-levels {:default {"iron_ore" 16 "copper_ore" 48 "gold_ore" -16 "lapis_ore" 0 "redstone_ore" -54 "diamond_ore" -54}
                 :doc "The feet height a strip tunnel for an ore runs at when :level is nil, by the ore's stone kind; an ore not listed tunnels where the body stands."
                 :spec (a/map-of string? (a/int-in nil nil))}})
(defn reach [] (settings/get settings ::reach))
(defn mend-reach [] (settings/get settings ::mend-reach))
(defn max-mend-failures [] (settings/get settings ::max-mend-failures))
(def air #{"air" "cave_air"})
(def passable "Blocks a body walks through in a tunnel cell: air and torches (never dug)." #{"air" "cave_air" "torch" "wall_torch"})

(defn base-ore
  "The stone kind of an ore block (deepslate_iron_ore: iron_ore); any other block as it is."
  [block]
  (let [plain (some-> block (str/replace #"^deepslate_" ""))]
    (if (contains? blocks/ores plain) plain block)))

(defn kinds
  "The block names that count as the mined block: an ore's stone and deepslate kinds with :variants, else :block alone."
  [{:keys [block variants]}]
  (let [base (base-ore block)]
    (if (and variants (contains? blocks/ores base)) #{base (str "deepslate_" base)} #{block})))

(defn tunnel-level
  "The feet height the strip tunnel should run at (args a): :level, else the ore's ::ore-levels entry; nil for none (:level false)."
  [{:keys [block level]}]
  (cond (false? level) nil
        (sequential? level) (second level)
        (map? level) (:y level)
        (some? level) level
        :else (get (settings/get settings ::ore-levels) (base-ore block))))
(def not-solid #{"air" "cave_air" "water" "lava" "short_grass" "tall_grass"})
(def faces [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]])

(defn carried
  "Total of the item carried over all stacks."
  [c]
  (let [item (blocks/item-name (:args c))]
    (transduce (comp (filter #(= item (:name %))) (map :count)) + 0 (u/inventory (:primitives c)))))

(defn around [{:keys [x y z]} [dx dy dz]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})

(defn face-names
  "The names of the 6 faces of cell pos, as the body knows them: the last seen block, nil for a cell never seen. The cells
  it stands in are felt, and lava directly on top shows as drips, so those read the world."
  [c pos]
  (let [p (:primitives c)
        feet (cell-of (u/self-pos c))
        own? #(or (= % feet) (= % (update feet :y inc)))]
    (map (fn [f]
           (let [cell (around pos f)
                 seen (look/seen-block p cell)]
             (cond
               (own? cell) (u/seen-name p cell)
               (and (= [0 1 0] f) (= "lava" (u/seen-name p cell))) "lava"
               (not (:unknown seen)) (:name seen))))
         faces)))

(defn classify
  "How the cell of a block would be dug: :ok, :wet (only water stops it), :buried (no seen air face) or :no (seen
  lava beside it, or lava on top). A face never seen is neither air nor a hazard."
  [c wet pos]
  (let [names (face-names c pos)]
    (cond
      (not-any? air names) :buried
      (some #{"lava"} names) :no
      (and (some #{"water"} names) (not wet)) :wet
      :else :ok)))

(defn rules-in
  "The rules input: another's plans always refuse, this body's own too unless :spare-own-builds is false; a base
  block (chest, bed, furnace ...) is never dug."
  [c]
  (assoc (access/rules-input c {:own-plans-ok? (false? (:spare-own-builds (:args c)))}) :keep-base? true))

(defn scan
  "{:targets [pos] nearest first (the higher of two as near), those over the ground snapshot after all others, :refused [verdict] of blocks a
  zone or plan refuses, :wet? true when an unskipped block was rejected only for water, :wet-n how many}. Only seen blocks that are
  still there count."
  [c]
  (let [{:keys [radius wet accept]} (:args c)
        p (:primitives c)
        skipped (set (:skipped (ctx/mem c)))
        ground (into #{} (map :pos) (:ground (ctx/mem c)))
        here (u/self-pos c)
        cells (->> (look/seen-blocks p {:names (vec (kinds (:args c))) :radius radius :max 128 :live? true})
                   (map :pos)
                   (remove skipped))
        graded (map (juxt identity #(classify c wet %)) cells)
        in (rules-in c)
        judged (->> graded
                    (filter #(= :ok (second %)))
                    (map (fn [[pos]] (let [v (access/may-dig? in pos)] [pos v (access/judge v accept)]))))]
    {:targets (->> judged (filter #(= :ok (nth % 2))) (map first) (sort-by (juxt #(if (ground %) 1 0) #(u/dist here %) #(- (:y %)))) vec)
     :refused (into [] (comp (filter #(= :refused (nth % 2))) (map second)) judged)
     :wet? (boolean (some #(= :wet (second %)) graded))
     :wet-n (count (filter #(= :wet (second %)) graded))}))

(defn off-ground?
  "Whether a target is not one of the ground snapshot's cells."
  [c pos]
  (not-any? #(= pos (:pos %)) (:ground (ctx/mem c))))

(defn check [c]
  (let [hurt (body/too-hurt (:primitives c))]
    (cond
      (and (nil? (known/zones c)) (not (:ignore-zones? (:args c)))) (access/decline! c :mine.declined "mine" {:reason :no-zones})
      hurt (ctx/wait c hurt)
      (:phase (ctx/mem c)) true
      (not (:block (:args c))) (ctx/wait c {:reason :no-block})
      :else (let [{:keys [targets refused]} (scan c)]
              (if (and (empty? targets) (seq refused))
                (access/decline! c :mine.declined "mine" (assoc (access/refusal-fields refused) :reason :refused))
                true)))))


(defn ground-pos
  "The cells of the 5x5 under start at y-1 and y-2."
  [start]
  (for [dy [-1 -2] dx (range -2 3) dz (range -2 3)] (around start [dx dy dz])))

(defn snapshot
  "The solid cells of the 5x5 under the start at y-1 and y-2 that the body sees (the cells under them it learns as it digs
  them: book-ground!), as {:pos :name}."
  [c start]
  (vec (for [pos (ground-pos start)
             :let [name (u/seen-name (:primitives c) pos)]
             :when (and name (not (not-solid name)))]
         {:pos pos :name name})))

(defn book-ground!
  "A cell of the ground footprint that the snapshot did not see, about to be dug: the block is known now, so record it for
  the mend."
  [c pos]
  (let [{:keys [start ground]} (ctx/mem c)
        cell (select-keys pos [:x :y :z])
        name (u/seen-name (:primitives c) cell)]
    (when (and (:mend (:args c)) start name (not (not-solid name))
               (some #{cell} (ground-pos start))
               (not-any? #(= cell (:pos %)) ground))
      (ctx/update-mem! c update :ground (fnil conj []) {:pos cell :name name}))))

(defn dug-text
  "The descent and the tunnel of a run, for the end texts: \"; stair down 6 steps; tunnel 3 blocks east at y 58\" (empty for neither)."
  [{:keys [descent tunnel]} reason]
  (str (when (and (pos? (:steps descent 0)) (not= :no-stone-found reason)) (str "; stair down " (:steps descent) " steps"))
       (when (pos? (:steps tunnel 0)) (str "; tunnel " (:steps tunnel) " blocks " (:heading tunnel) " at y " (:y (:origin tunnel))))))

(defn finish!
  "Emit the outcome, hand it to the parent and end the job."
  [c]
  (let [{:keys [goal mended dig-reason resumes tunnel left descent wet-skipped] :as m} (ctx/mem c)
        got (max 0 (- (carried c) (- goal (:count (:args c)))))
        reason (if (and (zero? got) (pos? (or wet-skipped 0)) (#{:none :no-drops :tunnel-length :tunnel-stopped} (:reason m))) :wet (:reason m))
        why (cond-> {} dig-reason (assoc :dig-reason dig-reason) (or (= :no-stone-found reason) (pos? (:steps descent 0))) (assoc :descent descent) (= :wet reason) (assoc :wet-skipped (or wet-skipped 0)) resumes (assoc :resumes resumes)
              (pos? (:steps tunnel 0)) (assoc :tunnel (-> tunnel (select-keys [:origin :heading :steps :stop :end :back-at :walked-back?]) (update :origin access/cell)))
              (seq left) (assoc :left (mapv (fn [[pos n]] {:pos pos :count n}) left)))]
    (ctx/emit! c :mine.done (if (= :not-home reason) :warn :info) (merge {:got got :reason reason :mended (or mended 0)
                                          :text (str "mine done: " (name reason) ", got " got ", mended " (or mended 0)
                                                     (when (= :no-stone-found reason) (str "; dug down " (:steps descent 0) " blocks through soil, found no stone"))
                                                     (when (= :wet reason) (str "; " (or wet-skipped 0) " seen blocks beside water skipped, pass :wet true to dig them"))
                                                     (dug-text m reason)
                                                     (when-let [{:keys [end back-at walked-back?]} (when (pos? (:steps tunnel 0)) tunnel)]
                                                       (str (when end (str ", ended at " (str/join "," end)))
                                                            (if walked-back?
                                                              (str ", walked back to " (str/join "," back-at))
                                                              ", did not get back to its origin"))))}
                                         why))
    (ctx/result! c (merge (cond
                            (zero? got) {:status :stopped :text (str "mine got nothing: " (name reason)
                                                                     (when (= :no-stone-found reason) (str "; dug down " (:steps descent 0) " blocks through soil, found no stone"))
                                                                     (dug-text m reason))}
                            (= :not-home reason) {:status :stopped :text (str "mine got " got " but did not get back to its start")})
                          {:got got :reason reason} why))
    :done))

(defn wrap-up!
  "The normal end: the body walks back to where it started (phase :home), whatever the run did, then finishes."
  [c]
  (ctx/update-mem! c assoc :phase :home)
  :again)

(defn to-mend!
  "End the dig phase with a reason: mend next, or wrap up when there is nothing to mend."
  [c reason]
  (ctx/update-mem! c assoc :phase :mend :reason reason :at-mend (carried c))
  (if (:mend (:args c))
    :again
    (wrap-up! c)))

(defn skip! [c pos] (ctx/update-mem! c update :skipped (fnil conj []) pos))

(defn drop-radius
  "The entity search radius that covers :collect-radius around the last dug cell, seen from the body: the body
  stands up to its reach from the cell and the drop may have fallen further."
  [c]
  (let [r (:collect-radius (:args c))
        dug (:dug-at (ctx/mem c))
        here (u/pos-of (.-pos (.self (:primitives c))))]
    (if dug
      (+ r (u/dist here (zipmap [:x :y :z] (map #(+ 0.5 %) dug))))
      r)))

(defn left-behind
  "Drops of the item lying within radius (default: :collect-radius around the last dug cell) of the body, as
  {[x y z] count}."
  ([c] (left-behind c (drop-radius c)))
  ([c radius]
  (let [item (blocks/item-name (:args c))]
    (into {}
          (comp (filter #(= item (some-> (.-item %) .-name)))
                (map (fn [e] [(access/cell (cell-of (u/pos-of (.-pos e)))) (or (some-> (.-item e) .-count) 1)])))
          (look/seen-items (:primitives c) {:radius radius :max 32})))))

(defn pickup-grace-ms [] (settings/get settings ::pickup-grace-ms))

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
                (do (await (ctx/act c :wait #js {:ms (pickup-grace-ms)}))
                    (into {} (filter (fn [[pos _]] (contains? first-look pos))) (left-behind c))))]
    (when (seq fresh)
      (ctx/emit! c :mine.left-behind :info {:items (mapv (fn [[pos n]] {:pos pos :count n}) fresh)
                                            :text (str "mine left drops it could not pick up at "
                                                       (str/join " " (map (fn [[pos _]] (str/join "," pos)) fresh)))})
      (ctx/update-mem! c update :left merge fresh))))

(defn ^:async look-for-drop!
  "Once per dug cell, glance at the dug cell and the cell under it, then down the column at each unsensed cell (jobs.lib.dig-look),
  where a drop of the dig that fell may lie out of sight."
  [c]
  (let [dug (:dug-at (ctx/mem c))]
    (when (and dug (not= dug (:drop-looked (ctx/mem c))))
      (ctx/update-mem! c assoc :drop-looked dug)
      (await (dig-look/look-at! c dug))
      (await (dig-look/look-at! c (update dug 1 dec)))
      (await (dig-look/look-unknown! c (map #(update dug 1 - %) [2 3 4]))))))

(defn ^:async collect! [c]
  (await (look-for-drop! c))
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                 (cond-> {:radius (drop-radius c) :filter [(blocks/item-name (:args c))] :dig true}
                                   (:dug-at (ctx/mem c)) (assoc :holes [(:dug-at (ctx/mem c))]))))]
    (when (not= :continue r)
      (await (note-left! c))
      (let [now (carried c)]
        (ctx/update-mem! c (fn [m]
                             (-> m
                                 (dissoc :collecting)
                                 (assoc :dry (if (> now (:last-carried m 0)) 0 (inc (:dry m 0)))
                                        :last-carried now))))))
    (if (= :continue r) :continue :again)))

(defn refused!
  "The rules refused pos (verdict v) right before the dig, no failure: a hazard skips it; a zone or a plan keeps it
  out of the targets anyway, and counted among the refused."
  [c pos v verdict]
  (when (= :hazard verdict) (skip! c pos))
  (ctx/emit! c :mine.refused :info (merge {:pos pos :verdict verdict}
                                          (select-keys v [:reason :zone :claim :owner :plan :hazards])
                                          {:text (str "mine left " (access/cell pos) ": " (name verdict))})))

(defn dug-booked!
  "Book a dig of the mined block at pos: drops to collect, a look round at the next cell."
  [c pos]
  (ctx/update-mem! c #(-> % (dissoc :digging) (assoc :failures 0 :collecting true :looked :dug :dug-at (access/cell pos)))))

(defn settle-dig!
  "A round cut during a dig of the mined block (:digging written before it) books the dig when the cell is air now."
  [c]
  (when-let [pos (:digging (ctx/mem c))]
    (ctx/update-mem! c dissoc :digging)
    (when (air (u/seen-name (:primitives c) pos))
      (dug-booked! c pos))))

(defn ^:async dig-cell!
  "Dig pos with a jobs.blocks.dig child: this job's rules judged the cell, the hazards it accepts are the child's."
  [c pos]
  (book-ground! c pos)
  (await (blocks/dig-cell! c pos {:accept (set (:accept (:args c))) :ignore-zones? (boolean (:ignore-zones? (:args c)))})))

(defn ^:async dig! [c pos]
  (await (tools/equip! c))
  (let [v (access/may-dig? (rules-in c) pos)
        verdict (access/judge v (:accept (:args c)))]
    (if (not= :ok verdict)
      (do (refused! c pos v verdict) :again)
      (let [_ (ctx/update-mem! c assoc :digging pos)
            outcome (await (dig-cell! c pos))]
        (when-not (= :continue outcome) (ctx/update-mem! c dissoc :digging))
        (case outcome
          :continue :continue
          :dug (dug-booked! c pos)
          :missing nil
          :cannot (skip! c pos)
          :refused (refused! c pos v :hazard)
          (do (skip! c pos) (ctx/update-mem! c update :failures (fnil inc 0))))
        (if (= :continue outcome) :continue :again)))))


(defn skip-failed!
  "Skip the target and count a failure."
  [c pos]
  (skip! c pos)
  (ctx/update-mem! c update :failures (fnil inc 0)))

;; ------------------------------------------------------------------ the strip tunnel

(defn cut-hazard
  "Why the tunnel may not take cell pos (a fluid in it, lava or unwanted water beside it seen, not loaded), else nil:
  {:reason r :at cell}, the cell being the offending one (the lava beside pos, not pos itself). Faces never seen are no hazard."
  [c pos]
  (let [own (u/seen-name (:primitives c) pos)
        beside (map (fn [f n] [(around pos f) n]) faces (face-names c pos))
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

(def max-open-skip 64)

(defn open-start
  "Where a strip tunnel really starts: from cell, along heading dir [dx dz] over cells already open (feet and head passable, a
  solid floor, as seen), stepping down where the line drops to an open cell with a floor (a stair); the last such cell."
  [name-at cell [dx dz]]
  (let [open? (fn [p] (and (passable (name-at p)) (passable (name-at (update p :y inc)))
                           (rules/solid-floor? name-at (update p :y dec))))
        ahead (fn [p] (-> p (update :x + dx) (update :z + dz)))]
    (loop [p cell n 0]
      (let [q (ahead p) down (update q :y dec)]
        (cond
          (>= n max-open-skip) p
          (open? q) (recur q (inc n))
          (and (passable (name-at q)) (open? down)) (recur down (inc n))
          :else p)))))

(defn tunnel-reason [stop] (if (= :tunnel-length stop) :tunnel-length :tunnel-stopped))

(defn walk-text [{:keys [status reason cell floor]}]
  (str " (walk " status (when reason (str " " reason)) ", cell " cell ", floor " floor ")"))

(defn tunnel-end!
  "The tunnel is done (stop: :tunnel-length or why it stopped at cell at): one warn mine.tunnel-end, then the mend.
  walk: for :walk-failed, what stopped the walk (step-to!)."
  [c stop at & [next walk]]
  (let [{:keys [goal tunnel]} (ctx/mem c)
        {:keys [heading steps]} tunnel
        got (max 0 (- (carried c) (- goal (:count (:args c)))))]
    (ctx/emit! c :mine.tunnel-end :warn
               (cond-> {:reason stop :heading heading :length steps :y (:y (:origin tunnel)) :got got
                        :text (str "mine's tunnel ended after " steps " blocks " heading " at y " (:y (:origin tunnel)) ": "
                                   (if (= :tunnel-length stop) "its length is used up" (str "stopped, " (name stop)))
                                   (when at (str " at " (str/join "," (access/cell at))))
                                   (when walk (walk-text walk))
                                   "; got " got " of " (:count (:args c)))}
                 at (assoc :at (access/cell at))
                 next (assoc :next (access/cell next))
                 walk (assoc :walk walk)))
    (ctx/update-mem! c assoc-in [:tunnel :stop] stop)
    (to-mend! c (tunnel-reason stop))))

(defn ^:async step-to!
  "Walk to the centre of cell pos: nil when there, else what stopped the walk {:status :reason :cell :floor} (the moveTo
  result's status and reason, and the names the body sees at pos and under it)."
  [c pos]
  (when-not (= (cell-of (u/self-pos c)) pos)
    (let [r (await (ctx/act c :moveTo (clj->js {:pos {:x (+ (:x pos) 0.5) :y (:y pos) :z (+ (:z pos) 0.5)}
                                                 :range 0})))]
      (when-not (= "arrived" (.-status r))
        (let [name-at #(u/seen-name (:primitives c) %)]
          {:status (.-status r) :reason (.-reason r)
           :cell (name-at pos) :floor (name-at (update pos :y dec))})))))

(defn ^:async cut!
  "Dig the cells in order (equip, rules, a blocks.dig child): :ok when each is air afterwards, else :refused or :dig-failed. A cut
  of the mined block is collected like a target."
  [c cells]
  (loop [cells cells]
    (if-let [pos (first cells)]
      (let [block (u/seen-name (:primitives c) pos)
            _ (await (tools/equip! c block))
            v (access/may-dig? (rules-in c) pos)
            verdict (access/judge v (:accept (:args c)))
            _ (when (and (= :ok verdict) (contains? (kinds (:args c)) block)) (ctx/update-mem! c assoc :digging pos))
            outcome (when (= :ok verdict) (await (dig-cell! c pos)))
            _ (when-not (= :continue outcome) (ctx/update-mem! c dissoc :digging))]
        (cond
          (not= :ok verdict) (do (refused! c pos v verdict) :refused)
          (= :continue outcome) :continue
          (not (#{:dug :missing} outcome)) :dig-failed
          :else (do (when (and (= :dug outcome) (contains? (kinds (:args c)) block))
                      (ctx/update-mem! c assoc :collecting true :dug-at (access/cell pos)))
                    (recur (rest cells)))))
      :ok)))

;; ------------------------------------------------------------------ torches

(defn end-torch-gap [] (settings/get settings ::end-torch-gap))

(defn torch-min [] (settings/get settings ::torch-min))
(defn torch-craft-count [] (settings/get settings ::torch-craft-count))

(defn count-of [c names]
  (transduce (comp (filter #(contains? names (:name %))) (map :count)) + 0 (u/inventory (:primitives c))))

(defn wood?
  "Whether an item crafts into sticks: a log or planks."
  [n]
  (boolean (and n (re-find #"_(log|planks)$" n))))

(defn craftable?
  "Whether torches can be crafted from what is carried: coal or charcoal, and sticks or wood to make them from."
  [c]
  (let [names (map :name (u/inventory (:primitives c)))]
    (and (pos? (count-of c #{"coal" "charcoal"}))
         (or (pos? (count-of c #{"stick"})) (some wood? names)))))

(defn on-line?
  "Whether the body stands on the tunnel's last cell, past its first."
  [c]
  (let [{:keys [tunnel]} (ctx/mem c)]
    (and (pos? (:steps tunnel 0)) (= (cell-of (u/self-pos c)) (step-cell tunnel (:steps tunnel))))))

(defn torch-gap
  "Steps since the last torch hung (the steps taken when none did yet)."
  [c]
  (let [m (ctx/mem c)] (- (get-in m [:tunnel :steps] 0) (or (:torch-at m) 0))))

(defn torch-due?
  "Whether the tunnel's next torch is owed: the first at step 1, then every :torch-interval."
  [c]
  (let [interval (:torch-interval (:args c))]
    (and (pos? (or interval 0)) (on-line? c)
         (or (nil? (:torch-at (ctx/mem c))) (>= (torch-gap c) interval)))))

(defn end-torch-due? [c]
  (and (pos? (or (:torch-interval (:args c)) 0)) (on-line? c) (>= (torch-gap c) (end-torch-gap))))

(defn branch-gap [] (settings/get settings ::branch-gap))

(defn line-dist
  "The distance from cell to the nearest cell of the tunnel dug so far."
  [tunnel cell]
  (apply min (map #(u/dist cell (step-cell tunnel %)) (range (inc (:steps tunnel 0))))))

(defn branch-torch-due?
  "Whether the body stands off the tunnel line by branch-gap or more with no branch torch within that of it."
  [c]
  (let [{:keys [tunnel branch-torches]} (ctx/mem c)
        here (cell-of (u/self-pos c))]
    (and (some? tunnel) (pos? (or (:torch-interval (:args c)) 0))
         (>= (line-dist tunnel here) (branch-gap))
         (every? #(>= (u/dist here %) (branch-gap)) branch-torches))))

(defn booked!
  "Book the torch step done: a tunnel torch at the tunnel's length, a stair one (branch :stair) at the descent's steps, a branch one
  (branch true) at the body's cell."
  [c branch]
  (cond
    (= :stair branch) (ctx/update-mem! c assoc-in [:descent :torch-at] (get-in (ctx/mem c) [:descent :steps] 0))
    branch (ctx/update-mem! c update :branch-torches (fnil conj []) (cell-of (u/self-pos c)))
    :else (ctx/update-mem! c assoc :torch-at (get-in (ctx/mem c) [:tunnel :steps]))))

(defn stair-torch-due?
  "Whether the stair down to the ore level owes a torch: the first after its first step, then every :torch-interval steps."
  [c]
  (let [interval (:torch-interval (:args c))
        {:keys [steps torch-at]} (:descent (ctx/mem c))]
    (and (pos? (or interval 0)) (pos? (or steps 0))
         (or (nil? torch-at) (>= (- steps torch-at) interval)))))

(defn left-out!
  "Book the torch step done, with an info event of the reason."
  [c cell reason branch]
  (booked! c branch)
  (ctx/emit! c :mine.torch-left-out :info {:cell cell :reason reason
                                           :text (str "mine hung no torch" (some->> cell (str/join ",") (str " at ")) ": " (name reason))}))

(defn branch-site
  "{:dir :site} for a torch on a branch: the run goes away from the tunnel along the axis it is farthest off on, the
  site is the cell behind the body; nil when the body is straight above or below the line (no side to run along)."
  [tunnel here]
  (let [near (apply min-key #(+ (Math/pow (- (:x here) (:x %)) 2) (Math/pow (- (:z here) (:z %)) 2))
                    (map #(step-cell tunnel %) (range (inc (:steps tunnel 0)))))
        dx (- (:x here) (:x near))
        dz (- (:z here) (:z near))
        dir (if (>= (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0] [0 (js/Math.sign dz)])]
    (when (not= [0 0] dir)
      {:dir dir :site (-> here (update :x - (first dir)) (update :z - (second dir)))})))

(defn ^:async hang-torch!
  "Hang a torch on the cell behind the body (a branch: behind it on its way off the tunnel), after a glance at it and
  its side walls when the support is not yet seen."
  [c branch]
  (let [p (:primitives c)
        {:keys [tunnel]} (ctx/mem c)
        block-at (fn [[x y z]] (u/seen-name p {:x x :y y :z z}))
        here (cell-of (u/self-pos c))
        {:keys [dir site]} (cond
                             (= :stair branch) (let [[dx dz :as d] (headings (:heading (ctx/mem c)))]
                                                 {:dir d :site (-> here (update :x - dx) (update :z - dz) (update :y inc))})
                             branch (branch-site tunnel here)
                             :else {:dir (headings (:heading tunnel)) :site (step-cell tunnel (dec (:steps tunnel)))})
        pick (fn [] (when site (torch/torch-at dir [(:x site) (:y site) (:z site)] (placement/eye (u/self-pos c)) block-at)))
        choice (or (let [c0 (pick)] (when-not (:refused c0) c0))
                   (when site (let [[dx dz] dir] (await (glance! c [[(- dx) (- dz)] [(- dz) dx] [dz (- dx)]]))))
                   (pick))
        cell (:cell choice)
        reason (cond (nil? site) :no-site
                     (:refused choice) (:refused choice)
                     (not (gate/allowed? c :mine.declined "mine" :place (zipmap [:x :y :z] cell))) :refused)]
    (if reason
      (left-out! c cell reason branch)
      (let [_ (await (torch/off-hand-to-inventory! c))
            r (await (ctx/act c :place (clj->js {:pos (zipmap [:x :y :z] cell) :item "torch"
                                                 :click (placement/js-click (:click choice))})))]
        (booked! c branch)
        (when-not (or (= "placed" (.-status r)) (torch/torch-blocks (block-at cell)))
          (left-out! c cell :place-failed branch))))))

(defn ^:async torch-step!
  "One round of a torch (the tunnel's; a branch's when branch is true, the stair's when :stair): craft more when under 2 are carried and coal or
  charcoal and a stick are, say so once with none to hang, else hang one. :continue."
  [c branch]
  (let [m (ctx/mem c)
        n (torch/torches-held (:primitives c))]
    (cond
      (and (< n (torch-min)) (craftable? c) (not (:craft-failed m)))
      (let [r (await (ctx/call-child c :torches 'jobs.items.craft {:item "torch" :count (torch-craft-count) :fetch (:fetch (:args c))}))]
        (when (and (not= :continue r) (not (and (= :done r) (pos? (:made (ctx/child-result c :torches) 0)))))
          (ctx/update-mem! c assoc :craft-failed true))
        (if (= :continue r) :continue :again))

      (zero? n)
      (do (when-not (get-in m [:no-torches-said (if (= :stair branch) :stair (boolean branch))])
            (ctx/emit! c :mine.no-torches :info {:text (str "mine has no torches and nothing to craft them from: the "
                                                            (cond (= :stair branch) "stair" branch "branch" :else "tunnel") " stays dark")}))
          (ctx/update-mem! c assoc-in [:no-torches-said (if (= :stair branch) :stair (boolean branch))] true)
          (booked! c branch)
          :again)

      :else (do (await (hang-torch! c branch)) :again))))

(defn ^:async end!
  "tunnel-end!, after a torch at the cut's end when one is due."
  [c stop at & [next walk]]
  (if (end-torch-due? c)
    (await (torch-step! c false))
    (tunnel-end! c stop at next walk)))

(defn max-recuts [] (settings/get settings ::max-recuts))

(defn after-cut
  "What a cut leaves, name-at the seen name of a cell: :unseen (a cell not seen yet: look again), :refilled (a block fell
  in) or :clear."
  [name-at cut]
  (let [names (map name-at cut)]
    (cond
      (some nil? names) :unseen
      (not-every? passable names) :refilled
      :else :clear)))

(defn ^:async recut!
  "A cut cell is not air after the cut (gravel or sand fell in from above): cut it again next round, at most max-recuts times
  per cell, then end :dig-failed."
  [c next]
  (let [n (inc (:recuts (ctx/mem c) 0))]
    (if (> n (max-recuts))
      (await (end! c :dig-failed next))
      (do (ctx/update-mem! c assoc :recuts n) :again))))

(defn ^:async tunnel-round!
  "One step of the strip tunnel: back onto its last cell, judge and cut the next two, look at them, step in."
  [c]
  (let [m (ctx/mem c)
        t (or (:tunnel m)
              (let [start (cell-of (u/self-pos c))
                    origin (open-start #(u/seen-name (:primitives c) %) start (headings (:heading m)))]
                (when (not= origin start)
                  (ctx/emit! c :mine.tunnel-skipped :info {:from (access/cell start) :to (access/cell origin) :heading (:heading m)
                                                           :text (str "mine's tunnel line is open from " (str/join "," (access/cell start)) " to "
                                                                      (str/join "," (access/cell origin)) "; it starts there")}))
                {:origin origin :heading (:heading m) :steps 0}))
        _ (when-not (:tunnel m) (ctx/update-mem! c assoc :tunnel t))
        {:keys [steps stop heading]} t
        name-at #(u/seen-name (:primitives c) %)
        from (step-cell t steps)
        next (step-cell t (inc steps))
        cut [(update next :y inc) next]
        hazard (some #(cut-hazard c %) cut)
        walk (atom nil)]
    (cond
      stop (to-mend! c (tunnel-reason stop))
      (>= steps (:tunnel-length (:args c))) (end! c :tunnel-length nil)
      (do (reset! walk (await (step-to! c from))) @walk) (end! c :walk-failed from nil @walk)
      (and (look/unseen? (:primitives c) cut) (not= next (:glanced m)))
      (do (ctx/update-mem! c assoc :glanced next)
          (await (glance! c [(headings heading)]))
          :again)
      (torch-due? c) (await (torch-step! c false))
      hazard (end! c (:reason hazard) (:at hazard) next)
      :else (let [_ (await (watch/watch! c {:risky? true :before-dig (first (remove #(passable (name-at %)) cut))}))
                  r (await (cut! c (remove #(passable (name-at %)) cut)))]
              (cond
                (= :continue r) :continue
                (not= :ok r) (if-let [h (some #(cut-hazard c %) cut)] (end! c (:reason h) (:at h) next) (end! c r next))
                :else
                (do (await (glance! c [(headings heading)]))
                    (cond
                      (and (= :unseen (after-cut name-at cut)) (not= next (:relooked (ctx/mem c))))
                      (do (ctx/update-mem! c assoc :relooked next)
                          (await (glance! c [(headings heading)]))
                          :again)
                      (not= :clear (after-cut name-at cut)) (await (recut! c next))
                      (not (rules/solid-floor? name-at (update next :y dec))) (end! c :no-floor next)
                      (do (reset! walk (await (step-to! c next))) (nil? @walk))
                      (do (ctx/update-mem! c #(-> % (update-in [:tunnel :steps] inc) (assoc :looked next) (dissoc :recuts)))
                          :again)
                      (some #(rules/falling? (name-at %)) cut) (await (recut! c next))
                      :else (end! c :walk-failed next nil @walk))))))))

;; ------------------------------------------------------------------ the dig phase

(defn drop-in-line?
  "Whether the line from the body's eye to where the drop of the block at pos will lie crosses no solid block (one behind
  another cannot be aimed at, nor its item seen)."
  [c pos]
  (let [p (:primitives c)
        {:keys [x y z]} (u/self-pos c)
        kind-at (fn [x y z] (reach-lib/arrow-kind-of (u/seen-block p {:x x :y y :z z})))]
    (reach-lib/ray-clear? kind-at
                          [(+ (js/Math.floor x) 0.5) (+ y game/eye-height) (+ (js/Math.floor z) 0.5)]
                          [(+ (:x pos) 0.5) (+ (:y pos) 0.25) (+ (:z pos) 0.5)])))

(defn in-line
  "The targets whose drop lies in a clear line from the body's eye, else all of them."
  [c targets]
  (let [clear (filterv #(drop-in-line? c %) targets)]
    (if (seq clear) clear targets)))

(defn ^:async walk-to-dig!
  "Walk toward pos until the body is within reach of it, and close enough to see its drop (range 1) when the line to it
  is blocked from here; a body within reach that cannot get closer digs from there. :there, :partial or :blocked
  (jobs.lib.near/go-near!)."
  [c pos]
  (let [walked (await (near/go-near! c pos (if (drop-in-line? c pos) (reach) 1) {:zone-tolls true}))]
    (if (and (not= :there walked) (u/within? (u/self-pos c) (cell-of pos) (reach)))
      :there
      walked)))

(defn ^:async next-target!
  "The target to walk to next: of the targets off the ground snapshot (else those over it) that the body sees in a
  clear line when any does (the block under the feet only when there is no other: it is dug from beside, where a drop
  deeper down is out of sight), the one the body walks to
  soonest (targets/nearest!), :searching while that search goes on, the nearest in a line when none is found reachable
  (its walk decides); nil with no targets."
  [c targets]
  (let [{:keys [ground start]} (ctx/mem c)
        ground (into (set (map :pos ground)) (when (and start (:mend (:args c))) (ground-pos start)))
        under (update (cell-of (u/self-pos c)) :y dec)
        pool (or (not-empty (filterv #(not= under %) targets)) targets)
        off (vec (remove ground pool))
        group (in-line c (if (seq off) off pool))]
    (when (seq group)
      (let [a (await (targets/nearest! c group (reach) {:tag :mine}))]
        (case (:status a)
          :found (:target a)
          :searching :searching
          (first group))))))

(def stone-types #{"stone" "cobblestone" "deepslate" "andesite" "granite" "diorite" "tuff"})

(defn to-level?
  "Whether the dig phase should first stair down to the tunnel level: one is set, the body stands above it, the tunnel is owed and not
  begun, and the stair has not stopped."
  [c]
  (let [level (tunnel-level (:args c))
        m (ctx/mem c)]
    (boolean (and level (pos? (:tunnel-length (:args c))) (nil? (:tunnel m)) (nil? (:stop (:descent m)))
                  (> (:y (cell-of (u/self-pos c))) level)))))

(defn descend-due?
  "Whether the dig phase should first stair down: to the tunnel level (to-level?), or the block is stone-type, with no seen target (nor
  seen block skipped for water) and the strip tunnel not begun."
  [c]
  (let [{:keys [block descend-limit]} (:args c)]
    (or (to-level? c)
        (and (nil? (tunnel-level (:args c))) (contains? stone-types block) (pos? descend-limit) (nil? (:tunnel (ctx/mem c)))))))

(defn no-stone!
  "The stair down found no stone: one warn mine.no-stone-found, then the mend and the walk home with :reason :no-stone-found."
  [c]
  (let [stop (:stop (:descent (ctx/mem c)))
        steps (:steps (:descent (ctx/mem c)) 0)]
    (ctx/emit! c :mine.no-stone-found :warn {:steps steps :stop stop :text (str "mine dug down " steps " blocks through soil, found no stone"
                                                                               (when stop (str "; the stair stopped: " (name stop))))})
    (to-mend! c :no-stone-found)))

(defn stair-stopped!
  "The stair down to the tunnel level stopped above it: one info mine.descent-stopped, and the tunnel runs where the body stands."
  [c]
  (let [{:keys [steps stop]} (:descent (ctx/mem c))
        y (:y (cell-of (u/self-pos c)))
        level (tunnel-level (:args c))]
    (ctx/emit! c :mine.descent-stopped :info {:steps (or steps 0) :stop stop :y y :level level
                                              :text (str "mine's stair down stopped at y " y " (" (name stop) "), short of y " level
                                                         "; it tunnels there")})
    :again))

(defn ^:async descend-round!
  "One step of the stair down (jobs.access.stair child, :steps 1) along the tunnel heading; the next round looks round and scans.
  To the tunnel level: a torch first when one is due (stair-torch-due?), :fetch passed on; the stair stopping ends the descent there.
  For stone: :fetch true; at :descend-limit steps, or when the stair stops, ends :no-stone-found."
  [c]
  (let [n (:steps (:descent (ctx/mem c)) 0)
        level? (to-level? c)]
    (cond
      (and level? (stair-torch-due? c)) (await (torch-step! c :stair))
      (and (not level?) (>= n (:descend-limit (:args c)))) (no-stone! c)
      :else
      (let [r (await (ctx/call-child c :stair 'jobs.access.stair
                                     {:dir :down :heading (keyword (:heading (ctx/mem c))) :steps 1 :fetch (if level? (:fetch (:args c)) true)
                                      :ignore-zones? (:ignore-zones? (:args c))}))
            res (when (not= :continue r) (ctx/child-result c :stair))]
        (cond
          (= :continue r) :continue
          (and (= :done r) (= :done (:status res)))
          (do (ctx/update-mem! c update-in [:descent :steps] (fnil + 0) (or (:steps res) 1)) :again)
          :else (do (ctx/update-mem! c update :descent assoc :stop (or (:reason res) :declined))
                    (if level? (stair-stopped! c) (no-stone! c))))))))

(defn ^:async dig-round! [c]
  (let [{:keys [goal failures dry looked]} (ctx/mem c)
        {:keys [max-failures wet dry-digs tunnel-length]} (:args c)
        {:keys [targets refused wet? wet-n]} (scan c)
        ready? (and (< (carried c) goal) (< (or dry 0) dry-digs) (< failures max-failures) (some? looked))
        pos (when ready? (await (next-target! c targets)))]
    (cond
      (>= (carried c) goal) (to-mend! c :count)
      (>= (or dry 0) dry-digs) (do (ctx/emit! c :mine.gave-up :warn {:reason :no-drops :dry dry :text (str "mine gave up: " dry " digs brought nothing")})
                                   (to-mend! c :no-drops))
      (>= failures max-failures) (do (ctx/emit! c :mine.gave-up :warn {:failures failures :text (str "mine gave up after " failures " failures")})
                                     (to-mend! c :gave-up))
      (nil? looked) (do (await (look-around! c)) :again)
      (= :searching pos) :again
      (some? pos) (let [walked (await (walk-to-dig! c pos))]
                    (cond
                      (= :blocked walked) (do (skip-failed! c pos) :again)
                      (= :partial walked) :continue
                      (branch-torch-due? c) (await (torch-step! c true))
                      :else (do (await (watch/watch! c {:before-dig pos}))
                                (await (dig! c pos)))))
      (not= looked (cell-of (u/self-pos c))) (do (await (look-around! c)) :again)
      :else (do (when (and wet? (not wet)) (ctx/update-mem! c update :wet-skipped #(max (or % 0) wet-n)))
                (when (seq refused)
                  (access/decline! c :mine.declined "mine" (assoc (access/refusal-fields refused) :reason :refused)))
                (cond
                  (and (empty? refused) (not (and wet? (not wet))) (descend-due? c)) (await (descend-round! c))
                  (pos? tunnel-length) (await (tunnel-round! c))
                  (seq refused) (to-mend! c :refused)
                  :else (to-mend! c (if (and wet? (not wet)) :wet :none)))))))

;; ------------------------------------------------------------------ mend

(defn owed
  "The ground entries whose cell is now air, cave_air or water (unloaded cells are not owed). After a descent the
  cells below the start level are not owed: they are the stair the body walks home by."
  [c]
  (let [{:keys [descent start]} (ctx/mem c)
        way-y (when (pos? (:steps descent 0)) (js/Math.floor (:y start)))]
    (filterv #(let [name (u/seen-name (:primitives c) (:pos %))]
                (and name (or (air name) (= "water" name))
                     (or (nil? way-y) (>= (:y (:pos %)) way-y))))
             (:ground (ctx/mem c)))))

(defn filler
  "The first carried block to fill with: dig-in's building blocks other than the
  mined item, then the item itself when it is a block (the mend must not eat the count)."
  [c]
  (let [{:keys [block] :as a} (:args c)
        item (blocks/item-name a)
        own (when (or (some #{item} blocks/building-blocks) (= item block)) [item])]
    (blocks/pick c (concat (remove #{item} blocks/building-blocks) own))))

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
  (let [walked (if (> (u/dist (u/self-pos c) pos) (mend-reach))
                 (await (near/go-near! c pos (reach) {:zone-tolls true}))
                 :there)]
    (if (not= :there walked)
      (do (when (= :blocked walked) (mend-fail! c)) (if (= :blocked walked) :again :continue))
      (let [status (if (gate/allowed? c :mine.declined "mine" :place pos)
                     (.-status (await (ctx/act c :place (clj->js {:pos pos :item item}))))
                     "refused")]
        (cond
          (= "placed" status) (ctx/update-mem! c #(-> % (assoc :mend-failures 0) (update :mended (fnil inc 0))))
          :else (mend-fail! c))
        :again))))

(defn ^:async raise! [c item]
  (let [status (.-status (await (ctx/act c :jumpPlace (clj->js {:item item :count 1}))))]
    (if (contains? #{"done" "partial"} status)
      (ctx/update-mem! c update :mended (fnil inc 0))
      (mend-fail! c))
    :again))

(defn cells-text [cells] (str/join " " (map #(str (:x (:pos %)) "," (:y (:pos %)) "," (:z (:pos %))) cells)))

(defn spent-on-mend?
  "Whether the mend ate into what the dig brought, leaving fewer than the goal carried."
  [c]
  (let [{:keys [goal at-mend]} (ctx/mem c)
        now (carried c)
        start (- goal (:count (:args c)))]
    (and (< now goal) (< now at-mend) (> at-mend start))))

(defn max-resumes [] (settings/get settings ::max-resumes))

(defn ^:async mended!
  "Nothing is owed. The mend spent the count: look around once from here, then dig on while targets off the ground
  remain (at most max-resumes times). Else finish."
  [c]
  (let [{:keys [resumes reason looked]} (ctx/mem c)
        more? (some #(off-ground? c %) (:targets (scan c)))]
    (cond
      (not (spent-on-mend? c)) (wrap-up! c)
      (not= looked (cell-of (u/self-pos c))) (do (await (look-around! c)) :again)
      (and more? (< (or resumes 0) (max-resumes)))
      (do (ctx/update-mem! c #(-> % (assoc :phase :dig :failures 0 :dry 0 :last-carried (carried c) :mend-failures 0)
                                  (dissoc :collecting)
                                  (update :resumes (fnil inc 0))))
          :again)
      :else (do (ctx/update-mem! c assoc :reason :spent-on-mend :dig-reason reason)
                (wrap-up! c)))))

(defn ^:async mend-round! [c]
  (let [cells (mend-permitted c (owed c))
        item (when (seq cells) (filler c))
        pos (mend-target c cells)]
    (cond
      (empty? cells) (await (mended! c))
      (>= (:mend-failures (ctx/mem c) 0) (max-mend-failures))
      (do (ctx/emit! c :mine.mend-failed :warn {:owed (mapv :pos cells) :text (str "mine could not mend: " (cells-text cells))})
          (wrap-up! c))
      (nil? item)
      (do (ctx/emit! c :mine.mend-short :warn {:owed (mapv :pos cells) :text (str "mine has nothing to mend with: " (cells-text cells))})
          (wrap-up! c))
      (some? pos) (await (place! c pos item))
      :else (await (raise! c item)))))

(defn still-left!
  "Keep in :left only the drops still lying in the world: one a later steer picked up (or that despawned) is
  dropped. Looks as far as the farthest listed drop, from where the body stands now."
  [c]
  (let [left (:left (ctx/mem c))
        here (u/self-pos c)
        radius (inc (apply max (map #(u/dist here (zipmap [:x :y :z] (key %))) left)))
        lying (left-behind c radius)]
    (ctx/update-mem! c assoc :left (into {} (filter #(contains? lying (key %))) left))))

(defn home-done!
  "Record whether the body got back to its start, say so when not, drop the left-behind drops it picked up, end."
  [c arrived?]
  (ctx/update-mem! c update :tunnel assoc :walked-back? (boolean arrived?) :back-at (access/cell (cell-of (u/self-pos c))))
  (when-not arrived?
    (ctx/update-mem! c #(assoc % :reason :not-home :dig-reason (or (:dig-reason %) (:reason %))))
    (ctx/emit! c :mine.not-home :warn {:to (access/cell (:start (ctx/mem c))) :at (access/cell (cell-of (u/self-pos c)))
                                       :text (str "mine did not get back to " (str/join "," (access/cell (:start (ctx/mem c)))))}))
  (when (seq (:left (ctx/mem c))) (still-left! c))
  (finish! c))

(defn beside?
  "Whether here is a cell go-to's range 1 accepts as next to the start cell: one step along one axis, and never below it (that is the
  hole the digging left)."
  [here start]
  (and (>= (:y here) (:y start))
       (<= (reduce + (map #(let [d (- (% here) (% start))] (* d d)) [:x :y :z])) 1)))

(defn solid-floor?
  "Whether the floor under the start cell is solid: as seen now (seen), else as felt at the start (felt); an unknown floor is no place
  to aim a walk at."
  [seen felt]
  (let [name (or seen felt)]
    (boolean (and name (not (not-solid name))))))

(defn home-goal
  "Where this walk home goes, decided once: {:cell c :range 0} the start cell when its floor is solid (seen now, else felt at the
  start); else, a hole the mend left or a floor never seen, a seen standable cell next to it ({:range 0}: the body must not end in
  the hole), else {:cell start :range 1}."
  [c start]
  (let [seen #(u/seen-name (:primitives c) %)
        below (fn [{:keys [x y z]}] {:x x :y (dec y) :z z})
        solid-at? #(solid-floor? (seen (below %)) nil)
        open-at? #(some-> (seen %) not-solid boolean)
        beside (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]]
                     :let [cell (-> start (update :x + dx) (update :z + dz))]
                     :when (and (solid-at? cell) (open-at? cell))]
                 cell)]
    (cond
      (solid-floor? (seen (below start)) (:start-floor (ctx/mem c))) {:cell start :range 0}
      (seq beside) {:cell (first beside) :range 0}
      :else {:cell start :range 1})))

(defn ^:async home-by-go-to!
  "The way home through a go-to child, with its escalation, to the goal home-round! decided (mem :home-goal). :continue while the walk
  goes on, else the job ends."
  [c start]
  (let [{:keys [cell range]} (:home-goal (ctx/mem c))
        arrived? #(let [here (cell-of (u/self-pos c))] (if (zero? range) (= here cell) (beside? here start)))
        r (if (arrived?)
            :arrived
            (await (ctx/call-child c :home 'jobs.movement.go-to
                                   {:pos {:x (+ (:x cell) 0.5) :y (:y cell) :z (+ (:z cell) 0.5)} :range range :escalate true :zone-tolls true
                                    :ignore-zones? (boolean (:ignore-zones? (:args c)))})))]
    (if (= :continue r)
      :continue
      (home-done! c (arrived?)))))

(defn ^:async home-round!
  "The last step of every normal run: back to the cell it started on through a go-to child (beside it when its floor is open or unseen),
  whatever the digging left open behind it; a warn mine.not-home when the walk did not arrive (the job ends :stopped :not-home)."
  [c]
  (let [{:keys [start homing]} (ctx/mem c)]
    (when-not homing
      (ctx/update-mem! c assoc :homing true :home-goal (home-goal c start))
      (ctx/update-mem! c assoc-in [:tunnel :end] (access/cell (cell-of (u/self-pos c)))))
    (await (home-by-go-to! c start))))

(defn no-tool?
  "Whether the block needs a tool to drop (get-tool's test: its harvestTools) and none carried is one of them."
  [c]
  (not (tools/can-harvest? (:primitives c) (:block (:args c)))))

(defn needed-kind [c]
  (tools/needed-kind (:primitives c) (:block (:args c))))

(defn no-tool!
  "End at once, before any dig: warn and hand over {:status :stopped :got 0 :reason :no-tool :tool <kind>}."
  [c]
  (let [kind (needed-kind c)]
    (ctx/emit! c :mine.no-tool :warn {:tool kind :text (str "mine has no " kind " for " (:block (:args c)))})
    (ctx/emit! c :mine.done :info {:got 0 :reason :no-tool :tool kind :mended 0 :text "mine done: no-tool, got 0, mended 0"})
    (ctx/result! c {:status :stopped :got 0 :reason :no-tool :tool kind})
    :done))

(defn tool-problem
  "The :no-tool wait for the block while no pickaxe is carried, else nil."
  [c]
  (when (no-tool? c) {:reason :no-tool :block (:block (:args c))}))

(defn ^:async fetch-pickaxe!
  "A pickaxe is owed: jobs.lib.fetch runs jobs.items.get-tool for the block (failures are booked, limits apply).
  :continue while it runs and once it has got one; else the end: before any dig (no phase) at once, in the dig phase
  the mend first, with :no-tool."
  [c]
  (let [r (await (fetch/fetch! c 'jobs.gather.mine tool-problem))]
    (cond
      r r
      (not (no-tool? c)) :again
      (nil? (:phase (ctx/mem c))) (no-tool! c)
      :else (do (ctx/emit! c :mine.no-tool :warn {:tool (needed-kind c) :text (str "mine has no " (needed-kind c) " for " (:block (:args c)) " and could not get one")})
                (to-mend! c :no-tool)))))

(defn toward
  "The heading name for a :heading arg h at start: a name, or the dominant axis from start to a cell [x y z]; nil when none."
  [start h]
  (if-not (sequential? h)
    (heading-name h)
    (let [dx (- (first h) (:x start)) dz (- (nth h 2) (:z start))]
      (cond (and (zero? dx) (zero? dz)) nil
            (>= (js/Math.abs dx) (js/Math.abs dz)) (if (pos? dx) "east" "west")
            :else (if (pos? dz) "south" "north")))))

(defn at-pre-start?
  "Whether the body stands on the cell the job began on."
  [c]
  (= (access/cell (:pre-start (ctx/mem c))) (access/cell (cell-of (u/self-pos c)))))

(defn ^:async back-to-pre-start!
  "Before the first dig, after a fetch walked the body off: a go-to child back to the cell the job began on (mem :pre-start),
  which the start, tunnel origin and walk home are taken from. Else the job ends
  :stopped :not-back. :continue while it walks, :again once there."
  [c]
  (let [{:keys [x y z] :as cell} (:pre-start (ctx/mem c))
        r (await (ctx/call-child c :back 'jobs.movement.go-to
                                 {:pos {:x (+ x 0.5) :y y :z (+ z 0.5)} :range 0 :escalate true :zone-tolls true
                                  :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (cond
      (= :continue r) :continue
      (at-pre-start? c) :again
      :else (do (ctx/emit! c :mine.done :warn {:got 0 :reason :not-back :text (str "mine could not get back to " (str/join "," (access/cell cell)) " after fetching its tool")})
                (ctx/result! c {:status :stopped :got 0 :reason :not-back})
                :done))))

(defn flooded!
  "The head is in water: no more digging (the cut may be what let it in). Warn; before the first dig end at once
  {:status :stopped :got 0 :reason :flooded}, later end the dig phase :flooded and walk home."
  [c]
  (ctx/emit! c :mine.flooded :warn {:text "mine stopped digging: the head is in water"})
  (if (:phase (ctx/mem c))
    (do (ctx/update-mem! c assoc :reason :flooded)
        (wrap-up! c))
    (do (ctx/emit! c :mine.done :info {:got 0 :reason :flooded :mended 0 :text "mine done: flooded, got 0, mended 0"})
        (ctx/result! c {:status :stopped :got 0 :reason :flooded :text "mine got nothing: the head is in water"})
        :done)))

(defn ^:async step [c]
  (settle-dig! c)
  (when (and (nil? (:phase (ctx/mem c))) (nil? (:pre-start (ctx/mem c))))
    (ctx/update-mem! c assoc :pre-start (cell-of (u/self-pos c))))
  (let [m (ctx/mem c)
        {:keys [heading direction]} (:args c)]
    (cond
      (and (contains? #{nil :dig} (:phase m)) (breath/head-under? (:primitives c))) (flooded! c)

      (and (contains? #{nil :dig} (:phase m)) (no-tool? c)) (await (fetch-pickaxe! c))

      (and (nil? (:phase m)) (not (no-tool? c)) (:pre-start m) (not (at-pre-start? c))) (await (back-to-pre-start! c))

      (nil? (:phase m))
      (let [faced (facing (:primitives c))
            _ (await (look/survey! c))
            now (carried c)
            start (cell-of (u/self-pos c))
            heading (or (toward start heading) (heading-name direction))]
        (ctx/update-mem! c assoc
                         :goal (+ now (:count (:args c))) :start start :start-floor (u/feel-name (:primitives c) {:x (:x start) :y (dec (:y start)) :z (:z start)})
                         :failures 0 :dry 0 :last-carried now
                         :heading (or heading faced)
                         :ground (if (:mend (:args c)) (snapshot c start) [])
                         :phase :dig)
        :again)

      (= :home (:phase m)) (await (home-round! c))
      (= :mend (:phase m)) (await (mend-round! c))
      (:collecting m) (await (collect! c))
      :else (await (dig-round! c)))))

(defn max-steps [] (settings/get settings ::max-steps))

(defn ^:async round
  [c]
  (if-let [t (or (await (trip/ready! c)) (await (trip/tool-ready! c (:block (:args c)))))]
    t
    (let [n (atom 0)]
      (await (pace/steps! c #(if (< (swap! n inc) (max-steps)) (step c) :continue))))))
