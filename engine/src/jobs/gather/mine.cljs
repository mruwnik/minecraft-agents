(ns jobs.gather.mine
  (:require [jobs.lib.choose :as choose]
            [jobs.lib.submitted :as submitted]
            [jobs.lib.child :as child]
            [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.blocks :as blocks]
            [clojure.string :as str]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.result :as result]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.body :as body]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.gate :as gate]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.tools :as tools]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.ore-pick :as ore-pick]
            [jobs.lib.pace :as pace]
            [jobs.lib.trip :as trip]
            [jobs.lib.watch :as watch]
            [jobs.lib.look :as look :refer [cell-of headings heading-name facing glance! look-around!]]
            [jobs.lib.breath :as breath]
            [jobs.lib.torch :as torch]
            [jobs.lib.placement :as placement]
            [jobs.gather.mine-stay :as stay]
            [jobs.access.stair :as stair]
            [jobs.lib.world :as known]
            [jobs.gather.mine.targets :refer [reach air kinds carried around face-names rules-in soil-blocks scan off-ground? snapshot book-ground! way-floor last-exit? skip! drop-radius left-behind skip-failed! skip-unreachable! stalled! step-cell zone-margin within-own-zone drop-in-line? next-target!]]
            [jobs.gather.mine.end :refer [wrap-up! to-mend! home-round!]]
            [jobs.gather.mine.descent :refer [to-level? descent-wanted? descend-due? shaft-due? no-stone! stair-stopped! stair-in-own-zone? shaft-top-here? shaft-round!]]))

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
  - Every carried tool that digs the block down to 3 uses (jobs.lib.trip/spent-tool?, no spare was had) ends the same way, :reason :no-tool,
    before the last one breaks deep down.
  - A pickaxe block (jobs.lib.tool-kind/tool-kind) with no *_pickaxe carried, at the start or later (the pick broke),
    runs jobs.items.get-tool as a child first (unless :fetch is false). Nothing got: before any dig it ends at once, warn mine.no-tool,
    {:status :stopped :got 0 :reason :no-tool :tool <kind needed: pickaxe, sword, shears>}; in the dig phase the mend and walk home follow with
    :reason :no-tool. Shovel and axe blocks drop by hand. Soil or wood dug with only a pickaxe held empties the
    hand first (no wear on the pickaxe).
  - The check passes when a phase is in memory, or :block is named, unless every seen target is refused.
  - No zone list (zones.edn missing or never valid) declines with one warn mine.declined {:reason :no-zones},
    also in the middle of the job. Nothing is dug or placed then.

  Before a descent or strip tunnel it stocks torches when under 8 are carried (jobs.lib.trip/torches-ready!: obtain, charcoal from logs; warn torch.no-stock, then it digs dark).
  Before the first round it deposits at the known chest when the bag is fullish (jobs.lib.trip/ready!; :deposit-first? false skips it).

  The first round writes to job memory before any dig: the goal (carried + :count), the start cell, the
  tunnel heading (:heading, else :direction, else the way the body faces) and the ground (the solid cells of the 5x5 under the
  start at y-1 and y-2; empty when :mend is false). So a cut or restart still mends.

  One call runs these steps in order, again and again until the job ends (it yields with :continue only while a child or
  a walk waits on the world, and after 400 steps):
  1. Collecting: after a dig the body glances at the dug cell, the cell under it and unsensed cells further down (once per cell), then the collect-drops child gathers the item within :collect-radius; when none came in, the body walks within 1 of the dug cell once (a dug cell no walk gets beside, dug from a distance over a wall, is first cut to: at most ::drop-cut-max cells, jobs.lib.dig-route; not with :stay-level), looks and collects again. The count is the
     inventory's, never the dig's report. Drops of the item still lying afterwards (out of reach) are reported
     once each, info mine.left-behind {:items [{:pos :count}]}. A dig after which nothing came in and no drop is in
     sight is told once, info mine.drop-lost {:cell}, and listed in the result's :lost. The result's :left is re-checked after the walk
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
  6b. Still none, a tunnel level set (:level, else the ore's mine.descent/ore-levels entry, e.g. iron_ore 16; :level false: none) and the
     body above it: a stair down the same way until the feet are at the level; the stair stopping ends the descent there
     (info mine.descent-stopped {:steps :stop :y :level}) and the tunnel runs at that height.
  7. Else the strip tunnel, at the level then stood on.

  A descent (6, 6b) goes by a mineshaft instead when one is usable (:shaft, jobs.lib.shafts): the body walks to the shaft's top (not when it stands on a step of the shaft: it goes on from there), a
  jobs.access.stair child runs its stair down to the plan's depth, and the strip tunnel is the shaft's next branch
  (shafts/next-for; remembered when the run ends). A shaft in another's zone or claim is not used (:ignore-zones? uses it); one in the
  body's own zone is, without walking out of the zone first. No usable shaft or a stair that stops: the descent above. The text says
  \"via shaft s (stair 54 steps to y 16), branch 2 north 32 blocks\".

  Strip tunnel: a 1-wide 2-high straight run at the level the body stood on at the start, along :heading or :direction
  (north, south, east, west or n/s/e/w; give :heading to choose it, else the way the body faces), at most :tunnel-length blocks per job; it starts past cells of the line already open (info
  mine.tunnel-skipped, also down a stair to a floor) and never digs a torch. 0 means no tunnel: then no
  target ends :wet when a seen block was rejected only for water, else :none. Each step walks back onto the last
  cell of the run (a go-to child; failing: :walk-failed) and cuts the next by a jobs.access.stair child of one :level
  step (:cut-only with :stay-level, which then walks in itself): its stops and its handling of exposed lava (sealed), laid-open water (plugged, a stop), a missing floor
  (bridged) and refilled cells (:refills) are the stair's. Water beside the cut is taken with :wet, a falling block
  when :accept has it, lava never. A cut of :block is collected like a target. Ore that comes into view is dug before the next step.

  The run used up ends the dig phase :tunnel-length, any stop ends it :tunnel-stopped. Either way one warn
  mine.tunnel-end {:y (the tunnel's feet height) :reason :heading :length :at :next :walk}: the reason is
  :tunnel-length, :own-zone, :walk-failed, :breach-sealed, :breach-open, :refused (the stair declined), a hazard by
  its key (:lava :water :falling-block, :at the hazard's cell) or the stair's stop. :at is the offending cell, :next the line cell it stopped before, :walk (walk-failed, step-failed) the go-to's status, reason and the names seen at the cell and its floor.

  :stay-level true is the cave-safe mode: the tunnel runs at the start height (no stair down, no :level, no descent through soil), only
  seen ore within one column of the body counts, and a cut that opens into air beyond the tunnel is refilled with a carried building
  block from the line cell before the body steps in, ending the tunnel :breach-sealed (:breach-open when it cannot seal). Seen ore 2 to :ore-reach blocks straight out from the body
  (feet or head height, through rock seen solid) is taken by a child mine run: a stay-level side cut that digs it and walks back, its
  mouth then shut with a carried block (info mine.side); the ore is skipped afterwards. Seen ore one column round the feet from one cell below to one over the head is dug from the stand; the hole it leaves is
  shut with a carried block once the drop is collected (the body steps back to the tunnel line first; warn mine.hole-open and
  :holes-open in the result when it cannot). Seen ore it did not take is listed in the result as :unreached (up to 10).

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
  With the start fewer than mine.end/home-dip blocks above the body the walk plans nothing deeper than that under it (no way round through caves).

  Zones and plans (jobs.lib.access.rules, through jobs.lib.access): a target must be a dig the rules permit,
  with only :accept hazards, when chosen and again right before the dig. A cell in a zone that bars :dig, or in
  an active plan's footprint, is never a target. One refused right before the dig is skipped without a failure
  (info mine.refused), as is one with a hazard not accepted. When all seen blocks are refused: before the
  first round the check declines. In the job the tunnel goes on (with :tunnel-length 0 the dig phase ends
  :refused). One warn mine.declined per job names the zones and plans ({:reason :refused :zones :plans}).

  Hands over {:got n :reason r} (with :status :stopped when :got is short of :count (text: mine got n of m: reason) or the walk home failed, :reason :not-home: never completed) plus
  :dig-reason, :resumes, :tunnel and :left when set; info mine.done with
  :mended, the cells filled; its text (and the result's text when :got is 0) names the stair's steps and the tunnel's length,
  heading and height (\"; stair down 48 steps; tunnel 32 blocks east at y 16\"); :descent {:steps :stop} is in the result after any descent. With reason :wet (also when a tunnel dug nothing and seen blocks were skipped for water), :wet-skipped counts the seen blocks left for water beside them. Its text says the reason, :got and :mended, and for a tunnel its length, heading, end cell
  and whether the body walked back. :got is how many more are carried than at the start, at least 0. :tunnel is
  {:origin :heading :steps :stop :end :back-at :walked-back?}, :end the cell it ended on before the walk back.
  :stay-level with no :heading or :direction goes on after a sealed breach or a fluid stop: it turns to a heading not yet tried from the
  cell it stands in (not back along its tunnel), the :tunnel-length shared by all legs; :legs lists them [{:heading :length :stop}].
  A breach's tunnel-end event and leg carry :behind {:names :cells}, the seen air and fluid cells beside the cut (read before the seal), also kept in body memory :breach-behind.")

(a/defargs args
  {:from {:doc "where the body stood at submit (jobs.mjs adds it): the job waits :moved-since-submit before its first round when the body is then over jobs.lib.submitted/limit away" :spec a/position? :default nil}
   :here {:doc "true: run where the body is, whatever :from says" :spec boolean? :default false}
   :block {:doc "name of the block to mine (required)" :spec a/name? :default nil}
   :item {:doc "the item the block drops; nil: the drop-item table, else the block name" :spec a/item? :default nil}
   :count {:doc "how many more to carry than at the start" :spec (a/int-in 0 nil) :default 8}
   :radius {:doc "seen blocks within this many blocks of the body count" :spec (a/num-in 0 nil) :default 16}
   :wet {:doc "dig blocks that touch water, and tunnel beside water" :spec boolean? :default false}
   :pick-up-ores {:doc "true: dig the ores of jobs.lib.ore-pick's ::ores setting that are exposed within reach of where it stands; false: none; or the ore block names to dig" :spec (a/or-of boolean? (a/coll-of a/name?)) :default true}
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
   :stay-level {:doc "caution for caves: dig only at the level the body starts at (the two-high tunnel, no stair down, no :level), mine only seen ore beside the body (:ore-reach: or a short side cut out), and seal a dug cell that opens into air beyond the tunnel with a carried block (tunnel ends :breach-sealed, or :breach-open when it cannot seal)" :spec boolean? :default false}
   :ore-reach {:doc "with :stay-level: seen ore this many blocks into the wall beside the tunnel is taken by a short side cut (a child mine run), shut behind with a carried block; 0 or 1: only ore beside the body" :spec (a/int-in 0 nil) :default 2}
   :variants {:doc "an ore's stone and deepslate kinds both count as the block (iron_ore and deepslate_iron_ore, either named); false: only :block" :spec boolean? :default true}
   :level {:doc "the feet height the strip tunnel runs at (a whole number, or a cell [x y z] / {:x :y :z} whose y it takes): with no target seen the body first stairs down to it (jobs.access.stair) when higher; nil: the ore's level (setting mine.descent/ore-levels), none for other blocks; false: where the body stands"
           :spec (a/pred "a whole number, a cell [x y z] or {:x :y :z}, nil or false" #(or (nil? %) (false? %) (integer? %) (and (sequential? %) (= 3 (count %)) (integer? (second %))) (and (map? %) (integer? (:y %))))) :default nil}
   :shaft {:doc "a mineshaft plan id to descend by instead of digging a stair (jobs.lib.shafts); nil: the nearest usable one within reach, when a descent is due; false: none; :required: the nearest usable one, and no shaft usable ends :stopped :no-shaft before anything is dug" :spec (a/pred "a shaft plan id, nil, false or :required" #(or (nil? %) (false? %) (= :required %) (string? %))) :default nil}
   :descend-limit {:doc "the most steps of stair down through soil to find stone, when the block is stone-type and none is in sight; 0: never descend" :spec (a/int-in 0 nil) :default 12}
   :torch-interval {:doc "the strip tunnel hangs a torch every this many steps; 0: none" :spec (a/int-in 0 nil) :default 10}
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent :falling-block); the lava and :wet rules above still hold"
            :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent :falling-block}}})

(a/defargs settings
  {::mend-reach {:default 4 :doc "Reach while mending a cut, in blocks." :spec (a/int-in 0 nil)}
   ::max-mend-failures {:default 6 :doc "Failed mends before the job gives up." :spec (a/int-in 1 nil)}
   ::pickup-grace-ms {:default 400 :doc "How long the body waits for a drop to be picked up, in ms." :spec (a/int-in 0 nil)}
   ::drop-cut-max {:default 4 :doc "The most cells dug to open a way beside a dug cell whose drop did not come in and that no walk gets beside; 0: no such cut." :spec (a/int-in 0 nil)}
   ::end-torch-gap {:default 4 :doc "A cut's end gets a torch when the last hangs this many steps back or more." :spec (a/int-in 0 nil)}
   ::branch-gap {:default 4 :doc "A dig this far from the tunnel line (and from any torch hung on a branch) gets a torch of its own." :spec (a/int-in 0 nil)}
   ::torch-min {:default 2 :doc "A torch round crafts more when fewer torches than this are carried." :spec (a/int-in 0 nil)}
   ::torch-craft-count {:default 4 :doc "Torches one craft makes." :spec (a/int-in 1 nil)}
   ::open-skip-drop {:default 3 :doc "The most blocks the strip tunnel's start steps down while it skips open cells on its line (a stair); the tunnel then runs at that level." :spec (a/int-in 0 nil)}
   ::max-resumes {:default 2 :doc "Resumes of a cut before the job gives up." :spec (a/int-in 0 nil)}
   ::max-restairs {:default 3 :doc "Times the descent starts a new stair where the body stands after the stair found it off its line (a reflex, a fall or a drop pick-up moved it) before the stop stands." :spec (a/int-in 0 nil)}
   ::max-steps {:default 400 :doc "Pieces of work of one call before it gives the round back with :continue." :spec (a/int-in 0 nil)}})

(defn mend-reach [] (settings/get settings ::mend-reach))

(defn max-mend-failures [] (settings/get settings ::max-mend-failures))

(def passable "Blocks a body walks through in a tunnel cell: air and torches (never dug)." #{"air" "cave_air" "torch" "wall_torch"})

(defn check [c]
  (let [hurt (body/too-hurt (:primitives c))]
    (cond
      (some? (submitted/away-wait c)) false
      (and (nil? (known/zones c)) (not (:ignore-zones? (:args c)))) (access/decline! c :mine.declined "mine" {:reason :no-zones})
      hurt (ctx/wait c hurt)
      (not (trip/torches-check c)) false
      (:phase (ctx/mem c)) true
      (not (:block (:args c))) (ctx/wait c {:reason :no-block})
      :else (let [{:keys [targets refused]} (scan c)]
              (if (and (empty? targets) (seq refused))
                (access/decline! c :mine.declined "mine" (assoc (access/refusal-fields refused) :reason :refused))
                true)))))

(defn pickup-grace-ms [] (settings/get settings ::pickup-grace-ms))

(defn ^:async note-left!
  "Report drops of the item collect-drops left lying (out of reach), once per cell. A drop still listed right after
  the collect may be one whose pickup packet has not arrived, so a drop counts only when it is still lying after a
  short wait. why maps a cell to the reason collect-drops left it, shown when known."
  [c why]
  (let [known (:left (ctx/mem c))
        fresh? (fn [[pos _]] (not (contains? known pos)))
        first-look (into {} (filter fresh?) (left-behind c))
        fresh (if (empty? first-look)
                first-look
                (do (await (ctx/act c :wait #js {:ms (pickup-grace-ms)}))
                    (into {} (filter (fn [[pos _]] (contains? first-look pos))) (left-behind c))))]
    (when (seq fresh)
      (ctx/emit! c :mine.left-behind :info {:items (mapv (fn [[pos n]] (cond-> {:pos pos :count n} (why pos) (assoc :reason (why pos)))) fresh)
                                            :text (str "mine left drops it could not pick up at "
                                                       (str/join " " (map (fn [[pos _]] (str/join "," pos)) fresh)))})
      (ctx/update-mem! c update :left merge fresh))
    fresh))

(defn drop-lost!
  "A dig after which the carried count did not rise and no drop of the item lies in sight: said once per dug cell, and
  kept for the result's :lost."
  [c]
  (let [{:keys [dug-at lost]} (ctx/mem c)]
    (when (and dug-at (not (some #{dug-at} lost)))
      (ctx/emit! c :mine.drop-lost :info {:cell dug-at
                                          :text (str "mine dug at " (str/join "," dug-at) ": nothing came in and no drop is in sight")})
      (ctx/update-mem! c update :lost (fnil conj []) dug-at))))

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

(defn ^:async dig-cell!
  "Dig pos with a jobs.blocks.dig child: this job's rules judged the cell, the hazards it accepts are the child's."
  [c pos]
  (book-ground! c pos)
  (await (blocks/dig-cell! c pos {:accept (set (:accept (:args c))) :ignore-zones? (boolean (:ignore-zones? (:args c)))})))

(defn drop-cut-max [] (settings/get settings ::drop-cut-max))

(defn drop-cut
  "The cells to dig, in walking order, for the cheapest way (jobs.lib.dig-route, falls of 1 at most, a cell never seen
  taken for stone) from the body's cell to a stand within 1 of the dug cell, at most max-digs of them; nil with no such way."
  [c dug max-digs]
  (let [p (:primitives c)
        at (fn [[x y z]] (or (u/seen-name p {:x x :y y :z z}) "stone"))
        from (u/cell-vec (u/self-pos c))]
    (->> [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 -1 0] [0 1 0]]
         (keep #(:dig (dig-route/route at from (mapv + dug %) {:max-digs max-digs :max-fall 1})))
         (filter #(<= (count %) max-digs))
         (sort-by count)
         first)))

(defn ^:async cut-to-drop!
  "Open a way beside the dug cell (drop-cut), one dig child per cell, at most ::drop-cut-max tries per dug cell; the
  way is planned again after each dig and a look at what it laid open. :cut (open: walk again), :partial (a dig waits on
  the world) or nil (no way, or a dig failed)."
  [c dug]
  (loop []
    (let [n (get-in (ctx/mem c) [:drop-cut dug] 0)
          cells (drop-cut c dug (- (drop-cut-max) n))]
      (cond
        (nil? cells) nil
        (empty? cells) :cut
        :else (let [cell (first cells)
                    o (await (dig-cell! c (zipmap [:x :y :z] cell)))]
                (case o
                  :continue :partial
                  (:dug :missing) (do (ctx/update-mem! c assoc :drop-cut {dug (inc n)})
                                      (await (dig-look/look-at! c cell))
                                      (await (dig-look/look-at! c (update cell 1 dec)))
                                      (recur))
                  nil))))))

(defn ^:async walk-to-drop!
  "A dig whose drop did not come in: once per dug cell, walk within 1 of it (near/go-near!, no escalation), where a drop
  out of sight or out of reach from the dig spot is seen and picked up; a dug cell no walk gets beside (dug from a
  distance over a wall) is cut to first (cut-to-drop!; not with :stay-level). :walked (look and collect again), :partial
  (go-to or a dig waits) or nil (no walk: it came in, the cell was walked to already, or the walk was blocked)."
  [c]
  (let [{:keys [dug-at drop-walked last-carried]} (ctx/mem c)]
    (when (and dug-at (not= dug-at drop-walked) (<= (carried c) (or last-carried 0)))
      (let [pos (zipmap [:x :y :z] dug-at)
            go! #(near/go-near! c pos 1 {:zone-tolls true :escalate false})
            w (await (go!))
            cut (when (and (= :blocked w) (not (:stay-level (:args c))) (let [res (ctx/child-result c :walk)] (or (result/unreachable-walk? res) (= :detour (:why res)))))
                  (await (cut-to-drop! c dug-at)))
            w (case cut :cut (await (go!)) :partial :partial w)]
        (when-not (= :partial w) (ctx/update-mem! c assoc :drop-walked dug-at))
        (when (= :there w) (ctx/update-mem! c dissoc :drop-looked))
        (case w :partial :partial :there :walked nil)))))

(defn ^:async collect! [c]
  (await (look-for-drop! c))
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                 (cond-> {:radius (drop-radius c) :filter [(blocks/item-name (:args c))] :dig true}
                                   (:dug-at (ctx/mem c)) (assoc :holes [(:dug-at (ctx/mem c))]))))
        why (into {} (map (fn [{:keys [pos reason]}] [[(:x pos) (:y pos) (:z pos)] reason])) (:left (ctx/child-result c :collect)))
        w (when (not= :continue r) (await (walk-to-drop! c)))]
    (when (and (not= :continue r) (nil? w))
      (when (and (empty? (await (note-left! c why))) (<= (carried c) (:last-carried (ctx/mem c) 0)))
        (drop-lost! c))
      (let [now (carried c)]
        (ctx/update-mem! c (fn [m]
                             (-> m
                                 (dissoc :collecting)
                                 (assoc :dry (if (> now (:last-carried m 0)) 0 (inc (:dry m 0)))
                                        :last-carried now))))))
    (if (or (= :continue r) (= :partial w)) :continue :again)))

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
  (ctx/update-mem! c #(cond-> (-> % (dissoc :digging) (assoc :failures 0 :collecting true :looked :dug :dug-at (access/cell pos)))
                        (:stay-level (:args c)) (update :holes (fnil conj []) (access/cell pos)))))

(defn settle-dig!
  "A round cut during a dig of the mined block (:digging written before it) books the dig when the cell is air now."
  [c]
  (when-let [pos (:digging (ctx/mem c))]
    (ctx/update-mem! c dissoc :digging)
    (when (air (u/seen-name (:primitives c) pos))
      (dug-booked! c pos))))

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

(defn max-restairs [] (settings/get settings ::max-restairs))

;; ------------------------------------------------------------------ the strip tunnel

(def max-open-skip 64)

(defn open-skip-drop [] (settings/get settings ::open-skip-drop))

(defn open-start
  "Where a strip tunnel really starts: from cell, along heading dir [dx dz] over cells already open (feet and head passable, a
  solid floor, as seen), stepping down where the line drops to an open cell with a floor (a stair), at most max-drop (default
  ::open-skip-drop) blocks below cell in all; the last such cell."
  [name-at cell [dx dz] & [max-drop]]
  (let [open? (fn [p] (and (passable (name-at p)) (passable (name-at (update p :y inc)))
                           (rules/solid-floor? name-at (update p :y dec))))
        ahead (fn [p] (-> p (update :x + dx) (update :z + dz)))]
    (loop [p cell n 0]
      (let [q (ahead p) down (update q :y dec)]
        (cond
          (>= n max-open-skip) p
          (open? q) (recur q (inc n))
          (and (passable (name-at q)) (open? down) (<= (- (:y cell) (:y down)) (or max-drop (open-skip-drop)))) (recur down (inc n))
          :else p)))))

(def heading-stops
  "Stops that say only this heading is blocked: the stair or tunnel at 0 steps tries the next heading."
  #{:breach-sealed :undercuts-way :own-zone :floor-unseen :not-loaded :base-block :crop :fluid-in-cut :place-failed :no-support-floor :no-floor :cave-below :walk-failed :step-failed :refused})

(def leg-stops
  "Stops after which a stay-level run goes on along another heading from the cell it stands in."
  (into heading-stops #{:water :lava :falling-block}))

(def opposite {"north" "south" "south" "north" "east" "west" "west" "east"})

(defn turn-heading!
  "Turn the run to the next of the four headings not yet tried (memory :tried-headings), forgetting the stop and the tunnel
  begun on the old one; info mine.turned. A tunnel of steps > 0 is kept as a leg (memory :legs, :spent) and the way back along
  it is not a heading. False (nothing changed) when a heading arg was given or all are tried."
  [c stop]
  (let [m (ctx/mem c)
        {:keys [heading direction]} (:args c)
        {:keys [steps] :as tunnel} (:tunnel m)
        mid? (pos? (or steps 0))
        tried (cond-> (conj (set (:tried-headings m)) (:heading m))
                mid? (conj (opposite (:heading m))))
        cycle ["north" "east" "south" "west"]
        order (map #(nth cycle (mod (+ (.indexOf cycle (:heading m)) %) 4)) (range 1 4))
        to (first (remove tried order))]
    (when (and to (nil? heading) (nil? direction))
      (ctx/emit! c :mine.turned :info {:from (:heading m) :to to :stop stop :length (or steps 0)
                                       :text (str "mine's " (name stop) " blocks its heading " (:heading m) (when mid? (str " after " steps " blocks")) "; it turns " to)})
      (when mid? (stay/record-tunnel! c tunnel (headings (:heading m))))
      (ctx/update-mem! c #(cond-> (-> % (assoc :heading to :tried-headings tried) (dissoc :tunnel :behind :torch-at :glanced :looked) (update :descent dissoc :stop))
                            tunnel (-> (update :legs (fnil conj []) (cond-> {:heading (:heading m) :length (or steps 0) :stop stop} (:behind m) (assoc :behind (:behind m))))
                                       (update :spent (fnil + 0) (or steps 0)))))
      true)))

(defn tunnel-reason [stop] (if (= :tunnel-length stop) :tunnel-length :tunnel-stopped))

(defn walk-text [{:keys [status reason cell floor]}]
  (str " (walk " (str/join " " (map #(if (keyword? %) (name %) %) (remove nil? [status reason])))
       (when cell (str ", cell " cell ", floor " floor)) ")"))

(defn tunnel-end!
  "The tunnel is done (stop: :tunnel-length or why it stopped at cell at): one warn mine.tunnel-end, then the mend.
  walk: for :walk-failed, what stopped the walk (step-to!)."
  [c stop at & [next walk]]
  (let [{:keys [goal tunnel]} (ctx/mem c)
        {:keys [heading steps]} tunnel
        got (max 0 (- (carried c) (- goal (:count (:args c)))))]
    (if (and (if (:stay-level (:args c)) (leg-stops stop) (and (zero? steps) (heading-stops stop)))
             (< (+ steps (:spent (ctx/mem c) 0)) (:tunnel-length (:args c)))
             (turn-heading! c stop))
      :again
      (do
    (ctx/update-mem! c update :legs (fnil conj []) (cond-> {:heading heading :length steps :stop stop} (:behind (ctx/mem c)) (assoc :behind (:behind (ctx/mem c)))))
    (ctx/emit! c :mine.tunnel-end :warn
               (cond-> {:reason stop :heading heading :length steps :y (:y (:origin tunnel)) :got got
                        :text (str "mine's tunnel ended after " steps " blocks " heading " at y " (:y (:origin tunnel)) ": "
                                   (if (= :tunnel-length stop) "its length is used up" (str "stopped, " (name stop)))
                                   (when at (str " at " (str/join "," (access/cell at))))
                                   (when walk (walk-text walk))
                                   "; got " got " of " (:count (:args c)))}
                 at (assoc :at (access/cell at))
                 next (assoc :next (access/cell next))
                 (:behind (ctx/mem c)) (assoc :behind (:behind (ctx/mem c)))
                 walk (assoc :walk walk)))
    (ctx/update-mem! c assoc-in [:tunnel :stop] stop)
    (stay/record-tunnel! c tunnel (headings heading))
    (to-mend! c (tunnel-reason stop))))))

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
    (and (pos? (torch/fuel-held (:primitives c)))
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
      (let [r (await (ctx/call-child c :torch-craft 'jobs.items.craft {:item "torch" :count (torch-craft-count) :fetch (:fetch (:args c)) :avoid (trip/line-ahead c)}))]
        (when (and (not= :continue r) (not (and (= :done r) (pos? (:made (ctx/child-result c :torch-craft) 0)))))
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

(defn ^:async leave-zone!
  "Before the first dig: a go-to child to a cell zone-margin outside the own zone zn the body stands near (memory :pre-start moves with it).
  One info mine.left-zone when out; else the job ends :stopped :zone-exit. :continue while it walks, :again once out."
  [c zn]
  (let [m (zone-margin)
        goal (zones/exit-goal (cell-of (u/self-pos c)) zn m)
        from (:pre-start (ctx/mem c))
        r (await (child/call-walk c :leave
                                 {:pos {:x (+ (:x goal) 0.5) :y (:y goal) :z (+ (:z goal) 0.5)} :range 2 :y-slack 8 :escalate true :zone-tolls true}))
        here (cell-of (u/self-pos c))]
    (cond
      (= :continue r) :continue
      (not (within-own-zone c (dec m) (access/cell here)))
      (do (ctx/emit! c :mine.left-zone :info {:zone (:name zn) :from (access/cell from) :to (access/cell here)
                                              :text (str "mine walked out of zone " (:name zn) " to " (str/join "," (access/cell here)) " before it digs")})
          (ctx/update-mem! c assoc :pre-start here)
          :again)
      :else (do (ctx/emit! c :mine.done :warn {:got 0 :reason :zone-exit :text (str "mine could not get out of zone " (:name zn) " before it digs")})
                (ctx/result! c {:status :stopped :got 0 :reason :zone-exit})
                :done))))

;; lava seen beside the cut always stops the tunnel; water only without :wet
(defn step-accept [{:keys [accept wet]}]
  (cond-> [] (some #{:falling-block} accept) (conj :falling-block) wet (conj :water)))

(defn step-stop
  "The tunnel's stop from the stair step's result res (nil: it declined): a hazard is named by its key (:lava :water
  :falling-block) at the hazard's cell; else the stair's reason (:refused when it declined) at its cell, as a map."
  [res]
  (let [h (first (:hazards res))
        at (some-> (or (when (= :hazard (:reason res)) (:at h)) (:cell res)) (#(zipmap [:x :y :z] %)))]
    {:reason (cond (nil? res) :refused (and h (= :hazard (:reason res))) (stair/hazard-key h) :else (:reason res)) :at at}))

(defn book-cut!
  "After a stair step: its dug cells booked for the mend (ground under the start), and the last mined block among them
  collected like a target."
  [c dug]
  (doseq [{:keys [cell block]} dug] (book-ground! c (zipmap [:x :y :z] cell) block))
  (when-let [cell (last (keep #(when (contains? (kinds (:args c)) (:block %)) (:cell %)) dug))]
    (ctx/update-mem! c assoc :collecting true :dug-at cell)))

(defn walk-of
  "What stopped a walk into pos: the go-to result r's status and reason, and the names the body sees at pos and under it."
  [c pos r]
  (let [name-at #(u/seen-name (:primitives c) %)]
    {:status (:status r) :reason (:reason r) :cell (name-at pos) :floor (name-at (update pos :y dec))}))

(defn ^:async back-on-line!
  "Walk into the tunnel's cell pos (a go-to child) unless there: nil when there, :continue while it walks, else what
  stopped the walk (walk-of)."
  [c pos]
  (when-not (= (cell-of (u/self-pos c)) pos)
    (let [r (await (child/walk-into! c :walk (access/cell pos)))]
      (cond
        (= :continue r) :continue
        (= pos (cell-of (u/self-pos c))) nil
        :else (walk-of c pos r)))))

(defn ^:async stair-step!
  "The next cells by a jobs.access.stair child, one :level step (it judges, digs, seals exposed lava, plugs laid-open
  water, bridges a missing floor and steps in). With :stay-level the stair only cuts: a cut that opened into air beyond
  the tunnel is sealed from the line cell, else the body walks in (a go-to child)."
  [c from next cut]
  (let [{:keys [heading]} (ctx/mem c)
        args (:args c)
        name-at #(u/seen-name (:primitives c) %)
        _ (await (watch/watch! c {:risky? true :before-dig (first (remove #(passable (name-at %)) cut))}))
        _ (ctx/update-mem! c assoc :stepping true)
        r (await (ctx/call-child c :step 'jobs.access.stair
                                 {:dir :level :heading (keyword heading) :steps 1 :accept (step-accept args) :fetch (:fetch args)
                                  :stock-torches false :pick-up-ores false :ignore-zones? (:ignore-zones? args)
                                  :spare-own-builds (:spare-own-builds args) :cut-only (boolean (:stay-level args))}))
        res (ctx/child-result c :step)]
    (when-not (= :continue r) (ctx/update-mem! c dissoc :stepping))
    (book-cut! c (:dug res))
    (cond
      (= :continue r) :continue
      (not= :done (:status res)) (let [{:keys [reason at]} (step-stop res)]
                                   (end! c reason at next (some->> (:walk res) (walk-of c next))))
      :else
      (let [own (ledger/own-cells c)
            open (when (:stay-level args) (seq (stay/breaches name-at from cut own)))]
        (if open
          (let [b (stay/behind name-at from cut own open)]
            (stay/remember-behind! c (access/cell (first open)) b)
            (ctx/update-mem! c assoc :behind b)
            (end! c (if (await (stay/seal! c (rules-in c) open)) :breach-sealed :breach-open) (first open) next))
          (do (ctx/update-mem! c #(-> % (update-in [:tunnel :steps] inc) (assoc :looked next)))
              (let [w (await (back-on-line! c next))]
                (cond
                  (= :continue w) :continue
                  w (end! c :walk-failed next nil w)
                  :else (do (await (glance! c [(headings heading)])) :again)))))))))

(defn ^:async tunnel-round!
  "One step of the strip tunnel: back onto its last cell (a go-to child), a torch when due, then the stair step (a step
  under way is gone on with first)."
  [c]
  (let [m (ctx/mem c)
        t (or (:tunnel m)
              (let [start (cell-of (u/self-pos c))
                    origin (open-start #(u/seen-name (:primitives c) %) start (headings (:heading m)) (when (:stay-level (:args c)) 0))]
                (when (not= origin start)
                  (ctx/emit! c :mine.tunnel-skipped :info {:from (access/cell start) :to (access/cell origin) :heading (:heading m)
                                                           :text (str "mine's tunnel line is open from " (str/join "," (access/cell start)) " to "
                                                                      (str/join "," (access/cell origin)) "; it starts there")}))
                {:origin origin :heading (:heading m) :steps 0}))
        _ (when-not (:tunnel m) (ctx/update-mem! c assoc :tunnel t))
        {:keys [steps stop heading]} t
        from (step-cell t steps)
        next (step-cell t (inc steps))
        cut [(update next :y inc) next]
        walk (atom nil)]
    (cond
      stop (to-mend! c (tunnel-reason stop))
      (:stepping m) (await (stair-step! c from next cut))
      (some #(within-own-zone c 0 (access/cell %)) cut) (end! c :own-zone next)
      (>= (+ steps (:spent m 0)) (:tunnel-length (:args c))) (end! c :tunnel-length nil)
      (do (reset! walk (await (back-on-line! c from))) (= :continue @walk)) :continue
      @walk (end! c :walk-failed from nil @walk)
      (and (look/unseen? (:primitives c) cut) (not= next (:glanced m)))
      (do (ctx/update-mem! c assoc :glanced next)
          (await (glance! c [(headings heading)]))
          :again)
      (torch-due? c) (await (torch-step! c false))
      :else (await (stair-step! c from next cut)))))

;; ------------------------------------------------------------------ the dig phase

(defn underground?
  "Whether the run has dug down or tunnelled, or the body stands on a floor of a stair it cut earlier: its walks go on at
  night inside its own cuts."
  [c]
  (let [m (ctx/mem c)
        feet (cell-of (u/self-pos c))]
    (boolean (or (pos? (get-in m [:descent :steps] 0)) (:tunnel m)
                 (contains? (ledger/ways-of c) (access/cell (update feet :y dec)))))))

(defn ^:async walk-to-dig!
  "Walk toward pos until the body is within reach of it, and close enough to see its drop (range 1) when the line to it
  is blocked from here; a body within reach that cannot get closer digs from there. :there, :partial or :blocked
  (jobs.lib.near/go-near!)."
  [c pos]
  (let [walked (await (near/go-near! c pos (if (or (:stay-level (:args c)) (drop-in-line? c pos)) (reach) 1) (cond-> {:zone-tolls true} (underground? c) (assoc :at-night :go))))]
    (if (and (not= :there walked) (u/within? (u/self-pos c) (cell-of pos) (reach)))
      :there
      walked)))

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
      (stair-in-own-zone? c) (do (ctx/update-mem! c assoc-in [:descent :stop] :own-zone)
                                 (ctx/emit! c :mine.own-zone :warn {:zone (:name (stair-in-own-zone? c))
                                                                    :text (str "mine's stair stops at the edge of its own zone " (:name (stair-in-own-zone? c)))})
                                 (if level? (stair-stopped! c) (to-mend! c :own-zone)))
      :else
      (let [_ (when-not (get-in (ctx/mem c) [:descent :top])
                (ctx/update-mem! c update :descent assoc :top (access/cell (cell-of (u/self-pos c))) :top-heading (keyword (:heading (ctx/mem c)))))
            r (await (ctx/call-child c :stair 'jobs.access.stair
                                     {:dir :down :heading (keyword (:heading (ctx/mem c))) :steps 1 :stock-torches false :fetch (if level? (:fetch (:args c)) true)
                                      :accept (filterv #{:falling-block} (:accept (:args c)))
                                      :ignore-zones? (:ignore-zones? (:args c))
                                      :pick-up-ores (:pick-up-ores (:args c))}))
            res (when (not= :continue r) (ctx/child-result c :stair))]
        (cond
          (= :continue r) :continue
          (and (= :done r) (= :done (:status res)))
          (do (ctx/update-mem! c update :descent (fn [d] (assoc d :steps (+ (:steps d 0) (or (:steps res) 1)) :bottom-y (js/Math.floor (:y (u/self-pos c))))))
              :again)
          (and (= :off-stair (:reason res)) (< (get-in (ctx/mem c) [:descent :restairs] 0) (max-restairs)))
          (do (ctx/emit! c :mine.restair :info {:at (access/cell (cell-of (u/self-pos c))) :text (str "mine's stair finds the body off its line (" (:why res) "); it starts a new stair here")})
              (ctx/update-mem! c #(-> % (update :children dissoc :stair) (update :descent (fn [d] (-> d (dissoc :top) (update :restairs (fnil inc 0)))))))
              :again)
          :else (let [stop (or (:reason res) :declined)]
                  (if (and (zero? n) (heading-stops stop) (turn-heading! c stop))
                    :again
                    (do (ctx/update-mem! c update :descent assoc :stop stop)
                        (if level? (stair-stopped! c) (no-stone! c))))))))))

(defn ^:async side-round!
  "Take ore in the tunnel wall by a child mine run: a stay-level tunnel of the cut's steps to the side, which digs the ore beside it and
  walks home. Then the cut's mouth is shut with a carried block, and the ore is skipped."
  [c {:keys [pos heading steps] :as cut}]
  (ctx/update-mem! c assoc :side-cut cut)
  (let [here (cell-of (u/self-pos c))
        args (-> (:args c)
                 (dissoc :direction)
                 (assoc :heading heading :tunnel-length steps :count 1 :ore-reach 0 :mend false :fetch false :shaft false :stay-level true))
        r (await (ctx/call-child c :side 'jobs.gather.mine args))]
    (if (= :continue r)
      :continue
      (let [_ (ctx/update-mem! c dissoc :side-cut)
            [dx dz] (headings heading)
            mouth (-> here (update :x + dx) (update :z + dz))
            name-at #(u/seen-name (:primitives c) %)
            open (filterv #(not (rules/solid-floor? name-at %)) [mouth (update mouth :y inc)])
            shut (or (empty? open) (await (stay/seal! c (rules-in c) open)))]
        (ctx/emit! c :mine.side :info {:pos (access/cell pos) :heading heading :steps steps :shut shut
                                       :text (str "mine took the wall ore at " (str/join "," (access/cell pos)) " by a side cut " steps " " heading
                                                  (when-not shut "; the cut stays open"))})
        (skip! c pos)
        :again))))

(defn ^:async dig-round-1! [c]
  (let [{:keys [goal failures dry looked]} (ctx/mem c)
        {:keys [max-failures wet dry-digs tunnel-length]} (:args c)
        {:keys [targets refused wet? wet-n side far]} (scan c)
        ready? (and (< (carried c) goal) (< (or dry 0) dry-digs) (< failures max-failures) (some? looked))
        pos (when ready? (await (next-target! c targets)))]
    (when (seq far) (ctx/update-mem! c update :unreached #(into (vec %) (remove (set %)) far)))
    (cond
      (>= (carried c) goal) (to-mend! c :count)
      (>= (or dry 0) dry-digs) (do (ctx/emit! c :mine.gave-up :warn {:reason :no-drops :dry dry :text (str "mine gave up: " dry " digs brought nothing")})
                                   (to-mend! c :no-drops))
      (>= failures max-failures) (do (ctx/emit! c :mine.gave-up :warn {:failures failures :text (str "mine gave up after " failures " failures")})
                                     (to-mend! c :gave-up))
      (nil? looked) (do (await (look-around! c)) :again)
      (= :searching pos) :again
      (and (some? pos) (way-floor c [pos])) (do (skip! c pos) :again)
      (and (some? pos) (last-exit? c pos)) (do (ctx/emit! c :mine.spared-exit :info {:pos (access/cell pos) :text (str "mine leaves " (str/join "," (access/cell pos)) ": it is the body's last way out")})
                                               (skip! c pos) :again)
      (some? pos) (let [walked (await (walk-to-dig! c pos))]
                    (cond
                      (and (= :blocked walked) (result/unreachable-walk? (ctx/child-result c :walk))) (do (skip-unreachable! c pos) :again)
                      (= :blocked walked) (do (skip-failed! c pos) :again)
                      (= :partial walked) (stalled! c pos)
                      (branch-torch-due? c) (await (torch-step! c true))
                      :else (do (await (watch/watch! c {:before-dig pos}))
                                (await (dig! c pos)))))
      (not= looked (cell-of (u/self-pos c))) (do (await (look-around! c)) :again)
      :else (do (when (and wet? (not wet)) (ctx/update-mem! c update :wet-skipped #(max (or % 0) wet-n)))
                (when (seq refused)
                  (access/decline! c :mine.declined "mine" (assoc (access/refusal-fields refused) :reason :refused)))
                (cond
                  (and (empty? refused) (not (and wet? (not wet))) (shaft-due? c)) (await (shaft-round! c))
                  (and (empty? refused) (not (and wet? (not wet))) (descend-due? c)) (await (descend-round! c))
                  (and (seq side) (< (carried c) goal)) (await (side-round! c (first side)))
                  (pos? tunnel-length) (await (tunnel-round! c))
                  (seq refused) (to-mend! c :refused)
                  :else (to-mend! c (if (and wet? (not wet)) :wet :none)))))))

(defn ^:async shut-holes!
  "Fill each cell an ore was dug from in the wall (stay-level, after its drop was collected) with a carried block, so no cut
  stays open beside the tunnel. A body that went into a hole to fetch its drop first steps back to the tunnel line. A hole that
  cannot be filled warns mine.hole-open and is listed in the result."
  [c]
  (let [{:keys [holes tunnel start]} (ctx/mem c)
        feet (cell-of (u/self-pos c))
        in-hole? (some #(and (= (:x feet) (% 0)) (= (:z feet) (% 2)) (<= 0 (- (% 1) (:y feet)) 1)) holes)
        line (if tunnel (step-cell tunnel (:steps tunnel)) start)
        w (when in-hole? (await (back-on-line! c line)))]
    (cond
      (= :continue w) :continue
      (some? w)
      (do (ctx/update-mem! c #(-> % (dissoc :holes) (update :holes-open into holes)))
          (ctx/emit! c :mine.hole-open :warn {:cells holes :text (str "mine could not leave the hole it dug at " (str/join " " (map #(str/join "," %) holes)) " to shut it")})
          :again)
      :else
      (loop [[[x y z] & more] holes open []]
        (if x
          (do (await (ctx/act c :look (clj->js {:pos {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)}})))
              (look/see! c)
              (recur more (cond-> open (rules/air (u/seen-name (:primitives c) {:x x :y y :z z})) (conj {:x x :y y :z z}))))
          (let [shut (or (empty? open) (await (stay/seal! c (rules-in c) open)))]
            (ctx/update-mem! c #(cond-> (dissoc % :holes) (not shut) (update :holes-open into (map access/cell open))))
            (when-not shut
              (ctx/emit! c :mine.hole-open :warn {:cells (mapv access/cell open)
                                                  :text (str "mine could not shut the hole it dug at " (str/join " " (map #(str/join "," (access/cell %)) open)))}))
            :again))))))

(defn ^:async dig-round!
  "A side cut or tunnel step under way goes on first, then a hole dug in the wall is shut, then the ores within reach, then
  one piece of the digging."
  [c]
  (cond
    (:side-cut (ctx/mem c)) (await (side-round! c (:side-cut (ctx/mem c))))
    (:stepping (ctx/mem c)) (await (tunnel-round! c))
    (seq (:holes (ctx/mem c))) (await (shut-holes! c))
    (= :continue (await (ore-pick/pick! c {:leave (kinds (:args c))}))) :continue
    :else (await (dig-round-1! c))))

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

(defn fillers
  "The blocks to fill with, in order: dig-in's building blocks other than the mined item, then the item itself
  when it is a block (the mend must not eat the count)."
  [c]
  (let [{:keys [block] :as a} (:args c)
        item (blocks/item-name a)
        own (when (or (some #{item} (choose/fillers :build)) (= item block)) [item])]
    (vec (concat (remove #{item} (choose/fillers :build)) own))))

(defn mend-permitted
  "The owed cells ({:pos ..}) the job may place on (zones and claims; one mine.declined warn per job when refused)."
  [c cells]
  (let [ok (set (gate/allowed c :mine.declined "mine" :place (map :pos cells)))]
    (filterv #(ok (:pos %)) cells)))

(defn cells-text [poss] (str/join " " (map #(str (:x %) "," (:y %) "," (:z %)) poss)))

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
      (do (ctx/update-mem! c #(-> % (assoc :phase :dig :failures 0 :dry 0 :last-carried (carried c))
                                  (dissoc :collecting)
                                  (update :resumes (fnil inc 0))))
          :again)
      :else (do (ctx/update-mem! c assoc :reason :spent-on-mend :dig-reason reason)
                (wrap-up! c)))))

(defn ^:async mend-round!
  "The permitted owed cells are filled by a blocks.fill-cells child; its end says whether the mend is done, short of
  blocks or failed."
  [c]
  (let [cells (mend-permitted c (owed c))]
    (if (empty? cells)
      (await (mended! c))
      (let [r (await (ctx/call-child c :fill 'jobs.blocks.fill-cells {:cells (mapv :pos cells) :any-of (fillers c)
                                                                      :reach (mend-reach) :max-failures (max-mend-failures)}))
            {:keys [mended why left]} (ctx/child-result c :fill)]
        (when (= :done r)
          (ctx/update-mem! c update :mended (fnil + 0) (or mended 0)))
        (cond
          (not= :done r) r
          (= :failed why)
          (do (ctx/emit! c :mine.mend-failed :warn {:owed (vec left) :text (str "mine could not mend: " (cells-text left))})
              (wrap-up! c))
          (= :short why)
          (do (ctx/emit! c :mine.mend-short :warn {:owed (vec left) :text (str "mine has nothing to mend with: " (cells-text left))})
              (wrap-up! c))
          :else :again)))))

(defn no-tool!
  "End at once, before any dig: warn and hand over {:status :stopped :got 0 :reason :no-tool :tool <kind>}."
  [c]
  (let [kind (tools/needed-kind (:primitives c) (:block (:args c)))]
    (ctx/emit! c :mine.no-tool :warn {:tool kind :text (str "mine has no " kind " for " (:block (:args c)))})
    (ctx/emit! c :mine.done :info {:got 0 :reason :no-tool :tool kind :mended 0 :text "mine done: no-tool, got 0, mended 0"})
    (ctx/result! c {:status :stopped :got 0 :reason :no-tool :tool kind})
    :done))

(defn tool-problem
  "The :no-tool wait for the block while no pickaxe is carried, else nil."
  [c]
  (when-not (tools/can-harvest? (:primitives c) (:block (:args c))) {:reason :no-tool :block (:block (:args c))}))

(defn ^:async fetch-pickaxe!
  "A pickaxe is owed: jobs.lib.fetch runs jobs.items.get-tool for the block (failures are booked, limits apply).
  :continue while it runs and once it has got one; else the end: before any dig (no phase) at once, in the dig phase
  the mend first, with :no-tool."
  [c]
  (let [r (await (fetch/fetch! c 'jobs.gather.mine tool-problem))]
    (cond
      r r
      (tools/can-harvest? (:primitives c) (:block (:args c))) :again
      (nil? (:phase (ctx/mem c))) (no-tool! c)
      :else (let [kind (tools/needed-kind (:primitives c) (:block (:args c)))]
              (ctx/emit! c :mine.no-tool :warn {:tool kind :text (str "mine has no " kind " for " (:block (:args c)) " and could not get one")})
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
        r (await (child/call-walk c :back
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

(defn hungry!
  "Food is short and get-food found none (trip/food-short?; not for soil, dug at the surface near home): before the first dig end at once {:status :stopped :got 0
  :reason :hungry}, later end the dig phase :hungry and walk home."
  [c]
  (ctx/emit! c :mine.hungry :warn {:text "mine stopped digging: hungry, no food carried and get-food found none"})
  (if (:phase (ctx/mem c))
    (do (ctx/update-mem! c assoc :reason :hungry)
        (wrap-up! c))
    (do (ctx/emit! c :mine.done :info {:got 0 :reason :hungry :mended 0 :text "mine done: hungry, got 0, mended 0"})
        (ctx/result! c {:status :stopped :got 0 :reason :hungry :text "mine got nothing: hungry, no food"})
        :done)))

(defn ^:async step [c]
  (settle-dig! c)
  (let [m (ctx/mem c)
        {:keys [heading direction]} (:args c)]
    (cond
      (and (contains? #{nil :dig} (:phase m)) (breath/head-under? (:primitives c))) (flooded! c)

      (and (contains? #{nil :dig} (:phase m)) (not (contains? soil-blocks (:block (:args c)))) (trip/food-short? c)) (hungry! c)

      (and (contains? #{nil :dig} (:phase m)) (not (tools/can-harvest? (:primitives c) (:block (:args c))))) (await (fetch-pickaxe! c))

      (and (contains? #{nil :dig} (:phase m)) (trip/spent-tool? (:primitives c) (:block (:args c))))
      (if (nil? (:phase m)) (no-tool! c) (to-mend! c :no-tool))

      (and (nil? (:phase m)) (tools/can-harvest? (:primitives c) (:block (:args c))) (:pre-start m) (not (at-pre-start? c))) (await (back-to-pre-start! c))

      (and (nil? (:phase m)) (within-own-zone c (dec (zone-margin)) (access/cell (cell-of (u/self-pos c)))) (not (shaft-top-here? c)))
      (await (leave-zone! c (within-own-zone c (dec (zone-margin)) (access/cell (cell-of (u/self-pos c))))))

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
  (submitted/begin! c)
  (when (and (nil? (:phase (ctx/mem c))) (nil? (:pre-start (ctx/mem c))))
    (ctx/update-mem! c assoc :pre-start (cell-of (u/self-pos c))))
  (if-let [t (or (await (trip/ready! c)) (await (trip/tool-ready! c (:block (:args c))))
                 (await (when (and (pos? (or (:torch-interval (:args c)) 0)) (nil? (:tunnel (ctx/mem c))) (or (pos? (:tunnel-length (:args c))) (descent-wanted? c)))
                          (trip/torches-ready! c))))]
    t
    (let [n (atom 0)]
      (await (pace/steps! c #(if (< (swap! n inc) (max-steps)) (step c) :continue))))))
