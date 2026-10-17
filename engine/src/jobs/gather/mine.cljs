(ns jobs.gather.mine
  (:require [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.gather.mine.descent :refer [descent-wanted? descend-due? descend-round! shaft-due? shaft-under-way? shaft-top-here? shaft-round!]]
            [jobs.gather.mine.drops :refer [collect! dig-cell!]]
            [jobs.gather.mine.end :refer [end-run to-mend! wrap-up! home-round!]]
            [jobs.gather.mine.mend :refer [mend-round!]]
            [jobs.gather.mine.side :as wall]
            [jobs.gather.mine.targets :refer [reach air kinds carried rules-in soil-blocks scan snapshot way-floor last-exit? skip! skip-failed! skip-unreachable! stalled! zone-margin within-own-zone drop-in-line? next-target!]]
            [jobs.gather.mine.torches :as torches]
            [jobs.gather.mine.tunnel :as tunnel]
            [jobs.lib.access :as access]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.breath :as breath]
            [jobs.lib.area :as area]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look :refer [cell-of heading-name facing look-around!]]
            [jobs.lib.near :as near]
            [jobs.lib.need :as need]
            [jobs.lib.ore-pick :as ore-pick]
            [jobs.lib.pace :as pace]
            [jobs.lib.result :as result]
            [jobs.lib.shelter :as shelter]
            [jobs.lib.submitted :as submitted]
            [jobs.lib.tools :as tools]
            [jobs.lib.trip :as trip]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]
            [jobs.lib.world :as known]))

(def doc
  "Mine like a player: dig out :count more of one block kind (:block) that the body has seen, strip-tunnelling
  to find more (for an ore at its ore level, :level), then mend the pit under where it started and walk back to the start cell.
  An ore's stone and deepslate kinds both count (:variants): {:block \"iron_ore\" :count 8} gets 8 raw_iron from iron_ore and
  deepslate_iron_ore.

  :center (default: where the body stands) with :radius is the area: the call waits (:outside-area, jobs.lib.area) while the body stands farther than :radius from the centre; the caller walks there first.

  Targets come only from perception (the primitives' seenBlocks, never x-ray). A target is a remembered :block
  within :radius that is still there and has at least one of its 6 face neighbours seen as air or cave_air. It is
  skipped when a seen neighbour is lava, or water unless :wet; lava on top counts as seen (it drips); a face never seen is no hazard. Without perception the body sees nothing and only
  tunnels.

  Impossible: :no-block. Conditions: :moved-since-submit, :no-zones (no zone list and not :ignore-zones?: one warn mine.declined; also
  mid-run, which ends the run), :low-health (can-run?, asked between digs too: the run waits :low-health until it heals)
  and :refused (every seen target refused by zones, claims or plans: one warn mine.declined {:reason :refused :zones :plans}).

  Ends early:
  - The head in water (jobs.lib.breath/head-under?) before a dig: no more digging, warn mine.flooded; before the first dig the job ends
    {:got 0 :reason :flooded}, later the dig phase ends :flooded and the body walks home.
  - Every carried tool that digs the block down to 3 uses (jobs.lib.trip/spent-tool?, no spare was had) ends the same way, :reason :no-tool,
    before the last one breaks deep down.
  - A pickaxe block (jobs.lib.tool-kind/tool-kind) with no *_pickaxe carried, at the start or later (the pick broke),
    gets one with jobs.items.get-tool through jobs.lib.need first (unless :fetch is false). Nothing got: before any dig it ends at once, warn mine.no-tool,
    {:got 0 :reason :no-tool :tool <kind needed: pickaxe, sword, shears>} (:why the fetch's cause); in the dig phase the mend and walk home follow with
    :reason :no-tool. Shovel and axe blocks drop by hand. Soil or wood dug with only a pickaxe held empties the
    hand first (no wear on the pickaxe).

  Before a descent or strip tunnel it stocks torches when under 8 are carried (jobs.lib.trip/torches-ready!: obtain, charcoal from logs; warn torch.no-stock, then it digs dark).
  Before the first round it deposits at the known chest when the bag is fullish (jobs.lib.trip/ready!).

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
     under the start is dug last, and so is any block below the feet (not soil or ore: its hole loses the drop) when one level with them or higher is seen, the block under the feet after all others; a block under the feet whose floor
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
     The limit used up, or the stair stopping, ends :no-stone-found (warn mine.no-stone-found, result
     :descent {:steps :stop}); the mend and the walk home follow.
  6b. Still none, a tunnel level set (:level, else the ore's mine.descent/ore-levels entry, e.g. iron_ore 16; :level false: none) and the
     body above it: a stair down the same way until the feet are at the level; the stair stopping ends the descent there
     (info mine.descent-stopped {:steps :stop :y :level}) and the tunnel runs at that height; more than mine.descent/level-slack (12) above the level
     (warn, same event) the dig phase ends :descent-stopped instead.
  6c. Before the first step of 6 or 6b, a stair this body cut earlier (memory :stair-way) with its top within mine.descent/own-stair-reach and level with
     the body, its foot 3+ below and not below the tunnel level, is walked down (a go-to child) and the new steps go on from its foot.
  7. Else the strip tunnel, at the level then stood on.

  A descent (6, 6b) goes by a mineshaft instead when one is usable (:shaft, jobs.lib.shafts): the body walks to the shaft's top (not when it stands on a step of the shaft: it goes on from there), a
  jobs.access.stair child runs its stair down to the plan's depth, and the strip tunnel is the shaft's next branch
  (shafts/next-for; remembered when the run ends). A shaft in another's zone or claim is not used (:ignore-zones? uses it); one in the
  body's own zone is, without walking out of the zone first. No usable shaft or a stair that stops: the descent above. The text says
  \"via shaft s (stair 54 steps to y 16), branch 2 north 32 blocks\".

  Strip tunnel: a 1-wide 2-high straight run at the level the body stood on at the start, along :heading or :direction
  (north, south, east, west or n/s/e/w; give :heading to choose it, else the way the body faces), at most :tunnel-length blocks per job; it starts past open cells of the line the body cut itself (info
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
  floor is open or unseen (warn mine.not-home when the walk does not arrive; the job then ends :not-home, the ore kept, :dig-reason the earlier reason).
  With the start fewer than mine.end/home-dip blocks above the body the walk plans nothing deeper than that under it (no way round through caves).

  Zones and plans (jobs.lib.access.rules, through jobs.lib.access): a target must be a dig the rules permit,
  with only :accept hazards, when chosen and again right before the dig. A cell in a zone that bars :dig, or in
  an active plan's footprint, is never a target. One refused right before the dig is skipped without a failure
  (info mine.refused), as is one with a hazard not accepted. When all seen blocks are refused after the start, the
  tunnel goes on (with :tunnel-length 0 the dig phase ends :refused).

  Ends :done when :got meets :count (or the call's :least; default :all) and the body got home. Else the cause is
  {:reason r} (:tool, :why): condition-failed for the facts :none :wet :refused :own-zone :no-shaft :flooded
  :no-tool (also a worn last tool), limit-reached {:line :food} for :hungry, nothing-possible for the tries that failed (:no-drops
  :gave-up :tunnel-length :tunnel-stopped :no-stone-found :descent-stopped :spent-on-mend :not-home :not-back :zone-exit). 0 of :count
  is never done. Quote: about ::item-s s an item, open-ended; yields up to :count of the item.

  Data {:got n :reason r} (text: mine got n of m: reason) plus
  :dig-reason, :resumes, :tunnel and :left when set, and
  :mended, the cells filled; its text (and the result's text when :got is 0) names the stair's steps and the tunnel's length,
  heading and height (\"; stair down 48 steps; tunnel 32 blocks east at y 16\"); :descent {:steps :stop} is in the result after any descent. With reason :wet (also when a tunnel dug nothing and seen blocks were skipped for water), :wet-skipped counts the seen blocks left for water beside them. Every ending's text says the reason, :got and :mended, and for a tunnel its length, heading, end cell
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
   :center {:doc "the centre of the area the job works, a cell [x y z] or {:x :y :z}; nil: where the body stands when the job first runs (jobs.lib.area)" :spec a/position? :default nil}
   :wet {:doc "dig blocks that touch water, and tunnel beside water" :spec boolean? :default false}
   :pick-up-ores {:doc "true: dig the ores of jobs.lib.ore-pick's ::ores setting that are exposed within reach of where it stands; false: none; or the ore block names to dig" :spec (a/or-of boolean? (a/coll-of a/name?)) :default true}
   :fetch {:doc "get a pickaxe when none is carried (jobs.items.get-tool through jobs.lib.need): true, a set of kinds or a map of which only :how and :explore apply; false ends :no-tool" :spec need/option? :default true}
   :ignore-zones? :shared
   :spare-own-builds {:doc "a cell of a plan this body made is never a target; false: it may be dug" :spec boolean? :default true}
   :mend {:doc "fill the ground under the start again afterwards" :spec boolean? :default true}
   :collect-radius {:doc "how far around to collect drops after a dig" :spec (a/num-in 0 nil) :default 6}
   :max-failures {:doc "failures in a row before giving up" :spec (a/int-in 1 nil) :default 3}
   :dry-digs {:doc "digs in a row after which the carried count of the item did not rise before giving up (:no-drops)" :spec (a/int-in 1 nil) :default 3}
   :heading {:doc "the strip tunnel's heading, over :direction: north, south, east or west (n/s/e/w), or a cell [x y z] (the dominant axis from the start toward it); nil: :direction" :spec (a/pred "a heading name or a cell [x y z]" #(or (and (or (string? %) (keyword? %)) (heading-name %)) (and (sequential? %) (= 3 (count %)) (every? number? %)))) :default nil}
   :direction {:doc "the strip tunnel's heading: north, south, east or west (n/s/e/w); nil: the way the body faces when the job starts" :spec (a/pred "north, south, east or west (n/s/e/w), a string or keyword" #(and (or (string? %) (keyword? %)) (heading-name %))) :default nil}
   :tunnel-length {:doc "the most blocks the strip tunnel runs in this job, at the body's level; 0: no tunnel, seen blocks only" :spec (a/int-in 0 nil) :default 32}
   :stay-level {:doc "caution for caves: dig only at the level the body starts at (the two-high tunnel, no stair down, no :level), mine only seen ore beside the body (:ore-reach: or a short side cut out), and seal a dug cell that opens into air beyond the tunnel with a carried block (tunnel ends :breach-sealed, or :breach-open when it cannot seal)" :spec boolean? :default false}
   :side-cut? {:doc "set by a run's own side cut (a child mine run starting inside its tunnel): skips the check that :stay-level starts under cover" :spec boolean? :default false}
   :ore-reach {:doc "with :stay-level: seen ore this many blocks into the wall beside the tunnel is taken by a short side cut (a child mine run), shut behind with a carried block; 0 or 1: only ore beside the body" :spec (a/int-in 0 nil) :default 2}
   :variants {:doc "an ore's stone and deepslate kinds both count as the block (iron_ore and deepslate_iron_ore, either named); false: only :block" :spec boolean? :default true}
   :level {:doc "the feet height the strip tunnel runs at (a whole number, or a cell [x y z] / {:x :y :z} whose y it takes): with no target seen the body first stairs down to it (jobs.access.stair) when higher; nil: the ore's level (setting mine.descent/ore-levels), none for other blocks; false: where the body stands"
           :spec (a/pred "a whole number, a cell [x y z] or {:x :y :z}, nil or false" #(or (nil? %) (false? %) (integer? %) (and (sequential? %) (= 3 (count %)) (integer? (second %))) (and (map? %) (integer? (:y %))))) :default nil}
   :shaft {:doc "a mineshaft plan id to descend by instead of digging a stair (jobs.lib.shafts); nil: the nearest usable one within reach, when a descent is due; false: none; :required: the nearest usable one, and no shaft usable ends :no-shaft before anything is dug" :spec (a/pred "a shaft plan id, nil, false or :required" #(or (nil? %) (false? %) (= :required %) (string? %))) :default nil}
   :descend-limit {:doc "the most steps of stair down through soil to find stone, when the block is stone-type and none is in sight; 0: never descend" :spec (a/int-in 0 nil) :default 12}
   :torch-interval {:doc "the strip tunnel hangs a torch every this many steps; 0: none" :spec (a/int-in 0 nil) :default 10}
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent :falling-block); the lava and :wet rules above still hold"
            :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent :falling-block}}})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds a mine call may take by default (its :spend :job-time)." :spec (a/num-in 0 nil)}
   ::stay-margin {:default 8 :doc "Blocks over the higher or under the lower of body and ore that a :stay-level walk to ore may plan through (go-to :y-margin): a body a reflex took far off does not climb a hill to get back." :spec (a/num-in 0 nil)}
   ::item-s {:default 6 :doc "Mean seconds one item takes (walk, dig, collect), for the quote." :spec (a/num-in 0 nil)}})

(def target [:block :heading :center])

(def defaults {:least :all :spend {:job-time ::job-time-s}})

(def safe? ready/safe-if-lit)

(def fit? ready/fit?)

;; ------------------------------------------------------------------ the contract

(defn away
  "The cause of a call submitted with a :from the body has since left (jobs.lib.submitted/away-cause), or nil."
  [c]
  (submitted/away-cause c))

(defn no-zones
  "{:reason :no-zones} while no zone list has been read (unless :ignore-zones?), with one warn mine.declined."
  [c]
  (when (and (:block (:args c)) (nil? (known/zones c)) (not (:ignore-zones? (:args c))))
    (ctx/warn-once! c [:access :no-zones] :mine.declined
                    {:reason :no-zones :text "mine declined: no zone list has been read (zones.edn missing or never valid)"})
    {:reason :no-zones}))

(defn all-refused
  "{:reason :refused ..} when every seen target is refused by zones, claims or plans, with one warn mine.declined."
  [c]
  (let [{:keys [targets refused]} (when (:block (:args c)) (scan c))]
    (when (and (empty? targets) (seq refused))
      (let [fields (assoc (access/refusal-fields refused) :reason :refused)]
        (ctx/warn-once! c [:access :refused] :mine.declined
                        (assoc fields :text (str "mine declined: every target is refused by " (access/refusal-text fields))))
        fields))))

(defn block-given
  "Never :no-block without the :block to mine."
  [c]
  (if (:block (:args c))
    (contract/ok :args "a block given")
    (contract/never :no-block "no :block to mine")))

(defn surface-cause
  "{:reason :on-surface :text ..} for a :stay-level run whose head cell has full sky light (head-light 15, as loaded): it stands in
  the open and its tunnel would breach open at the first cell. Less light is a cave mouth or an overhang (spilled light): nil, as without light data."
  [args head-light]
  (when (and (:stay-level args) (= 15 head-light))
    {:reason :on-surface
     :text "mine :stay-level declined: the body stands in the open; cut a stair down first (access.stair {:dir :down :y 16}) or run it without :stay-level with a :level"}))

(defn on-surface
  "surface-cause of the body's head cell, before the run has begun (a resumed run stands where it dug)."
  [c]
  (let [p (:primitives c)
        {:keys [x y z]} (shelter/feet p)]
    (when-not (or (:goal (ctx/mem c)) (:side-cut? (:args c)))
      (surface-cause (:args c) (shelter/sky-light-at p {:x x :y (inc y) :z z})))))

(def can-start?
  (cond/and (checks/refusing :here "where it was submitted" away)
            area/inside
            block-given
            (checks/refusing :underground "not in the open" on-surface)
            (checks/refusing :zones-read "zone list read" no-zones)
            (checks/refusing :target "a target no zone refuses" all-refused)))

(def can-run? checks/low-health)

(defn done?
  "Progress in items: got against :count. Always :later: the round ends the run after the mend and the walk home, which
  memory does not show."
  [c]
  (let [n (:count (:args c))
        got (if-let [goal (:goal (ctx/mem c))] (max 0 (- (carried c) (- goal n))) 0)]
    (contract/later :mining (str "mine: " got " of " n ", the round ends the run") :units :items :done got :left (max 0 (- n got))
                    :result {:got got})))

(defn quote-of
  "Finding out: about ::item-s per item, open-ended (a tunnel may find none); yields up to :count of the item."
  [c]
  (let [{:keys [count] :as a} (:args c)
        mean (* count (settings/get settings ::item-s))]
    {:uses {:job-time [(min count mean) mean ##Inf]}
     :yields {:items {(blocks/item-name a) [0 count count]}}
     :done [0 count count]}))

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

(defn ^:async leave-zone!
  "Before the first dig: a go-to child to a cell zone-margin outside the own zone zn the body stands near (memory :pre-start moves with it).
  One info mine.left-zone when out; else the job ends :zone-exit. :continue while it walks, :again once out."
  [c zn]
  (let [m (zone-margin)
        goal (zones/exit-goal (cell-of (u/self-pos c)) zn m)
        from (:pre-start (ctx/mem c))
        r (await (child/call-walk* c :leave
                                 {:pos {:x (+ (:x goal) 0.5) :y (:y goal) :z (+ (:z goal) 0.5)} :range 2 :y-slack 8 :zone-tolls true}))
        here (cell-of (u/self-pos c))]
    (cond
      (= :continue r) :continue
      (not (within-own-zone c (dec m) (access/cell here)))
      (do (ctx/emit! c :mine.left-zone :info {:zone (:name zn) :from (access/cell from) :to (access/cell here)
                                              :text (str "mine walked out of zone " (:name zn) " to " (str/join "," (access/cell here)) " before it digs")})
          (ctx/update-mem! c assoc :pre-start here)
          :again)
      :else (end-run c {:got 0 :reason :zone-exit :mended 0
                        :text (str "mine could not get out of zone " (:name zn) " before it digs")}))))

;; ------------------------------------------------------------------ the dig phase

(defn underground?
  "Whether the run has dug down or tunnelled, or the body stands on a floor of a stair it cut earlier: its walks go on at
  night inside its own cuts."
  [c]
  (let [m (ctx/mem c)
        feet (cell-of (u/self-pos c))]
    (boolean (or (pos? (get-in m [:descent :steps] 0)) (:tunnel m)
                 (contains? (ledger/ways-of c) (access/cell (update feet :y dec)))))))

(defn dig-walk-opts
  "The go-near! options of the walk to a dig: other ways only beyond reach, walks inside own cuts ignore the dark (the :checks forms given, when any), a level bound with :stay-level."
  [args in-reach? checks]
  (cond-> {:zone-tolls true}
    in-reach? (assoc :ways #{:walk :clear-seal})
    (seq checks) (assoc :checks checks)
    (:stay-level args) (assoc :y-margin (settings/get settings ::stay-margin))))

(defn ^:async walk-to-dig!
  "Walk toward pos until the body is within reach of it, and close enough to see its drop (range 1) when the line to it
  is blocked from here; a body within reach that cannot get closer digs from there. :there, :partial or :blocked
  (jobs.lib.near/go-near!)."
  [c pos]
  (let [in-reach? (u/within? (u/self-pos c) (cell-of pos) (reach))
        walked (await (near/go-near! c pos (if (or (:stay-level (:args c)) (drop-in-line? c pos)) (reach) 1)
                                     (dig-walk-opts (:args c) in-reach? (when (underground? c) (ready/unforced c {:safe? '(not (threatened))})))))]
    (if (and (not= :there walked) (u/within? (u/self-pos c) (cell-of pos) (reach)))
      :there
      walked)))


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
      (shaft-under-way? c) (await (shaft-round! c))
      (and (some? pos) (way-floor c [pos])) (do (skip! c pos) :again)
      (and (some? pos) (last-exit? c pos)) (do (ctx/emit! c :mine.spared-exit :info {:pos (access/cell pos) :text (str "mine leaves " (str/join "," (access/cell pos)) ": it is the body's last way out")})
                                               (skip! c pos) :again)
      (some? pos) (let [walked (await (walk-to-dig! c pos))]
                    (cond
                      (and (= :blocked walked) (result/unreachable-walk? (near/last-walk c))) (do (skip-unreachable! c pos) :again)
                      (= :blocked walked) (do (skip-failed! c pos) :again)
                      (= :partial walked) (stalled! c pos)
                      (torches/branch-torch-due? c) (await (torches/torch-step! c true))
                      :else (do (await (watch/watch! c {:before-dig pos}))
                                (await (dig! c pos)))))
      (not= looked (cell-of (u/self-pos c))) (do (await (look-around! c)) :again)
      :else (do (when (and wet? (not wet)) (ctx/update-mem! c update :wet-skipped #(max (or % 0) wet-n)))
                (when (seq refused)
                  (access/decline! c :mine.declined "mine" (assoc (access/refusal-fields refused) :reason :refused)))
                (cond
                  (and (empty? refused) (not (and wet? (not wet))) (shaft-due? c)) (await (shaft-round! c))
                  (and (empty? refused) (not (and wet? (not wet))) (descend-due? c)) (await (descend-round! c))
                  (and (seq side) (< (carried c) goal)) (await (wall/side-round! c (first side)))
                  (pos? tunnel-length) (await (tunnel/tunnel-round! c))
                  (seq refused) (to-mend! c :refused)
                  :else (to-mend! c (if (and wet? (not wet)) :wet :none)))))))

(defn ^:async dig-round!
  "A side cut or tunnel step under way goes on first, then a hole dug in the wall is shut, then the ores within reach, then
  one piece of the digging."
  [c]
  (cond
    (:side-cut (ctx/mem c)) (await (wall/side-round! c (:side-cut (ctx/mem c))))
    (:stepping (ctx/mem c)) (await (tunnel/tunnel-round! c))
    (seq (:holes (ctx/mem c))) (await (wall/shut-holes! c))
    (= :continue (await (ore-pick/pick! c {:leave (kinds (:args c))}))) :continue
    :else (await (dig-round-1! c))))

;; ------------------------------------------------------------------ the pickaxe

(defn no-tool!
  "End at once, before any dig: warn, and end with {:got 0 :reason :no-tool :tool <kind>} (end-run), :why the fetch's
  cause when one failed."
  [c & [why]]
  (let [kind (tools/needed-kind (:primitives c) (:block (:args c)))]
    (ctx/emit! c :mine.no-tool :warn {:tool kind :text (str "mine has no " kind " for " (:block (:args c)))})
    (end-run c (cond-> {:got 0 :reason :no-tool :tool kind :mended 0 :text "mine done: no-tool, got 0, mended 0"} why (assoc :why why)))))

(defn fetch-limits
  "The limits map of :fetch when it allows a tool, else nil."
  [c]
  (let [o (need/opts c 'jobs.gather.mine)]
    (when (and o (contains? (:what o) :tool)) o)))

(defn ^:async fetch-pickaxe!
  "A pickaxe is owed: jobs.items.get-tool for the block through jobs.lib.need (:fetch's :how and :explore apply).
  :continue while it runs, :again once it got one; else the end: before any dig (no phase) at once, in the dig phase
  the mend first, with :no-tool."
  [c]
  (let [{:keys [block]} (:args c)
        o (fetch-limits c)
        e (when o (await (need/tool! c {:block block} (select-keys o [:how :explore]))))
        why (when (map? e) (contract/cause-key (or (:because e) e)))]
    (cond
      (= :continue e) :continue
      (tools/can-harvest? (:primitives c) block) :again
      (nil? (:phase (ctx/mem c))) (no-tool! c why)
      :else (let [kind (tools/needed-kind (:primitives c) block)]
              (ctx/emit! c :mine.no-tool :warn {:tool kind :text (str "mine has no " kind " for " block " and could not get one")})
              (ctx/update-mem! c assoc :end-cause (cond-> {:tool kind} why (assoc :why why)))
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
  :not-back. :continue while it walks, :again once there."
  [c]
  (let [{:keys [x y z] :as cell} (:pre-start (ctx/mem c))
        r (await (child/call-walk* c :back
                                 {:pos {:x (+ x 0.5) :y y :z (+ z 0.5)} :range 0 :zone-tolls true
                                  :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (cond
      (= :continue r) :continue
      (at-pre-start? c) :again
      :else (end-run c {:got 0 :reason :not-back :mended 0
                        :text (str "mine could not get back to " (str/join "," (access/cell cell)) " after fetching its tool")}))))

(defn flooded!
  "The head is in water: no more digging (the cut may be what let it in). Warn; before the first dig end at once
  {:got 0 :reason :flooded}, later end the dig phase :flooded and walk home."
  [c]
  (ctx/emit! c :mine.flooded :warn {:text "mine stopped digging: the head is in water"})
  (if (:phase (ctx/mem c))
    (do (ctx/update-mem! c assoc :reason :flooded)
        (wrap-up! c))
    (end-run c {:got 0 :reason :flooded :mended 0 :text "mine got nothing: the head is in water"})))

(defn hungry!
  "Food is short and get-food found none (trip/food-short?; not for soil, dug at the surface near home): before the first dig end at once {:got 0
  :reason :hungry}, later end the dig phase :hungry and walk home."
  [c]
  (ctx/emit! c :mine.hungry :warn {:text "mine stopped digging: hungry, no food carried and get-food found none"})
  (if (:phase (ctx/mem c))
    (do (ctx/update-mem! c assoc :reason :hungry)
        (wrap-up! c))
    (end-run c {:got 0 :reason :hungry :mended 0 :text "mine got nothing: hungry, no food"})))

(defn ^:async step [c]
  (settle-dig! c)
  (let [m (ctx/mem c)
        {:keys [heading direction]} (:args c)]
    (cond
      (and (contains? #{nil :dig} (:phase m)) (breath/head-under? (:primitives c))) (flooded! c)

      (and (contains? #{nil :dig} (:phase m)) (not (contains? soil-blocks (:block (:args c)))) (trip/food-short? c)) (hungry! c)

      (and (contains? #{nil :dig} (:phase m)) (not (tools/can-harvest? (:primitives c) (:block (:args c))))) (await (fetch-pickaxe! c))

      (and (contains? #{nil :dig} (:phase m)) (trip/spent-tool? (:primitives c) (:block (:args c))))
      (do (ctx/update-mem! c assoc :end-cause {:tool (tools/needed-kind (:primitives c) (:block (:args c))) :why :spent})
          (if (nil? (:phase m)) (no-tool! c :spent) (to-mend! c :no-tool)))

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

(defn digs-left
  "The digs the call still has to make: items short of the goal once it is written, else :count (an ore drops at least one item per dig)."
  [c]
  (max 1 (if-let [goal (:goal (ctx/mem c))] (- goal (carried c)) (:count (:args c)))))

(defn ^:async round
  [c]
  (submitted/begin! c)
  (when (and (nil? (:phase (ctx/mem c))) (nil? (:pre-start (ctx/mem c))))
    (ctx/update-mem! c assoc :pre-start (cell-of (u/self-pos c))))
  (if-let [t (or (await (trip/ready! c)) (await (trip/tool-ready! c (:block (:args c)) (digs-left c)))
                 (await (when (and (pos? (or (:torch-interval (:args c)) 0)) (nil? (:tunnel (ctx/mem c))) (or (pos? (:tunnel-length (:args c))) (descent-wanted? c)))
                          (trip/torches-ready! c))))]
    t
    (await (pace/steps! c #(step c)))))

(defn ^:async cleanup
  "A run that ended by itself has walked home already. Cut by the limit, a cancel or a parent's end after it started digging:
  the walk back to the start cell (home-round!), a step per round, done when it ends (arrived or not; not-home warns)."
  [c _]
  (let [m (ctx/mem c)]
    (if (or (:finished m) (not (:start m)))
      (ctx/end c :done nil {})
      (do (ctx/update-mem! c assoc :cleaning true)
          (await (home-round! c))))))
