(ns jobs.gather.mine
  (:require [jobs.lib.blocks :as blocks]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.gate :as gate]
            [jobs.lib.tools :as tools]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.targets :as targets]
            [jobs.lib.watch :as watch]
            [jobs.lib.reach :as reach-lib]
            [jobs.lib.look :as look :refer [cell-of headings heading-name facing glance! look-around!]]
            [jobs.lib.torch :as torch]
            [jobs.lib.placement :as placement]
            [jobs.lib.world :as known]))

(def doc
  "Mine like a player: dig out :count more of one block kind (:block) that the body has seen, strip-tunnelling
  at its own level to find more, then mend the pit under where it started and walk back to the start cell.

  Targets come only from perception (the primitives' seenBlocks, never x-ray). A target is a remembered :block
  within :radius that is still there and has at least one of its 6 face neighbours seen as air or cave_air. It is
  skipped when a seen neighbour is lava, or water unless :wet; lava on top counts as seen (it drips); a face never seen is no hazard. Without perception the body sees nothing and only
  tunnels.

  Declines and ends early:
  - A pickaxe block (jobs.lib.tools/tool-kind) with no *_pickaxe carried, at the start or later (the pick broke),
    runs jobs.items.get-tool as a child first (unless :fetch is false). Nothing got: before any dig it ends at once, warn mine.no-tool,
    {:status :stopped :got 0 :reason :no-tool :tool <kind needed: pickaxe, sword, shears>}; in the dig phase the mend and walk home follow with
    :reason :no-tool. Shovel and axe blocks drop by hand. Soil or wood dug with only a pickaxe held empties the
    hand first (no wear on the pickaxe).
  - The check passes when a phase is in memory, or :block is named, unless every seen target is refused.
  - No zone list (zones.edn missing or never valid) declines with one warn mine.declined {:reason :no-zones},
    also in the middle of the job. Nothing is dug or placed then.

  The first round writes to job memory before any dig: the goal (carried + :count), the start cell, the
  tunnel heading (:direction, else the way the body faces) and the ground (the solid cells of the 5x5 under the
  start at y-1 and y-2; empty when :mend is false). So a cut or restart still mends.

  One call runs these steps in order, again and again until the job ends (it yields with :continue only while a child or
  a walk waits on the world, and after 400 steps):
  1. Collecting: the collect-drops child gathers the item within :collect-radius. The count is the
     inventory's, never the dig's report. Drops of the item still lying afterwards (out of reach) are reported
     once each, info mine.left-behind {:items [{:pos :count}]}. The result's :left is re-checked after the walk
     home and keeps only drops still lying then.
  2. Carrying the goal ends :count.
  3. :max-failures failures end :gave-up (warn mine.gave-up). :dry-digs digs in a row after which the carried
     count did not rise end :no-drops (warn mine.gave-up).
  4. Dig the nearest target by walking (one bounded search, jobs.lib.targets, going on in the next step; a seen
     block out of every stand's reach is passed over; when the search finds none reachable, the nearest in a straight
     line is tried; targets over the ground snapshot come last, so the floor
     under the start is dug last; targets whose drop lies in a clear line from the eye come first, and a body
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
  7. Else the strip tunnel, at the level then stood on.

  Strip tunnel: a 1-wide 2-high straight run at the level the body stood on at the start, along :direction
  (north, south, east, west or n/s/e/w), at most :tunnel-length blocks per job. 0 means no tunnel: then no
  target ends :wet when a seen block was rejected only for water, else :none. Each step stands on the last
  cell of the run and judges the next cell and the one over it: no fluid in it, no seen lava beside it, no seen water
  beside it unless :wet, a solid floor, the zone and plan rules with only :accept hazards. It digs them head
  first (a cut of :block is collected like a target), looks ahead level and down so perception records what
  the cut exposed, and steps in. Ore that comes into view is then an ordinary target and is dug before the
  next step.

  The run used up ends the dig phase :tunnel-length, any stop ends it :tunnel-stopped. Either way one warn
  mine.tunnel-end {:reason (:tunnel-length :lava :water :fluid :no-floor :not-loaded :refused :dig-failed
  :walk-failed) :heading :length :at :next}. :at is the offending cell, :next the line cell it stopped before.

  Torches: a dig of seen ore that takes the body 4+ blocks off the tunnel line hangs one torch there the same way
  (none within 4 of an earlier branch torch). The strip tunnel hangs a torch (jobs.access.tunnel/torch-at: a wall torch in the head cell, else a floor
  torch) behind the body at its first step, then every :torch-interval steps, and at the cut's end when none hangs
  within 4 blocks of it; 0 hangs none. Torches are the body's own and stay: nothing takes them back. Under 2
  carried with a coal or charcoal and a stick, the torch craft is run as a child (4 torches). With no torch and
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

  Every job ends, after the mend, by walking back to the cell it started on (warn mine.not-home when the walk
  does not arrive; the job then ends {:status :stopped :reason :not-home}, the ore kept, :dig-reason the earlier reason).

  Zones and plans (jobs.lib.access.rules, through jobs.lib.access): a target must be a dig the rules permit,
  with only :accept hazards, when chosen and again right before the dig. A cell in a zone that bars :dig, or in
  an active plan's footprint, is never a target. One refused right before the dig is skipped without a failure
  (info mine.refused), as is one with a hazard not accepted. When all seen blocks are refused: before the
  first round the check declines. In the job the tunnel goes on (with :tunnel-length 0 the dig phase ends
  :refused). One warn mine.declined per job names the zones and plans ({:reason :refused :zones :plans}).

  Hands over {:got n :reason r} (with :status :stopped when :got is 0 or the walk home failed, :reason :not-home: never completed) plus
  :dig-reason, :resumes, :tunnel and :left when set; info mine.done with
  :mended, the cells filled. With reason :wet (also when a tunnel dug nothing and seen blocks were skipped for water), :wet-skipped counts the seen blocks left for water beside them. Its text says the reason, :got and :mended, and for a tunnel its length, heading, end cell
  and whether the body walked back. :got is how many more are carried than at the start, at least 0. :tunnel is
  {:origin :heading :steps :stop :end :back-at :walked-back?}, :end the cell it ended on before the walk back.")

(def args
  {:block {:doc "name of the block to mine (required)" :default nil}
   :item {:doc "the item the block drops; nil: the drop-item table, else the block name" :default nil}
   :count {:doc "how many more to carry than at the start" :default 8}
   :radius {:doc "seen blocks within this many blocks of the body count" :default 16}
   :wet {:doc "dig blocks that touch water, and tunnel beside water" :default false}
   :fetch {:doc "get a pickaxe when none is carried (jobs.items.get-tool via jobs.lib.fetch limits): true, a set of kinds or a map of limits; false ends :no-tool" :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :spare-own-builds {:doc "a cell of a plan this body made is never a target; false: it may be dug" :default true}
   :mend {:doc "fill the ground under the start again afterwards" :default true}
   :collect-radius {:doc "how far around to collect drops after a dig" :default 6}
   :max-failures {:doc "failures in a row before giving up" :default 3}
   :dry-digs {:doc "digs in a row after which the carried count of the item did not rise before giving up (:no-drops)" :default 3}
   :direction {:doc "the strip tunnel's heading: north, south, east or west (n/s/e/w); nil: the way the body faces when the job starts" :default nil}
   :tunnel-length {:doc "the most blocks the strip tunnel runs in this job, at the body's level; 0: no tunnel, seen blocks only" :default 32}
   :descend-limit {:doc "the most steps of stair down through soil to find stone, when the block is stone-type and none is in sight; 0: never descend" :default 12}
   :torch-interval {:doc "the strip tunnel hangs a torch every this many steps; 0: none" :default 10}
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent :falling-block :under-feet); the lava and :wet rules above still hold"
            :default #{:fluid-adjacent :falling-block :under-feet}}})

(def reach 3)
(def mend-reach 4)
(def max-mend-failures 6)
(def air #{"air" "cave_air"})
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
               (own? cell) (u/block-name p cell)
               (and (= [0 1 0] f) (= "lava" (u/block-name p cell))) "lava"
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
  "The rules input: another's plans always refuse, this body's own too unless :spare-own-builds is false."
  [c]
  (access/rules-input c {:own-plans-ok? (false? (:spare-own-builds (:args c)))}))

(defn scan
  "{:targets [pos] nearest first (the higher of two as near), those over the ground snapshot after all others, :refused [verdict] of blocks a
  zone or plan refuses, :wet? true when an unskipped block was rejected only for water, :wet-n how many}. Only seen blocks that are
  still there count."
  [c]
  (let [{:keys [block radius wet accept]} (:args c)
        p (:primitives c)
        skipped (set (:skipped (ctx/mem c)))
        ground (into #{} (map :pos) (:ground (ctx/mem c)))
        here (u/self-pos c)
        cells (->> (look/seen-blocks p {:names [block] :radius radius :max 128 :live? true})
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
  (cond
    (and (nil? (known/zones c)) (not (:ignore-zones? (:args c)))) (access/decline! c :mine.declined "mine" {:reason :no-zones})
    (:error (fetch/opts c 'jobs.gather.mine))
    (let [why (:error (fetch/opts c 'jobs.gather.mine))]
      (ctx/warn-once! c [:fetch :bad-args] :mine.declined {:reason :bad-args :why why :text (str "mine declined: " why)})
      (ctx/wait c {:reason :bad-args :why why}))
    (:phase (ctx/mem c)) true
    (not (:block (:args c))) (ctx/wait c {:reason :no-block})
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
  (let [{:keys [goal mended dig-reason resumes tunnel left descent wet-skipped] :as m} (ctx/mem c)
        got (max 0 (- (carried c) (- goal (:count (:args c)))))
        reason (if (and (zero? got) (pos? (or wet-skipped 0)) (#{:none :no-drops :tunnel-length :tunnel-stopped} (:reason m))) :wet (:reason m))
        why (cond-> {} dig-reason (assoc :dig-reason dig-reason) (= :no-stone-found reason) (assoc :descent descent) (= :wet reason) (assoc :wet-skipped (or wet-skipped 0)) resumes (assoc :resumes resumes)
              (pos? (:steps tunnel 0)) (assoc :tunnel (-> tunnel (select-keys [:origin :heading :steps :stop :end :back-at :walked-back?]) (update :origin access/cell)))
              (seq left) (assoc :left (mapv (fn [[pos n]] {:pos pos :count n}) left)))]
    (ctx/emit! c :mine.done (if (= :not-home reason) :warn :info) (merge {:got got :reason reason :mended (or mended 0)
                                          :text (str "mine done: " (name reason) ", got " got ", mended " (or mended 0)
                                                     (when (= :no-stone-found reason) (str "; dug down " (:steps descent 0) " blocks through soil, found no stone"))
                                                     (when (= :wet reason) (str "; " (or wet-skipped 0) " seen blocks beside water skipped, pass :wet true to dig them"))
                                                     (when-let [{:keys [end back-at walked-back?]} (when (pos? (:steps tunnel 0)) tunnel)]
                                                       (str "; tunnel " (:steps tunnel) " blocks " (:heading tunnel)
                                                            (when end (str ", ended at " (str/join "," end)))
                                                            (if walked-back?
                                                              (str ", walked back to " (str/join "," back-at))
                                                              ", did not get back to its origin"))))}
                                         why))
    (ctx/result! c (merge (cond
                            (zero? got) {:status :stopped :text (str "mine got nothing: " (name reason)
                                                                     (when (= :no-stone-found reason) (str "; dug down " (:steps descent 0) " blocks through soil, found no stone")))}
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
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                 {:radius (drop-radius c) :filter [(blocks/item-name (:args c))]}))]
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
    (when (air (u/block-name (:primitives c) pos))
      (dug-booked! c pos))))

(defn ^:async dig-cell!
  "Dig pos with a jobs.blocks.dig child: this job's rules judged the cell, the hazards it accepts are the child's."
  [c pos]
  (await (blocks/dig-cell! c pos {:accept #{:fluid-adjacent :falling-block :under-feet} :ignore-zones? (boolean (:ignore-zones? (:args c)))})))

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
  (let [own (u/block-name (:primitives c) pos)
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
  "Dig the cells in order (equip, rules, a blocks.dig child): :ok when each is air afterwards, else :refused or :dig-failed. A cut
  of the mined block is collected like a target."
  [c cells]
  (loop [cells cells]
    (if-let [pos (first cells)]
      (let [block (u/block-name (:primitives c) pos)
            _ (await (tools/equip! c block))
            v (access/may-dig? (rules-in c) pos)
            verdict (access/judge v (:accept (:args c)))
            _ (when (and (= :ok verdict) (= block (:block (:args c)))) (ctx/update-mem! c assoc :digging pos))
            outcome (when (= :ok verdict) (await (dig-cell! c pos)))
            _ (when-not (= :continue outcome) (ctx/update-mem! c dissoc :digging))]
        (cond
          (not= :ok verdict) (do (refused! c pos v verdict) :refused)
          (= :continue outcome) :continue
          (not (#{:dug :missing} outcome)) :dig-failed
          :else (do (when (and (= :dug outcome) (= block (:block (:args c))))
                      (ctx/update-mem! c assoc :collecting true :dug-at (access/cell pos)))
                    (recur (rest cells)))))
      :ok)))

;; ------------------------------------------------------------------ torches

(def end-torch-gap "A cut's end gets a torch when the last hangs this many steps back or more." 4)

(defn count-of [c names]
  (transduce (comp (filter #(contains? names (:name %))) (map :count)) + 0 (u/inventory (:primitives c))))

(defn craftable? [c]
  (and (pos? (count-of c #{"coal" "charcoal"})) (pos? (count-of c #{"stick"}))))

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
  (and (pos? (or (:torch-interval (:args c)) 0)) (on-line? c) (>= (torch-gap c) end-torch-gap)))

(def branch-gap "A dig this far from the tunnel line (and from any torch hung on a branch) gets a torch of its own." 4)

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
         (>= (line-dist tunnel here) branch-gap)
         (every? #(>= (u/dist here %) branch-gap) branch-torches))))

(defn booked!
  "Book the torch step done: a tunnel torch at the tunnel's length, a branch one at the body's cell."
  [c branch]
  (if branch
    (ctx/update-mem! c update :branch-torches (fnil conj []) (cell-of (u/self-pos c)))
    (ctx/update-mem! c assoc :torch-at (get-in (ctx/mem c) [:tunnel :steps]))))

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
  "Hang a torch on the cell behind the body (a branch: behind it on its way off the tunnel)."
  [c branch]
  (let [p (:primitives c)
        {:keys [tunnel]} (ctx/mem c)
        block-at (fn [[x y z]] (u/block-name p {:x x :y y :z z}))
        {:keys [dir site]} (if branch (branch-site tunnel (cell-of (u/self-pos c))) {:dir (headings (:heading tunnel)) :site (step-cell tunnel (dec (:steps tunnel)))})
        choice (when site (torch/torch-at dir [(:x site) (:y site) (:z site)]
                                          (placement/eye (u/self-pos c)) block-at))
        cell (:cell choice)
        reason (cond (nil? site) :no-site
                     (:refused choice) (:refused choice)
                     (not (gate/allowed? c :mine.declined "mine" :place (zipmap [:x :y :z] cell))) :refused)]
    (if reason
      (left-out! c cell reason branch)
      (let [r (await (ctx/act c :place (clj->js {:pos (zipmap [:x :y :z] cell) :item "torch"
                                                 :click (placement/js-click (:click choice))})))]
        (if branch (booked! c branch) (ctx/update-mem! c assoc :torch-at (:steps tunnel)))
        (when-not (or (= "placed" (.-status r)) (torch/torch-blocks (block-at cell)))
          (left-out! c cell :place-failed branch))))))

(defn ^:async torch-step!
  "One round of a torch (the tunnel's, or a branch's when branch): craft more when under 2 are carried and coal or
  charcoal and a stick are, say so once with none to hang, else hang one. :continue."
  [c branch]
  (let [m (ctx/mem c)
        n (torch/torches-carried (:primitives c))]
    (cond
      (and (< n 2) (craftable? c) (not (:craft-failed m)))
      (let [r (await (ctx/call-child c :torches 'jobs.items.craft {:item "torch" :count 4 :fetch false}))]
        (when (and (not= :continue r) (not (and (= :done r) (pos? (:made (ctx/child-result c :torches) 0)))))
          (ctx/update-mem! c assoc :craft-failed true))
        (if (= :continue r) :continue :again))

      (zero? n)
      (do (when-not (get-in m [:no-torches-said (boolean branch)])
            (ctx/emit! c :mine.no-torches :info {:text (str "mine has no torches and nothing to craft them from: the "
                                                            (if branch "branch" "tunnel") " stays dark")}))
          (ctx/update-mem! c assoc-in [:no-torches-said (boolean branch)] true)
          (booked! c branch)
          :again)

      :else (do (await (hang-torch! c branch)) :again))))

(defn ^:async end!
  "tunnel-end!, after a torch at the cut's end when one is due."
  [c stop at & [next]]
  (if (end-torch-due? c)
    (await (torch-step! c false))
    (tunnel-end! c stop at next)))

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
      (>= steps (:tunnel-length (:args c))) (end! c :tunnel-length nil)
      (not (await (step-to! c from))) (end! c :walk-failed from)
      (torch-due? c) (await (torch-step! c false))
      hazard (end! c (:reason hazard) (:at hazard) next)
      (not (rules/solid-floor? name-at (update next :y dec))) (end! c :no-floor next)
      :else (let [_ (await (watch/watch! c {:risky? true :before-dig (first (remove #(air (name-at %)) cut))}))
                  r (await (cut! c (remove #(air (name-at %)) cut)))]
              (cond
                (= :continue r) :continue
                (not= :ok r) (end! c r next)
                :else
                (do (await (glance! c [(headings heading)]))
                    (if (await (step-to! c next))
                      (do (ctx/update-mem! c #(-> % (update-in [:tunnel :steps] inc) (assoc :looked next)))
                          :again)
                      (end! c :walk-failed next))))))))

;; ------------------------------------------------------------------ the dig phase

(defn drop-in-line?
  "Whether the line from the body's eye to where the drop of the block at pos will lie crosses no solid block (one behind
  another cannot be aimed at, nor its item seen)."
  [c pos]
  (let [p (:primitives c)
        {:keys [x y z]} (u/self-pos c)
        kind-at (fn [x y z] (reach-lib/arrow-kind-of (u/block-at p {:x x :y y :z z})))]
    (reach-lib/ray-clear? kind-at
                          [(+ (js/Math.floor x) 0.5) (+ y reach-lib/eye-height) (+ (js/Math.floor z) 0.5)]
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
  (let [walked (await (near/go-near! c pos (if (drop-in-line? c pos) reach 1) {:zone-tolls true}))]
    (if (and (not= :there walked) (u/within? (u/self-pos c) (cell-of pos) reach))
      :there
      walked)))

(defn ^:async next-target!
  "The target to walk to next: of the targets off the ground snapshot (else those over it) that the body sees in a
  clear line when any does, the one the body walks to
  soonest (targets/nearest!), :searching while that search goes on, the nearest in a line when none is found reachable
  (its walk decides); nil with no targets."
  [c targets]
  (let [ground (into #{} (map :pos) (:ground (ctx/mem c)))
        off (vec (remove ground targets))
        group (in-line c (if (seq off) off targets))]
    (when (seq group)
      (let [a (await (targets/nearest! c group reach {:tag :mine}))]
        (case (:status a)
          :found (:target a)
          :searching :searching
          (first group))))))

(def stone-types #{"stone" "cobblestone" "deepslate" "andesite" "granite" "diorite" "tuff"})

(defn descend-due?
  "Whether the dig phase should first stair down: the block is stone-type, no seen target (nor seen block skipped for water), the strip tunnel not begun."
  [c]
  (let [{:keys [block descend-limit]} (:args c)]
    (and (contains? stone-types block) (pos? descend-limit) (nil? (:tunnel (ctx/mem c))))))

(defn no-stone!
  "The stair down found no stone: one warn mine.no-stone-found, then the mend and the walk home with :reason :no-stone-found."
  [c]
  (let [stop (:stop (:descent (ctx/mem c)))
        steps (:steps (:descent (ctx/mem c)) 0)]
    (ctx/emit! c :mine.no-stone-found :warn {:steps steps :stop stop :text (str "mine dug down " steps " blocks through soil, found no stone"
                                                                               (when stop (str "; the stair stopped: " (name stop))))})
    (to-mend! c :no-stone-found)))

(defn ^:async descend-round!
  "One step of the stair down (jobs.access.stair child, :steps 1, :fetch) along the tunnel heading; the next round
  looks round and scans. At :descend-limit steps, or when the stair stops, ends :no-stone-found."
  [c]
  (let [n (:steps (:descent (ctx/mem c)) 0)]
    (if (>= n (:descend-limit (:args c)))
      (no-stone! c)
      (let [r (await (ctx/call-child c :stair 'jobs.access.stair
                                     {:dir :down :heading (keyword (:heading (ctx/mem c))) :steps 1 :fetch true
                                      :ignore-zones? (:ignore-zones? (:args c))}))
            res (when (not= :continue r) (ctx/child-result c :stair))]
        (cond
          (= :continue r) :continue
          (and (= :done r) (= :done (:status res)))
          (do (ctx/update-mem! c update-in [:descent :steps] (fnil + 0) (or (:steps res) 1)) :again)
          :else (do (ctx/update-mem! c update :descent assoc :stop (or (:reason res) :declined))
                    (no-stone! c)))))))

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
    (filterv #(let [name (u/block-name (:primitives c) (:pos %))]
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
  (let [walked (if (> (u/dist (u/self-pos c) pos) mend-reach)
                 (await (near/go-near! c pos reach {:zone-tolls true}))
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

(def max-resumes 2)

(defn ^:async mended!
  "Nothing is owed. The mend spent the count: look around once from here, then dig on while targets off the ground
  remain (at most max-resumes times). Else finish."
  [c]
  (let [{:keys [resumes reason looked]} (ctx/mem c)
        more? (some #(off-ground? c %) (:targets (scan c)))]
    (cond
      (not (spent-on-mend? c)) (wrap-up! c)
      (not= looked (cell-of (u/self-pos c))) (do (await (look-around! c)) :again)
      (and more? (< (or resumes 0) max-resumes))
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
      (>= (:mend-failures (ctx/mem c) 0) max-mend-failures)
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

(defn ^:async home-by-go-to!
  "After a descent the way home is the stair up (and a pit the digging left at its foot): go-to walks it, with its
  escalation. :continue while the walk goes on, else the job ends."
  [c start]
  (let [at-home? #(= (cell-of (u/self-pos c)) start)
        r (if (at-home?)
            :arrived
            (await (ctx/call-child c :home 'jobs.movement.go-to
                                   {:pos {:x (+ (:x start) 0.5) :y (:y start) :z (+ (:z start) 0.5)} :range 0 :escalate true :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))})))]
    (if (= :continue r)
      :continue
      (home-done! c (at-home?)))))

(defn ^:async home-round!
  "The last step of every normal run: back to the cell it started on (moveTo; go-to after a descent), whatever the
  digging left open behind it; a warn mine.not-home when the walk did not arrive (the job ends :stopped :not-home). The job ends either way."
  [c]
  (let [{:keys [start descent homing]} (ctx/mem c)]
    (when-not homing
      (ctx/update-mem! c assoc :homing true)
      (ctx/update-mem! c assoc-in [:tunnel :end] (access/cell (cell-of (u/self-pos c)))))
    (if (pos? (:steps descent 0))
      (await (home-by-go-to! c start))
      (home-done! c (await (step-to! c start))))))

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

(defn ^:async step [c]
  (settle-dig! c)
  (let [m (ctx/mem c)
        heading (heading-name (:direction (:args c)))]
    (cond
      (and (contains? #{nil :dig} (:phase m)) (no-tool? c)) (await (fetch-pickaxe! c))

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
        :again)

      (= :home (:phase m)) (await (home-round! c))
      (= :mend (:phase m)) (await (mend-round! c))
      (:collecting m) (await (collect! c))
      :else (await (dig-round! c)))))

(def max-steps "Pieces of work of one call before it gives the round back with :continue." 400)

(defn ^:async round
  [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) max-steps) (step c) :continue)))))
