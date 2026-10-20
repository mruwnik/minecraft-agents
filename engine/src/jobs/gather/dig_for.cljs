(ns jobs.gather.dig-for
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.gather.mine.cause :as cause]
            [jobs.gather.mine.descent :refer [descend-due? descend-round! shaft-due? shaft-under-way? shaft-round!]]
            [jobs.gather.mine.drops :refer [collect! dig-cell!]]
            [jobs.gather.mine.end :refer [stop! wanted]]
            [jobs.gather.mine.side :as wall]
            [jobs.gather.mine.targets :as targets :refer [owed reach kinds carried rules-in scan snapshot way-floor last-exit? skip! skip-failed! skip-unreachable! stalled! drop-in-line? next-target!]]
            [jobs.gather.mine.torches :as torches]
            [jobs.gather.mine.tunnel :as tunnel]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.area :as area]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look :refer [cell-of heading-name facing look-around!]]
            [jobs.lib.near :as near]
            [jobs.lib.need :as need]
            [jobs.lib.ore-pick :as ore-pick]
            [jobs.lib.pace :as pace]
            [jobs.lib.result :as result]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]
            [jobs.lib.world :as known]))

(def doc
  "Dig for the item of one block kind (:block) until :count of it are carried, from where the body stands, with what it carries: the
  digging half of jobs.gather.mine, which equips first, calls this and walks back. An ore's stone and deepslate kinds both count
  (:variants). The run's own area is :center / :radius (waits :outside-area while the body is farther; jobs.lib.area).

  Needs: a pickaxe carried when the block wants one (a shovel or axe block drops by hand), torches carried or craftable when a
  tunnel or stair is lit, food carried when hungry, health (fit? waits :low-health, at the start only), and a read zone list (:no-zones unless
  :ignore-zones?). Impossible: :no-block. :stay-level declines :on-surface in the open. Fetches only seal blocks and torch ingredients (:fetch), never the tool or food, and walks nowhere
  home: the body stays where the dig ends. Targets come only from perception (seenBlocks, never x-ray): a seen block within
  :radius with a seen air face, not beside seen lava (or water unless :wet), that the zone rules, claims and plans let it dig
  (:refused when all are barred).

  One call, in this order each step, until it ends:
  0. A head in water ends :flooded; hungry with no food carried (trip/food-short?; not for soil) ends :hungry; no usable
     tool carried (none that harvests the block, or every one nearly spent) ends :no-tool; no torch left to hang ends :no-torches.
     Each text says what is missing and that jobs.gather.mine gets it first.
  1. Collecting the drop of a dig (mine.targets, mine.drops): collect-drops within the collect-radius setting, one walk or cut to the dug cell, a
     drop still lying is mine.left-behind, a dig that brought nothing and shows no drop is mine.drop-lost. The count is the inventory's.
  2. Carrying the goal ends :count; max-failures digs failed in a row end :gave-up; :dry-digs (setting) digs that raised no count end :no-drops.
  3. Dig the nearest target (the 5x5x2 ground under the start last with :track-ground; a walk within reach first; mine.targets).
  4. No target: look round once per cell, then by a mineshaft (:shaft, mine.descent) or a stair down (:descend-limit, to the ore's :level) to
     find stone or the tunnel level; a stair that finds no stone ends :no-stone-found.
  5. Else the strip tunnel (mine.tunnel): 1-wide 2-high at the start level along :heading / :direction, :tunnel-length blocks, torches where the planner says (jobs.lib.torch-plan;
     :torches); any stop ends the dig :tunnel-stopped with one warn mine.tunnel-end {:reason :at :next :walk}.
  :stay-level (jobs.gather.mine-stay, mine.side) keeps the start height and seals a cut that opens into air; seen ore out of reach is
  :unreached in the result.

  Ore beside the way is picked up by jobs.lib.ore-pick (:pick-up-ores).

  Ends :done when :got meets :count (or the call's :least); else condition-failed for the facts :none :wet :refused :own-zone
  :no-shaft :flooded :no-tool :no-torches, limit-reached {:line :food} for :hungry, nothing-possible for failed tries (:no-drops
  :gave-up :tunnel-length :tunnel-stopped :no-stone-found :descent-stopped :not-back). 0 of :count is never done.
  Data {:got n :reason r} plus :descent, :tunnel, :left, :lost, :ores, :unreached, :ground (with :track-ground), :start-floor-dug (the cell under the start was dug); text says reason,
  :got, the stair and the tunnel. Quote: about ::item-s s an item, open-ended.")

(a/defargs args
  {:block {:doc "name of the block to mine (required)" :spec a/name? :default nil}
   :item {:doc "the item the block drops; nil: the drop-item table, else the block name" :spec a/item? :default nil}
   :count {:doc "how many to carry at least (carried at the start count)" :spec (a/int-in 0 nil) :default 8}
   :radius {:doc "seen blocks within this many blocks of the body count" :spec (a/num-in 0 nil) :default 16}
   :center {:doc "the centre of the area the job works, a cell [x y z] or {:x :y :z}; nil: where the body stands when the job first runs (jobs.lib.area)" :spec a/position? :default nil}
   :wet {:doc "dig blocks that touch water, and tunnel beside water" :spec boolean? :default false}
   :pick-up-ores {:doc "true: dig the ores of jobs.lib.ore-pick's ::ores setting that are exposed within reach of where it stands; false: none; or the ore block names to dig" :spec (a/or-of boolean? (a/coll-of a/name?)) :default true}
   :fetch {:doc "get what the dig runs out of on its way through its children (blocks to seal a cut, the ingredients of torches: jobs.lib.need; not for the stair of a shaft descent, which always fetches): true, a set of kinds or a map of limits; false: it makes do with what is carried. The pickaxe, a spare and the torches are the trip's (jobs.gather.mine)" :spec need/option? :default true}
   :ignore-zones? :shared
   :track-ground {:doc "dig the 5x5x2 ground under the start last and list the cells of it that were open at the end or dug in :ground of the result, for a caller that fills them again" :spec boolean? :default false}
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
   :torches {:doc "hang torches where jobs.lib.torch-plan says a dug cell would stay dark; false: none" :spec boolean? :default true}
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent :falling-block); the lava and :wet rules above still hold"
            :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent :falling-block}}})

(a/defargs settings
  {::dry-digs {:default 3 :doc "Digs in a row after which the carried count of the item did not rise before giving up (:no-drops)." :spec (a/int-in 1 nil)}
   ::max-failures {:default 3 :doc "Dig failures in a row before giving up." :spec (a/int-in 1 nil)}
   ::job-time-s {:default 1800 :doc "Seconds a mine call may take by default (its :spend :job-time)." :spec (a/num-in 0 nil)}
   ::stay-margin {:default 8 :doc "Blocks over the higher or under the lower of body and ore that a :stay-level walk to ore may plan through (go-to :y-margin): a body a reflex took far off does not climb a hill to get back." :spec (a/num-in 0 nil)}
   ::item-s {:default 6 :doc "Mean seconds one item takes (walk, dig, collect), for the quote." :spec (a/num-in 0 nil)}})

(def target [:block :heading :center])

(def defaults {:least :all :spend {:job-time ::job-time-s}})

(def safe? ready/safe-if-lit)

(def fit? ready/fit?)

;; ------------------------------------------------------------------ the contract

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
        {:keys [x y z]} (sh-sense/feet p)]
    (when-not (or (:goal (ctx/mem c)) (:start (ctx/mem c)) (:side-cut? (:args c)))
      (surface-cause (:args c) (sh-sense/sky-light-at p {:x x :y (inc y) :z z})))))

(def can-start?
  (cond/and area/inside
            block-given
            (checks/refusing :underground "not in the open" on-surface)
            (checks/refusing :zones-read "zone list read" no-zones)
            (checks/refusing :target "a target no zone refuses" all-refused)))

(defn done?
  "Progress in items: got against :count carried. :ok at once when :count is carried before the run began, else once the run has finished
  (:finished in memory) with the wanted items carried; else :later, the round ends the run."
  [c]
  (let [n (:count (:args c))
        m (ctx/mem c)
        got (if (:goal m) (targets/got c) 0)
        extra {:units :items :done got :left (max 0 (- (owed c) got)) :result {:got got}}]
    (if (or (and (:finished m) (>= got (wanted c)))
            (and (not (:goal m)) (>= (carried c) n)))
      (apply contract/ok :mined (str "dig-for: " got " of " n) (mapcat identity extra))
      (apply contract/later :mining (str "dig-for: " got " of " n ", the round ends the run") (mapcat identity extra)))))

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
    (when (bs/air (u/seen-name (:primitives c) pos))
      (dug-booked! c pos))))

(defn ^:async dig! [c pos]
  (await (tools/equip! c))
  (let [v (access/may-dig? (rules-in c) pos)
        verdict (access/judge v (:accept (:args c)))]
    (if (not= :ok verdict)
      (do (refused! c pos v verdict) :again)
      (let [_ (ctx/update-mem! c #(cond-> (assoc % :digging pos)
                                   (= (access/cell pos) (when-let [{:keys [x y z]} (:start %)] [x (dec y) z])) (-> (dissoc :start-floor) (assoc :start-floor-dug true))))
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
  (cond-> {}
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
        {:keys [wet tunnel-length]} (:args c)
        dry-digs (settings/get settings ::dry-digs)
        {:keys [targets refused wet? wet-n side far]} (scan c)
        ready? (and (< (carried c) goal) (< (or dry 0) dry-digs) (< failures (settings/get settings ::max-failures)) (some? looked))
        pos (when ready? (await (next-target! c targets)))]
    (when (seq far) (ctx/update-mem! c update :unreached #(into (vec %) (remove (set %)) far)))
    (cond
      (>= (carried c) goal) (stop! c :count)
      (>= (or dry 0) dry-digs) (stop! c :no-drops)
      (>= failures (settings/get settings ::max-failures)) (stop! c :gave-up)
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
                  (seq refused) (stop! c :refused)
                  :else (stop! c (if (and wet? (not wet)) :wet :none)))))))

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

(defn toward
  "The heading name for a :heading arg h at start: a name, or the dominant axis from start to a cell [x y z]; nil when none."
  [start h]
  (if-not (sequential? h)
    (heading-name h)
    (let [dx (- (first h) (:x start)) dz (- (nth h 2) (:z start))]
      (cond (and (zero? dx) (zero? dz)) nil
            (>= (js/Math.abs dx) (js/Math.abs dz)) (if (pos? dx) "east" "west")
            :else (if (pos? dz) "south" "north")))))

(defn stop-cause
  "mine.cause/stop-cause: why the dig cannot go on, as {:reason r :text t ..}, else nil."
  [c]
  (cause/stop-cause c))

(defn ^:async step [c]
  (settle-dig! c)
  (let [m (ctx/mem c)
        {:keys [heading direction]} (:args c)]
    (cond
      (stop-cause c) (let [cause (stop-cause c)]
                       (stop! c (:reason cause) (select-keys cause [:tool :why :text])))

      (nil? (:phase m))
      (let [faced (facing (:primitives c))
            _ (await (look/survey! c))
            now (carried c)
            start (cell-of (u/self-pos c))
            heading (or (toward start heading) (heading-name direction))]
        (ctx/update-mem! c assoc
                         :goal (:count (:args c)) :had now :start start :start-floor (u/feel-name (:primitives c) {:x (:x start) :y (dec (:y start)) :z (:z start)})
                         :failures 0 :dry 0 :last-carried now
                         :heading (or heading faced)
                         :ground (if (:track-ground (:args c)) (snapshot c start) [])
                         :phase :dig)
        :again)

      (:collecting m) (await (collect! c))
      :else (await (dig-round! c)))))

(defn ^:async round
  [c]
  (await (pace/steps! c #(step c))))

(def meta {:tags [:mine :dig :ore :light] :position-bound? true})
