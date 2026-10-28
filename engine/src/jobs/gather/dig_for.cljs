(ns jobs.gather.dig-for
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.need :as need]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.gather.mine.cause :as cause]
            [jobs.gather.mine.descent :refer [descend-due? descend-round! dig-here-due? shaft-due? shaft-under-way? shaft-round!]]
            [jobs.gather.mine.drops :refer [collect! dig-cell! cut-to-drop! cut-after? target-cut-max]]
            [jobs.gather.mine.end :refer [stop! wanted]]
            [jobs.gather.mine.branch :as branch]
            [jobs.gather.mine.side :as wall]
            [jobs.gather.mine.targets :as targets :refer [owed reach kinds items main-block carried rules-in scan snapshot way-floor last-exit? skip! skip-failed! skip-unreachable! stalled! drop-in-line? next-target!]]
            [jobs.gather.mine.torches :as torches]
            [jobs.gather.mine.tunnel :as tunnel]
            [jobs.gather.mine.way-set :as mw]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.area :as area]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.lava :as lava]
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
  "Dig for an item (:item, or what :block drops; several items count together) until :count of it are carried, from where the body stands, with what it carries: the
  digging half of jobs.gather.mine, which equips first, calls this and walks back. The blocks that drop it come from the drop table (an ore's
  stone and deepslate kinds both); :block narrows them. The run's own area is :center / :radius (walks there first while the body is farther; jobs.lib.area).

  Needs: a pickaxe carried when the block wants one (a shovel or axe block drops by hand), torches carried or craftable when a
  tunnel or stair is lit, health (fit? waits :low-health, at the start only), and a read zone list (:no-zones unless
  :ignore-zones?). Impossible: :no-target. :ways without a descent decline :on-surface in the open. Fetches only seal blocks and torch ingredients (the call's ways), never the tool or food, and walks nowhere
  home: the body stays where the dig ends. Targets come only from perception (seenBlocks, never x-ray): a seen block within
  :radius with a seen air face, not beside seen lava (or water unless :wet), that the zone rules, claims and plans let it dig
  (:refused when all are barred).

  One call, in this order each step, until it ends:
  0. A head in water ends :flooded; no usable
     tool carried (none that harvests the block, or every one nearly spent) ends :no-tool; no torch left to hang ends :no-torches.
     Each text says what is missing and that jobs.gather.mine gets it first.
  1. Collecting the drop of a dig (mine.targets, mine.drops): collect-drops within the collect-radius setting, one walk or cut to the dug cell, a
     drop still lying is mine.left-behind, a dig that brought nothing and shows no drop is mine.drop-lost (judged within 8 blocks of the dug cell; from farther mine.drop-unreached). The count is the inventory's.
  2. Carrying the goal ends :count; max-failures digs failed in a row end :gave-up; :dry-digs (setting) digs that raised no count end :no-drops.
  3. Dig the nearest target (the 5x5x2 ground under the start last with :track-ground; a walk within reach first; mine.targets).
  4. No target: look round once per cell, then by a mineshaft (:descend-shaft, mine.descent) or a stair down (:descend-stair, :descend-limit, to the ore's :level) to
     find stone or the tunnel level; a stair that finds no stone ends :no-stone-found.
  5. Else the strip tunnel (mine.tunnel): 1-wide 2-high at the start level along :heading / :direction, :tunnel-length blocks, torches where the planner says (jobs.lib.torch-plan;
     :torches); any stop ends the dig :tunnel-stopped with :stop :length :at :next-cell :walk :fluid :behind.
  a level-keeping tunnel (no descending way; jobs.gather.mine-stay, mine.side) keeps the start height and seals a cut that opens into air; seen ore out of reach is
  :unreached in the result.

  It marks itself for the :ore-seen trigger (jobs.lib.ore-pick): ore beside the way is dug, except the block it digs.

  Ends :done when :got meets :count (or the call's :least); else condition-failed for the facts :none :wet :refused :own-zone
  :no-shaft :flooded :no-tool :no-torches, nothing-possible for failed tries (:no-drops
  :gave-up :tunnel-length :tunnel-stopped :no-stone-found :descent-stopped :not-back). 0 of :count is never done.
  Data {:got n :reason r} plus :descent, :tunnel, :left, :lost, :unreached, :ground (with :track-ground), :start-floor-dug (the cell under the start was dug), :cut-blocks (the tunnel, stair, branch and side-cut cells it dug); text says reason,
  :got, the stair and the tunnel. Quote: about ::item-s s an item, open-ended.")

(a/defargs args
  {:block :shared
   :item :shared
   :count :shared
   :radius :shared
   :center :shared
   :wet :shared
   :skip {:doc "cells [x y z] never targeted (a caller passes the ones an earlier call gave up on: :skipped of its result)" :spec (a/coll-of (a/pred "a cell [x y z]" #(and (sequential? %) (= 3 (count %)) (every? number? %)))) :default nil}
   :ignore-zones? :shared
   :track-ground {:doc "dig the 5x5x2 ground under the start last and list the cells of it that were open at the end or dug in :ground of the result, for a caller that fills them again" :spec boolean? :default false}
   :heading :shared
   :direction :shared
   :tunnel-length :shared
   :branches {:doc "on a level-keeping tunnel (no descending way): cut a side branch to each side of the tunnel every few blocks (mine-stay ::branch-every, ::branch-length), more rock in view per block walked" :spec boolean? :default false}
   :level :shared
   :shaft :shared
   :descend-limit :shared
   :torches :shared
   :shed {:doc "stop :surplus between digs while the pack holds junk (what make-room would throw) over jobs.lib.trip/::junk-cap, for a caller (jobs.gather.mine) that throws it away and goes on" :spec boolean? :default false}
   :accept :shared})

(a/defargs settings
  {::dry-digs {:default 3 :doc "Digs in a row after which the carried count of the item did not rise before giving up (:no-drops)." :spec (a/int-in 1 nil)}
   ::max-failures {:default 3 :doc "Dig failures in a row before giving up." :spec (a/int-in 1 nil)}
   ::job-time-s {:default 1800 :doc "Seconds a mine call may take by default (its :spend :job-time)." :spec (a/num-in 0 nil)}
   ::stay-margin {:default 8 :doc "Blocks over the higher or under the lower of body and ore that a level-keeping walk to ore may plan through (go-to :y-margin): a body a reflex took far off does not climb a hill to get back." :spec (a/num-in 0 nil)}
   ::item-s {:default 6 :doc "Mean seconds one item takes (walk, dig, collect), for the quote." :spec (a/num-in 0 nil)}})

(def target [:item :block :heading :center])

(def ways
  "The ways the dig may use (the call's :ways, default all): :seen (dig what is in view), :descend-stair (stair down to the ore's level, or through soil to stone; :descend-limit steps), :descend-shaft (down a mineshaft plan, :shaft names it; with no :descend-stair and no usable shaft the run ends :no-shaft before anything is dug), :strip-tunnel (a tunnel at the start level, :tunnel-length blocks). With :strip-tunnel and no descent the tunnel keeps its level: seen ore only beside it (or by a short side cut), and a cut that opens into air beyond it is sealed with a carried block."
  (into mw/ordered need/fetch-ways))

(def defaults {:least :all :spend {:job-time ::job-time-s}})

(def safe? (ready/unless-outmatched (price/staying :long (ready/safe-at-or-roaming work-place/dig-spot true))))

(def fit? ready/fit?)

;; ------------------------------------------------------------------ the contract

(defn no-zones
  "{:reason :no-zones} while no zone list has been read (unless :ignore-zones?)."
  [c]
  (when (and (seq (items (:args c))) (nil? (known/zones c)) (not (:ignore-zones? (:args c))))
    {:reason :no-zones}))

(defn all-refused
  "{:reason :refused ..} when every seen target is refused by zones, claims or plans."
  [c]
  (let [{:keys [targets refused]} (when (seq (items (:args c))) (scan c))]
    (when (and (empty? targets) (seq refused))
      (assoc (access/refusal-fields refused) :reason :refused))))

(defn target-given
  "Never :no-target without an :item (or a :block) to mine for."
  [c]
  (if (seq (items (:args c)))
    (contract/ok :args "a target given")
    (contract/never :no-target "no :item to mine for")))

(defn surface-cause
  "{:reason :on-surface :text ..} for a :stay-level run whose head cell has full sky light (head-light 15, as loaded): it stands in
  the open and its tunnel would breach open at the first cell. Less light is a cave mouth or an overhang (spilled light): nil, as without light data."
  [args head-light]
  (when (and (mw/stay-level? args) (= 15 head-light))
    {:reason :on-surface
     :text "mine without a descending way declined: the body stands in the open; cut a stair down first (access.stair {:dir :down :y 16}) or allow a descending way with a :level"}))

(defn on-surface
  "surface-cause of the body's head cell, before the run has begun (a resumed run stands where it dug)."
  [c]
  (let [p (:primitives c)
        {:keys [x y z]} (sh-sense/feet p)]
    (when-not (or (:goal (ctx/mem c)) (:start (ctx/mem c)) (seq (:slots c)))
      (surface-cause (:args c) (sh-sense/sky-light-at p {:x x :y (inc y) :z z})))))

(def can-start?
  (cond/and target-given
            (area/unless-outside (checks/refusing :underground "not in the open" on-surface))
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
     :yields {:items {(first (items a)) [0 count count]}}
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
  (ctx/update-mem! c #(cond-> (-> % (dissoc :digging) (assoc :failures 0 :collecting true ::look/looked :dug :dug-at (access/cell pos)))
                        (mw/stay-level? (:args c)) (update :holes (fnil conj []) (access/cell pos)))))

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
                                   (= (access/cell pos) (when-let [{:keys [x y z]} (:start %)] [x (dec y) z])) (assoc :start-floor-dug true)))
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
  "The go-near! options of the walk to a dig: other ways only beyond reach, no dive under water, walks inside own cuts ignore the dark (the :checks forms given, when any), a level bound with :stay-level."
  [args in-reach? checks & [tolls]]
  (cond-> {:dive false}
    (seq tolls) (assoc :tolls tolls)
    in-reach? (assoc :ways #{:walk :clear-seal})
    (seq checks) (assoc :checks checks)
    (mw/stay-level? args) (assoc :y-margin (settings/get settings ::stay-margin))))

(def lava-side-toll "The factor on a stand beside seen lava as the goal of a dig walk: the dig takes another when one reaches." 50)

(defn lava-side-tolls
  "The tolls of the stands within range 1 of the cell pos that lie beside seen lava (lava/beside-lava?)."
  [c pos]
  (let [p (:primitives c) {:keys [x y z]} (cell-of pos)]
    (vec (for [[dx dy dz] [[0 0 0] [1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]
               :let [cell [(+ x dx) (+ y dy) (+ z dz)]]
               :when (lava/beside-lava? p cell)]
           (zipmap [:x :y :z :factor] (conj cell lava-side-toll))))))

(defn ^:async walk-to-dig!
  "Walk toward pos until the body is within reach of it, and close enough to see its drop (range 1) when the line to it
  is blocked from here; a body within reach that cannot get closer digs from there. :there, :partial or :blocked
  (jobs.lib.near/go-near!)."
  [c pos]
  (let [in-reach? (u/within? (u/self-pos c) (cell-of pos) (reach))
        close? (not (or (mw/stay-level? (:args c)) (drop-in-line? c pos)))
        walked (await (near/go-near! c pos (if close? 1 (reach))
                                     (dig-walk-opts (:args c) in-reach? (when (underground? c) (ready/unforced c {:safe? '(not (threatened))}))
                                                    (when close? (lava-side-tolls c pos)))))]
    (if (and (not= :there walked) (u/within? (u/self-pos c) (cell-of pos) (reach)))
      :there
      walked)))


(defn ^:async dig-round-1! [c]
  (let [{:keys [goal failures dry]} (ctx/mem c)
        looked (::look/looked (ctx/mem c))
        {:keys [wet]} (:args c)
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
      (and (some? pos) (empty? refused) (dig-here-due? c pos)) (await (descend-round! c))
      (and (some? pos) (way-floor c [pos])) (do (skip! c pos) :again)
      (and (some? pos) (last-exit? c pos)) (do (ctx/emit! c :mine.spared-exit :info {:pos (access/cell pos) :text (str "mine leaves " (str/join "," (access/cell pos)) ": it is the body's last way out")})
                                               (skip! c pos) :again)
      (some? pos) (let [walked (await (walk-to-dig! c pos))
                        cell (access/cell pos)
                        cut (when (and (= :blocked walked) (not (mw/stay-level? (:args c))) (cut-after? (near/last-walk c))
                                       (not (#{:off-plan :needs-health} (result/why (near/last-walk c))))
                                       (not (contains? (:target-cut (ctx/mem c)) cell)))
                              (let [r (await (cut-to-drop! c cell (target-cut-max)))]
                                (ctx/emit! c :mine.cut-toward :info {:pos cell :result r :text (str "mine cuts toward " (str/join "," cell) ": " (or (some-> r name) "no way"))})
                                r))]
                    (cond
                      (= :cut cut) (do (ctx/update-mem! c update :target-cut (fnil conj #{}) cell) :again)
                      (= :partial cut) :continue
                      (and (= :blocked walked) (result/unreachable-walk? (near/last-walk c))) (do (skip-unreachable! c pos targets) :again)
                      (= :blocked walked) (let [w (near/last-walk c)]
                                            (ctx/emit! c :mine.walk-failed :info {:pos (access/cell pos) :why (result/why w) :cause (contract/cause-key w)
                                                                                  :text (str "mine's walk to " (str/join "," (access/cell pos)) " gave up (" (result/why w) "); it skips the target and counts a failure")})
                                            (skip-failed! c pos) :again)
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
                  (mw/tunnel? (:args c)) (await (tunnel/tunnel-round! c))
                  (seq refused) (stop! c :refused)
                  :else (stop! c (if (and wet? (not wet)) :wet :none)))))))

(defn ^:async dig-round!
  "A side cut or tunnel step under way goes on first, then a hole dug in the wall is shut, then
  one piece of the digging."
  [c]
  (cond
    (:side-cut (ctx/mem c)) (await (wall/side-round! c (:side-cut (ctx/mem c))))
    (:branch (ctx/mem c)) (await (branch/branch-round! c))
    (:stepping (ctx/mem c)) (await (tunnel/tunnel-round! c))
    (seq (:holes (ctx/mem c))) (await (wall/shut-holes! c))
    :else (do (ore-pick/mark! c {:leave (kinds (:args c))})
              (await (dig-round-1! c)))))

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
        {:keys [heading direction]} (:args c)
        w (when (nil? (:phase m)) (await (area/walk-in! c)))]
    (cond
      (= :there w) :again
      (= :continue w) :continue
      w w
      (stop-cause c) (let [cause (stop-cause c)]
                       (stop! c (:reason cause) (select-keys cause [:tool :why :text])))

      (nil? (:phase m))
      (let [faced (facing (:primitives c))
            _ (await (look/survey! c))
            now (carried c)
            start (cell-of (u/self-pos c))
            heading (or (toward start heading) (heading-name direction))]
        (ctx/update-mem! c assoc
                         :goal (:count (:args c)) :had now :start start
                         :failures 0 :dry 0 :last-carried now
                         :skipped (mapv (fn [[x y z]] {:x x :y y :z z}) (:skip (:args c)))
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
