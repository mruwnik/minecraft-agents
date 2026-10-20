(ns jobs.lib.ore-pick
  "Ore a player passes digs itself: an exposed ore block within reach of the stand a stair, a tunnel or a mine is working
  from, with no walk to it. A mining job (dig-for, a root stair or tunnel) marks its memory (mark!) with what it digs itself;
  the :ore-seen trigger holds while a mark is live and candidates finds a block, and fires jobs.gather.pick-ores, whose
  pick! takes the nearest such block (a seen air face, none of its faces fluid, a carried tool that harvests it, room in
  the bag, the rules' dig verdict ok) with a jobs.blocks.dig child, then steps back to the stand it dug from. Picked blocks
  are listed in that job's memory (picked) for its result."
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.access.rules :as rules]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.waits :as waits]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::ores {:default ["coal_ore" "iron_ore" "copper_ore" "gold_ore" "redstone_ore" "lapis_ore" "diamond_ore" "emerald_ore"]
           :doc "The ore blocks a stair, tunnel or mine digs when it passes them exposed within reach (the deepslate kinds too)."
           :spec (a/coll-of string?)}
   ::radius {:default 5 :doc "Blocks around the body searched for an ore to pick up on the way." :spec (a/int-in 1 8)}
   ::per-call {:default 6 :doc "Ore blocks one call picks up before the job goes on." :spec (a/int-in 1 nil)}})

(def faces rules/neighbour-deltas)

(defn names
  "The block names to pick up: the ::ores and their deepslate kinds."
  []
  (into #{} (mapcat (fn [n] [n (str "deepslate_" n)])) (settings/get settings ::ores)))

(defn mark!
  "Say in this job's memory that the :ore-seen trigger may fire for it. opts :leave (block names the job digs itself) and
  :except (cells it never lets go: its own target)."
  [c {:keys [leave except]}]
  (let [m {:leave (vec leave) :except (vec except) :ignore-zones? (boolean (:ignore-zones? (:args c))) :for-plan (:for-plan (:args c))}]
    (when (not= m (:ore-mark (ctx/mem c)))
      (ctx/update-mem! c assoc :ore-mark m))))

(defn marks
  "The marks (mark!) of every listed job and its children."
  [view]
  (->> (keys (:entries (:data view)))
       (filter mem/job-kind?)
       (mapcat #(tree-seq map? (fn [m] (vals (:children m))) (:data (mem/latest view %))))
       (keep :ore-mark)))

(defn merged
  "One opts map for candidates and pick! over several marks: what any job digs itself or keeps stays; zones are
  ignored only when every job ignores them."
  [ms]
  {:leave (into #{} (mapcat :leave) ms)
   :except (into #{} (map vec) (mapcat :except ms))
   :ignore-zones? (boolean (and (seq ms) (every? :ignore-zones? ms)))
   :for-plan (some :for-plan ms)})

(def left-kind :ore-left)

(defn left-cells [view] (into #{} (mapcat (comp :cells :data)) (mem/entries view left-kind)))

(defn left!
  "Remember cells the :ore-seen trigger saw and the job could not take, so the trigger does not fire for them again."
  [c cells]
  (when (seq cells)
    (ctx/remember! c left-kind {:cells (vec cells)} {:cap 20 :ttl (* 5 60 1000)})))

(defn picked [c] (:picked (:ore-pick (ctx/mem c))))

(defn face-names [p [x y z]]
  (map (fn [[dx dy dz]] (u/seen-name p {:x (+ x dx) :y (+ y dy) :z (+ z dz)})) faces))

(defn exposed?
  "Whether a seen air face touches the cell and no seen face is fluid."
  [p cell]
  (let [ns (face-names p cell)]
    (and (some bs/air ns) (not-any? bs/fluids ns))))

(defn solid-under?
  "Whether the cell under this one is seen and a floor: digging an ore there leaves a dent the body climbs out of, not a drop."
  [p [x y z]]
  (let [n (u/seen-name p {:x x :y (dec y) :z z})]
    (boolean (and n (not (bs/passable? n)) (not (bs/fluids n)) (not (bs/bad-floor n))))))

(defn candidates
  "Seen ore blocks to pick up from here, nearest first; leave: block names left to the job, except: cells, skipped:
  cells passed over. With in (blocks/rules-in) the rules' dig verdict must be ok; the trigger leaves it out, so a block
  only the job may not dig is found and remembered (left!) by the job."
  [c {:keys [except leave skipped in]}]
  (let [p (:primitives c)
        wanted (apply disj (names) leave)
        here (u/self-pos c)
        floor-y (second (blocks/feet-cell c))]
    (when (seq wanted)
      (->> (look/seen-blocks p {:names (vec wanted) :radius (settings/get settings ::radius) :max 32 :live? true})
           (filter (fn [{:keys [pos name]}]
                     (let [cell (blocks/cell pos)]
                       (and (not (contains? skipped cell)) (not (contains? except cell))
                            (or (>= (second cell) floor-y) (solid-under? p cell))
                            (blocks/in-reach? c pos true)
                            (exposed? p cell)
                            (tools/can-harvest? p name)
                            (blocks/room-for? p (blocks/item-name {:block name}))
                            (or (nil? in) (= :ok (access/judge (access/may-dig? in pos) #{})))))))
           (sort-by #(u/eye-dist here (:pos %)))))))

(defn book! [c f & args] (apply ctx/update-mem! c update :ore-pick f args))

(defn ^:async go-home!
  "Back to the stand a pick began from: :continue while the walk waits, else nil."
  [c home]
  (when (not= home (blocks/feet-cell c))
    (when (= :continue (await (child/call-walk* c :ore-walk {:pos home :range 0 :ways #{:walk :clear-seal}})))
      :continue)))

(defn ^:async dig!
  "The dig child over the booked :target: :continue while it waits, else nil with the pick booked or the cell skipped."
  [c {:keys [target block before home]}]
  (let [p (:primitives c)
        e (waits/waiting-out c (await (ctx/call c :ore 'jobs.blocks.dig {:pos target :ways #{:carried} :walk false :on-lava :leave :ignore-zones? (boolean (:ignore-zones? (:args c))) :for-plan (:for-plan (:args c))})))]
    (when-not (= :continue e)
      (let [reason (if (= :done (:kind e)) (:reason (:data e)) (contract/cause-key e))
            dug? (= :dug reason)
            item (blocks/item-name {:block block})]
        (book! c (fn [m] (-> (dissoc m :target)
                             (cond-> dug? (update :picked (fnil conj []) {:cell target :block block :got (- (u/carried-count p item) before)})
                                     (not dug?) (update :skipped (fnil conj []) target)))))
        (ctx/emit! c (if dug? :ore-pick.dug :ore-pick.skipped) :info
                   {:cell target :from home :block block :reason reason
                    :text (str (if dug? "picked up " "left ") block " at " (pr-str target) " on the way")})))
    (when (= :continue e) :continue)))

(defn skipped-of [c opts]
  (into (set (:skipped (:ore-pick (ctx/mem c)))) (:left opts)))

(defn ^:async pick!
  "Dig the exposed ores within reach one after another (at most ::per-call), each followed by the walk back to the stand.
  :continue while a child waits on the world, else nil. An ore below the feet is left unless the cell under it is seen solid: digging one over a cave or void
  makes a hole the body can fall into and not climb out of. opts :except (cells never dug: the job's own target), :leave
  (block names the job digs for itself) and :left (cells the trigger must not see again)."
  ([c] (pick! c {}))
  ([c opts]
   (loop []
     (let [{:keys [target home n] :as m} (:ore-pick (ctx/mem c))]
       (cond
         target (or (await (dig! c m))
                    (await (go-home! c home))
                    (do (book! c dissoc :home) (recur)))
         home (or (await (go-home! c home)) (do (book! c dissoc :home) (recur)))
         (>= (or n 0) (settings/get settings ::per-call)) (do (book! c dissoc :n) nil)
         :else
         (if-let [{:keys [pos name]} (first (candidates c (assoc opts :skipped (skipped-of c opts) :in (blocks/rules-in c))))]
           (let [cell (blocks/cell pos)]
             (book! c assoc :target cell :block name :home (blocks/feet-cell c)
                    :before (u/carried-count (:primitives c) (blocks/item-name {:block name})))
             (book! c update :n (fnil inc 0))
             (recur))
           (do (book! c dissoc :n) nil)))))))
