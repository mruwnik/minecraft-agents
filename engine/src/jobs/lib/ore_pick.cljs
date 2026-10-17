(ns jobs.lib.ore-pick
  "Ore a player passes digs itself: an exposed ore block within reach of the stand a stair, a tunnel or a mine is working
  from, with no walk to it. pick! takes the nearest such block (a seen air face, none of its faces fluid, a carried tool
  that harvests it, room in the bag, the rules' dig verdict ok) with a jobs.blocks.dig child, then steps back to the
  stand it dug from. Picked blocks are listed in the job's memory (picked) for its result. The arg :pick-up-ores is true
  (the ::ores setting), false (none) or a coll of ore block names."
  (:require [jobs.lib.access.rules :as rules]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
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
  "The block names to pick up under the arg: the ores and their deepslate kinds, empty for none."
  [c]
  (let [arg (:pick-up-ores (:args c) true)
        base (cond (false? arg) [] (true? arg) (settings/get settings ::ores) :else arg)]
    (into #{} (mapcat (fn [n] [n (str "deepslate_" n)])) base)))

(defn picked [c] (:picked (:ore-pick (ctx/mem c))))

(defn face-names [p [x y z]]
  (map (fn [[dx dy dz]] (u/seen-name p {:x (+ x dx) :y (+ y dy) :z (+ z dz)})) faces))

(defn exposed?
  "Whether a seen air face touches the cell and no seen face is fluid."
  [p cell]
  (let [ns (face-names p cell)]
    (and (some blocks/air ns) (not-any? blocks/fluids ns))))

(defn count-of [p item] (transduce (comp (filter #(= item (:name %))) (map :count)) + 0 (u/inventory p)))

(defn candidates
  "Cells of ore blocks to pick up from here, nearest first; leave: block names left to the job, except: cells."
  [c {:keys [except leave]}]
  (let [p (:primitives c)
        wanted (apply disj (names c) leave)
        skipped (set (:skipped (:ore-pick (ctx/mem c))))
        here (u/self-pos c)
        in (blocks/rules-in c)]
    (when (seq wanted)
      (->> (look/seen-blocks p {:names (vec wanted) :radius (settings/get settings ::radius) :max 32 :live? true})
           (filter (fn [{:keys [pos name]}]
                     (let [cell (blocks/cell pos)]
                       (and (not (skipped cell)) (not (contains? except cell))
                            (blocks/in-reach? c pos true)
                            (exposed? p cell)
                            (tools/can-harvest? p name)
                            (blocks/room-for? p (blocks/item-name {:block name}))
                            (= :ok (access/judge (access/may-dig? in pos) #{}))))))
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
  [c {:keys [target block before]}]
  (let [p (:primitives c)
        e (await (ctx/call c :ore 'jobs.blocks.dig {:pos target :fetch false :walk false :on-lava :leave :ignore-zones? (boolean (:ignore-zones? (:args c))) :for-plan (:for-plan (:args c))}))]
    (when-not (= :continue e)
      (let [reason (if (= :done (:kind e)) (:reason (:data e)) (contract/cause-key e))
            dug? (= :dug reason)
            item (blocks/item-name {:block block})]
        (book! c (fn [m] (-> (dissoc m :target)
                             (cond-> dug? (update :picked (fnil conj []) {:cell target :block block :got (- (count-of p item) before)})
                                     (not dug?) (update :skipped (fnil conj []) target)))))
        (ctx/emit! c (if dug? :ore-pick.dug :ore-pick.skipped) :info
                   {:cell target :block block :reason reason
                    :text (str (if dug? "picked up " "left ") block " at " (pr-str target) " on the way")})))
    (when (= :continue e) :continue)))

(defn ^:async pick!
  "Dig the exposed ores within reach one after another (at most ::per-call), each followed by the walk back to the stand.
  :continue while a child waits on the world, else nil. opts :except (cells never dug: the job's own target) and :leave
  (block names the job digs for itself)."
  ([c] (pick! c {}))
  ([c opts]
   (loop [n 0]
     (let [{:keys [target home] :as m} (:ore-pick (ctx/mem c))]
       (cond
         target (or (await (dig! c m))
                    (await (go-home! c home))
                    (do (book! c dissoc :home) (recur n)))
         home (or (await (go-home! c home)) (do (book! c dissoc :home) (recur n)))
         (>= n (settings/get settings ::per-call)) nil
         :else
         (when-let [{:keys [pos name]} (first (candidates c opts))]
           (let [cell (blocks/cell pos)]
             (book! c assoc :target cell :block name :home (blocks/feet-cell c)
                    :before (count-of (:primitives c) (blocks/item-name {:block name})))
             (recur (inc n)))))))))
