(ns jobs.items.craft
  (:require [clojure.string :as str]
            [jobs.lib.least :as least]
            [engine.args :as a]
            [jobs.items.shortfall :as craft]
            [jobs.items.recipes :as recipes]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.child :as child]
            [jobs.lib.crafting-table :as crafting-table]
            [jobs.lib.look :as look]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.escape :as escape]
            [jobs.lib.tools :as tools]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.world :as known]
            [jobs.lib.need :as need]
            [jobs.lib.checks :as checks]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.spent :as spent]
            [jobs.lib.storage :as storage]))

(def doc
  "Craft :count more of :item. 2x2 recipes work anywhere. Bigger ones need a crafting table: :table, else the
  nearest within :radius, else the nearest seen within :far (up to 64), walking there when out of reach (a go-to child;
  a carried table is put down instead, with no :table, also with :fetch false; a seen table the walk cannot reach is
  left, and the carried or a fetched table is put down).
  The count carried at the start is kept in memory and the target is that plus :count, so a cut or a partial
  batch loses nothing.
  Needs: an item name (impossible :bad-args).
  Ends :done with {:made n :item :text}, the number of items gained (plus :spent and :spent-at when it put a table down).
  Stopping short it ends with :made, :item and a :text as well: condition-failed with the cause :need (data :short {name n}, an ingredient
  ran out, and :alternatives), :no-table, :not-a-table or :full (the bag is full and one
  jobs.storage.make-room child freed nothing), or nothing-possible with :unreachable (a walk to a table failed) or :cannot (data :detail, the reason the game
  gives) or :craft-failed (three failed crafts in a row, data :detail the status)
  One call is the whole craft: it walks (a go-to child) and crafts again until the count is carried or it stops; it
  yields only while a child waits on the world.
  An ingredient that runs out is got with a jobs.items.obtain child (jobs.lib.need) and the craft goes on (a
  further fetch for each batch while the last one led to more made), unless :fetch is false or the fetch failed: then
  it ends :need with a :text. With no table in reach a carried one is put down beside the body, else one is fetched
  (an obtain child for a crafting_table) and put down, the same way; :fetch false or a failed fetch ends :no-table.
  A table this call put down is dug up again by the cleanup, however the call ends (stopped short or cut too), unless :keep-table, it stands in an area
  of the body's own or a zone refuses the dig. No table is put in a cell of a stair or tunnel the body cut (memory
  :stair-way, :tunnel-way).")

(a/defargs args
  {:item {:doc "item name to craft" :spec a/item? :default nil}
   :count {:doc "how many more to end up with" :spec (a/int-in 0 nil) :default 1}
   :table {:doc "crafting table position; the nearest seen within :radius when nil and the recipe needs one" :spec ::a/pos :default nil}
   :radius {:doc "how far to look for a crafting table" :spec (a/num-in 0 nil) :default 32}
   :far {:doc "how far a seen crafting table is walked to when none is within :radius (the seen memory reaches 64)" :spec (a/num-in 0 64) :default 64}
   :fetch {:doc "get an ingredient that runs out (jobs.lib.need): true, a set of kinds or a map of limits; false stops :short" :spec need/option? :default true}
   :keep-table {:doc "leave a table this call put down standing; false: it is dug up again when the call ends" :spec boolean? :default false}
   :avoid {:doc "cells [x y z] a table is never put in or dug for: the dig line of a job that crafts as its child" :spec (a/coll-of a/position?) :default nil}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a craft may take: the walk, the batches and the table." :spec (a/num-in 0 nil)}
   ::batch-s {:default 0.5 :doc "Mean seconds one batch of a craft takes, for the quote." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))
(defn batch-s [] (settings/get settings ::batch-s))

(def target [:item])

(def defaults {:least :all :spend {:job-time ::job-time-s}})

(defn bad-args
  "The cause when no item name is given."
  [c]
  (when-not (string? (:item (:args c)))
    {:reason :bad-args :why "no item name"}))

(defn can-start? [c] (checks/cause-status :never (bad-args c)))

(defn pickup-spot
  "The table cell this call put down and is still to dig up, else nil: not with :keep-table, once dug, gone, in an area of the
  body's own or refused by a zone."
  [c]
  (let [spot (:table-placed (ctx/mem c))]
    (when (and spot (not (:keep-table (:args c))) (not (:picked-up (ctx/mem c)))
               (= "crafting_table" (u/seen-name (:primitives c) spot))
               (not (access/in-own-area? c spot))
               (:ok (access/may? c :dig spot)))
      spot)))

(defn done?
  "Done once :count more of :item are carried than at the start; else progress."
  [c]
  (let [{:keys [item count]} (:args c)
        have (storage/carried (u/inventory (:primitives c)) item)
        start (:start (ctx/mem c) have)
        made (max 0 (- have start))
        data [:units :items :done made :left (max 0 (- count made)) :result {:made made}]]
    (if (>= made count)
      (apply contract/ok :crafted (str "crafted " made " " item) data)
      (apply contract/later :crafting (str "craft: " made " of " count " " item) data))))

(defn quote-of
  "P: the cheapest recipe for :count more of :item from what is carried. It uses its ingredients (net of the item
  itself, a least of 0 since another recipe may serve) and yields at least :count."
  [c]
  (let [{:keys [item count]} (:args c)
        p (:primitives c)
        rs (get (recipes/recipes-for (game/version-of p)) item)
        st {:have (storage/carried-totals (u/inventory p))}
        r (first (sort-by #(recipes/short-count st % (recipes/ceil-div count (:count %))) rs))
        batches (if r (recipes/ceil-div count (:count r)) 0)
        mean (+ 1 (* batches (batch-s)))]
    (cond-> {:uses {:job-time [(* batches (batch-s)) mean (max mean (time-s))]}}
      r (assoc-in [:yields :items item] [count (* batches (:count r)) (* batches (:count r))])
      (seq (dissoc (:needs r) item))
      (assoc-in [:uses :items] (into {} (map (fn [[k v]] [k [0 (* v batches) (* v batches)]])) (dissoc (:needs r) item))))))

(defn unreachable-tables
  "The table cells a walk gave up on during this call."
  [c]
  (:unreachable-tables (ctx/mem c) #{}))

(defn no-table-text
  "Why no table is used: none within radius, naming the nearest seen one (up to 64 blocks) when there is one, the ones a
  walk could not reach, and why a carried table was not put down."
  [c radius]
  (let [skip (unreachable-tables c)
        far (crafting-table/nearest (:primitives c) 64 skip)
        {:keys [x y z]} far
        from (u/self-pos c)
        why (:table-why (ctx/mem c))]
    (str "no crafting table within " radius
         (when far (str "; the nearest known is at " "[" x " " y " " z "]" ", " (js/Math.round (u/dist from far)) " blocks away"))
         (when (seq skip) (str "; the table at " (str/join ", " (map #(str "[" (str/join " " %) "]") (sort skip))) " cannot be reached"))
         (when why (str "; " why)))))

(def condition-reasons "Stops that read as a failed condition, the others as nothing possible." #{:need :no-table :not-a-table :full})

(defn finish!
  "The ending: done with the made count, or
  the stop extra names by its :reason (a condition-failed or nothing-possible ending, extra's other fields and the made
  count as data); a stop with at least the call's :least items made is done (:all wants the whole :count)."
  [c made extra]
  (let [placed (spent/log c)
        reason (:reason extra)
        data (cond-> (merge {:made made :item (:item (:args c))} (dissoc extra :reason))
               (seq placed) (merge (spent/summary placed)
                                   {:text (str/join "; " (keep identity [(spent/text placed "crafting table") (:text extra)]))}))
        total (:count (:args c))]
    (cond
      (nil? reason) (ctx/end c :done nil data)
      (condition-reasons reason) (least/end! c :condition-failed (cond-> {:reason reason} (:short extra) (assoc :short (:short extra))) data made total)
      :else (least/end! c :nothing-possible reason data made total))))

(defn give-up!
  "u/fail!: :again until the third failure in a row, then finish with the made count and the reason."
  ([c made reason] (give-up! c made reason nil))
  ([c made reason extra]
   (if (u/count-fail! c)
     (finish! c made (assoc extra :reason reason :text (str "craft gave up: " (name reason) (when-let [d (:detail extra)] (str " " d)))))
     :again)))

(def table-offsets [[1 0] [-1 0] [0 1] [0 -1] [1 1] [-1 1] [1 -1] [-1 -1]])

(defn solid? [n] (and n (not (b/air n)) (not (b/fluids n)) (not (b/clearable n))))

(defn home-cell?
  "Whether a table in pos would spoil a room: on a bed, or in the cell beside a door (the way in and out)."
  [p {:keys [x y z] :as pos}]
  (let [name-at #(or (u/seen-name p %) "")
        ends? (fn [n suffix] (str/ends-with? n suffix))]
    (or (ends? (name-at (update pos :y dec)) "_bed")
        (some (fn [[dx dz]] (some #(ends? (name-at {:x (+ x dx) :y (+ y %) :z (+ z dz)}) "_door") [0 1]))
              [[1 0] [-1 0] [0 1] [0 -1]]))))

(defn spot-cells
  "The candidate table cells beside the body, outside its box (the server refuses a place the body touches), not in :avoid, not in a plan's cells, not over a bed or beside a door (home-cell?) and not in a stair or tunnel this body cut (its way out): [pos floor-pos] for each offset at its level, then (a 1-wide stair has
  no free cell at its level) each one level up. dig-spot-cells keeps to the level ones."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)
        cols (access/body-columns x z)
        avoid (into (ledger/own-cells c) (map (comp (juxt :x :y :z) a/cell)) (:avoid (:args c)))
        plan-cells (known/footprints c)
        [x y z] (mapv #(js/Math.floor %) [x y z])]
    (for [dy [0 1]
          [dx dz] table-offsets
          :let [pos {:x (+ x dx) :y (+ y dy) :z (+ z dz)}]
          :when (and (not (cols [(:x pos) (:z pos)])) (not (avoid [(:x pos) (:y pos) (:z pos)]))
                     (not (contains? plan-cells [(:x pos) (:y pos) (:z pos)]))
                     (not (home-cell? (:primitives c) pos)))]
      [pos (update pos :y dec)])))

(defn table-spot
  "A free cell beside the body to put a table in: air at the body's level with a solid block under it, or nil. Where
  fewer than three of the four cells beside the body are open (a tunnel, a pocket), a cell that leads on to more open
  air is a passage and is not taken: it would shut the way along the corridor."
  [c]
  (let [p (:primitives c)
        {:keys [x y z]} (u/self-pos c)
        [x y z] (mapv #(js/Math.floor %) [x y z])
        sides [[1 0] [-1 0] [0 1] [0 -1]]
        open? (fn [cx cz] (b/air (u/seen-name p {:x cx :y y :z cz})))
        cramped? (< (count (filter (fn [[dx dz]] (open? (+ x dx) (+ z dz))) sides)) 3)
        passage? (fn [{px :x pz :z}]
                   (some (fn [[dx dz]] (and (not= [(+ px dx) (+ pz dz)] [x z]) (open? (+ px dx) (+ pz dz)))) sides))]
    (some (fn [[pos floor]]
            (when (and (b/air (u/seen-name p pos)) (solid? (u/seen-name p floor))
                       (not (and cramped? (= y (:y pos)) (= 1 (+ (js/Math.abs (- x (:x pos))) (js/Math.abs (- z (:z pos)))))
                                 (passage? pos))))
              pos))
          (spot-cells c))))

(defn ^:async find-spot!
  "table-spot, and when the cells beside the body are not all seen: look at each unseen floor cell once (per standing
  cell), then read again."
  [c]
  (or (table-spot c)
      (let [here (look/cell-of (u/self-pos c))]
        (when-not (= here (:looked-spot (ctx/mem c)))
          (ctx/update-mem! c assoc :looked-spot here)
          (doseq [[pos floor] (spot-cells c)
                  :when (or (nil? (u/seen-name (:primitives c) pos)) (nil? (u/seen-name (:primitives c) floor)))]
            (await (ctx/act c :look (clj->js {:pos {:x (+ (:x floor) 0.5) :y (+ (:y floor) 0.5) :z (+ (:z floor) 0.5)}})))
            (look/see! c))
          (table-spot c)))))

(defn dig-spot-cells
  "With no free cell beside the body (a shut-in pocket): the cells beside it at its level that would be a table spot
  once dug, quickest to break first: seen natural terrain (jobs.lib.escape/natural?, never a build) on a solid floor
  or one not seen yet (rock hides it), not tried yet (memory :spot-dug)."
  [c]
  (let [p (:primitives c)
        tried (:spot-dug (ctx/mem c) #{})]
    (->> (spot-cells c)
         (filter (fn [[pos _]] (= (:y pos) (js/Math.floor (:y (u/self-pos c))))))
         (keep (fn [[pos floor]]
                 (let [n (u/seen-name p pos)
                       f (u/seen-name p floor)]
                   (when (and (escape/natural? n) (solid? n) (or (nil? f) (solid? f)) (not (tried pos)))
                     pos))))
         (sort-by #(tools/clear-ms p (u/seen-name p %))))))

(defn ^:async dig-spot!
  "Make a table spot where there is none: dig the first dig-spot-cells cell (a jobs.blocks.dig child, bare-handed when
  no carried tool harvests it; the drop is not kept; a cell beside water or lava is passed over, :on-fluid :fail), then
  look at its floor. A cell that could not be dug goes to the next. The spot, :continue while the child waits, or nil
  when no cell can be dug."
  [c]
  (loop []
    (when-let [cell (or (:spot-digging (ctx/mem c)) (first (dig-spot-cells c)))]
      (ctx/update-mem! c assoc :spot-digging cell)
      (let [r (await (ctx/call c :spot-dig 'jobs.blocks.dig {:pos cell :collect false :need-drop false :fetch false :on-fluid :fail}))]
        (if (= :continue r)
          :continue
          (do (ctx/update-mem! c #(-> % (dissoc :spot-digging) (update :spot-dug (fnil conj #{}) cell)))
              (if (and (= :done (:kind r)) (:dug (:data r)))
                (do (await (ctx/act c :look (clj->js {:pos {:x (+ (:x cell) 0.5) :y (- (:y cell) 0.5) :z (+ (:z cell) 0.5)}})))
                    (look/see! c)
                    (when (and (b/air (u/seen-name (:primitives c) cell)) (solid? (u/seen-name (:primitives c) (update cell :y dec))))
                      cell))
                (recur))))))))

(defn ^:async spot!
  "A cell beside the body to put a table in: a free one (find-spot!), else one dug (dig-spot!). :continue while the dig
  waits, nil when there is none."
  [c]
  (or (await (find-spot! c)) (await (dig-spot! c))))

(defn carried-table? [c]
  (pos? (storage/carried (u/inventory (:primitives c)) "crafting_table")))

(defn table-problem
  "The crafting table to get while one is wanted and none is carried, else nil."
  [c]
  (when (and (:want-table (ctx/mem c)) (not (carried-table? c)))
    {:item "crafting_table" :count 1}))

(defn ^:async fetch!
  "Get what (problem c) names with jobs.items.obtain children (jobs.lib.need) until nothing is due any more: nil (the item
  arrived, the fetch failed or :fetch is off; the craft judges where it stands), :continue while a child waits."
  [c problem]
  (loop []
    (when-let [{:keys [item count]} (and (:fetch (:args c)) (problem c))]
      (let [e (await (need/item! c item count {:slot (keyword (str "need-" item))}))]
        (cond
          (= :continue e) :continue
          (not= :done (:kind e)) nil
          (ctx/alive? c) (do (await (pace/pace!)) (recur))
          :else :continue)))))

(defn table-why! [c why] (ctx/update-mem! c assoc :table-why why) nil)

(defn ^:async place-table!
  "Put the carried table down in a free cell beside the body (a jobs.blocks.place child; the floor is looked at first
  when no spot is seen, and with none a cell beside is dug: spot!). :again once it stands (it is the table to craft
  at), :continue while a child waits, nil when it could not be put down."
  [c]
  (let [spot (or (:table-spot (ctx/mem c)) (await (spot! c)))]
    (cond
      (= :continue spot) :continue
      (not spot) (table-why! c "the carried crafting_table could not be put down: no free cell beside the body")
      :else
      (do
        (ctx/update-mem! c assoc :table-spot spot)
        (let [r (await (ctx/call c :place 'jobs.blocks.place {:item "crafting_table" :pos spot :fetch false}))]
          (if (= :continue r)
            :continue
            (do
              (ctx/update-mem! c dissoc :table-spot)
              (if (and (= :done (:kind r)) (:placed (:data r)))
                (do (ctx/update-mem! c #(-> % (assoc :table spot :table-placed spot) (dissoc :want-table)))
                    (spent/note! c "crafting_table" spot)
                    :again)
                (table-why! c (str "the carried crafting_table could not be put down: "
                                   (if (= :done (:kind r)) "placing it failed" (name (contract/cause-key r)))))))))))))

(declare craft-act! settle!)

(defn ^:async room!
  "A full bag: once per craft, a jobs.storage.make-room child (child :room). :continue while it waits, :again when it
  ran, nil when it was tried before."
  [c]
  (when-not (:room-tried (ctx/mem c))
    (let [r (await (ctx/call c :room 'jobs.storage.make-room {}))]
      (if (= :continue r)
        :continue
        (do (ctx/update-mem! c assoc :room-tried true)
            :again)))))

(defn ^:async table-fetch!
  "With no table in reach: put a carried table down (also with :fetch off), else fetch one (:fetch) and put it down. Once per craft.
  :again / :continue to go on, nil when there is nothing to do (give up). Putting a carried table down and the craft call at
  it run as one urgent section (ctx/atomic :craft), so a reflex does not leave the table unused; the
  follow-up on the craft's status (settle!) is outside it."
  [c]
  (let [o (:fetch (:args c))]
    (when-not (:table-tried (ctx/mem c))
      (when (and o (not (carried-table? c))) (ctx/update-mem! c assoc :want-table true))
      (let [f (when (:want-table (ctx/mem c)) (await (fetch! c table-problem)))
            r (when (and (nil? f) (carried-table? c))
                (await (ctx/atomic c :urgent :craft
                                   (fn ^:async place-and-craft []
                                     (let [placed (await (place-table! c))]
                                       (if (= :again placed) (await (craft-act! c)) placed))))))
            r (if (map? r) (await (settle! c r)) r)]
        (when-not (or f r) (ctx/update-mem! c #(-> % (dissoc :want-table) (assoc :table-tried true))))
        (or f r)))))

(defn ^:async no-table!
  "No crafting table is within the radius or one can be reached: fetch one (table-fetch!), else warn and finish, with
  reason \"unreachable\" when a walk to a table failed. :again, :continue or :done."
  [c made]
  (or (await (table-fetch! c))
      (finish! c made {:reason (if (seq (unreachable-tables c)) :unreachable :no-table)
                       :text (no-table-text c (:radius (:args c)))})))

(defn ^:async reach-table!
  "The craft was unreachable: walk to the remembered or the nearest table with a go-to child. A table the walk cannot
  reach (not one handed in as :table) is dropped for this call and a carried or fetched table is put down instead.
  :again to craft again, :continue while go-to waits, :done when there is none or the walk was given up."
  [c made]
  (let [{:keys [table radius far]} (merge (:args c) (ctx/mem c))
        p (:primitives c)
        handed? (some? table)
        arg? (some? (:table (:args c)))
        skip (unreachable-tables c)
        table (or table (crafting-table/nearest p radius skip) (crafting-table/nearest p far skip))]
    (cond
      (and (nil? table) (not (look/surveyed? c)))
      (do (await (look/survey! c)) :again) ; a table behind the body is not seen until it looks

      (nil? table)
      (await (no-table! c made))

      :else
      (do (ctx/update-mem! c assoc :table table)
          (if (u/within? (u/self-pos c) table 3)
            (if handed? (give-up! c made :unreachable) :again)
            (let [r (await (child/call-walk* c :walk {:pos table :range 3 :ways #{:walk :clear-seal} :warn false :retry false :zone-tolls true}))]
              (cond
                (= :continue r) :continue
                (:arrived (:data r)) :again
                arg? (give-up! c made :unreachable)
                :else (do (ctx/update-mem! c #(-> % (update :unreachable-tables (fnil conj #{}) (crafting-table/cell-vec table)) (dissoc :table)))
                          (or (await (table-fetch! c)) :again)))))))))

(defn problem
  "The first ingredient of the last shortage still not carried up to the amount it needed, as {:item :count}, else nil."
  [c]
  (let [have (u/inventory (:primitives c))]
    (some (fn [[name want]]
            (let [n (storage/carried have name)]
              (when (< n want) {:item name :count (- want n)})))
          (:wants (ctx/mem c)))))

(defn ^:async short!
  "A craft ran out of an ingredient: fetch what it lacks (:fetch) and craft again (:again, :continue while a fetch
  waits), else emit the info and finish with what is missing, and the alternatives (name [cousins]) when other
  recipes use different ingredients.
  The primitive hands back every candidate recipe and the counts carried; jobs.items.shortfall chooses."
  [c item r made]
  (let [{:keys [short] :as shortage} (craft/no-item (js->clj (.-recipes r)) (js->clj (.-have r)))
        have (js->clj (.-have r))]
    (when (and (seq short) (:fetch (:args c)) (not (contains? (:fetched (ctx/mem c)) [made short])))
      (ctx/update-mem! c assoc :wants (into {} (map (fn [[name n]] [name (+ n (get have name 0))])) short)))
    (let [f (when (seq (:wants (ctx/mem c))) (await (fetch! c problem)))]
      (cond
        f f
        (and (seq (:wants (ctx/mem c))) (nil? (problem c)))
        (do (ctx/update-mem! c #(-> % (update :fetched (fnil conj #{}) [made short]) (dissoc :wants))) :again)

        :else
        (do (ctx/update-mem! c dissoc :wants)
            (finish! c made (assoc shortage :reason :need :text (str "craft " item " is missing " (pr-str short)))))))))

(defn ^:async craft-act!
  "The craft call alone (no follow-up): the target is carried already -> finish!'s result, else {:r the primitive's
  result :table table :start start} for settle!."
  [c]
  (let [p (:primitives c)
        {:keys [item count]} (:args c)
        _ (when-not (contains? (ctx/mem c) :start)
            (ctx/update-mem! c assoc :start (storage/carried (u/inventory p) item)))
        start (:start (ctx/mem c))
        target (+ start count)
        have (storage/carried (u/inventory p) item)]
    (if (>= have target)
      (finish! c (- have start) {})
      (let [table (or (:table (ctx/mem c)) (:table (:args c)))]
        {:r (await (ctx/act c :craft (clj->js {:item item :count (- target have) :table table})))
         :table table :start start}))))

(defn ^:async settle!
  "Act on the craft call's status (a craft-act! map): :again, :continue (go-to waits) or :done. Anything else is
  craft-act!'s own result."
  [c act]
  (if-not (:r act)
    act
    (let [p (:primitives c)
          {:keys [item]} (:args c)
          {:keys [r table start]} act
          status (.-status r)
          made (- (storage/carried (u/inventory p) item) start)]
      (when table (ctx/update-mem! c assoc :table table))
      (case status
        "crafted" (finish! c made {:text (str "crafted " made " " item)})
        "no-item" (await (short! c item r made))
        "partial" (if (= "no-item" (.-reason r))
                    (await (short! c item r made))
                    (do (u/progress! c) :again))
        "out-of-reach" (let [handed (u/pos-of (.-table r))]
                         (cond
                           (:table (:args c)) (await (reach-table! c made))
                           (carried-table? c) (or (await (table-fetch! c)) (await (reach-table! c made)))
                           (> (u/dist (u/self-pos c) handed) (:radius (:args c))) (await (no-table! c made))
                           :else (do (ctx/update-mem! c assoc :table handed)
                                     (await (reach-table! c made)))))
        "unreachable" (cond
                        (= "no-table" (.-reason r)) (do (ctx/update-mem! c dissoc :table)
                                                        (or (when (and (carried-table? c) (not (:table (:args c)))) (await (table-fetch! c)))
                                                            (await (reach-table! c made))))
                        (not= "not-a-table" (.-reason r)) (await (reach-table! c made))
                        (:table (:args c)) (finish! c made {:reason :not-a-table :text (str "not a crafting table: " (pr-str table))})
                        :else (do (ctx/update-mem! c dissoc :table) :again))
        "full" (or (await (room! c))
                   (finish! c made {:reason :full :text "inventory is full and make-room freed nothing"}))
        "cannot" (finish! c made {:reason :cannot :detail (.-reason r) :text (str "cannot craft " item ": " (.-reason r))})
        (give-up! c made :craft-failed {:detail status})))))

(defn ^:async cleanup
  "However the call ended: dig up the table this call put down, once, unless :keep-table, a zone or claim refuses the dig
  or it stands in an area of this body's own (a home): jobs.blocks.dig child, :continue while it waits. Nothing-possible
  :table-left (data :table) when the dig did not end done."
  [c _]
  (let [spot (pickup-spot c)]
    (if-not spot
      (ctx/end c :done nil {})
      (let [r (await (ctx/call c :pickup 'jobs.blocks.dig {:pos spot :collect true :need-drop false :fetch false :on-fluid :fail}))]
        (if (= :continue r)
          :continue
          (do (ctx/update-mem! c assoc :picked-up true)
              (if (= :done (:kind r))
                (ctx/end c :done nil {})
                (ctx/end c :nothing-possible :table-left {:table spot}))))))))

(defn ^:async step!
  "Stop when the target is carried, else craft the rest and act on the status: :again, :continue (go-to waits) or :done.
  A table known to be out of reach is walked to first: the craft act would only fail again while the walk waits."
  [c]
  (let [table (:table (ctx/mem c))]
    (if (and table (not (u/within? (u/self-pos c) table 3)))
      (let [have (storage/carried (u/inventory (:primitives c)) (:item (:args c)))]
        (await (reach-table! c (- have (:start (ctx/mem c) have)))))
      (await (settle! c (await (craft-act! c)))))))

(defn ^:async round
  "The whole craft in one call: step! until it ends; the ending (the table is picked up by cleanup)."
  [c]
  (await (pace/steps! c #(step! c))))
