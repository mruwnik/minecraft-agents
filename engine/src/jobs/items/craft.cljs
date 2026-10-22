(ns jobs.items.craft
  (:require [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [jobs.lib.waits :as waits]
            [jobs.lib.least :as least]
            [engine.args :as a]
            [jobs.items.shortfall :as craft]
            [jobs.items.recipes :as recipes]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.near :as near]
            [jobs.lib.crafting-table :as crafting-table]
            [jobs.lib.access :as access]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.world :as known]
            [jobs.lib.need :as need]
            [jobs.lib.checks :as checks]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.cost :as cost]
            [jobs.movement.go-to.risk :as risk]
            [jobs.lib.spent :as spent]
            [jobs.lib.spot :as free-spot]
            [jobs.lib.storage :as storage]))

(def doc
  "Craft until at least :count of :item are carried. 2x2 recipes work anywhere; bigger ones use a crafting table
  (:table, else the nearest seen within the table-radius setting), put down from the bag or fetched when none is in reach.
  Needs: an item name (impossible :bad-args); a table known when the recipe needs one, else one look round first (:look :no-table). An ingredient that runs out is fetched (jobs.lib.need) from the ways the call allows (:ways #{:carried}: stops :short).
  Returns :done {:made n :item :text}. Stopping short it ends with :made, :item and :text: condition-failed
  :need (:short, :alternatives), :no-table, :not-a-table or :full; nothing-possible :unreachable, :cannot or :craft-failed.
  A table this call put down is dug up again however the call ends, unless :keep-table.")

(a/defargs args
  {:item {:doc "item name to craft" :spec a/item? :default nil}
   :count {:doc "how many to carry at least" :spec (a/int-in 0 nil) :default 1}
   :table {:doc "crafting table position; the nearest seen within the table-radius setting when nil and the recipe needs one" :spec ::a/pos :default nil}
   :keep-table {:doc "leave a table this call put down standing; false: it is dug up again when the call ends" :spec boolean? :default false}
   :avoid {:doc "cells [x y z] a table is never put in or dug for: the dig line of a job that crafts as its child" :spec (a/coll-of a/position?) :default nil}
   :for-plan {:doc "id of the plan whose work this is: a table may stand in its own cells; nil: no plan's cells are used" :spec a/name? :default nil}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a craft may take: the walk, the batches and the table." :spec (a/num-in 0 nil)}
   ::batch-s {:default 0.5 :doc "Mean seconds one batch of a craft takes, for the quote." :spec (a/num-in 0 nil)}
   ::reach {:default 3 :doc "Blocks from a crafting table within which the body crafts without walking." :spec (a/num-in 0 nil)}
   ::table-radius {:default 64 :doc "How far to look for a crafting table, and to walk to a seen one (the seen memory reaches 64)." :spec (a/num-in 0 64)}})
(defn time-s [] (settings/get settings ::job-time-s))
(defn batch-s [] (settings/get settings ::batch-s))
(defn reach [] (settings/get settings ::reach))
(def seen-reach "How far the seen memory reaches, for naming a table beyond the table-radius setting." 64)
(defn table-radius [] (settings/get settings ::table-radius))

(def target [:item])
(def ways need/fetch-ways)

(defn stay-s
  "Seconds the craft keeps the body where it is: the batches and the walk to a seen table beyond reach and back."
  [c]
  (let [p (:primitives c)
        table (or (:table (:args c)) (crafting-table/nearest p (table-radius)))
        away (if table (max 0 (- (u/dist (u/self-pos c) table) (reach))) 0)]
    (+ (* (max 1 (or (:count (:args c)) 1)) (batch-s)) (/ (* 2 away) (cost/walk-blocks-per-s)))))

(def safe? (risk/priced ready/safe-here-by-night stay-s))
(def fit? ready/ok)

(def defaults {:least :all :spend {:job-time ::job-time-s}})

(defn bad-args
  "The cause when no item name is given."
  [c]
  (when-not (string? (:item (:args c)))
    {:reason :bad-args :why "no item name"}))

(defn carried-table? [c]
  (pos? (storage/carried (u/inventory (:primitives c)) "crafting_table")))

(defn needs-table?
  "Whether every recipe of the item has a grid bigger than the 2x2 of the inventory."
  [c]
  (let [rs (get (recipes/recipes-for (game/version-of (:primitives c))) (:item (:args c)))]
    (and (seq rs) (every? :table? rs))))

(defn table-known?
  "A table handed in, chosen, carried or seen within the radius."
  [c]
  (boolean (or (:table (:args c)) (:table (ctx/mem c)) (carried-table? c)
               (crafting-table/nearest (:primitives c) (table-radius)))))

(defn can-start?
  "Never without an item name. Look (once) when the recipe needs a table and none is known: the round fetches or puts one down after that."
  [c]
  (contract/all [(checks/cause-status :never (bad-args c))
                 (if (needs-table? c)
                   (checks/seen-or-look c (table-known? c) :no-table
                                        {:known "a crafting table is known" :missing "no crafting table in sight" :fetches? true})
                   (checks/holds))]))

(defn pickup-spot
  "The table cell this call put down and is still to dig up, else nil: not with :keep-table, once dug, gone, in an area of the
  body's own (but a cell of the :for-plan plan is dug: the build needs it) or refused by a zone."
  [c]
  (let [spot (:table-placed (ctx/mem c))
        for-plan (:for-plan (:args c))
        in-plan? (and for-plan spot (= for-plan (get (known/footprints c) (mapv #(js/Math.floor %) [(:x spot) (:y spot) (:z spot)]))))]
    (when (and spot (not (:keep-table (:args c))) (not (:picked-up (ctx/mem c)))
               (= "crafting_table" (u/seen-name (:primitives c) spot))
               (or in-plan? (not (access/in-own-area? c spot)))
               (:ok (access/may? (access/rules-input c {:except (:for-plan (:args c))}) :dig spot)))
      spot)))

(defn done?
  "Done once :count of :item are carried; else progress."
  [c]
  (let [{:keys [item count]} (:args c)
        have (storage/usable-carried (u/inventory (:primitives c)) item)
        start (:start (ctx/mem c) have)
        made (max 0 (- have start))
        data [:units :items :done made :left (max 0 (- count have)) :result {:made made}]]
    (if (>= have count)
      (if (and (zero? made) (pos? have))
        (apply contract/ok :crafted (str "already carried " have " " item "; pass :count N for more") data)
        (apply contract/ok :crafted (str "crafted " made " " item) data))
      (apply contract/later :crafting (str "craft: " made " of " count " " item) data))))

(defn chooser
  "A craft under a torch stock (slot :torches) or a spare tool (slot :spare) asks the register as :craft-held: the kit's fuel and sticks stay out of it.
  Other crafts, the kit's own and the mine's light-now torches, ask as :craft."
  [c]
  (if (some #{"torches" "spare"} (str/split (str (:id c)) #"/")) :craft-held :craft))

(defn spendable
  "{name n} of the ingredients this call may use: carried less what the register holds, the item made counted whole.
  Members of an interchange class (planks of any wood) count at the class total unless raw?."
  [c & [raw?]]
  (let [item (:item (:args c))
        free (storage/carried-totals (reserve/free c (chooser c)))]
    (assoc (if raw? free (recipes/pooled (game/version-of (:primitives c)) item free)) item (storage/usable-carried (u/inventory (:primitives c)) item))))

(defn quote-of
  "P: the cheapest recipe for the rest of :count of :item from what is carried. It uses its ingredients (net of the item
  itself, a least of 0 since another recipe may serve) and yields at least that rest."
  [c]
  (let [{:keys [item]} (:args c)
        p (:primitives c)
        count (max 0 (- (:count (:args c)) (storage/usable-carried (u/inventory p) item)))
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
  "Why no table is used: none within radius, naming the nearest seen one  when there is one, the ones a
  walk could not reach, and why a carried table was not put down."
  [c radius]
  (let [skip (unreachable-tables c)
        far (crafting-table/nearest (:primitives c) seen-reach skip)
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
        total (max 0 (- (:count (:args c)) (:start (ctx/mem c) 0)))]
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

(defn table-problem
  "The crafting table to get while one is wanted and none is carried, else nil."
  [c]
  (when (and (:want-table (ctx/mem c)) (not (carried-table? c)))
    {:item "crafting_table" :count 1}))

(defn ^:async fetch!
  "Get what (problem c) names with jobs.items.obtain children (jobs.lib.need) until nothing is due any more: nil (the item
  arrived, the fetch failed or the call's ways allow none; the craft judges where it stands), :continue while a child waits."
  [c problem]
  (loop []
    (when-let [{:keys [item count]} (and (seq (need/fetch-how c)) (problem c))]
      (let [e (await (need/item! c item count {:how (need/fetch-how c) :slot (keyword (str "need-" item)) :for-plan (:for-plan (:args c))}))]
        (cond
          (= :continue e) :continue
          (not= :done (:kind e)) nil
          (ctx/alive? c) (do (await (pace/pace!)) (recur))
          :else :continue)))))

(defn table-why! [c why] (ctx/update-mem! c assoc :table-why why) nil)

(defn ^:async place-table!
  "Put the carried table down in a free cell beside the body (free-spot/place-beside!). :again once it stands (it is the
  table to craft at), :continue while a child waits, nil when it could not be put down."
  [c]
  (let [r (await (free-spot/place-beside! c "crafting_table" [:table-spot] {:args {:for-plan (:for-plan (:args c))}}))]
    (cond
      (= :continue r) :continue
      (:placed r) (let [spot (:placed r)]
                    (ctx/update-mem! c #(-> % (assoc :table spot :table-placed spot) (dissoc :want-table)))
                    (spent/note! c "crafting_table" spot)
                    :again)
      :else (table-why! c (str "the carried crafting_table could not be put down: " (:why r))))))

(declare craft-act! settle! short-of-ingredients!)

(defn keep-items
  "The names make-room must not toss: the item and every ingredient of every recipe for it, and the table."
  [c]
  (let [item (:item (:args c))
        rs (get (recipes/recipes-for (game/version-of (:primitives c))) item)]
    (vec (distinct (concat [item "crafting_table"] (mapcat (comp keys :needs) rs))))))

(defn ^:async room!
  "A full bag: once per craft, a jobs.storage.make-room child (child :room) that keeps the recipe's inputs. :continue
  while it waits, :again when it ran, nil when it was tried before."
  [c]
  (when-not (:room-tried (ctx/mem c))
    (let [r (await (ctx/call c :room 'jobs.storage.make-room {:keep-items (keep-items c)}))]
      (if (= :continue r)
        :continue
        (do (ctx/update-mem! c assoc :room-tried true)
            :again)))))

(defn ^:async table-step!
  "table-fetch! once the ingredients are in hand: fetch a table when wanted, put a carried one down and craft at it."
  [c]
  (when (and (seq (need/fetch-how c)) (not (carried-table? c))) (ctx/update-mem! c assoc :want-table true))
  (let [f (when (:want-table (ctx/mem c)) (await (fetch! c table-problem)))
        r (when (and (nil? f) (carried-table? c))
            (await (ctx/atomic c :urgent :craft
                               (fn ^:async place-and-craft []
                                 (let [placed (await (place-table! c))]
                                   (if (= :again placed) (await (craft-act! c)) placed))))))
        r (if (map? r) (await (settle! c r)) r)]
    (when-not (or f r) (ctx/update-mem! c #(-> % (dissoc :want-table) (assoc :table-tried true))))
    (or f r)))

(defn ^:async table-fetch!
  "With no table in reach: put a carried table down (also when the ways allow no fetch), else fetch one and put it down. Once per craft.
  Missing ingredients come first (short-of-ingredients!): a table is the last step. :again / :continue to go on, nil when
  there is nothing to do (give up). Putting a carried table down and the craft call at
  it run as one urgent section (ctx/atomic :craft), so a reflex does not leave the table unused; the
  follow-up on the craft's status (settle!) is outside it."
  [c]
  (when-not (:table-tried (ctx/mem c))
    (or (await (short-of-ingredients! c))
        (await (table-step! c)))))

(defn ^:async no-table!
  "No crafting table is within the radius or one can be reached: fetch one (table-fetch!), else warn and finish, with
  reason \"unreachable\" when a walk to a table failed. :again, :continue or :done."
  [c made]
  (or (await (table-fetch! c))
      (finish! c made {:reason (if (seq (unreachable-tables c)) :unreachable :no-table)
                       :text (no-table-text c (table-radius))})))

(defn ^:async reach-table!
  "The craft was unreachable: walk to the remembered or the nearest table with a go-to child. A table the walk cannot
  reach (not one handed in as :table) is dropped for this call and a carried or fetched table is put down instead.
  :again to craft again, :continue while go-to waits, :done when there is none or the walk was given up."
  [c made]
  (let [{:keys [table]} (merge (:args c) (ctx/mem c))
        p (:primitives c)
        handed? (some? table)
        arg? (some? (:table (:args c)))
        skip (unreachable-tables c)
        table (or table (crafting-table/nearest p (table-radius) skip))]
    (cond
      (nil? table)
      (await (no-table! c made))

      :else
      (do (ctx/update-mem! c assoc :table table)
          (if (u/within? (u/self-pos c) table (reach))
            (if handed? (give-up! c made :unreachable) :again)
            (let [r (await (near/go-near! c table (reach) {:ways #{:walk :clear-seal}}))]
              (cond
                (= :partial r) :continue
                (and (= :there r) (u/within? (u/self-pos c) table (reach))) :again
                arg? (give-up! c made :unreachable)
                :else (do (ctx/update-mem! c #(-> % (update :unreachable-tables (fnil conj #{}) (crafting-table/cell-vec table)) (dissoc :table)))
                          (or (await (table-fetch! c)) :again)))))))))

(defn problem
  "The first ingredient of the last shortage still not carried up to the amount it needed, as {:item :count}, else nil."
  [c]
  (let [have (reserve/free c (chooser c))]
    (some (fn [[name want]]
            (let [n (storage/carried have name)]
              (when (< n want) {:item name :count (- want n)})))
          (:wants (ctx/mem c)))))

(defn ^:async short!
  "A craft ran out of an ingredient: fetch what it lacks and craft again (:again, :continue while a fetch
  waits), else emit the info and finish with what is missing, and the alternatives (name [cousins]) when other
  recipes use different ingredients. shortage is jobs.items.shortfall/no-item over the candidate recipes, have the
  counts carried."
  [c item shortage have made]
  (let [{:keys [short]} shortage]
    (when (and (seq short) (seq (need/fetch-how c)) (not (contains? (:fetched (ctx/mem c)) [made short])))
      (ctx/update-mem! c assoc :wants (into {} (map (fn [[name n]] [name (+ n (get have name 0))])) short)))
    (let [f (when (seq (:wants (ctx/mem c))) (await (fetch! c problem)))]
      (cond
        f f
        (and (seq (:wants (ctx/mem c))) (nil? (problem c)))
        (do (ctx/update-mem! c #(-> % (update :fetched (fnil conj #{}) [made short]) (dissoc :wants))) :again)

        :else
        (do (ctx/update-mem! c dissoc :wants)
            (finish! c made (assoc shortage :reason :need :text (str "craft " item " is missing " (pr-str short)))))))))

(defn makeable
  "How many of the item the best recipe makes from the spendable ingredients (whole batches, no sub-crafts)."
  [c]
  (let [{:keys [item]} (:args c)
        have (spendable c)
        rs (get (recipes/recipes-for (game/version-of (:primitives c))) item)]
    (reduce max 0 (for [r rs
                        :let [needs (dissoc (:needs r) item)]]
                    (* (:count r) (reduce min ##Inf (map (fn [[k v]] (quot (get have k 0) v)) needs)))))))

(defn ^:async short-of-ingredients!
  "Before a table is put down or fetched: when no recipe of the item can be made from what is carried, short! (fetch
  them or end :need), else nil. A table is the last step. With n, the shortage is for n more of the item."
  [c & [n]]
  (let [p (:primitives c)
        {:keys [item count]} (:args c)
        have (spendable c)
        start (:start (ctx/mem c) (get have item 0))
        todo (or n (max 1 (- count (get have item 0))))
        rs (get (recipes/recipes-for (game/version-of p)) item)
        needs (for [r rs :let [b (recipes/ceil-div todo (:count r))]]
                (into {} (map (fn [[k v]] [k (* v b)])) (dissoc (:needs r) item)))]
    (when (and (seq needs) (every? #(seq (craft/missing-for % have)) needs))
      (await (short! c item (craft/no-item needs have) (spendable c true) (- (get have item 0) start))))))

(defn ^:async short-from!
  "short! for a craft call that answered no-item."
  [c item r made]
  (let [have (js->clj (.-have r))]
    (await (short! c item (craft/no-item (js->clj (.-recipes r)) have) have made))))

(defn ^:async craft-act!
  "The craft call alone (no follow-up): the target is carried already -> finish!'s result, else {:r the primitive's
  result :table table :start start} for settle!."
  [c]
  (let [p (:primitives c)
        {:keys [item count]} (:args c)
        _ (when-not (contains? (ctx/mem c) :start)
            (ctx/update-mem! c assoc :start (storage/usable-carried (u/inventory p) item)))
        start (:start (ctx/mem c))
        target count
        have (storage/usable-carried (u/inventory p) item)]
    (if (>= have target)
      (finish! c (- have start) {})
      (let [held? (seq (reserve/held c (chooser c)))
            cap (if held? (makeable c) ##Inf)]
        (or (when (zero? cap) (await (short-of-ingredients! c 1)))
            (let [table (or (:table (ctx/mem c)) (:table (:args c)))]
              {:r (await (ctx/act c :craft (clj->js {:item item :count (min cap (- target have)) :table table :standIns (recipes/interchange (game/version-of (:primitives c)) item)})))
               :table table :start start}))))))

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
          made (- (storage/usable-carried (u/inventory p) item) start)]
      (when table (ctx/update-mem! c assoc :table table))
      (case status
        "crafted" (if (< (storage/usable-carried (u/inventory p) item) (:count (:args c)))
                    (do (u/progress! c) :again)
                    (finish! c made {:text (str "crafted " made " " item)}))
        "no-item" (await (short-from! c item r made))
        "partial" (if (= "no-item" (.-reason r))
                    (await (short-from! c item r made))
                    (do (u/progress! c) :again))
        "out-of-reach" (let [handed (u/pos-of (.-table r))]
                         (cond
                           (:table (:args c)) (await (reach-table! c made))
                           (carried-table? c) (or (await (table-fetch! c)) (await (reach-table! c made)))
                           (> (u/dist (u/self-pos c) handed) (table-radius)) (await (no-table! c made))
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
      (let [r (waits/waiting-out c (await (ctx/call c :pickup 'jobs.blocks.dig {:pos spot :collect true :need-drop false :ways #{:carried} :on-fluid :fail :for-plan (:for-plan (:args c))})))]
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
    (if (and table (not (u/within? (u/self-pos c) table (reach))))
      (let [have (storage/carried (u/inventory (:primitives c)) (:item (:args c)))]
        (await (reach-table! c (- have (:start (ctx/mem c) have)))))
      (await (settle! c (await (craft-act! c)))))))

(defn ^:async round
  "The whole craft in one call: step! until it ends; the ending (the table is picked up by cleanup)."
  [c]
  (await (pace/steps! c #(step! c))))

(def meta {:tags [:craft]})
