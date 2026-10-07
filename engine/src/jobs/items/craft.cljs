(ns jobs.items.craft
  (:require [jobs.items.shortfall :as craft]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.look :as look]
            [jobs.lib.storage :as storage]))

(def doc
  "Craft :count more of :item. 2x2 recipes work anywhere. Bigger ones need a crafting table: :table, else the
  nearest within :radius, walking there when out of reach.
  The count carried at the start is kept in memory and the target is that plus :count, so a cut or a partial
  batch loses nothing.
  Ends with {:made n}, the number of items gained. When it stops short it adds :short {name n} (an ingredient ran
  out) or :reason (\"no-table\", \"not-a-table\", \"unreachable\", \"full\", the cannot reason, or the failed
  status), after a warn, and its status is stopped. One call is the whole craft: it walks (a go-to child) and crafts
  again until the count is carried or it stops; it yields only while go-to waits on the world.
  An ingredient that runs out is fetched (jobs.lib.fetch: a jobs.items.obtain child for each missing item) and the
  craft goes on, unless :fetch is false or the fetch failed: then it stops with :short. With no table in reach a
  carried one is put down beside the body, else one is fetched (a crafting_table obtain child) and put down, the same
  way; :fetch false or a failed fetch stops with :reason \"no-table\".")

(def args
  {:item {:doc "item name to craft" :default nil}
   :count {:doc "how many more to end up with" :default 1}
   :table {:doc "crafting table position; the nearest seen within :radius when nil and the recipe needs one" :type :pos :default nil}
   :radius {:doc "how far to look for a crafting table" :default 32}
   :fetch {:doc "get an ingredient that runs out (jobs.lib.fetch): true, a set of kinds or a map of limits; false stops :short" :default true}})

(defn check
  "An item name is given."
  [c]
  (or (string? (:item (:args c)))
      (ctx/wait c {:reason :bad-args :why "no item name"})))

(defn nearest-table
  "The position of the nearest crafting table the body has seen within radius, or nil."
  [p radius]
  (:pos (first (look/seen-blocks p {:names ["crafting_table"] :radius radius :max 8 :live? true}))))

(defn finish!
  "Hand the parent a result, made so far plus extra (a stop when extra is not empty: :status :stopped), and return :done."
  [c made extra]
  (ctx/result! c (cond-> (merge {:made made} extra) (seq extra) (assoc :status :stopped)))
  :done)

(defn give-up!
  "u/fail!: :again until the third failure in a row, then finish with the made count and the reason."
  [c made reason]
  (let [r (u/fail! c :craft.gave-up (str "craft gave up: " reason))]
    (if (= :done r) (finish! c made {:reason reason}) :again)))

(def table-offsets [[1 0] [-1 0] [0 1] [0 -1] [1 1] [-1 1] [1 -1] [-1 -1]])

(defn spot-cells
  "The candidate table cells beside the body: [pos floor-pos] for each offset."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)
        [x y z] (mapv #(js/Math.floor %) [x y z])]
    (for [[dx dz] table-offsets
          :let [pos {:x (+ x dx) :y y :z (+ z dz)}]]
      [pos (update pos :y dec)])))

(defn table-spot
  "A free cell beside the body to put a table in: air at the body's level with a solid block under it, or nil."
  [c]
  (let [p (:primitives c)
        solid? (fn [n] (and n (not (b/air n)) (not (b/fluids n)) (not (b/clearable n))))]
    (some (fn [[pos floor]]
            (when (and (b/air (u/seen-name p pos)) (solid? (u/seen-name p floor)))
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

(defn carried-table? [c]
  (pos? (storage/carried (u/inventory (:primitives c)) "crafting_table")))

(defn table-problem
  "The :need wait for a crafting table while one is wanted and none is carried, else nil."
  [c]
  (when (and (:want-table (ctx/mem c)) (not (carried-table? c)))
    {:reason :need :item "crafting_table" :count 1}))

(defn ^:async place-table!
  "Put the carried table down in a free cell beside the body (a jobs.blocks.place child; the floor is looked at first
  when no spot is seen). :again once it stands (it is the table to craft at), :continue while the child waits, nil when
  it could not be put down."
  [c]
  (let [spot (or (:table-spot (ctx/mem c)) (await (find-spot! c)))]
    (when spot
      (ctx/update-mem! c assoc :table-spot spot)
      (let [r (await (ctx/call-child c :place 'jobs.blocks.place {:item "crafting_table" :pos spot :fetch false}))]
        (if (= :continue r)
          :continue
          (do (ctx/update-mem! c dissoc :table-spot)
              (when (and (= :done r) (:placed (ctx/child-result c :place)))
                (ctx/update-mem! c #(-> % (assoc :table spot) (dissoc :want-table)))
                :again)))))))

(defn ^:async table-fetch!
  "With no table in reach: put a carried table down, else fetch one (:fetch) and put it down. Once per craft.
  :again / :continue to go on, :done for a bad :fetch arg, nil when there is nothing to do (give up)."
  [c]
  (let [o (fetch/opts c 'jobs.items.craft)]
    (when (and o (not (:error o)) (not (:table-tried (ctx/mem c))))
      (when-not (carried-table? c) (ctx/update-mem! c assoc :want-table true))
      (let [f (when (:want-table (ctx/mem c)) (await (fetch/fetch! c 'jobs.items.craft table-problem)))
            r (when (and (nil? f) (carried-table? c)) (await (place-table! c)))]
        (when-not (or f r) (ctx/update-mem! c #(-> % (dissoc :want-table) (assoc :table-tried true))))
        (or f r)))))

(defn ^:async no-table!
  "No crafting table is within the radius: fetch one (table-fetch!), else warn and finish. :again, :continue or :done."
  [c made]
  (or (await (table-fetch! c))
      (do (ctx/emit! c :craft.no-table :warn {:text (str "no crafting table within " (:radius (:args c)))})
          (finish! c made {:reason "no-table"}))))

(defn ^:async reach-table!
  "The craft was unreachable: walk to the remembered or the nearest table with a go-to child.
  :again to craft again, :continue while go-to waits, :done when there is none or the walk was given up."
  [c made]
  (let [{:keys [table radius]} (merge (:args c) (ctx/mem c))
        p (:primitives c)
        handed? (some? table)
        table (or table (nearest-table p radius))]
    (cond
      (and (nil? table) (not (look/surveyed? c)))
      (do (await (look/survey! c)) :again) ; a table behind the body is not seen until it looks

      (nil? table)
      (await (no-table! c made))

      :else
      (do (ctx/update-mem! c assoc :table table)
          (if (u/within? (u/self-pos c) table 3)
            (if handed? (give-up! c made "unreachable") :again)
            (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos table :range 3 :escalate false :warn false :retry false :zone-tolls true}))]
              (cond
                (= :continue r) :continue
                (:arrived (ctx/child-result c :walk)) :again
                :else (give-up! c made "unreachable"))))))))

(defn problem
  "The :need wait for the first ingredient of the last shortage still not carried up to the amount it needed, else nil."
  [c]
  (let [have (u/inventory (:primitives c))]
    (some (fn [[name want]]
            (let [n (storage/carried have name)]
              (when (< n want) {:reason :need :item name :count (- want n)})))
          (:wants (ctx/mem c)))))

(defn ^:async short!
  "A craft ran out of an ingredient: fetch what it lacks (:fetch) and craft again (:again, :continue while a fetch
  waits), else emit the info and finish with what is missing, and the alternatives (name [cousins]) when other
  recipes use different ingredients.
  The primitive hands back every candidate recipe and the counts carried; jobs.items.shortfall chooses."
  [c item r made]
  (let [{:keys [short] :as shortage} (craft/no-item (js->clj (.-recipes r)) (js->clj (.-have r)))
        have (js->clj (.-have r))]
    (when (and (seq short) (fetch/opts c 'jobs.items.craft) (not (:fetched (ctx/mem c))))
      (ctx/update-mem! c assoc :wants (into {} (map (fn [[name n]] [name (+ n (get have name 0))])) short)))
    (let [f (when (seq (:wants (ctx/mem c))) (await (fetch/fetch! c 'jobs.items.craft problem)))]
      (cond
        f f
        (and (seq (:wants (ctx/mem c))) (nil? (problem c)))
        (do (ctx/update-mem! c #(-> % (assoc :fetched true) (dissoc :wants))) :again)

        :else
        (do (ctx/update-mem! c dissoc :wants)
            (ctx/emit! c :craft.short :info {:text (str "craft " item " is missing " (pr-str short))})
            (finish! c made shortage))))))

(defn ^:async step!
  "Stop when the target is carried, else craft the rest and act on the status: :again, :continue (go-to waits) or :done."
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
      (let [table (or (:table (ctx/mem c)) (:table (:args c)))
            r (await (ctx/act c :craft (clj->js {:item item :count (- target have) :table table})))
            status (.-status r)
            made (- (storage/carried (u/inventory p) item) start)]
        (when table (ctx/update-mem! c assoc :table table))
        (case status
          "crafted" (finish! c made {})
          "no-item" (await (short! c item r made))
          "partial" (if (= "no-item" (.-reason r))
                      (await (short! c item r made))
                      (do (u/progress! c) :again))
          "out-of-reach" (let [handed (u/pos-of (.-table r))]
                           (cond
                             (:table (:args c)) (await (reach-table! c made))
                             (> (u/dist (u/self-pos c) handed) (:radius (:args c))) (await (no-table! c made))
                             :else (do (ctx/update-mem! c assoc :table handed)
                                       (await (reach-table! c made)))))
          "unreachable" (cond
                          (= "no-table" (.-reason r)) (do (ctx/update-mem! c dissoc :table)
                                                          (await (reach-table! c made)))
                          (not= "not-a-table" (.-reason r)) (await (reach-table! c made))
                          (:table (:args c)) (do (ctx/emit! c :craft.no-table :warn {:text (str "not a crafting table: " (pr-str table))})
                                                 (finish! c made {:reason "not-a-table"}))
                          :else (do (ctx/update-mem! c dissoc :table) :again))
          "full" (do (ctx/emit! c :craft.full :warn {:text "inventory is full"})
                     (finish! c made {:reason "full"}))
          "cannot" (do (ctx/emit! c :craft.cannot :warn {:text (str "cannot craft " item ": " (.-reason r))})
                       (finish! c made {:reason (.-reason r)}))
          (give-up! c made status))))))

(defn ^:async round
  "The whole craft in one call: step! until it is done or stopped."
  [c]
  (await (pace/steps! c (fn ^:async craft-step [] (await (step! c))))))
