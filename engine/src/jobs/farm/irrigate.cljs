(ns jobs.farm.irrigate
  (:require [jobs.lib.block-sets :as bs]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.farm.find-spot :as find-spot]
            [jobs.farm.till :as till]
            [jobs.lib.blocks :as b]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.gate :as gate]
            [jobs.lib.job-common :as common]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.util :as u]
            [jobs.lib.work-place :as work-place]))

(def doc
  "Give the farm cells of a box (:from/:to) or square (:center/:radius, the cells of jobs.farm.till) water: a cell of
  the field the body has seen, farmland or ground a hoe would till, is dry when every block within 4 in x and z of it, at its height
  and one above, is seen and none is water; a cell with an unseen block in reach and no water is unknown, never dry. For the spot
  that wets the most dry cells (one of the field's own cells under open air whose four sides are seen solid and underside not seen open, nearest
  first) it digs the cell (a jobs.blocks.dig child) and pours a water bucket in it (a jobs.blocks.use-on child), then
  looks again (a pour that fails, or a bucket gone before it, has the cell refilled with dirt (a jobs.blocks.place child); a cell that stays open is named in :holes and ends the call, a refilled one only drops that spot); one bucket wets one spot, so it ends when no cell is dry, no water bucket is left or no spot helps. A pour
  counts only once the cell is seen as water. A water bucket is fetched (jobs.lib.need, :fetch) when dry cells are left and none is carried; a
  failed fetch ends the call with its ending as the :because. Zones: a spot in another owner's zone or claim, or in a
  plan's footprint (unless :for-plan), is not used, with an irrigate.declined warn once.
  Refused before it starts: impossible {:reason :bad-args :why}; failed conditions {:reason :no-zones} (unless :ignore-zones?)
  and {:reason :need :item \"water_bucket\"} (dry cells, none carried, :fetch false).
  Ends with data {:poured [cells] :dry [cells still dry] :unknown [cells with unseen reach] :failed [spots given up] :restored
  [dug cells refilled] :holes [dug cells left open]}: done when no cell is dry (and, if none was poured, none is unknown), or the cells
  wetted meet :least (default 1; :all: every dry cell); else condition-failed :need (no bucket), nothing-possible
  :nothing-seen (no field cell seen), :unseen (only unknown cells left) or :no-spot.
  done? is ok when a field cell is seen, none is dry or unknown after a look and no child is under way; it counts the dry cells left.")

(a/defargs args
  {:from {:doc "box corner (inclusive); with :to, any order" :spec ::a/pos :default nil}
   :to {:doc "opposite box corner (inclusive)" :spec ::a/pos :default nil}
   :center {:doc "centre of a square of cells at its y; with :radius" :spec ::a/pos :default nil}
   :radius {:doc "the square covers |dx|,|dz| <= radius, 0 to 7" :spec (a/int-in 0 7) :default nil}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :spec a/name? :default nil}
   :fetch {:doc "get a water bucket when none is carried (jobs.lib.need): true, a set of kinds or a map of limits; false refuses the call :need" :spec need/option? :default true}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 180 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}})

(def target [:from :to :center :radius])
(def defaults {:spend {:job-time ::job-time-s}})
(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)

(def field-ground (conj till/tillable "farmland"))

(defn field-cells
  "The seen cells of the box that are farmland or ground a hoe tills (none for bad cell args, which can-start? refuses)."
  [c]
  (let [p (:primitives c)
        cs (try (till/cells (:args c)) (catch :default _ []))]
    (filterv #(field-ground (u/seen-name p %)) cs)))

(defn waters
  "The seen water as a set of [x y z] near the cells; a cell poured is water only once it is seen as such."
  [c cs]
  (let [me (u/self-pos c)
        reach (min 64 (+ (find-spot/water-reach) 1 (apply max 0 (map #(u/dist me %) cs))))]
    (into #{}
          (map (fn [{:keys [x y z]}] [x y z]))
          (map :pos (look/seen-blocks (:primitives c) {:names ["water"] :radius reach :max 4096 :live? true})))))

(defn reach-seen?
  "Whether every block a water source could wet the cell from is seen."
  [p {:keys [x y z]}]
  (let [r (find-spot/water-reach)]
    (every? #(u/seen-name p %)
            (for [dx (range (- r) (inc r)) dz (range (- r) (inc r)) dy [0 1]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)}))))

(defn readings
  "The field cells by state: :dry (reach seen, no water), :unknown (water absent, reach partly unseen), :wet."
  [c]
  (let [cs (field-cells c)
        ws (waters c cs)
        p (:primitives c)]
    (group-by (fn [{:keys [x y z] :as cell}]
                (cond (find-spot/hydrated? ws x y z) :wet
                      (reach-seen? p cell) :dry
                      :else :unknown))
              cs)))

(defn dry-cells
  "The field cells with the whole reach seen and no water in it."
  [c]
  (vec (:dry (readings c))))

(defn unknown-cells [c] (vec (:unknown (readings c))))

(defn covers
  "How many of dry a water cell at spot wets."
  [dry spot]
  (let [ws #{[(:x spot) (:y spot) (:z spot)]}]
    (count (filter (fn [{:keys [x y z]}] (find-spot/hydrated? ws x y z)) dry))))

(defn permitted? [c pos]
  (gate/allowed? c :irrigate.declined "irrigate" :dig pos {:except (:for-plan (:args c))}))

(defn solid-at?
  "Whether the block at pos is seen and neither air, fluid nor clearable."
  [p pos]
  (let [n (u/seen-name p pos)]
    (and n (not (bs/air n)) (not (bs/fluids n)) (not (b/clearable n)))))

(defn contained?
  "Whether the cell's four sides are seen solid and its underside is not seen open (farmland hides what it rests on until
  the cell is dug), so a source poured there stays put."
  [p {:keys [x y z]}]
  (and (every? (fn [[dx dz]] (solid-at? p {:x (+ x dx) :y y :z (+ z dz)})) [[1 0] [-1 0] [0 1] [0 -1]])
       (let [under {:x x :y (dec y) :z z}]
         (or (nil? (u/seen-name p under)) (solid-at? p under)))))

(defn spot
  "The field cell to turn into water: the one that wets the most of dry, nearest first; nil when none helps. Only a
  cell with air above and a contained hole, not given up, that the zone rules let the body dig."
  [c dry]
  (let [p (:primitives c)
        me (u/self-pos c)
        failed (:failed (ctx/mem c) #{})
        open (->> (field-cells c)
                  (remove failed)
                  (remove (set (:poured (ctx/mem c))))
                  (filter #(= "air" (u/seen-name p (update % :y inc))))
                  (filter #(contained? p %))
                  (filter #(permitted? c %)))]
    (->> open
         (keep (fn [pos] (let [n (covers dry pos)] (when (pos? n) [n pos]))))
         (sort-by (fn [[n pos]] [(- n) (u/dist me pos)]))
         first
         second)))

(defn has-bucket? [p] (boolean (some #(= "water_bucket" (:name %)) (u/inventory p))))

(defn fetch-opts
  "The limits {:how :explore} for fetching a water bucket, nil when :fetch does not allow items or leaves only
  :gather (which would scoop back the water just poured)."
  [c]
  (when-let [o (need/opts c 'jobs.farm.irrigate)]
    (let [how (disj (set (or (:how o) need/sources)) :gather)]
      (when (and (contains? (:what o) :item) (seq how)) (assoc (select-keys o [:explore]) :how how)))))

(defn needs-bucket?
  "Whether a bucket is to be got: dry cells, none carried and no pour waiting."
  [c]
  (and (not (has-bucket? (:primitives c))) (nil? (:hole (ctx/mem c))) (nil? (:restore (ctx/mem c))) (seq (dry-cells c))))

(defn no-bucket-source [c]
  (when (and (needs-bucket? c) (nil? (fetch-opts c))) {:reason :need :item "water_bucket"}))

(def can-start? (cond/and till/bad-args
                          (checks/zones-read :irrigate.declined "irrigate")
                          (checks/refusing :bucket "water bucket carried or fetchable" no-bucket-source)))

(defn result-of [c]
  (let [m (ctx/mem c)
        {:keys [dry unknown]} (readings c)]
    {:poured (vec (:poured m)) :dry (vec dry) :unknown (vec unknown) :failed (vec (:failed m)) :restored (vec (:restored m)) :holes (vec (:holes m))}))

(defn done?
  [c]
  (let [{:keys [dry unknown wet]} (readings c)
        dry (count dry)
        data [:units :cells :done (count (:poured (ctx/mem c))) :left dry :result (result-of c)]]
    (cond
      (pos? dry) (apply contract/later :dry (str "irrigate: " dry " cells dry") data)
      (not (look/surveyed? c)) (apply contract/later :looking "irrigate: no cell dry, looking round" data)
      (seq unknown) (apply contract/later :unseen (str "irrigate: " (count unknown) " cells with unseen reach") data)
      (empty? wet) (apply contract/later :nothing-seen "irrigate: no field cell seen" data)
      (checks/child-under-way? c) (apply contract/later :wrapping-up "irrigate: wrapping up" data)
      :else (apply contract/ok :irrigated "irrigate: every cell has water in reach" data))))

(defn quote-of [c]
  (let [n (count (dry-cells c))]
    {:uses {:job-time [(if (pos? n) 5 0) (if (pos? n) 30 0) (if (pos? n) 120 0)] :items {"water_bucket" [0 (min 1 n) 1]}}
     :done [0 n n]}))

(defn conclude!
  "The ending when no more can be poured: reason is why dry cells remain (:need or :no-spot)."
  [c reason]
  (let [{:keys [dry unknown holes poured] :as result} (result-of c)
        dry0 (:dry0 (ctx/mem c) (count dry))
        text (str "irrigated " (count poured) " spots, " (count dry) " cells dry"
                  (when (seq unknown) (str ", " (count unknown) " unseen"))
                  (when (seq holes) (str ", " (count holes) " dug holes left open")))
        data (assoc result :text text)]
    (cond
      (and (empty? (field-cells c)) (empty? poured)) (ctx/end c :nothing-possible :nothing-seen data)
      (and (empty? dry) (or (empty? unknown) (seq poured))) (ctx/end c :done nil data)
      (and (seq dry) (common/enough? c (- dry0 (count dry)) dry0)) (ctx/end c :done nil data)
      (= :need reason) (ctx/end c :condition-failed :need data)
      (empty? dry) (ctx/end c :nothing-possible :unseen data)
      :else (ctx/end c :nothing-possible reason data))))

(defn unpour!
  "A dug cell the water did not fill is to be refilled (:restore) and not tried again."
  [c hole]
  (ctx/update-mem! c #(-> (dissoc % :hole) (assoc :restore hole) (update :failed (fnil conj #{}) hole))))

(defn ^:async restore!
  "Put dirt back in the dug cell; a cell that stays open is booked in :holes. :continue while the child waits."
  [c hole]
  (let [e (await (ctx/call c :restore 'jobs.blocks.place (merge (select-keys (:args c) [:ignore-zones?]) {:pos hole :item "dirt" :clear false})))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (ctx/update-mem! c #(-> (dissoc % :restore) (update :restored (fnil conj #{}) hole))) :again)
      :else (do (ctx/update-mem! c #(-> (dissoc % :restore) (update :holes (fnil conj #{}) hole))) :again))))

(defn ^:async pour!
  "Pour the bucket in the dug hole. :again, or :continue while the child waits."
  [c hole]
  (let [e (await (ctx/call c :pour 'jobs.blocks.use-on (merge (select-keys (:args c) [:ignore-zones?]) {:pos hole :item "water_bucket"})))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (ctx/update-mem! c #(-> (dissoc % :hole) (update :poured (fnil conj #{}) hole))) :again)
      :else (do (unpour! c hole) :again))))

(defn ^:async dig!
  "Dig the field cell at hole; the pour waits in memory (:hole) so a restart finishes it."
  [c hole]
  (let [e (await (ctx/call c :dig 'jobs.blocks.dig (till/cover-args c hole)))]
    (cond
      (= :continue e) :continue
      (and (= :done (:kind e)) (#{:dug :already-clear} (:reason (:data e)))) (do (ctx/update-mem! c assoc :hole hole) :again)
      :else (do (ctx/update-mem! c update :failed (fnil conj #{}) hole) :again))))

(defn ^:async step
  "One step: pour a dug hole; else with dry cells left fetch a bucket if none is carried, and dig the best spot."
  [c]
  (let [p (:primitives c)
        {:keys [hole restore]} (ctx/mem c)
        _ (when (and (not hole) (not restore) (empty? (dry-cells c)) (not (look/surveyed? c :cell))) (await (look/survey! c :cell)))
        dry (when-not (or hole restore) (dry-cells c))
        _ (when (and dry (not (:dry0 (ctx/mem c)))) (ctx/update-mem! c assoc :dry0 (count dry)))]
    (cond
      restore (await (restore! c restore))
      hole (if (and (has-bucket? p) (solid-at? p (update hole :y dec))) (await (pour! c hole)) (do (unpour! c hole) :again))
      (empty? dry) (conclude! c nil)
      (seq (:holes (ctx/mem c))) (conclude! c :no-spot)
      (not (has-bucket? p))
      (let [e (await (need/item! c "water_bucket" 1 (fetch-opts c)))]
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) :again
          :else (conclude! c :need)))
      :else (if-let [s (spot c dry)] (await (dig! c s)) (conclude! c :no-spot)))))

(defn ^:async round
  "The whole attempt: loop the steps until no cell is dry or nothing more can be poured; :continue only while a child waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
