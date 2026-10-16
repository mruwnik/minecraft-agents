(ns jobs.farm.irrigate
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.farm.find-spot :as find-spot]
            [jobs.farm.till :as till]
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
  the field the body has seen, farmland or ground a hoe would till, is dry when no seen water lies within 4 blocks in x and z
  of it, at its height or one above. For the spot that wets the most dry cells (one of the field's own cells under open air, nearest
  first) it digs the cell (a jobs.blocks.dig child) and pours a water bucket in it (a jobs.blocks.use-on child), then
  looks again; one bucket wets one spot, so it ends when no cell is dry, no water bucket is left or no spot helps. Blocks
  never seen are never counted. A water bucket is fetched (jobs.lib.need, :fetch) when dry cells are left and none is carried; a
  failed fetch ends the call with its ending as the :because. Zones: a spot in another owner's zone or claim, or in a
  plan's footprint (unless :for-plan), is not used, with an irrigate.declined warn once.
  Refused before it starts: impossible {:reason :bad-args :why}; failed conditions {:reason :no-zones} (unless :ignore-zones?)
  and {:reason :need :item \"water_bucket\"} (dry cells, none carried, :fetch false).
  Ends with data {:poured [cells] :dry [cells still dry] :failed [spots given up]}: done when no cell is dry, or the cells
  wetted meet :least (default 1; :all: every dry cell); else condition-failed :need (no bucket), nothing-possible :no-spot.
  done? is ok when no cell is dry after a look and no child is under way; it counts the dry cells left.")

(a/defargs args
  {:from {:doc "box corner (inclusive); with :to, any order" :spec ::a/pos :default nil}
   :to {:doc "opposite box corner (inclusive)" :spec ::a/pos :default nil}
   :center {:doc "centre of a square of cells at its y; with :radius" :spec ::a/pos :default nil}
   :radius {:doc "the square covers |dx|,|dz| <= radius, 0 to 7" :spec (a/int-in 0 7) :default nil}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :spec a/name? :default nil}
   :fetch {:doc "get a water bucket when none is carried (jobs.lib.need): true, a set of kinds or a map of limits; false refuses the call :need" :spec need/option? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

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
  "The seen water as a set of [x y z] near the cells, with the cells poured in this call."
  [c cs]
  (let [me (u/self-pos c)
        reach (min 64 (+ (find-spot/water-reach) 1 (apply max 0 (map #(u/dist me %) cs))))]
    (into (set (map (juxt :x :y :z) (:poured (ctx/mem c))))
          (map (fn [{:keys [x y z]}] [x y z]))
          (map :pos (look/seen-blocks (:primitives c) {:names ["water"] :radius reach :max 4096 :live? true})))))

(defn dry-cells
  "The field cells with no water within reach."
  [c]
  (let [cs (field-cells c)
        ws (waters c cs)]
    (vec (remove (fn [{:keys [x y z]}] (find-spot/hydrated? ws x y z)) cs))))

(defn covers
  "How many of dry a water cell at spot wets."
  [dry spot]
  (let [ws #{[(:x spot) (:y spot) (:z spot)]}]
    (count (filter (fn [{:keys [x y z]}] (find-spot/hydrated? ws x y z)) dry))))

(defn permitted? [c pos]
  (gate/allowed? c :irrigate.declined "irrigate" :dig pos {:except (:for-plan (:args c))}))

(defn spot
  "The field cell to turn into water: the one that wets the most of dry, nearest first; nil when none helps. Only a
  cell with air above, not given up, that the zone rules let the body dig."
  [c dry]
  (let [p (:primitives c)
        me (u/self-pos c)
        failed (:failed (ctx/mem c) #{})
        open (->> (field-cells c)
                  (remove failed)
                  (filter #(= "air" (u/seen-name p (update % :y inc))))
                  (filter #(permitted? c %)))]
    (->> open
         (keep (fn [pos] (let [n (covers dry pos)] (when (pos? n) [n pos]))))
         (sort-by (fn [[n pos]] [(- n) (u/dist me pos)]))
         first
         second)))

(defn has-bucket? [p] (boolean (some #(= "water_bucket" (:name %)) (u/inventory p))))

(defn fetch-opts
  "The limits {:how :explore} for fetching a water bucket, nil when :fetch does not allow items."
  [c]
  (when-let [o (need/opts c 'jobs.farm.irrigate)]
    (when (contains? (:what o) :item) (select-keys o [:how :explore]))))

(defn needs-bucket?
  "Whether a bucket is to be got: dry cells, none carried and no pour waiting."
  [c]
  (and (not (has-bucket? (:primitives c))) (nil? (:hole (ctx/mem c))) (seq (dry-cells c))))

(defn no-bucket-source [c]
  (when (and (needs-bucket? c) (nil? (fetch-opts c))) {:reason :need :item "water_bucket"}))

(def can-start? (cond/and till/bad-args
                          (checks/zones-read :irrigate.declined "irrigate")
                          (checks/refusing :bucket "water bucket carried or fetchable" no-bucket-source)))

(defn result-of [c]
  (let [m (ctx/mem c)]
    {:poured (vec (:poured m)) :dry (dry-cells c) :failed (vec (:failed m))}))

(defn done?
  [c]
  (let [dry (count (dry-cells c))
        data [:units :cells :done (count (:poured (ctx/mem c))) :left dry :result (result-of c)]]
    (cond
      (pos? dry) (apply contract/later :dry (str "irrigate: " dry " cells dry") data)
      (not (look/surveyed? c)) (apply contract/later :looking "irrigate: no cell dry, looking round" data)
      (checks/child-under-way? c) (apply contract/later :wrapping-up "irrigate: wrapping up" data)
      :else (apply contract/ok :irrigated "irrigate: every cell has water in reach" data))))

(defn quote-of [c]
  (let [n (count (dry-cells c))]
    {:uses {:job-time [(if (pos? n) 5 0) (if (pos? n) 30 0) (if (pos? n) 120 0)] :items {"water_bucket" [0 (min 1 n) 1]}}
     :done [0 n n]}))

(defn conclude!
  "The ending when no more can be poured: reason is why dry cells remain (:need or :no-spot)."
  [c reason]
  (let [{:keys [dry] :as result} (result-of c)
        dry0 (:dry0 (ctx/mem c) (count dry))
        text (str "irrigated " (count (:poured result)) " spots, " (count dry) " cells dry")
        data (assoc result :text text)]
    (cond
      (or (empty? dry) (common/enough? c (- dry0 (count dry)) dry0)) (ctx/end c :done nil data)
      (= :need reason) (ctx/end c :condition-failed :need data)
      :else (ctx/end c :nothing-possible reason data))))

(defn ^:async pour!
  "Pour the bucket in the dug hole. :again, or :continue while the child waits."
  [c hole]
  (let [e (await (ctx/call c :pour 'jobs.blocks.use-on {:pos hole :item "water_bucket"}))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (ctx/update-mem! c #(-> (dissoc % :hole) (update :poured (fnil conj #{}) hole))) :again)
      :else (do (ctx/update-mem! c #(-> (dissoc % :hole) (update :failed (fnil conj #{}) hole))) :again))))

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
        hole (:hole (ctx/mem c))
        _ (when-not hole (when (empty? (field-cells c)) (await (look/survey! c :cell))))
        dry (when-not hole (dry-cells c))
        _ (when (and dry (not (:dry0 (ctx/mem c)))) (ctx/update-mem! c assoc :dry0 (count dry)))]
    (cond
      hole (if (has-bucket? p) (await (pour! c hole)) (do (ctx/update-mem! c dissoc :hole) :again))
      (empty? dry) (conclude! c nil)
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
