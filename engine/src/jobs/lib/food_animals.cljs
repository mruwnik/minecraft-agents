(ns jobs.lib.food-animals
  "The food animals get-food may hunt: which are in sight and why one is spared, and what a kill says."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [engine.notes :as notes]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.foods :as foods]
            [jobs.lib.look :as look]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::note-ttl-s {:default 600 :doc "Seconds a note of a food animal seen lasts: it counts toward the pair a hunt keeps." :spec (a/num-in 0 nil)}
   ::pen-look-range {:default 4 :doc "Blocks from a food animal the body walks to when the cells round it were out of sight and may be a fenced pen." :spec (a/num-in 0 nil)}})

(def food-animals #{"cow" "pig" "sheep" "chicken" "rabbit"})

(defn noted-adults
  "The uuids of adult animals of `kind` that live :seen notes (any body's) place within radius of the body and that are not among `seen-uuids`: a pair standing out of each other's sight is still a pair."
  [c kind seen-uuids radius]
  (let [me (u/self-pos c)]
    (into #{}
          (comp (filter #(and (= :seen (:kind %)) (= kind (:what %)) (:id %) (not (:baby %)) (not (seen-uuids (:id %)))
                              (<= (u/flat-dist me (zipmap [:x :y :z] (:pos %))) radius)))
                (map :id))
          (notes/notes c))))

(defn note-seen!
  "Write the adults and babies of food animals in sight as :seen notes, so a later look at one of a pair counts the other."
  [c {:keys [radius]}]
  (let [seen (look/seen-objs (:primitives c) {:radius radius :kind "passive" :names food-animals :max 16})]
    (when (seq seen)
      (notes/note! c (mapv (fn [e] (let [p (u/pos-of (.-pos e))]
                                     {:kind :seen :what (.-name e) :id (.-uuid e) :baby (true? (.-baby e))
                                      :pos (mapv #(js/Math.floor %) [(:x p) (:y p) (:z p)])
                                      :ttl-ms (* 1000 (settings/get settings ::note-ttl-s))}))
                           seen)))))

(defn in-sight
  "The food animals the entity scan lists within radius, nearest first, as {:entity e :why r :adults n :rule k}: r is nil for one
  to hunt, else :tried (attacked this job), :baby, :refused (another's zone, claim or plan), or what jobs.lib.animal-spare/why
  says (:off-limits, :pen: a zone or plan of its own or a closed enclosure, :kept: the last ::keep-adults of a kind are lifted only at
  lift food or less with no food carried); n counts the adults of its kind in sight and k is the spare rule that allowed the hunt.
  own? (the :pen way): an animal in a zone or plan of its own is no pen, and its rule is :own-pen."
  [c {:keys [radius lift own?]}]
  (let [skipped (set (:skipped-animals (ctx/mem c)))
        seen (look/seen-objs (:primitives c) {:radius radius :kind "passive" :names food-animals :max 16})
        adult-es (remove #(true? (.-baby %)) seen)
        seen-uuids (set (map #(.-uuid %) seen))
        adults (merge-with + (frequencies (map #(.-name %) adult-es))
                           (into {} (map (fn [k] [k (count (noted-adults c k seen-uuids radius))])) (distinct (map #(.-name %) adult-es))))
        self (.self (:primitives c))
        pens (atom [])
        starved? (and (<= (.-food self) lift) (not (foods/carries-food? self)))]
    (->> seen
         (mapv (fn [e]
                 (let [n (adults (.-name e))
                       mine? (and own? (access/in-own-area? c (u/pos-of (.-pos e))))
                       penned? (and (not mine?) (not (skipped (.-id e))) (not (true? (.-baby e)))
                                    (spare/penned? c pens e))
                       refusal (when-not (or (skipped (.-id e)) (true? (.-baby e)))
                                 (access/trespass-refusal c :take (u/pos-of (.-pos e))))
                       why (cond (skipped (.-id e)) :tried
                                 (true? (.-baby e)) :baby
                                 refusal :refused
                                 :else (spare/why (spare/keep-adults) (.-name e) n penned? starved?))]
                   {:entity e :why why :adults n
                    :by (when refusal (access/refusal-text (access/refusal-fields [refusal])))
                    :rule (when-not why (if mine? :own-pen (spare/allowed-by (spare/keep-adults) n penned?)))}))))))

(defn nearest
  "The nearest adult food animal in-sight with opts finds to hunt, as an in-sight entry."
  [c opts]
  (->> (in-sight c opts) (remove :why) first))

(defn ^:async pen-looked
  "The nearest entry to hunt (or :continue while the walk is not done) after the body has looked at the cells of any
  enclosure round the target that a first read left unseen, walking within sight range of it when they lie beyond:
  a fenced pen is no wild surplus, and the spare check reads seen cells only."
  [c opts target]
  (let [{:keys [x y z] :as pos} (u/pos-of (.-pos (:entity target)))
        undecided? (fn ^:async undecided []
                     (= :unloaded (:reason (await (pen-read/read! c {:at {:x (js/Math.floor x) :y (js/Math.floor (+ y 0.01)) :z (js/Math.floor z)}
                                                                     :max-cells (settings/get spare/settings ::spare/pen-max-cells)}
                                                                  #(= :unloaded (:reason %)))))))]
    (if-not (await (undecided?))
      (nearest c opts)
      (let [e (await (child/run!* c :pen-look 'jobs.movement.go-to {:pos pos :range (settings/get settings ::pen-look-range) :ways #{:walk}}))]
        (if (= :continue e)
          :continue
          (do (await (undecided?)) (nearest c opts)))))))

(defn cell-of
  "The rounded cell [x y z] of a position map or JS object, or nil when the sighting gave none."
  [pos]
  (when pos
    (let [p (if (map? pos) pos (u/pos-of pos))]
      (mapv #(js/Math.round %) [(:x p) (:y p) (:z p)]))))

(defn spared-sightings
  "The animals in sight that were not hunted, as {:name :id :why :pos}."
  [c opts]
  (->> (in-sight c opts) (filter :why)
       (mapv (fn [{:keys [entity why by]}] (cond-> {:name (.-name entity) :id (.-id entity) :uuid (.-uuid entity) :why why :pos (cell-of (.-pos entity))}
                                              by (assoc :by by))))))

(defn note-spared!
  "Remember the animals spared now by id: entities are sensed only in the view cone, so a later look at the end of a survey no longer lists them."
  [c opts]
  (let [seen (spared-sightings c opts)]
    (when (seq seen)
      (ctx/update-mem! c update :spared-seen #(merge % (into {} (map (juxt :id identity)) seen))))))

(defn ^:async nearest-or-look
  "The nearest food animal in sight (an in-sight entry), its surroundings looked at first when they may be a pen (see pen-looked); with none, the body looks around once from where it stands and tries again, remembering what it spared on the way."
  [c opts]
  (note-seen! c opts)
  (when-let [target (or (nearest c opts) (await (look/survey-until! c :cell (fn [] (note-spared! c opts) (nearest c opts)))))]
    (if (= :own-pen (:rule target)) target (await (pen-looked c opts target)))))

(defn note-kill!
  "Say which animal the hunt attacked and where, with the adults of its kind counted, the keep threshold, the food level
  and the rule that allowed it, and keep it for the result."
  [c {animal :entity :keys [adults rule]}]
  (let [p (u/pos-of (.-pos animal))
        cell (mapv #(js/Math.floor %) [(:x p) (:y p) (:z p)])
        food (.-food (.self (:primitives c)))
        why {:adults adults :keep (spare/keep-adults) :food food :rule rule}]
    (notes/note! c [{:kind :seen :what (.-name animal) :id (.-uuid animal) :pos cell :ttl-ms 0}])
    (ctx/update-mem! c update :killed (fnil conj []) (merge {:name (.-name animal) :pos cell} why))
    (ctx/emit! c :food.killed :info (merge {:name (.-name animal) :pos cell
                                            :text (str "hunted a " (.-name animal) " at " (pr-str cell)
                                                       " (" adults " adults, keep " (:keep why) ", food " food ", " (name rule) ")")}
                                           why))))

(defn passed-over
  "The animals not hunted, as {:name :id :why :pos}: those in sight now and those spared earlier in a look."
  [c opts]
  (let [now (spared-sightings c opts)
        ids (set (map :id now))]
    (into now (remove #(ids (:id %))) (vals (:spared-seen (ctx/mem c))))))

(defn nearby
  "The mobs of any kind the body sees or heard within radius, as {:name :kind :distance :pos}: tells a hunt filter from
  an empty scan when no animal qualified."
  [c radius]
  (->> (look/seen-entities (:primitives c) {:radius radius :max 16})
       (remove #(#{"item" "player"} (:kind %)))
       (mapv (fn [e] {:name (:name e) :kind (:kind e) :distance (/ (js/Math.round (* 10 (:distance e))) 10)
                     :pos (cell-of (:pos e))}))))
