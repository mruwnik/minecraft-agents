(ns jobs.survival.get-food.animals
  "The food animals get-food may hunt: which are in sight and why one is spared, and what a kill says."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.foods :as foods]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]))

(def food-animals #{"cow" "pig" "sheep" "chicken" "rabbit"})

(defn in-sight
  "The food animals the entity scan lists within radius, nearest first, as {:entity e :why r :adults n :rule k}: r is nil for one
  to hunt, else :tried (attacked this job), :baby, :refused (another's zone, claim or plan), or what jobs.lib.animal-spare/why
  says (:off-limits, :pen: a zone or plan of its own or a closed enclosure, :kept: the last :keep-adults of a kind are lifted only at
  lift food or less with no food carried); n counts the adults of its kind in sight and k is the spare rule that allowed the hunt.
  own? (the :pen way): an animal in a zone or plan of its own is no pen, and its rule is :own-pen."
  [c {:keys [radius lift own?]}]
  (let [skipped (set (:skipped-animals (ctx/mem c)))
        seen (look/seen-objs (:primitives c) {:radius radius :kind "passive" :names food-animals :max 16})
        adults (frequencies (map #(.-name %) (remove #(true? (.-baby %)) seen)))
        self (.self (:primitives c))
        pens (atom [])
        starved? (and (<= (.-food self) lift) (not (foods/carries-food? self)))]
    (->> seen
         (mapv (fn [e]
                 (let [n (adults (.-name e))
                       mine? (and own? (access/in-own-area? c (u/pos-of (.-pos e))))
                       penned? (and (not mine?) (not (skipped (.-id e))) (not (true? (.-baby e)))
                                    (spare/penned? c pens e))
                       why (cond (skipped (.-id e)) :tried
                                 (true? (.-baby e)) :baby
                                 (access/trespass-refusal c :take (u/pos-of (.-pos e))) :refused
                                 :else (spare/why (:args c) (.-name e) n penned? starved?))]
                   {:entity e :why why :adults n
                    :rule (when-not why (if mine? :own-pen (spare/allowed-by (:args c) n penned?)))}))))))

(defn nearest
  "The nearest adult food animal in-sight with opts finds to hunt, as an in-sight entry."
  [c opts]
  (->> (in-sight c opts) (remove :why) first))

(defn ^:async nearest-or-look
  "The nearest food animal in sight (an in-sight entry); with none, the body looks around once from where it stands and tries again."
  [c opts]
  (or (nearest c opts)
      (await (look/survey-until! c :cell #(nearest c opts)))))

(defn note-kill!
  "Say which animal the hunt attacked and where, with the adults of its kind counted, the keep threshold, the food level
  and the rule that allowed it, and keep it for the result."
  [c {animal :entity :keys [adults rule]}]
  (let [p (u/pos-of (.-pos animal))
        cell (mapv #(js/Math.floor %) [(:x p) (:y p) (:z p)])
        food (.-food (.self (:primitives c)))
        why {:adults adults :keep (:keep-adults (:args c)) :food food :rule rule}]
    (ctx/update-mem! c update :killed (fnil conj []) (merge {:name (.-name animal) :pos cell} why))
    (ctx/emit! c :food.killed :info (merge {:name (.-name animal) :pos cell
                                            :text (str "hunted a " (.-name animal) " at " (pr-str cell)
                                                       " (" adults " adults, keep " (:keep why) ", food " food ", " (name rule) ")")}
                                           why))))

(defn cell-of
  "The rounded cell [x y z] of a position map or JS object, or nil when the sighting gave none."
  [pos]
  (when pos
    (let [p (if (map? pos) pos (u/pos-of pos))]
      (mapv #(js/Math.round %) [(:x p) (:y p) (:z p)]))))

(defn passed-over
  "The animals in sight that were not hunted, as {:name :id :why :pos}."
  [c opts]
  (->> (in-sight c opts) (filter :why)
       (mapv (fn [{:keys [entity why]}] {:name (.-name entity) :id (.-id entity) :why why :pos (cell-of (.-pos entity))}))))

(defn nearby
  "The mobs of any kind the body sees or heard within radius, as {:name :kind :distance :pos}: tells a hunt filter from
  an empty scan when no animal qualified."
  [c radius]
  (->> (look/seen-entities (:primitives c) {:radius radius :max 16})
       (remove #(#{"item" "player"} (:kind %)))
       (mapv (fn [e] {:name (:name e) :kind (:kind e) :distance (/ (js/Math.round (* 10 (:distance e))) 10)
                     :pos (cell-of (:pos e))}))))
