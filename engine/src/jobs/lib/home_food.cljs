(ns jobs.lib.home-food
  "The food a body has at home, from what it sensed: chests it booked food in (memory :fetch/stock) that zones and
  claims let it take from, ripe crops it saw in its own zones and plans, and the surplus of the food animals it saw or
  noted there (animal-spare's keep rule: the breeders stay). (sources c) [{:kind :pos :points :walk-s}];
  (carry-points c) what the body must carry (jobs.lib.cost.food/carry-points over them)."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.notes :as notes]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.food :as food]
            [jobs.lib.crops :as crops]
            [jobs.lib.foods :as foods]
            [jobs.lib.look :as look]
            [jobs.lib.stock :as stock]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::radius {:default 256 :doc "Blocks within which food at home counts toward the carried reserve; farther it is out of a trip's reach." :spec (a/num-in 0 nil)}})

(def crop-food
  "Per field crop: the food one ripe crop gives (at least one item, of the dropped item; wheat as a third of a loaf)."
  {"wheat" ["bread" (/ 1 3)] "carrots" ["carrot" 1] "potatoes" ["potato" 1] "beetroots" ["beetroot" 1]})

(def meat-of (into {} (map (fn [[m animal]] [animal m])) game/meat-animal))

(defn item-points
  "Hunger points count of name give: reserve food at its points, wheat as loaves."
  [name count]
  (cond (food/reserve-food? name) (* count (foods/points name))
        (= "wheat" name) (* (quot count 3) (foods/points "bread"))
        :else 0))

(defn chest-sources [c]
  (for [[cell items] (stock/stock-of (ctx/view c))
        :let [pos (zipmap [:x :y :z] cell)
              points (reduce + 0 (map (fn [[n k]] (item-points n k)) items))]
        :when (and (pos? points) (not (access/container-refusal c :take pos)))]
    {:kind :chest :pos pos :points points}))

(defn crop-sources [c]
  (for [{:keys [name pos age]} (crops/seen-crops (:primitives c) (keys crop-food) 64 256)
        :when (and age (>= age (crops/ripe-age name)) (access/in-own-area? c pos))
        :let [[item share] (crop-food name)]]
    {:kind :crop :pos pos :points (* share (foods/points item))}))

(defn pen-sources [c]
  (let [p (:primitives c)
        seen (remove #(true? (.-baby %)) (look/seen-objs p {:radius 64 :kind "passive" :names (vec (keys meat-of)) :max 64}))
        ids (set (map #(.-uuid %) seen))
        noted (for [n (notes/notes c)
                    :when (and (= :seen (:kind n)) (meat-of (:what n)) (:id n) (not (:baby n)) (not (ids (:id n)))
                               (not (spare/gone? c n 64 ids)))]
                {:kind (:what n) :pos (zipmap [:x :y :z] (:pos n))})
        own (filter #(access/in-own-area? c (:pos %))
                    (concat (map (fn [e] {:kind (.-name e) :pos (u/pos-of (.-pos e))}) seen) noted))]
    (for [[kind as] (group-by :kind own)
          :let [meat (meat-of kind)
                drops (or (cost/kill-drops kind meat :version (game/version-of p)) cost/expected-drops)
                spare-n (spare/takeable-adults (spare/keep-adults) (count as))]
          :when (pos? spare-n)]
      {:kind :pen :pos (:pos (first as)) :points (* spare-n drops (foods/points meat))})))

(defn sources
  "See the ns doc: within ::radius, each with the seconds of the walk back to it."
  [c]
  (let [here (u/self-pos c)
        per-s (cost/walk-blocks-per-s)]
    (for [s (concat (chest-sources c) (crop-sources c) (pen-sources c))
          :let [d (u/dist here (:pos s))]
          :when (<= d (settings/get settings ::radius))]
      (assoc s :walk-s (/ d per-s)))))

(defn carry-points
  "Food points the body must carry: jobs.lib.cost.food/carry-points of its home food (sources)."
  [c]
  (food/carry-points (sources c)))
