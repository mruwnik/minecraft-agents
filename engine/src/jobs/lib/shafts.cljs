(ns jobs.lib.shafts
  "Mineshafts (plans of :kind :mineshaft with a :stair part, see plan.shape): finding one a body may use, and the
  compact memory of the branches it has dug off it.

  Lookup is a social rule, not a safety rule: a shaft is shared (any body may dig through it), but a shaft whose top cell
  lies in another's zone or claim, or in another plan's footprint, is not used. The caller can override the rule
  (:foreign-zone? true). The shaft plan's own footprint is no obstacle.

  Branch memory: body memory kind :mine/shafts, one entry {shaft-id {:branches [{:from [x y z] :heading h :length n}]}}.
  A branch is a point, a heading and a length (a point implying a line), never a list of blocks, and only the newest
  max-branches of a shaft are kept, so the entry stays small however long the tunnels are."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]
            [plan.shape :as shape]))

(a/defargs settings
  {::shaft-radius {:default 128 :doc "Blocks from the start within which a mineshaft's top is looked for." :spec (a/num-in 0 nil)}
   ::max-branches {:default 24 :doc "Branches remembered per shaft; the oldest are forgotten first." :spec (a/int-in 1 nil)}
   ::branch-spacing {:default 3 :doc "Cells between two side branches along the first run, so each opens new rock." :spec (a/int-in 1 nil)}})
(defn shaft-radius [] (settings/get settings ::shaft-radius))
(defn max-branches [] (settings/get settings ::max-branches))
(defn branch-spacing [] (settings/get settings ::branch-spacing))

(def kind :mine/shafts)
(def policy {:cap 1 :ttl :forever})

;; ------------------------------------------------------------------ geometry

(defn shaft-of
  "{:id :from :heading :to-y :top :bottom} of the first :stair part of a mineshaft plan, or nil. :top is the feet cell of
  the top step, :bottom the feet cell after the last step."
  [{:keys [id parts]}]
  (when-let [{:keys [from heading to-y] :as stair} (some #(when (and (:stair %) (nil? (shape/stair-error (:stair %)))) (:stair %)) parts)]
    (let [[dx dy dz] (shape/delta :down heading)
          n (shape/stair-steps stair)]
      {:id id :from from :top from :heading heading :to-y to-y
       :bottom (shape/add from [(* n dx) (* n dy) (* n dz)])})))

(def turns {:north {:left :west :right :east} :south {:left :east :right :west}
            :east {:left :north :right :south} :west {:left :south :right :north}})

(defn next-branch
  "The branch the next run should dig off shaft (shaft-of), given the branches {:from :heading :length} already recorded:
  the first runs straight on along the shaft's heading from its bottom; branch k >= 1 leaves the first run sideways, left
  for odd k and right for even, every spacing cells along it (k = 1, 2 at spacing, 3 and 4 at twice that ...). The
  answer is {:from :heading}, or nil when the first run is too short to carry that branch (the shaft is used up)."
  [{:keys [bottom heading]} branches spacing]
  (let [k (count branches)]
    (if (zero? k)
      {:from bottom :heading heading}
      (let [along (* spacing (quot (inc k) 2))
            run (:length (first branches) 0)
            [dx _ dz] (shape/delta :down heading)]
        (when (<= along run)
          {:from (shape/add bottom [(* along dx) 0 (* along dz)])
           :heading (get-in turns [heading (if (odd? k) :left :right)])})))))

(defn with-branch
  "The memory map {shaft-id {:branches [...]}} with branch appended to shaft id, keeping the newest max-n."
  [shafts id branch max-n]
  (update-in (or shafts {}) [id :branches] #(vec (take-last max-n (conj (vec %) (select-keys branch [:from :heading :length]))))))

;; ------------------------------------------------------------------ memory

(defn shafts-memory [c] (or (:data (ctx/latest c kind)) {}))

(defn branches [c id] (get-in (shafts-memory c) [id :branches] []))

(defn record-branch!
  "Remember that a run of branch {:from :heading :length} was dug off shaft id."
  [c id branch]
  (ctx/remember! c kind (with-branch (shafts-memory c) id branch (max-branches)) policy))

(defn next-for
  "next-branch for shaft (shaft-of) over what this body has recorded."
  [c shaft]
  (next-branch shaft (branches c (:id shaft)) (branch-spacing)))

;; ------------------------------------------------------------------ lookup

(defn verdict-at
  "The social verdict for a :dig at the top cell of shaft, with the shaft's own footprint left out and its permission
  not used (the plan is no excuse for a foreign zone)."
  [c {:keys [id top]}]
  (zones/verdict (assoc (select-keys (access/zone-input c {:except id}) [:zones :footprints :shafts :claims :self :now])
                        :plan-cells #{} :action :dig :cell top)))

(defn lookup
  "The mineshafts near start ({:x :y :z}): {:usable [shaft ...] :refused [{:shaft :verdict} ...]}, each nearest first,
  a shaft being shaft-of plus :dist. opts: :radius (default shaft-radius), :foreign-zone? true to use a shaft the zone
  rules refuse (the social rule is a default, not a safety rule)."
  [c start {:keys [radius foreign-zone?]}]
  (let [radius (or radius (shaft-radius))
        near (->> (known/shafts c)
                  (keep #(some-> (known/plan c %) :plan shaft-of))
                  (map #(assoc % :dist (u/dist start (zipmap [:x :y :z] (:top %)))))
                  (filter #(<= (:dist %) radius))
                  (sort-by :dist))
        judged (map (fn [s] [s (verdict-at c s)]) near)]
    {:usable (vec (for [[s v] judged :when (or foreign-zone? (:ok v))] s))
     :refused (vec (for [[s v] judged :when (not (or foreign-zone? (:ok v)))] {:shaft s :verdict v}))}))
