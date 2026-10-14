(ns jobs.survival.dig-in.mode
  "dig-in's choice of mode (plug, walls or pit), once per call, and the rounds of a site that has no mode to run."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.choose :as choose]
            [jobs.lib.dig-look :as look]
            [jobs.lib.seal :as seal]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter :as sh]
            [jobs.lib.solid :as solid]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-in.site :as site]))

(a/defargs settings
  {::side-mob-radius {:default 8 :doc "Blocks out to which a hostile in a side column rules it out." :spec (a/int-in 0 nil)}})

(defn mode-choice
  "[mode refusal] for the shelter from start: the first of :plug (only with a room-plug cell), :walls (only when
  walls-ok?) and :dig whose cells are all permitted, else the first of them with its refusal (nil when permitted).
  dig-plan is the pit's {:roof :depth} (and :cells, the cells it digs, when they are not the column under start), nil when it cannot be roofed (the start cell's rules are then checked)."
  [c start walls-cells walls-ok? dig-plan plug]
  (let [in (access/rules-input c)
        {:keys [roof depth cells]} (or dig-plan {:roof start :depth 2})
        walls-v (some #(access/trespass-refusal in :place %) walls-cells)
        dig-v (or (some #(access/trespass-refusal in :dig %) (or cells (map #(update start :y - %) (range 1 (inc depth)))))
                  (access/trespass-refusal (assoc in :feet nil) :place roof))
        plug-v (when plug (access/trespass-refusal (assoc in :feet nil) :place plug))
        options (cond-> [] plug (conj [:plug plug-v]) walls-ok? (conj [:walls walls-v]) :always (conj [:dig dig-v]))]
    (or (first (filter (comp nil? second) options)) (first options))))

(defn refused-column
  "The straight pit under start when the body sees a hazard in the first cell to dig or not solid under it: kept, so the
  descent refuses it with its reason (a thin floor, water) rather than a zigzag hiding that. A bad cell deeper down
  leaves the pit to the zigzag, which never digs under the feet onto a floor it has not seen. nil otherwise."
  [p start {:keys [depth]}]
  (let [shape (dig-cells/pit-steps start nil depth)
        bad? (fn [cell] (let [n (u/seen-name p cell)] (and n (or (not (solid/solid? n)) (dig-cells/wet? p cell)))))]
    (when (some bad? (let [{:keys [dig floor]} (first (:steps shape))] (conj dig floor)))
      shape)))

(defn side-mob-radius [] (settings/get settings ::side-mob-radius))

(defn side-ok?
  "Whether the side column at feet height (cell and the one over it) is seen open, dry and free of hostiles."
  [p cell]
  (let [mobs (seal/hostile-cells p (side-mob-radius))]
    (every? #(and (dig-cells/open-cell? p %) (not (mobs %))) [cell (update cell :y inc)])))

(defn pit-cells
  "The cells the pit of dig-plan digs from start (its :cells, else the column under start for its :depth)."
  [start dig-plan]
  (let [{:keys [depth cells]} (or dig-plan {:depth 2})]
    (or cells (map #(update start :y - %) (range 1 (inc depth))))))

(defn spared-cell
  "The first cell of the pit that lies in a plan the body made (access/own-keep), as its verdict, else nil."
  [c start dig-plan]
  (let [in (access/rules-input c {:spare-own? true})]
    (some #(access/own-keep in %) (pit-cells start dig-plan))))

(defn ^:async find-shape!
  "The pit to dig from start (refused-column, else the first of dig-cells/pit-shapes), or nil. With none, the body looks
  at the side column cells it has not sensed (look/look-unknown!) and tries once more."
  [c start plan]
  (let [p (:primitives c)
        find #(or (refused-column p start plan) (first (dig-cells/pit-shapes p start (partial side-ok? p))))]
    (or (find)
        (do (await (look/look-unknown! c (for [[dx dz] dig-cells/sides, dy [0 1]]
                                           [(+ (:x start) dx) (+ (:y start) dy) (+ (:z start) dz)])))
            (find)))))

(defn ^:async choose-mode
  "Record in job memory how this shelter is built, once. Chosen again only when the body leaves a dig-mode column.
  :walls stores :mode, and :start at the shaft top when the body is at the bottom of a 1x1 shaft.
  :dig stores :roof, :start and :target-y. :plug stores the one cell to fill.
  See mode-choice for the zone rule."
  [c]
  (let [p (:primitives c)
        {:keys [mode roof]} (ctx/mem c)
        {:keys [x z] :as start} (sh/feet p)
        moved (and (= :dig mode) (not (contains? (:cols (ctx/mem c)) [x z])))]
    (when moved (ctx/update-mem! c dissoc :mode :roof :target-y :shape :cols :pit-drops))
    (when (or moved (not mode))
      (let [cells (dig-cells/open-cells p start)
            have (reduce + (map :count (lb/carried c (:blocks (:args c)))))
            cheap (reduce + (map :count (lb/carried c (choose/cheap-blocks (:blocks (:args c)) (:wall-value (:args c))))))
            enclose (:enclose (:args c))
            plan (when-not enclose (await (sense/decide! c ::dig-plan #(dig-cells/dig-plan p start))))
            shape (when plan (await (find-shape! c start plan)))
            plan (if shape
                   (assoc plan :roof (:roof shape) :cells (mapcat :dig (:steps shape)))
                   plan)
            plug (when (and (pos? have) (not enclose)) (dig-cells/room-plug p start (:roof-height (:args c))))
            spared (when (and shape (not enclose)) (spared-cell c start plan))
            [chosen refusal] (if enclose
                               [(if (>= have (count cells)) :walls :no-blocks)
                                (some #(access/trespass-refusal (access/rules-input c) :place %) cells)]
                               (mode-choice c start cells (or (>= cheap (count cells)) (and (nil? shape) (>= have (count cells)))) plan plug))]
        (access/trespass! c "dig-in" refusal)
        (cond
          (= :no-blocks chosen) (ctx/update-mem! c assoc :mode :no-blocks)
          (= :plug chosen) (ctx/update-mem! c assoc :mode :plug :plug plug)
          (= :walls chosen) (ctx/update-mem! c #(cond-> (assoc % :mode :walls)
                                                  (dig-cells/shaft-top p start) (assoc :start {:x x :y (dig-cells/shaft-top p start) :z z})))
          (nil? plan) (ctx/update-mem! c assoc :mode :no-roof-support)
          (nil? shape) (ctx/update-mem! c assoc :mode :no-floor)
          spared (ctx/update-mem! c assoc :mode :spared :spared spared)
          :else (ctx/update-mem! c assoc :mode :dig :roof (:roof shape) :start start :shape shape
                                 :cols (into #{[x z]} (map (comp (juxt :x :z) :to) (:steps shape)))
                                 :target-y (:y (:to (peek (:steps shape))))))))))

(defn no-roof-round
  "No cell of the pit could be roofed (nothing solid beside the start cell or the ground cell under it): do not dig,
  since the body would be left in an open pit."
  [c]
  (site/remember-failed-site! c :no-roof-support)
  (ctx/emit! c :dig_in_failed :warn {:text "nothing solid beside the roof cell to place against; not digging a pit"})
  :done)

(defn no-floor-round
  "No pit can be dug whose floor the body sees or knows solid (straight down needs every floor known, a zigzag a seen
  open side column): do not dig, since the drop would be onto an unseen cell."
  [c]
  (site/remember-failed-site! c :no-floor)
  (ctx/emit! c :dig_in_failed :warn {:text "no floor the body sees solid under a pit; not digging down"})
  :done)

(defn spared-round
  "The only way left is a pit through the body's own zone or plan: do not dig, so the night shelter looks elsewhere."
  [c]
  (let [{:keys [zone plan cell]} (:spared (ctx/mem c))]
    (site/remember-failed-site! c :spared)
    (ctx/emit! c :dig_in_failed :warn {:cell cell :zone zone :plan plan
                                       :text (str "the pit would cut into the body's own " (if zone (str "zone " zone) (str "plan " plan))
                                                  " at " (pr-str cell) "; not digging")})
    :done))
