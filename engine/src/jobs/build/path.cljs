(ns jobs.build.path
  (:require [engine.args :as a]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.gate :as gate]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.toll-cells :as tc]
            [jobs.lib.world :as known]))

(def doc
  "Shovel grass_block, dirt, coarse_dirt, podzol, mycelium or rooted_dirt into dirt_path over a set of ground cells,
  given as the box :from/:to or the square :center/:radius (at most 256 cells).
  Needs a shovel: without one it fetches one (jobs.lib.fetch, child :fetch) while cells wait; with :fetch false, or
  after a failed fetch, the check waits :no-tool (kind shovel) and cells left without a shovel are skipped :no-shovel.
  Ground cover over a cell (grass, ferns, snow layer) is dug first with a jobs.blocks.dig child. If that child
  declines or gives up, it counts a try.
  Cells are skipped with a reason: :not-pathable, :covered (something else above), :not-permitted (zone rules),
  :unreachable, :gone, :refused (the shovel failed twice), :no-shovel or :cover-stuck (two failed cover digs). A cell
  that already is a path is left alone and not counted.
  Zones and the footprints of plans are checked when a cell is chosen and again before the shovel or cover dig.
  The job declines while no zone list has been read, unless :ignore-zones? is true, and before its first round when
  every pathable cell is refused (wait :refused); cells refused later are skipped :not-permitted.
  Bad cell args wait :bad-args with a :why.
  Result: {:pathed n :skipped {pos reason}}; with none pathed and cells skipped it is {:status :stopped :reason r}, r the
  one skip reason or :nothing-pathed when mixed.")

(a/defargs args
  {:from {:doc "box corner (inclusive); with :to, any order" :spec ::a/pos :default nil}
   :to {:doc "opposite box corner (inclusive)" :spec ::a/pos :default nil}
   :center {:doc "centre of a square of cells at its y; with :radius" :spec ::a/pos :default nil}
   :radius {:doc "the square covers |dx|,|dz| <= radius, 0 to 7" :spec (a/int-in 0 7) :default nil}
   :fetch {:doc "get a shovel when none is carried (jobs.lib.fetch): true, a set of kinds or a map of limits; false waits :no-tool" :spec fetch/option? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(def max-cells 256)
(def pathable #{"grass_block" "dirt" "coarse_dirt" "podzol" "mycelium" "rooted_dirt"})
(def ground-cover #{"short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "snow"})
(def air #{"air" "cave_air" "void_air"})
(def max-tries 2)
(def no-shovel {:reason :no-tool :kind "shovel"})

(defn span [a b] (range (min a b) (inc (max a b))))
(defn span-size [a b] (inc (js/Math.abs (- a b))))

(defn cells
  "The ground cells [{:x :y :z} ...] of args: the box :from/:to, or the square :center/:radius at center's y. Throws
  ex-info for neither form or over max-cells cells."
  [{:keys [from to center radius]}]
  (let [[xa xb ya yb za zb] (cond
                              (and from to) [(:x from) (:x to) (:y from) (:y to) (:z from) (:z to)]
                              (and center radius) [(- (:x center) radius) (+ (:x center) radius) (:y center) (:y center)
                                                   (- (:z center) radius) (+ (:z center) radius)]
                              :else (throw (ex-info "path needs :from and :to, or :center and :radius" {})))
        n (* (span-size xa xb) (span-size ya yb) (span-size za zb))]
    (when (> n max-cells)
      (throw (ex-info (str "path covers " n " cells, at most " max-cells) {:cells n})))
    (vec (for [x (span xa xb) y (span ya yb) z (span za zb)] {:x x :y y :z z}))))

(defn shovel-of
  "The name of a carried item ending in _shovel, or nil."
  [p]
  (some #(when (.endsWith (:name %) "_shovel") (:name %)) (u/inventory p)))

(defn pending
  "[[pos block-name-or-nil] ...] of the cells not skipped and not already a path."
  [c]
  (let [skipped (:skipped (ctx/mem c) {})]
    (->> (cells (:args c))
         (remove #(contains? skipped %))
         (map (fn [pos] [pos (u/seen-name (:primitives c) pos)]))
         (remove (fn [[_ n]] (= "dirt_path" n)))
         vec)))

(defn permitted?
  "Whether action at pos passes the zone rules and the plans' footprints."
  [c action pos]
  (gate/allowed? c :path.declined "path" action pos {}))

(defn started? [c]
  (boolean (some #(contains? (ctx/mem c) %) [:pathed :skipped :tries])))

(defn refused-all
  "The refusals when every pathable pending cell is refused by a zone, claim or plan, else nil."
  [c]
  (let [in (access/zone-input c {})
        verdicts (into [] (keep (fn [[pos n]]
                                  (when (or (nil? n) (pathable n))
                                    (let [v (rules/social-verdict :dig (assoc in :cell (access/cell pos)))]
                                      (if (gate/refused? v) v ::open)))))
                       (pending c))]
    (when (and (seq verdicts) (every? #(not= ::open %) verdicts)) verdicts)))

(defn check
  "True when nothing is pending (the round can finish) or a shovel is carried (fetched first, if any); a wait without
  a shovel; declines while no zone list has been read (unless :ignore-zones?) and, before the first round, when every
  pathable cell is refused."
  [c]
  (let [cs (try (cells (:args c)) (catch :default e (ex-message e)))]
    (if (string? cs)
      (ctx/wait c {:reason :bad-args :why cs})
      (and (or (:ignore-zones? (:args c)) (some? (known/zones c))
               (access/decline! c :path.declined "path" {:reason :no-zones}))
           (if-let [vs (and (not (started? c)) (not (:ignore-zones? (:args c))) (refused-all c))]
             (access/decline! c :path.declined "path" (assoc (access/refusal-fields vs) :reason :refused))
             (or (empty? (pending c)) (some? (shovel-of (:primitives c))) (fetch/check c 'jobs.build.path no-shovel)))))))

(defn problem
  "The :no-tool wait for a shovel while none is carried and cells are left to path, else nil."
  [c]
  (when (and (nil? (shovel-of (:primitives c))) (seq (pending c)))
    no-shovel))

(defn skip! [c poss reason]
  (ctx/update-mem! c update :skipped #(into (or % {}) (map (fn [p] [p reason])) poss))
  (doseq [pos poss]
    (ctx/emit! c :path.skipped :info {:pos pos :reason reason
                                      :text (str "skipped " (pr-str pos) ": " (name reason))})))

(defn bump!
  "Count a failed try on pos; at max-tries skip it with reason."
  [c pos reason]
  (let [n (inc (get-in (ctx/mem c) [:tries pos] 0))]
    (if (>= n max-tries)
      (skip! c [pos] reason)
      (ctx/update-mem! c assoc-in [:tries pos] n))))

(defn cover-args
  "The jobs.blocks.dig args for the ground cover at pos: no tool needed, the drop left."
  [c pos]
  (merge (select-keys (:args c) [:ignore-zones?])
         {:pos pos :collect false :need-drop false :accept #{:fluid-adjacent :falling-block :under-feet}}))

(defn nothing-pathed
  "Stopped outcome for a run that pathed nothing: the one skip reason, or :nothing-pathed when they are mixed."
  [skipped]
  (let [counts (frequencies (vals skipped))
        reason (if (= 1 (count counts)) (key (first counts)) :nothing-pathed)
        text (str/join ", " (map (fn [[r n]] (str n " " (name r))) (sort-by (comp name key) counts)))]
    {:status :stopped :reason reason
     :text (if (= :no-shovel reason) "no shovel, nothing pathed" (str "nothing pathed: " text))}))

(defn finish! [c]
  (let [mem (ctx/mem c)
        pathed (count (:pathed mem))
        skipped (:skipped mem {})]
    (ctx/emit! c :path.done :info {:pathed pathed :skipped (count skipped)
                                   :text (str "pathed " pathed ", skipped " (count skipped))})
    (ctx/result! c (cond-> {:pathed pathed :skipped skipped}
                     (and (zero? pathed) (seq skipped)) (merge (nothing-pathed skipped))))
    :done))

(defn ^:async shovel!
  "Use the shovel on target and book the outcome."
  [c target shovel]
  (let [r (await (ctx/act c :useOn #js {:pos (clj->js target) :item shovel :face "up"}))
        status (.-status r)]
    (cond
      (and (= "used" status) (= "dirt_path" (some-> (.-after r) .-name)))
      (ctx/update-mem! c #(-> % (update :pathed (fnil conj #{}) target) (update :tries dissoc target)))

      (= "missing" status) (skip! c [target] :gone)
      (= "no-item" status) (bump! c target :no-shovel)
      :else (bump! c target :refused))
    :again))

(defn ^:async work!
  "Walk to target and clear the cover above it, or shovel it."
  [c target shovel]
  (let [p (:primitives c)
        w (await (near/go-near! c target 3 {:tolls (tc/walk-tolls c (near/cell-of target))}))]
    (if (= :partial w)
      :continue
      (let [above-pos (update target :y inc)
            above (u/seen-name p above-pos)]
        (cond
          (= :blocked w) (do (bump! c target :unreachable) :again)
          (not (permitted? c :dig target)) (do (skip! c [target] :not-permitted) :again)
          (and (ground-cover above) (not (permitted? c :dig above-pos))) (do (skip! c [target] :not-permitted) :again)

          (ground-cover above)
          (let [r (await (ctx/call-child c :cover 'jobs.blocks.dig (cover-args c above-pos)))]
            (when (or (= :declined r)
                      (and (= :done r) (not (#{:dug :already-clear} (:reason (ctx/child-result c :cover))))))
              (bump! c target :cover-stuck))
            :again)

          (not (or (nil? above) (air above))) (do (skip! c [target] :covered) :again)
          :else (await (shovel! c target shovel)))))))

(defn ^:async path-step!
  "One step: skip what cannot be pathed, else work the cell nearest the body."
  [c]
  (let [todo (pending c)
        bad (into [] (comp (filter (fn [[_ n]] (and (some? n) (not (pathable n))))) (map first)) todo)]
    (when (seq bad) (skip! c bad :not-pathable))
    (let [unpermitted (into [] (comp (map first) (remove (set bad)) (remove #(permitted? c :dig %))) todo)
          _ (when (seq unpermitted) (skip! c unpermitted :not-permitted))
          bad? (into (set bad) unpermitted)
          cands (remove (fn [[pos _]] (bad? pos)) todo)
          shovel (shovel-of (:primitives c))
          me (u/self-pos c)]
      (cond
        (empty? cands) (finish! c)
        (nil? shovel) (do (skip! c (map first cands) :no-shovel) :again)
        :else (await (work! c (first (apply min-key #(u/dist me (first %)) cands)) shovel))))))

(defn ^:async round
  "The whole attempt: fetch a shovel when none is carried, then loop the steps until the cells are done; :continue only
  while a walk or fetch waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s []
                          (or (await (fetch/fetch! c 'jobs.build.path problem))
                              (await (path-step! c)))))))
