(ns jobs.farm.till
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(def doc
  "Hoe dirt, grass_block or dirt_path into farmland over a set of ground cells,
  given as the box :from/:to or the square :center/:radius (at most 256 cells).
  Water nearby is not its concern. A cell that is not tillable, is covered,
  cannot be reached or refuses the hoe twice is skipped with a reason. Ends with
  a result {:tilled n :skipped {pos reason}}.")

(def args
  {:from {:doc "box corner (inclusive); with :to, any order" :default nil}
   :to {:doc "opposite box corner (inclusive)" :default nil}
   :center {:doc "centre of a square of cells at its y; with :radius" :default nil}
   :radius {:doc "the square covers |dx|,|dz| <= radius" :default nil}})

(def max-cells 256)
(def tillable #{"dirt" "grass_block" "dirt_path"})
(def ground-cover #{"short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "snow"})
(def air #{"air" "cave_air" "void_air"})
(def max-tries 2)

(defn span
  "[lo..hi] inclusive of two numbers in either order."
  [a b]
  (range (min a b) (inc (max a b))))

(defn cells
  "The ground cells [{:x :y :z} ...] of args: the box :from/:to, or the square
  :center/:radius at center's y. Throws ex-info for neither form or over
  max-cells cells."
  [{:keys [from to center radius]}]
  (let [[xs ys zs] (cond
                     (and from to) [(span (:x from) (:x to)) (span (:y from) (:y to)) (span (:z from) (:z to))]
                     (and center radius) [(span (- (:x center) radius) (+ (:x center) radius))
                                          [(:y center)]
                                          (span (- (:z center) radius) (+ (:z center) radius))]
                     :else (throw (ex-info "till needs :from and :to, or :center and :radius" {})))
        n (* (count xs) (count ys) (count zs))]
    (when (> n max-cells)
      (throw (ex-info (str "till covers " n " cells, at most " max-cells) {:cells n})))
    (vec (for [x xs y ys z zs] {:x x :y y :z z}))))

(defn hoe-of
  "The name of a carried item ending in _hoe, or nil."
  [p]
  (some #(when (.endsWith (:name %) "_hoe") (:name %)) (u/inventory p)))

(defn pending
  "[[pos block-name-or-nil] ...] of the cells not skipped and not farmland."
  [c]
  (let [skipped (:skipped (ctx/mem c) {})]
    (->> (cells (:args c))
         (remove #(contains? skipped %))
         (map (fn [pos] [pos (u/block-name (:primitives c) pos)]))
         (remove (fn [[_ n]] (= "farmland" n)))
         vec)))

(defn check
  "True when nothing is pending (the round can finish), false without a hoe.
  Bad args pass, so the round throws them. Walks the cells lazily and stops at
  the first one that needs work, reading each cell at most once."
  [c]
  (let [skipped (:skipped (ctx/mem c) {})
        p (:primitives c)
        todo? (try (some #(not (or (contains? skipped %) (= "farmland" (u/block-name p %))))
                         (cells (:args c)))
                   (catch :default _ nil))]
    (or (nil? todo?) (some? (hoe-of p)))))

(defn skip!
  "Record the cells as skipped with reason and emit one :till.skipped each."
  [c poss reason]
  (ctx/update-mem! c update :skipped #(into (or % {}) (map (fn [p] [p reason])) poss))
  (doseq [pos poss]
    (ctx/emit! c :till.skipped :info {:pos pos :reason reason
                                      :text (str "skipped " (pr-str pos) ": " (name reason))})))

(defn bump!
  "Count a failed try on pos; at max-tries skip it with reason."
  [c pos reason]
  (let [n (inc (get-in (ctx/mem c) [:tries pos] 0))]
    (if (>= n max-tries)
      (skip! c [pos] reason)
      (ctx/update-mem! c assoc-in [:tries pos] n))))

(defn nearest
  "The [pos name] of todo nearest the body."
  [c todo]
  (let [me (u/self-pos c)]
    (apply min-key #(u/dist me (first %)) todo)))

(defn ^:async round
  "One bounded step: skip what cannot be tilled, else walk to the nearest cell,
  clear ground cover above it or use the hoe on it."
  [c]
  (let [p (:primitives c)
        todo (pending c)
        bad (into [] (comp (filter (fn [[_ n]] (and (some? n) (not (tillable n))))) (map first)) todo)]
    (when (seq bad) (skip! c bad :not-tillable))
    (let [bad? (set bad)
          cands (remove (fn [[pos _]] (bad? pos)) todo)
          hoe (hoe-of p)]
      (cond
        (empty? cands)
        (let [mem (ctx/mem c)
              tilled (count (:tilled mem))
              skipped (:skipped mem {})]
          (ctx/emit! c :till.done :info {:tilled tilled :skipped (count skipped)
                                         :text (str "tilled " tilled ", skipped " (count skipped))})
          (ctx/result! c {:tilled tilled :skipped skipped})
          :done)

        (nil? hoe) :continue

        :else
        (let [[target _] (nearest c cands)
              w (await (u/walk-near! c target 3))]
          (case w
            :partial :continue
            :blocked (do (bump! c target :unreachable) :continue)
            (let [above-pos (update target :y inc)
                  above (u/block-name p above-pos)]
              (cond
                (ground-cover above)
                (let [d (await (ctx/act c :dig (clj->js {:pos above-pos})))]
                  (when (not= "dug" (.-status d)) (bump! c target :cover-stuck))
                  :continue)

                (not (or (nil? above) (air above)))
                (do (skip! c [target] :covered) :continue)

                :else
                (let [r (await (ctx/act c :useOn #js {:pos (clj->js target) :item hoe :face "up"}))
                      status (.-status r)]
                  (cond
                    (and (= "used" status) (= "farmland" (some-> (.-after r) .-name)))
                    (ctx/update-mem! c #(-> % (update :tilled (fnil conj #{}) target) (update :tries dissoc target)))

                    (= "missing" status) (skip! c [target] :gone)
                    (= "no-item" status) nil
                    :else (bump! c target :refused))
                  :continue)))))))))
