(ns jobs.build.clear-box
  (:require [engine.ctx :as ctx]
            [engine.jobs.tools :as tools]
            [engine.jobs.util :as u]))

(def doc
  "Dig out a box from the top down (site levelling, demolition). Beds,
  containers and fluids are kept. Ends with a result {:dug n :skipped {pos
  reason} :kept n :fluids {name count}}.")

(def args
  {:from {:doc "box corner (inclusive); any order" :default nil}
   :to {:doc "opposite box corner (inclusive); at most 400 cells" :default nil}
   :keep {:doc "extra block names to leave alone" :default []}})

(def max-cells 400)
(def max-tries 2)
(def air #{"air" "cave_air" "void_air"})
(def fluids #{"water" "lava" "bubble_column"})
(def kept-pattern
  #"(_bed|chest|barrel|furnace|smoker|crafting_table|shulker_box|hopper|dispenser|dropper|brewing_stand|anvil|enchanting_table)$")

(defn span [a b] (range (min a b) (inc (max a b))))

(defn cells
  "The cells [{:x :y :z} ...] of the box :from/:to. Throws ex-info for a
  missing corner or over max-cells cells."
  [{:keys [from to]}]
  (when-not (and from to) (throw (ex-info "clear-box needs :from and :to" {})))
  (let [xs (span (:x from) (:x to)) ys (span (:y from) (:y to)) zs (span (:z from) (:z to))
        n (* (count xs) (count ys) (count zs))]
    (when (> n max-cells)
      (throw (ex-info (str "clear-box covers " n " cells, at most " max-cells) {:cells n})))
    (vec (for [x xs y ys z zs] {:x x :y y :z z}))))

(defn kept? [keep n]
  (or (some? (re-find kept-pattern n)) (boolean (some #{n} keep))))

(defn named
  "[[pos block-name] ...] of the loaded cells not skipped."
  [c]
  (let [skipped (:skipped (ctx/mem c) {})]
    (->> (cells (:args c))
         (remove #(contains? skipped %))
         (keep (fn [pos] (when-let [n (u/block-name (:primitives c) pos)] [pos n]))))))

(defn pending
  "[[pos block-name-or-nil] ...]: the loaded cells to dig, and the unloaded
  ones (nil name) not skipped, which are still to be walked to."
  [c]
  (let [keep (:keep (:args c) [])
        skipped (:skipped (ctx/mem c) {})]
    (->> (cells (:args c))
         (remove #(contains? skipped %))
         (map (fn [pos] [pos (u/block-name (:primitives c) pos)]))
         (remove (fn [[_ n]] (and n (or (air n) (fluids n) (kept? keep n)))))
         vec)))

(defn check [_c] true)

(defn skip! [c pos reason]
  (ctx/update-mem! c update :skipped assoc pos reason)
  (ctx/emit! c :clear-box.skipped :info {:pos pos :reason reason
                                         :text (str "skipped " (pr-str pos) ": " (name reason))}))

(defn bump! [c pos reason]
  (let [n (inc (get-in (ctx/mem c) [:tries pos] 0))]
    (if (>= n max-tries)
      (skip! c pos reason)
      (ctx/update-mem! c assoc-in [:tries pos] n))))

(defn under-foot
  "The set of cells the body stands on or in: the cell below its feet, and its
  feet and head cells."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)
        fx (js/Math.floor x) fy (js/Math.floor y) fz (js/Math.floor z)]
    #{{:x fx :y (dec fy) :z fz} {:x fx :y fy :z fz} {:x fx :y (inc fy) :z fz}}))

(defn target
  "The pending [pos name] at the highest y among the loaded cells not under the
  body's feet, nearest the body; with none loaded, the nearest unloaded one.
  Under-foot cells are picked only when nothing else is pending (nil then)."
  [c todo]
  (let [me (u/self-pos c)
        feet (under-foot c)
        others (remove #(and (second %) (feet (first %))) todo)
        loaded (filter second others)]
    (cond
      (empty? others) nil
      (empty? loaded) (apply min-key #(u/dist me (first %)) others)
      :else (let [top (apply max (map #(:y (first %)) loaded))]
              (apply min-key #(u/dist me (first %)) (filter #(= top (:y (first %))) loaded))))))

(defn ^:async step-off!
  "Only under-foot cells are left: move to just west of the box at the feet
  level. A blocked step bumps the cell the body is standing on."
  [c todo]
  (let [me (u/self-pos c)
        min-x (apply min (map :x (cells (:args c))))
        dest {:x (dec min-x) :y (js/Math.floor (:y me)) :z (js/Math.floor (:z me))}
        r (await (ctx/act c :moveTo (clj->js {:pos dest :range 0})))]
    (when (= "blocked" (.-status r))
      (bump! c (first (apply min-key #(u/dist me (first %)) todo)) :unreachable))
    :continue))

(defn finish! [c]
  (let [mem (ctx/mem c)
        everything (named c)
        keep (:keep (:args c) [])
        kept (count (filter (fn [[_ n]] (and (not (fluids n)) (not (air n)) (kept? keep n))) everything))
        wet (frequencies (filter fluids (map second everything)))
        skipped (:skipped mem {})
        dug (:dug mem 0)]
    (ctx/emit! c :clear-box.done :info {:dug dug :skipped (count skipped)
                                        :text (str "dug " dug ", skipped " (count skipped))})
    (ctx/result! c {:dug dug :skipped skipped :kept kept :fluids wet})
    :done))

(defn ^:async round
  "One bounded step: walk to the highest nearest pending cell, equip a tool, dig."
  [c]
  (let [p (:primitives c)
        todo (pending c)]
    (if (empty? todo)
      (finish! c)
      (if-let [[pos n] (target c todo)]
        (let [w (await (u/walk-near! c pos 3))]
        (case w
          :partial :continue
          :blocked (do (bump! c pos :unreachable) :continue)
          (if (nil? n)
            :continue
            (let [tool (tools/best-tool (map :name (u/inventory p)) n)]
            (when (and tool (not= tool (.-held (.self p))))
              (await (ctx/act c :equip #js {:item tool :dest "hand"})))
            (let [status (.-status (await (ctx/act c :dig (clj->js {:pos pos}))))]
              (case status
                ("dug" "missing") (ctx/update-mem! c #(cond-> (update % :tries dissoc pos)
                                                        (= "dug" status) (update :dug (fnil inc 0))))
                "cannot" (skip! c pos :cannot)
                (bump! c pos :refused)))
            :continue))))
        (await (step-off! c todo))))))
