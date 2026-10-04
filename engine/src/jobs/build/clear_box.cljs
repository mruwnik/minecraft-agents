(ns jobs.build.clear-box
  (:require [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.tools :as tools]
            [engine.jobs.util :as u]))

(def doc
  "Dig out a box from the top down (site levelling, demolition). Beds,
  containers and fluids are kept. Ends with a result {:dug n :skipped {pos
  reason} :kept n :fluids {name count}}.
  Zones and plans (engine.access.rules, through engine.jobs.access): each round the loaded cells a zone (one that does
  not allow :dig) or an active plan's footprint refuses are skipped for good (reason :zone or :footprint; one
  clear-box.refused info per round names the zones and plans), and the chosen cell is asked again right before the
  dig. A cell whose dig has a hazard not in :accept counts a try (reason :hazard after two). A box with every pending
  cell refused declines before its first round, with one clear-box.declined warn {:reason :refused :zones :plans};
  no zone list (zones.edn missing or never valid) declines with one clear-box.declined warn {:reason :no-zones}, also
  in the middle of the job.")

(def args
  {:from {:doc "box corner (inclusive); any order" :default nil}
   :to {:doc "opposite box corner (inclusive); at most 400 cells" :default nil}
   :keep {:doc "extra block names to leave alone" :default []}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :accept {:doc "dig hazards of engine.access.rules taken (:fluid-adjacent :falling-block :under-feet)"
            :default #{:fluid-adjacent :falling-block}}})

(def max-cells 400)
(def max-tries 2)
(def air #{"air" "cave_air" "void_air"})
(def fluids #{"water" "lava" "bubble_column"})
(def kept-pattern
  #"(_bed|chest|barrel|furnace|smoker|crafting_table|shulker_box|hopper|dispenser|dropper|brewing_stand|anvil|enchanting_table)$")

(defn span [a b] (range (min a b) (inc (max a b))))

(defn box-error
  "Why the box :from/:to cannot be cleared, or nil."
  [{:keys [from to]}]
  (if-not (and from to)
    "clear-box needs :from and :to"
    (let [n (* (count (span (:x from) (:x to))) (count (span (:y from) (:y to))) (count (span (:z from) (:z to))))]
      (when (> n max-cells)
        (str "clear-box covers " n " cells, at most " max-cells)))))

(defn cells
  "The cells [{:x :y :z} ...] of the box :from/:to. Throws ex-info for a
  missing corner or over max-cells cells."
  [{:keys [from to] :as args}]
  (when-let [e (box-error args)] (throw (ex-info e {})))
  (vec (for [x (span (:x from) (:x to)) y (span (:y from) (:y to)) z (span (:z from) (:z to))] {:x x :y y :z z})))

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

(defn sort-out
  "{:allowed [[pos name] ..] :refused [[pos verdict] ..]}: the pending cells split by the rules' permission (a zone or
  another plan's footprint); unloaded cells are allowed (walked to)."
  [c todo]
  (let [in (access/rules-input c)
        accept (:accept (:args c))
        judged (map (fn [[pos n]] (let [v (when n (access/may-dig? in pos))]
                                    [pos n v (when v (access/judge v accept))]))
                    todo)]
    {:allowed (into [] (comp (remove #(= :refused (nth % 3))) (map (fn [[pos n]] [pos n]))) judged)
     :refused (into [] (comp (filter #(= :refused (nth % 3))) (map (fn [[pos _ v]] [pos v]))) judged)}))

(defn started?
  "Whether a round has left anything in memory (dug, skipped or tried a cell)."
  [c]
  (boolean (some #(contains? (ctx/mem c) %) [:dug :skipped :tries])))

(defn check
  "Declines without a zone list, and before the first round when every pending cell is refused."
  [c]
  (cond
    (and (nil? (ctx/zones c)) (not (:ignore-zones? (:args c)))) (access/decline! c :clear-box.declined "clear-box" {:reason :no-zones})
    (or (started? c) (box-error (:args c))) true
    :else (let [{:keys [allowed refused]} (sort-out c (pending c))]
            (if (and (empty? allowed) (seq refused))
              (access/decline! c :clear-box.declined "clear-box"
                               (assoc (access/refusal-fields (map second refused)) :reason :refused))
              true))))

(defn skip! [c pos reason]
  (ctx/update-mem! c update :skipped assoc pos reason)
  (ctx/emit! c :clear-box.skipped :info {:pos pos :reason reason
                                         :text (str "skipped " (pr-str pos) ": " (name reason))}))

(defn skip-refused!
  "Skip the refused cells [[pos verdict] ..] for good (reason :zone or :footprint), with one info naming who refused."
  [c refused]
  (let [fields (access/refusal-fields (map second refused))]
    (ctx/update-mem! c update :skipped merge (into {} (map (fn [[pos v]] [pos (:reason v)])) refused))
    (ctx/emit! c :clear-box.refused :info (assoc fields :cells (count refused)
                                                 :text (str "left " (count refused) " cells: " (access/refusal-text fields))))))

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

(defn verdict
  "How the rules judge digging pos now: :ok, :refused, :hazard, :no-zones or :not-loaded, with the verdict."
  [c pos]
  (let [v (access/may-dig? (access/rules-input c) pos)]
    [(access/judge v (:accept (:args c))) v]))

(defn ^:async dig!
  "Equip, ask the rules again, dig."
  [c pos n]
  (let [p (:primitives c)
        tool (tools/best-tool (map :name (u/inventory p)) n)]
    (when (and tool (not= tool (.-held (.self p))))
      (await (ctx/act c :equip #js {:item tool :dest "hand"})))
    (let [[judged v] (verdict c pos)]
      (case judged
        :ok (let [status (.-status (await (ctx/act c :dig (clj->js {:pos pos}))))]
              (case status
                ("dug" "missing") (ctx/update-mem! c #(cond-> (update % :tries dissoc pos)
                                                        (= "dug" status) (update :dug (fnil inc 0))))
                "cannot" (skip! c pos :cannot)
                (bump! c pos :refused)))
        :refused (skip-refused! c [[pos v]])
        :hazard (bump! c pos :hazard)
        nil)
      :continue)))

(defn ^:async round
  "One bounded step: skip the refused cells, walk to the highest nearest pending cell, equip a tool, dig."
  [c]
  (let [{:keys [allowed refused]} (sort-out c (pending c))]
    (when (seq refused) (skip-refused! c refused))
    (if (empty? allowed)
      (finish! c)
      (if-let [[pos n] (target c allowed)]
        (if (and n (= :hazard (first (verdict c pos))))
          (do (bump! c pos :hazard) :continue)
          (let [w (await (u/walk-near! c pos 3))]
            (case w
              :partial :continue
              :blocked (do (bump! c pos :unreachable) :continue)
              (if (nil? n)
                :continue
                (await (dig! c pos n))))))
        (await (step-off! c allowed))))))
