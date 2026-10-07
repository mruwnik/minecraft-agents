(ns jobs.build.clear-box
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.world :as known]))

(def doc
  "Dig out a box from the top down (site levelling, demolition). Beds, containers and fluids are kept (:keep adds
  more block names). The box is :from and :to, each {:x :y :z} or [x y z], at most 400 cells.
  Each cell is dug by a jobs.blocks.dig child: it walks in reach, holds the best carried tool and picks up the
  drop. No tool is needed (a block whose tool is missing is dug and its drop lost). While the child waits on
  the body (:inventory-full) the job waits with the same reason.
  Zones and plans: loaded cells that a zone or an active plan's footprint refuses are skipped for good (reason
  :zone or :footprint, one clear-box.refused info per round). The chosen cell is checked again before the dig.
  A dig hazard not in :accept counts a try (reason :hazard after two).
  The job declines:
  - on a missing corner or too many cells (clear-box.declined {:reason :bad-args}).
  - before the first round when every pending cell is refused (clear-box.declined {:reason :refused :zones
    :plans}).
  - when no zone list has been read (clear-box.declined {:reason :no-zones}), also in the middle of the job.
  Result: {:dug n :skipped {pos reason} :kept n :fluids {name count}}.")

(def args
  {:from {:doc "box corner (inclusive); any order; [x y z] or {:x :y :z}" :type :pos :default nil}
   :to {:doc "opposite box corner (inclusive); at most 400 cells" :type :pos :default nil}
   :keep {:doc "extra block names to leave alone" :default []}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent :falling-block :under-feet)"
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
  (cond
    (not (and from to)) "clear-box needs :from and :to"
    :else (let [[a b] [from to]
                n (* (count (span (:x a) (:x b))) (count (span (:y a) (:y b))) (count (span (:z a) (:z b))))]
            (when (> n max-cells)
              (str "clear-box covers " n " cells, at most " max-cells)))))

(defn cells
  "The cells [{:x :y :z} ...] of the box :from/:to. Throws ex-info for a
  missing corner or over max-cells cells."
  [{:keys [from to] :as args}]
  (when-let [e (box-error args)] (throw (ex-info e {})))
  (let [[a b] [from to]]
    (vec (for [x (span (:x a) (:x b)) y (span (:y a) (:y b)) z (span (:z a) (:z b))] {:x x :y y :z z}))))

(defn kept? [keep n]
  (or (some? (re-find kept-pattern n)) (boolean (some #{n} keep))))

(defn dig-args
  "The jobs.blocks.dig args for the cell at pos: no tool needed, the drop picked up, the job's :accept and opt-out."
  [c pos]
  (merge (select-keys (:args c) [:accept :ignore-zones?])
         {:pos pos :need-drop false :collect true}))

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
  "Whether a round has left anything in memory (dug, skipped, tried or started on a cell)."
  [c]
  (boolean (some #(contains? (ctx/mem c) %) [:dug :skipped :tries :target])))

(defn decline-box!
  "Warn once clear-box.declined {:reason :bad-args} naming the box's problem; waits with it."
  [c]
  (let [text (box-error (:args c))]
    (ctx/warn-once! c [:access :bad-args] :clear-box.declined {:reason :bad-args :text text})
    (ctx/wait c {:reason :bad-args :why text})))

(defn check
  "Declines without a zone list, and before the first round when every pending cell is refused."
  [c]
  (cond
    (and (not (started? c)) (box-error (:args c))) (decline-box! c)
    (and (nil? (known/zones c)) (not (:ignore-zones? (:args c)))) (access/decline! c :clear-box.declined "clear-box" {:reason :no-zones})
    (:target (ctx/mem c)) (let [w (blocks/child-wait c :dig 'jobs.blocks.dig (dig-args c (:target (ctx/mem c))))]
                            (if (blocks/body-wait? w) (ctx/wait c w) true))
    (started? c) true
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
  "Only under-foot cells are left: walk to the nearest cell outside the box (jobs.lib.step-off) to leave the cell the job
  must clear. A step that fails bumps the cell the body is standing on."
  [c todo]
  (let [me (u/self-pos c)
        box (cells (:args c))
        xs (map :x box) zs (map :z box)
        reach (max 2 (inc (max (- (apply max xs) (apply min xs)) (- (apply max zs) (apply min zs)))))
        feet {:x (js/Math.floor (:x me)) :y (js/Math.floor (:y me)) :z (js/Math.floor (:z me))}
        r (await (step-off/step-off! c feet {:avoid (into #{} (map (juxt :x :y :z)) box) :reach reach :ok? (step-off/zone-ok (access/rules-input c))}))]
    (when (:unreachable r)
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

(defn declined!
  "Book the cell t whose dig child declined with wait reason w: a body reason keeps it (the check waits on it), a
  refusal or a missing zone list skips or leaves it, a hazard counts a try, anything else skips it as :unreachable."
  [c t w]
  (when-not (blocks/body-wait? w)
    (ctx/update-mem! c dissoc :target)
    (case (:reason w)
      :not-allowed (when (#{:zone :claim :footprint} (:by w)) (skip-refused! c [[t (assoc w :reason (:by w))]]))
      :hazard (bump! c t :hazard)
      :not-loaded nil
      (skip! c t :unreachable)))
  :continue)

(defn ^:async dig!
  "One round of the dig child on the target cell t; book its end. A refused dig counts a try (:refused after two)."
  [c t]
  (let [args (dig-args c t)
        r (await (ctx/call-child c :dig 'jobs.blocks.dig args))]
    (case r
      :continue :continue
      :declined (declined! c t (blocks/child-wait c :dig 'jobs.blocks.dig args))
      (let [res (ctx/child-result c :dig)]
        (ctx/update-mem! c dissoc :target)
        (case (:reason res)
          :dug (ctx/update-mem! c #(-> % (update :tries dissoc t) (update :dug (fnil inc 0))))
          :already-clear (ctx/update-mem! c update :tries dissoc t)
          (:cannot :fluid) (skip! c t :cannot)
          (bump! c t :refused))
        :continue))))

(defn ^:async round
  "One bounded step: a round of the dig child on the cell under way (until it has picked up the drop), else skip the
  refused cells and start on the highest nearest pending cell; an unloaded one is walked to first."
  [c]
  (if-let [t (:target (ctx/mem c))]
    (await (dig! c t))
    (let [{:keys [allowed refused]} (sort-out c (pending c))]
      (when (seq refused) (skip-refused! c refused))
      (if (empty? allowed)
        (finish! c)
        (if-let [[pos n] (target c allowed)]
          (if (nil? n)
            (let [w (await (near/go-near! c pos 3 {:zone-tolls true}))]
              (when (= :blocked w) (bump! c pos :unreachable))
              :continue)
            (do (ctx/update-mem! c assoc :target pos)
                (await (dig! c pos))))
          (await (step-off! c allowed)))))))
