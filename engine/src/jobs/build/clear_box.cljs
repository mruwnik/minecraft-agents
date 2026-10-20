(ns jobs.build.clear-box
  (:require [clojure.string :as str]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.job-common :as common]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.build.kept :refer [kept?]]
            [jobs.lib.choose :as choose]
            [jobs.lib.own-pen :as own-pen]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.step-off :as step-off]))

(def doc
  "Dig out a box from the top down (site levelling, demolition). Beds, containers and fluids are kept (:keep adds
  more block names). The box is :from and :to, each {:x :y :z} or [x y z], at most 400 cells.
  Each cell is dug by a jobs.blocks.dig child: it walks in reach, holds the best carried tool and picks up the
  drop. No tool is needed (a block whose tool is missing is dug and its drop lost). While the child waits on
  the body (:inventory-full) the job yields :continue.
  Zones and plans: loaded cells that a zone or an active plan's footprint refuses are skipped for good (reason
  :zone or :footprint, one clear-box.refused info per round). The chosen cell is checked again before the dig.
  A cell that is the wall of an own pen holding livestock is skipped for good (reason :own-pen, one clear-box.refused
  info) unless :open-pens?.
  A dig hazard not in :accept counts a try (reason :hazard after two). A cell reached but still unseen counts a try (reason :unloaded after two).
  Refused before it starts: {:reason :bad-args :why} (impossible: a missing corner or too many cells), as can-start?
  refusals {:reason :no-zones} (no zone list read, unless :ignore-zones?, also in the middle of the job) and
  {:reason :refused :zones :plans} (every pending cell is refused, before the first round).
  Ends with data {:dug n :skipped {pos reason} :kept n :fluids {name count}}: done when it dug some (the least accepted
  is :least, cells) or had nothing to dig; with none dug and cells skipped, condition-failed for :zone, :footprint and
  :cannot, else nothing-possible (:unreachable, :hazard, :refused), the cause the one skip reason or :nothing-dug when
  mixed. Already done when every cell is loaded and air, fluid or kept; done? counts the cells dug this call and the
  cells left. With :only, blocks not ending in one of its names stay (a tree-clearing keeps the ground).")

(a/defargs args
  {:from {:doc "box corner (inclusive); any order; [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :to {:doc "opposite box corner (inclusive); at most 400 cells" :spec ::a/pos :default nil}
   :keep {:doc "extra block names to leave alone" :spec (a/coll-of a/name?) :default []}
   :only {:doc "block name endings to clear, e.g. [\"_log\" \"_leaves\"]; other blocks are left as they are; none = every block" :spec (a/coll-of a/name?) :default []}
   :ignore-zones? :shared
   :open-pens? {:doc "true: cells that are the wall of an own pen holding livestock may be dug (the animals walk out)" :spec boolean? :default false}
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent :falling-block)"
            :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent :falling-block}}})

(a/defargs settings
  {::max-cells {:default 400 :doc "Cells one call looks at before it gives the round back." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 600 :doc "Seconds one call may take." :spec (a/int-in 1 nil)}
   ::max-tries {:default 2 :doc "Attempts before the job gives up." :spec (a/int-in 0 nil)}
   ::cell-s {:default 1.5 :doc "Mean seconds one cell takes (walk, dig, pick up, pace), for the quote." :spec (a/num-in 0 nil)}})
(defn max-cells [] (settings/get settings ::max-cells))
(defn max-tries [] (settings/get settings ::max-tries))
(defn left-alone?
  "Whether block n stays: air, fluid, kept, or (with :only) not one of the endings."
  [{:keys [keep only]} n]
  (or (bs/air n) (bs/fluids n) (kept? keep n)
      (and (seq only) (not-any? #(str/ends-with? n %) only))))
(defn span [a b] (range (min a b) (inc (max a b))))

(defn box-error
  "Why the box :from/:to cannot be cleared, or nil."
  [{:keys [from to]}]
  (cond
    (not (and from to)) "clear-box needs :from and :to"
    :else (let [[a b] [from to]
                n (* (count (span (:x a) (:x b))) (count (span (:y a) (:y b))) (count (span (:z a) (:z b))))]
            (when (> n (max-cells))
              (str "clear-box covers " n " cells, at most " (max-cells))))))

(defn cells
  "The cells [{:x :y :z} ...] of the box :from/:to. Throws ex-info for a
  missing corner or over max-cells cells."
  [{:keys [from to] :as args}]
  (when-let [e (box-error args)] (throw (ex-info e {})))
  (let [[a b] [from to]]
    (vec (for [x (span (:x a) (:x b)) y (span (:y a) (:y b)) z (span (:z a) (:z b))] {:x x :y y :z z}))))

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
         (keep (fn [pos] (when-let [n (u/seen-name (:primitives c) pos)] [pos n]))))))

(defn pending
  "[[pos block-name-or-nil] ...]: the loaded cells to dig, and the unloaded
  ones (nil name) not skipped, which are still to be walked to."
  [c]
  (let [skipped (:skipped (ctx/mem c) {})]
    (->> (cells (:args c))
         (remove #(contains? skipped %))
         (map (fn [pos] [pos (u/seen-name (:primitives c) pos)]))
         (remove (fn [[_ n]] (and n (left-alone? (:args c) n))))
         vec)))

(defn pending-all
  "pending, skipped cells included."
  [c]
  (let [args (:args c)]
    (->> (cells args)
         (map (fn [pos] [pos (u/seen-name (:primitives c) pos)]))
         (remove (fn [[_ n]] (and n (left-alone? args n))))
         vec)))

(defn split-by-rules
  "{:allowed [[pos name] ..] :refused [[pos verdict] ..]}: the todo cells split by the verdict of rules input in."
  [in accept todo]
  (let [judged (map (fn [[pos n]] (let [v (access/may-dig? in pos)]
                                    [pos n v (when v (access/judge v accept))]))
                    todo)]
    {:allowed (into [] (comp (remove #(= :refused (nth % 3))) (map (fn [[pos n]] [pos n]))) judged)
     :refused (into [] (comp (filter #(= :refused (nth % 3))) (map (fn [[pos _ v]] [pos v]))) judged)}))

(defn sort-out
  "{:allowed [[pos name] ..] :refused [[pos verdict] ..]}: the pending cells split by the rules' permission (a zone or
  another plan's footprint); unloaded cells are allowed (walked to)."
  [c todo]
  (split-by-rules (access/rules-input c) (:accept (:args c)) todo))

(defn bad-args
  "Never :bad-args for a missing corner or too big a box."
  [c]
  (if-let [text (box-error (:args c))]
    (contract/never :bad-args text :why text)
    (contract/ok :args "the box fits")))

(def no-zones
  "Later :no-zones when no zone list could be read (unless :ignore-zones?), also in the middle of the job."
  (checks/refusing :zones-read "a zone list is read"
                      #(when-not (box-error (:args %)) (common/no-zones % :clear-box.declined "clear-box"))))

(def all-refused
  "Later :refused when every pending cell is refused by a zone or plan."
  (checks/refusing
   :not-refused "a cell is not refused"
   (fn [c]
     (when-not (box-error (:args c))
       (let [{:keys [allowed refused]} (sort-out c (pending c))]
         (when (and (empty? allowed) (seq refused))
           (let [fields (assoc (access/refusal-fields (map second refused)) :reason :refused)]
             (ctx/warn-once! c [:access :refused] :clear-box.declined
                             (assoc fields :text (str "clear-box declined: every target is refused by " (access/refusal-text fields))))
             fields)))))))

(def target [:from :to])
(def defaults {:spend {:job-time ::job-time-s}})
(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)
(def can-start? (cond/and bad-args no-zones all-refused))
(def can-run? no-zones)

(declare result-of)

(defn done?
  "Ok when every cell is loaded and nothing is left to dig; else later, with the cells dug this call, the cells left to dig
  and the result so far."
  [c]
  (checks/goal (and (not (box-error (:args c))) (empty? (pending-all c)))
               [:cleared "nothing is left to dig"] [:owed "cells are left to dig"]
               {:units :cells :done (:dug (ctx/mem c) 0) :left (if (box-error (:args c)) 0 (count (pending c)))
                :result (when-not (box-error (:args c)) (result-of c))}))

(defn quote-of [c]
  (let [p (:primitives c)
        blocks (if (box-error (:args c)) [] (remove bs/air (map #(u/seen-name p %) (cells (:args c)))))
        n (count blocks)
        s (* n (settings/get settings ::cell-s))]
    {:uses (cond-> {:job-time [(* 0.5 s) s (* 4 s)]}
             (pos? n) (assoc :wear (choose/dig-wear c blocks)))
     :done [0 n n]}))

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

(defn skip-pen-walls!
  "Skip the cells that are the wall of an own pen holding livestock for good (reason :own-pen), with one info."
  [c poss]
  (ctx/update-mem! c update :skipped merge (zipmap poss (repeat :own-pen)))
  (ctx/emit! c :clear-box.refused :info {:reason :own-pen :cells (count poss)
                                         :text (str "left " (count poss) " cells: the wall of a pen with animals in it (:open-pens? true digs it)")}))

(defn bump! [c pos reason]
  (let [n (inc (get-in (ctx/mem c) [:cell-tries pos] 0))]
    (if (>= n (max-tries))
      (skip! c pos reason)
      (ctx/update-mem! c assoc-in [:cell-tries pos] n))))

(defn under-foot
  "The set of cells the body stands on or in: the cell below its feet, and its
  feet and head cells."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)
        fx (js/Math.floor x) fy (js/Math.floor y) fz (js/Math.floor z)]
    #{{:x fx :y (dec fy) :z fz} {:x fx :y fy :z fz} {:x fx :y (inc fy) :z fz}}))

(defn top-cell
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
        r (await (step-off/step-off-zoned! c feet {:avoid (into #{} (map (juxt :x :y :z)) box) :reach reach}))]
    (when (:unreachable r)
      (bump! c (first (apply min-key #(u/dist me (first %)) todo)) :unreachable))
    :again))

(def condition-skips
  "Skip reasons that name a cell the job may not or cannot dig; any other skip is nothing-possible."
  #{:zone :footprint :cannot :own-pen})

(defn result-of
  "The data of the call's ending: what is dug, skipped, kept and wet so far."
  [c]
  (let [everything (named c)
        keep (:keep (:args c) [])]
    {:dug (:dug (ctx/mem c) 0) :skipped (:skipped (ctx/mem c) {})
     :kept (count (filter (fn [[_ n]] (and (not (bs/fluids n)) (not (bs/air n)) (kept? keep n))) everything))
     :fluids (frequencies (filter bs/fluids (map second everything)))}))

(defn finish!
  "The ending of a run with no cell left to work (job-common/conclude!)."
  [c]
  (let [{:keys [dug skipped] :as result} (result-of c)]
    (common/conclude! c {:verb "dug" :mixed :nothing-dug :condition-skips condition-skips} dug skipped result)))

(defn declined?
  "Whether the dig child's ending e is a refusal at its start (no data), not a dig that failed."
  [e]
  (and (= :condition-failed (:kind e)) (nil? (:data e))))

(defn wait-of [e] (let [cause (:cause e)] (if (map? cause) cause {:reason cause})))

(defn declined!
  "Book the cell t whose dig child was refused with cause w: a body reason or an unloaded cell keeps it (the job yields
  :continue), a refusal or a missing zone list skips or leaves it, a hazard counts a try, anything else skips it as
  :unreachable."
  [c t w]
  (when-not (or (blocks/body-wait? w) (= :not-loaded (:reason w)))
    (ctx/update-mem! c dissoc :target)
    (case (:reason w)
      :not-allowed (when (#{:zone :claim :footprint} (:by w)) (skip-refused! c [[t (assoc w :reason (:by w))]]))
      :hazard (bump! c t :hazard)
      (skip! c t :unreachable)))
  (if (or (blocks/body-wait? w) (= :not-loaded (:reason w))) :continue :again))

(defn ^:async dig!
  "One round of the dig child on the target cell t; book its end. A refused dig counts a try (:refused after two)."
  [c t]
  (let [e (await (ctx/call c :dig 'jobs.blocks.dig (dig-args c t)))]
    (cond
      (= :continue e) :continue
      (declined? e) (declined! c t (wait-of e))
      :else
      (do (ctx/update-mem! c dissoc :target)
          (case (if (= :done (:kind e)) (:reason (:data e)) (contract/cause-key e))
            :dug (ctx/update-mem! c #(-> % (update :cell-tries dissoc t) (update :dug (fnil inc 0))))
            :already-clear (ctx/update-mem! c update :cell-tries dissoc t)
            (:cannot :fluid) (skip! c t :cannot)
            :unreachable (skip! c t :unreachable)
            (bump! c t :refused))
          :again))))

(defn ^:async step
  "One step (the pen is read anew: digging in shows a hidden pit): a round of the dig child on the cell under way (until it has picked up the drop), else skip the
  refused cells and start on the highest nearest pending cell; an unloaded one is walked to first."
  [c pen-wall-fn]
  (if-let [t (:target (ctx/mem c))]
    (await (dig! c t))
    (let [{:keys [allowed refused]} (sort-out c (pending c))
          pen-wall? (pen-wall-fn)
          {open true walled false} (group-by (fn [[pos _]] (not (pen-wall? [(:x pos) (:y pos) (:z pos)]))) allowed)
          allowed (vec open)]
      (when (seq refused) (skip-refused! c refused))
      (when (seq walled) (skip-pen-walls! c (map first walled)))
      (if (empty? allowed)
        (finish! c)
        (if-let [[pos n] (top-cell c allowed)]
          (if (nil? n)
            (let [w (await (common/walk! c pos 3 {}))]
              (when (= :blocked w) (bump! c pos :unreachable))
              (when (and (= :there w) (nil? (u/seen-name (:primitives c) pos))) (bump! c pos :unloaded))
              (if (= :continue w) :continue :again))
            (do (ctx/update-mem! c assoc :target pos)
                (await (dig! c pos))))
          (await (step-off! c allowed)))))))

(defn ^:async round
  "The whole attempt: loop the steps until the box is cleared or declined; :continue only while a walk or the body waits."
  [c]
  (let [pen-wall-fn (if (:open-pens? (:args c)) (constantly (constantly false)) #(own-pen/box-wall? c (cells (:args c))))]
    (await (pace/steps! c (fn ^:async s [] (await (step c pen-wall-fn)))))))
