(ns jobs.farm.tidy
  (:require [engine.args :as a]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.tidy-rules :as tr]
            [jobs.lib.toll-cells :as tc]
            [plan.shape :as shape]
            [jobs.lib.world :as known]))

(def doc
  "Dig the stray blocks over a plan's cells (:plan, optionally only its :part) and pick up the drops.
  The plan does not say what a stray is, this job does:
  - a cell the plan wants :clear holds a stray when anything is in it.
  - a crop cell (want {:crop c}) holds one when it holds anything but that crop (weeds, saplings, leaves,
    stone, dirt ...).
  - the one cell of air above each crop cell counts like a crop cell, unless the plan names it.
  Never dug, only listed:
  - the planned crop itself (ripe or not) and any block the plan wants.
  - water and lava, so the plan's own water is never drained. A cell that wants water is never dug.
  - farmland where a crop cell or a :clear cell wants something else (listed as :wrong).
  - a container or workstation, a bed, sign, banner, head or light where the plan wants something else.
    Result :kept {:pos :block :why :container|:owned|:light|:fluid}.
  - a block in a cell that wants another block, and a crop of another kind in a crop cell. Result :wrong
    {:pos :found :want} and warn tidy.wrong. Fixing these is a build, till or plant job's work.
  Cells that are not loaded hold no strays.
  Every dig is checked against zones and the footprints of all other active plans, when the cell is chosen and
  again before the dig. A zone, claim, footprint or undiggable cell is refused at once. A dig hazard that
  :accept does not name defers the cell until nothing else is left. It is then tried :give-up times and refused
  as :hazard. A cell that cannot be walked to or reached :give-up times is refused as :unreachable.
  Refused cells are in the result's :refused [{:pos :block :reason}] and the warn tidy.refused.
  The sweep goes top-down, nearest first. It ends by collecting the drops within the field's extent plus 4 blocks
  (jobs.forestry.collect-drops as a child).
  Result: {:dug n :collected n :kept [...] :wrong [...] :refused [...]}, info tidy.done. Over a tidy field it
  digs nothing.
  The job declines (one tidy.declined warn naming the plan and the reason) while the plan is missing, unreadable
  or has no cells (in :part), and while no zone list has been read.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world" :spec a/name? :default nil}
   :part {:doc "only the cells of this part (and the air above its crop cells)" :spec a/name? :default nil}
   :accept {:doc "dig hazards accepted: :fluid-adjacent (water beside; lava beside is :lava-adjacent and is not accepted by default), :falling-block"
            :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent}}
   :reach {:doc "cells whose centre is this close to the eye are dug without walking, in blocks" :spec (a/num-in 0 nil) :default u/eye-reach}
   :give-up {:doc "failed walks, failed digs or hazard-blocked tries after which a cell is refused" :spec (a/int-in 1 nil) :default 3}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})


;; ------------------------------------------------------------------ access

(def crop-blocks
  #{"wheat" "carrots" "potatoes" "beetroots" "melon_stem" "pumpkin_stem" "attached_melon_stem" "attached_pumpkin_stem"
    "melon" "pumpkin" "sugar_cane" "bamboo" "bamboo_sapling" "sweet_berry_bush" "nether_wart" "cocoa" "torchflower_crop"
    "pitcher_crop"})

(defn crop-want? [want] (and (map? want) (contains? want :crop)))
(defn tree-want? [want] (and (map? want) (contains? want :tree)))

(defn classify
  "What the world's block (nil: nobody has seen it) is in a cell with this want: nil (nothing for this job), :dig,
  :wrong (reported, never dug) or [:kept why]. A want is a plan want, or :headroom for the air above a crop cell."
  [want block]
  (let [n (:name block)]
    (cond
      (or (nil? block) (shape/air? n)) nil
      (tree-want? want) nil
      (and (crop-want? want) (contains? (shape/crop-names (:crop want)) n)) nil
      (or (= :clear want) (= :headroom want) (crop-want? want))
      (if-let [why (tr/keep-why n)]
        [:kept why]
        (cond
          (= "farmland" n) :wrong
          (and (crop-blocks n) (= :headroom want)) nil
          (and (crop-blocks n) (crop-want? want)) :wrong
          :else :dig))
      :else (when (= :wrong (shape/judge want block)) :wrong))))

(defn work-cells
  "The cells the job looks at: the plan's cells (of part when given) [{:pos :want ...}] and the cell above each crop
  cell that no cell of the plan names, as {:pos :want :headroom}."
  [cells part]
  (let [claimed (set (map :pos cells))
        mine (filterv #(or (nil? part) (= part (:part %))) cells)
        heads (->> mine (filter #(crop-want? (:want %))) (map #(update (:pos %) 1 inc)) (remove claimed) distinct)]
    (into mine (map (fn [pos] {:pos pos :want :headroom})) heads)))

(defn want-text [want] (if (= :headroom want) "air" (shape/want-text want)))

(defn strays
  "{:dig [{:pos :block}] :kept [{:pos :block :why}] :wrong [{:pos :found :want}]} of the cells, read through block-at
  (a fn [pos] -> {:name n :state s} or nil)."
  [cells block-at]
  (reduce (fn [acc {:keys [pos want]}]
            (let [block (block-at pos)
                  verdict (classify want block)]
              (cond
                (nil? verdict) acc
                (= :dig verdict) (update acc :dig conj {:pos pos :block (:name block)})
                (= :wrong verdict) (update acc :wrong conj {:pos pos :found (:name block) :want (want-text want)})
                :else (update acc :kept conj {:pos pos :block (:name block) :why (second verdict)}))))
          {:dig [] :kept [] :wrong []}
          cells))

(defn plan-trouble
  "Why a plan answer cannot be worked (the cells in part), or nil."
  [answer part]
  (cond
    (nil? answer) "no such plan"
    (:broken answer) (str "the plan cannot be read: " (:broken answer))
    (not-any? #(or (nil? part) (= part (:part %))) (:cells answer)) "no cells"))

;; ------------------------------------------------------------------ reading the world

(defn in-reach? [c pos] (<= (u/eye-dist (u/self-pos c) pos) (:reach (:args c))))

(defn pos-map [[x y z]] {:x x :y y :z z})

;; ------------------------------------------------------------------ check

(defn planned
  "{:answer plan-answer} for the plan in the args, or {:trouble text} (warned once per reason)."
  [c]
  (let [{:keys [plan part]} (:args c)
        answer (known/plan c plan)
        trouble (or (plan-trouble answer part)
                    (let [{:keys [zones ignore-zones?]} (tr/access-world c)]
                      (when (and (nil? zones) (not ignore-zones?)) "no zone list has been read")))]
    (if-not trouble
      {:answer answer}
      (do (ctx/warn-once! c [plan trouble] :tidy.declined
                          {:plan plan :part part :reason trouble
                           :text (str "tidy declines plan " plan (when part (str " part " part)) ": " trouble)})
          {:trouble trouble}))))

(defn check [c]
  (if-let [trouble (:trouble (planned c))]
    (ctx/wait c {:reason :plan-trouble :why trouble})
    true))

;; ------------------------------------------------------------------ steps

(defn refuse
  "m with pos refused: {:pos :block ...why}."
  [m {:keys [pos block]} why]
  (-> m
      (update :fails dissoc pos)
      (update :refused (fnil conj []) (merge {:pos pos :block block} why))))

(defn count-fail
  "m with one more failure on the stray; refused with :reason why at the give-up-th."
  [m {:keys [pos] :as stray} why give-up]
  (let [n (inc (get-in m [:fails pos] 0))]
    (if (>= n give-up)
      (refuse m stray {:reason why})
      (assoc-in m [:fails pos] n))))

(defn ^:async dig-one!
  "Dig the stray (a blocks.dig child) after asking the access rules once more; a refusal, a failure or a dig that did nothing is booked."
  [c {:keys [pos] :as stray}]
  (let [give-up (:give-up (:args c))
        d (tr/decide c pos)]
    (cond
      (and (vector? d) (= :refuse (first d))) (ctx/update-mem! c refuse stray (second d))
      (not= :dig d) nil
      :else
      (do
        (ctx/update-mem! c assoc :digging pos :collect true)
        (let [;; decide has judged the hazards, lava apart from water
              dig-args {:for-plan (:plan (:args c)) :accept #{:fluid-adjacent :falling-block}
                        :ignore-zones? (:ignore-zones? (:args c))}
              outcome (await (blocks/dig-cell! c (pos-map pos) dig-args))]
          (when-not (= :continue outcome) (ctx/update-mem! c dissoc :digging))
          (case outcome
            :continue :continue
            :dug (ctx/update-mem! c update :dug (fnil inc 0))
            :missing nil
            :cannot (ctx/update-mem! c refuse stray {:reason :cannot})
            :refused (let [why (:reason (blocks/child-wait c :dig 'jobs.blocks.dig (merge {:collect false :need-drop false :fetch false} dig-args {:pos (pos-map pos)})) :hazard)]
                       (ctx/update-mem! c count-fail stray why give-up))
            :unreachable (ctx/update-mem! c count-fail stray :unreachable give-up)
            (ctx/update-mem! c count-fail stray :failed give-up)))))))

(defn ^:async walk-to!
  "Walk to within 3 of the stray; a walk that is blocked, or ends out of reach, counts a failure."
  [c {:keys [pos] :as stray}]
  (let [give-up (:give-up (:args c))
        w (await (near/go-near! c (pos-map pos) 3 {:tolls (tc/walk-tolls c (near/cell-of pos))}))]
    (when (= :blocked w)
      (ctx/update-mem! c count-fail stray :unreachable give-up))
    (when (and (= :there w) (not (in-reach? c pos)))
      (ctx/update-mem! c count-fail stray :unreachable give-up))
    (if (= :partial w) :continue :again)))

(defn split-by-decision
  "{:ready [strays] :deferred [[stray reasons]]} of the strays; a refused one is booked on the way, a stray whose
  chunk went away since it was read is dropped from both (the next round reads the strays again)."
  [c todo]
  (reduce (fn [acc stray]
            (let [d (tr/decide c (:pos stray))]
              (cond
                (= :dig d) (update acc :ready conj stray)
                (= :skip d) acc
                (= :refuse (first d)) (do (ctx/update-mem! c refuse stray (second d)) acc)
                :else (update acc :deferred conj [stray (second d)]))))
          {:ready [] :deferred []}
          todo))

(defn nearest [c strays]
  (let [body (u/self-pos c)]
    (first (sort-by #(u/dist body (pos-map (:pos %))) strays))))

(defn ^:async work!
  "One step over the strays still to dig: dig what is in reach (top-down), else walk to the nearest ready one; with
  only hazard-blocked ones left, count a try on those in reach, else walk to the nearest."
  [c todo]
  (let [{:keys [ready deferred]} (split-by-decision c todo)
        near (->> ready
                  (filter #(in-reach? c (:pos %)))
                  (sort-by (juxt #(- (get (:pos %) 1)) #(u/dist (u/self-pos c) (pos-map (:pos %))))))]
    (cond
      (seq near) (loop [left near]
                   (if-let [stray (first left)]
                     (if (= :continue (await (dig-one! c stray)))
                       :continue
                       (recur (rest left)))
                     :again))
      (seq ready) (await (walk-to! c (nearest c ready)))
      (seq deferred) (let [stray (nearest c (map first deferred))
                           reasons (second (first (filter #(= (:pos stray) (:pos (first %))) deferred)))]
                       (if (in-reach? c (:pos stray))
                         (do (ctx/update-mem! c (fn [m]
                                                  (let [n (inc (get-in m [:fails (:pos stray)] 0))]
                                                    (if (>= n (:give-up (:args c)))
                                                      (refuse m stray {:reason :hazard :hazards reasons})
                                                      (assoc-in m [:fails (:pos stray)] n)))))
                             :again)
                         (await (walk-to! c stray))))
      :else :again)))

(defn field-radius
  "How far from the body drops can lie: the cells' extent plus 4, at most 48."
  [cells]
  (let [ps (map :pos cells)
        lo (apply map min ps)
        hi (apply map max ps)]
    (min 48 (+ 4 (js/Math.ceil (apply js/Math.hypot (map - hi lo)))))))

(defn ^:async collect! [c cells]
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius (field-radius cells)}))]
    (when (= :done r)
      (ctx/update-mem! c #(-> % (dissoc :collect) (update :collected (fnil + 0) (:collected (ctx/child-result c :collect) 0)))))
    (when (= :declined r) (ctx/update-mem! c dissoc :collect))
    (if (= :continue r) :continue :again)))

(defn finish! [c found]
  (let [m (ctx/mem c)
        plan (:plan (:args c))
        refused (vec (:refused m))
        result {:dug (:dug m 0) :collected (:collected m 0) :kept (:kept found) :wrong (:wrong found) :refused refused}]
    (when (seq refused)
      (ctx/emit! c :tidy.refused :warn {:plan plan :refused refused
                                        :text (str "tidy of " plan " refused " (count refused) " strays: "
                                                   (str/join ", " (map #(str (pr-str (:pos %)) " " (:block %) " " (name (:reason %))) refused)))}))
    (when (seq (:wrong found))
      (ctx/emit! c :tidy.wrong :warn {:plan plan :cells (:wrong found)
                                      :text (str "tidy of " plan " left " (count (:wrong found)) " wrong blocks: "
                                                 (str/join ", " (map #(str (pr-str (:pos %)) " " (:found %)) (:wrong found))))}))
    (ctx/emit! c :tidy.done :info {:plan plan :dug (:dug result) :collected (:collected result) :kept (count (:kept found))
                                   :wrong (count (:wrong found)) :refused (count refused)
                                   :text (str "tidy of " plan " done: "
                                              (if (and (zero? (:dug result)) (empty? refused)) "nothing to dig" (str "dug " (:dug result)))
                                              (when (seq refused) (str ", refused " (count refused))))})
    (ctx/result! c result)
    :done))

(defn settle-digging!
  "A dig the last run was in when it was cut: counted as dug once its stray is gone from the world."
  [c found]
  (when-let [pos (:digging (ctx/mem c))]
    (ctx/update-mem! c (fn [m] (cond-> (dissoc m :digging)
                                 (not-any? #(= pos (:pos %)) (:dig found)) (update :dug (fnil inc 0)))))))

(defn cells-unseen?
  "Whether a cell of the work was never seen (the body has not looked at it), so no stray is known there."
  [c cells]
  (look/unseen? (:primitives c) (map (comp pos-map :pos) cells)))

(defn ^:async step [c]
  (let [{:keys [answer trouble]} (planned c)]
    (if trouble
      :declined
      (let [cells (work-cells (:cells answer) (:part (:args c)))
            found (strays cells #(tr/world-block (:primitives c) %))
            _ (settle-digging! c found)
            m (ctx/mem c)
            refused (set (map :pos (:refused m)))
            todo (remove #(refused (:pos %)) (:dig found))]
        (cond
          (seq todo) (await (work! c todo))
          (:collect m) (await (collect! c cells))
          (and (not (look/surveyed? c)) (cells-unseen? c cells)) (do (await (look/survey! c)) :again)
          :else (finish! c found))))))

(defn ^:async round
  "The whole attempt: loop the steps (dig, walk, sweep the drops) until no stray is left; :continue only while a walk or
  the sweep waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
