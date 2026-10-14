(ns jobs.animals.pen-check
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.near :as near]
            [jobs.lib.pen :as pen]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.util :as u]))

(def doc
  "Read-only: say whether an animal can walk out of a pen. Flood-fills what a cow can walk (rules in
  jobs.lib.pen: fences, walls and closed gates are 1.5 high, an open gate is a way out, a drop of more than 3
  is not taken) from the feet cell :at [x y z], or from every surface inside :box (or the box round the barrier cells of :plan), where a step out of the box
  is a leak. Reads what the body has seen; a pen it has not seen closed is checked again after it looks at the
  cells it has not seen (jobs.lib.sense/decide!); a cell that stays unseen (hidden behind a block) is walked to, at
  most 3 times, until the eye sees it. Never digs or places. Ends condition-failed :no-pen without :at, :box and :plan, and :plan-trouble when :plan is missing or has no fence, wall or gate cells (before any look).

  The fill stops after :max-cells cells: a bigger pen is not closed, reason :unbounded. A fill without a box
  that finds no wall either side of a gap says that too.

  Emits (info pen-check.done) and ends with the data {:closed? :reason :cells :leaks :gates}: :done when closed, else
  condition-failed with the :reason as the cause:
  - :reason is nil when closed, else :leak, :unbounded, :unloaded (a cell, or the start's floor, was never seen) or :no-start (:at is no floor to stand on).
  - :leaks lists {:pos {:x :y :z} :why}, why one of :open-gate :gap :climb :open :unloaded (:climb with a box names the fence, wall or gate top first stood on, reached from a block or raised floor beside it). At most 12 are
    listed, :leaks-total gives the count when more.
  - :cells counts the inside cells. For a leaky pen without a box it counts the cells the pen would enclose with
    its leaks shut, and 0 when shutting up to 4 rounds of them does not close it (a low wall all round).
  - :remembered lists the cells the answer rests on that are remembered, not in view even after a look (at most 12,
    :remembered-total when more).
  - :gate-drops (only when found) lists {:gate :from :to :drop}: a gate whose side of the pen is cut off by a drop of more than 1 block (animals will not step down 2).
  - :gates lists every fence gate in or beside those cells with :open?.
  - :animals lists the livestock on those cells (or up to 3 above one: a ledge inside the fence) as {:kind :id :pos [x y z]}, :animals-in counts them, so a loss can be dated from the log.")

(a/defargs args
  {:at {:doc "feet cell [x y z] or {:x :y :z} of a spot inside the pen (the floor's top, where the animal's feet are)" :spec ::a/pos :default nil}
   :box {:doc "the pen: {:min :max}, each corner {:x :y :z} or [x y z], inclusive; a step out of it is a leak" :spec pen-read/box-arg :default nil}
   :plan {:doc "id of a plan of the body's world: the pen is the box round its fence, gate and wall cells (instead of :box)" :spec a/name? :default nil}
   :part {:doc "with :plan, only the cells of this part" :spec a/name? :default nil}
   :max-cells {:doc "most cells the fill visits before it gives up with :unbounded" :spec (a/int-in 1 nil) :default pen/default-max-cells}})

(defn pen-box
  "{:box} for the args (:box, else the box of :plan), {:trouble text}, or {} with only :at."
  [c]
  (let [{:keys [box plan part]} (:args c)]
    (cond
      box {:box (pen-read/norm-box box)}
      plan (pen-read/plan-box c plan part)
      :else {})))

(def target [:at :box :plan])

(def defaults {})

(defn problem
  "The cause of a call with no pen to read, or nil."
  [c]
  (let [{:keys [at box plan]} (:args c)]
    (cond
      (not (or at box plan)) {:reason :no-pen :why "needs :at, :box or :plan"}
      (:trouble (pen-box c)) {:reason :plan-trouble :why (:trouble (pen-box c))})))

(def conditions
  "None wait: a call with no pen to read ends its first round (problem), so a queued order never blocks the queue."
  [])

(defn done?
  "Counts only (the round ends the call with its data): the cells of the pen read."
  [c]
  {:done? false :units :cells :done (:cells (ctx/mem c) 0) :left 0})

(defn quote-of [_c]
  {:uses {:time [1 5 30]}})

(def max-walks 3)

(def animal-radius 48)

(defn unseen-leak
  "The nearest-listed leak cell that is unseen, as {:x :y :z}."
  [checked]
  (:pos (first (filter #(= :unloaded (:why %)) (:leaks checked)))))

(defn leak-text
  "\": why at [x y z]\" listing the first leaks of result, with how many more."
  [{:keys [leaks leaks-total]}]
  (let [shown (take 3 leaks)
        more (- (or leaks-total (count leaks)) (count shown))]
    (when (seq shown)
      (str ": " (str/join ", " (map (fn [{:keys [pos why]}] (str (name why) " at [" (:x pos) " " (:y pos) " " (:z pos) "]")) shown))
           (when (pos? more) (str " and " more " more"))))))

(defn gate-drop-text
  [{:keys [gate-drops]}]
  (when-let [{:keys [gate drop]} (first gate-drops)]
    (str "; gate at [" (:x gate) " " (:y gate) " " (:z gate) "] has a drop of " drop " inside, animals will not step down")))

(defn ^:async read-pen! [c]
  (let [{:keys [at max-cells]} (:args c)
        box (:box (pen-box c))
        checked (await (pen-read/read! c {:at at :box box :max-cells max-cells}))
        unseen (when (= :unloaded (:reason checked)) (unseen-leak checked))
        walks (:walks (ctx/mem c) 0)
        walk (when (and unseen (< walks max-walks))
               (await (near/go-near! c unseen 1 {:doors :never :escalate false})))]
    (case walk
      :partial :continue
      :there (do (ctx/update-mem! c update :walks (fnil inc 0)) :continue)
      (let [rows (animals/in-pen-rows (:primitives c) checked animal-radius)
            result (merge (pen/summary checked) (pen-read/listed checked (u/self-pos c)) {:animals rows :animals-in (count rows)})]
        (ctx/emit! c :pen-check.done :info
                   (assoc result :text (if (:closed? result)
                                         (str "pen closed, " (:cells result) " cells" (gate-drop-text result))
                                         (str "pen not closed: " (name (:reason result)) (leak-text result) (gate-drop-text result)))))
        (ctx/update-mem! c assoc :cells (:cells result))
        (if (:closed? result)
          (ctx/end c :done nil result)
          (ctx/end c :condition-failed (:reason result) result))))))

(defn ^:async round [c]
  (if-let [p (problem c)]
    (ctx/end c :condition-failed p nil)
    (await (read-pen! c))))
