(ns jobs.animals.herd-survey
  "The pen read for jobs.animals.herd: the verdict on the pen and its gate from what the body sees, the looks at cells never seen, and the survey phase."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.gate :as gate]
            [jobs.lib.near :as near]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.sense :as sense]
            [jobs.lib.animals :as animals]
            [jobs.lib.util :as u]
            [jobs.animals.herd-pen :refer [approach-cells approach-free? axis choose-gate depth free-floor? gate-diagnosis gate-look-range look-range max-approach-walks min-depth open-stage other-leaks precheck?]]
            [jobs.animals.herd-run :refer [approach-range box-centre cell detail-text pen-box finish! in-pen-adults near-pen pen-opts read-pen]]))

(defn box-gates
  "The fence gates the body has seen on the ring of cells just outside the box (at its floor level), as {:x :y :z}."
  [c]
  (let [{:keys [min max]} (pen-box c)
        y (:y min)]
    (for [x (range (dec (:x min)) (+ 2 (:x max))) z (range (dec (:z min)) (+ 2 (:z max)))
          :when (not (and (<= (:x min) x (:x max)) (<= (:z min) z (:z max))))
          :let [pos {:x x :y y :z z}]
          :when (some-> (u/seen-name (:primitives c) pos) (.endsWith "_fence_gate"))]
      pos)))

(defn unseen-extra
  "What names the cell that kept the pen from being read: the first unseen leak cell, else the body's feet."
  [answer]
  (let [leak (first (filter #(= :unloaded (:why %)) (:leaks answer)))]
    (if leak {:why :unseen :cell (cell (:pos leak))} {:why (or (:reason answer) :unread)})))

(defn ^:async look-from-gate!
  "The unseen cells stayed unseen from where the body was (a fence hides the floor beside it): go near the pen's gate once,
  forget what was looked at and read again. No gate to go to, one already visited, or a blocked walk ends :no-pen."
  [c answer]
  (let [m (ctx/mem c)
        wanted (:gate (:args c))
        gates (->> (concat (map :pos (:gates answer)) (box-gates c)) (filter #(or (nil? wanted) (= (cell wanted) (cell %))))
                   (sort-by #(u/dist (u/self-pos c) %)))
        g (first gates)]
    (cond
      (or (nil? g) (:gate-visited m)) (finish! c :no-pen (unseen-extra answer))
      (u/within? (u/self-pos c) g (gate-look-range))
      (do (ctx/update-mem! c #(-> % (assoc :gate-visited true) (dissoc :looked-pen)))
          :continue)
      (= :blocked (await (near/go-near! c g (gate-look-range) {:doors :never :ways #{:walk :clear-seal}}))) (finish! c :no-pen (unseen-extra answer))
      :else :continue)))

(defn ^:async check-unknown!
  "The pen has cells the body has not seen (a gate in the dark decays): go near the nearest not yet looked at, look at it
  once, then read the pen again. No such cell left, or one too far to look at, goes to the pen's gate to look in (look-from-gate!)."
  [c answer]
  (let [here (u/self-pos c)
        centre (fn [{:keys [x y z]}] {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)})
        looked (set (:looked-pen (ctx/mem c)))
        unk (->> (:leaks answer) (filter #(= :unloaded (:why %))) (map :pos) (remove #(contains? looked (cell %)))
                 (sort-by #(u/dist here (centre %))) first)]
    (cond
      (nil? unk) (await (look-from-gate! c answer))
      (not (u/within? here unk (look-range)))
      (if (= :blocked (await (near/go-near! c unk (look-range) {:doors :never :ways #{:walk :clear-seal}})))
        (await (look-from-gate! c answer))
        :continue)
      :else (do (await (sense/look-at! c (cell unk)))
                (ctx/update-mem! c update :looked-pen (fnil conj []) (cell unk))
                :continue))))

(defn verdict
  "What the pen read says of the run: {:refuse reason :extra map} or {:chosen {:gate :in :out}}."
  [c answer]
  (let [{:keys [target]} (:args c)
        {:keys [gate in out] :as chosen} (choose-gate c answer)
        leaks (when chosen (other-leaks answer gate))
        inside (count (in-pen-adults c answer))
        refuse (fn [reason & [extra]] {:refuse reason :extra extra})]
    (cond
      (= :no-start (:reason answer)) (refuse :no-pen {:why :no-start})
      (nil? chosen) (refuse :no-gate (gate-diagnosis c answer))
      (not (gate/allowed? c :herd.declined "herd" :place gate {:own-plans-ok? true})) (refuse :refused)
      (not (approach-free? (:primitives c) (:inside answer) (cell gate) in out))
      (refuse :no-gate {:why :no-approach
                        :cell (first (remove #(free-floor? (:primitives c) %) (take approach-cells (axis (:primitives c) (:inside answer) (cell gate) in out 0))))})
      (seq leaks) (refuse :leaky {:leaks (vec (take 12 leaks))})
      (>= inside target) (refuse :full)
      (< (depth (:inside answer) (cell gate) in) min-depth)
      (refuse :too-shallow {:cell in :depth (depth (:inside answer) (cell gate) in) :need min-depth})
      :else {:chosen chosen})))

(defn unseen-approach
  "A cell of the approach cell (or the cell outside the gate) that refused the gate (its floor, feet or head) the body has
  never seen and not yet looked at, or nil."
  [c extra]
  (when (#{:no-approach :no-floor-outside} (:why extra))
    (->> [-1 0 1]
         (map #(update (vec (:cell extra)) 1 + %))
         (remove (set (:looked-pen (ctx/mem c))))
         (filter #(sense/unknown? (:primitives c) %))
         first)))


(defn centre [[x y z]] {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)})

(defn ^:async survey-pen!
  "The pen is read in full: choose the gate and go to the leash phase, or end with the reason. An approach cell never
  seen is looked at first."
  [c answer]
  (let [{:keys [refuse extra chosen]} (verdict c answer)
        {:keys [gate in out]} chosen
        unseen (when refuse (unseen-approach c extra))
        axis-cells (when chosen (axis (:primitives c) (:inside answer) (cell gate) in out (depth (:inside answer) (cell gate) in)))]
    (cond
      (and unseen (< (:approach-walks (ctx/mem c) 0) (max-approach-walks))
           (not= :there (await (near/go-near! c (centre unseen) (look-range) {:doors :never :ways #{:walk :clear-seal} :see true}))))
      (do (ctx/update-mem! c update :approach-walks (fnil inc 0))
          :continue)
      unseen
      (do (await (sense/look-at! c unseen))
          (ctx/update-mem! c update :looked-pen (fnil conj []) unseen)
          :continue)
      refuse
      (finish! c refuse extra)
      :else
      (do (ctx/update-mem! c assoc :phase :leash :in-pen-start (mapv animals/key-of (in-pen-adults c answer)) :gate gate :inside-cell (u/cell-pos in) :outside-cell (u/cell-pos out)
                           :axis axis-cells
                           :stage (open-stage axis-cells (let [pens (atom #{})
                                                  held? (fn [[x y z :as cell]]
                                                          (or (some-> (u/seen-name (:primitives c) {:x x :y y :z z}) (.endsWith "_fence_gate"))
                                                              (spare/cell-enclosed? c pens (vec cell))))]
                                              held?)))
          :continue))))

(defn pen-refusal
  "Why the pen, as the body sees and remembers it from where it stands, will not do: later with the verdict's refusal as
  its reason and its :why, :cell, :leaks or :depth. ok when it will do, already holds :target (done? answers)
  or cannot be read from here (the run reads it at the pen), or lacks only a look at an approach cell never seen (the run looks). The setting ::precheck? false skips it."
  [c]
  (let [answer (when (precheck?) (check-cache/cached c ::pen-refusal #(read-pen c)))
        {:keys [refuse extra]} (when-not (or (nil? answer)
                                             (#{:unloaded :no-start} (:reason answer))
                                             (>= (count (in-pen-adults c answer)) (:target (:args c))))
                                 (let [v (verdict c answer)]
                                   (when-not (and (= :no-gate (:refuse v)) (unseen-approach c (:extra v))) v)))]
    (if refuse
      (apply contract/later refuse (str "herd waits: the pen will not do, " (name refuse) (detail-text extra)) (mapcat identity extra))
      (contract/ok :pen "the pen will do, or is read at the pen"))))

(defn ^:async survey! [c]
  (let [centre (box-centre (pen-box c))]
    (if (> (u/dist (u/self-pos c) centre) (near-pen))
      (if (= :blocked (await (near/go-near! c centre (approach-range) {:doors :never :ways #{:walk :clear-seal}})))
        (finish! c :unreachable)
        :continue)
      (let [answer (await (pen-read/read! c (pen-opts c) #(= :unloaded (:reason %))))]
        (if (= :unloaded (:reason answer))
          (await (check-unknown! c answer))
          (await (survey-pen! c answer)))))))
