(ns jobs.access.tunnel.estimate
  "The tunnel's target, size estimates and how a child's ending reads as a stop."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.access.tunnel.line :as ln]
            [jobs.lib.reach :as reach]
            [plan.shape :as shape]))

(defn target-of
  "The :target arg as [x y z]; with only a :heading, the cell :length + 1 blocks along it from where the call started
  (the body's cell, remembered in the job memory); else nil."
  [c]
  (if-let [{:keys [x y z]} (:target (:args c))]
    [x y z]
    (when-let [h (:heading (:args c))]
      (let [{:keys [length max-length]} (:args c)
            [sx sy sz] (or (:start (ctx/mem c)) (reach/standing-vec (:primitives c)))
            [dx dz] (shape/headings h)
            n (inc (or length (max 1 (dec max-length))))]
        [(+ sx (* n dx)) sy (+ sz (* n dz))]))))

(defn spans
  "[horizontal vertical] blocks from feet to target."
  [feet target]
  (let [[dx dy dz] (map - target feet)]
    [(+ (js/Math.abs dx) (js/Math.abs dz)) (js/Math.abs dy)]))

(defn steps-estimate [feet target] (apply max (spans feet target)))

(defn cells-estimate
  "About how many cells a tunnel from feet to target cuts: 3 a stair step down or up, 2 a flat run step."
  [feet target]
  (let [[h v] (spans feet target)]
    (+ (* 3 v) (* 2 (max 0 (- h v))))))

(defn bad-target? [t] (not (and (vector? t) (every? int? t))))

(def cause-keys [:cell :hazards :zone :zones :claim :plan :fluid :block :tool :why :headings :in :fetch])

(defn ending-result
  "The old child result of ending e: its data with a :status and a :reason."
  [e]
  (merge {:status (if (= :done (:kind e)) :done :stopped) :reason (contract/cause-key e)} (:data e)))

(defn stop-of
  "The stop map of a child's ending e that did not end :done: its cause and data fields, its reason, and where it was."
  [e in]
  (assoc (select-keys (merge (:data e) (when (map? (:cause e)) (:cause e))) (conj cause-keys :walk)) :reason (contract/cause-key e) :in in))

(defn run-index
  "k when feet is the run's cell k steps from end, 0 <= k <= run; else nil."
  [end feet heading run]
  (first (filter #(= feet (ln/ahead end heading %)) (range 0 (inc run)))))
