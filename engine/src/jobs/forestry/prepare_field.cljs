(ns jobs.forestry.prepare-field
  "Reading the field for jobs.forestry.prepare: the planned cells, their assessed states, the cells to work on, and
  what the access rules say of digging or placing at a cell."
  (:require [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.forestry.prepare-rule :as rule]
            [jobs.lib.util :as u]
            [jobs.lib.tidy-rules :as tidy]
            [jobs.lib.forestry :as lib]
            [jobs.lib.world :as known]))

;; ------------------------------------------------------------------ reading the field

(defn skipped
  "{pos {:pos :reason}} of the cells this call gave up on."
  [c]
  (:skipped (ctx/mem c) {}))

(defn planned
  "{:trees {pos species} :planned #{[x y z]}} for the plan, or {:trouble text} (warned once per reason)."
  [c]
  (let [{:keys [plan part]} (:args c)
        answer (known/plan c plan)
        trees (lib/tree-cells answer part)
        no-zones (when (and (nil? (known/zones c)) (not (:ignore-zones? (:args c)))) "no zone list")
        trouble (or (lib/plan-trouble answer trees) no-zones)]
    (if-not trouble
      {:trees trees :planned (set (map (comp vec :pos) (:cells answer)))}
      (do (ctx/warn-once! c [plan trouble] :prepare.declined
                          {:plan plan :part part :reason trouble
                           :text (str "prepare declines plan " plan (when part (str " part " part)) ": " trouble)})
          {:trouble trouble :later? (or (known/unreadable? answer) (= no-zones trouble))}))))

(declare place-verdict)

(defn settle-wet
  "The assessed cell with a water state checked against what is known now: a :fill / :dam the access rules refuse is
  :wet with the reason, and an untraced cell beside a dam of this call is :receding until the water is gone."
  [c {:keys [state why pos] :as cell}]
  (let [v (when (#{:fill :dam} state) (place-verdict c (lib/cell-vec (:target cell))))
        dammed? (contains? (:recede (ctx/mem c)) pos)]
    (cond
      (vector? v) (assoc cell :state :wet :why (second v))
      (and (= :wet state) (= :untraced why) dammed?) (assoc cell :state :receding)
      :else cell)))

(defn assessments
  "{pos {:pos :species :state ...}} of every planned tree cell, read now."
  [c {:keys [trees planned]}]
  (let [m (ctx/mem c)
        world {:planned planned
               :holes (set (:holes m))
               :carried (u/carried-names (:primitives c))
               :over (:headroom (:args c))}]
    (into {} (map (fn [[pos species]]
                    [pos (settle-wet c (assoc (rule/assess (:primitives c) pos species world) :pos pos :species species))]))
          trees)))

(defn todo
  "The cells with a step to take, not skipped, nearest to the body first."
  [c states]
  (let [skip (skipped c)
        here (u/self-pos c)]
    (->> (vals states)
         (filter #(and (rule/steps (:state %)) (not (contains? skip (:pos %)))))
         (sort-by #(u/dist here (:pos %))))))

(defn in-column?
  "Whether the body stands in the column of pos."
  [c pos]
  (let [[x _ z] (lib/feet-cell c)]
    (and (= x (:x pos)) (= z (:z pos)))))

(defn dig-verdict
  "What the access rules say of digging the cell pos: :ok, :wait (not loaded) or [:refuse reason]."
  [c pos]
  (let [d (tidy/decide c pos)]
    (cond
      (= :dig d) :ok
      (= :skip d) :wait
      (= :refuse (first d)) [:refuse (:reason (second d))]
      :else [:refuse (first (second d))])))

(defn place-verdict
  "What the access rules say of placing at the cell pos: :ok, :wait (not loaded, or the body in it) or [:refuse reason]."
  [c pos]
  (let [p (:primitives c)
        v (rules/may-place? (merge {:block-at (fn [[x y z]] (u/seen-name p {:x x :y y :z z})) :cell pos
                                    :feet (lib/feet-cell c) :ledger #{}}
                                   (tidy/access-world c)))]
    (cond
      (:ok v) :ok
      (#{:not-loaded :own-body} (:reason v)) :wait
      :else [:refuse (:reason v)])))
