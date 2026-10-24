(ns world-test.arrow-shots
  "tools/event-counts.mjs arrow-shots: counts the measured prior jobs.lib.cost.threat ::arrow-hits from the bodies' event logs.
  A meet is a respond run's first decision about a seen bow mob (its :pos is the mob's), to flee or to fight, the run deciding
  about no other kind, from where the body stood (a position within pos-age-ms before) until the run ends, decides otherwise, or
  the body dies.
  The mob stands where it was seen while the body is within its fire range (vanilla's ranged goals), so the seconds the body
  spends within that range of the spot, between two known positions half each end's, are its exposure; its shots are those
  seconds at threat/shot-rate of the difficulty, its hits the hurt events from its kind (:hits each) during the meet. A mob
  seen beyond its range walks in (vanilla), so meets from there are reported apart and not in :in-range.
  Pure part: meets, report."
  (:require [jobs.lib.cost.threat :as threat]))

(def pos-age-ms "Longest age of the position a flight is measured from." 2000)

(def bands "Distance bands, blocks from the mob at the decision, the counts are split by." [[0 5] [5 9] [9 15] [15 ##Inf]])

(defn dist [a b] (js/Math.hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b))))

(defn meets
  "Rows [{:id [gen job] :mob :decision :d0 :in-s :hits :end}] of the meets in records (world-test.event-counts/scan-line's, sorted by :t)."
  [records]
  (let [close (fn [st pred end] (let [{gone true kept false} (group-by (comp boolean pred) (vals (:open st)))]
                                  (-> st
                                      (assoc :open (into {} (map (juxt :id identity)) kept))
                                      (update :out into (map #(-> % (assoc :end end) (select-keys [:id :mob :decision :d0 :in-s :hits :end :mixed?]))) gone))))
        in-range (fn [{:keys [mob at]} p] (if (<= (dist p at) (threat/fire-range mob)) 1 0))
        step (fn [{:keys [last-pos] :as st} {:keys [k gen job t mob decision] :as r}]
               (let [id [gen job]]
                 (case k
                   :decided (let [f (get-in st [:open id])]
                              (cond
                                (and f (not= mob (:mob f))) (assoc-in st [:open id :mixed?] true)
                                (and f (not= decision (:decision f))) (close st #(= id (:id %)) :decided)
                                f st
                                (get-in st [:seen id]) st
                                (not (and (contains? threat/bow-mobs mob) (#{:flee :fight} decision) (:at r))) (assoc-in st [:seen id] true)
                                :else (let [p (get last-pos gen)]
                                        (cond-> (assoc-in st [:seen id] true)
                                          (and p (<= (- t (:t p)) pos-age-ms))
                                          (assoc-in [:open id] {:id id :gen gen :mob mob :decision decision :at (:at r) :d0 (dist p (:at r)) :in-s 0 :hits 0 :from (assoc p :t t)})))))
                   (:pos :hurt) (let [p (if (= :pos k) r (some-> (:at r) (assoc :t t)))
                                      st (cond-> st p (assoc-in [:last-pos gen] p))
                                      mine (filter #(= gen (:gen %)) (vals (:open st)))]
                                  (reduce (fn [st {:keys [id from] :as f}]
                                            (cond-> st
                                              p (update-in [:open id] assoc :from p
                                                           :in-s (+ (:in-s f) (* (/ (- t (:t from)) 1000) (/ (+ (in-range f from) (in-range f p)) 2))))
                                              (and (= :hurt k) (contains? (:causes r) (:mob f))) (update-in [:open id :hits] + (or (:hits r) 1))))
                                          st mine))
                   :ended (close st #(= id (:id %)) :ended)
                   :died (close st #(= gen (:gen %)) :died)
                   st)))]
    (->> (:out (close (reduce step {:last-pos {} :open {} :seen {} :out []} records) identity :logs-end))
         (remove :mixed?))))

(defn r3 [x] (/ (js/Math.round (* 1000 x)) 1000))

(defn tally
  "{:flights :in-s :shots :hits :fraction} of rows at shot rate rate: fraction the Beta(k + 1, n - k + 1) posterior mean, k hits of n shots."
  [rows rate]
  (let [in-s (reduce + 0 (map :in-s rows))
        n (* rate in-s)
        k (reduce + 0 (map :hits rows))]
    {:meets (count rows) :in-s (r3 in-s) :shots (r3 n) :hits k :deaths (count (filter #(= :died (:end %)) rows))
     :fraction (r3 (/ (inc k) (+ n 2)))}))

(defn report
  "Per decision, the tally of rows met within fire range (:in-range) and by bands of :d0, at threat/shot-rate of the difficulty."
  [rows]
  (let [rate (threat/shot-rate)
        band (fn [rs [lo hi]] (filter #(and (<= lo (:d0 %)) (< (:d0 %) hi)) rs))]
    (into {:shot-rate (r3 rate)}
          (map (fn [[decision rs]]
                 [decision {:in-range (tally (remove #(< (threat/fire-range (:mob %)) (:d0 %)) rs) rate)
                            :by-band (into (sorted-map) (map (fn [b] [(first b) (tally (band rs b) rate)])) bands)}]))
          (group-by :decision rows))))
