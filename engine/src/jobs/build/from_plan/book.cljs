(ns jobs.build.from-plan.book
  "What jobs.build.from-plan books in its memory as it places and digs cells, and how a build ends."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.contract :as contract]
            [jobs.lib.job-common :as common]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.util :as u]
            [jobs.build.from-plan.cells :refer [pos-map]]
            [plan.shape :as shape]
            [jobs.lib.build.plan :as plan :refer [retry-distance max-retries count-fail decide rules-input refuse how misplaced]]
            [jobs.lib.build.text :refer [summary stopped-text assoc-fetch-failed]]))

(defn walk-fail
  "count-fail for a failed walk or an out-of-reach arrival, which the job's own stand choice caused: a cell it gives up
  is noted with where the body stood (:walk-given), for retry-walks."
  [m pos reason limit at]
  (let [m2 (count-fail m pos reason limit)]
    (cond-> m2
      (and (contains? (:given-up m2) pos) (not (contains? (:given-up m) pos))) (assoc-in [:walk-given pos] at))))

(defn retry-walks
  "m with the cells given up for a walk opened again (given-up and :walk-given dropped, :retries counted): those the body
  stands retry-distance from where it gave up, all of them when finishing?; a cell opened max-retries times stays."
  [m body finishing?]
  (let [open (filter (fn [[pos at]]
                       (and (< (get-in m [:retries pos] 0) (max-retries))
                            (or finishing? (>= (u/dist body at) (retry-distance)))))
                     (:walk-given m))]
    (reduce (fn [m [pos _]]
              (-> m (update :given-up dissoc pos) (update :walk-given dissoc pos) (update-in [:retries pos] (fnil inc 0))))
            m open)))

(defn warn-gap!
  "A build.gap warn when item, a fence or wall just placed at pos, stands over a seen cell of air: an animal walks out under it."
  [c pos item]
  (let [below (u/seen-name (:primitives c) (zipmap [:x :y :z] (update pos 1 dec)))]
    (when (and (re-find #"_(fence|wall)$" item) below (shape/air? below))
      (ctx/emit! c :build.gap :warn {:plan (:plan (:args c)) :cells [pos]
                                     :text (str "build of " (:plan (:args c)) ": " item " at " (pr-str pos) " stands over air, a gap under it")}))))

(defn way-in-cells
  "The cells of the batch that the body has stood in (feet or head), above the plan's lowest layer, that are not a door,
  gate or trapdoor: filling them shuts the way the body came in by."
  [c cells batch]
  (let [floor-y (apply min (map #(get (:pos %) 1) cells))
        stood (:stood (ctx/mem c))]
    (vec (for [{:keys [pos item]} batch
               :when (and (> (get pos 1) floor-y) (contains? stood pos) (not (re-find #"_(door|trapdoor|fence_gate)$" (str item))))]
           pos))))

(defn warn-way-in!
  "A build.way-in warn, once per cell, for cells about to be filled that the body stood in."
  [c cells batch]
  (let [fresh (remove (set (:way-warned (ctx/mem c))) (way-in-cells c cells batch))]
    (when (seq fresh)
      (ctx/update-mem! c update :way-warned (fnil into []) fresh)
      (ctx/emit! c :build.way-in :warn {:plan (:plan (:args c)) :cells (vec fresh)
                                        :text (str "build of " (:plan (:args c)) ": filling " (str/join ", " (map pr-str (take 3 fresh)))
                                                   ", where the body stood: the way in may close, the plan has no door there")}))))

(defn ^:async place-one!
  "Place the cell's item after asking the access rules and jobs.lib.placement once more; a refusal is booked, nothing
  is placed."
  [c {:keys [pos item want] :as cell} limit]
  (let [d (decide (rules-input c) (:accept (:args c)) pos)
        h (how c cell)]
    (cond
      (vector? d) (ctx/update-mem! c refuse pos (second d))
      (not= :place d) nil
      (:refused h) (ctx/update-mem! c assoc-in [:unplaceable pos] (:refused h))
      :else
      (let [_ (ctx/update-mem! c assoc :placing pos)
            e (await (blocks/place-call! c (zipmap [:x :y :z] pos) item
                                         {:for-plan (:plan (:args c)) :ignore-zones? (boolean (:ignore-zones? (:args c)))
                                          :click (:click h) :clear false}))
            outcome (blocks/place-outcome e)]
        (case outcome
          :continue :continue
          :placed (let [wrong (misplaced want item (:block (:data e)))]
                    (warn-gap! c pos item)
                    (ctx/update-mem! c #(cond-> (update (dissoc % :placing) :placed (fnil inc 0))
                                          wrong (assoc-in [:misplaced pos] wrong))))
          :already (ctx/update-mem! c dissoc :placing)
          (:occupied :need :no-item :no-support :unreachable) (ctx/update-mem! c #(count-fail (dissoc % :placing) pos (if (= :need outcome) :no-item outcome) limit))
          (ctx/update-mem! c #(count-fail (dissoc % :placing) pos :refused limit)))))))

(defn settle-placing!
  "A place that a cut left unbooked (:placing, set before the act): the world is the answer, so a cell the plan now
  judges :match was placed and counts. Another body placing the same block there in the gap would count too."
  [c cells]
  (when-let [pos (:placing (ctx/mem c))]
    (let [placed? (boolean (some #(and (= pos (:pos %)) (= :match (:answer %))) cells))]
      (ctx/update-mem! c #(cond-> (dissoc % :placing) placed? (update :placed (fnil inc 0)))))))

(defn ^:async dig-one!
  "Dig the wrong block of the cell (a blocks.dig child) after asking the access rules once more; a refusal is
  booked, a failed dig counts a failure, a dug cell counts a dig (given up as :refilled when it keeps coming back)."
  [c {:keys [pos]} limit]
  (let [v (access/may-dig? (rules-input c) (pos-map pos))
        accept (set (:accept (:args c)))
        judged (access/judge v accept)]
    (case judged
      :ok (let [outcome (await (blocks/dig-cell! c (pos-map pos)
                                                 {:accept accept
                                                  :ignore-zones? (boolean (:ignore-zones? (:args c)))
                                                  :for-plan (:plan (:args c))}))]
            (case outcome
              :continue :continue
              :dug (ctx/update-mem! c #(let [m (update-in % [:digs pos] (fnil inc 0))]
                                         (if (>= (get-in m [:digs pos]) limit)
                                           (assoc-in m [:given-up pos] :refilled)
                                           m)))
              :missing (ctx/update-mem! c count-fail pos :missing limit)
              (ctx/update-mem! c count-fail pos :refused limit)))
      :refused (ctx/update-mem! c refuse pos (select-keys v [:reason :zone :plan :claim]))
      :hazard (ctx/update-mem! c refuse pos {:reason :hazard
                                             :hazards (vec (distinct (remove accept (map access/hazard-key (:hazards v)))))})
      nil)))

(defn result-data
  "What the build left (text/summary), with the failed fetches of the call."
  [c cells]
  (assoc-fetch-failed c (summary c cells (ctx/mem c) (plan/sturdy-ground? c))))

(defn ending-of
  "The ending of a build with nothing more to do: done when nothing is missing (and no block came out wrong: nothing-possible :wrong-blocks) or the placed cells meet the call's
  :least; else by what holds the rest back: given-up cells (nothing-possible :incomplete), items not carried and not
  fetched (condition-failed :no-items), cells refused by the rules (condition-failed :refused)."
  [c {:keys [placed missing short given-up refused wrong] :as result}]
  (let [data (assoc result :text (stopped-text (:plan (:args c)) result))]
    (cond
      (and (empty? missing) (seq wrong)) (ctx/end c :nothing-possible :wrong-blocks data)
      (or (empty? missing) (common/enough? c placed (+ placed (count missing)))) (ctx/end c :done nil result)
      (seq given-up) (ctx/end c :nothing-possible :incomplete data)
      (seq short) (ctx/end c :condition-failed {:reason :no-items :needs short} data)
      (seq refused) (ctx/end c :condition-failed :refused data)
      :else (ctx/end c :nothing-possible :incomplete data))))

(defn finish! [c cells]
  (ending-of c (result-data c cells)))

(defn book-failed!
  "Book the failed fetch for wait w (not asked again in this call); the ending names it in :fetch-failed."
  [c w e]
  (let [cause (:cause e)
        failed (if (map? cause) (or (:why cause) (:reason cause)) (contract/cause-key e))]
    (ctx/update-mem! c #(-> % (assoc-in [:fetch-failed (plan/fetch-key w)] {:failed failed})))))
