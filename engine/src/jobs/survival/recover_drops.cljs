(ns jobs.survival.recover-drops
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.triggers.died :as died]
            [engine.value :as value]))

(def doc
  "After a death, go back for the drops when they are worth it. Round: take
  the latest :died entry with no newer :recovered. Skip (a :recovered entry
  with :decision :skip) when engine.value/inventory-value of what was carried
  is at most engine.value/retrieval-cost plus :margin. Otherwise walk to the
  death point (jobs.movement.go-to), then collect everything within
  :collect-radius (jobs.forestry.collect-drops) and record :collected. Gives
  up as :abandoned when the point is unreachable, nothing is left there, or
  the five minute despawn window closes. A hostile within :danger-radius
  makes the round yield without acting, so a reflex can deal with it; the
  trip resumes afterwards. The check holds while a death is unrecovered,
  however old, so an expired trip still gets to write :abandoned.")

(def args
  {:margin {:doc "added to the retrieval cost before comparing it to the value" :default 0}
   :danger-radius {:doc "a hostile this close makes the job yield without acting" :default 8}
   :collect-radius {:doc "collect drops within this many blocks of the death point" :default 6}})

(def arrive-range 2)
(def day-ms (* 24 60 60 1000))
(def recovered-policy {:cap 10 :ttl day-ms})

(defn check [c] (some? (died/unrecovered-death (ctx/view c))))

(defn finite [n] (if (js/isFinite n) n :infinite))

(defn finish!
  "Write the :recovered entry and end the job."
  [c decision fields]
  (ctx/remember! c :recovered (merge {:decision decision} fields) recovered-policy)
  :done)

(defn entity-positions [p radius kind max]
  (map (fn [e] (u/pos-of (.-pos e))) (array-seq (.entities p #js {:radius radius :kind kind :max max}))))

(defn estimate
  "{:value :cost} of recovering the drops of the :died data."
  [c {:keys [pos inventory cause experience]} elapsed]
  (let [p (:primitives c)]
    {:value (value/inventory-value inventory (:level experience))
     :cost (value/retrieval-cost pos (u/self-pos c) (entity-positions p 64 "hostile" 32) cause elapsed)}))

(defn ^:async go! [c pos]
  (let [r (await (ctx/call-child c :go 'jobs.movement.go-to {:pos pos :range arrive-range}))]
    (cond
      (not= :done r) :continue
      (not (:arrived (ctx/child-result c :go))) (finish! c :abandoned (assoc (:decided (ctx/mem c)) :reason :unreachable))
      :else (do (ctx/update-mem! c assoc :phase :collect) :continue))))

(defn ^:async collect! [c radius]
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius radius}))
        items (:collected (ctx/child-result c :collect) 0)]
    (cond
      (= :continue r) :continue
      (pos? items) (finish! c :collected (assoc (:decided (ctx/mem c)) :items items))
      :else (finish! c :abandoned (assoc (:decided (ctx/mem c)) :items 0 :reason :nothing-found)))))

(defn ^:async round [c]
  (let [{:keys [margin danger-radius collect-radius]} (:args c)
        entry (died/unrecovered-death (ctx/view c))
        elapsed (- (ctx/now c) (:t entry))
        threatened? (seq (entity-positions (:primitives c) danger-radius "hostile" 1))]
    (cond
      (nil? entry) :done
      (>= elapsed value/despawn-ms) (finish! c :abandoned (assoc (:decided (ctx/mem c)) :reason :window-closed))
      threatened? :continue
      :else
      (let [decided (or (:decided (ctx/mem c))
                        (let [e (estimate c (:data entry) elapsed)]
                          (ctx/update-mem! c assoc :decided {:value (:value e) :cost (finite (:cost e))})
                          {:value (:value e) :cost (finite (:cost e))}))
            pos (:pos (:data entry))]
        (cond
          (<= (:value decided) (+ (if (= :infinite (:cost decided)) js/Infinity (:cost decided)) margin))
          (finish! c :skip decided)

          (= :collect (:phase (ctx/mem c))) (await (collect! c collect-radius))
          :else (await (go! c pos)))))))
