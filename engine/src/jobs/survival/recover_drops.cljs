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
  :collect-radius (jobs.forestry.collect-drops) and record :collected with the
  count of the pile's items that entered the inventory since the decision (those
  picked up on the walk count; a pile already carried again ends :collected
  without a collect step). Gives
  up as :abandoned when the point is unreachable, nothing is left there, or
  the five minute despawn window closes. Nothing is decided or walked until
  a :respawned entry newer than the death exists (the body is alive again); the
  window still closes meanwhile. A hostile within :danger-radius
  makes the round yield without acting, so a reflex can deal with it; the
  trip resumes afterwards. The check holds while a death is unrecovered,
  however old, so an expired trip still gets to write :abandoned.")

(def args
  {:margin {:doc "added to the retrieval cost before comparing it to the value" :default 0}
   :danger-radius {:doc "a hostile this close makes the job yield without acting" :default 8}
   :collect-radius {:doc "collect drops within this many blocks of the death point" :default 6}})

(def arrive-range 2)
(def settle-ms
  "After a respawn the client needs this long to re-receive nearby entities;
  an estimate made sooner sees no hostiles."
  2000)
(def day-ms (* 24 60 60 1000))
(def recovered-policy {:cap 10 :ttl day-ms})

(defn check [c] (some? (died/unrecovered-death (ctx/view c))))

(defn finite [n] (if (js/isFinite n) n :infinite))

(defn decision-text [decision {:keys [value cost reason items]} margin]
  (case decision
    :skip (str "skip: value " value " <= cost " (if (= :infinite cost) "infinite" (.toFixed cost 1)) " (+ margin " margin ")")
    :collected (str "collected " items " items")
    :abandoned (str "abandoned: " (name (or reason :unknown)))))

(defn finish!
  "Write the :recovered entry, report the decision and end the job."
  [c decision fields]
  (let [pos (:pos (:data (died/unrecovered-death (ctx/view c))))]
    (ctx/remember! c :recovered (merge {:decision decision} fields) recovered-policy)
    (ctx/emit! c :recover-drops.decided :info
               (assoc (select-keys fields [:value :cost :reason :items])
                      :decision decision :pos pos
                      :text (decision-text decision fields (:margin (:args c)))))
    :done))

(defn settling?
  "True while the latest :respawned entry, newer than the death, is younger than settle-ms."
  [c entry]
  (let [respawned (ctx/latest c :respawned)]
    (boolean (and respawned
                  (> (:t respawned) (:t entry))
                  (< (- (ctx/now c) (:t respawned)) settle-ms)))))

(defn key-to-death!
  "Reset the job memory when it belongs to another death than entry."
  [c entry]
  (when (not= (:t entry) (:death-t (ctx/mem c)))
    (ctx/update-mem! c (constantly {:death-t (:t entry)}))))

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

(defn carried-counts
  "{item name total} of what the body carries."
  [c]
  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} (u/inventory (:primitives c))))

(defn recovered-items
  "How many of the pile's items entered the inventory since the baseline (taken when the
  trip was decided): per item name the rise, capped by what the pile held. Items picked up
  on the walk count as well as those the collect step picks up."
  [c pile]
  (let [baseline (:baseline (ctx/mem c))
        now (carried-counts c)
        wanted (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} pile)]
    (reduce + 0 (map (fn [[name n]] (min n (max 0 (- (get now name 0) (get baseline name 0))))) wanted))))

(defn ^:async collect! [c radius pile]
  (let [r (if (>= (recovered-items c pile) (reduce + 0 (map :count pile)))
            :done
            (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius radius})))
        items (recovered-items c pile)]
    (cond
      (= :continue r) :continue
      (pos? items) (finish! c :collected (assoc (:decided (ctx/mem c)) :items items))
      :else (finish! c :abandoned (assoc (:decided (ctx/mem c)) :items 0 :reason :nothing-found)))))

(defn ^:async round [c]
  (let [{:keys [margin danger-radius collect-radius]} (:args c)
        entry (died/unrecovered-death (ctx/view c))
        elapsed (- (ctx/now c) (:t entry))
        threatened? (seq (entity-positions (:primitives c) danger-radius "hostile" 1))]
    (when entry (key-to-death! c entry))
    (cond
      (nil? entry) :done
      (>= elapsed value/despawn-ms) (finish! c :abandoned (assoc (:decided (ctx/mem c)) :reason :window-closed))
      threatened? :continue
      (not (died/respawned-since? (ctx/view c) entry)) :continue
      (and (nil? (:decided (ctx/mem c))) (settling? c entry)) :continue
      :else
      (let [decided (or (:decided (ctx/mem c))
                        (let [e (estimate c (:data entry) elapsed)]
                          (ctx/update-mem! c assoc :decided {:value (:value e) :cost (finite (:cost e))}
                                           :baseline (carried-counts c))
                          {:value (:value e) :cost (finite (:cost e))}))
            pos (:pos (:data entry))]
        (cond
          (<= (:value decided) (+ (if (= :infinite (:cost decided)) js/Infinity (:cost decided)) margin))
          (finish! c :skip decided)

          (= :collect (:phase (ctx/mem c))) (await (collect! c collect-radius (:inventory (:data entry))))
          :else (await (go! c pos)))))))
