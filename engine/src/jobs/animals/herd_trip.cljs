(ns jobs.animals.herd-trip
  "One animal's trip for jobs.animals.herd: leashing the next, regathering, the steps through the gate and the release."
  (:require [jobs.lib.animals :as animals]
            [engine.ctx :as ctx]
            [jobs.lib.pen :as pen]
            [jobs.lib.util :as u]
            [jobs.animals.herd-run :refer [animal-now deepest end! finish! gate-open? in-pen-adults near-pen out-1 read-pen regather-radius set-phase!]]
            [jobs.animals.herd-pen :refer [animal-pos pinned-ms settle-at! step-in-step]]))


(def max-tries 2)

(defn tired-keys
  "The keys of animals leashed max-tries times already (tries {key n}) and not in the pen (a set of keys)."
  [tries pen-keys]
  (into [] (comp (filter #(>= (val %) max-tries)) (map key) (remove pen-keys)) tries))

(defn skip-keys
  "Keys never to leash: babies, animals in the pen, the ones given up on, those already fetched twice and outside."
  [c]
  (let [{:keys [mob radius]} (:args c)
        answer (read-pen c)
        near (animals/herd (:primitives c) mob (+ radius near-pen))
        in-pen? #(pen/in-pen? answer (u/pos-of (.-pos %)))
        skip? #(or (true? (.-baby %)) (in-pen? %))
        pen-keys (into #{} (comp (filter in-pen?) (map animals/key-of)) near)]
    (-> (vec (keys (:given-up (ctx/mem c))))
        (into (comp (filter skip?) (map animals/key-of)) near)
        (into (tired-keys (:tries (ctx/mem c)) pen-keys)))))

(defn ^:async leash-next!
  "Lead one more animal and start its trip, or end when the pen holds enough or none more can be led."
  [c]
  (let [{:keys [mob radius target]} (:args c)
        m (ctx/mem c)]
    (if (>= (count (in-pen-adults c (read-pen c))) target)
      (finish! c nil)
      (let [r (await (ctx/call-child c :leash 'jobs.animals.leash
                                 (cond-> {:mob mob :radius radius :skip (skip-keys c)} (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))
            res (when (= :done r) (ctx/child-result c :leash))]
        (cond
          (nil? res) :continue
          (= :leashed (:reason res))
          (set-phase! c :approach {:animal (:animal res) :led [(:animal res)] :animal-started (ctx/now c)
                                   :tries (update (:tries m) (:animal res) (fnil inc 0))
                                   :pos-index 0 :backs 0 :retried false :deepest-tried false})
          (and (empty? (:brought m)) (empty? (:given-up m))) (finish! c (:reason res))
          :else (finish! c nil))))))

(defn prune-led!
  "Drop from :led the animals no longer on this body's lead, booking each (:lead-broke seen, :lost not seen);
  how many were dropped."
  [c]
  (let [gone (keep (fn [k] (let [a (animal-now c k)]
                             (cond (nil? a) [k :lost]
                                   (not (animals/led-by-me? a)) [k :lead-broke])))
                   (:led (ctx/mem c)))]
    (when (seq gone)
      (ctx/update-mem! c #(-> %
                              (update :led (fn [led] (vec (remove (set (map first gone)) led))))
                              (update :given-up merge (into {} gone)))))
    (count gone)))

(defn regather-or-lose!
  "Nobody is led any more: shut the gate if it stands open, pick the leads up and start again once, else give up :lost."
  [c]
  (if (:regathered (ctx/mem c))
    (end! c :lost)
    (do (ctx/update-mem! c #(-> %
                                (assoc :regathered true :animal nil)
                                (update :given-up (fn [g] (into {} (remove (fn [[_ r]] (= :lead-broke r))) g)))))
        (set-phase! c (if (gate-open? c) :shut-gate :regather)))))

(defn ^:async regather! [c]
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius regather-radius :filter ["lead"]}))]
    (if (= :done r) (set-phase! c :leash) :continue)))



(defn released!
  "The led animal is let go: booked :brought when it stands on a pen cell, else given up :outside (or with the unleash child's reason when its lead stayed on); on to phase next."
  [c next]
  (let [k (:animal (ctx/mem c))
        there (animal-pos c)
        in? (boolean (and there (pen/in-pen? (read-pen c) there)))
        failed (get (:unleash-failed (ctx/mem c)) k)]
    (ctx/update-mem! c #(-> %
                            (dissoc :release-tried :releasing :release-declines :unleash-failed)
                            (assoc :led [] :animal nil)
                            (cond-> (and in? (not failed) (not (some #{k} (:brought %)))) (update :brought (fnil conj []) k))
                            (cond-> (and (or failed (not in?)) (not (contains? (:given-up %) k)))
                              (update :given-up assoc k (or failed :outside)))))
    (set-phase! c next)))

(defn pinned!
  "The animal stays too far behind at position index idx: how the stepping goes on (step-in-step)."
  [c idx]
  (let [m (ctx/mem c)
        backs (:backs m 0)]
    (case (step-in-step backs (boolean (:retried m)))
      :back (set-phase! c :step {:pos-index (max out-1 (dec idx)) :back-step true :backs (inc backs) :deepest-tried false})
      :retry-from-out (set-phase! c :retry-back {:backs 0 :retried true :deepest-tried false})
      :give-up (set-phase! c :give-up-walk))))

(defn step-position!
  "One round of the step through the gate: walk to the position and settle; settled goes on to the next one (the last
  to the shut), pinned to pinned!, a position reached on a back-step to the one it backed from."
  [c]
  (let [{:keys [pos-index back-step]} (ctx/mem c)]
    (settle-at! c pos-index pinned-ms
                #(cond
                   back-step (set-phase! c :step {:pos-index (inc pos-index) :back-step false})
                   (= :pinned %) (pinned! c pos-index)
                   (= pos-index deepest) (set-phase! c :shut-behind)
                   :else (set-phase! c :step {:pos-index (inc pos-index)})))))
