(ns jobs.survival.retreat-flight
  "The state of one flight (jobs.survival.retreat): the tried options and sweeps, who still chases, and the flight's end."
  (:require [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.reach :as reach]
            [jobs.lib.result :as r]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]))

(def wait-ms "One hold of a hidden or cornered body before it looks again." 1000)

(defn dead-ids
  "The ids of hostiles a cornered fight has killed: their corpses may stay listed a while."
  [c]
  (vec (:dead (ctx/mem c))))

(defn tried? [c option] (contains? (:tried (ctx/mem c)) option))

(defn tried! [c option] (ctx/update-mem! c update :tried (fnil conj #{}) option))

(defn near-known
  "The hostiles p can be said to know within radius (ranged ones within ranged-radius), the dead skipped."
  [p dead radius ranged-radius]
  (remove #(contains? dead (.-id %)) (danger-q/known-hostiles p radius {:ranged-radius ranged-radius})))

(defn count-sweep
  "mem with one more sweep of every option failed."
  [mem]
  (update mem :sweeps (fnil inc 0)))

(defn cannot-escape?
  "Whether two sweeps of every option failed with no gain between."
  [mem]
  (>= (:sweeps mem 0) 2))

(defn note-gap
  "mem after a flight step that left the nearest chaser gap blocks off: a gain of a block over the best gap resets the
  sweeps and the no-gain count, else the count grows. No gap (no hostile near) is no sample: the flight is succeeding,
  the no-gain count resets and the best gap stays."
  [mem gap]
  (let [best (:best-gap mem)]
    (cond
      (nil? gap) (assoc mem :since-gain 0)
      (or (nil? best) (>= gap (inc best)))
      (assoc mem :best-gap gap :since-gain 0 :sweeps 0)
      :else
      (update mem :since-gain (fnil inc 0)))))

(defn nearest-gap
  "Blocks from the body to the nearest of hostiles, nil with none."
  [p hostiles]
  (when (seq hostiles)
    (apply min (map #(danger-q/mob-distance p %) hostiles))))

(defn no-gain?
  "Whether the flight went n steps without a gain."
  [mem n]
  (>= (:since-gain mem 0) n))

;; ------------------------------------------------------------------ who still chases

(defn stopped-chasing
  "Pure: nil while a mob still chases, else why it stopped. m is the mob as seen now ({:mob :distance :in-line? :way?};
  :way? false: no walkable way, nor a line of fire for a ranged mob), nil when no longer listed (dead, despawned, out
  of tracking); seen-t when it was last in line. Vanilla: a mob drops its target past its follow range or a while out
  of sight."
  [m seen-t now lost-ms]
  (cond
    (nil? m) :gone
    (> (:distance m) (threats/follow-range (:mob m))) :far
    (and (not (:in-line? m)) (> (- now seen-t) lost-ms)) :lost
    (false? (:way? m)) :closed
    :else nil))

(defn seen-now
  "What stopped-chasing needs of hostile e (a JS entity; visible: a clear line from the eye, whichever way the body
  faces). A way: a walkable way to the body, or for a ranged mob a line of fire (a shut door takes both)."
  [p e]
  {:mob (.-name e) :distance (danger-q/mob-distance p e) :in-line? (true? (.-visible e))
   :way? (or (reach/walkable-way? p (danger-q/mob-pos p e) (u/pos-of (.-pos (.self p))))
             (and (combat/ranged? e) (danger-q/danger? p e {:sight? false})))})

(defn known-chasers
  "The hostiles the body knows of (seen or heard, or remembered where last sensed) within the longest follow range."
  [p]
  (danger-q/known-hostiles p threats/max-follow-range {}))

(def resume-gap-ms "A flight cut for longer than this starts its clocks afresh when it resumes." 5000)

(defn resume-flight
  "mem of a flight resumed at now: after a gap past resume-gap-ms since its last step, the flight starts now and every
  chaser counts as just seen (the world is judged afresh); else as it was."
  [mem now]
  (if (or (nil? (:last-step mem)) (<= (- now (:last-step mem)) resume-gap-ms))
    mem
    (cond-> (assoc mem :flight-start now :last-step now)
      (:chasers mem) (update :chasers update-vals #(assoc % :seen-t now)))))

(defn chaser-entry
  "A chaser's flight entry: its id, uuid, name and place (threats/place-of: exact only when seen)."
  [p e now]
  (merge {:id (.-id e) :uuid (.-uuid e) :mob (.-name e) :seen-t now} (threats/place-of p e)))

(defn look!
  "Update the flight's chasers (job memory :chasers, by id): the real dangers within :radius (ranged :ranged-radius)
  join or are seen afresh; one that stopped chasing (stopped-chasing) moves to :fled with :ended why. The chasers
  still on, as JS entities, nearest first."
  [c]
  (let [{:keys [radius ranged-radius lost-s]} (:args c)
        p (:primitives c)
        now (ctx/now c)
        dead (set (dead-ids c))
        live (remove #(dead (.-id %)) (known-chasers p))
        listed (into {} (map (juxt #(.-id %) identity)) live)
        joining (remove #(dead (.-id %)) (danger-q/dangers p radius {:ranged-radius ranged-radius} {:sight? false}))
        chasers (merge (:chasers (ctx/mem c)) (into {} (map (juxt #(.-id %) #(chaser-entry p % now))) joining))
        judged (for [[id ch] chasers
                     :let [e (listed id)
                           seen-t (if (and e (true? (.-visible e))) now (:seen-t ch))
                           ch (cond-> (assoc ch :seen-t seen-t) e (-> (dissoc :pos :direction :band :from) (merge (threats/place-of p e))))]]
                 {:id id :ch ch :e e :why (stopped-chasing (some->> e (seen-now p)) seen-t now (* 1000 lost-s))})
        on (filter #(nil? (:why %)) judged)
        off (remove #(nil? (:why %)) judged)]
    (ctx/update-mem! c #(-> %
                            (assoc :chasers (into {} (map (juxt :id :ch)) on))
                            (update :fled (fnil into []) (map (fn [{:keys [ch why]}] (assoc ch :ended why))) off)))
    (sort-by #(danger-q/mob-distance p %) (map :e on))))

(defn end-flight!
  "Write a :threat entry per mob fled (jobs.lib.threats; one per mob, its last way out) and end the flight: done with
  {:fled [ids] :ended}, :ended why the last chaser stopped (or ended, given for the chasers still on: :hidden); ended
  :cannot-escape stops."
  [c ended]
  (let [{:keys [chasers fled]} (ctx/mem c)
        by-id (merge (into {} (map (juxt :id identity)) fled)
                     (into {} (map (fn [[id ch]] [id (assoc ch :ended ended)])) chasers))
        all (vals by-id)
        why (or ended (:ended (peek (vec fled))) :none)
        ids (mapv :id all)]
    (doseq [t all] (threats/remember! c t))
    (ctx/update-mem! c dissoc :chasers :fled)
    (if (= :cannot-escape ended)
      (r/stop! c :cannot_escape "every way of escaping failed twice in a row" :fled ids)
      (r/finish! c {:fled ids :ended why}))))
