(ns jobs.lib.shelter.spawn
  "The respawn bed as the body knows it, and the beds it knows of."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::spawn-near {:default 3 :spec (a/int-in 0 nil)
                 :doc "Blocks within which a :spawn-set or :spawn-bed entry counts as the respawn point of a bed (either half)."}})

(defn spawn-near [] (settings/get settings ::spawn-near))

(def spawn-bed-policy {:cap 1 :ttl :forever})

(defn remember-spawn-bed!
  "Record bed as the respawn bed."
  [c bed]
  (ctx/remember! c :spawn-bed {:pos bed} spawn-bed-policy))

(defn spawn-bed-entry
  "The latest :spawn-bed entry, live or not."
  [view]
  (mem/latest view :spawn-bed))

(defn spawn-ended-at
  "When the respawn point was last reset (:spawn-reset, the server's word) or found lost (:spawn-lost), or nil."
  [view]
  (some->> [:spawn-reset :spawn-lost] (keep #(:t (mem/latest view %))) seq (apply max)))

(defn spawn-bed-live
  "The durable :spawn-bed entry of view unless a :spawn-reset or :spawn-lost came after it."
  [view]
  (let [e (spawn-bed-entry view)
        ended (spawn-ended-at view)]
    (when-not (and e ended (> ended (:t e)))
      e)))

(defn spawn-lost-pos
  "The position of the respawn bed the body knows is gone: the latest :spawn-bed when a reset or loss came after it,
  or the live one whose block is seen and no longer a bed."
  [p view]
  (let [e (spawn-bed-entry view)
        pos (:pos (:data e))
        ended (spawn-ended-at view)]
    (cond
      (nil? pos) nil
      (and ended (> ended (:t e))) pos
      (let [n (u/seen-name p pos)] (and n (not (.endsWith n "_bed")))) pos)))

(defn seen-not-bed?
  "Whether the block at pos is seen and is no bed."
  [p pos]
  (let [n (u/seen-name p pos)]
    (and (some? n) (not (.endsWith n "_bed")))))

(defn known-beds
  "The remembered :bed place and the live :spawn-bed (unless its block is seen not a bed), at any distance, nearest first."
  [p view]
  (let [me (u/self-pos {:primitives p})
        spawn (:pos (:data (spawn-bed-live view)))
        place (mem/place view :bed)
        ok? #(or (nil? %) (not (seen-not-bed? p %)))]
    (->> [(when (ok? place) place) (when (ok? spawn) spawn)]
         (filter some?)
         distinct
         (sort-by #(u/dist me %)))))

(defn standing-beds
  "The known-beds within radius, nearest first (the :bed place's job retracts it on finding it gone; the live
  :spawn-bed is set to a bed the body slept in or set the spawn at, whichever half the place key holds)."
  [p view radius]
  (let [me (u/self-pos {:primitives p})]
    (filter #(<= (u/dist me %) radius) (known-beds p view))))

(defn standing-bed
  "The nearest of standing-beds, or nil."
  [p view radius]
  (first (standing-beds p view radius)))

(defn spawn-state
  "What the body knows of its respawn point: {:bed pos} (the live :spawn-bed), {:lost pos :why w} (:why the :spawn-lost entry's,
  :reset, or :seen-gone), else nil."
  [p view]
  (if-let [pos (spawn-lost-pos p view)]
    (let [t (:t (spawn-bed-entry view))
          after? (fn [kind] (when-let [e (mem/latest view kind)] (> (:t e) t)))]
      {:lost pos :why (cond (after? :spawn-lost) (:why (:data (mem/latest view :spawn-lost)))
                            (after? :spawn-reset) :reset
                            :else :seen-gone)})
    (when-let [e (spawn-bed-live view)]
      {:bed (:pos (:data e))})))

(def spawn-lost-policy {:cap 1 :ttl :forever})

(defn lose-spawn!
  "A job found the bed at pos gone or cut off: when it is the live respawn bed, remember :spawn-lost and warn
  spawn_lost once (why: a short word)."
  [c pos why]
  (let [view (ctx/view c)]
    (when (some-> (:pos (:data (spawn-bed-live view))) (u/dist pos) (<= (spawn-near)))
      (ctx/remember! c :spawn-lost {:pos pos :why why} spawn-lost-policy)
      (ctx/emit! c :spawn_lost :warn {:pos pos :why why :text (str "the respawn bed at " (pr-str pos) " is gone (" (name why) "): the next death respawns at the world spawn")}))))

(defn retract-bed!
  "A bed cleanup took (an item ending _bed among the removed cells) is no longer the remembered :bed or :home, nor the respawn bed."
  [c removed]
  (when-let [pos (some-> (first (filter #(.endsWith (str (:item %)) "_bed") removed)) :cell u/cell-pos)]
    (doseq [kind [:bed :home]
            :when (= (mem/place (ctx/view c) kind) pos)]
      (ctx/remember! c kind {:gone true :was pos} mem/place-policy))
    (lose-spawn! c pos :taken)))

(defn spawn-bed-at?
  "Whether the live :spawn-bed is within spawn-near of pos (the bed's other half counts)."
  [view pos]
  (boolean (some-> (:pos (:data (spawn-bed-live view))) (u/dist pos) (<= (spawn-near)))))

(defn spawn-known?
  "Whether the body has seen the respawn point set at bed since t0, or earlier and not reset since (the server says it
  only when the point changes, so a bed slept in before is silent; :spawn-bed outlives the :spawn-set entry)."
  [view bed t0]
  (let [near? #(some-> (:pos (:data %)) (u/dist bed) (<= (spawn-near)))
        reset-t (:t (mem/latest view :spawn-reset))
        current? #(or (>= (:t %) t0) (nil? reset-t) (> (:t %) reset-t))]
    (boolean (or (some #(and (current? %) (or (>= (:t %) t0) (near? %))) (mem/entries view :spawn-set))
                 (some-> (spawn-bed-live view) near?)))))
