(ns engine.hurt
  "The :hurt log event: what hurt the body and how much, merged over a short window so a fight is one event, not one
  per hit. The adapter reports each health loss as a raw body event (amount, health, food and what a normal client
  knows: the attacker entity from the damage packet, the damage type name, the block cause lava/fire/void); this
  namespace names the cause and does the merging."
  (:require [engine.args :as a]
            [clojure.string :as str]
            [engine.settings :as settings]))

(a/defargs settings
  {:engine.hurt/window-ms {:default 1000 :spec (a/int-in 1 nil)
                           :doc "Hits within this long of the first merge into one :hurt event, ms."}})

(def type-causes
  "Damage type name (minecraft:damage_type) -> the cause the event names."
  {"fall" "fall" "fly_into_wall" "fall" "stalagmite" "fall"
   "in_fire" "fire" "on_fire" "fire" "campfire" "fire" "hot_floor" "fire"
   "lava" "lava"
   "drown" "drowning" "in_wall" "suffocation" "cramming" "cramming"
   "starve" "starvation"
   "explosion" "explosion" "player_explosion" "explosion"
   "out_of_world" "void"
   "cactus" "cactus" "sweet_berry_bush" "berry bush" "freeze" "freezing" "lightning_bolt" "lightning"
   "magic" "magic" "wither" "wither" "outside_border" "world border"})

(defn cause
  "The cause of one raw hurt body event m: the attacker's name, else the block cause the adapter saw (lava, fire,
  void), else the damage type's cause, else nil (unknown)."
  [m]
  (or (get-in m [:attacker :name])
      (:cause m)
      (get type-causes (some-> (:damageType m) (str/replace #"^minecraft:" "")))))

(defn causes-of
  "All causes of raw hurt event m, in packet order: one per damage packet of one health loss (a blast and a hit in the
  same tick are one loss), else the single cause."
  [m]
  (if-let [ds (seq (:damages m))]
    (vec (distinct (keep #(cause (merge (select-keys m [:cause]) %)) ds)))
    (some-> (cause m) vector)))

(defn new-state [] (atom {:window-ms (settings/get settings :engine.hurt/window-ms) :pending nil}))

(defn set-window!
  "Set the merge window of an engine's hurt state (ms)."
  [eng ms]
  (swap! (:hurt eng) assoc :window-ms ms))

(defn flush!
  "Emit the pending merged event, if any, through emit! (the engine's emit of a map)."
  [eng emit!]
  (let [{:keys [pending]} @(:hurt eng)]
    (when pending
      (js/clearTimeout (:timer pending))
      (swap! (:hurt eng) assoc :pending nil)
      (let [causes (vec (distinct (:causes pending)))
            amount (:amount pending)]
        (emit! {:source :body :kind :hurt :level :info
                :amount amount :hits (:hits pending) :causes causes
                :health (:health pending) :food (:food pending) :since-ms (:since-ms pending)
                :text (str "hurt " amount " by " (if (seq causes) (str/join ", " causes) "an unknown cause")
                           " (health " (:health pending) ")")})))))

(defn totals
  "{cause health} lost since the engine started, by named cause (a loss with several causes under \"several\"): an act's share is the change over it."
  [eng]
  (:totals @(:hurt eng)))

(defn record!
  "One raw hurt body event m (keyword map). The first hit of a window starts it; later hits add into it; at the
  window's end flush! emits one event with the total, the hit count, the causes, the last health and when the window started (:since-ms)."
  [eng emit! m]
  (let [cs (causes-of m)
        c (if (> (count cs) 1) "several" (first cs))]
    (when c
      (swap! (:hurt eng) update-in [:totals c] (fnil + 0) (or (:amount m) 0))))
  (let [add (fn [p] (-> (or p {:amount 0 :hits 0 :causes [] :since-ms (js/Date.now)})
                        (update :amount + (or (:amount m) 0))
                        (update :hits inc)
                        (update :causes into (causes-of m))
                        (assoc :health (:health m) :food (:food m))))
        st (swap! (:hurt eng) update :pending add)]
    (when-not (:timer (:pending st))
      (let [timer (js/setTimeout #(flush! eng emit!) (:window-ms st))]
        (swap! (:hurt eng) assoc-in [:pending :timer] timer)))))
