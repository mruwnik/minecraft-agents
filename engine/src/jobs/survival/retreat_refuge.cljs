(ns jobs.survival.retreat-refuge
  "A cornered body's refuges (jobs.survival.retreat): a pillar (retreat-pillar) or a plug (jobs.survival.shut-in), and the hiding in them."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.combat :as combat]
            [jobs.lib.pace :as pace]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.ready :as ready]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [jobs.lib.pillar :as pl]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spent :as spent]
            [jobs.lib.threats :as threats]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-flight :as flight]
            [jobs.survival.retreat-pillar :as pillar]))

(a/defargs settings
  {::leave-for-breakers {:default true :doc "Whether a hidden body leaves its refuge, before it is hit, for a creeper (its blast breaks any refuge) or, on a pillar, a spider (it climbs) within :breaker-reach." :spec boolean?}
   ::foot-reach {:default 4 :doc "Blocks within which a melee hostile kept off a hidden body counts as waiting at its foot: the hide then outlasts ::hide-s, up to ::hide-max-s, since the first step off the refuge would pass it." :spec (a/num-in 0 nil)}
   ::dig-out-steps {:default 6 :doc "Steps of the stair a body sealed in a plug or pit digs up and away from a melee hostile kept off it at its foot, once ::hide-s has passed." :spec (a/int-in 1 nil)}
   ::quiet-s {:default 30 :doc "Seconds a hidden body keeps its refuge after the last danger." :spec (a/num-in 0 nil)}
   ::hide-s {:default 60 :doc "Seconds in its refuge after which a hidden body hands back, though a danger it keeps off stays, once no real danger is on it as it stands." :spec (a/num-in 0 nil)}
   ::hide-max-s {:default 300 :doc "Seconds after which a hidden body hands back though a melee hostile still waits at its foot." :spec (a/num-in 0 nil)}
   ::breaker-reach {:default 6 :doc "Blocks within which a creeper or a climber counts as breaching a hidden body's refuge." :spec (a/num-in 0 nil)}
   ::reach-radius {:default 3 :doc "Blocks within which a hostile counts as the one that hurt a hidden body." :spec (a/num-in 0 nil)}})
(defn held?
  "Whether a retreat child (its memory, as the parent sees it) holds a refuge: sealed in, on a pillar or in a pit."
  [child-mem]
  (some? (:refuge child-mem)))
(defn reach-radius [] (settings/get settings ::reach-radius))
(defn leave-for-breakers? [] (settings/get settings ::leave-for-breakers))
(defn breaker-reach [] (settings/get settings ::breaker-reach))
(defn quiet-s [] (settings/get settings ::quiet-s))
(defn hide-s [] (settings/get settings ::hide-s))
(defn foot-reach [] (settings/get settings ::foot-reach))
(defn hide-max-s [] (settings/get settings ::hide-max-s))

(def hide-texts
  {:plug "cornered: shut in with blocks until the hostile leaves"
   :pillar "cornered: pillared up out of reach until the hostile leaves"})

(declare hide-now! hide-hold! hostiles-known)

(defn ^:async abandon-refuge!
  "The refuge failed: forget it, never try that option again this flight."
  [c]
  (let [kind (:kind (:refuge (ctx/mem c)))]
    (ctx/update-mem! c dissoc :refuge)
    (flight/tried! c kind)
    :again))

(def max-knocks 3)

(defn ^:async knock-off!
  "A hostile stands in the feet cell the pillar's next block goes into (the server refuses a block where a mob stands):
  strike it once, which knocks it out of the cell. True when it was struck, false when none is there or the knocks are used."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        hostiles (filter #(= "hostile" (:kind %)) (look/seen-entities p {:radius 4}))
        ;; the jump may still be in the air: the cell of the block is the feet cell or the one under it
        mob (when (< (:knocks (ctx/mem c) 0) max-knocks)
              (some #(pl/mob-in hostiles [(:x feet) % (:z feet)]) [(:y feet) (dec (:y feet))]))]
    (when mob
      (ctx/update-mem! c update :knocks (fnil inc 0))
      (let [{:keys [x y z]} (:pos mob)]
        (await (ctx/act c :look #js {:pos #js {:x x :y (+ y 1) :z z}}))
        (await (ctx/act c :attack #js {:id (:id mob)}))
        true))))

(defn ^:async refuge-round!
  "A step in a refuge: building the pillar (a hostile in the cell of the next block is struck out of it; one more try after a refusal, which a hit or knockback can cause), or hidden in it (to the end of the flight)."
  [c]
  (let [{:keys [kind hidden] :as refuge} (:refuge (ctx/mem c))]
    (if hidden
      (await (hide-hold! c))
      (case (await (pillar/step! c refuge))
        :failed (cond
                  (and (= :pillar kind) (await (knock-off! c))) :again
                  (or (not= :pillar kind) (:retried refuge)) (await (abandon-refuge! c))
                  :else (do (ctx/update-mem! c assoc-in [:refuge :retried] true) :again))
        :hidden (hide-now! c refuge (hide-texts kind))
        :again))))

(defn ^:async start-refuge!
  "Note the pillar refuge (a map with :kind, :anchor the feet cell it starts from, :cells [x y z] the cells it fills) in
  job memory and run its first step."
  [c refuge]
  (ctx/update-mem! c assoc :refuge refuge)
  (await (refuge-round! c)))

(defn plug-cells
  "The cells of the blocks this flight's plugs hold (the open ledger entries of the flight's plug purpose), as [x y z]."
  [c]
  (let [purpose (flight/plug-purpose c)]
    (vec (keep #(when (= purpose (:purpose %)) (:cell %)) (ledger/open-entries (ctx/view c))))))

(def all-ways #{:walls :pit :pocket})

(defn ^:async plug!
  "Shut the body in with jobs.survival.shut-in by ways and hide there (a :plug refuge): :again while it works or once shut
  in, nil when no way shuts it. :plugging marks the option, ways and cell a plug is under way at (plugging-here?)."
  [c option ways]
  (let [{:keys [on-lava]} (:args c)
        radius (danger-q/radius-in (:args c))
        ranged-radius (danger-q/ranged-radius-in (:args c))
        e (await (ctx/call c option 'jobs.survival.shut-in
                           (cond-> {:ways ways :on-lava on-lava :trespass? (not= :plug option) :settings {:jobs.lib.danger/hostile-radius radius :jobs.lib.danger/ranged-radius ranged-radius}}
                             (ctx/use c :blocks) (assoc :use {:blocks (ctx/use c :blocks)}))))
        feet (sh-sense/feet (:primitives c))]
    (cond
      (= :continue e) (do (ctx/update-mem! c assoc :plugging {:at feet :option option :ways ways}) :again)
      (= :done (:kind e)) (do (ctx/update-mem! c dissoc :plugging)
                              (spent/absorb! c (:data e))
                              (hide-now! c {:kind (if (= :plug-zone option) :plug option) :anchor feet :cells (plug-cells c)} (hide-texts :plug)))
      :else (do (ctx/update-mem! c dissoc :plugging)
                (when (= :trespass (get-in e [:data :stops :walls :reason])) (ctx/update-mem! c assoc :zone-walls true))
                nil))))

(defn ^:async plug-option!
  "A plug option of a cornered body (:plug: the walls, :plug-dig: pit or pocket), once a flight: plug!'s result, nil when
  it failed or was tried; a pit or pocket is not dug in a body already shut in (its walls did not keep the hostile off)."
  [c option ways]
  (when-not (or (flight/tried? c option)
                (and (= :plug-dig option) (dig-cells/sealed-in? (:primitives c) :walls sh-sense/default-roof-height)))
    (or (await (plug! c option ways)) (do (flight/tried! c option) nil))))

(defn plugging-here?
  "Whether a plug was begun at the body's feet cell this flight and has not failed: it is finished before the walk goes on."
  [c]
  (let [{:keys [at option]} (:plugging (ctx/mem c))]
    (and (= at (sh-sense/feet (:primitives c))) (not (flight/tried? c option)))))

(defn ^:async plug-on!
  "The plug begun here, one more step."
  [c]
  (let [{:keys [option ways]} (:plugging (ctx/mem c))]
    (await (plug! c option ways))))

(defn ^:async pillar!
  "Start a pillar (retreat-pillar/plan!, through another's area only with trespass?) when it fits: a step's result, else nil."
  [c trespass?]
  (when-let [refuge (await (pillar/plan! c trespass?))]
    (await (start-refuge! c refuge))))

(defn refuge-danger?
  "Whether a hostile within its follow range (jobs.lib.threats; ranged ones at least :ranged-radius), the dead skipped,
  would have a walkable way to the refuge's anchor cell were the refuge's own cells open (jobs.lib.danger): a danger
  the refuge keeps off."
  [c {:keys [anchor cells]}]
  (let [p (:primitives c)
        open (set cells)
        ranged-radius (danger-q/ranged-radius-in (:args c))
        reach-of (fn [e] (cond-> (threats/follow-range (.-name e)) (combat/ranged? e) (max ranged-radius)))
        known (flight/near-known p (set (flight/dead-ids c)) threats/max-follow-range ranged-radius)]
    (boolean (some #(and (<= (danger-q/mob-distance p %) (reach-of %))
                         (reach/walkable-way? p (danger-q/mob-pos p %) anchor #{} open))
                   known))))

(defn waiting-melee
  "The known melee hostiles within foot-reach of the body."
  [c]
  (let [p (:primitives c)
        r (foot-reach)]
    (remove combat/ranged? (flight/near-known p (set (flight/dead-ids c)) r r))))

(defn waiting-at-foot?
  "Whether a known melee hostile stands within foot-reach of the body."
  [c]
  (boolean (seq (waiting-melee c))))

(defn ^:async dig-out!
  "Sealed in a plug or pit with a melee hostile at the foot past ::hide-s: dig a stair up (jobs.access.stair, ::dig-out-steps)
  on the heading away from that mob, the next heading when one stops. :again while it digs, :out when it is done, nil when
  no heading is left. Progress is the :dig-out entry of job memory (heading index and the first mob's place)."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        {:keys [i from] :or {i 0}} (:dig-out (ctx/mem c))
        from (or from (some->> (waiting-melee c) first (danger-q/mob-pos p)))
        order (dig-leave/heading-order feet (when from {:x (- (* 2 (:x feet)) (:x from)) :z (- (* 2 (:z feet)) (:z from))}))
        heading (get order i)]
    (ctx/update-mem! c assoc :dig-out {:i i :from from})
    (if-not heading
      nil
      (let [e (await (ctx/call c (keyword (str "dig-out-" i)) 'jobs.access.stair
                               {:dir :up :heading heading :steps (settings/get settings ::dig-out-steps) :fetch false
                                :checks (ready/unforced c {:fit? '(always)})}))]
        (cond
          (= :continue e) :again
          (= :done (:kind e)) :out
          :else (do (ctx/update-mem! c assoc-in [:dig-out :i] (inc i)) :again))))))

(defn breaker-of
  "The known hostile that would breach the refuge: a creeper (its blast breaks any refuge) or, against a pillar, a climber,
  within breaker-reach; nil when none or leave-for-breakers is off."
  [c {:keys [kind]}]
  (when (leave-for-breakers?)
    (let [p (:primitives c)
          reach (breaker-reach)]
      (->> (flight/near-known p (set (flight/dead-ids c)) reach reach)
           (filter #(or (combat/creeper? %) (and (= :pillar kind) (combat/climber? %))))
           first))))

(defn ^:async hide-hold!
  "Plugged in or up the pillar: hold (a :wait, why hiding) while the refuge keeps a danger off
  (refuge-danger?) and the ::quiet-s setting after the last one, then end the flight :hidden. Past the ::hide-s setting (while a melee hostile waits at the foot, ::hide-max-s) in the refuge with no real
  danger on the body as it stands (danger-q/danger-near?, the hostile-near trigger's test) it ends :hidden at once,
  the trigger fires again should one reach it. Sealed in a plug or pit with a melee hostile at the foot past ::hide-s it first digs a stair up
  and away from it (dig-out!), then ends :hidden {:hold :spent :dug-out true}.
  A body hurt while hidden (a mob reaching it round a corner of the seal) leaves the refuge as failed (abandon-refuge!),
  a pillar raised once first (retreat-pillar/raise!)."
  [c]
  (let [now (ctx/now c)]
    (when-not (:quiet-from (ctx/mem c)) (ctx/update-mem! c assoc :quiet-from now))
    (when-not (:hidden-since (ctx/mem c)) (ctx/update-mem! c assoc :hidden-since now)))
  (loop []
    (when (refuge-danger? c (:refuge (ctx/mem c))) (ctx/update-mem! c assoc :quiet-from (ctx/now c)))
    (let [radius (danger-q/radius-in (:args c))
          ranged-radius (danger-q/ranged-radius-in (:args c))
          {:keys [quiet-from hidden-since refuge hold-health]} (ctx/mem c)
          health (.-health (.self (:primitives c)))
          hurt? (< health (or hold-health health))
          breaker (breaker-of c refuge)
          _ (ctx/update-mem! c assoc :hold-health health)
          quiet? (>= (- (ctx/now c) quiet-from) (* 1000 (quiet-s)))
          held-ms (- (ctx/now c) hidden-since)
          spent? (and (>= held-ms (* 1000 (hide-s)))
                      (or (>= held-ms (* 1000 (hide-max-s))) (not (waiting-at-foot? c)))
                      (not (danger-q/danger-near? (:primitives c) radius ranged-radius)))
          dig-out? (or (:dig-out (ctx/mem c))
                       (and (#{:plug :pit} (:kind refuge))
                            (>= held-ms (* 1000 (hide-s)))
                            (seq (waiting-melee c))
                            (not (danger-q/danger-near? (:primitives c) radius ranged-radius))))]
      (cond
        hurt? (let [ids (map (fn [e] (.-id e)) (flight/near-known (:primitives c) #{} (reach-radius) (reach-radius)))]
                (ctx/update-mem! c #(-> % (dissoc :quiet-from :hidden-since :hold-health :dig-out) (update :reached (fnil into #{}) ids)))
                (ctx/emit! c :retreat.hide-hurt :warn {:text "hurt while hidden: the refuge does not keep the hostile off" :pos (sh-sense/feet (:primitives c))
                                                       :hostiles (hostiles-known c)})
                (if (= :again (await (pillar/raise! c))) :again (await (abandon-refuge! c))))
        breaker (let [name (.-name breaker)]
                  (ctx/update-mem! c #(-> % (dissoc :quiet-from :hidden-since :hold-health :dig-out) (update :reached (fnil conj #{}) (.-id breaker))))
                  (ctx/emit! c :retreat.hide-breached :warn {:text (str "a " name " is near: the refuge does not keep it off, leaving before it strikes")
                                                              :mob name :pos (sh-sense/feet (:primitives c))})
                  (await (abandon-refuge! c)))
        dig-out? (let [r (await (dig-out! c))]
                   (if (= :again r)
                     :again
                     (do (ctx/update-mem! c dissoc :quiet-from :hidden-since :dig-out)
                         (flight/end-flight! c :hidden {:hold :spent :dug-out (= :out r)}))))
        quiet? (do (ctx/update-mem! c dissoc :quiet-from :hidden-since)
                   (flight/end-flight! c :hidden {:hold :quiet}))
        spent? (do (ctx/update-mem! c dissoc :quiet-from :hidden-since)
                   (flight/end-flight! c :hidden {:hold :spent}))
        :else (do (await (child/pause! c (/ (flight/wait-ms) 1000) "hiding"))
                  (await (pace/pace!))
                  (recur))))))

(defn hostiles-known
  "The hostiles the body knows within follow range, as [{:name :pos [x y z]}]; empty when it only heard one."
  [c]
  (let [p (:primitives c)]
    (mapv (fn [e] {:name (.-name e) :pos (mapv #(js/Math.floor (% (danger-q/mob-pos p e))) [:x :y :z])})
          (flight/near-known p (set (flight/dead-ids c)) threats/max-follow-range (danger-q/ranged-radius-in (:args c))))))

(defn hide-now! [c refuge text]
  (ctx/update-mem! c assoc :refuge (assoc refuge :hidden true))
  (let [placed (spent/log c)
        hostiles (hostiles-known c)
        from (if (seq hostiles)
               (str/join ", " (map #(str (:name %) " at " (pr-str (:pos %))) hostiles))
               "a hostile heard, not seen")]
    (ctx/emit! c :retreat_sealed :warn (merge {:text (str text "; hiding from " from (some->> (spent/text placed "refuge") (str "; "))) :pos (sh-sense/feet (:primitives c)) :hostiles hostiles}
                                              (when (seq placed) (spent/summary placed)))))
  :again)
