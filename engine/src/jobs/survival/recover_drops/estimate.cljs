(ns jobs.survival.recover-drops.estimate
  "What a pile is worth against the trip to it: value, cost with its parts, the texts that say so, deep piles."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.aim :as aim]
            [jobs.lib.breath :as breath]
            [jobs.lib.escape :as escape]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.reach :as reach]
            [jobs.lib.threat-field :as field]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.survival.recover-drops.config :as cfg]))

(defn finite [n] (if (js/isFinite n) n :infinite))

(defn round1 [n] (/ (js/Math.round (* 10 n)) 10))

(defn why-text
  "The value and the cost with their main parts: \"value 3058.5 (24 raw_iron 2880, ...), cost 31 (trip 10, walk 6,
  danger 15: zombie 1.5)\"."
  [{:keys [value cost top-items other-items parts reason top-mobs]} margin]
  (str "value " (round1 value)
       (when (seq top-items)
         (str " (" (str/join ", " (map (fn [{:keys [name count value]}] (str count " " name " " (round1 value))) top-items))
              (when other-items (str ", " (:kinds other-items) " other kinds " (round1 (:value other-items))))
              ")"))
       ", cost " (if (= :infinite cost) (str "infinite (" (name (or reason :unknown)) ")") (round1 cost))
       (when (seq parts)
         (str " (" (str/join ", " (map (fn [[k n]] (str (name k) " " (round1 n))) parts))
              (when (seq top-mobs) (str ": " (str/join ", " (map (fn [{:keys [name danger]}] (str name " " (round1 (* danger cost/per-danger)))) top-mobs))))
              ")"))
       (when (and (number? margin) (not (zero? margin))) (str " + margin " margin))))

(defn decision-text [decision {:keys [reason items detail] :as decided} margin]
  (case decision
    :skip (str "skip: " (why-text decided margin) (when detail (str ": " detail)))
    :fetch (str "fetch: " (why-text decided margin))
    :collected (str "collected " items " items")
    :partial (str "partial: " items " items back, left " (str/join ", " (map (fn [[k n]] (str n " " k)) (:left decided)))
                  ", " (case reason :unreachable "in sight but not picked up" "not in sight"))
    :abandoned (str "abandoned: " (name (or reason :unknown)) (when detail (str " (" detail ")")))))

(defn decided-wait
  "The wait reason when no death is unrecovered: {:reason :no-drops}, with :decision and a :text naming it when the
  latest death was already decided (a skip is final for that death, whoever asks again)."
  [view]
  (let [died (mem/latest view :died)
        recovered (mem/latest view :recovered)
        decision (when (and died recovered (> (:t recovered) (:t died))) (:data recovered))]
    (cond-> {:reason :no-drops}
      decision (assoc :decision (:decision decision)
                      :text (str "the latest death is already decided: " (decision-text (:decision decision) decision nil))))))

(def maybe-water?
  "Whether a kind of the wet-lookup may be water: an unseen cell may be, the worse case for a drowning question."
  #{:water :unseen})

(defn wet-pile?
  "Whether the pile's cell or the one above it is, or may be (unseen), water (the body would swim to collect).
  kind-at is the wet-lookup."
  [kind-at pos]
  (let [[x y z] (map js/Math.floor [(:x pos) (:y pos) (:z pos)])]
    (boolean (and pos (or (maybe-water? (kind-at x y z)) (maybe-water? (kind-at x (inc y) z)))))))

(defn dive-s
  "Seconds to swim down from the surface of the water over the pile's cell and back up: twice the water cells over it;
  the first unseen cell counts as one more and ends the column."
  [kind-at pos]
  (let [[x y z] (map js/Math.floor [(:x pos) (:y pos) (:z pos)])
        kinds (map #(kind-at x % z) (range y (+ y 48)))
        wet (count (take-while #(= :water %) kinds))
        depth (if (= :unseen (nth kinds wet nil)) (inc wet) wet)]
    (/ (* 2 depth (breath/swim-ms)) 1000)))

(defn no-air-way
  "The text that names why a pile under water is not gone for, else nil: the trip there and back does not fit one breath (the quoted
  route drowns, or the dive down the pile's column and up is longer than the air less ::breath/margin-s) and no block is carried to
  make an air pocket or a footing with."
  [p kind-at pos drown-hp]
  (when (and pos (wet-pile? kind-at pos) (nil? (escape/carried-pillar-item p)))
    (let [dive (dive-s kind-at pos)
          air (- (breath/air-left-s (.self p)) (breath/margin-s))]
      (cond
        (pos? drown-hp) (str "the way there drowns the body (" drown-hp " hp) and it carries no block to make air with")
        (> dive air) (str "a dive of " (js/Math.round dive) " s there and back, " (js/Math.round air) " s of air and no block to make air with")))))

(defn later-killers
  "[{:name :pos :left-ms}] (:left-ms of its guard) of the killers of the body's other deaths inside ::killer-guard-ms, each assumed where it killed: a
  mob that killed the body again near an older pile guards that pile too. Not the death at pos itself."
  [c pos]
  (let [view (ctx/view c)]
    (for [d (mem/entries view :died)
          :let [{:keys [cause] at :pos} (:data d)
                left (- (cfg/killer-guard-ms) (settings/ticks->ms (game/ticks-since d view)))]
          :when (and at (not= at pos) (string? cause) (pos? left))]
      {:name cause :pos at :left-ms left})))

(defn remembered-mobs
  "[{:name :pos}] of the hostiles an earlier decline of this death (the :recover-danger entry) saw, still priced where they
  stood: until the body is within ::see-range of the place with a clear line to it and no such mob is known near it, as a
  player forgets a mob only on seeing the place empty. known is [{:name :pos}] of the hostiles known now."
  [c known]
  (let [here (u/self-pos c)
        entry (last (filter #(= (:death-t (ctx/mem c)) (:death-t (:data %))) (mem/entries (ctx/view c) :recover-danger)))
        sees? (fn [{:keys [pos]}]
                (let [[x y z] (map #(js/Math.floor %) [(:x pos) (:y pos) (:z pos)])]
                  (and (<= (u/dist here pos) (cfg/see-range)) (or (aim/body-sees? c [x y z]) (aim/body-sees? c [x (inc y) z])))))
        still-there? (fn [{:keys [name pos]}] (some #(and (= name (:name %)) (<= (u/dist pos (:pos %)) (cfg/mob-spot-radius))) known))]
    (vec (for [m (:mobs (:data entry))
               :when (and (not (sees? m)) (not (still-there? m)))]
           m))))

(defn unchanged-after-hold?
  "True when a hold for this death (its :recover-danger entry) has run out and the mobs decided saw ({:seen-mobs}) all stand
  where that hold left them: waiting longer would change nothing."
  [c decided]
  (let [entry (last (filter #(= (:death-t (ctx/mem c)) (:death-t (:data %))) (mem/entries (ctx/view c) :recover-danger)))
        before (:mobs (:data entry))
        same? (fn [m] (some #(and (= (:name m) (:name %)) (<= (u/dist (:pos m) (:pos %)) (cfg/mob-spot-radius))) before))]
    (boolean (and entry (<= (:until (:data entry)) (ctx/now c)) (seq (:seen-mobs decided)) (every? same? (:seen-mobs decided))))))

(defn flight-s
  "Seconds a flight from the priced mobs ([{:name :pos}]) may add to the walk back: until the farthest gives up the chase
  (jobs.lib.threats/follow-range past it), at walking speed."
  [here mobs]
  (/ (apply max 0 (for [{:keys [name pos]} mobs :when pos] (- (threats/follow-range name) (u/dist here pos))))
     (cost/walk-blocks-per-s)))

(defn estimate
  "{:value :cost :parts :reason :top-items :top-mobs :hold-ms :flight-s} (:hold-ms: how long a skip for this danger is remembered,
  ::danger-memory-ms, or what is left of the killer guards when only assumed killers add danger; :flight-s: flight-s) of recovering the drops of the :died data (cost js/Infinity when they cannot be fetched). quoted, go-to's quote of the walk when given, lengthens the walk to its planned seconds and prices the hp its route drowns as danger."
  ([c data elapsed] (estimate c data elapsed nil))
  ([c {:keys [pos inventory cause experience]} elapsed quoted]
  (let [p (:primitives c)
        seen (danger-q/seen-hostiles p)
        known-spots (mapv (fn [m] {:name (.-name m) :pos (danger-q/mob-pos p m)}) seen)
        known (concat seen (remembered-mobs c known-spots))
        here (u/self-pos c)
        kind-at (reach/seen-lookup p reach/kind-of :ground)
        wet-at (reach/seen-lookup p reach/kind-of :unseen)
        worth (cost/item-value inventory :overrides (:value-overrides (:args c)))
        route (when pos (conj (cost/straight-route kind-at here pos) pos)) ; the straight route keeps to the surface: a pile far below is its end all the same
        version (game/version-of p)
        guard (concat (when (and pos (string? cause) (< elapsed (cfg/killer-guard-ms))) [{:name cause :pos pos :left-ms (- (cfg/killer-guard-ms) elapsed)}]) ; route-danger drops a cause that is no hostile mob
                      (later-killers c pos))
        guard-spots (set (map :pos guard))
        threat (if route
                 (cost/route-danger version #(field/reach-fn (field/field-of c {:mob (:name %) :pos (:pos %)})) route (concat known guard) (.-equipment (.self p))
                                    :overrides (:danger-overrides (:args c)) :weapon (combat/best-weapon p combat/default-weapons) :health (.-health (.self p))
                                    :death-s (cost/death-cost (cost/death-body p (ctx/view c)) pos))
                 {:danger 0 :mobs []})
        drown-hp (get-in quoted [:route :drown] 0)
        wet (when (or (= "drowning" cause) (wet-pile? wet-at pos) (pos? drown-hp))
              {:name (if (= "drowning" cause) "drowning" "water")
               :danger (max drown-hp (if (or (= "drowning" cause) (wet-pile? wet-at pos)) (cfg/wet-danger) 0))})
        quoted-s (let [mean (second (get-in quoted [:uses :job-time]))] (when (and (number? mean) (js/isFinite mean)) mean))
        threat (cond-> threat wet (-> (update :danger + (:danger wet)) (update :mobs #(vec (cons wet %)))))
        straight-s (when route (+ (/ (u/dist here pos) (cost/walk-blocks-per-s)) (cost/unseen-route-s kind-at route)))
        fetch (cost/fetch-cost {:distance (when pos (* (max straight-s (or quoted-s 0)) (cost/walk-blocks-per-s))) :danger (:danger threat) :elapsed-ms elapsed :cause cause})
        drowns (when-not (:reason fetch) (no-air-way p wet-at pos drown-hp))]
    (cond-> {:value (+ (:value worth) (cost/experience-cost experience))
             :cost (if drowns js/Infinity (:cost fetch))
             :hold-ms (if (and (seq (:mobs threat)) (every? guard-spots (map :pos (:mobs threat))))
                        (min (cfg/danger-memory-ms) (apply max 0 (map :left-ms guard)))
                        (cfg/danger-memory-ms))
             :flight-s (flight-s here (:mobs threat))
             :top-items (mapv #(select-keys % [:name :count :value]) (take (cfg/top-n) (:items worth)))}
      (seq (drop (cfg/top-n) (:items worth))) (assoc :other-items (let [rest-items (drop (cfg/top-n) (:items worth))]
                                                                    {:kinds (count rest-items) :value (reduce + 0 (map :value rest-items))}))
      drowns (assoc :reason :no-air-way :detail drowns)
      (and (not drowns) (:parts fetch)) (assoc :parts (:parts fetch))
      (and (not drowns) (:reason fetch)) (assoc :reason (:reason fetch))
      (seq (:mobs threat)) (assoc :seen-mobs (vec (for [m (:mobs threat) :when (and (:pos m) (not (guard-spots (:pos m))))] (select-keys m [:name :pos])))
                                  :top-mobs (mapv #(select-keys % [:name :danger]) (take (cfg/top-n) (:mobs threat))))))))

(defn danger-only?
  "Whether the estimate e is a skip for danger alone: the pile would be fetched were the route safe."
  [e margin]
  (and (number? (:cost e))
       (pos? (get-in e [:parts :danger] 0))
       (> (:value e) (+ (- (:cost e) (get-in e [:parts :danger])) margin))))

(defn deep-pile
  "{:below n :aside d} when the pile lies ::deep-drop or more under the body and within ::deep-reach aside of it, else nil.
  A walk that did not arrive comes first: the stair is for a pile no walk reaches."
  [c pos]
  (let [here (u/self-pos c)
        below (- (js/Math.floor (:y here)) (js/Math.floor (:y pos)))
        aside (u/dist (assoc here :y 0) (assoc pos :y 0))]
    (when (and (>= below (cfg/deep-drop)) (<= aside (cfg/deep-reach)))
      {:below below :aside aside})))

(defn unreachable-detail
  "Where the pile lies from the body and what the agent can do about it."
  [c pos]
  (let [here (u/self-pos c)]
    (str (js/Math.round (u/dist (assoc here :y 0) (assoc pos :y 0))) " blocks aside and " (js/Math.round (- (:y here) (:y pos)))
         " below, no walk got nearer; run (jobs.survival.recover-drops {:retry true}) from a spot above it, or a stair down yourself")))
