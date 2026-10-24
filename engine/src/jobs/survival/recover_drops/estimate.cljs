(ns jobs.survival.recover-drops.estimate
  "What a pile is worth against the trip to it: value, cost with its parts, the texts that say so, deep piles."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.breath :as breath]
            [jobs.lib.escape :as escape]
            [jobs.lib.cost :as cost]
            [jobs.lib.reach :as reach]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [jobs.lib.walk-pick :as pick]
            [jobs.survival.recover-drops.config :as cfg]))

(defn finite [n] (if (js/isFinite n) n :infinite))

(defn round1 [n] (/ (js/Math.round (* 10 n)) 10))

(defn why-text
  "The value and the cost with their main parts: \"value 3058.5 (24 raw_iron 2880, ...), cost 31 (trip 10, walk 6,
  danger 15: zombie, skeleton)\"."
  [{:keys [value cost top-items other-items parts reason top-mobs]} margin]
  (str "value " (round1 value)
       (when (seq top-items)
         (str " (" (str/join ", " (map (fn [{:keys [name count value]}] (str count " " name " " (round1 value))) top-items))
              (when other-items (str ", " (:kinds other-items) " other kinds " (round1 (:value other-items))))
              ")"))
       ", cost " (if (= :infinite cost) (str "infinite (" (name (or reason :unknown)) ")") (round1 cost))
       (when (seq parts)
         (str " (" (str/join ", " (map (fn [[k n]] (str (name k) " " (round1 n))) parts))
              (when (seq top-mobs) (str ": " (str/join ", " (map :name top-mobs))))
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

(defn pile-worth
  "Seconds the pile of :died data is worth: its items' item-value (args :value-overrides) and its experience."
  [c {:keys [inventory experience]}]
  (+ (:value (cost/item-value inventory :overrides (:value-overrides (:args c)))) (cost/experience-cost experience)))

(defn pile-age-ms
  "Milliseconds the pile of death `entry` has lain."
  [c entry]
  (settings/ticks->ms (game/ticks-since entry (ctx/view c))))

(defn ^:async quote-trip
  "go-to's quote of the walk to the pile of death `entry` (its planned seconds, its walk risk, and the hp the route drowns),
  nil without a pos: onto the pile itself, where the collect picks it up (the walk's ::arrive-range may end outside a
  mob's reach the pile lies in); its risk prices a death on the way as losing the pile too (:stake, pile-worth, lain :stake-age-ms)."
  [c entry]
  (let [{:keys [pos] :as data} (:data entry)]
    (when pos (await (ctx/quote c 'jobs.movement.go-to {:pos pos :range 0 :stake (pile-worth c data) :stake-age-ms (pile-age-ms c entry)})))))

(defn estimate
  "{:value :cost :parts :reason :top-items :top-mobs :hold-ms :flight-s} (:hold-ms: how long a skip for this danger is remembered,
  ::danger-memory-ms; :flight-s: flight-s) of recovering the drops of the :died data (cost js/Infinity when they cannot be
  fetched). quoted is go-to's quote of the walk (nil: none yet): its planned seconds lengthen the walk, its risk
  (walk-pick/risk-s: its walk risk and planned hp, naming the mobs priced, with the hp its route drowns) is the danger."
  ([c data elapsed] (estimate c data elapsed nil))
  ([c {:keys [pos inventory cause] :as data} elapsed quoted]
  (let [p (:primitives c)
        here (u/self-pos c)
        wet-at (reach/seen-lookup p reach/kind-of :unseen)
        worth (cost/item-value inventory :overrides (:value-overrides (:args c)))
        mobs (mapv (fn [{:keys [mob pos]}] {:name mob :pos pos}) (get-in quoted [:risk :threats]))
        drown-hp (get-in quoted [:route :drown] 0)
        wet (when (or (= "drowning" cause) (wet-pile? wet-at pos) (pos? drown-hp))
              {:name (if (= "drowning" cause) "drowning" "water")
               :danger (max drown-hp (if (or (= "drowning" cause) (wet-pile? wet-at pos)) (cfg/wet-danger) 0))})
        quoted-s (let [mean (second (get-in quoted [:uses :job-time]))] (when (and (number? mean) (js/isFinite mean)) mean))
        danger-s (pick/risk-s c quoted (:danger wet 0))
        straight-s (when pos (/ (u/dist here pos) (cost/walk-blocks-per-s)))
        fetch (cost/fetch-cost {:distance (when pos (* (max straight-s (or quoted-s 0)) (cost/walk-blocks-per-s))) :danger-s danger-s :elapsed-ms elapsed :cause cause})
        drowns (when-not (:reason fetch) (no-air-way p wet-at pos drown-hp))
        named (cond-> (mapv #(select-keys % [:name]) mobs) wet (conj (select-keys wet [:name :danger])))]
    (cond-> {:value (pile-worth c data)
             :cost (if drowns js/Infinity (:cost fetch))
             :hold-ms (cfg/danger-memory-ms)
             :flight-s (flight-s here mobs)
             :top-items (mapv #(select-keys % [:name :count :value]) (take (cfg/top-n) (:items worth)))}
      (seq (drop (cfg/top-n) (:items worth))) (assoc :other-items (let [rest-items (drop (cfg/top-n) (:items worth))]
                                                                    {:kinds (count rest-items) :value (reduce + 0 (map :value rest-items))}))
      drowns (assoc :reason :no-air-way :detail drowns)
      (and (not drowns) (:parts fetch)) (assoc :parts (:parts fetch))
      (and (not drowns) (:reason fetch)) (assoc :reason (:reason fetch))
      (seq mobs) (assoc :seen-mobs mobs)
      (seq named) (assoc :top-mobs (vec (take (cfg/top-n) named)))))))

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
