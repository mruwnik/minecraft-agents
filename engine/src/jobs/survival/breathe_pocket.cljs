(ns jobs.survival.breathe-pocket
  "Air made where a drowning body is, for jobs.survival.breathe when it sees none. From solid ground: jobs.survival.shut-in's
  pocket way digs the two wall cells beside the body (feet and head height), steps in and plugs the opening behind it; the
  water left inside has no source and drains (make!). Afloat: a carried block placed under the feet comes first (footing!)."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.breath :as breath]
            [jobs.lib.dig-look :as look]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.pace :as pace]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-pit :as pit]))

(a/defargs settings
  {::dig-share {:default 0.6 :doc "Share of the air left (oxygen, then health) the two digs of a dry pocket may take; a slower pocket is not started." :spec (a/num-in 0 1)}
   ::near {:default 16 :doc "Blocks around a pocket made in the last 5 minutes in which no other is made: the body left that one, so the next run takes the other ways." :spec (a/int-in 0 nil)}
   ::drain-ticks {:default 40 :doc "Game ticks a plugged pocket is given to drain before the run reads the head cell again." :spec (a/int-in 0 nil)}})

(def falling "Blocks that fall when the cell under them is dug: they would drop into the water or the pocket." #{"sand" "red_sand" "gravel" "suspicious_sand" "suspicious_gravel"})

(def made-policy {:cap 5 :ttl (* 5 60 1000)})

(defn made-near?
  "A :breathe-pocket entry lies within ::near of cell."
  [c {:keys [x y z]}]
  (boolean (some (fn [e] (let [q (:pos (:data e))]
                           (<= (js/Math.hypot (- x (:x q)) (- y (:y q)) (- z (:z q))) (settings/get settings ::near))))
                 (ctx/entries c :breathe-pocket))))

(defn footed?
  "The cell under the feet is a full block, felt or seen: a dig from here is not a floating one."
  [p]
  (let [below (update (breath/feet-cell (.self p)) :y dec)]
    (boolean (or (breath/suffocates-here? p below) (some-> (u/seen-block p below) .-fullCube)))))

(defn ^:async site!
  "Where a dry pocket can be made from where the body stands, {:cells [feet head]} (jobs.survival.retreat-pit/pocket-site), after a
  look at the wall cells not seen and with the best tool held; else {:why kw}: :off (arg :pocket false), the
  outcome of the run's earlier pocket (one a run), :made-near (see ::near), :no-block (no pillar block carried to plug it), :afloat, :no-wall
  (no side with two natural wall cells under a roof that does not fall) or :too-slow (the digs do not fit ::dig-share
  of the air left). A side in a zone is taken only when no other is left."
  [c]
  (let [p (:primitives c)
        feet (breath/feet-cell (.self p))
        item (:item (escape/pillar-item c))]
    (cond
      (false? (:pocket (:args c))) {:why :off}
      (#{:made :dig-failed :step-failed} (:pocket (ctx/mem c))) {:why (:pocket (ctx/mem c))}
      (made-near? c feet) {:why :made-near}
      (nil? item) {:why :no-block}
      (not (footed? p)) {:why :afloat}
      :else
      (do (doseq [[dx dz] dig-cells/sides
                  :let [f (-> feet (update :x + dx) (update :z + dz))]
                  {:keys [x y z]} [f (update f :y inc)]]
            (when (look/unknown? p [x y z]) (await (look/look-at! c [x y z]))))
          (if-let [cells (pit/pocket-site c feet)]
            (do (await (tools/equip! c (u/seen-name p (first cells))))
                (if (< (* (settings/get settings ::dig-share) (breath/air-left-ms (.self p)))
                       (reduce + (map #(.digTime p (clj->js %)) cells)))
                  {:why :too-slow}
                  {:cells cells}))
            {:why :no-wall})))))

(defn ^:async put!
  "Place item in cell (in another's area too: a last resort, noted for the restore) and write it to the scaffold
  ledger under purpose. True when placed."
  [c cell item purpose]
  (let [at [(:x cell) (:y cell) (:z cell)]
        before (or (u/seen-name (:primitives c) cell) "water")
        placed? (= "placed" (.-status (await (tidy/place! c cell item true))))]
    (when placed?
      (ledger/remember! c (-> (ledger/intend (ledger/open-entries (ctx/view c)) {:cell at :item item :before before :job (:id c) :purpose purpose})
                              (ledger/confirm at))))
    placed?))

(defn ^:async make!
  "Dig site's pocket and wall the way in (jobs.survival.shut-in, the pocket way, no walk), (site! decided the digs fit ::dig-share of the air left), then
  give the water left inside ::drain-ticks to go. True when the body stands in the pocket, nil when it was not made; the outcome is
  kept in job memory (:pocket :made or :dig-failed), a pocket made also in body memory (:breathe-pocket, 5 min)."
  [c {:keys [cells]}]
  (let [p (:primitives c)
        e (loop []
            (let [e (await (ctx/call c :pocket 'jobs.survival.shut-in
                                     {:ways #{:pocket}
                                      :settings {:jobs.survival.dig-pocket/walk-radius 0}}))]
              (if (and (= :continue e) (ctx/alive? c))
                (do (await (pace/pace!)) (recur))
                e)))
        outcome (if (= :done (:kind e)) :made :dig-failed)]
    (ctx/update-mem! c assoc :pocket outcome)
    (when (= :made outcome)
      (ctx/remember! c :breathe-pocket {:pos (first cells)} made-policy)
      (loop [n (js/Math.ceil (/ (settings/ticks->ms (settings/get settings ::drain-ticks)) pace/pace-ms))]
        (when (and (pos? n) (ctx/alive? c) (breath/situation p (:min-oxygen (:args c))))
          (await (pace/pace!))
          (recur (dec n))))
      true)))

(defn ^:async footing!
  "Afloat with a carried pillar block: place one in the water cell under the feet, when a seen neighbour holds it, and
  settle on it, so the next pass digs from the ground (a floating dig is 5 times slower again). True when placed; one
  a run, never with arg :pocket false."
  [c]
  (let [p (:primitives c)
        feet (breath/feet-cell (.self p))
        below (update feet :y dec)
        item (:item (escape/pillar-item c))]
    (when (and item (not (false? (:pocket (:args c)))) (not (:footing (ctx/mem c)))
               (contains? breath/swimmable (breath/seen-water-name p below)) (blocks/support? p below))
      (ctx/update-mem! c assoc :footing true)
      (when (await (put! c below item :breathe-step))
        ;; raw moveTo kept: an emergency step to air or out of water (range 0), where the planner may have no standable cell; no time for a plan.
        (await (ctx/act c :moveTo (clj->js {:pos feet :range 0})))
        true))))
