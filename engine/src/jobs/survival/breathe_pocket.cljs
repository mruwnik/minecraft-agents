(ns jobs.survival.breathe-pocket
  "Air made where a drowning body is, for jobs.survival.breathe when it sees none. From solid ground: dig the two wall
  cells beside the body (feet and head height), step in and plug the opening behind it with a carried block; the water
  left inside has no source and drains. Afloat: a carried block placed under the feet comes first (footing!)."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.breath :as breath]
            [jobs.lib.dig-look :as look]
            [jobs.lib.escape :as escape]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.pace :as pace]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::dig-share {:default 0.6 :doc "Share of the air left (oxygen, then health) the two digs of a dry pocket may take; a slower pocket is not started." :spec (a/num-in 0 1)}
   ::near {:default 16 :doc "Blocks around a pocket made in the last 5 minutes in which no other is made: the body left that one, so the next run takes the other ways." :spec (a/int-in 0 nil)}
   ::drain-ticks {:default 40 :doc "Game ticks a plugged pocket is given to drain before the run reads the head cell again." :spec (a/int-in 0 nil)}})

(def falling "Blocks that fall when the cell under them is dug: they would drop into the water or the pocket." #{"sand" "red_sand" "gravel" "suspicious_sand" "suspicious_gravel"})

(def sides [[1 0] [-1 0] [0 1] [0 -1]])

(def made-policy {:cap 5 :ttl (* 5 60 1000)})

(defn made-near?
  "A :breathe-pocket entry lies within ::near of cell."
  [c {:keys [x y z]}]
  (boolean (some (fn [e] (let [q (:pos (:data e))]
                           (<= (js/Math.hypot (- x (:x q)) (- y (:y q)) (- z (:z q))) (settings/get settings ::near))))
                 (ctx/entries c :breathe-pocket))))

(defn wall?
  "A seen cell a pocket may be dug in: a full natural block, not protected, not one that falls."
  [p cell]
  (let [n (u/seen-name p cell)]
    (boolean (and n (breath/suffocates? p cell) (escape/natural? n) (not (escape/protected? n)) (not (contains? falling n))))))

(defn footed?
  "The cell under the feet is a full block, felt or seen: a dig from here is not a floating one."
  [p]
  (let [below (update (breath/feet-cell (.self p)) :y dec)]
    (boolean (or (breath/suffocates-here? p below) (some-> (u/seen-block p below) .-fullCube)))))

(defn ^:async site!
  "Where a dry pocket can be made from where the body stands, {:cells [feet head] :entry [feet head] :item :trespass},
  after a look at the wall cells not seen and with the best tool held; else {:why kw}: :off (arg :pocket false), the
  outcome of the run's earlier pocket (one a run), :made-near (see ::near), :no-block (no pillar block carried to plug it), :afloat, :no-wall
  (no side with two natural wall cells under a roof that does not fall) or :too-slow (the digs do not fit ::dig-share
  of the air left). A side in a zone is taken only when no other is left."
  [c]
  (let [p (:primitives c)
        feet (breath/feet-cell (.self p))
        item (:item (escape/pillar-item c))
        options (for [[dx dz] sides] (let [f (-> feet (update :x + dx) (update :z + dz))] [f (update f :y inc)]))]
    (cond
      (false? (:pocket (:args c))) {:why :off}
      (#{:made :dig-failed :step-failed} (:pocket (ctx/mem c))) {:why (:pocket (ctx/mem c))}
      (made-near? c feet) {:why :made-near}
      (nil? item) {:why :no-block}
      (not (footed? p)) {:why :afloat}
      :else
      (do (doseq [{:keys [x y z]} (apply concat options)]
            (when (look/unknown? p [x y z]) (await (look/look-at! c [x y z]))))
          (let [dry-roof? (fn [[_ head]] (not (contains? falling (u/seen-name p (update head :y inc)))))
                {:keys [option trespass]} (access/choose c :dig (filter #(and (every? (partial wall? p) %) (dry-roof? %)) options) identity)]
            (if-not option
              {:why :no-wall}
              (do (await (tools/equip! c (u/seen-name p (first option))))
                  (if (< (* (settings/get settings ::dig-share) (breath/air-left-ms (.self p)))
                         (reduce + (map #(.digTime p (clj->js %)) option)))
                    {:why :too-slow}
                    {:cells option :entry [feet (update feet :y inc)] :item item :trespass trespass}))))))))

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
  "Dig site's two cells (the head cell first), step in, plug the opening (the feet cell first: the floor holds it, and
  it holds the head plug), then give the water left inside ::drain-ticks to go. True when the body stands in the
  pocket, nil when a dig or the step failed; the outcome is kept in job memory (:pocket :made, :dig-failed or
  :step-failed), a pocket made also in body memory (:breathe-pocket, 5 min)."
  [c {:keys [cells entry item trespass]}]
  (let [p (:primitives c)
        [feet head] cells
        _ (access/trespass! c "breathe" trespass)
        dug? (fn ^:async f [cell] (contains? #{"dug" "missing"} (.-status (await (tidy/dig! c cell true)))))
        outcome (cond
                  (not (and (await (dug? head)) (await (dug? feet)))) :dig-failed
                  ;; raw moveTo kept: an emergency step to air or out of water (range 0), where the planner may have no standable cell; no time for a plan.
                  (not= "arrived" (.-status (await (ctx/act c :moveTo (clj->js {:pos feet :range 0}))))) :step-failed
                  :else :made)]
    (ctx/update-mem! c assoc :pocket outcome)
    (when (= :made outcome)
      (ctx/remember! c :breathe-pocket {:pos feet} made-policy)
      (doseq [cell entry] (await (put! c cell item :breathe-plug)))
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
