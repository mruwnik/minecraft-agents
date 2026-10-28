(ns jobs.survival.stand-in
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.checks :as checks]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.escape :as escape]
            [jobs.lib.hostile-fight :as fight]
            [jobs.lib.look :as look]
            [jobs.lib.ready :as ready]
            [jobs.lib.shore :as shore]
            [jobs.lib.util :as u]
            [jobs.survival.respond.goal :as respond.goal]
            [jobs.survival.respond.options :as options]))

(def doc
  "Walk to the nearest seen place of one kind within :reach (blocks; the settings give each kind's default) and stand there:
  :water (in water, which an enderman keeps away from), :roof (a 2-high gap on a solid floor under a solid roof, which a
  3-tall enderman cannot enter; a gap with unseen cells over it counts once looked at) or :dry (a dry standing cell, to swim
  out of water: past a mob if that is the only way). A cell not seen reads as rock, so for :water and :roof the body looks round once (jobs.lib.look/look-around-until!) when
  none is found at first (:dry never: a swimmer's every second counts). Each round is one jobs.movement.run-away leg of :leg-s seconds, then :continue; the caller holds still
  once it is done.
  Done {:where :pos} when the body stands in such a place (at once when it does). Nothing-possible :no-place (none within reach) or :blocked (a leg got no nearer or
  was declined). Quote: one leg of job time; ##Inf when no place is seen. Called by the flight (jobs.survival.respond-flight) for
  an angry enderman and a drowned in the water; any agent may submit it.")

(a/defargs args
  {:where {:doc "the kind of place: :water, :roof or :dry" :spec #{:water :roof :dry} :default nil}
   :reach {:doc "blocks within which to look for it; nil: the setting of the kind" :spec (a/num-in 0 nil) :default nil}
   :leg-s {:doc "seconds one leg may take; nil: the flight step setting" :spec (a/num-in 0.1 120) :default nil}})

(a/defargs settings
  {::water-reach {:default 6 :doc "Blocks within which a body hunted by an enderman looks for water to stand in." :spec (a/num-in 0 nil)}
   ::roof-reach {:default 16 :doc "Blocks within which a body hunted by an enderman looks for a 2-high gap under a roof (its own hut counts)." :spec (a/num-in 0 nil)}
   ::enderman-gap {:default 4 :doc "Blocks from a known enderman within which a roof gap is no place to run to: the walk would end beside it." :spec (a/num-in 0 nil)}
   ::dry-reach {:default 16 :doc "Blocks within which a body in water looks for dry land to swim out to." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 60 :doc "Seconds a stand may take." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s}})

(def meta {:category :reflex})

(def safe? ready/ok)

(def fit? ready/ok)

(def can-start?
  (checks/args-fit "needs a :where of :water, :roof or :dry" (fn [c] (contains? #{:water :roof :dry} (:where (:args c))))))

(defn reach-of [c]
  (or (:reach (:args c))
      (case (:where (:args c))
        :water (settings/get settings ::water-reach)
        :roof (settings/get settings ::roof-reach)
        (settings/get settings ::dry-reach))))

(defn feet-cell [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)}))

(defn water-cell
  "The nearest cell within reach (blocks, horizontally and up to 2 vertically) of from that block-at reads as water, or nil."
  [block-at from reach]
  (let [[fx fy fz] (mapv #(js/Math.floor %) [(:x from) (:y from) (:z from)])]
    (->> (for [dx (range (- reach) (inc reach)) dy (range -2 3) dz (range (- reach) (inc reach))
               :when (<= (+ (* dx dx) (* dz dz)) (* reach reach))
               :let [cell {:x (+ fx dx) :y (+ fy dy) :z (+ fz dz)}]
               :when (= "water" (block-at cell))]
           cell)
         (sort-by #(u/dist from {:x (+ 0.5 (:x %)) :y (:y %) :z (+ 0.5 (:z %))}))
         first)))

(defn roof-cell
  "The nearest cell within reach (blocks, horizontally and 1 vertically) of from that block-at reads as a 2-high gap
  on a solid floor under a solid roof, which a 3-tall enderman cannot enter, or nil. A cell with no cell an enderman
  could stand in (3 clear above a solid floor) within 2 blocks comes first: it cannot reach one at the roof's edge.
  A gap within enderman-gap of one of avoid (positions) is left out."
  ([block-at from reach] (roof-cell block-at from reach []))
  ([block-at from reach avoid]
  (let [[fx fy fz] (mapv #(js/Math.floor %) [(:x from) (:y from) (:z from)])
        at (fn [cell dy] (block-at (update cell :y + dy)))
        gap? (fn [cell] (and (escape/solid? (at cell -1)) (bs/passable? (at cell 0))
                             (bs/passable? (at cell 1)) (escape/solid? (at cell 2))))
        tall? (fn [cell] (and (escape/solid? (at cell -1)) (bs/passable? (at cell 0))
                              (bs/passable? (at cell 1)) (bs/passable? (at cell 2))))
        deep? (fn [cell] (not-any? tall? (for [dx (range -2 3) dz (range -2 3)]
                                           (-> cell (update :x + dx) (update :z + dz)))))]
    (->> (for [dx (range (- reach) (inc reach)) dy (range -1 2) dz (range (- reach) (inc reach))
               :when (<= (+ (* dx dx) (* dz dz)) (* reach reach))
               :let [cell {:x (+ fx dx) :y (+ fy dy) :z (+ fz dz)}]
               :when (gap? cell)
               :when (every? #(> (u/dist % {:x (+ 0.5 (:x cell)) :y (:y cell) :z (+ 0.5 (:z cell))}) (settings/get settings ::enderman-gap)) avoid)]
           cell)
         (sort-by (juxt (complement deep?) #(u/dist from {:x (+ 0.5 (:x %)) :y (:y %) :z (+ 0.5 (:z %))})))
         first))))

(defn dry-cell
  "The dry stand the hostile answer prices the swim to (jobs.lib.shore/swim-to-land: joined to the body by seen water and
  air, the nearest swim with land beyond need blocks to walk on, a ledge only when nothing else), or nil."
  [p from need]
  (:stand (shore/swim-to-land p from need)))

(defn place-of
  "The cell to stand at for where, or nil: the nearest of its kind within reach."
  [c block-at]
  (let [p (:primitives c)
        reach (reach-of c)]
    (case (:where (:args c))
      :water (water-cell block-at (u/self-pos c) reach)
      :roof (roof-cell block-at (u/self-pos c) reach
                     (mapv #(danger-q/mob-pos p %) (filter combat/enderman? (danger-q/known-hostiles p reach {}))))
      :dry (dry-cell p (u/self-pos c) (reduce max 0 (map #(cost/hit-range (fight/mob-near p %)) (danger-q/known-hostiles p reach {})))))))

(defn here?
  "Whether the body stands in a place of its kind."
  [c]
  (let [block-at (respond.goal/block-at-fn (:primitives c))
        here (feet-cell c)
        up (fn [n] (block-at (update here :y + n)))]
    (case (:where (:args c))
      :water (= "water" (block-at here))
      :roof (and (bs/passable? (up 1)) (escape/solid? (up 2)))
      :dry (and (not= "water" (up 0)) (not= "water" (up 1))))))

(defn done? [c]
  (let [here (here? c)]
    (checks/goal here [:there "the body stands in such a place"] [:elsewhere "not in such a place yet"]
                 {:result {:where (:where (:args c)) :pos (u/cell-vec (feet-cell c))}})))

(defn quote-of [c]
  (let [leg (or (:leg-s (:args c)) (options/flight-timeout-s))
        found? (or (here? c) (some? (place-of c (respond.goal/block-at-fn (:primitives c)))))]
    {:uses {:job-time [0 leg (if found? (* 3 leg) ##Inf)]}}))

(defn ^:async find-once!
  "The place, else once a call after a look round (a cell not seen reads as rock) the first one that follows a look."
  [c block-at]
  (or (place-of c block-at)
      (when-not (or (= :dry (:where (:args c))) (:glanced (ctx/mem c)))
        (ctx/update-mem! c assoc :glanced true)
        (await (look/look-around-until! c #(place-of c block-at))))))

(defn ^:async round
  [c]
  (let [p (:primitives c)
        block-at (respond.goal/block-at-fn p)
        here (feet-cell c)]
    (when (and (= :roof (:where (:args c))) (true? (some-> (u/sensed p (update here :y + 2)) .-unknown)))
      (await (ctx/act c :look (clj->js {:pos {:x (+ 0.6 (:x here)) :y (+ 2.5 (:y here)) :z (+ 0.5 (:z here))}})))
      (look/see! c))
    (if-let [cell (await (find-once! c block-at))]
      (if (await (options/leg-step! c (:where (:args c)) {:x (+ 0.5 (:x cell)) :y (:y cell) :z (+ 0.5 (:z cell))} 0
                                    (or (:leg-s (:args c)) (options/flight-timeout-s))))
        :continue
        (ctx/end c :nothing-possible :blocked {:text (str "no leg led to the " (name (:where (:args c))) " at " (pr-str (u/cell-vec cell)))}))
      (ctx/end c :nothing-possible :no-place {:text (str "no " (name (:where (:args c))) " within " (reach-of c) " blocks")}))))
