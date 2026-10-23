(ns jobs.movement.go-to.faults
  "Cells a go-to walk keeps out of: the walker's faults (stuck or off-plan cells), a climbable it hangs in, and the zones of other owners."
  (:require [engine.ctx :as ctx]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]))

(def max-fault-cells "Cells a call keeps its plans away from (the walker's faults: fault-cells), the oldest dropped." 16)

(def hurt-window-ms "How recent a hurt event is that an off-plan walk is taken to have fallen." 10000)

(def fall-column "Cells above the point a damaging off-plan walk left its plan that the next plans avoid too: where the fall began." 6)

(defn foreign?
  "Whether cell [x y z] lies in, or next to, a zone of an owner other than self-name."
  [zones self-name [x y z]]
  (boolean (some (fn [{:keys [owner min max]}]
                   (and (not= owner self-name)
                        (every? true? (map (fn [v lo hi] (<= (dec lo) v (inc hi))) [x y z] min max))))
                 zones)))

(defn shut-foreign
  "The walker's :shut-also (jobs.lib.pass): a block opened in or next to another owner's zone is shut again whatever
  :doors says, so a pass through somebody's pen lets nothing out."
  [c]
  (fn [cell] (foreign? (known/zones c) (ctx/self-name c) cell)))

(defn fault-cells
  "The cells a walk that failed at the walker (:stuck at :target, :off-plan at :at) says it cannot pass, [x y z] each.
  An off-plan walk that was hurt (a fall) also takes the cells above :at, where the fall began (a vine or ladder it
  left). A partial plan walked to its end (:partial) is a normal end."
  ([result] (fault-cells result false))
  ([{:keys [status target at partial]} hurt?]
   (case status
     :stuck (some-> target vector)
     :off-plan (when-not partial
                 (when-let [[x y z] at]
                   (if hurt?
                     (mapv (fn [dy] [x (+ y dy) z]) (range (inc fall-column)))
                     [at])))
     nil)))

(def climbables "Blocks a body hangs in (a vine slides it off once nothing else holds it)." #{"vine" "ladder" "scaffolding" "twisting_vines" "twisting_vines_plant" "weeping_vines" "weeping_vines_plant" "cave_vines" "cave_vines_plant"})

(def max-hang-walks "Walks off a floorless climbable a call tries before it takes the stand." 2)

(defn hanging-cell
  "The feet cell [x y z] when the body hangs in a climbable with no floor of its own under it and pos is no climbable
  cell itself (a walk to a vine means to climb), else nil."
  [c pos]
  (let [p (:primitives c)
        feet (reach/standing-cell p)
        name-at (fn [{:keys [x y z]}] (u/feel-name p {:x x :y y :z z}))]
    (when (and (climbables (name-at feet))
               (not (climbables (u/seen-name p {:x (js/Math.floor (:x pos)) :y (js/Math.floor (:y pos)) :z (js/Math.floor (:z pos))})))
               (not (reach/standable-cell? p feet (reach/lookup p))))
      [(:x feet) (:y feet) (:z feet)])))

(defn refused-hang
  "The cell [x y z] the body hangs in while a floor stand is still to be found (hanging-cell, up to max-hang-walks)."
  [c pos]
  (when (< (:hang-walks (ctx/mem c) 0) max-hang-walks) (hanging-cell c pos)))

(defn note-fault!
  "A walk that failed at the walker is a bug to see (a :walker-fault warn) and a cell to keep the next plans off
  (mem :fault-cells, at most max-fault-cells; a new call starts with none): the walks go round it when there is a way.
  The warn asks for no attention: the call goes on, and its give-up is the notice."
  [c {:keys [status] :as result}]
  (when-let [cells (seq (fault-cells result (pos? (ctx/count-in c :hurt hurt-window-ms))))]
    (ctx/emit! c :walker-fault :warn (cond-> {:why status :target (first cells) :attention :none}
                                       (:move result) (assoc :kind (:move result))
                                       (:why result) (assoc :detail (:why result))))
    (ctx/update-mem! c update :fault-cells #(vec (take-last max-fault-cells (distinct (into (vec %) cells)))))))
