(ns jobs.lib.swim
  "Swimming up a drowning body's own column; shared by jobs.survival.breathe and the air-first leg of jobs.movement.go-to."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.breath :as breath]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::reach {:default 10 :doc "Blocks above the feet the air search climbs." :spec (a/num-in 0 nil)}
   ::step-s {:default 3 :doc "Seconds the sideways step to another air column may take; the body is drowning, so a longer one is given up and the air is looked for again." :spec (a/num-in 0.1 nil)}
   ::air-radius {:default 24 :doc "When no air is near, a go-to child searches for air-reaching water this many blocks sideways." :spec (a/int-in 1 32)}})
(defn reach [] (settings/get settings ::reach))
(defn step-s [] (settings/get settings ::step-s))
(defn air-radius [] (settings/get settings ::air-radius))

(defn rose?
  "A swim that ended with the feet at floor y is progress only above the best height reached so far (best, nil before
  the first swim) and the feet height the pass started at (fy): a body pushed back down between passes is not progress."
  [best fy y]
  (> y (max (or best fy) fy)))

(defn status [r] (.-status r))

(defn cap-beyond-dig?
  "The own column over the head is water or air, unseen included, up to a seen block within rise that lies farther than
  the 3 cells dig-cap reaches: the body must rise under it first."
  [p head rise]
  (loop [k 0]
    (let [n (when (<= k (inc rise)) (breath/seen-water-name p (update head :y + k)))]
      (cond
        (> k (inc rise)) false
        (or (nil? n) (breath/passable-water-or-air? n)) (recur (inc k))
        :else (> k 3)))))

(defn ^:async swim-up!
  "Drowning: swim up the own column when it reaches air, else walk (through
  water, at the feet's height) to the nearest column that does (target, from breath/find-air!). With no air seen and no block seen over the own
  column, swim up anyway: a body in the dark learns by moving, as a player does; so does one under a seen cap more than 3
  above its head (cap-beyond-dig?), to dig it from below. True when the action succeeded (a swim
  counts when it surfaced or rose above the best height reached); :no-air
  when there is no air in reach; nil when the move failed (failure counted)."
  [c target]
  (let [p (:primitives c)
        pos (u/self-pos c)
        fx (js/Math.floor (:x pos))
        fy (js/Math.floor (:y pos))
        fz (js/Math.floor (:z pos))
        ;; a swim that timed out but rose is progress, not a failed try: a deep column takes several 3 s swims
        swim! (fn ^:async f []
                (let [surfaced? (= "surfaced" (status (await (ctx/act c :swim #js {}))))
                      y (js/Math.floor (:y (u/self-pos c)))
                      rose (rose? (:best-y (ctx/mem c)) fy y)]
                  (if surfaced?
                    (ctx/update-mem! c #(-> % (assoc :surfaced true) (dissoc :best-y)))
                    (ctx/update-mem! c update :best-y #(max (or % y) y)))
                  (or surfaced? rose)))]
    (cond
      (and (nil? target) (breath/column-open? p fx fz fy (reach)))
      (or (await (swim!)) :no-air)

      (and (nil? target) (cap-beyond-dig? p (breath/eye-cell (.self p)) (reach)))
      (or (await (swim!)) :no-air)

      (nil? target) :no-air
      (and (= fx (:x target)) (= fz (:z target)))
      (or (await (swim!))
          ;; seen as open yet the swim rose no higher: something the body cannot see holds it, so the next search skips this column
          (do (ctx/update-mem! c update :failed-columns (fnil conj []) target)
              false))

      :else
      ;; raw moveTo kept: an emergency step to air or out of water (range 0), where the planner may have no standable cell; no time for a plan.
      (do (ctx/emit! c :breathe.step :info {:text (str "air column at " (:x target) " " (:z target) ": stepping sideways from " fx " " fz) :target target :from {:x fx :y fy :z fz}})
          (= "arrived" (status (await (ctx/act c :moveTo (clj->js {:pos {:x (:x target) :y fy :z (:z target)}
                                                                    :range 0 :timeoutS (step-s)})))))))))
