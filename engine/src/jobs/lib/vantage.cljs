(ns jobs.lib.vantage
  "Stepping to see: a body that hears animals it cannot see (below a pit lip, behind a bank) walks a few blocks toward
  where the sound came from and looks round there, as a player steps to the edge, before a job says they are absent."
  (:require [engine.ctx :as ctx]
            [engine.entity-observations :as obs]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.util :as u]))

(def max-steps "The most steps toward a sound in one run." 4)
(def reach "Blocks one step goes." 3)

(defn done?
  "Whether every step was taken this run."
  [c]
  (>= (:steps (:vantage (ctx/mem c)) 0) max-steps))

(defn heard
  "The unseen animals named names within radius that the body hears, nearest first (look/heard-entities)."
  [c names radius]
  (look/heard-entities (:primitives c) {:names names :radius radius}))

(defn toward
  "The cell reach blocks from the body's own toward the compass direction of a heard row, at its height."
  [c {:keys [direction]}]
  (let [self (look/cell-of (u/self-pos c))
        a (* (/ js/Math.PI 4) (.indexOf obs/directions (keyword direction)))]
    {:x (+ (:x self) (js/Math.round (* reach (js/Math.sin a))))
     :y (:y self)
     :z (- (:z self) (js/Math.round (* reach (js/Math.cos a))))}))

(defn ^:async step!
  "Walk reach blocks toward the nearest heard animal (rows from heard) and look round there, testing (f) after each
  look. Resolves to :found (a look showed what (f) tests for), :partial (the walk waits on the world: yield, call
  again), :next (nothing yet) or nil when no steps are left."
  [c rows f]
  (let [steps (:steps (:vantage (ctx/mem c)) 0)]
    (if (or (>= steps max-steps) (empty? rows))
      nil
      (let [r (await (near/go-near! c (toward c (first rows)) 1 {:doors :shut}))]
        (if (= :partial r)
          :partial
          (do (ctx/update-mem! c assoc-in [:vantage :steps] (inc steps))
              (if (await (look/look-around-until! c f (look/search-effort))) :found :next)))))))
