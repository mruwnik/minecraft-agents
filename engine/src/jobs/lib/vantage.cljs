(ns jobs.lib.vantage
  "Stepping to see: a body that heard animals it cannot see (below a pit lip, behind a bank) walks to a few cells round
  the place it started from and looks round at each, as a player steps to the edge, before a job says they are absent."
  (:require [engine.ctx :as ctx]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.util :as u]))

(def ring
  "Offsets {x z} from the first cell, one look-out each."
  [[3 0] [-3 0] [0 3] [0 -3]])

(defn done?
  "Whether every look-out was tried this run."
  [c]
  (>= (:index (:vantage (ctx/mem c)) 0) (count ring)))

(defn ^:async step!
  "Walk to the next look-out and look round there, testing (f) after each look. Resolves to :found (a look showed what (f) tests for), :partial (the walk waits on the world: yield, call again), :next (nothing yet, more look-outs left) or
  nil when none is left."
  [c f]
  (let [{:keys [home index] :or {index 0}} (:vantage (ctx/mem c))
        home (or home (look/cell-of (u/self-pos c)))]
    (ctx/update-mem! c assoc-in [:vantage :home] home)
    (if (>= index (count ring))
      nil
      (let [[dx dz] (nth ring index)
            r (await (near/go-near! c {:x (+ (:x home) dx) :y (:y home) :z (+ (:z home) dz)} 1
                                    {:doors :shut :dangers false :escalate false}))]
        (if (= :partial r)
          :partial
          (do (ctx/update-mem! c assoc-in [:vantage :index] (inc index))
              (if (await (look/look-around-until! c f)) :found :next)))))))
