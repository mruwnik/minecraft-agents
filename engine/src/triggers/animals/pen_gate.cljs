(ns triggers.animals.pen-gate
  "The pen-gate trigger, and the pure reading the shut-gate job shares with it.

  A gate is a cell of a plan whose want is a fence gate. The index {cell plan-id} is built once per plan change
  (jobs.lib.world-files/derived), never per tick.

  The trigger holds when a planned gate was last seen open (perception's memory, never through a wall), within :radius
  (8) of the body and farther than :min-dist (2). It keeps no state: a gate a job opened on purpose is hidden by memory
  entries, three kinds:
    :gate-gave-up  written by jobs.animals.shut-gate; the cell is skipped for :quiet-s (10 min)
    :gate-held     written by a job leading animals through, dropped after it shuts the gate; skipped while
                   younger than :held-s (30)
    :opened        written by a walk (jobs.lib.pass) that opened it; the shut-doors job owns that gate"
  (:require [jobs.lib.pen-gate :as pg]))

(def defaults pg/defaults)

(defn holds?
  "Whether a planned gate near the body was last seen open, with the body away, and no memory entry hides it."
  [p view args kn]
  (let [{:keys [quiet-s held-s] :as args} (merge defaults args)
        gates (some-> (pg/gate-index kn) keys)]
    (if (empty? gates)
      false
      (let [pos (.-pos (.self p))
            self {:x (.-x pos) :y (.-y pos) :z (.-z pos)}
            hidden (into (pg/opened-cells view) (concat (pg/quiet-cells view (* 1000 quiet-s)) (pg/held-cells view (* 1000 held-s))))
            open (pg/seen-open-cells p (remove hidden (pg/candidates self gates args)))]
        (boolean (seq open))))))

(defn pen-gate
  "Holds when holds? says so; args :radius :min-dist :quiet-s :held-s. The job it starts shuts the gate."
  [world view args kn]
  (if kn (holds? world view args kn) false))
