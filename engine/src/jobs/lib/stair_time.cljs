(ns jobs.lib.stair-time
  "How long a stair takes to cut, for the quotes of the stair, the tunnel and the walk-back-or-redo choice of the mine descent."
  (:require [engine.args :as a]
            [engine.settings :as settings]))

(a/defargs settings
  {::cell-s {:default 1.2 :doc "Mean seconds one cut cell of a step takes (look, dig, walk), for the quote." :spec (a/num-in 0 nil)}})

(defn steps-s "Mean seconds n steps of a stair dir take to cut." [dir n] (* n (if (= :level dir) 2 3) (settings/get settings ::cell-s)))
(defn cell-s [] (settings/get settings ::cell-s))
