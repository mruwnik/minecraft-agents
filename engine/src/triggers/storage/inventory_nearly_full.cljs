(ns triggers.storage.inventory-nearly-full
  "The inventory-nearly-full trigger: few free slots are left."
  (:require [jobs.lib.body :as body]
            [engine.args :as a]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::free {:default 2 :doc "Empty main and hotbar slots at or below which the inventory counts as nearly full." :spec (a/int-in 0 36)}
   ::rest-s {:default 120 :doc "Seconds the trigger rests after make-room tossed something: the tossed items lie at the body's feet and a sealed-in body picks them straight back up." :spec (a/num-in 0 nil)}})
(defn nearly-full-free [] (settings/get settings ::free))

(defn default-rest-s [] (settings/get settings ::rest-s))

(def defaults {})

(defn resting?
  "Whether make-room tossed (a :make-room-rest entry) less than :rest-s ago."
  [view args]
  (let [t (some-> view (mem/latest :make-room-rest) :t)]
    (boolean (and t (< (- (:now view) t) (* 1000 (:rest-s args (default-rest-s))))))))

(defn inventory-nearly-full
  "Holds when at most :free (args, default 2) of the 36 main and hotbar slots are empty.
  make-room puts things in a known chest, else tosses the least valuable stacks.
  The 120 s cooldown stops a body with nothing it may toss from retrying every tick; :rest-s (setting, default 120 s)
  keeps it quiet after a toss, whose items the body may pick up again."
  [world memory args]
  (and (not (body/recovery-pending? memory :inventory-nearly-full))
       (<= (u/free-slots world) (:free args (nearly-full-free)))
       (not (resting? memory args))))
