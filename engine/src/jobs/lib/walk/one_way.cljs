(ns jobs.lib.walk.one-way
  "The price of a one-way drop go-to's :one-way :open takes into land past what is loaded or seen (Q385): the chance the
  landing is a trap (no way back up) times what being trapped costs, against what the goal is worth. A landing seen lit is
  nearly free; one seen dark or not seen costs ::dark-chance, a dark hall (sh-inside/dark-hall-at?) ::hall-chance; a body carrying a light sees the floor from the rim and pays the lit chance."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.torch :as torch]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::lit-chance {:default 0.02 :spec (a/num-in 0 1)
                 :doc "Chance a drop onto a seen, lit floor leaves the body trapped."}
   ::dark-chance {:default 0.1 :spec (a/num-in 0 1)
                  :doc "Chance a drop onto a floor seen dark or not seen leaves the body trapped."}
   ::hall-chance {:default 0.3 :spec (a/num-in 0 1)
                  :doc "Chance a drop into a dark hall (a cave where mobs spawn) leaves the body trapped."}
   ::trap-s {:default 300 :spec (a/num-in 0 nil)
             :doc "Seconds being trapped below a drop costs: digging out and the death share."}})

(defn light-carried?
  "Whether the body carries a torch or lantern: it holds one up at the rim and sees the floor lit."
  [p]
  (boolean (or (some (comp torch/light-items :name) (u/inventory p))
               (torch/light-items (some-> (.self p) .-equipment .-offHand .-name)))))

(defn lit-in-view?
  "Whether the cell `at` is lit and the body sees it, or could by turning to it (look/glimpse?: a clear line, lit enough to see)."
  [p at]
  (boolean (and (or (sh-sense/cell-light p at) (look/glimpse? p at))
                (look/lit-light? (or (sh-sense/block-light-at p at) 0) (or (sh-sense/sky-light-at p at) 0) (sh-sense/sky-subtract-now p)))))

(defn chance
  "The chance that a drop landing at `at` ({:x :y :z}) traps the body: 0 for a one-block drop onto a floor seen (a step down the
  body can jump back up); ::lit-chance when it has seen the floor lit or carries a
  light to see it by, else (seen dark, or not seen at all) ::dark-chance, ::hall-chance in a dark hall."
  [p at small?]
  (cond
    (and small? (sh-sense/seen-cell? p (update at :y dec))) 0
    (light-carried? p) (settings/get settings ::lit-chance)
    (lit-in-view? p at) (settings/get settings ::lit-chance)
    (sh-inside/dark-hall-at? p at) (settings/get settings ::hall-chance)
    :else (settings/get settings ::dark-chance)))

(defn price
  "Seconds of expected loss of the drop landing at `at`; small? for a drop of one block."
  [p at small?]
  (* (chance p at small?) (settings/get settings ::trap-s)))

(defn worth
  "What a goal `straight` blocks off is worth to the walk: the caller's `worth` (seconds, above 0), else the tour's worth."
  [straight worth]
  (if (pos? worth) worth (cost/tour-worth straight)))

(defn taken?
  "Whether the drop landing at [x y z] is worth taking toward a goal `straight` blocks off, `goal-worth` the caller's :worth."
  [p [x y z] small? straight goal-worth]
  (<= (price p {:x x :y y :z z} small?) (worth straight goal-worth)))
