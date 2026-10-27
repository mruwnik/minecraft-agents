(ns jobs.lib.walk.one-way
  "The price of a one-way drop go-to's :one-way :open takes into land past what is loaded or seen (Q385): the chance the
  landing is a trap (no walk back up) times what climbing back out to the rim costs, against what the goal is worth. A landing seen lit is
  nearly free; one seen dark or not seen costs ::dark-chance, a dark hall (sh-inside/dark-hall-at?) ::hall-chance; a body carrying a light sees the floor from the rim and pays the lit chance."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.ways :as cw]
            [jobs.lib.escape :as escape]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.tools :as tools]
            [jobs.lib.torch :as torch]
            [jobs.lib.util :as u]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::lit-chance {:default 0.02 :spec (a/num-in 0 1)
                 :doc "Chance a drop onto a seen, lit floor leaves the body trapped."}
   ::dark-chance {:default 0.1 :spec (a/num-in 0 1)
                  :doc "Chance a drop onto a floor seen dark or not seen leaves the body trapped."}
   ::hall-chance {:default 0.3 :spec (a/num-in 0 1)
                  :doc "Chance a drop into a dark hall (a cave where mobs spawn) leaves the body trapped."}})

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

(defn climb-out-s
  "Seconds the cheapest way back up from landing `at` to the rim it dropped from takes, as go-to's escalations price them: a
  pillar of the drop's height when the body carries the climb blocks (the column it fell through is open), or a stair of
  that height cut along the cheapest heading, each cut at what the carried kit (or the bare hand) takes on its block, an open floor at a laid block, a cell not seen or not loaded
  read as rock (the worse case). ##Inf when neither can be made."
  [p rim at]
  (let [rim (mapv rim [:x :y :z])
        at (mapv at [:x :y :z])
        h (- (second rim) (second at))
        block-at (escape/block-at-of p)
        cut-s (fn [cell] (let [n (or (block-at cell) access/hidden-guess)]
                           (when-not (bs/passable? n) (/ (tools/clear-ms p n) 1000))))
        unseen? (escape/unseen-of p)
        step-s (:cost (cost/walk-cost {:distance 1}))
        unseen-s (cost/unseen-route-s (wworld/unseen-share p) (constantly :ground) [{:x 0 :y 0 :z 0}])
        fill-s (cw/fill-cost (:count (escape/carried-pillar-item p) 0))
        step-cost (fn [cuts]
                    (let [floor (escape/add (nth cuts 2) [0 -1 0])]
                      (cw/stair-step-cost step-s (cw/stair-cost 1)
                                          (cond-> (mapv cut-s cuts) (bs/passable? (block-at floor)) (conj fill-s))
                                          (if (some unseen? (cons floor cuts)) unseen-s 0))))
        stair (fn [d] (transduce (map step-cost) + 0 (partition 3 (escape/stair-cuts at d h))))
        pillar (if (<= h (:count (escape/carried-pillar-item p) 0)) (cw/pillar-cost h) ##Inf)]
    (apply min pillar (map stair escape/cardinals))))

(defn price
  "Seconds of expected loss of the drop from `rim` landing at `at` (both {:x :y :z}); small? for a drop of one block."
  [p rim at small?]
  (let [chance (chance p at small?)]
    (if (zero? chance) 0 (* chance (climb-out-s p rim at)))))

(defn worth
  "What a goal `straight` blocks off is worth to the walk: the caller's `worth` (seconds, above 0), else the tour's worth."
  [straight worth]
  (if (pos? worth) worth (cost/tour-worth straight)))

(defn taken?
  "Whether the drop from rim to the landing at (both [x y z]) is worth taking toward a goal `straight` blocks off, `goal-worth` the caller's :worth."
  [p rim at small? straight goal-worth]
  (<= (price p (zipmap [:x :y :z] rim) (zipmap [:x :y :z] at) small?) (worth straight goal-worth)))
