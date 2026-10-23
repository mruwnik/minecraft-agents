(ns jobs.lib.cost.look
  "The value of a look: look first when what the look could change is worth more than the look costs. Pure.

  look-answer: a look of some looks under the mobs near (answer/build-answer over look-s, the blows of the seconds it
  takes), then, by the chance of each thing it may show, the cheapest answer the body has once it has seen that
  (answer/decide): an answer with :outcomes, priced by answer/answer-price beside the answers open now, so decide takes
  the look exactly when look-value's :value is over its :cost.
  closed-chance: the chance the unseen part of a boundary is closed, given its seen part is (a room's walls).

  Callers: respond's cover-answers (an unseen room cell, the door). Others it fits: shut-in's :room way and enter-room
  (an unseen room judged open), look-edge's look at a region's unseen sides, a pit or pocket's unsensed cells
  (plug-way ::unsensed-s), a seen-but-dark route cell in go-to."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost.answer :as answer]))

(a/defargs settings
  {::wall-prior {:default {:k 1 :n 2} :spec (a/map-with {:k (a/int-in 0 nil) :n (a/int-in 1 nil)})
                 :doc "Pseudo-counts of the boundary cells closed (k) of all (n) before any is seen: the uniform prior (Laplace's rule of succession). No trial log records room boundaries to count; the room's own seen cells are the evidence that replaces it."}})

(def look-tick-s
  "Seconds one look takes: the look primitive turns at once and waits for the physics tick that sends the rotation, and
  the sight pass after it reads that tick (one game tick)."
  0.05)

(defn look-s "Seconds looks looks take." [looks] (* looks look-tick-s))

(defn closed-chance
  "The chance every one of unseen boundary cells is closed when the seen ones are all closed: the rule of succession
  over ::wall-prior, each unseen cell in turn ((k + seen + i) / (n + seen + i)). 1 with none unseen."
  [seen unseen]
  (let [{:keys [k n]} (settings/get settings ::wall-prior)]
    (reduce * 1 (map #(/ (+ k seen %) (+ n seen %)) (range unseen)))))

(defn look-answer
  "The answer of looks looks against mobs [answer/flee-mob's] wearing equipment, then the cheapest of each outcome's
  answers for body at `at`: outcomes [[chance {key answer-or-nil}]], the chances summing to 1. nil when an outcome has
  no answer open."
  [body {:keys [equipment mobs looks outcomes]} at]
  (let [look (answer/build-answer {:equipment equipment :mobs mobs :seconds (look-s looks)})
        then (map (fn [[chance answers]]
                    (when-let [{k :answer} (answer/decide (answer/after body look) answers at)]
                      [chance (get answers k)]))
                  outcomes)]
    (when (every? some? then)
      (assoc look :outcomes (vec then)))))

(defn look-value
  "{:value :cost} of look (look-answer's) beside the answers {key answer-or-nil} open now for body at `at`: :value the
  price of the cheapest now less the expected price of each outcome's cheapest (at the health now), :cost the rest of
  the look's price (its blows and seconds, and the outcomes paid at the health they leave). 0 value when the look
  changes nothing; decide takes the look exactly when the value is over the cost."
  [body now {:keys [outcomes] :as look} at]
  (let [best (apply min (vals (:prices (answer/decide body now at))))
        after (reduce + (map (fn [[chance a]] (* chance (answer/answer-price body a at))) outcomes))]
    {:value (- best after)
     :cost (- (answer/answer-price body look at) after)}))
