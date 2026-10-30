(ns jobs.lib.cost.look
  "The value of a look: look first when what the look could change is worth more than the look costs. Pure.

  look-answer: a look of some looks under the mobs near (answer/build-answer over look-s, the blows of the seconds it
  takes), then, by the chance of each thing it may show, the cheapest answer the body has once it has seen that, with
  the mobs come nearer meanwhile (approached; answer/decide): an answer with :outcomes, priced by answer/answer-price beside the answers open now, so decide takes
  the look exactly when look-value's :value is over its :cost.
  closed-chance: the chance the unseen part of a boundary is closed, given its seen part is (a room's walls); chance-closed under any prior.
  turn-chance: the chance the next look turns a decision, after the fruitless ones; turn-saving: the seconds a turn saves,
  by how far a cell seen otherwise moves the chosen plan past the runner-up; cell-looks: how many looks at unknown cells
  pay that at that chance; redo-every: the looks between two decisions,
  so deciding again costs no more CPU than the looks take on the clock.
  round-looks: whether a round that stops at the first open wall pays a turn's gap: all of its looks or none.

  Callers: respond's cover-answers (an unseen room cell, the door). Others it fits: shut-in's :room way and enter-room
  (an unseen room judged open), look-edge's look at a region's unseen sides, a pit or pocket's unsensed cells
  (pit-cells/shell-fails), a seen-but-dark route cell in go-to."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost.answer :as answer]
            [jobs.lib.cost.danger :as danger]
            [jobs.lib.cost.death :as death]
            [jobs.lib.cost.threat :as threat]))

(a/defargs settings
  {::look-tick-s {:default 0.054 :spec (a/num-in 0 nil)
                  :doc "Seconds one look takes on the job clock: the median of `node tools/event-counts.mjs look-seconds` (0.054 s over n=4 look-only rounds, once survey rounds are left out; one physics tick, :sense.looked :wall-ms). The turn waits for the physics tick that sends it, then the sight pass reads it."}
   ::turn-prior {:default {:a 1 :b 42} :spec (a/map-with {:a (a/num-in 0 nil) :b (a/num-in 0 nil)})
                 :doc "The chance a look at an unknown cell a decision read turns the decision (or a check's verdict): a Beta(a, b) over that rate, so after i fruitless looks the next turns it at a / (a + b + i). Mean 1/43, at the bound the :sense.looked events give: of 646 decide! calls (46 bodies, to 2026-10-30) at most the 91 that ended inside the old 8-look cap turned, over 3892 looks (2.3%); those events had no outcome, so this errs toward looking. Refit from :sense.looked :turned?."}
   ::wall-prior {:default {:k 5.237 :n 8.729 :sealed 0.15} :spec (a/map-with {:k (a/num-in 0 nil) :n (a/num-in 0.1 nil) :sealed (a/num-in 0 1)})
                 :doc "Roofed-room prior: pseudo-counts of the boundary cells closed (k) of all (n) before any is seen, rule-of-succession style, and the share :sealed of rooms closed whole (chance-closed). Fitted by the beta-binomial likelihood with a sealed share (a room is either sealed or not, so its sides are not independent) over the 19 rooms in the recorded chunks that stand under open sky with a roof of planks, slab, glass, wool, cobblestone or the like, natural structures' bricks left out (`node tools/event-counts.mjs wall-chunks --stride 40`: 107 side cells closed of 169, 4 rooms sealed; of the 510 roofed spaces 45 are sealed, 232 have a built roof and only those 19 sky, the mineshafts and caves the rest). A thin count: refit as the chunks grow."}})

(defn look-s "Seconds looks looks take." [looks] (* looks (settings/get settings ::look-tick-s)))

(defn chance-closed
  "The chance every one of unseen boundary cells is closed when the seen ones are all closed, under prior {:k :n :sealed}: a share
  :sealed of boundaries is closed whole, the rest has each cell closed by the rule of succession ((k + i) / (n + i), i the
  cells before it, seen or not). 1 with none unseen."
  [{:keys [k n sealed]} seen unseen]
  (let [alike (fn [cells] (reduce * 1 (map #(/ (+ k %) (+ n %)) (range cells))))]
    (/ (+ sealed (* (- 1 sealed) (alike (+ seen unseen)))) (+ sealed (* (- 1 sealed) (alike seen))))))

(defn turn-chance
  "The chance the next look turns the decision after `looked` looks that did not (::turn-prior's rule of succession)."
  [looked]
  (let [{:keys [a b]} (settings/get settings ::turn-prior)]
    (/ a (+ a b looked))))

(defn turn-saving
  "The seconds a turn of a decision saves, from its priced alternatives {:gap :delta}: delta the seconds the chosen plan
  pays more when a cell it read is not what the decision took it for (a walk round, a dig, danger, the walk to see it),
  gap the runner-up's price less the chosen plan's. A turn pays only what delta passes the gap by, so far-apart plans buy
  fewer looks than close ones. nil (no priced runner-up, gap ##Inf) saves nothing: no look."
  [{:keys [gap delta]}]
  (if (and gap delta) (max 0 (- delta gap)) 0))

(defn cell-looks
  "How many of `looks` looks at unknown cells pay a turn saving `saving` seconds (turn-saving), `looked` fruitless looks
  taken before them, each costing look-s plus redo-s (its share of deciding again): the next is taken while saving x
  chance (a fn of the fruitless looks before, default turn-chance) is over that. The chance falls with each fruitless
  look, so a finite saving stops."
  ([saving looks] (cell-looks saving looks 0 0))
  ([saving looks looked redo-s] (cell-looks saving looks looked redo-s nil))
  ([saving looks looked redo-s chance]
   (let [chance (or chance turn-chance)]
     (count (take-while #(> (* saving (chance (+ looked %))) (+ (look-s 1) redo-s)) (range looks))))))

(defn closed-cell-chance
  "The chance a look at one more unseen boundary cell of a roofed room shows it closed, after `looked` shown open:
  ::wall-prior's rule of succession (k / (n + looked))."
  [looked]
  (let [{:keys [k n]} (settings/get settings ::wall-prior)]
    (/ k (+ n looked))))

(defn redo-every
  "Looks to take between two decisions when one takes redo-s seconds of CPU: a round costs max(game clock, CPU), so
  deciding again waits until the looks since cost as much on the clock."
  [redo-s]
  (max 1 (long (Math/ceil (/ redo-s (look-s 1))))))

(defn closed-chance
  "chance-closed under ::wall-prior."
  [seen unseen]
  (chance-closed (settings/get settings ::wall-prior) seen unseen))

(defn round-looks
  "How many of `walls` looks at the unseen walls of a room pay a turn saving `saving` seconds (turn-saving). A look buys nothing alone:
  the call starts only if every wall still unseen proves closed, so with i seen it goes on while saving x (closed-chance
  of the walls seen and left) is over look-s of the looks left. The chance rises and the cost falls with every look, so
  a round that pays at its start pays to its end: all or none."
  [saving walls]
  (count (take-while #(> (* saving (closed-chance % (- walls %))) (look-s (- walls %))) (range walls))))

(defn approached
  "mobs [answer/flee-mob's] seconds later: each melee one (not threat/ranged?) nearer by seconds of its chase speed, not
  inside danger/melee-reach."
  [mobs seconds]
  (mapv (fn [{:keys [name distance speed] :as m}]
          (if (or (threat/ranged? m) (nil? distance))
            m
            (assoc m :distance (max (min distance (danger/melee-reach)) (- distance (* seconds (or speed (danger/chase-speed name))))))))
        mobs))

(defn look-answer
  "The answer of looks looks against mobs [answer/flee-mob's] wearing equipment, then the cheapest of each outcome's
  answers for body at `at`: outcomes [[chance answers-of]], the chances summing to 1, answers-of a fn of the mobs
  (approached by the look's seconds) to {key answer-or-nil}. nil when an outcome has no answer open."
  [body {:keys [equipment mobs looks outcomes]} at]
  (let [seconds (look-s looks)
        look (answer/build-answer {:equipment equipment :mobs mobs :seconds seconds})
        moved (approached mobs seconds)
        then (map (fn [[chance answers-of]]
                    (let [answers (answers-of moved)]
                      (when-let [{k :answer} (answer/decide (answer/after body look) answers at)]
                        [chance (get answers k)])))
                  outcomes)]
    (when (every? some? then)
      (assoc look :outcomes (vec then)))))

(defn look-value
  "{:value :cost} of look (look-answer's) beside the answers {key answer-or-nil} open now for body at `at`: :value the
  price of the cheapest now less the expected price of each outcome's cheapest (at the health now), :cost the rest of
  the look's price (its blows and seconds, and the outcomes paid at the health they leave). 0 value when the look
  changes nothing; decide takes the look exactly when the value is over the cost."
  [body now {:keys [outcomes] :as look} at]
  (let [death (death/death-cost body at)
        price #(answer/answer-price* body % at death)
        best (apply min (map price (filter some? (vals now))))
        after (reduce + (map (fn [[chance a]] (* chance (price a))) outcomes))]
    {:value (- best after)
     :cost (- (price look) after)}))
