(ns jobs.animals.herd
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [jobs.lib.animals :as animals]
            [engine.ctx :as ctx]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]
            [jobs.lib.child :as child]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.cond :as cond]
            [jobs.lib.escort :as escort]
            [jobs.lib.watch :as watch]
            [jobs.lib.bait :as bait]
            [jobs.animals.herd-run :refer [adults after-shut! pen-box pen-count-text cell census! crowd-in-radius crowd-ms crowd-out-radius deepest end! exit-shut-ms gate-cell gate-index gate-open? hold-gate! in-1 in-pen-adults led-now lure? animal-now out-1 pause! read-pen release-step! set-phase! settle-out-ms shut-failed! shut-look-ms transient-keys]]
            [jobs.lib.pace :as pace]
            [jobs.animals.herd-survey :refer [pen-refusal survey!]]
            [jobs.animals.herd-pen :refer [body-in-pen? cell-middle crowded-near? exit-dash! go-then! let-go-cell look-wait! max-leaves min-depth on-gate? set-gate! settle-at! shut-side! shut-when-clear! wait-clear! waited-ms]]
            [jobs.animals.herd-trip :refer [food-first! leash-next! pinned! remember-start! prune-led! regather! regather-or-lose! released! step-position!]]))

(def doc
  "Bring grown animals of type :mob into the pen :box until it holds :target of them. Each animal goes
  on a lead (:method :lead) or follows the food held in hand (:method :lure, the mob's breeding food: wheat,
  carrots, seeds), and is walked through the gate one cell at a time (a long drag jams in a 1-wide gate).
  :auto (the default) leads when a lead or a slime ball is carried and lures otherwise. A lure run fetches the food
  when none is carried (no source: ends :no-food, the text naming the food), walks within 6 blocks of the nearest animal outside the pen, and
  puts the food away at the far end of the pen so nothing follows the body out. The lead steps below hold for both;
  \"on the lead\" means following the food in a lure run, \"let go\" means the food put away.

  Done at once while the box holds :target adults before the gate is touched. Impossible without :mob or :box, or with
  a box whose side along every axis a gate could have (only the :gate's, when given) is under 5 cells.
  A body more than 16 blocks from the pen first walks within 12 of it.
  The adults listed within :radius of the body before that walk are remembered; once the body stands at the pen and none is listed any
  more (a server loads animals only near the body), it walks back to within 12 of each remembered spot to see it again.
  A lured or led animal follows only within about 10 blocks: from where it was met the body walks to the pen in short legs,
  holding still while the animal trails more than 7 behind, and gives it up (given-up :lost unseen 3 s after a walk back to where it was last seen, :animal-stalled when it stops coming nearer for 10 s) rather than outrun it.

  The pen checks. At the start the pen as the body sees and remembers it is judged (a start condition, the setting herd-pen/precheck? false
  skips it): a refusal waits (condition-failed under a parent) with the cause {:reason :text} and the fields below. A pen
  it cannot read from where it stands is read at the pen, where a refusal ends the run condition-failed.
  A lure run gets its food before the walk. The reasons:
  - :no-pen: the pen cannot be read from :box (the result's :cell names the first unseen cell when there is one; a cell it has not seen is gone near and looked at once; one still unknown goes to the pen's gate to look in, once; still unknown ends here).
  - :leaky: the pen has a leak other than the chosen gate (:leaks lists them).
  - :no-gate: no usable gate. The gate is :gate, else the pen's gate nearest the body. It needs pen floor on
    one side and free floor straight across (a corner gate has none). The four cells straight out from the gate
    must be free floor too, each a step up or down from the one before (:why :no-approach). The result and the failure text name the cause: :why
    :no-floor-outside (a drop or a blocked cell outside the gate), :not-in-wall (a corner gate), :no-gate-in-pen or
    :no-approach, with the :cell.
  - :refused: the gate stands in another owner's zone or claim, or in a plan's footprint (or no zone list was read;
    warn herd.declined once). :ignore-zones? true skips the check.
  - :too-shallow: fewer than 5 pen cells in a line inward from the gate (the result gives :cell, the first pen cell
    inside the gate, :depth and :need).
  - :full: adults already on the pen's cells make :target.

  One round trip per animal:
  - Leash the nearest adult outside the pen (jobs.animals.leash, :radius). Babies, animals in the pen or in another closed enclosure,
    animals given up on and animals already fetched twice and still outside are skipped.
  - Approach the cell 4 out from the gate and wait until the animal rests (up to 8 s).
  - Wait up to 10 s for other adults to leave the cells inside the gate (one info herd.crowded-gate).
  - Line up on the cells 3, 2 and 1 out (each waits up to 8 s for the animal to rest), open the gate
    (jobs.access.toggle), then step through the gate and 5 cells in. Each of those stops waits up to 6 s.
  - An animal still too far behind is pinned. The body steps back one cell and tries again, twice. Then it
    walks out, shuts the gate and starts the trip over once. After that the animal is given up as :jammed and
    let go outside.
  - Shut the gate behind the animal once no adult overlaps its cell, from one cell back.
  - Walk to the pen cell farthest from the gate and let the animal go there (jobs.animals.unleash, lead
    picked up). It counts as brought when it stands on a pen cell. The body empties its hand so the animal
    does not follow the lead out.
  - Walk out. It waits up to 10 s while an adult is within 2 of the cell inside the gate or on the gate cell
    (one info herd.crowded-gate), then opens, passes and shuts the gate in one go. If the gate is left open with the body outside, it shuts it from there. If it stays open, one
    warn herd.gate-open. A shut that leaves the body inside opens the gate again and goes out, twice at most,
    then ends :gate-stuck. A run that ends with the gate open and the body on the pen side goes out the same
    way first (twice at most).
  - Count the adults on the pen's cells before the exit and after the shut. Fewer is an escape (warn
    herd.escaped, counted in :escaped).

  While the gate is open a :gate-held memory entry {:cell [x y z]} is kept fresh, so the pen-gate trigger
  leaves it alone. It is dropped after each shut.

  Lead watching: an animal seen off this body's lead is given up on (:lead-broke), one not seen at all too
  (:lost); a lured animal seen beyond follow-range while the gate stands open is kept (the pinned steps go back for it). With nobody led, the open gate is shut, leads within 8 blocks are picked up
  (jobs.items.collect-drops) and leashing starts again once, with the :lead-broke animals forgiven. A second time ends :lost. An end with an animal on the lead lets it go where the body
  stands and shuts the gate from the body's side; a cut by the engine's limit or a cancel does the same in the job's cleanup.

  Phases, in order (the run's memory :phase): survey (read the pen, pick the gate; a lure run gets its food first) ->
  leash (lead or lure the next animal; none left or the pen full -> finish) -> approach -> clear-out -> line-up ->
  open -> step -> shut-behind -> deep (let go, brought) -> exit -> clear-in -> exit-dash (open, pass, shut) -> census
  -> back to leash. A pinned animal goes to retry-back/retry-shut/retry-out and line-up again, then give-up (:jammed).
  A run that ends (end!) lets a led animal go (let-go), shuts the gate (shut-gate, going out first when the body is
  inside) and finishes. The goal and can-run? are asked between steps (jobs.lib.pace/steps!).

  One call is the whole run. Waits for an animal to settle or a cell to clear are
  jobs.time.noop pauses. A cut can leave the body unsafe (animal led, gate
  open, body in the pen); the next call restarts from what it sees: led with the body in the pen
  steps on from the nearest axis cell, led with the body outside goes back and lines up again, nobody led
  with the body in the pen goes out, nobody led with the gate open shuts it.

  Ends with result {:reason :inside n :target :brought [keys] :given-up {key reason}
  :gate pos :escaped n :animals [{:kind :id :pos}] (the livestock on the pen cells)}. :inside is counted on the pen's cells at the end. An animal is booked brought once, however often it is led. Reasons:
  :brought (pen holds :target), :short (fewer than :target in the pen: some came, or none came and no other
  reason), :full,
  :no-pen, :leaky, :no-gate, :too-shallow, :unreachable (a walk was blocked), :gate-stuck, :lost,
  :no-food (a lure run found no food), :no-hold (the food would not stay in hand),
  :none (nobody left to fetch), or :no-leash with the leash child's ending as the :because. The body ends outside with the gate shut. Endings: done when the pen
  holds :target (:brought, :full) or the call's :least (default 1) animals were brought; else condition-failed for
  :no-pen, :leaky, :no-gate, :refused, :no-zones, :too-shallow, :no-food (the cause names the cell), nothing-possible
  with the reason for the rest; the data is the result, its :text naming the animals given up and why (or that none other was in reach). The count
  \"N of M in the pen\" includes the animals known in the pen that the sensing cannot show from where the body ends.

  Zones: the leash child also refuses an animal standing in another owner's zone or claim (see jobs.animals.leash);
  :ignore-zones? true is passed to it.")

(a/defargs args
  {:mob {:doc "the animal's name, such as \"cow\"" :spec a/name? :default nil}
   :box {:doc "the pen: {:min :max}, each corner {:x :y :z} or [x y z], inclusive, the feet cells of its floor" :spec pen-read/box-arg :default nil}
   :target {:doc "grown animals of :mob the pen should hold" :spec (a/int-in 0 nil) :default 2}
   :gate {:doc "the fence gate {:x :y :z} to bring them through; the pen's usable gate nearest the body when nil" :spec ::a/pos :default nil}
   :radius {:doc "animals within this many blocks of where the body started the run are fetched" :spec (a/num-in 0 nil) :default 24}
   :method {:doc "how animals are brought: :lead (each on a lead), :lure (following the food in hand), :auto (a lead when one is carried or can be crafted from a carried slime ball, else the food)" :spec #{:auto :lead :lure} :default :auto}
   :fetch {:doc "get the food when none is carried and the run lures (jobs.lib.need): true, a set of kinds or a map of limits; false ends :no-food" :spec need/option? :default true}
   :ignore-zones? :shared})

(def leading
  "The phases in which the animal is on the lead and the walk is the body's own: watched before each step, bound
  cut by the engine's limit."
  #{:approach :clear-out :line-up :open :step :shut-behind :shut-deepest :shut-back :shut-click :retry-back :retry-shut
    :retry-out
    :give-up-walk :deep})


(a/defargs settings
  {::pace-ms {:default 100 :doc "A step that changed nothing and took less than this is followed by a pause of settle-ms." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 900 :doc "Seconds a herd run may take, its pauses included." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 1800 :doc "Seconds a herd run may last from its start, its pauses included." :spec (a/num-in 0 nil)}})

(defn pace-ms [] (settings/get settings ::pace-ms))

(def target [:mob :box])

(def safe? ready/safe-if-lit)
(def fit? ready/fit?)


(def defaults {:spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(defn box-sides
  "The box's lengths in cells along x and z that a gate's axis could run, only the :gate's when it stands beside a side."
  [{:keys [min max]} gate]
  (let [xs (inc (- (:x max) (:x min)))
        zs (inc (- (:z max) (:z min)))]
    (cond
      (and gate (or (< (:x gate) (:x min)) (> (:x gate) (:x max)))) [xs]
      (and gate (or (< (:z gate) (:z min)) (> (:z gate) (:z max)))) [zs]
      :else [xs zs])))

(defn bad-args
  "Never without :mob or :box, or with a box no gate axis can run min-depth cells into."
  [c]
  (let [{:keys [mob gate]} (:args c)
        box (some-> (:box (:args c)) pen-read/norm-box)]
    (cond
      (not (and mob box)) (contract/never :no-mob-or-box "herd needs a :mob and a :box")
      (< (apply max (box-sides box gate)) min-depth)
      (let [{:keys [min max]} box]
        (contract/never
         :too-shallow
         (str "herd cannot use the box " (cell min) ".." (cell max) ": it is "
              (inc (- (:x max) (:x min))) "x" (inc (- (:z max) (:z min))) ", " min-depth
              " cells in a line needed inward from the gate, so a led animal clears the gate cell")
         :box box :need min-depth))
      :else (contract/ok :args "a :mob and a box deep enough"))))

(def can-start?
  "Starts with a :mob and a box deep enough (bad-args), and a pen seen at the start that will do (pen-refusal)."
  (cond/and bad-args pen-refusal))

(defn done?
  "Ok when the box holds :target adults before the run touched the gate, or once the run has wrapped up (in between, the
  run ends itself, outside with the gate shut); the animals brought, and those still missing."
  [c]
  (let [box (pen-box c)
        target (:target (:args c))
        m (ctx/mem c)
        inside (count (filter #(animals/in-box? box (u/pos-of (.-pos %))) (adults c)))
        data [:units :animals :done (count (:brought m)) :left (max 0 (- target inside))
              :result {:reason :brought :inside inside :target target}]]
    (if (and (or (:finished m) (and (nil? (:gate m)) (empty? (:led m)))) (>= inside target))
      (apply contract/ok :brought (str "herd done: " (pen-count-text inside target)) data)
      (apply contract/later :herding (pen-count-text inside target) data))))

(defn quote-of
  "Find-out: a minute an animal as the guess, no most."
  [c]
  {:uses {:job-time [0 (* 60 (max 1 (:target (:args c) 2))) ##Inf]}})

(defn lure-lost!
  "The led or lured animal is given up (reason :lost, unseen; :lead-broke; :animal-stalled, seen but not coming; :unreachable); the run goes on as when nobody is led."
  [c reason]
  (let [k (:animal (ctx/mem c))]
    (ctx/emit! c :herd.lure-lost :info {:animal k :reason reason :text (str "herd gives up the " (if (lure? c) "lured " "led ") (:mob (:args c)) ": " (if (= :animal-stalled reason) "it stopped following" (name reason)))})
    (ctx/update-mem! c #(-> % (assoc :led []) (update :given-up assoc k reason) (dissoc :unseen-since :progress :waiting :back-to :search-rounds)))
    (regather-or-lose! c)))

(defn ^:async lure-approach!
  "The walk to the pen's first cell with a led or lured animal: an animal follows the food only within follow-range (a lead is 10 long), so the body
  goes in short go-to legs and, by escort/follow-verdict, holds still (or walks back toward the last sighting) while the
  animal trails more than (escort/safe-gap); unseen long, or seen and not coming nearer, it is given up. The last leg is settle-at!'s."
  [c]
  (let [m (ctx/mem c)
        a (animal-now c (:animal m))
        target (u/cell-pos (nth (:axis m) 0))
        verdict (escort/follow-verdict c a (or (lure? c) (nil? a) (animals/led-by-me? a)) (escort/safe-gap))]
    (if verdict
      (if (= :wait verdict)
        (do (await (escort/wait-for-animal! c :never)) :continue)
        (lure-lost! c verdict))
      (let [{:keys [leg searching]} (await (escort/leg-plan c target 1))
            n (inc (:search-rounds m 0))]
        (ctx/update-mem! c assoc :search-rounds (if searching n 0))
        (cond
          (and searching (<= n (escort/max-search-rounds))) :continue
          searching (lure-lost! c :unreachable)
          (not leg) (await (settle-at! c 0 (settle-out-ms) #(set-phase! c :clear-out)))
          :else (do (await (child/call-walk* c :walk {:pos leg :range 1 :doors :never :ways #{:walk :clear-seal}}))
                    :continue))))))

(defn ^:async step! [c phase]
  (let [m (ctx/mem c)
        axis-cell #(nth (:axis m) %)]
    (case phase
      :survey (do (remember-start! c)
                  (or (await (food-first! c))
                      (await (survey! c))))
      :regather (await (regather! c))
      :leash (await (leash-next! c))
      :approach (if-not (:walked m)
                  (await (lure-approach! c))
                  (await (settle-at! c 0 (settle-out-ms) #(set-phase! c :clear-out))))
      :clear-out (await (wait-clear! c (crowded-near? c (axis-cell in-1) (crowd-out-radius)) (crowd-ms) :out
                                     #(set-phase! c :line-up {:pos-index 1})))
      :line-up (let [i (:pos-index m)]
                 (await (settle-at! c i (settle-out-ms)
                                    #(if (= i out-1) (set-phase! c :open) (set-phase! c :line-up {:pos-index (inc i)})))))
      :open (await (set-gate! c :open :step {:then {:pos-index gate-index :back-step false}}))
      :step (await (step-position! c))
      ;; the retry walks out and lines up again with the gate shut: out-1, the shut (an animal on the gate cell is
      ;; waited for, then the gate stays open), out-4
      :retry-back (await (go-then! c (axis-cell out-1) :retry-shut))
      :retry-shut (await (shut-when-clear! c (shut-look-ms) :retry-out nil #(set-phase! c :retry-out)))
      :retry-out (await (settle-at! c 0 (settle-out-ms) #(set-phase! c :line-up {:pos-index 1})))
      ;; the animal is clear of the gate cell: step back one cell and shut; one that stays on it is waited for, then
      ;; looked at once more from the deepest cell, else it is as good as pinned
      :shut-behind (cond
                     (not (on-gate? c)) (set-phase! c :shut-back)
                     (< (waited-ms c) (if (:deepest-tried m) 0 (shut-look-ms))) (await (look-wait! c))
                     (:deepest-tried m) (pinned! c deepest)
                     :else (set-phase! c :shut-deepest {:deepest-tried true}))
      :shut-deepest (await (go-then! c (axis-cell deepest) :shut-behind))
      :shut-back (await (go-then! c (axis-cell (dec deepest)) :shut-click))
      :shut-click (await (shut-when-clear! c (shut-look-ms) :deep nil #(set-phase! c :shut-behind)))
      :deep (await (go-then! c (let-go-cell (:inside (read-pen c)) (gate-cell c) (cell (:inside-cell m))) :deep-release))
      :deep-release (await (release-step! c #(released! c :deep-unequip)))
      :deep-unequip (do (await (ctx/act c :unequip #js {})) (await (bait/put-food-away! c)) (set-phase! c :exit))
      :exit (await (go-then! c (axis-cell in-1) :clear-in))
      :clear-in (await (wait-clear! c (or (crowded-near? c (axis-cell in-1) (crowd-in-radius)) (on-gate? c)) (crowd-ms) :in
                                    #(set-phase! c :exit-dash {:pen-before (count (in-pen-adults c (read-pen c))) :dash-tries 0 :reopens 0})))
      :exit-dash (await (exit-dash! c))
      :exit-open (await (set-gate! c :open :exit-out))
      :exit-out (await (go-then! c (axis-cell out-1) :shut-out))
      :shut-out (await (shut-when-clear! c (exit-shut-ms) :shut-side nil
                                         #(if (:stepped-back m)
                                            (shut-failed! c)
                                            (set-phase! c :shut-back-in {:stepped-back true}))))
      :shut-side (shut-side! c)
      :census (census! c)
      :shut-back-in (await (go-then! c (axis-cell in-1) :shut-out))
      :give-up-walk (await (go-then! c (axis-cell 1) :give-up-unleash {:given-up (assoc (:given-up m) (:animal m) :jammed)}))
      :give-up-unleash (await (release-step! c #(released! c :exit-out)))
      :let-go (await (release-step! c #(do (ctx/update-mem! c dissoc :release-tried :releasing :unleash-failed)
                                          (set-phase! c :shut-gate))))
      ;; a run ending with the body on the pen side goes out first (the exit's own way, then the census ends it)
      :shut-gate (cond
                   (not (gate-open? c)) (after-shut! c)
                   (and (body-in-pen? c) (< (:leaves m 0) (max-leaves)))
                   (set-phase! c :exit {:leaves (inc (:leaves m 0)) :stepped-back false})
                   :else (await (shut-when-clear! c (shut-look-ms) :shut-gate nil #(shut-failed! c)))))))

(defn ^:async step-once!
  "One step of the run: the lead looked at, the :gate-held entry kept fresh, then the phase's step."
  [c]
  (let [{:keys [phase gate]} (ctx/mem c)
        phase (or phase :survey)
        lead-phase? (contains? leading phase)]
    (when (and gate (gate-open? c)) (hold-gate! c))
    (cond
      (and lead-phase? (not (and (= :approach phase) (not (:walked (ctx/mem c))))) (pos? (prune-led! c)) (empty? (:led (ctx/mem c)))) (regather-or-lose! c)
      :else (do (when lead-phase? (await (watch/watch! c {})))
                (await (step! c phase))))))

;; ------------------------------------------------------------------ the round: the whole run

(defn led-by-me
  "The keys of the adults of :mob on this body's lead now, as the world shows them (not the memory)."
  [c]
  (if (lure? c)
    (led-now c)
    (into [] (comp (filter animals/led-by-me?) (map animals/key-of)) (adults c))))

(defn free-to-leave?
  "True when the body may be given away: before the survey has chosen the gate (nothing touched yet), else no animal
  on this body's lead, the gate shut and the body neither on a pen cell nor on the gate cell. Read from the world."
  [c]
  (or (nil? (:gate (ctx/mem c)))
      (and (empty? (led-by-me c)) (not (gate-open? c)) (not (body-in-pen? c)))))

(defn nearest-axis-index
  "The index of the axis cell between the gate cell and the deepest step position nearest (flat) to the body."
  [c]
  (let [axis (:axis (ctx/mem c))
        here (u/self-pos c)]
    (apply min-key #(u/flat-dist here (cell-middle (nth axis %))) (range gate-index (inc (min deepest (dec (count axis))))))))

(defn restart!
  "The round starts unsafe: the last one was cut there (or the world changed). Go on from what the world shows, the
  leads taken from it and the walk and wait state, children included, dropped: a run that was ending ends (end!); an
  animal led with the body in the pen steps on from the nearest axis cell; led with the body outside goes back to
  out-1, shuts the gate and lines up again (the retry's way, its one retry not used up); nobody led with the body in
  the pen goes out (:exit); else the open gate is shut (:shut-gate)."
  [c]
  (let [m (ctx/mem c)
        led (led-by-me c)
        inside? (body-in-pen? c)
        open? (gate-open? c)]
    (ctx/emit! c :herd.restarted :info {:led led :inside inside? :gate-open open? :phase (:phase m)
                                        :text (str "herd restarts from what it sees: " (count led) " led, body "
                                                   (if inside? "in the pen" "outside") ", gate " (if open? "open" "shut"))})
    (ctx/update-mem! c #(-> (apply dissoc % :releasing :release-tried transient-keys)
                            (assoc :children {} :led led :animal (first led))))
    (cond
      (:ending m) (end! c (:ending m))
      (and (seq led) inside?) (set-phase! c :step {:pos-index (nearest-axis-index c) :back-step false :backs 0
                                                   :deepest-tried false})
      (seq led) (set-phase! c :retry-back {:backs 0 :deepest-tried false})
      inside? (set-phase! c :exit {:stepped-back false})
      :else (set-phase! c :shut-gate))))

(defn ^:async round
  "The whole run: steps until it ends (pace/steps!: the goal and can-run? are asked between steps). A round that starts
  unsafe restarts first (restart!). A step that changed no memory and took under pace-ms is followed by a pause, so a
  step that keeps answering at once never spins."
  [c]
  (animals/reserve-bait! c (:mob (:args c)))
  (ctx/update-mem! c update :origin #(or % (u/self-pos c)))
  (when-not (free-to-leave? c) (restart! c))
  (await (pace/steps! c (fn ^:async herd-step []
                          (let [before (ctx/mem c)
                                from (ctx/now c)
                                r (await (step-once! c))]
                            (when (and (= :continue r) (= before (ctx/mem c)) (< (- (ctx/now c) from) (pace-ms)))
                              (await (pause! c :herd-step-waits)))
                            (if (= :continue r) :again r))))))

(defn ^:async cleanup
  "A run that ended by itself is left as it is. Cut by the limit, a cancel or a parent's end: an animal still on this body's lead is let go and an open gate
  shut, by the run's own ending phases, a step per round. Done when the body is safe (animal free, gate shut, body
  outside)."
  [c _]
  (if (or (:finished (ctx/mem c)) (free-to-leave? c))
    (ctx/end c :done nil {})
    (do (when-not (:ending (ctx/mem c)) (end! c :cancelled))
        (let [r (await (step-once! c))]
          (if (= :continue r) :continue (ctx/end c :done nil {}))))))
