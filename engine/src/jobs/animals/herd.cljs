(ns jobs.animals.herd
  (:require [engine.args :as a]
            [jobs.lib.animals :as animals]
            [engine.ctx :as ctx]
            [jobs.lib.declined :as declined]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]
            [jobs.animals.herd-run :refer [adults after-shut! cell census! crowd-in-radius crowd-ms crowd-out-radius deepest end! exit-shut-ms gate-cell gate-index gate-open? hold-gate! in-1 in-pen-adults out-1 read-pen release-step! set-phase! settle-ms settle-out-ms shut-failed! shut-look-ms transient-keys]]
            [jobs.animals.herd-pen :refer [body-in-pen? cell-middle crowded-near? exit-dash! flat-dist go-then! let-go-cell look-wait! max-leaves on-gate? set-gate! settle-at! shut-side! shut-when-clear! survey! wait-clear! waited-ms]]
            [jobs.animals.herd-trip :refer [leash-next! pinned! prune-led! regather! regather-or-lose! released! step-position!]]))

(def doc
  "Bring grown animals of type :mob into the pen :box until it holds :target of them. Each animal goes
  on a lead and is walked through the gate one cell at a time (a long drag jams in a 1-wide gate).
  No food is needed.

  Declines while the box already holds :target adults. A started run always passes, so a cut job resumes.
  A body more than 16 blocks from the pen first walks within 12 of it.

  Checks before the first animal (each ends the run with its reason):
  - :no-pen: the pen cannot be read from :box (a cell it has not seen is gone near and looked at once; a cell still unknown ends here).
  - :leaky: the pen has a leak other than the chosen gate (:leaks lists them).
  - :no-gate: no usable gate. The gate is :gate, else the pen's gate nearest the body. It needs pen floor on
    one side and free floor straight across (a corner gate has none). The four cells straight out from the gate
    must be free floor too (:why :no-approach).
  - :refused: the gate stands in another owner's zone or claim, or in a plan's footprint (or no zone list was read;
    warn herd.declined once). :ignore-zones? true skips the check.
  - :too-shallow: fewer than 5 pen cells in a line inward from the gate.
  - :full: adults already on the pen's cells make :target.

  One round trip per animal:
  - Leash the nearest adult outside the pen (jobs.animals.leash, :radius). Babies, animals in the pen,
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
  (:lost). With nobody led, the open gate is shut, leads within 8 blocks are picked up
  (jobs.forestry.collect-drops) and leashing starts again once, with the :lead-broke animals forgiven. A second time ends :lost. :timeout-s limits one
  animal from its lead on until it is let go. An end with an animal on the lead lets it go where the body
  stands and shuts the gate from the body's side.

  One call is the whole run: it ends with the run, never yields. A cut can leave the body unsafe (animal led, gate
  open, body in the pen); the next call restarts from what it sees: led with the body in the pen
  steps on from the nearest axis cell, led with the body outside goes back and lines up again, nobody led
  with the body in the pen goes out, nobody led with the gate open shuts it.

  Ends with info herd.done and result {:reason :inside n :target :brought [keys] :given-up {key reason}
  :gate pos :escaped n}. :inside is counted on the pen's cells at the end. Every reason but :brought and
  :full also gives one warn herd.gave-up. An animal is booked brought once, however often it is led. Reasons:
  :brought (pen holds :target), :short (fewer than :target in the pen: some came, or none came and no other
  reason), :full,
  :no-pen, :leaky, :no-gate, :too-shallow, :unreachable (a walk was blocked), :gate-stuck, :lost, :timeout,
  or the leash reason when nobody could be led. The body ends outside with the gate shut. The job ends completed
  when the pen holds :target or is :full, or when some animals were brought (a partial run, :short or another reason);
  when none was brought it fails with the reason, after the result and herd.done were given.

  Zones: the leash child also refuses an animal standing in another owner's zone or claim (see jobs.animals.leash);
  :ignore-zones? true is passed to it.")

(a/defargs args
  {:mob {:doc "the animal's name, such as \"cow\"" :spec a/name? :default nil}
   :box {:doc "the pen: {:min {:x :y :z} :max {:x :y :z}}, inclusive, the feet cells of its floor" :spec a/box? :default nil}
   :target {:doc "grown animals of :mob the pen should hold" :spec (a/int-in 0 nil) :default 2}
   :gate {:doc "the fence gate {:x :y :z} to bring them through; the pen's usable gate nearest the body when nil" :spec ::a/pos :default nil}
   :radius {:doc "animals within this many blocks of the body are fetched" :spec (a/num-in 0 nil) :default 24}
   :timeout-s {:doc "seconds one animal may take from its lead on until it is let go" :spec (a/num-in 0 nil) :default 180}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false :spec boolean?}})

(def leading
  "The phases in which the animal is on the lead and the walk is the body's own: watched before each step, bound
  by :timeout-s."
  #{:approach :clear-out :line-up :open :step :shut-behind :shut-deepest :shut-back :shut-click :retry-back :retry-shut
    :retry-out
    :give-up-walk :deep})


(defn check
  "A started run always passes; else :mob and :box are given and the box holds fewer than :target adults."
  [c]
  (let [{:keys [mob box target]} (:args c)]
    (cond
      (:started (ctx/mem c)) (declined/check c)
      (not (and mob box)) (ctx/wait c {:reason :no-mob-or-box})
      (< (count (filter #(animals/in-box? box (u/pos-of (.-pos %))) (adults c))) target) true
      :else (ctx/wait c {:reason :at-target :target target}))))

(defn ^:async step! [c phase]
  (let [m (ctx/mem c)
        axis-cell #(nth (:axis m) %)]
    (case phase
      :survey (await (survey! c))
      :regather (await (regather! c))
      :leash (await (leash-next! c))
      :approach (await (settle-at! c 0 settle-out-ms #(set-phase! c :clear-out)))
      :clear-out (await (wait-clear! c (crowded-near? c (axis-cell in-1) crowd-out-radius) crowd-ms :out
                                     #(set-phase! c :line-up {:pos-index 1})))
      :line-up (let [i (:pos-index m)]
                 (await (settle-at! c i settle-out-ms
                                    #(if (= i out-1) (set-phase! c :open) (set-phase! c :line-up {:pos-index (inc i)})))))
      :open (await (set-gate! c :open :step {:then {:pos-index gate-index :back-step false}}))
      :step (await (step-position! c))
      ;; the retry walks out and lines up again with the gate shut: out-1, the shut (an animal on the gate cell is
      ;; waited for, then the gate stays open), out-4
      :retry-back (await (go-then! c (axis-cell out-1) :retry-shut))
      :retry-shut (await (shut-when-clear! c 3 shut-look-ms :retry-out nil #(set-phase! c :retry-out)))
      :retry-out (await (settle-at! c 0 settle-out-ms #(set-phase! c :line-up {:pos-index 1})))
      ;; the animal is clear of the gate cell: step back one cell and shut; one that stays on it is waited for, then
      ;; looked at once more from the deepest cell, else it is as good as pinned
      :shut-behind (cond
                     (not (on-gate? c)) (set-phase! c :shut-back)
                     (< (waited-ms c) (if (:deepest-tried m) 0 shut-look-ms)) (await (look-wait! c))
                     (:deepest-tried m) (pinned! c deepest)
                     :else (set-phase! c :shut-deepest {:deepest-tried true}))
      :shut-deepest (await (go-then! c (axis-cell deepest) :shut-behind))
      :shut-back (await (go-then! c (axis-cell (dec deepest)) :shut-click))
      :shut-click (await (shut-when-clear! c 4 shut-look-ms :deep nil #(set-phase! c :shut-behind)))
      :deep (await (go-then! c (let-go-cell (:inside (read-pen c)) (gate-cell c) (cell (:inside-cell m))) :deep-release))
      :deep-release (await (release-step! c #(released! c :deep-unequip)))
      :deep-unequip (do (await (ctx/act c :unequip #js {})) (set-phase! c :exit))
      :exit (await (go-then! c (axis-cell in-1) :clear-in))
      :clear-in (await (wait-clear! c (or (crowded-near? c (axis-cell in-1) crowd-in-radius) (on-gate? c)) crowd-ms :in
                                    #(set-phase! c :exit-dash {:pen-before (count (in-pen-adults c (read-pen c))) :dash-tries 0 :reopens 0})))
      :exit-dash (await (exit-dash! c))
      :exit-open (await (set-gate! c :open :exit-out))
      :exit-out (await (go-then! c (axis-cell out-1) :shut-out))
      :shut-out (await (shut-when-clear! c 3 exit-shut-ms :shut-side nil
                                         #(if (:stepped-back m)
                                            (shut-failed! c)
                                            (set-phase! c :shut-back-in {:stepped-back true}))))
      :shut-side (shut-side! c)
      :census (census! c)
      :shut-back-in (await (go-then! c (axis-cell in-1) :shut-out))
      :give-up-walk (await (go-then! c (axis-cell 1) :give-up-unleash {:given-up (assoc (:given-up m) (:animal m) :jammed)}))
      :give-up-unleash (await (release-step! c #(released! c :exit-out)))
      :let-go (await (release-step! c #(do (ctx/update-mem! c dissoc :release-tried :releasing :release-declines :unleash-failed)
                                          (set-phase! c :shut-gate))))
      ;; a run ending with the body on the pen side goes out first (the exit's own way, then the census ends it)
      :shut-gate (cond
                   (not (gate-open? c)) (after-shut! c)
                   (and (body-in-pen? c) (< (:leaves m 0) max-leaves))
                   (set-phase! c :exit {:leaves (inc (:leaves m 0)) :stepped-back false})
                   :else (await (shut-when-clear! c 3 shut-look-ms :shut-gate nil #(shut-failed! c)))))))

(defn ^:async step-once!
  "One step of the run: the timeout and the lead looked at, the :gate-held entry kept fresh, then the phase's step."
  [c]
  (let [now (ctx/now c)
        {:keys [phase animal-started gate]} (ctx/mem c)
        phase (or phase :survey)
        lead-phase? (contains? leading phase)]
    (when (and gate (gate-open? c)) (hold-gate! c))
    (cond
      (and lead-phase? animal-started (>= (- now animal-started) (* 1000 (:timeout-s (:args c))))) (end! c :timeout)
      (and lead-phase? (pos? (prune-led! c)) (empty? (:led (ctx/mem c)))) (regather-or-lose! c)
      :else (do (when lead-phase? (await (watch/watch! c {})))
                (await (step! c phase))))))

;; ------------------------------------------------------------------ the round: the whole run

(defn led-by-me
  "The keys of the adults of :mob on this body's lead now, as the world shows them (not the memory)."
  [c]
  (into [] (comp (filter animals/led-by-me?) (map animals/key-of)) (adults c)))

(defn safe?
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
    (apply min-key #(flat-dist here (cell-middle (nth axis %))) (range gate-index (inc (min deepest (dec (count axis))))))))

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
                            (assoc :children {} :led led :animal (first led))
                            (cond-> (and (seq led) (nil? (:animal-started %))) (assoc :animal-started (ctx/now c)))))
    (cond
      (:ending m) (end! c (:ending m))
      (and (seq led) inside?) (set-phase! c :step {:pos-index (nearest-axis-index c) :back-step false :backs 0
                                                   :deepest-tried false})
      (seq led) (set-phase! c :retry-back {:backs 0 :deepest-tried false})
      inside? (set-phase! c :exit {:stepped-back false})
      :else (set-phase! c :shut-gate))))

(def pace-ms "A step that changed nothing and took less than this is followed by a wait of settle-ms." 100)

(defn ^:async round
  "The whole run: steps until it ends. A round that starts unsafe restarts first (restart!). A step that changed no
  memory and took under pace-ms is followed by a wait, so a step that keeps answering at once never spins."
  [c]
  (declined/begin! c)
  (ctx/update-mem! c update :started #(or % (ctx/now c)))
  (when-not (safe? c) (restart! c))
  (loop []
    (let [before (ctx/mem c)
          from (ctx/now c)
          r (await (step-once! c))]
      (if (not= :continue r)
        r
        (do (when (and (= before (ctx/mem c)) (< (- (ctx/now c) from) pace-ms))
              (await (ctx/act c :wait #js {:ms settle-ms})))
            (recur))))))
