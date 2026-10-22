(ns jobs.access.toggle
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.ready :as ready]
            [jobs.lib.click :as click]
            [jobs.lib.look :as look]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [engine.contract :as contract]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.places :as places]))

(def doc
  "Put one block into a WANTED :state. The state is read first. A block already there is left alone: the job is done
  with no click. :pos is [x y z] or {:x :y :z}.

  States by block:
  - :open / :closed: fence gates, doors and trapdoors of every wood and of copper. A door is two blocks that
    move together, either half may be named. A double door is two blocks, so two runs.
  - :on / :off: a lever.
  - :press: a button. It counts when the click shows it powered.

  Iron doors and iron trapdoors ignore a hand: they are worked through a seen lever or button within ::switch-radius (6) of
  them (the switch way): nearest first, each is a toggle child of this job (a lever to the other state, a button pressed), then a short wait
  and a read of the door; the first switch that puts the door in the wanted state ends the run :changed (data of the door), and when none does
  the run is nothing-possible :unchanged. With no switch seen the start is :needs-redstone. Any other block is :not-toggleable. A state the
  block cannot have is :bad-state with :valid. Zones and footprints do not apply, since no block type changes.

  The body walks within the ::reach setting (3) of the block (jobs.movement.go-to child with :doors :never, so the approach
  opens nothing). A walk that gives up is :unreachable. It then clicks once with the hand as it is (useOn with no
  item: what is held stays in hand) and reads the block again.
  A click the game answers out of reach (it measures from the eye to the block's middle) walks one cell closer
  and clicks again, twice at most and never closer than 1, then is :unreachable.

  One call is the whole run (walk, click, walks closer); it yields :continue only while the go-to child waits on the
  world. There is no second click.

  Checked at the start (never): :bad-args, :needs-redstone, :not-toggleable and :bad-state (with :valid). Waits
  before every round (later): :no-block and :not-loaded (the block is not loaded). A body standing in the door or
  gate to be closed first steps to a free cell beside it. A called job whose block has no block ends condition-failed
  :no-block.

  Ends :done (:changed or already there; data :pos :block :wanted :was :now), or impossible with the start causes
  above, or condition-failed with :no-block, :gone (block vanished or unloaded) or :refused (useOn said cannot or
  no-item), or nothing-possible with :unreachable, :unchanged (a click that changed nothing: iron-like block,
  protected area or lag, which cannot be told apart), :wrong-way (a block that moved but not to the wanted state) or
  :unseen (the block cannot be made out after the click).")

(a/defargs args
  {:pos {:doc "the block, [x y z] or {:x :y :z}; either half of a door" :spec ::a/pos :default nil}
   :state {:doc ":open or :closed (gate, door, trapdoor), :on or :off (lever), :press (button)" :spec (a/or-of #{:open :closed :on :off :press} #{"open" "closed" "on" "off" "press"}) :default nil}})

(a/defargs settings
  {::reach {:default 3 :doc "Walk until within this many cells of the block (the click reaches 4.5 from the eye)." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 120 :doc "Seconds a toggle may take: the walk, the click and the walks closer." :spec (a/num-in 0 nil)}
   ::switch-radius {:default 6 :doc "How far from an iron door or trapdoor a lever or button counts as its switch." :spec (a/num-in 0 nil)}
   ::switch-wait-s {:default 0.5 :doc "Seconds waited after a switch is worked before the iron door is read." :spec (a/num-in 0 nil)}
   ::max-closer {:default 2 :doc "How often a click the game answers out of reach is followed by a walk one cell closer." :spec (a/int-in 0 nil)}})
(defn max-closer [] (settings/get settings ::max-closer))
(defn time-s [] (settings/get settings ::job-time-s))

(def safe? (ready/safe-at-build (fn [c] (a/cell (:pos (:args c))))))

(def fit? ready/fit?)

(def target [:pos])

(def defaults {:spend {:job-time ::job-time-s}})

(def valid-states
  {:openable #{:open :closed} :iron #{:open :closed} :lever #{:on :off} :button #{:press}})

(defn switches
  "The levers and buttons seen within ::switch-radius of the iron block at pos that this run has not worked, nearest to it first."
  [c pos]
  (let [r (settings/get settings ::switch-radius)
        tried (set (when (:view c) (:tried (ctx/mem c))))]
    (->> (look/seen-blocks (:primitives c) {:match #(contains? #{:lever :button} (click/kind-of %)) :radius (+ r (u/dist (u/self-pos c) pos)) :live? true :properties? true})
         (filter #(and (<= (u/dist (:pos %) pos) r) (not (tried (:pos %)))))
         (sort-by #(u/dist (:pos %) pos)))))

(defn parse
  "{:pos {:x :y :z} :state kw} of the args, or {:error text}."
  [{:keys [pos state]}]
  (let [parsed (places/parse-pos pos)
        s (some-> state keyword)]
    (cond
      (nil? pos) {:error "toggle needs :pos"}
      (:reason parsed) {:error (:message parsed)}
      (nil? s) {:error "toggle needs :state"}
      :else {:pos (:pos parsed) :state s})))

(defn report
  "The result map for status and reason, with the block's facts."
  [status reason pos state more]
  (merge {:status status :reason reason :pos pos :wanted state} more))

(def condition-failures "Give-up reasons that read as a failed condition, the others as nothing possible." #{:gone :refused})

(defn give-up!
  [c reason pos state text more]
  (let [result (report :stopped reason pos state more)]
    (ctx/end c (if (condition-failures reason) :condition-failed :nothing-possible) {:reason reason :hint text} (dissoc result :status :reason))))

(def bad-args
  "Never :bad-args unless :pos and :state parse."
  (checks/args-fit "toggle needs a :pos and a :state" #(nil? (:error (parse (:args %))))))

(defn known-block
  "The block at pos as the body senses it, or nil when the cell is unloaded or not made out (unknown)."
  [p pos]
  (let [b (u/block-at p pos)]
    (when-not (true? (some-> b .-unknown)) b)))

(defn ^:async look-block!
  "known-block after a look at pos when it is not made out (a gate in the dark, up close)."
  [c pos]
  (let [{:keys [x y z]} pos]
    (when (dig-look/unknown? (:primitives c) [x y z]) (await (dig-look/look-at! c [x y z])))
    (known-block (:primitives c) pos)))

(defn block-facts
  "Never :needs-redstone, :not-toggleable or :bad-state when the block at :pos can never be put in :state by hand;
  later :no-block when it is air. A block not loaded says nothing (can-run? waits)."
  [c]
  (let [{:keys [pos state error]} (parse (:args c))
        b (when-not error (known-block (:primitives c) pos))
        block (some-> b .-name)
        kind (click/kind-of block)]
    (cond
      (or error (nil? b)) (contract/ok :ready "nothing settled yet")
      (click/air? block) (contract/later :no-block (str "no block at " (pr-str pos)) :pos pos)
      (and (= :iron kind) (empty? (switches c pos)))
      (contract/never :needs-redstone (str block " at " (pr-str pos) " ignores a hand and no lever or button is seen beside it") :block block)
      (nil? kind) (contract/never :not-toggleable (str block " at " (pr-str pos) " is not a gate, door, trapdoor, lever or button") :block block)
      (not (contains? (valid-states kind) state))
      (contract/never :bad-state (str block " at " (pr-str pos) " cannot be " (name state)) :block block :valid (sort (valid-states kind)))
      :else (contract/ok :ready "a block a hand can toggle"))))

(def can-start? (cond/and bad-args block-facts))

(defn waits
  "Later :not-loaded (the block is not loaded) or :no-block (it is gone)."
  [c]
  (let [{:keys [pos state error]} (parse (:args c))]
    (if error
      (contract/ok :args "nothing to wait for")
      (let [b (u/block-at (:primitives c) pos)]
        (cond
          (nil? b) (contract/later :not-loaded "the block is not loaded" :pos pos)
          (true? (.-unknown b)) (contract/ok :ready "not made out yet: the walk looks at it")
          (click/air? (.-name b)) (contract/later :no-block (str "no block at " (pr-str pos)) :pos pos)
          :else (contract/ok :ready "nothing to wait for"))))))

(def can-run? waits)

(defn done?
  "Done once the block is in the wanted state; the click's facts (:was :now) are the result when this call made it so
  and the block still shows the state that click gave it (else another actor flipped it since)."
  [c]
  (let [{:keys [pos state error]} (parse (:args c))
        b (when-not error (known-block (:primitives c) pos))
        reached (boolean (and b (contains? (valid-states (click/kind-of (.-name b))) state) (click/reached? state (click/props-of b))))
        saved (:clicked (ctx/mem c))
        clicked (when (and b (= (:now saved) (click/props-of b))) saved)]
    (if reached
      (contract/ok (if clicked :changed :already) (str (.-name b) " at " (pr-str pos) " is " (name state))
                   :units :toggles :done 1 :left 0
                   :result (merge {:pos pos :wanted state :reason (if clicked :changed :already) :block (.-name b) :now (click/props-of b)} clicked))
      (contract/later :not-yet "the block is not in the wanted state" :units :toggles :done 0 :left 1))))

(defn quote-of [_c] {:uses {:job-time [0.1 2 (time-s)]}})

(defn reach
  "The reach the walk aims for now: :reach, less one for each walk closer after a click out of reach (at least 1)."
  [c]
  (max 1 (- (settings/get settings ::reach) (:closer (ctx/mem c) 0))))

(defn closer!
  "The click was out of reach (the game measures from the eye to the block's middle, so a body that stopped at the
  edge of :reach can be short): walk again, one cell closer."
  [c]
  (ctx/update-mem! c #(-> % (dissoc :arrived) (update :closer (fnil inc 0))))
  :again)

(defn ^:async click!
  "Click once with the hand as it is and read the block again."
  [c pos state block]
  (let [{:keys [outcome facts status reason]} (await (click/click! c pos state block))
        text (str block " at " (pr-str pos))]
    (case outcome
      :gone (give-up! c :gone pos state (str text " is gone") facts)
      :unseen (give-up! c :unseen pos state (str text " cannot be made out after the click (too dark or too far)") facts)
      :unreachable (if (and (< (:closer (ctx/mem c) 0) (max-closer)) (> (reach c) 1))
                     (closer! c)
                     (give-up! c :unreachable pos state (str text " is out of reach") facts))
      :refused (give-up! c :refused pos state (str text " refused the click: " status " " reason) facts)
      :changed (do (ctx/update-mem! c assoc :clicked (select-keys facts [:was :now])) :again)
      :unchanged (give-up! c :unchanged pos state (str text " did not change after the click (protected, iron-like or lag)") facts)
      :wrong-way (give-up! c :wrong-way pos state (str text " moved, but not to " (name state)) facts))))

(defn ^:async walk! [c pos state block]
  (let [r (await (child/call-walk* c :walk {:pos pos :range (reach c) :doors :never :ways #{:walk :clear-seal}}))]
    (cond
      (= :continue r) :continue
      (:arrived (:data r)) (do (ctx/update-mem! c assoc :arrived true) :again)
      :else (give-up! c :unreachable pos state (str "cannot walk within reach of " block " at " (pr-str pos)) {:block block}))))

(defn ^:async step-out!
  "Step off the column the body stands in (jobs.lib.step-off, a cell beside it)."
  [c pos state block]
  (let [[x y z] (map #(js/Math.floor %) (let [{:keys [x y z]} (u/self-pos c)] [x y z]))
        r (await (step-off/step-off-zoned! c {:x x :y y :z z} {:reach 1}))]
    (cond
      (= :continue r) :continue
      (= :arrived r) :again
      :else (give-up! c :unreachable pos state (str "cannot step out of " block " at " (pr-str pos)) {:block block :why (:unreachable r)}))))

(defn switch-state
  "The state that works switch sw: a lever to the other side of its powered, a button pressed."
  [sw]
  (if (= :lever (click/kind-of (:name sw)))
    (if (click/reached? :on (:properties sw)) :off :on)
    :press))

(defn ^:async work-switch!
  "The iron block at pos is not in state: work the nearest untried switch (a toggle child, :continue while it waits), wait a moment and read the door.
  :again, or the ending when no switch is left or the door is gone."
  [c pos state block]
  (let [sw (or (:switch (ctx/mem c)) (first (switches c pos)))]
    (if-not sw
      (give-up! c :unchanged pos state (str block " at " (pr-str pos) " did not move for any lever or button seen beside it") {:block block})
      (let [_ (ctx/update-mem! c assoc :switch sw)
            e (if (:worked (ctx/mem c))
                :done
                (await (child/run!* c :switch 'jobs.access.toggle {:pos (:pos sw) :state (switch-state sw)})))]
        (if (= :continue e)
          :continue
          (let [_ (ctx/update-mem! c assoc :worked true)
                w (await (child/pause! c (settings/get settings ::switch-wait-s) :waiting-for-door))]
            (if (= :continue w)
              :continue
              (let [b (known-block (:primitives c) pos)]
                (ctx/update-mem! c #(-> % (update :tried (fnil conj []) (:pos sw)) (dissoc :switch :worked)))
                (when (and b (click/reached? state (click/props-of b)))
                  (ctx/update-mem! c assoc :clicked {:was nil :now (click/props-of b)}))
                :again))))))))

(defn ^:async next!
  "One piece of the run: the step out of the closed door's column, the walk or the click. :again, :continue (a walk waits) or the ending."
  [c]
  (let [{:keys [pos state]} (parse (:args c))
        b (known-block (:primitives c) pos)
        block (some-> b .-name)]
    (cond
      (= :iron (click/kind-of block)) (await (work-switch! c pos state block))

      (and (= :openable (click/kind-of block)) (= :closed state)
           (not (click/reached? state (click/props-of b)))
           (click/standing-in? (u/self-pos c) pos (:half (click/props-of b))))
      (await (step-out! c pos state block))

      (and (not (:arrived (ctx/mem c)))
           (not (or (u/within? (u/self-pos c) pos (reach c))
                    (<= (u/dist (u/self-pos c) pos) (reach c)))))
      (await (walk! c pos state block))

      :else (let [b (await (look-block! c pos))]
              (await (click! c pos state (or (some-> b .-name) block)))))))

(defn ^:async round
  "The whole run: next! until it ends, a pace between pieces."
  [c]
  (await (pace/steps! c #(next! c))))
