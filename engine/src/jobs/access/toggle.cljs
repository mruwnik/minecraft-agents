(ns jobs.access.toggle
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.click :as click]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [jobs.lib.places :as places]))

(def doc
  "Put one block into a WANTED :state. The state is read first. A block already there is left alone: the job is done
  with no click. :pos is [x y z] or {:x :y :z}.

  States by block:
  - :open / :closed: fence gates, doors and trapdoors of every wood and of copper. A door is two blocks that
    move together, either half may be named. A double door is two blocks, so two runs.
  - :on / :off: a lever.
  - :press: a button. It counts when the click shows it powered.

  Iron doors and iron trapdoors ignore a hand (:needs-redstone). Any other block is :not-toggleable. A state the
  block cannot have is :bad-state with :valid. Zones and footprints do not apply, since no block type changes.

  The body walks within :reach of the block (jobs.movement.go-to child with :doors :never, so the approach
  opens nothing). A walk that gives up is :unreachable. It then clicks once with the hand as it is (useOn with no
  item: what is held stays in hand) and reads the block again.
  A click the game answers out of reach (it measures from the eye to the block's middle) walks one cell closer
  and clicks again, twice at most and never closer than 1, then is :unreachable.

  One call is the whole run (walk, click, walks closer); it yields :continue only while the go-to child waits on the
  world. There is no second click.

  Waits (conditions): :not-loaded (the block is not loaded) and :standing-in (the body stands in the door or gate to be
  closed; it must step out).

  Ends :done (:changed or already there; data :pos :block :wanted :was :now), or impossible with the cause :bad-args,
  :not-toggleable, :needs-redstone or :bad-state (with :valid), or condition-failed with :no-block, :gone (block vanished or
  unloaded) or :refused (useOn said cannot or no-item), or nothing-possible with :unreachable, :unchanged (a click
  that changed nothing: iron-like block, protected area or lag, which cannot be told apart), :wrong-way (a block that
  moved but not to the wanted state) or :unseen. Each refusal is one warn toggle.declined, each give-up one warn
  toggle.gave-up, a click that worked info toggle.done. The pen-gate trigger may shut a planned pen gate this job
  opened once the body has been more than 2 blocks away for 4 s.")

(a/defargs args
  {:pos {:doc "the block, [x y z] or {:x :y :z}; either half of a door" :spec ::a/pos :default nil}
   :state {:doc ":open or :closed (gate, door, trapdoor), :on or :off (lever), :press (button)" :spec (a/or-of #{:open :closed :on :off :press} #{"open" "closed" "on" "off" "press"}) :default nil}
   :reach {:doc "walk until within this many cells of the block (the click reaches 4.5 from the eye)" :spec (a/num-in 0 nil) :default 3}})

(a/defargs settings
  {::time-s {:default 120 :doc "Seconds a toggle may take: the walk, the click and the walks closer." :spec (a/num-in 0 nil)}
   ::max-closer {:default 2 :doc "How often a click the game answers out of reach is followed by a walk one cell closer." :spec (a/int-in 0 nil)}})
(defn max-closer [] (settings/get settings ::max-closer))
(defn time-s [] (settings/get settings ::time-s))

(def target [:pos])

(def defaults {:spend {:time ::time-s}})

(def valid-states
  {:openable #{:open :closed} :lever #{:on :off} :button #{:press}})

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

(def impossible-reasons "Decline reasons the args or the block's kind settle for good." #{:bad-args :needs-redstone :not-toggleable :bad-state})

(defn decline!
  ([c reason pos state text] (decline! c reason pos state text nil))
  ([c reason pos state text more]
   (let [result (report :declined reason pos state more)]
     (ctx/emit! c :toggle.declined :warn (assoc result :text (str "toggle: " text)))
     (ctx/end c (if (impossible-reasons reason) :impossible :condition-failed) reason (dissoc result :status :reason)))))

(defn give-up!
  [c reason pos state text more]
  (let [result (report :stopped reason pos state more)]
    (ctx/emit! c :toggle.gave-up :warn (assoc result :text (str "toggle: " text)))
    (ctx/end c (if (condition-failures reason) :condition-failed :nothing-possible) reason (dissoc result :status :reason))))

(defn finish!
  [c reason pos state text more]
  (let [result (report :done reason pos state more)]
    (ctx/emit! c :toggle.done :info (assoc result :text text))
    (ctx/end c :done nil (dissoc result :status))))

(defn waits
  "The cause of the wait the block's state needs from the body, or nil: :not-loaded (the block is not loaded) or
  :standing-in (the body stands in the door or gate to be closed; it must step out). Bad args and every other refusal
  end the round, so do these for a toggle run as an old child (herd, rail-line), whose parent reads the result."
  [c]
  (let [{:keys [pos state error]} (parse (:args c))]
    (when-not error
      (let [b (u/block-at (:primitives c) pos)
            props (some-> b click/props-of)]
        (cond
          (nil? b) {:reason :not-loaded :pos pos}
          (and (= :openable (click/kind-of (.-name b))) (= :closed state)
               (not (click/reached? state props))
               (click/standing-in? (u/self-pos c) pos (:half props)))
          {:reason :standing-in :pos pos :block (.-name b)})))))

(def conditions [[:fn waits]])

(defn done?
  "Whether the block is in the wanted state already."
  [c]
  (let [{:keys [pos state error]} (parse (:args c))
        b (when-not error (u/block-at (:primitives c) pos))
        reached (boolean (and b (contains? (valid-states (click/kind-of (.-name b))) state) (click/reached? state (click/props-of b))))]
    {:done? reached :units :toggles :done (if reached 1 0) :left (if reached 0 1)}))

(defn quote-of [_c] {:uses {:time [0.1 2 (time-s)]}})

(defn reach
  "The reach the walk aims for now: :reach, less one for each walk closer after a click out of reach (at least 1)."
  [c]
  (max 1 (- (:reach (:args c)) (:closer (ctx/mem c) 0))))

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
      :changed (finish! c :changed pos state (str text " is now " (name state)) facts)
      :unchanged (give-up! c :unchanged pos state (str text " did not change after the click (protected, iron-like or lag)") facts)
      :wrong-way (give-up! c :wrong-way pos state (str text " moved, but not to " (name state)) facts))))

(defn ^:async walk! [c pos state block]
  (let [r (await (child/call-walk* c :walk {:pos pos :range (reach c) :doors :never :escalate false}))]
    (cond
      (= :continue r) :continue
      (:arrived (:data r)) (do (ctx/update-mem! c assoc :arrived true) :again)
      :else (give-up! c :unreachable pos state (str "cannot walk within reach of " block " at " (pr-str pos)) {:block block}))))

(defn ^:async next!
  "One piece of the run: a decline, the walk, or the click. :again, :continue (the walk waits) or the ending."
  [c]
  (let [{:keys [pos state error]} (parse (:args c))]
    (if error
      (decline! c :bad-args nil nil error)
      (let [b (u/block-at (:primitives c) pos)
            block (some-> b .-name)
            kind (click/kind-of block)
            text (str block " at " (pr-str pos))]
        (cond
          (nil? b) (decline! c :not-loaded pos state (str (pr-str pos) " is not loaded"))
          (click/air? block) (decline! c :no-block pos state (str "no block at " (pr-str pos)))
          (= :iron kind) (decline! c :needs-redstone pos state (str text " ignores a hand") {:block block})
          (nil? kind) (decline! c :not-toggleable pos state (str text " is not a gate, door, trapdoor, lever or button") {:block block})
          (not (contains? (valid-states kind) state))
          (decline! c :bad-state pos state (str text " cannot be " (name state)) {:block block :valid (sort (valid-states kind))})
          (click/reached? state (click/props-of b)) (finish! c :already pos state (str text " is already " (name state)) {:block block :now (click/props-of b)})
          (and (= :openable kind) (= :closed state) (click/standing-in? (u/self-pos c) pos (:half (click/props-of b))))
          (decline! c :standing-in pos state (str "the body stands in " text) {:block block})
          (and (not (:arrived (ctx/mem c)))
               (not (or (u/within? (u/self-pos c) pos (reach c))
                        (<= (u/dist (u/self-pos c) pos) (reach c))))) (await (walk! c pos state block))
          :else (await (click! c pos state block)))))))

(defn ^:async round
  "The whole run: next! until it ends, a pace between pieces."
  [c]
  (await (pace/steps! c #(next! c))))
