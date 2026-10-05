(ns jobs.access.toggle
  (:require [engine.access.click :as click]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.places :as places]))

(def doc
  "Put one block into a WANTED :state. The state is read first. A block already there is left alone and the run
  ends :already with no click. :pos is [x y z] or {:x :y :z}.

  States by block:
  - :open / :closed: fence gates, doors and trapdoors of every wood and of copper. A door is two blocks that
    move together, either half may be named. A double door is two blocks, so two runs.
  - :on / :off: a lever.
  - :press: a button. It counts when the click shows it powered.

  Iron doors and iron trapdoors ignore a hand (:needs-redstone). Any other block is :not-toggleable. A state the
  block cannot have is :bad-state with :valid. Closing a door, gate or trapdoor the body stands in is declined
  :standing-in (the check waits for it), opening never is. Zones and footprints do not apply, since no block
  type changes.

  The body walks within :reach of the block (jobs.movement.go-to child with :doors :never, so the approach
  opens nothing). A walk that gives up is :unreachable. It then clicks once with an empty hand (useOn with no
  item, which never tosses what was held; a hand that cannot be emptied is :no-room) and reads the block again.
  A click the game answers out of reach (it measures from the eye to the block's middle) walks one cell closer
  and clicks again, twice at most and never closer than 1, then is :unreachable.

  There is no second click. A click that changed nothing (iron-like block, protected area or lag, which cannot
  be told apart) is :unchanged. A block that moved but not to the wanted state is :wrong-way. Both give one
  warn toggle.gave-up. Other reasons: :gone (block vanished or unloaded), :refused (useOn said cannot or
  no-item).

  Declines before any walk with warn toggle.declined and :reason :bad-args, :not-loaded, :no-block,
  :not-toggleable, :needs-redstone, :bad-state or :standing-in.

  Ends with info toggle.done (:changed or :already) and the result {:status :done|:declined|:gave-up :reason
  :pos :block :wanted :was :now}. The pen-gate trigger may shut a planned pen gate this job opened once the
  body has been more than 2 blocks away for 4 s.")

(def args
  {:pos {:doc "the block, [x y z] or {:x :y :z}; either half of a door" :default nil}
   :state {:doc ":open or :closed (gate, door, trapdoor), :on or :off (lever), :press (button)" :default nil}
   :reach {:doc "walk until within this many cells of the block (the click reaches 4.5 from the eye)" :default 3}})

;; a walk the go-to child gives up on is three fruitless rounds
(def backoff {:after 9})

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

(defn decline!
  ([c reason pos state text] (decline! c reason pos state text nil))
  ([c reason pos state text more]
   (let [result (report :declined reason pos state more)]
     (ctx/emit! c :toggle.declined :warn (assoc result :text (str "toggle: " text)))
     (ctx/result! c result)
     :done)))

(defn give-up!
  [c reason pos state text more]
  (let [result (report :gave-up reason pos state more)]
    (ctx/emit! c :toggle.gave-up :warn (assoc result :text (str "toggle: " text)))
    (ctx/result! c result)
    :done))

(defn finish!
  [c reason pos state text more]
  (let [result (report :done reason pos state more)]
    (ctx/emit! c :toggle.done :info (assoc result :text text))
    (ctx/result! c result)
    :done))

(defn check
  "True, or a wait for what the block's state needs from the body: :not-loaded (the block is not loaded) or
  :standing-in (the body stands in the door or gate to be closed; it must step out). Bad args and every other decline
  stay with the round, which ends with its result; so do these for a toggle run as a child (herd, rail-line), whose
  parent reads the result."
  [c]
  (let [{:keys [pos state error]} (parse (:args c))]
    (if error
      true
      (let [b (.blockAt (:primitives c) (clj->js pos))
            props (some-> b click/props-of)]
        (cond
          (nil? b) (ctx/wait c {:reason :not-loaded :pos pos})
          (and (= :openable (click/kind-of (.-name b))) (= :closed state)
               (not (click/reached? state props))
               (click/standing-in? (u/self-pos c) pos (:half props)))
          (ctx/wait c {:reason :standing-in :pos pos :block (.-name b)})
          :else true)))))

(def max-closer "How often a click the game answers out of reach is followed by a walk one cell closer." 2)

(defn reach
  "The reach the walk aims for now: :reach, less one for each walk closer after a click out of reach (at least 1)."
  [c]
  (max 1 (- (:reach (:args c)) (:closer (ctx/mem c) 0))))

(defn closer!
  "The click was out of reach (the game measures from the eye to the block's middle, so a body that stopped at the
  edge of :reach can be short): walk again, one cell closer."
  [c]
  (ctx/update-mem! c #(-> % (dissoc :arrived) (update :closer (fnil inc 0))))
  :continue)

(defn ^:async click!
  "Click once with an empty hand and read the block again."
  [c pos state block]
  (let [{:keys [outcome facts why status reason]} (await (click/click! c pos state block))
        text (str block " at " (pr-str pos))]
    (case outcome
      :gone (give-up! c :gone pos state (str text (if (= :unloaded why) " is no longer loaded" " is gone")) facts)
      :no-room (give-up! c :no-room pos state (str "no room to empty the hand at " text) facts)
      :unreachable (if (and (< (:closer (ctx/mem c) 0) max-closer) (> (reach c) 1))
                     (closer! c)
                     (give-up! c :unreachable pos state (str text " is out of reach") facts))
      :refused (give-up! c :refused pos state (str text " refused the click: " status " " reason) facts)
      :changed (finish! c :changed pos state (str text " is now " (name state)) facts)
      :unchanged (give-up! c :unchanged pos state (str text " did not change after the click (protected, iron-like or lag)") facts)
      :wrong-way (give-up! c :wrong-way pos state (str text " moved, but not to " (name state)) facts))))

(defn ^:async walk! [c pos state block]
  (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos pos :range (reach c) :doors :never}))]
    (cond
      (not= :done r) :continue
      (:arrived (ctx/child-result c :walk)) (do (ctx/update-mem! c assoc :arrived true) :continue)
      :else (give-up! c :unreachable pos state (str "cannot walk within reach of " block " at " (pr-str pos)) {:block block}))))

(defn ^:async round [c]
  (let [{:keys [pos state error]} (parse (:args c))]
    (if error
      (decline! c :bad-args nil nil error)
      (let [b (.blockAt (:primitives c) (clj->js pos))
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
