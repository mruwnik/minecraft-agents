(ns jobs.lib.click
  "Working a door, gate, trapdoor, lever or button with one click of an empty hand: which blocks a hand works, the state of a
  block read back, and the click itself. jobs.access.toggle and the walk driver (jobs.lib.pass) share it."
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.util :as u]))

(defn kind-of
  "What works block name: :iron (a hand cannot), :openable, :lever, :button, or nil."
  [name]
  (cond
    (nil? name) nil
    (#{"iron_door" "iron_trapdoor"} name) :iron
    (re-find #"_(fence_gate|door|trapdoor)$" name) :openable
    (= "lever" name) :lever
    (str/ends-with? name "_button") :button
    :else nil))

(defn reached?
  "Whether a block with properties props (a map with keyword keys, or nil) is in state; the strings \"true\" and
  \"false\" read as booleans."
  [state props]
  (let [on? (fn [k] (let [v (get props (keyword k))] (or (true? v) (= "true" v))))]
    (case state
      :open (on? "open")
      :closed (not (on? "open"))
      :on (on? "powered")
      :off (not (on? "powered"))
      :press (on? "powered")
      false)))

(defn props-of [b] (some-> b .-properties (js->clj :keywordize-keys true)))

(defn air? [name] (or (nil? name) (= "air" name) (str/ends-with? name "_air")))

(defn standing-in?
  "Whether the body stands in the column of an openable block at cell: its feet or its head are in that cell, or in
  either cell of a door (half is the :half property of the block, or nil for a gate or trapdoor)."
  [self {:keys [x y z]} half]
  (let [fx (js/Math.floor (:x self)) fy (js/Math.floor (:y self)) fz (js/Math.floor (:z self))
        low (if (= "upper" half) (dec y) y)
        high (if half (inc low) low)]
    (and (= x fx) (= z fz) (<= low (inc fy)) (<= fy high))))

(defn ^:async click!
  "Click the block at pos once with an empty hand, then read it again. Returns {:outcome :facts ...}, facts
  being {:block :was :now}. outcome is one of:
  - :changed, the block is now in state
  - :unchanged, the click moved nothing
  - :wrong-way, it moved, but not to state
  - :gone, with :why :missing (not there) or :unloaded (can no longer be read)
  - :no-room, the hand cannot be emptied
  - :unreachable, out of reach
  - :refused, the click was turned away (:status and :reason are the primitive's)"
  [c pos state block]
  (let [r (await (ctx/act c :useOn (clj->js {:pos pos})))
        status (.-status r)
        before (js->clj (some-> r .-before .-properties) :keywordize-keys true)
        after (js->clj (some-> r .-after .-properties) :keywordize-keys true)
        b (u/block-at (:primitives c) pos)
        now (props-of b)
        facts {:block block :was before :now now}
        out (fn [outcome & {:as more}] (merge {:outcome outcome :facts facts} more))]
    (cond
      (= "missing" status) (out :gone :why :missing)
      (= "no-room" status) (out :no-room)
      (= "unreachable" status) (out :unreachable)
      (not (#{"used" "unchanged"} status)) (out :refused :status status :reason (.-reason r))
      (nil? b) (out :gone :why :unloaded)
      (or (reached? state now) (reached? state after)) (out :changed)
      (= before now) (out :unchanged)
      :else (out :wrong-way))))
