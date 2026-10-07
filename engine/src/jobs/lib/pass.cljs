(ns jobs.lib.pass
  "Walking a plan that opens doors, gates and trapdoors by hand, or iron doors by a button, lever or plate. The plan is cut at each step that opens something.
  The walk goes up to the step before it, the block is clicked open with an empty hand (jobs.lib.click) and read
  back, and the walk goes on. Once the body is out of the opened block's column the walker shuts it again, if its
  policy says so. The executor's tick never sees an :opens step.

  Policy (the :doors arg of jobs.movement.go-to):
  - :shut: the walker shuts what it opened.
  - :leave-open: it leaves it open.
  - :never: the plan has no such step.
  :shut-also (a fn of cell [x y z]) shuts a block whatever the policy when it says so: go-to's passes one for blocks in
  or next to another owner's zone, so a pass through somebody's pen lets nothing out. The walker only shuts a block it
  found shut, never one that was open already.

  Every block the walker opens gets an :opened memory entry {:cell {:x :y :z} :by job-id :t ms :shut? bool} before the
  click (:shut? false: the policy leaves it open). The entry is dropped when the block is shut.
  A walk cut between the open and the shut leaves the entry. The next round of the same job shuts it
  (shut-leftovers!), or the door-left trigger (triggers.maintenance.door-left) does, with jobs.maintenance.shut-doors.
  A block that stays open (an animal stands in its cell, so the walker waits and tries again but never pushes; or the
  click did nothing) keeps its entry and gets one :door-left-open warn."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.click :as click]
            [engine.ctx :as ctx]
            [jobs.lib.combat :as combat]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.world :as wworld]))

(a/defargs settings
  {::shut-waits {:default 4 :doc "Tries to shut a block with an animal in its cell, after the first." :spec (a/int-in 0 nil)}
   ::shut-wait-ms {:default 500 :doc "Wait between those tries, in ms." :spec (a/int-in 0 nil)}
   ::leftover-reach {:default 4 :doc "A leftover :opened entry is shut when the body is this near." :spec (a/int-in 0 nil)}
   ::await-polls {:default 6 :doc "Reads of a door a button or plate was to open." :spec (a/int-in 1 nil)}
   ::await-poll-ms {:default 100 :doc "Wait between those reads, in ms." :spec (a/int-in 0 nil)}})

(def opened-policy {:cap 50 :ttl :forever})

(defn shut-waits [] (settings/get settings ::shut-waits))
(defn shut-wait-ms [] (settings/get settings ::shut-wait-ms))
(defn leftover-reach [] (settings/get settings ::leftover-reach))

;; ---------------------------------------------------------------- pure

(defn with-climbs
  "steps with each :open move (a vertical step into a trapdoor's cell) as the climb it is, up or down."
  [steps]
  (vec (map-indexed (fn [i s]
                      (if (= :open (:move s))
                        (assoc s :move (if (> (:y s) (:y (nth steps (dec i)))) :climb-up :climb-down))
                        s))
                    steps)))

(defn column-of
  "The column of the openable block at cell {:x :y :z} with properties props: {:cell :low :high}, the y of the lowest and
  highest cell of the block (a door has two)."
  [{:keys [x y z] :as cell} props]
  (let [half (:half props)
        low (if (= "upper" half) (dec y) y)]
    {:cell (select-keys cell [:x :y :z]) :x x :z z :low low :high (if half (inc low) low)}))

(defn in-column?
  "Whether a body with its feet in the cell step ({:x :y :z}) has its feet or head in the column col."
  [{:keys [x z low high]} {sx :x sy :y sz :z}]
  (and (= x sx) (= z sz) (<= low (inc sy)) (<= sy high)))

(defn first-in-run
  "The index of the first of the steps just before step k that stand in the columns of the blocks step k opens (k itself when
  the step before it is out of them), but never before step s+1: a plan that goes through a gate's cell gets opened from the
  cell before it."
  [steps s k]
  (let [cols (map (fn [{:keys [x y z]}] {:x x :z z :low y :high y}) (:opens (nth steps k)))]
    (loop [j k]
      (if (and (> (dec j) s) (some #(in-column? % (nth steps (dec j))) cols))
        (recur (dec j))
        j))))

(defn segment
  "The segment that starts at step s, with the columns in pending still open: {:end the index of its last step, :open the
  index of the step whose :opens are to be opened once the body is there, nil when it is not}. The segment ends at the step
  before the first step of the run into the next opening's columns, or at the first step out of every column in pending (so
  a block can be shut as soon as the body is clear of it), or at the last step."
  [steps s pending]
  (let [last-i (dec (count steps))
        later (range (inc s) (inc last-i))
        k (first (filter #(seq (:opens (nth steps %))) later))
        cut (some->> k (first-in-run steps s) dec)
        clear (when (seq pending)
                (first (filter (fn [i] (not-any? #(in-column? % (nth steps i)) pending)) later)))
        end (apply min (remove nil? [cut clear last-i]))]
    {:end end :open (when (and k (= end cut)) k)}))

;; ---------------------------------------------------------------- the body's side

(defn block-at
  "The block at cell as the body sees or remembers it (JS), nil when unloaded or never seen."
  [c {:keys [x y z]}]
  (u/seen-block (:primitives c) {:x x :y y :z z}))

(defn ^:async seen-at!
  "block-at, after a look at cell when the body has not seen it (a door behind the head)."
  [c {:keys [x y z] :as cell}]
  (when (dig-look/unknown? (:primitives c) [x y z])
    (await (dig-look/look-at! c [x y z])))
  (block-at c cell))

(defn self-name [c] (.-username (.self (:primitives c))))

(defn shut?
  "Whether the walker shuts the block at cell after passing it, under policy doors and the caller's shut-also."
  [doors shut-also {:keys [x y z]}]
  (boolean (or (= :shut doors) (and shut-also (shut-also [x y z])))))

(defn ^:async open-block!
  "Open the block at cell by hand unless it is open. {:result :opened :column} (the walker opened it, its entry written,
  with shut? saying whether the walker will shut it again), {:result :was-open}, or {:result :stuck}: not a block a hand
  opens, or it did not open."
  [c cell shut?]
  (let [b (await (seen-at! c cell))
        name (some-> b .-name)
        props (click/props-of b)]
    (cond
      (nil? b) {:result :stuck}
      (click/air? name) {:result :was-open}
      (not= :openable (click/kind-of name)) {:result :stuck}
      (click/reached? :open props) {:result :was-open}
      :else
      (do (ctx/remember! c :opened {:cell cell :by (:id c) :t (ctx/now c) :shut? shut?} opened-policy)
          (let [{:keys [outcome]} (await (click/click! c cell :open name))]
            (if (= :changed outcome)
              {:result :opened :column (column-of cell props)}
              (do (ctx/forget-where! c :opened #(= cell (:cell %)))
                  {:result :stuck})))))))

(defn animals-in
  "The entities that are not items, and not the body, standing in the column col."
  [c {:keys [x z low high]}]
  (let [p (:primitives c) me (self-name c)]
    (filterv (fn [e]
               (let [pos (.-pos e)]
                 (and (not= "item" (.-kind e)) (not= me (.-username e))
                      (= x (js/Math.floor (.-x pos))) (= z (js/Math.floor (.-z pos)))
                      (<= low (js/Math.floor (.-y pos)) high))))
             (combat/sensed p {:radius 8 :max 64}))))

(defn left-open!
  "Warn that the block at cell stays open, and why."
  [c {:keys [x y z]} why]
  (ctx/emit! c :door-left-open :warn {:cell [x y z] :why why
                                      :text (str "the walker left the block at " [x y z] " open: " (name why))}))

(defn ^:async shut-column!
  "Shut the block of column col: an animal in its cell is waited out (shut-waits times shut-wait-ms) and never pushed. Its
  :opened entry is dropped when it is shut; when it is not, it stays and a :door-left-open warn says why."
  [c {:keys [cell] :as col}]
  (let [b (block-at c cell)
        name (some-> b .-name)]
    (cond
      (or (nil? b) (not= :openable (click/kind-of name)) (not (click/reached? :open (click/props-of b))))
      (ctx/forget-where! c :opened #(= cell (:cell %)))

      :else
      (loop [n 0]
        (cond
          (seq (animals-in c col))
          (if (< n (shut-waits))
            (do (await (ctx/act c :wait #js {:ms (shut-wait-ms)})) (recur (inc n)))
            (left-open! c cell :animal-in-the-way))

          :else
          (let [{:keys [outcome]} (await (click/click! c cell :closed name))]
            (if (= :changed outcome)
              (ctx/forget-where! c :opened #(= cell (:cell %)))
              (left-open! c cell :shut-failed))))))))

(defn ^:async shut-ready!
  "Shut each block of the columns pending that the body is out of; the columns it is still in."
  [c pending]
  (let [here (wworld/body-cell c)]
    (loop [todo pending kept []]
      (if-let [col (first todo)]
        (if (in-column? col here)
          (recur (rest todo) (conj kept col))
          (do (await (shut-column! c col))
              (recur (rest todo) kept)))
        kept))))

(defn await-polls [] (settings/get settings ::await-polls))
(defn await-poll-ms [] (settings/get settings ::await-poll-ms))

(defn ^:async await-open!
  "Read the block at cell back every await-poll-ms, up to await-polls times, until it is open: {:result :was-open} (it shuts
  itself again, so there is no entry to keep) or {:result :stuck}."
  [c cell]
  (loop [n 0]
    (let [b (block-at c cell)]
      (cond
        (and b (click/reached? :open (click/props-of b))) {:result :was-open}
        (>= n (await-polls)) {:result :stuck}
        :else (do (await (ctx/act c :wait #js {:ms (await-poll-ms)})) (recur (inc n)))))))

(defn ^:async work-activator!
  "Open the door at cell through its opener o, {:via \"button\" :at cell}: the button is pressed, a lever is pulled on (and
  left on, so the door stays open), a plate is the body's weight (the walk stops on it), then the door is read back. Same results as await-open!. A door a hand can open, behind a plate,
  is opened by hand when the plate did not."
  [c cell {:keys [via at]} shut?]
  (let [name (some-> (block-at c at) .-name)
        door (block-at c cell)
        pressed (when (and (#{"button" "lever"} via) (not (some-> door click/props-of (->> (click/reached? :open)))))
                  (if (= (keyword via) (click/kind-of name))
                    (:outcome (await (click/click! c at (if (= "lever" via) :on :press) name)))
                    :unchanged))
        r (if (#{nil :changed} pressed) (await (await-open! c cell)) {:result :stuck})]
    (if (and (= :stuck (:result r)) (= "plate" via) (= :openable (click/kind-of (some-> (block-at c cell) .-name))))
      (await (open-block! c cell shut?))
      r)))

(defn ^:async open-all!
  "Open each block of cells (a step's :opens: a hand opens it, or its :via button, lever or plate does), the walker's policy doors and
  shut-also deciding which it will shut again: {:pending the columns to shut, :stuck the cells that would not open}.
  What a button or plate opened shuts itself, and a lever is left on: neither has a column to shut."
  [c cells doors shut-also]
  (loop [todo cells pending [] stuck []]
    (if-let [o (first todo)]
      (let [cell (select-keys o [:x :y :z])
            will-shut? (shut? doors shut-also cell)
            {:keys [result column]} (await (if (:via o)
                                             (work-activator! c cell o will-shut?)
                                             (open-block! c cell will-shut?)))]
        (recur (rest todo)
               (cond-> pending (and (= :opened result) will-shut?) (conj column))
               (cond-> stuck (= :stuck result) (conj cell))))
      {:pending pending :stuck stuck})))

(defn body-at [c] (let [pos (.-pos (.self (:primitives c)))] [(.-x pos) (.-y pos) (.-z pos)]))

(defn ^:async walk!
  "Follow steps (a plan that may open things) once, segment by segment: [done ms] as jobs.lib.walk/walk! answers (its
  :step counted in steps), or {:status :door-stuck :cells [...]} for blocks that would not open. watch, when given, is the
  look-ahead (jobs.lib.walk.watch/watch-stop) of every segment walked with nothing left to shut behind, the rest of the plan as
  its :ahead. A walk that ends off its plan or stuck still shuts what
  it can; what it cannot (the body is in its column, or the walk was cut) keeps its :opened entry."
  [c steps {:keys [timeout-s doors shut-also watch]}]
  (let [last-i (dec (count steps))]
    (loop [steps (with-climbs steps) s 0 pending [] ms 0]
      (let [{e :end k :open} (segment steps s pending)
            seg-watch (when (and watch (empty? pending)) (assoc watch :ahead (subvec steps (inc e))))
            [done seg-ms] (if (> e s) (await (walk/walk! c (subvec steps s (inc e)) timeout-s seg-watch)) [nil 0])
            ms (+ ms seg-ms)
            pending (if done (await (shut-ready! c pending)) pending)]
        (cond
          (and done (not= :arrived (:status done)))
          [(cond-> done (:step done) (update :step + s)) ms]

          (and (= e last-i) (nil? k))
          [(or done {:status :arrived :at (body-at c)}) ms]

          (nil? k)
          (recur steps e pending ms)

          :else
          (let [{opened :pending :keys [stuck]} (await (open-all! c (:opens (nth steps k)) doors shut-also))]
            (if (seq stuck)
              [{:status :door-stuck :cells stuck :at (body-at c)} ms]
              (recur (update steps k dissoc :opens) e (into pending opened) ms))))))))

(defn ^:async shut-leftovers!
  "At the start of a round: shut the blocks an earlier round of this job opened and left (a walk cut between the open and the
  shut) that are within leftover-reach, when the entry says it was to be shut and the body is out of their column."
  [c]
  (let [mine (filterv #(= (:id c) (:by %)) (map :data (mem/entries (ctx/view c) :opened)))]
    (loop [todo mine]
      (when-let [{:keys [cell shut?]} (first todo)]
        (let [b (block-at c cell)
              col (column-of cell (click/props-of b))]
          (when (and shut?
                     (<= (u/dist (u/self-pos c) cell) (leftover-reach))
                     (not (in-column? col (wworld/body-cell c))))
            (await (shut-column! c col)))
          (recur (rest todo)))))))
