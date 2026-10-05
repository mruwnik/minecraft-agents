(ns jobs.maintenance.unstick
  (:require [engine.access.ledger :as ledger]
            [engine.jobs.tidy :as tidy]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.util :as u]
            [engine.jobs.reach :as reach]
            [engine.path.near :as near]
            [engine.triggers.stuck :as stuck]))

(def doc
  "Get a body out of being stuck: a job keeps calling moveTo and the body gets
  nowhere. Each round is one attempt, counted in job memory, except a round
  after which the body's feet are higher than ever yet in this spell (:best-y,
  from the start position): that is progress and is not counted, so climbing
  an 8-deep pit one block per round does not use the attempts up. Only counted
  attempts reach :max-attempts; a hard cap of :max-attempts + 8 (max-pillar)
  rounds per spell bounds the rest. First it waits
  (the :wait primitive, 50 ms steps, up to 1 s) for the body to be on the
  ground, so the cells it picks are the landed ones; still airborne, it uses
  them anyway. In the first round, before anything is placed or dug, it walks
  toward the stored goal with the engine's planner (engine.path.near/walk-near!
  with :doors :shut, at most walk-timeout-s per steer): a shut door, gate or
  trapdoor the job's moveTo cannot pass is opened, passed and shut again, so a
  body in a room is not pillared and dug out of it. When that walk moves the
  body more than :min-move blocks the spell is over (:done). Then: attempt 1 steps back one block the way it came, unless
  the body is in a pit (at least three of the four sides solid at feet+k for
  k = 0, 1, ... gives the depth d, capped at 8), where it goes straight to the
  next rule. Otherwise, when a placeable block is carried in a pit, it
  pillars: jumpPlace with the largest stack and count d (blocks it digged are
  carried, so pillaring is preferred every round). Otherwise it digs, toward
  the stored goal (no goal: the first cardinal), as a DOOR when the wall in
  front is one block thick (the cells two ahead at feet and head height are
  open): the solid blocks in front at feet and head height. Else as a STAIR,
  lifting the body one block: it digs each solid one of H, the cell above the
  head (headroom to jump), and F1, F2, the feet and head cells one block up
  in front, which stand on the front block at feet height (left solid; if
  it is open, the next cardinal is tried). When no side of the body's cell is
  solid (it stands inside a hollow wider than a pit, so it is not \"in a
  pit\" either), it first walks (moveTo range 0) to the nearest cell at the
  same level with a solid side, reached through standable cells within
  wall-search-radius (8) blocks, and digs the stair from there (none:
  \"stair: no solid step\"; the walk falls short: \"wall: moveTo <status>\"),
  then moves to F1 with range 0 (no reason when that arrives or the feet rose,
  else \"stair: moveTo <status>\"). A cell is not dug when the one above it is
  sand or gravel or any of its six neighbours is water or lava (the reason
  says which); any dig status but dug or missing ends the attempt with
  \"dig: <status>\" (bedrock gives cannot, a slow block timeout). The best
  pickaxe carried is equipped first. Each attempt that did nothing useful
  records why in job memory (:reasons). After every attempt it
  tries a moveTo toward the stored goal itself, capped at hop-blocks (3) along
  the way (range 1, :maxDistance 3; within 3 blocks it simply walks there). If
  that did not move the body more than :min-move blocks from where it stood
  (status is ignored), it retries once uncapped (range 1, :timeoutS 6). If
  either moved it that far the spell is over (:done); otherwise :continue. After :max-attempts counted attempts, or at the round cap, it emits the warn
  event unstick.failed with the position, :attempts (counted), :rounds (used, also what the :text reports), :reasons (each distinct reason once, with \" (xN)\" when repeated; job memory keeps the raw list) and a :text naming them, writes a :stuck memory entry (cap 10,
  ttl 1 hour) and ends so the list resumes.

  How it pairs with the stuck trigger (engine.triggers.stuck): both read only
  the :moved entries act writes after every moveTo. The trigger fires when the
  last :n are all bad and the newest is inside :window-ms; this job's check is that same
  condition, or an attempt already begun and not yet proven by a good move, so
  the spell is not abandoned halfway when its own bad attempts push the old
  evidence out of the window. The job's own moveTo calls write :moved entries
  too: the good one that ends the spell becomes the newest of the last :n, so
  the trigger is false at once and cannot re-fire until :n new bad moves
  happen. On failure the :stuck entry makes the trigger ignore every move
  before it, and stay quiet for :quiet-ms (5 min), so neither the failed
  attempts nor a body still blocked under the resumed job re-fire it.")

(def args
  {:n {:doc "bad moves in a row that count as stuck" :default (:n stuck/defaults)}
   :min-move {:doc "blocks a move must cover to count as progress" :default (:min-move stuck/defaults)}
   :window-ms {:doc "the newest of the bad moves must be at most this many ms old" :default (:window-ms stuck/defaults)}
   :quiet-ms {:doc "after giving up, the trigger stays quiet this many ms" :default (:quiet-ms stuck/defaults)}
   :max-attempts {:doc "attempts before giving up with unstick.failed" :default 6}})

(def backoff
  "Off: it bounds itself (gives up after :max-attempts with unstick.failed and a :stuck entry that quiets the trigger), and a backoff would end it before that give-up."
  false)

(def stuck-policy {:cap 10 :ttl (* 60 60 1000)})

(def hop-blocks 3)

(def retry-timeout-s 6)

(def open-names #{"air" "cave_air" "void_air"})

(def liquid-names #{"water" "lava"})

(def falling-names #{"sand" "red_sand" "gravel"})

(def pickaxes ["netherite_pickaxe" "diamond_pickaxe" "iron_pickaxe" "stone_pickaxe" "golden_pickaxe" "wooden_pickaxe"])

(def pillar-items
  "Solid blocks worth pillaring with. No sand or gravel, they fall."
  ["cobblestone" "dirt" "stone" "cobbled_deepslate" "deepslate" "netherrack" "andesite" "diorite" "granite"
   "oak_planks" "spruce_planks" "birch_planks" "oak_log" "spruce_log" "birch_log"])

(def max-pillar 8)

(def land-step-ms 50)

(def land-max-steps 20)

;; ------------------------------------------------------------------ geometry

(defn cell [pos]
  {:x (js/Math.floor (:x pos)) :y (js/Math.floor (:y pos)) :z (js/Math.floor (:z pos))})

(defn up [pos n] (update pos :y + n))

(defn block-name [c pos]
  (u/block-name (:primitives c) pos))

(defn open? [c pos] (contains? open-names (block-name c pos)))

(defn solid? [c pos]
  (let [n (block-name c pos)]
    (not (or (open-names n) (liquid-names n)))))

(defn heading
  "Unit step {:dx :dz} along the larger horizontal axis from a to b, or nil."
  [a b]
  (let [dx (- (:x b) (:x a)) dz (- (:z b) (:z a))]
    (cond
      (and (zero? dx) (zero? dz)) nil
      (>= (js/Math.abs dx) (js/Math.abs dz)) {:dx (js/Math.sign dx) :dz 0}
      :else {:dx 0 :dz (js/Math.sign dz)})))

(defn shift [pos {:keys [dx dz]}] (-> pos (update :x + dx) (update :z + dz)))

(defn reverse-heading [{:keys [dx dz]}] {:dx (- dx) :dz (- dz)})

(def cardinals [{:dx 1 :dz 0} {:dx -1 :dz 0} {:dx 0 :dz 1} {:dx 0 :dz -1}])

(defn last-move [c] (:data (ctx/latest c :moved)))

(defn bearings
  "What the stuck body was doing, read from the latest :moved entry before the
  attempts add their own: its :goal (the moveTo target), and :back, the way it
  came (from where that move ended to where it began; failing that, away from
  the goal)."
  [c here]
  (let [{:keys [from to target]} (last-move c)
        ahead (some->> target (heading here))]
    {:goal target
     :back (or (when (and from to) (heading to from))
               (some-> ahead reverse-heading))}))

(defn forward
  "The way the body was trying to go, from the stored goal."
  [c here]
  (some->> (:goal (ctx/mem c)) (heading here)))

(defn free-cell? [c pos] (and (open? c pos) (open? c (up pos 1))))

;; ------------------------------------------------------------------ attempts

(defn ^:async go! [c pos]
  (await (ctx/act c :moveTo (clj->js {:pos pos :range 0}))))

(defn ^:async step-back!
  "Attempt 1: one block back the way it came, if that cell is free (with no
  direction known, the first free neighbour)."
  [c]
  (let [here (cell (u/self-pos c))
        headings (if-let [h (:back (ctx/mem c))] [h] cardinals)
        spot (->> headings (map #(shift here %)) (filter #(free-cell? c %)) first)]
    (if spot
      (do (await (go! c spot)) nil)
      "step-back: no free cell")))

(def dig-ok-statuses #{"dug" "missing"})

(defn ^:async dig!
  [c pos]
  (await (tidy/dig! c pos true)))

(defn best-pickaxe [c]
  (let [have (set (map :name (u/inventory (:primitives c))))]
    (first (filter have pickaxes))))

(defn on-ground?
  "False only when the body says it is airborne (a missing key counts as on the ground)."
  [c]
  (not (false? (.-onGround (.self (:primitives c))))))

(defn ^:async land!
  "Wait in short steps until the body is on the ground, up to land-max-steps."
  [c]
  (loop [i 0]
    (when (and (< i land-max-steps) (not (on-ground? c)))
      (await (ctx/act c :wait (clj->js {:ms land-step-ms})))
      (recur (inc i)))))

(defn door-cells
  "The solid blocks in front at feet and head height when the wall is one block
  thick (both cells two ahead are open), else nil."
  [c here dir]
  (let [front (shift here dir)
        beyond (shift front dir)]
    (when (and (open? c beyond) (open? c (up beyond 1)))
      (seq (filter #(solid? c %) [front (up front 1)])))))

(defn stair-options
  "[{:front f :cells [...]} ...] for each heading (the goal's first) whose front block at feet height is solid to
  stand on: the solid ones of H (above the head), F1 and F2 (the next step's feet and head cells)."
  [c here dir]
  (keep (fn [d]
          (let [front (shift here d)]
            (when (solid? c front)
              {:front (up front 1)
               :cells (filter #(solid? c %) [(up here 2) (up front 1) (up front 2)])})))
        (distinct (cons dir cardinals))))

(defn stair-cells
  "The first of stair-options (the goal's heading first), or nil when no heading has a step."
  [c here dir]
  (first (stair-options c here dir)))

(defn unsafe-reason
  "Why digging pos is unsafe (a falling block above, liquid beside), else nil."
  [c pos]
  (let [at (str "(" (:x pos) " " (:y pos) " " (:z pos) ")")
        above (block-name c (up pos 1))
        beside (->> [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]
                    (keep (fn [[dx dy dz]] (liquid-names (block-name c (-> pos (update :x + dx) (update :y + dy) (update :z + dz)))))))]
    (cond
      (falling-names above) (str "dig: " above " above " at)
      (seq beside) (str "dig: " (first beside) " next to " at))))

(defn ^:async dig-cells!
  "Dig each pos in turn: nil when all were dug (or already gone), else the reason."
  [c cells]
  (loop [ts cells]
    (if-let [t (first ts)]
      (let [st (.-status (await (dig! c t)))]
        (if (contains? dig-ok-statuses st)
          (recur (rest ts))
          (str "dig: " st)))
      nil)))

(defn feet-y [c] (:y (cell (u/self-pos c))))

(defn ^:async climb!
  "moveTo the stair step pos. nil when it arrived or the body's feet rose, else the reason."
  [c pos]
  (let [before (feet-y c)
        status (.-status (await (go! c pos)))]
    (when-not (or (= "arrived" status) (> (feet-y c) before))
      (str "stair: moveTo " status))))

(def wall-search-radius
  "How far (blocks along x and along z) wall-spot looks for a wall to stair into."
  8)

(defn walled-side? [c pos] (boolean (some #(solid? c (shift pos %)) cardinals)))

(defn wall-spot
  "The nearest cell at the feet level that has a solid side at feet height (a stair step), reached by walking
  through standable cells (breadth-first, the goal's heading tried first) no more than wall-search-radius blocks
  away along x and z; nil when there is none. For a body in a hollow wider than a 1x1 pit, where no side of its
  own cell is solid."
  [c here dir]
  (let [headings (distinct (cons dir cardinals))
        near? (fn [p] (and (<= (js/Math.abs (- (:x p) (:x here))) wall-search-radius)
                           (<= (js/Math.abs (- (:z p) (:z here))) wall-search-radius)))]
    (loop [queue #queue [here] seen #{here}]
      (when-let [p (peek queue)]
        (if (and (not= p here) (walled-side? c p))
          p
          (let [nbrs (->> headings
                          (map #(shift p %))
                          (filter #(and (not (seen %)) (near? %) (reach/standable-cell? (:primitives c) %))))]
            (recur (into (pop queue) nbrs) (into seen nbrs))))))))

(declare dig-step!)

(defn ^:async approach-wall!
  "No side of the body's cell is solid: walk to wall-spot (moveTo range 0) and dig the stair from there. nil
  when that did something useful, else the reason."
  [c here dir]
  (if-let [spot (wall-spot c here dir)]
    (let [status (.-status (await (go! c spot)))]
      (if (= spot (cell (u/self-pos c)))
        (await (dig-step! c false))
        (str "wall: moveTo " status)))
    "stair: no solid step"))

(defn ^:async dig-step!
  "Dig toward the goal: a door through a one-block wall, else a stair step that
  lifts the body one block (then moves onto it). With no solid side to step on
  (the body stands inside a hollow wider than a pit) and approach? true, it
  first walks to the nearest wall (approach-wall!). Returns nil when it did
  something useful, else the reason it did not."
  [c approach?]
  (let [here (cell (u/self-pos c))
        dir (or (forward c here) (first cardinals))
        door (door-cells c here dir)
        options (concat (when door [{:cells door}]) (stair-options c here dir))
        {:keys [option trespass]} (access/choose c :dig options :cells)
        {:keys [cells front]} option
        stair (when front option)]
    (cond
      (and (nil? option) approach? (not (walled-side? c here))) (await (approach-wall! c here dir))
      (nil? option) "stair: no solid step"
      :else
      (if-let [unsafe (some #(unsafe-reason c %) cells)]
        unsafe
        (do
          (access/trespass! c "unstick" trespass)
          (when-let [pick (and (seq cells) (best-pickaxe c))]
            (await (ctx/act c :equip (clj->js {:item pick :dest "hand"}))))
          (let [bad (await (dig-cells! c cells))]
            (or bad
                (when stair (await (climb! c front))))))))))

(defn walled-at?
  "True when at least three of the four sides are solid at the height of pos."
  [c pos]
  (>= (count (filter #(solid? c (shift pos %)) cardinals)) 3))

(defn pit-depth
  "Levels upward from the feet, k = 0, 1, ..., with at least three of the four
  sides solid at feet+k, capped at max-pillar."
  [c here]
  (count (take-while #(walled-at? c (up here %)) (range max-pillar))))

(defn pillar-item
  "The carried pillar block with the largest count, or nil."
  [c]
  (let [have (->> (u/inventory (:primitives c))
                  (filter #(contains? (set pillar-items) (:name %)))
                  (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {}))]
    (when (seq have) (key (apply max-key val have)))))

(defn pillar-plan
  "{:item :count} when a pillar is possible here, else {:why reason}."
  [c here]
  (let [depth (pit-depth c here)
        item (pillar-item c)]
    (cond
      (not item) {:why "pillar: no block in inventory"}
      (zero? depth) {:why "pillar: not in a pit"}
      (not (open? c (up here 2))) {:why "pillar: no headroom"}
      :else {:item item :count depth})))

(defn pillar-possible? [c] (some? (:item (pillar-plan c (cell (u/self-pos c))))))

(defn pillar-skipped-reason [c] (:why (pillar-plan c (cell (u/self-pos c)))))

(def purpose
  "The scaffold ledger purpose of the blocks the pillar places."
  :unstick-pillar)

(defn ^:async pillar!
  "Pillar up with jumpPlace, depth-many blocks of the largest stack carried. Every cell is written to the scaffold
  ledger (engine.access.ledger) as an intent before the jump and settled from the cells after it, so the blocks it
  really placed stay open entries that jobs.access.cleanup takes back (the trigger :scaffold-left) once the body is out.
  Returns nil when it did something useful, else the reason it did not."
  [c]
  (let [here (cell (u/self-pos c))
        {:keys [item count why]} (pillar-plan c here)]
    (if-not item
      why
      (let [_ (access/trespass! c "unstick" (:trespass (access/choose c :place [(mapv #(up here %) (range count))] identity)))
            cells (mapv #(access/cell (up here %)) (range count))
            block-at (fn [[x y z]] (u/block-name (:primitives c) {:x x :y y :z z}))
            l (reduce #(ledger/intend %1 {:cell %2 :item item :before (block-at %2) :job (:id c) :purpose purpose})
                      (ledger/reconcile (ledger/open-entries (ctx/view c)) block-at)
                      cells)
            _ (ledger/remember! c l)
            r (await (ctx/act c :jumpPlace (clj->js {:item item :count count})))
            _ (ledger/remember! c (ledger/reconcile l block-at))
            status (.-status r)
            placed (.-placed r)]
        (cond
          (= "done" status) nil
          (= "partial" status) (str "pillar: partial " placed " of " count)
          :else (str "pillar: " (or (.-reason r) status)))))))

(defn ^:async hop!
  "A moveTo toward the stored goal, capped at hop-blocks; when that does not
  succeed, one uncapped retry with :timeoutS retry-timeout-s. Success is
  displacement alone: the body ended more than min-move blocks from where it
  stood before, whatever the status."
  [c min-move]
  (let [before (u/self-pos c)]
    (when-let [goal (:goal (ctx/mem c))]
      (letfn [(moved? [] (> (u/dist before (u/self-pos c)) min-move))]
        (await (ctx/act c :moveTo (clj->js {:pos goal :range 1 :maxDistance hop-blocks})))
        (or (moved?)
            (do (await (ctx/act c :moveTo (clj->js {:pos goal :range 1 :timeoutS retry-timeout-s})))
                (moved?)))))))

(def walk-timeout-s
  "The bound of the first round's walk (one steer of engine.path.near/walk-near!)."
  10)

(defn ^:async walk-out!
  "The first round's try before anything is placed or dug: a walk toward the stored goal with the engine's planner
  (engine.path.near/walk-near!, doors :shut: a shut door, gate or trapdoor is opened, passed and shut again), which
  the moveTo a job got stuck with cannot do. Skipped while the body is airborne (land! gave up). True when it moved
  the body more than min-move blocks."
  [c min-move]
  (let [before (u/self-pos c)
        goal (:goal (ctx/mem c))]
    (when (and goal (on-ground? c))
      (await (near/walk-near! c goal 1 {:doors :shut :timeout-s walk-timeout-s}))
      (> (u/dist before (u/self-pos c)) min-move))))

(defn summarize-reasons
  "Each distinct reason once, in first-seen order, with \" (xN)\" appended when it occurred N > 1 times."
  [reasons]
  (let [counts (frequencies reasons)]
    (mapv #(if (> (counts %) 1) (str % " (x" (counts %) ")") %)
          (distinct reasons))))

(defn give-up! [c attempts rounds]
  (let [pos (u/self-pos c)
        reasons (summarize-reasons (:reasons (ctx/mem c)))]
    (ctx/emit! c :unstick.failed :warn {:pos pos :attempts attempts :rounds rounds :reasons reasons
                                         :text (str "still stuck after " rounds " attempts: " (str/join "; " reasons))})
    (ctx/remember! c :stuck {:pos pos} stuck-policy)
    :done))

;; ------------------------------------------------------------------ job

(defn check [c]
  (or (pos? (:rounds (ctx/mem c) 0))
      (stuck/stuck? (ctx/view c) (:args c))))

(defn ^:async attempt!
  "One counted attempt (step back, pillar or dig), then the hop toward the goal: :done when the hop moved the body."
  [c attempt min-move]
  (let [here (cell (u/self-pos c))
        pillar? (pillar-possible? c)
        action (cond
                 (and (= 1 attempt) (zero? (pit-depth c here))) :step-back
                 pillar? :pillar
                 :else :dig)
        skipped (when (and (= :dig action) (not pillar?)) (pillar-skipped-reason c))
        why (await (case action
                     :step-back (step-back! c)
                     :pillar (pillar! c)
                     :dig (dig-step! c true)))]
    (ctx/update-mem! c update :reasons (fnil into []) (keep identity [skipped why])))
  (let [y (feet-y c)
        rose? (> y (:best-y (ctx/mem c)))]
    (ctx/update-mem! c #(-> % (assoc :best-y (max y (:best-y %))) (update :attempts (fnil + 0) (if rose? 0 1)))))
  (if (await (hop! c min-move))
    (do (ctx/update-mem! c dissoc :attempts :rounds :best-y :reasons)
        :done)
    :continue))

(defn ^:async round [c]
  (let [{:keys [min-move max-attempts]} (:args c)
        {:keys [attempts rounds] :or {attempts 0 rounds 0}} (ctx/mem c)]
    (if (or (>= attempts max-attempts) (>= rounds (+ max-attempts max-pillar)))
      (give-up! c attempts rounds)
      (let [attempt (inc rounds)]
        (await (land! c))
        (ctx/update-mem! c merge
                         (when (= 1 attempt) (merge (bearings c (cell (u/self-pos c))) {:best-y (feet-y c)}))
                         {:rounds attempt})
        (if (and (= 1 attempt) (await (walk-out! c min-move)))
          (do (ctx/update-mem! c dissoc :attempts :rounds :best-y :reasons)
              :done)
          (await (attempt! c attempt min-move)))))))
