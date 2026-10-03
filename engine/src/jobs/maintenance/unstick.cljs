(ns jobs.maintenance.unstick
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.triggers.stuck :as stuck]))

(def doc
  "Get a body out of being stuck: a job keeps calling moveTo and the body gets
  nowhere. Each round is one attempt, escalating with the attempt count kept
  in job memory: 1 steps back one block the way it came, 2 digs the block in
  front at feet and head height and the one above the head in a tight gap,
  3 places a block under the feet and steps up when walled in like a pit
  and a pillar block is carried (attempts past 3 repeat 3). After every attempt it
  tries a short moveTo toward where the body was going. If that ends arrived
  or partial and moves the body more than :min-move blocks the spell is over
  (:done); otherwise :continue. After :max-attempts attempts it emits the warn
  event unstick.failed with the position, writes a :stuck memory entry (cap 10,
  ttl 1 hour) and ends so the list resumes.

  How it pairs with the stuck trigger (engine.triggers.stuck): both read only
  the :moved entries act writes after every moveTo. The trigger fires when the
  last :n are all bad inside :window-ms; this job's check is that same
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
   :window-ms {:doc "the bad moves must all fall within this many ms" :default (:window-ms stuck/defaults)}
   :quiet-ms {:doc "after giving up, the trigger stays quiet this many ms" :default (:quiet-ms stuck/defaults)}
   :max-attempts {:doc "attempts before giving up with unstick.failed" :default 4}})

(def stuck-policy {:cap 10 :ttl (* 60 60 1000)})

(def hop-blocks 3)

(def open-names #{"air" "cave_air" "void_air"})

(def liquid-names #{"water" "lava"})

(def falling-names #{"sand" "red_sand" "gravel"})

(def pickaxes ["netherite_pickaxe" "diamond_pickaxe" "iron_pickaxe" "stone_pickaxe" "golden_pickaxe" "wooden_pickaxe"])

(def pillar-items ["cobblestone" "dirt" "stone" "cobbled_deepslate" "netherrack" "andesite" "diorite" "granite"])

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

(defn hop-point
  "A point at most hop-blocks from here toward the stored goal, or nil."
  [c here]
  (when-let [target (:goal (ctx/mem c))]
    (let [d (u/dist here target)]
      (if (<= d hop-blocks)
        (cell target)
        (let [f (/ hop-blocks d)]
          {:x (js/Math.round (+ (:x here) (* f (- (:x target) (:x here)))))
           :y (js/Math.round (+ (:y here) (* f (- (:y target) (:y here)))))
           :z (js/Math.round (+ (:z here) (* f (- (:z target) (:z here)))))})))))

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
    (when spot (await (go! c spot)))))

(def gravity-or-liquid? (some-fn liquid-names falling-names))

(defn ^:async dig!
  [c pos]
  (await (ctx/act c :dig (clj->js {:pos pos}))))

(defn best-pickaxe [c]
  (let [have (set (map :name (u/inventory (:primitives c))))]
    (first (filter have pickaxes))))

(defn ^:async dig-ahead!
  "Attempt 2: dig the solid blocks in front at feet and head height, and the
  block above the head when it is solid and the head cell is open (a gap one
  block too low to jump out of). Sand and gravel overhead are left alone. The
  best pickaxe carried is equipped first, as a pillar block in hand digs far
  too slowly."
  [c]
  (let [here (cell (u/self-pos c))
        front (some->> (forward c here) (shift here))
        above (up here 2)
        targets (concat (when front (filter #(solid? c %) [front (up front 1)]))
                        (when (and (solid? c above) (open? c (up here 1))
                                   (not (gravity-or-liquid? (block-name c above))))
                          [above]))]
    (when-let [pick (and (seq targets) (best-pickaxe c))]
      (await (ctx/act c :equip (clj->js {:item pick :dest "hand"}))))
    (loop [ts targets]
      (when-let [t (first ts)]
        (await (dig! c t))
        (recur (rest ts))))))

(defn walled?
  "True when at least three of the four sides are solid at feet and head height."
  [c here]
  (>= (count (filter (fn [h] (let [side (shift here h)]
                               (and (solid? c side) (solid? c (up side 1)))))
                     cardinals))
      3))

(defn pillar-item [c]
  (let [have (set (map :name (u/inventory (:primitives c))))]
    (first (filter have pillar-items))))

(defn ^:async pillar!
  "Attempt 3: walled in and carrying a pillar block, place it under the feet
  and step up onto it."
  [c]
  (let [here (cell (u/self-pos c))
        item (pillar-item c)]
    (when (and item (walled? c here))
      (await (ctx/act c :place (clj->js {:pos here :item item})))
      (await (go! c (up here 1))))))

(defn ^:async hop!
  "A short moveTo toward the target. True when it ended arrived or partial and
  the body moved more than min-move blocks."
  [c min-move]
  (let [before (u/self-pos c)]
    (when-let [spot (hop-point c before)]
      (let [r (await (go! c spot))]
        (and (contains? stuck/ok-statuses (.-status r))
             (> (u/dist before (u/self-pos c)) min-move))))))

(defn give-up! [c attempts]
  (let [pos (u/self-pos c)]
    (ctx/emit! c :unstick.failed :warn {:pos pos :attempts attempts
                                         :text (str "still stuck after " attempts " attempts")})
    (ctx/remember! c :stuck {:pos pos} stuck-policy)
    :done))

;; ------------------------------------------------------------------ job

(defn check [c]
  (or (pos? (:attempts (ctx/mem c) 0))
      (stuck/stuck? (ctx/view c) (:args c))))

(defn ^:async round [c]
  (let [{:keys [min-move max-attempts]} (:args c)
        tried (:attempts (ctx/mem c) 0)]
    (if (>= tried max-attempts)
      (give-up! c tried)
      (let [attempt (inc tried)]
        (ctx/update-mem! c merge (when (= 1 attempt) (bearings c (cell (u/self-pos c)))) {:attempts attempt})
        (case (min attempt 3)
          1 (await (step-back! c))
          2 (await (dig-ahead! c))
          3 (await (pillar! c)))
        (if (await (hop! c min-move))
          (do (ctx/update-mem! c dissoc :attempts)
              :done)
          :continue)))))
