(ns jobs.maintenance.unstick
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.triggers.stuck :as stuck]))

(def doc
  "Get a body out of being stuck: a job keeps calling moveTo and the body gets
  nowhere. Each round is one attempt, escalating with the attempt count kept
  in job memory: 1 steps back one block the way it came; 2 pillars out when
  the body is in a pit (at least three of the four sides solid at feet+k for
  k = 0, 1, ... gives the depth d, capped at 8) and a placeable block is
  carried: jumpPlace with the largest stack and count d; otherwise 2 digs the
  block in front at feet and head height and the one above the head in a
  tight gap; 3 digs if 2 pillared, else pillars; later attempts repeat the
  pillar if possible, else dig. Each attempt that did nothing useful records
  why in job memory (:reasons). After every attempt it
  tries a moveTo toward the stored goal itself, capped at hop-blocks (3) along
  the way (range 1, :maxDistance 3; within 3 blocks it simply walks there). If
  that did not move the body more than :min-move blocks from where it stood
  (status is ignored), it retries once uncapped (range 1, :timeoutS 6). If
  either moved it that far the spell is over (:done); otherwise :continue. After :max-attempts attempts it emits the warn
  event unstick.failed with the position, :reasons and a :text naming them, writes a :stuck memory entry (cap 10,
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
   :max-attempts {:doc "attempts before giving up with unstick.failed" :default 4}})

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

(def gravity-or-liquid? (some-fn liquid-names falling-names))

(defn ^:async dig!
  [c pos]
  (await (ctx/act c :dig (clj->js {:pos pos}))))

(defn best-pickaxe [c]
  (let [have (set (map :name (u/inventory (:primitives c))))]
    (first (filter have pickaxes))))

(defn ^:async dig-ahead!
  "Dig the solid blocks in front at feet and head height, and the
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
    (if (empty? targets)
      "dig: nothing to dig"
      (loop [ts targets bad nil]
        (if-let [t (first ts)]
          (let [r (await (dig! c t))
                st (.-status r)]
            (recur (rest ts) (or bad (when-not (contains? dig-ok-statuses st) (str "dig: " st)))))
          bad)))))

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
      :else {:item item :count depth})))

(defn pillar-possible? [c] (some? (:item (pillar-plan c (cell (u/self-pos c))))))

(defn pillar-skipped-reason [c] (:why (pillar-plan c (cell (u/self-pos c)))))

(defn ^:async pillar!
  "Pillar up with jumpPlace, depth-many blocks of the largest stack carried.
  Returns nil when it did something useful, else the reason it did not."
  [c]
  (let [{:keys [item count why]} (pillar-plan c (cell (u/self-pos c)))]
    (if-not item
      why
      (let [r (await (ctx/act c :jumpPlace (clj->js {:item item :count count})))
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

(defn give-up! [c attempts]
  (let [pos (u/self-pos c)
        reasons (vec (:reasons (ctx/mem c)))]
    (ctx/emit! c :unstick.failed :warn {:pos pos :attempts attempts :reasons reasons
                                         :text (str "still stuck after " attempts " attempts: " (str/join "; " reasons))})
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
        (let [pillar? (pillar-possible? c)
              pillared? (:pillared (ctx/mem c))
              action (case attempt
                       1 :step-back
                       2 (if pillar? :pillar :dig)
                       (if (and (= 3 attempt) pillared?) :dig (if pillar? :pillar :dig)))
              skipped (when (and (= :dig action) (not pillar?)) (pillar-skipped-reason c))
              why (await (case action
                           :step-back (step-back! c)
                           :pillar (pillar! c)
                           :dig (dig-ahead! c)))]
          (ctx/update-mem! c #(cond-> (update % :reasons (fnil into []) (keep identity [skipped why]))
                                (= :pillar action) (assoc :pillared true))))
        (if (await (hop! c min-move))
          (do (ctx/update-mem! c dissoc :attempts :reasons :pillared)
              :done)
          :continue)))))
