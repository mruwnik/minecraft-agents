(ns jobs.lib.sense
  "Look before concluding. A job's sync decision reads cells and areas through the seen-* helpers (jobs.lib.util,
  jobs.lib.look); a cell the body never looked at reads as unknown, and a decision that ends the job (none, no support,
  not standable) must not rest on it. decide! records the unknown cells the decision read, looks at them (nearest to the
  eye first, sight passes after each look) and decides again, bounded: as many looks a call as what a turn saves pays at
  the chance a look turns the decision (cost/turn-gap, cost/cell-looks), each cell at most once per run and body cell,
  and one look round per run and body cell for an area or entity query that found nothing, when its first look pays. Only looks: never moves, digs or places, and sees nothing a player could not."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.perception :as perception]
            [jobs.lib.clock :as clock]
            [jobs.lib.memory-age :as memory-age]
            [engine.settings :as settings]
            [jobs.lib.cost.look :as cost]
            [jobs.lib.look :as look]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::passes {:default 3 :spec (a/int-in 1 nil)
             :doc "Most sight passes a look takes while its cell stays unknown: a floor seen edge-on shows in some passes only."}
   ::max-looked {:default 512 :spec (a/int-in 1 nil)
                 :doc "Most cells a job remembers looking at from one cell; past it decide! looks no more there."}})

(defn cell-looks
  "How many of n looks at unknown cells c's call pays for (cost/cell-looks over cost/turn-gap of its worth, the decision
  unpriced), after looked fruitless ones each followed by redo-s of deciding again."
  ([c n] (cell-looks c n 0 0))
  ([c n looked redo-s] (cost/cell-looks (cost/turn-gap (ctx/worth c) nil) n looked redo-s)))

(defn max-looked [] (settings/get settings ::max-looked))
(defn passes [] (settings/get settings ::passes))

(defn negative?
  "The default retry?: the decision found nothing (nil or false)."
  [v]
  (or (nil? v) (false? v)))

(defn decide
  "[v reads]: (decide-fn) with what it read unknown recorded (u/*reads*). Throws when decide-fn returns a Promise: the
  record does not cross an await."
  [decide-fn]
  (let [r (volatile! {})
        v (binding [u/*reads* r] (decide-fn))]
    (when (instance? js/Promise v)
      (throw (js/Error. "sense/decide!: the decision returned a Promise; it must be sync")))
    [v @r]))

(defn want-of
  "The look/see! want for a look round that serves a decision which read these (decide's reads): only hazards for an
  entity query, the kinds of its seen-blocks queries, nil (a full pass) when one of them asked for any block."
  [{:keys [area]}]
  (cond
    (empty? area) look/hazards-only
    (some #(not (or (:names %) (:match %))) area) nil
    :else (let [ms (keep :match area)]
            {:names (into #{} (mapcat :names) area)
             :match (when (seq ms) (fn [n] (some #(% n) ms)))
             :radius (apply max (map :radius area))})))

(defn radius
  "How far the body of primitives p sees, in blocks."
  [p]
  (or (some-> (aget p "perception") :opts :radius) (settings/get perception/settings :engine.perception/radius)))

(defn targets
  "The cells of cells ([x y z]) not in looked and within sight radius of the feet here, nearest to the eye first."
  [p here cells looked]
  (->> cells
       (remove looked)
       (filter #(<= (u/eye-dist here %) (radius p)))
       (u/nearest-first here identity)))

(defn unknown? [p [x y z]] (true? (some-> (u/sensed p {:x x :y y :z z}) .-unknown)))

(defn ^:async look-at!
  "Turn to cell's centre (or the point aim [x y z]) and take a whole sight pass at search effort, again while the cell
  is still unknown, at most (passes)."
  [c cell & [aim]]
  (let [[ax ay az] (or aim (mapv + cell [0.5 0.5 0.5]))]
    (await (ctx/act c :look (clj->js {:pos {:x ax :y ay :z az}})))
    (loop [n 0]
      (when (< n (passes))
        (look/see! c (look/search-effort) :look-at)
        (when (unknown? (:primitives c) cell) (recur (inc n)))))))

(defn ^:async look-at-unknown!
  "look-at! each of cells that is still unknown when its turn comes (a look's sight pass shows its neighbours too), at
  most n looks: [looks cells-passed stopped?]. With stop? (a fn of a cell just looked at or already known), the first
  cell it holds for ends the batch: stopped? is true."
  ([c cells n] (look-at-unknown! c cells n nil))
  ([c cells n stop?]
   (loop [[cell & more] cells looks 0 passed []]
     (cond
       (or (nil? cell) (>= looks n)) [looks passed false]
       (unknown? (:primitives c) cell)
       (do (await (look-at! c cell))
           (if (and stop? (stop? cell))
             [(inc looks) (conj passed cell) true]
             (recur more (inc looks) (conj passed cell))))
       :else (if (and stop? (stop? cell))
               [looks (conj passed cell) true]
               (recur more looks (conj passed cell)))))))

(defn round-mem
  "c's look-round memory {:at :cells :looks} for the body's cell, {} from another."
  [c]
  (let [m (::round-looked (ctx/mem c))]
    (if (= (:at m) (look/cell-of (u/self-pos c))) m {})))

(defn round-looked
  "The cells [x y z] a look round of c's frame asked for from the body's cell, looked at or not: a :look status asks
  for them once, then the wall they hide counts as open (Q345)."
  [c]
  (:cells (round-mem c) #{}))

(defn round-plan
  "{:pay :unpaid :walls} for a look round at cells from here, of those in sight not round-looked, in the order of cells:
  :pay the room walls among them (reach-room/unseen-sides, :walls) when their round pays the call's worth
  (cost/round-looks), the other known ones (no look needed), and the nearest other unknown ones as many as cost/cell-looks pays after the looks
  this spot's rounds took, both at cost/turn-gap of the call's worth; :unpaid the rest."
  [c cells]
  (let [p (:primitives c)
        asked (targets p (u/self-pos c) cells (round-looked c))
        walls (if (seq asked) (set (map vec (reach-room/unseen-sides p (sh-sense/feet p)))) #{})
        {wall :wall other :other} (group-by #(cond (contains? walls %) :wall (unknown? p %) :other :else :known) asked)
        gap (cost/turn-gap (ctx/worth c) nil)
        unpaid (set (concat (when-not (= (count wall) (cost/round-looks gap (count wall))) wall)
                            (drop (cost/cell-looks gap (count other) (:looks (round-mem c) 0) 0) other)))
        in-sight (set asked)]
    {:pay (filterv #(and (in-sight %) (not (unpaid %))) cells) :unpaid (filterv unpaid cells) :walls walls}))

(defn ^:async look-round!
  "The :look/around hook: look at the cells of cells (a :look status's :look-at) round-plan pays for, then look-around! for
  those cells (a full pass when cells is empty: the check named none). Every one of cells counts as round-looked: the
  engine gives a :look one round (a second counts as :later), so what it left unseen, unpaid ones too, counts as unseen
  (a wall open). Each unknown paid cell the batch reached counts as a fruitless look. A room wall seen open ends the
  batch and the round: the room is open whatever the other walls are."
  [c cells]
  (let [p (:primitives c)
        {:keys [pay walls]} (round-plan c cells)
        unknown (set (filter #(unknown? p %) pay))
        open? (fn [cell] (and (contains? walls cell) (pos? (:open (reach-room/open-sides p [cell])))))
        [_ passed stopped] (await (look-at-unknown! c (u/nearest-first (u/self-pos c) identity pay) (count pay) open?))
        n (count (filter unknown passed))]
    (when (and (not stopped) (or (empty? cells) (seq unknown)))
      (await (look/look-around! c (look/glance-effort) (when (seq cells) {:cells pay}))))
    (ctx/update-mem! c (fn [m] (let [here (look/cell-of (u/self-pos c))
                                     kept (if (= here (:at (::round-looked m))) (::round-looked m) {})]
                                 (assoc m ::round-looked {:at here
                                                          :cells (into (:cells kept #{}) cells)
                                                          :looks (+ (:looks kept 0) n)}))))))

(defn known-memory
  "memory-age/epoch of the body's memory: the same while all it knows of the world is (no cell written, no remembered
  cell's change chance moved a level); nil without a perception."
  [p]
  (some-> ^js (:st (aget p "perception")) memory-age/epoch))

(defn step-ms
  "[total max] ms perception's sight steps have taken so far, nil without a perception."
  [p]
  (when-let [^js st (some-> (aget p "perception") :st)]
    [(.-stepMs st) (.-stepMsMax st)]))

(defn ^:async decide!
  "(decide-fn), a sync decision over what the body has seen, decided again after a look at each unknown cell it read,
  until it is positive (not (retry? v)) or no look pays; the last value. A look pays while what a turn saves (cost/turn-gap
  of the call's worth and (gap v)) x the chance it turns the decision (cost/cell-looks, falling with each fruitless look)
  is over its seconds and its share of deciding again: the looks between two decisions cost as much on the clock as one decision's CPU (cost/redo-every).
  The cells looked at before from this body cell count as fruitless looks; at most (max-looked) of them. A positive that read a guessed cell
  (u/block-name-or) is checked after a look at it too with :verify-guesses true. An area (look/seen-blocks) or entity
  (look/seen-entities) query with no cell left to look at gets one look round (look/survey-until!, per body cell) when
  its first look pays, stopping at the first look that makes the decision positive. Unloaded cells are never looked at. k names the
  decision in the :sense.looked debug event, emitted when the call looked. opts: :retry? (default negative?),
  :verify-guesses, :batch (look at every unknown cell it read that pays, nearest first, before deciding again), :gap (a
  fn of the value: the seconds a turn saves, its runner-up's price less the chosen plan's; nil unpriced). Call it
  from a round only: it acts."
  ([c k decide-fn] (decide! c k decide-fn {}))
  ([c k decide-fn {:keys [retry? verify-guesses batch gap] :or {retry? negative?}}]
   (let [p (:primitives c)
         here (u/self-pos c)
         cell (look/cell-of here)
         mem (::looked (ctx/mem c))
         looked0 (if (= cell (:at mem)) (:cells mem #{}) #{})
         worth (ctx/worth c)
         t0 (step-ms p)
         start (clock/now)
         [v looked surveyed looks]
         (loop [looks 0 looked looked0 surveyed false]
           (let [t (clock/now)
                 [v reads] (decide decide-fn)
                 redo-s (/ (- (clock/now) t) 1000)
                 guessed (when (and verify-guesses (not (retry? v))) (seq (remove looked (:guessed reads))))
                 saves (cost/turn-gap worth (when gap (gap v)))
                 fruitless (+ (count looked0) looks)]
             (if (or (>= (count looked) (max-looked)) (and (not (retry? v)) (not guessed)))
               [v looked surveyed looks]
               (let [found (vec (take (- (max-looked) (count looked)) (targets p here (if guessed guessed (:cells reads)) looked)))
                     k (if (and batch (not guessed)) (count found) (min (count found) (cost/redo-every redo-s)))
                     n (min k (cost/cell-looks saves (count found) fruitless (/ redo-s (max k 1))))
                     ask (when (and (retry? v) (pos? (cost/cell-looks saves 1 fruitless redo-s)))
                           (or (seq (:area reads)) (seq (:entities reads))))]
                 (cond
                   (and guessed (pos? n))
                   (do (await (look-at! c (first found)))
                       (recur (inc looks) (conj looked (first found)) surveyed))

                   (pos? n)
                   (let [[taken passed] (await (look-at-unknown! c found n))]
                     (recur (+ looks (max taken 1)) (into looked passed) surveyed))

                   (and ask (not surveyed) (not (look/surveyed? c :cell (want-of reads))))
                   (let [want (want-of reads)]
                     (await (look/survey-until! c :cell #(let [[v] (decide decide-fn)] (when-not (retry? v) [v])) (look/glance-effort) want))
                     (recur looks looked {:want want}))

                   :else [v looked surveyed looks])))))
         new (remove looked0 looked)]
     (when (or (seq new) surveyed)
       (let [[total-ms max-ms] (step-ms p)]
         (ctx/update-mem! c assoc ::looked {:at cell :cells looked})
         (when surveyed (look/note-surveyed! c cell (:want surveyed)))
         (ctx/emit! c :sense.looked :debug
                    (cond-> {:decision k :looks (count new) :cells (vec new) :surveyed? (boolean surveyed) :turned? (and (pos? looks) (not (retry? v))) :wall-ms (Math/round (- (clock/now) start))}
                      t0 (assoc :step-ms (- total-ms (first t0)) :step-ms-max max-ms)))))
     v)))
