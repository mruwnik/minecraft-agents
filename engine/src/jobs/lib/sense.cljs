(ns jobs.lib.sense
  "Look before concluding. A job's sync decision reads cells and areas through the seen-* helpers (jobs.lib.util,
  jobs.lib.look); a cell the body never looked at reads as unknown, and a decision that ends the job (none, no support,
  not standable) must not rest on it. decide! records the unknown cells the decision read, looks at them (nearest to the
  eye first, sight passes after each look) and decides again, bounded: at most ::max-looks looks a call, each cell at
  most once per run and body cell, and one look round per run and body cell for an area or entity query that found
  nothing. Only looks: never moves, digs or places, and sees nothing a player could not."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.perception :as perception]
            [engine.settings :as settings]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::max-looks {:default 8 :spec (a/int-in 0 nil) :doc "Most looks at unknown cells one decide! takes."}
   ::passes {:default 3 :spec (a/int-in 1 nil)
             :doc "Most sight passes a look takes while its cell stays unknown: a floor seen edge-on shows in some passes only."}
   ::max-looked {:default 512 :spec (a/int-in 1 nil)
                 :doc "Most cells a job remembers looking at from one cell; past it decide! looks no more there."}})

(defn max-looks [] (settings/get settings ::max-looks))
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
        (look/see! c (look/search-effort))
        (when (unknown? (:primitives c) cell) (recur (inc n)))))))

(defn ^:async look-at-unknown!
  "look-at! each of cells that is still unknown when its turn comes (a look's sight pass shows its neighbours too), at
  most n looks: [looks cells-passed]."
  [c cells n]
  (loop [[cell & more] cells looks 0 passed []]
    (cond
      (or (nil? cell) (>= looks n)) [looks passed]
      (unknown? (:primitives c) cell) (do (await (look-at! c cell)) (recur more (inc looks) (conj passed cell)))
      :else (recur more looks (conj passed cell)))))

(defn ^:async look-round!
  "The :look/around hook: look at each unknown one of cells (a :look status's :look-at), at most ::max-looks looks,
  then look-around!."
  [c cells]
  (await (look-at-unknown! c (targets (:primitives c) (u/self-pos c) cells #{}) (max-looks)))
  (await (look/look-around! c))
  (ctx/update-mem! c (fn [m] (let [here (look/cell-of (u/self-pos c))
                                   kept (if (= here (:at (::round-looked m))) (:cells (::round-looked m)) #{})]
                               (assoc m ::round-looked {:at here :cells (into kept cells)})))))

(defn round-looked
  "The cells [x y z] a look round of c's frame already looked at from the body's cell: a :look status asks for them once,
  then the wall they hide counts as open (Q345)."
  [c]
  (let [m (::round-looked (ctx/mem c))]
    (if (= (:at m) (look/cell-of (u/self-pos c))) (:cells m) #{})))

(defn step-ms
  "[total max] ms perception's sight steps have taken so far, nil without a perception."
  [p]
  (when-let [^js st (some-> (aget p "perception") :st)]
    [(.-stepMs st) (.-stepMsMax st)]))

(defn ^:async decide!
  "(decide-fn), a sync decision over what the body has seen, decided again after a look at each unknown cell it read,
  until it is positive (not (retry? v)) or no look is left; the last value. A positive that read a guessed cell
  (u/block-name-or) is checked after a look at it too with :verify-guesses true. An area (look/seen-blocks) or entity
  (look/seen-entities) query with no cell left to look at gets one look round (look/survey-until!, per body cell),
  stopping at the first look that makes the decision positive. Unloaded cells are never looked at. k names the
  decision in the :sense.looked debug event, emitted when the call looked. opts: :retry? (default negative?),
  :verify-guesses, :batch (for a decision that is costly to make: look at the unknown cells it read, nearest first, while
  still unknown and looks are left, before deciding again). Call it from a round only: it acts."
  ([c k decide-fn] (decide! c k decide-fn {}))
  ([c k decide-fn {:keys [retry? verify-guesses batch] :or {retry? negative?}}]
   (let [p (:primitives c)
         here (u/self-pos c)
         cell (look/cell-of here)
         mem (::looked (ctx/mem c))
         looked0 (if (= cell (:at mem)) (:cells mem #{}) #{})
         full? (>= (count looked0) (max-looked))
         t0 (step-ms p)
         [v looked surveyed]
         (loop [looks 0 looked looked0 surveyed false]
           (let [[v reads] (decide decide-fn)
                 guessed (when (and verify-guesses (not (retry? v))) (seq (remove looked (:guessed reads))))]
             (if (or full? (and (not (retry? v)) (not guessed)))
               [v looked surveyed]
               (let [found (targets p here (if guessed guessed (:cells reads)) looked)
                     target (first found)
                     ask (when (retry? v) (or (seq (:area reads)) (seq (:entities reads))))]
                 (cond
                   (and target batch (< looks (max-looks)))
                   (let [[n passed] (await (look-at-unknown! c found (- (max-looks) looks)))]
                     (recur (+ looks (max n 1)) (into looked passed) surveyed))

                   (and target (< looks (max-looks)))
                   (do (await (look-at! c target))
                       (recur (inc looks) (conj looked target) surveyed))

                   (and ask (not surveyed) (not (look/surveyed? c :cell)))
                   (do (await (look/survey-until! c :cell #(let [[v] (decide decide-fn)] (when-not (retry? v) [v]))))
                       (recur looks looked true))

                   :else [v looked surveyed])))))
         new (remove looked0 looked)]
     (when (or (seq new) surveyed)
       (let [[total-ms max-ms] (step-ms p)]
         (ctx/update-mem! c #(cond-> (assoc % ::looked {:at cell :cells looked})
                               surveyed (assoc :looked cell)))
         (ctx/emit! c :sense.looked :debug
                    (cond-> {:decision k :looks (count new) :cells (vec new) :surveyed? (boolean surveyed)}
                      t0 (assoc :step-ms (- total-ms (first t0)) :step-ms-max max-ms)))))
     v)))
