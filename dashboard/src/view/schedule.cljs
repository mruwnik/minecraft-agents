(ns view.schedule
  "When the hub (view.hub) renders which of its targets, and the replay of the shared /poses stream. Pure: time is passed in.

   Runs every animation frame, so it is written in the tuned style (see view.interp): targets are the hub's JS objects
   (id, visible, raf, dueAt, fps), read and written in place; JS arrays, no persistent data, no seqs.")

(set! *warn-on-infer* true)

(defn by-due ^number [^js a ^js b] (- (.-dueAt a) (.-dueAt b)))

(defn due-targets
  "The targets to render now: the visible ones that are raf (every animation frame; first, all of them) or whose dueAt has
   passed (the longest-overdue first, ties keep their order, at most `max` of them)."
  ([^js targets now] (due-targets targets now js/Infinity))
  ([^js targets now max]
   (let [out #js []
         timed #js []]
     (dotimes [i (alength targets)]
       (let [^js t (aget targets i)]
         (when (.-visible t)
           (cond
             (.-raf t) (.push out t)
             (<= (.-dueAt t) now) (.push timed t)))))
     (.sort timed by-due) ; Array.prototype.sort is stable
     (dotimes [i (min max (alength timed))] (.push out (aget timed i)))
     out)))

(defn due-scenes
  "The ids of due-targets."
  ([targets now] (due-scenes targets now js/Infinity))
  ([targets now max] (.map (due-targets targets now max) (fn [^js t] (.-id t)))))

(defn next-due-at
  "When a target rendered at `now` is due again: one period after it was due, so the rate stays exactly `fps` however the
   frames fall; a target more than a period behind is not caught up with, it restarts from now. \"raf\": the next frame."
  ^number [^number due-at ^number now fps]
  (if (= fps "raf")
    now
    (let [period (/ 1000 fps)
          next (+ due-at period)]
      (if (> next now) next (+ now period)))))

(defn plan-frame
  "One animation frame's renders. Every due raf target renders. The others render while the frame's total cost (raf ones
   included) is under budget-ms, so cards give way to a big view; but a card more than half a period late renders anyway, one
   per frame, so a slow big view slows the cards without starving them. (run target) renders one target and returns its cost
   in ms. Returns the ids rendered."
  [^js targets now budget-ms run]
  (let [due (due-targets targets now)
        rendered #js []]
    (loop [i 0 spent 0 late 0]
      (if (< i (alength due))
        (let [^js t (aget due i)
              over? (and (not (.-raf t)) (>= spent budget-ms))
              overdue? (> (- now (.-dueAt t)) (/ 500 (.-fps t)))]
          (if (and over? (or (not overdue?) (pos? late)))
            (recur (inc i) spent late)
            (let [cost (run t)]
              (.push rendered (.-id t))
              (recur (inc i) (+ spent cost) (if over? (inc late) late)))))
        rendered))))

(defn percentile
  "The p-quantile of a JS array of numbers (nil when empty)."
  [^js values p]
  (when (pos? (alength values))
    (let [sorted (.sort (.slice values) (fn [a b] (- a b)))]
      (aget sorted (min (dec (alength sorted)) (js/Math.floor (* p (alength sorted))))))))

(defn keep!
  "Appends value to the JS array xs, keeping the last `max` values."
  [^js xs value max]
  (.push xs value)
  (when (> (alength xs) max) (.shift xs)))

(defn count-since
  "How many of the JS array of times are within window-ms before now."
  [^js times now window-ms]
  (loop [i 0 n 0]
    (if (< i (alength times))
      (recur (inc i) (if (<= (- now (aget times i)) window-ms) (inc n) n))
      n)))

;; The last pose and hud event of each agent seen on the shared stream. The stream reopens only when the set of agents changes,
;; so a scene added later for an agent already streamed is fed these at once (replay) instead of waiting for a pose that may be
;; far off. Data is the stream's JS object; its agent is data.agent.
(deftype EventCache [^js last] ; "event|agent" -> data
  Object
  (record [_ event ^js data]
    (when (or (= event "pose") (= event "hud"))
      (.set last (str event "|" (.-agent data)) data)))
  (replay [_ agent feed]
    (doseq [event ["pose" "hud"]]
      (let [data (.get last (str event "|" agent))]
        (when (some? data) (feed event data)))))
  (keepOnly [_ agents]
    (let [wanted (set agents)]
      (doseq [k (vec (es6-iterator-seq (.keys last)))]
        (when-not (contains? wanted (.-agent ^js (.get last k))) (.delete last k))))))

(defn event-cache [] (EventCache. (js/Map.)))

(defn stream-key
  "The /poses stream that serves scenes of these agents (any order, repeats) at these radii: #js {key, agents (sorted,
   distinct), radius (the largest, at least 0)}. The hub reopens the stream when the key changes."
  [agents radii]
  (let [agents (vec (sort (distinct agents)))
        radius (reduce max 0 radii)]
    #js {:key (str (.join (clj->js agents) ",") "|" radius) :agents (clj->js agents) :radius radius}))
