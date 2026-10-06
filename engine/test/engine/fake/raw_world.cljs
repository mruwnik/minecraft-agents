(ns engine.fake.raw-world
  "The rawWorld reader (the shape engine/js/raw-world.mjs gives the real body) over a fake's world atom, so
  engine.perception runs over the fake exactly as over the body. Block state ids are the default states of the fake's
  block names; a cell in the spec's :unloaded reads -1. Light comes from the world's :light {[x y z] [sky block]}, else
  :light-default (open daylight [15 0] when unset). The eye is the centre of the body's cell at eye height, looking along the
  world's :yaw/:pitch (Minecraft degrees, as drive sets them), in mineflayer radians. Block changes come from a watch
  on :blocks. The sight table and state names are raw-world.mjs's own, over the fixture registry. Test-only."
  (:require [engine.fake.node :as node]
            [engine.path.fixture :as fx]))

(def raw-mod (delay (node/require-here "./js/raw-world.mjs")))
(def sight (delay (.sightTable ^js @raw-mod fx/registry)))
(def info (delay (.stateInfo ^js @raw-mod fx/registry)))
(def state-of (memoize (fn [block] (fx/default-state block))))
(def min-y -64)
(def max-y 320)
(def eye-height 1.62)

(defn state-at-slow [w x y z]
  (cond
    (or (< y min-y) (>= y max-y)) -1
    (contains? (:unloaded w) [x y z]) -1
    :else (state-of (get (:blocks w) [x y z] "air"))))

(defn light-at-slow [w x y z]
  (let [[sky block] (or (get (:light w) [x y z]) (:light-default w) [15 0])]
    (bit-or (bit-shift-left sky 4) block)))

(def coord-limit 1048576)

(defn packed-key
  "x, y, z as one number (21 + 9 + 21 bits), or nil outside that range: numbers hash far faster than vector keys."
  [x y z]
  (when (and (< -1 (+ x coord-limit) (* 2 coord-limit)) (< -1 (+ z coord-limit) (* 2 coord-limit))
             (<= min-y y max-y))
    (+ (* (+ x coord-limit) 1073741824) (* (+ z coord-limit) 512) (- y min-y))))

(defn lookup-cache
  "A lazy per-key cache for fn f over the world values (selected by sel); reset when any selected value changes."
  [sel slow]
  (let [seen (volatile! nil) cache (volatile! (js/Map.))]
    (fn [w x y z]
      (let [vals (sel w)]
        (when-not (and (identical? (nth @seen 0 nil) (nth vals 0)) (identical? (nth @seen 1 nil) (nth vals 1)))
          (vreset! seen vals)
          (vreset! cache (js/Map.)))
        (if-let [k (packed-key x y z)]
          (let [^js m @cache v (.get m k)]
            (if (undefined? v)
              (let [v (slow w x y z)] (.set m k v) v)
              v))
          (slow w x y z))))))

(defn state-at-fn []
  (lookup-cache (fn [w] [(:blocks w) (:unloaded w)]) state-at-slow))

(defn light-at-fn []
  (lookup-cache (fn [w] [(:light w) (:light-default w)]) light-at-slow))

(defn eye [w]
  (when-not (:offline w)
    (let [[x y z] (get-in w [:self :pos])
          rad (/ js/Math.PI 180)]
      #js {:x (+ (js/Math.floor x) 0.5) :y (+ y eye-height) :z (+ (js/Math.floor z) 0.5)
           :yaw (- js/Math.PI (* (or (:yaw w) 0) rad)) :pitch (- (* (or (:pitch w) 0) rad))
           :dimension (get-in w [:self :dimension] "overworld")})))

(defn changed-cells [old new]
  (when-not (identical? old new)
    (filter #(not= (get old %) (get new %)) (distinct (concat (keys old) (keys new))))))

(defn create
  "The rawWorld object over fake primitives p (engine.fake/create)."
  [p]
  (let [state (.-state (.-world p))
        watch-key (keyword (gensym "raw-world"))
        listeners (atom #{})
        epoch (atom 0)
        state-at (state-at-fn)
        light-at (light-at-fn)]
    (add-watch state watch-key
               (fn [_ _ old new]
                 (when (not= (select-keys old [:blocks :unloaded :light :light-default])
                             (select-keys new [:blocks :unloaded :light :light-default]))
                   (swap! epoch inc))
                 (doseq [[x y z :as pos] (changed-cells (:blocks old) (:blocks new))
                         f @listeners]
                   (f x y z (state-of (get (:blocks new) pos "air"))))))
    #js {:stateAt (fn [x y z] (state-at @state x y z))
         :lightAt (fn [x y z] (light-at @state x y z))
         :eye (fn [] (eye @state))
         :offHand (fn [] (get-in @state [:self :offhand]))
         :heldItem (fn [] (get-in @state [:self :held]))
         :sky (fn [] (let [w @state]
                       #js {:timeOfDay (:time w) :rain (if (:raining w) 1 0) :thunder (if (:thundering w) 1 0)}))
         :epoch (fn [] @epoch)
         :version (fn [] fx/MC-VERSION)
         :sightTable (fn [] @sight)
         :stateInfo (fn [id] (@info id))
         :onBlockChange (fn [f] (swap! listeners conj f) (fn [] (swap! listeners disj f)))
         :close (fn [] (remove-watch state watch-key) (reset! listeners #{}))}))
