(ns engine.fake.raw-world
  "The rawWorld reader (the shape engine/js/raw-world.mjs gives the real body) over a fake's world atom, so
  engine.perception runs over the fake exactly as over the body. Block state ids are the default states of the fake's
  block names, with the properties of the world's :states for a door, button, lever, ladder or bubble column (open, powered, facing...); a cell in the spec's :unloaded reads -1. Light comes from the world's :light {[x y z] [sky block]}, else
  :light-default (open daylight [15 0] when unset). The eye is the centre of the body's cell at eye height (the body's own x and z with :exact-eye), looking along the
  world's :yaw/:pitch (Minecraft degrees, as drive sets them), in mineflayer radians. Block changes come from a watch
  on :blocks and :states. The sight table and state names are raw-world.mjs's own, over the fixture registry. Test-only."
  (:require [engine.fake.doors :as doors]
            [engine.fake.node :as node]
            [engine.path.fixture :as fx]))

(def raw-mod (delay (node/require-here "./js/raw-world.mjs")))
(def sight (delay (.sightTable ^js @raw-mod fx/registry)))
(def info (delay (.stateInfo ^js @raw-mod fx/registry)))
(def state-of (memoize (fn [block] (fx/default-state block))))
(def min-y -64)
(def max-y 320)
(def eye-height 1.62)

(defn state-at-pos
  "The state id of the fake's block at pos: its name's default state, with the :states props the planner reads."
  [w pos]
  (let [name (get (:blocks w) pos "air")
        props (doors/path-props w pos)]
    (if (empty? props) (state-of name) (fx/state-id name props))))

(defn state-at-slow [w x y z]
  (cond
    (or (< y min-y) (>= y max-y)) -1
    (contains? (:unloaded w) [x y z]) -1
    :else (state-at-pos w [x y z])))

(defn light-at-slow [w x y z]
  (let [[sky block] (or (get (:light w) [x y z]) (:light-default w) [15 0])]
    (bit-or (bit-shift-left sky 4) block)))

(def coord-limit 1048576)

(defn packed-key
  "x, y, z as one number, or nil outside the range: numbers hash far faster than vector keys. Near the origin
  (x, z in [-1024, 1024)) the key is an int32 (11 + 11 + 9 bits), which the js/Map hashes much faster than the
  wide key (21 + 9 + 21 bits) used elsewhere."
  [x y z]
  (let [ux (+ x 1024) uz (+ z 1024) uy (- y min-y)]
    (if (and (<= 0 ux 2047) (<= 0 uz 2047) (<= 0 uy 384))
      (bit-or (bit-shift-left ux 20) (bit-shift-left uz 9) uy)
      (when (and (< -1 (+ x coord-limit) (* 2 coord-limit)) (< -1 (+ z coord-limit) (* 2 coord-limit))
                 (<= min-y y max-y))
        (+ (* (+ x coord-limit) 1073741824) (* (+ z coord-limit) 512) uy 0.5)))))  ; .5: never equals an int32 key

(defn lookup-cache
  "A lazy per-key cache for fn slow over the world values under ks (two or three keys); reset when any of them changes.
  The hot path is one identity test on the world, then one packed number into a js/Map."
  [ks slow]
  (let [k0 (nth ks 0) k1 (nth ks 1) k2 (get ks 2)
        last-w (volatile! nil) v0 (volatile! nil) v1 (volatile! nil) v2 (volatile! nil)
        cache (volatile! (js/Map.))]
    (fn [w x y z]
      (when-not (identical? w @last-w)
        (let [a (get w k0) b (get w k1) c (when k2 (get w k2))]
          (when-not (and @last-w (identical? a @v0) (identical? b @v1) (identical? c @v2))
            (vreset! v0 a) (vreset! v1 b) (vreset! v2 c)
            (vreset! cache (js/Map.)))
          (vreset! last-w w)))
      (if-let [k (packed-key x y z)]
        (let [^js m @cache v (.get m k)]
          (if (undefined? v)
            (let [v (slow w x y z)] (.set m k v) v)
            v))
        (slow w x y z)))))

(defn state-at-fn []
  (lookup-cache [:blocks :unloaded :states] state-at-slow))

(defn light-at-fn []
  (lookup-cache [:light :light-default] light-at-slow))

(defn eye [w]
  (when-not (:offline w)
    (let [[x y z] (get-in w [:self :pos])
          rad (/ js/Math.PI 180)]
      #js {:x (if (:exact-eye w) x (+ (js/Math.floor x) 0.5)) :y (+ y eye-height) :z (if (:exact-eye w) z (+ (js/Math.floor z) 0.5))
           :yaw (- js/Math.PI (* (or (:yaw w) 0) rad)) :pitch (- (* (or (:pitch w) 0) rad))
           :dimension (get-in w [:self :dimension] "overworld")})))

(defn changed-cells [old new]
  (when-not (identical? old new)
    (filter #(not= (get old %) (get new %)) (distinct (concat (keys old) (keys new))))))

(defn changed-states
  "Cells whose block or :states differ between worlds old and new."
  [old new]
  (distinct (concat (changed-cells (:blocks old) (:blocks new)) (changed-cells (:states old) (:states new)))))

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
                 (when (not= (select-keys old [:blocks :states :unloaded :light :light-default])
                             (select-keys new [:blocks :states :unloaded :light :light-default]))
                   (swap! epoch inc))
                 (doseq [[x y z :as pos] (changed-states old new)
                         f @listeners]
                   (f x y z (state-at-pos new pos)))))
    #js {:stateAt (fn [x y z] (state-at @state x y z))
         :lightAt (fn [x y z] (light-at @state x y z))
         :eye (fn [] (eye @state))
         :offHand (fn [] (or (get-in @state [:self :offhand]) (get-in @state [:equipment "offHand" :name])))
         :heldItem (fn [] (get-in @state [:self :held]))
         :sky (fn [] (let [w @state]
                       #js {:timeOfDay (:time w) :rain (if (:raining w) 1 0) :thunder (if (:thundering w) 1 0)}))
         :epoch (fn [] @epoch)
         :version (fn [] fx/MC-VERSION)
         :sightTable (fn [] @sight)
         :stateInfo (fn [id] (@info id))
         :onBlockChange (fn [f] (swap! listeners conj f) (fn [] (swap! listeners disj f)))
         :close (fn [] (remove-watch state watch-key) (reset! listeners #{}))}))
