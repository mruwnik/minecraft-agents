(ns jobs.lib.seen-light
  "The light of remembered cells as the body's memory gives it, not the world's live light: block light flooded from the
  light sources among the remembered blocks, sky light from the cells last seen at full sky
  (engine.perception.store/note-sky!) spread sideways and down, both by vanilla's rules (engine/js/light.mjs's table)
  over remembered blocks only, by light.mjs's own relight (one formula). A cell never seen is a wall to the flood (Q16, unseen
  is the worse case): light never reaches a seen cell through rock the body has not seen, so a cell reads dark unless
  the way to its light is known; a lit tunnel with unseen stretches reads darker than it is, the safe side for spawn
  danger.
  Nothing ages: a remembered torch or roof lights its cells until the cell is seen again, as a player would think;
  memory-age prices only fire, doors and fluids, the one measured kinds, and no change rate of torches or roofs is
  measured. Flooded per section over the 27 sections round it, kept until a cell in them changes."
  (:require [engine.perception.store :as store]))

(def ^:private ^:const n 48)
(def ^:private ^:const cells 110592)
(def ^:private max-kept "Sections whose light is kept per body, past which the kept ones are dropped." 1024)

(def ^:private scratch
  (delay #js {:states (js/Int32Array. cells) :block (js/Uint8Array. cells) :sky (js/Uint8Array. cells)}))

(defn- opaque-id
  "A state of the table that lets no light through and gives none: what a cell never seen is to the flood."
  [^js table]
  (let [^js filt (.-filter table) ^js emit (.-emit table) ^js faces (.-faces table)]
    (loop [i 0] (if (and (== 15 (aget filt i)) (zero? (aget emit i)) (zero? (aget faces i))) i (recur (inc i))))))

(defn- section-light
  "Uint8Array(4096) by store/cell-index: the packed light (sky << 4 | block) of section cx sy cz from memory, by
  light.mjs's relight over the 27 sections round it."
  [^js table ^js store cx sy cz]
  (let [^js s @scratch
        ^js states (.-states s) ^js sky (.-sky s) ^js block (.-block s)
        ^js t-filter (.-filter table)
        wall (opaque-id table)]
    (.fill block 0)
    (dotimes [a 3] (dotimes [b 3] (dotimes [c 3]
      (let [^js sec (.get store (store/section-key (+ cx a -1) (+ sy b -1) (+ cz c -1)))
            ^js ids (some-> sec .-ids)
            ^js bits (some-> sec .-sky)]
        (dotimes [i 4096]
          (let [k (+ (* (+ (* (+ (* 16 b) (bit-shift-right i 8)) n) (* 16 c) (bit-and (bit-shift-right i 4) 15)) n) (* 16 a) (bit-and i 15))
                v (if ids (aget ids i) 0)
                id (if (pos? v) (dec v) wall)]
            (aset states k id)
            (aset sky k (if (and bits (pos? v) (< (aget t-filter id) 15) (pos? (bit-and (aget bits (bit-shift-right i 3)) (bit-shift-left 1 (bit-and i 7))))) 15 0))))))))
    (let [^js out (.relight table #js {:table table :states states :sky sky :block block :size #js [n n n] :interiorSky true})
          ^js lsky (.-sky out) ^js lblock (.-block out)
          res (js/Uint8Array. 4096)]
      (dotimes [i 4096]
        (let [k (+ (* (+ (* (+ 16 (bit-shift-right i 8)) n) 16 (bit-and (bit-shift-right i 4) 15)) n) 16 (bit-and i 15))]
          (aset res i (bit-or (bit-shift-left (aget lsky k) 4) (aget lblock k)))))
      res)))

(defn- neighbours-mask
  "The sections of the 27 round cx sy cz that are in memory, as bits, and the latest write among them."
  [^js store cx sy cz]
  (loop [i 0 mask 0 wrote 0]
    (if (== i 27)
      #js [mask wrote]
      (let [^js sec (.get store (store/section-key (+ cx (dec (mod i 3))) (+ sy (dec (mod (quot i 3) 3))) (+ cz (dec (quot i 9)))))]
        (recur (inc i) (if sec (bit-or mask (bit-shift-left 1 i)) mask) (max wrote (or (some-> sec .-wrote) 0)))))))

(defonce ^:private kept (js/WeakMap.))

(defn light-fn
  "(fn [x y z]) the packed light (sky << 4 | block) the body's memory gives the cell (see the ns doc), -1 for a cell it
  has not seen; nil without a perception's memory or a light table."
  [p]
  (when-let [^js st (some-> (aget p "perception") :st)]
    (when-let [^js table (and (.-stores st) (store/light-table st))]
      (let [dim (.-dim st)
            ^js store (store/store-of st dim)
            ^js by-dim (or (.get kept st) (let [m (js/Map.)] (.set kept st m) m))
            ^js secs (or (.get by-dim dim) (let [m (js/Map.)] (.set by-dim dim m) m))]
        (fn [x y z]
          (let [cx (bit-shift-right x 4) sy (bit-shift-right y 4) cz (bit-shift-right z 4)
                key (store/section-key cx sy cz)
                ^js sec (.get store key)
                i (store/cell-index x y z)]
            (if (or (nil? sec) (zero? (aget (.-ids sec) i)))
              -1
              (let [^js hit (.get secs key)
                    writes (.-writes st)]
                (if (and hit (== writes (.-at hit)))
                  (aget (.-light hit) i)
                  (let [^js now (neighbours-mask store cx sy cz)]
                    (if (and hit (== (aget now 0) (.-mask hit)) (<= (aget now 1) (.-at hit)))
                      (do (set! (.-at hit) writes) (aget (.-light hit) i))
                      (let [light (section-light table store cx sy cz)]
                        (when (>= (.-size secs) max-kept) (.clear secs))
                        (.set secs key #js {:at writes :mask (aget now 0) :light light})
                        (aget light i)))))))))))))
