(ns jobs.lib.seen-light
  "The light of remembered cells as the body's memory gives it, not the world's live light: block light flooded from the
  light sources among the remembered blocks, sky light from the cells last seen at full sky
  (engine.perception.store/note-sky!) spread sideways and down, both by vanilla's rules (engine/js/light.mjs's table)
  over remembered blocks only (a cell never seen stops light: Q16, unseen is its worse case). A torch broken out of
  sight still lights what it lit, as a player would think. Flooded per section over the 27 sections round it, kept
  until a cell in them changes."
  (:require [engine.perception.store :as store]))

(def ^:private ^:const n 48)
(def ^:private ^:const cells 110592)
(def ^:private max-kept "Sections whose light is kept per body, past which the kept ones are dropped." 1024)

(def ^:private scratch
  (delay #js {:filter (js/Uint8Array. cells) :faces (js/Uint8Array. cells) :block (js/Uint8Array. cells) :sky (js/Uint8Array. cells)
              :buckets (into-array (repeatedly 16 #(js/Int32Array. cells))) :counts (js/Int32Array. 16)}))

(def ^:private dx #js [0 0 0 0 -1 1])
(def ^:private dy #js [-1 1 0 0 0 0])
(def ^:private dz #js [0 0 -1 1 0 0])

(defn- flood!
  "light.mjs's flood over the window: from the bucketed cells, each level less a cell's filter (at least 1) a step,
  through faces neither side covers."
  [^js levels ^js filt ^js faces ^js buckets ^js counts]
  (let [strides #js [(- (* n n)) (* n n) (- n) n -1 1]
        ^js dx dx ^js dy dy ^js dz dz]
    (loop [l 15]
      (when (pos? l)
        (let [^js q (aget buckets l)]
          (loop [h 0]
            (when (< h (aget counts l))
              (let [a (aget q h)]
                (when (== l (aget levels a))
                  (let [x (mod a n) z (mod (js/Math.floor (/ a n)) n) y (js/Math.floor (/ a (* n n)))
                        fa (aget faces a)]
                    (dotimes [d 6]
                      (let [bx (+ x (aget dx d)) by (+ y (aget dy d)) bz (+ z (aget dz d))]
                        (when (and (< -1 bx n) (< -1 by n) (< -1 bz n))
                          (let [b (+ a (aget strides d))
                                f (aget filt b)
                                nl (- l (max f 1))]
                            (when (and (> nl (aget levels b))
                                       (zero? (bit-and 1 (bit-shift-right fa d)))
                                       (zero? (bit-and 1 (bit-shift-right (aget faces b) (bit-xor d 1)))))
                              (aset levels b nl)
                              (aset (aget buckets nl) (aget counts nl) b)
                              (aset counts nl (inc (aget counts nl)))))))))))
              (recur (inc h)))))
        (recur (dec l))))))

(defn- section-light
  "Uint8Array(4096) by store/cell-index: the packed light (sky << 4 | block) of section cx sy cz from memory."
  [^js table ^js store cx sy cz]
  (let [^js s @scratch
        ^js filt (.-filter s) ^js faces (.-faces s) ^js block (.-block s) ^js sky (.-sky s)
        ^js buckets (.-buckets s) ^js counts (.-counts s)
        ^js emit (.-emit table) ^js t-filter (.-filter table) ^js t-faces (.-faces table)
        push! (fn [l k] (aset (aget buckets l) (aget counts l) k) (aset counts l (inc (aget counts l))))]
    (.fill counts 0)
    (.fill sky 0)
    (doseq [a [0 1 2] b [0 1 2] c [0 1 2]]
      (let [^js sec (.get store (store/section-key (+ cx a -1) (+ sy b -1) (+ cz c -1)))
            ^js ids (some-> sec .-ids)
            ^js bits (some-> sec .-sky)]
        (dotimes [i 4096]
          (let [k (+ (* (+ (* (+ (* 16 b) (bit-shift-right i 8)) n) (* 16 c) (bit-and (bit-shift-right i 4) 15)) n) (* 16 a) (bit-and i 15))
                v (if ids (aget ids i) 0)]
            (if (zero? v)
              (do (aset filt k 15) (aset faces k 0) (aset block k 0))
              (let [id (dec v) e (aget emit id) f (aget t-filter id)]
                (aset filt k f)
                (aset faces k (aget t-faces id))
                (aset block k e)
                (when (pos? e) (push! e k))
                (when (and bits (< f 15) (pos? (bit-and (aget bits (bit-shift-right i 3)) (bit-shift-left 1 (bit-and i 7)))))
                  (aset sky k 15))))))))
    (flood! block filt faces buckets counts)
    (.fill counts 0)
    ;; only a full-sky cell beside a dimmer one has light to give
    (dotimes [gy n]
      (dotimes [gz n]
        (dotimes [gx n]
          (let [k (+ (* (+ (* gy n) gz) n) gx)]
            (when (and (== 15 (aget sky k))
                       (or (and (pos? gx) (< (aget sky (dec k)) 15)) (and (< gx (dec n)) (< (aget sky (inc k)) 15))
                           (and (pos? gz) (< (aget sky (- k n)) 15)) (and (< gz (dec n)) (< (aget sky (+ k n)) 15))
                           (and (pos? gy) (< (aget sky (- k (* n n))) 15)) (and (< gy (dec n)) (< (aget sky (+ k (* n n))) 15))))
              (push! 15 k))))))
    (flood! sky filt faces buckets counts)
    (let [out (js/Uint8Array. 4096)]
      (dotimes [i 4096]
        (let [k (+ (* (+ (* (+ 16 (bit-shift-right i 8)) n) 16 (bit-and (bit-shift-right i 4) 15)) n) 16 (bit-and i 15))]
          (aset out i (bit-or (bit-shift-left (aget sky k) 4) (aget block k)))))
      out)))

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
