(ns engine.perception.light
  "The light curve: how bright a cell looks to the body (tools/view/web/shading.mjs: vanilla's lightmap at default
  brightness, max channel), and the table of packed light levels bright enough to make out.")

(defn clamp [v lo hi] (min hi (max lo v)))
(defn mix [a b t] (+ a (* (- b a) t)))
(defn fract [v] (- v (js/Math.floor v)))

(defn celestial-angle [t]
  (let [d (fract (- (/ t 24000) 0.25))
        e (- 0.5 (/ (js/Math.cos (* d js/Math.PI)) 2))]
    (/ (+ (* 2 d) e) 3)))

(defn sky-darken [t rain thunder]
  (let [f (celestial-angle t)
        g (- 1 (clamp (- 1 (+ (* (js/Math.cos (* f 2 js/Math.PI)) 2) 0.2)) 0 1))
        g (* g (- 1 (/ (* rain 5) 16)) (- 1 (/ (* thunder 5) 16)))]
    (+ (* g 0.8) 0.2)))

(defn brightness [level] (let [f (/ level 15)] (/ f (- 4 (* 3 f)))))

(defn channel [v sky-v s]
  (let [c (clamp (mix (+ v (* sky-v s)) 0.75 0.04) 0 1)
        c (mix c (- 1 (js/Math.pow (- 1 c) 4)) 0.5)]
    (clamp (mix c 0.75 0.04) 0 1)))

(defn seeing
  "How bright a cell with sky and block light (0..15) looks: the brightest channel of the lightmap, 0.099..0.99."
  [sky block darken]
  (let [s (* (brightness sky) (+ (* darken 0.95) 0.05))
        b (* (brightness block) 1.5)
        sv (mix darken 1 0.35)]
    (max (channel b sv s)
         (channel (* b (+ (* (+ (* b 0.6) 0.4) 0.6) 0.4)) sv s)
         (channel (* b (+ (* b b 0.6) 0.4)) 1 s))))

(defn visible-table
  "Uint8Array over packed light (sky << 4 | block): 1 where the cell is bright enough to make out."
  [darken seeing-min]
  (let [out (js/Uint8Array. 256)]
    (dotimes [i 256]
      (when (>= (seeing (bit-shift-right i 4) (bit-and i 15) darken) seeing-min) (aset out i 1)))
    out))

(defn darken-now [raw]
  (let [sky (.sky ^js raw)]
    (sky-darken (.-timeOfDay sky) (.-rain sky) (.-thunder sky))))

(defn table-now [raw seeing-min]
  (visible-table (darken-now raw) seeing-min))

(defn max-light
  "Per channel, the brighter of two packed lights."
  [a b]
  (bit-or (max (bit-and a 0xF0) (bit-and b 0xF0)) (max (bit-and a 15) (bit-and b 15))))
