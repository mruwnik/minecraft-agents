(ns view.window
  "The toroidal window of chunk columns a scene (view.scene) keeps around the eye: (2 radius + 1)^2 columns, each in the GL
   world slot (mod cx n, mod cz n), so a step of the eye replaces one row of slots and keeps the rest.

   Runs on every chunk step of the eye, so it is written in the tuned style (see view.interp): JS Maps and column objects,
   mutated in place, no persistent data. A column is the JS object #js {cx, cz, dist, status}; status is \"pending\",
   \"loaded\" or \"missing\".")

(set! *warn-on-infer* true)

(defn modulo ^number [^number a ^number n] (js-mod (+ (js-mod a n) n) n))
(defn key-of [cx cz] (str cx "." cz))
(defn slot-key [cx cz n] (str (modulo cx n) "." (modulo cz n)))

(defn move-window
  "The window around the eye's chunk (ccx, ccz). Columns already owned by their slot are kept (the same object); the rest get
   a fresh #js {cx, cz, dist 0, status \"pending\"} and take the slot over (owners, slot -> column key, is updated in place).
   Every column's dist is from the eye chunk. Returns #js {columns: the new Map key -> column, fresh: the fresh columns}, whose
   slots the caller clears and refills."
  [ccx ccz radius ^js previous ^js owners]
  (let [n (inc (* 2 radius))
        columns (js/Map.)
        fresh #js []]
    (loop [cx (- ccx radius)]
      (when (<= cx (+ ccx radius))
        (loop [cz (- ccz radius)]
          (when (<= cz (+ ccz radius))
            (let [k (key-of cx cz)
                  slot (slot-key cx cz n)
                  ^js known (.get previous k)
                  dist (js/Math.hypot (- cx ccx) (- cz ccz))]
              (if (and (some? known) (= (.get owners slot) k))
                (do (set! (.-dist known) dist)
                    (.set columns k known))
                (let [column #js {:cx cx :cz cz :dist dist :status "pending"}]
                  (.set owners slot k)
                  (.set columns k column)
                  (.push fresh column))))
            (recur (inc cz))))
        (recur (inc cx))))
    #js {:columns columns :fresh fresh}))
