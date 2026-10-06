(ns dashboard.rcon-cart
  "tools/cart-sample.mjs: follows one minecart over RCON, samples game time, Pos and Motion as fast as RCON allows, writes
  JSON lines and prints the windowed-speed summary (minimum and where, per-cell speeds, recovery distance). The maths is
  pure and tested; main does the I/O. Speeds are blocks per game tick over a window of ticks (3D distance)."
  (:require ["fs" :as fs]
            [clojure.string :as str]
            [dashboard.rcon :as rcon]))

(def uuid-pattern #"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")
(def name-pattern #"^[A-Za-z0-9_]{3,16}$")
(def number-pattern #"^[+-]?\d+(\.\d+)?$")
(def batch-samples "Samples per RCON connection." 20)

(defn fail [& parts] (throw (js/Error. (apply str parts))))

;; ---- reply parsing

(defn parse-triple
  "[x y z] from a `data get entity ... Pos|Motion` reply."
  [reply]
  (let [inner (second (re-find #"\[([^\]]*)\]\s*$" reply))
        nums (when inner (mapv #(js/parseFloat (str/replace % #"d$" "")) (str/split inner #",\s*")))]
    (when-not (and nums (= 3 (count nums)) (every? #(not (js/isNaN %)) nums))
      (fail "no entity data in the reply: " (str/trim reply)))
    nums))

(defn parse-gametime [reply]
  (if-let [[_ n] (re-find #"[Tt]he (?:game )?time is (\d+)" reply)]
    (js/parseInt n 10)
    (fail "no game time in the reply (time query): " (str/trim reply))))

(defn int-array->uuid
  "The hyphenated UUID from a `data get entity ... UUID` reply holding [I; a, b, c, d]."
  [reply]
  (let [[_ body] (re-find #"\[I;([^\]]*)\]" reply)
        ints (when body (mapv #(js/parseInt % 10) (str/split (str/trim body) #",\s*")))]
    (when-not (= 4 (count ints)) (fail "no entity UUID in the reply: " (str/trim reply)))
    (let [hex (apply str (map #(.padStart (.toString (unsigned-bit-shift-right % 0) 16) 8 "0") ints))]
      (str (subs hex 0 8) "-" (subs hex 8 12) "-" (subs hex 12 16) "-" (subs hex 16 20) "-" (subs hex 20)))))

;; ---- the maths

(defn dedupe-ticks
  "Keeps the first sample of each game tick (RCON can answer several times within one tick)."
  [samples]
  (:out (reduce (fn [{:keys [last-t out]} s]
                  (if (= (:t s) last-t) {:last-t last-t :out out} {:last-t (:t s) :out (conj out s)}))
                {:last-t nil :out []}
                samples)))

(defn distance [a b]
  (js/Math.sqrt (reduce + (map #(let [d (- %1 %2)] (* d d)) a b))))

(defn windowed-speeds
  "One entry per sample that has an earlier sample at least `window` game ticks before it: {:t :pos :ticks :speed}, speed
  = distance from the latest such earlier sample / ticks between them, attributed to this sample's position."
  [samples window]
  (let [v (vec samples)]
    (loop [j 0, i 0, out []]
      (if (= j (count v))
        out
        (let [sj (v j)
              ;; advance i to the latest sample at least `window` ticks before j
              i (loop [i i] (if (and (< (inc i) j) (>= (- (:t sj) (:t (v (inc i)))) window)) (recur (inc i)) i))
              ticks (- (:t sj) (:t (v i)))]
          (recur (inc j) i
                 (if (and (< i j) (>= ticks window))
                   (conj out {:t (:t sj) :pos (:pos sj) :ticks ticks :speed (/ (distance (:pos sj) (:pos (v i))) ticks)})
                   out)))))))

(defn min-window
  "The windowed-speed entry with the lowest speed (the first on a tie), or nil."
  [samples window]
  (reduce (fn [best w] (if (or (nil? best) (< (:speed w) (:speed best))) w best))
          nil (windowed-speeds samples window)))

(defn cell-of [pos] [(int (js/Math.floor (nth pos 0))) (int (js/Math.floor (nth pos 2)))])

(defn cell-speeds
  "Per block column [x z] in order of first visit: {:cell :n :min :mean} of the windowed speeds."
  [windowed]
  (let [groups (reduce (fn [m w] (update m (cell-of (:pos w)) (fnil conj []) (:speed w))) {} windowed)
        order (distinct (map #(cell-of (:pos %)) windowed))]
    (mapv (fn [c] (let [xs (groups c)] {:cell c :n (count xs) :min (apply min xs) :mean (/ (reduce + xs) (count xs))}))
          order)))

(defn recovery
  "From the first windowed sample in `cell`: the first dip below threshold, then the first sample back at or above it.
  {:distance (path length from the cell entry) :ticks}; zero when there is no dip; :distance nil when it never recovers;
  nil when the cart never reached the cell."
  [windowed cell threshold]
  (let [from (vec (drop-while #(not= cell (cell-of (:pos %))) windowed))
        dip (first (keep-indexed (fn [i w] (when (< (:speed w) threshold) i)) from))
        hit (when dip (first (keep-indexed (fn [i w] (when (and (> i dip) (>= (:speed w) threshold)) i)) from)))]
    (cond
      (empty? from) nil
      (nil? dip) {:distance 0.0 :ticks 0}
      (nil? hit) {:distance nil :ticks nil}
      :else (let [path (map :pos (take (inc hit) from))]
              {:distance (reduce + 0.0 (map distance path (rest path)))
               :ticks (- (:t (from hit)) (:t (first from)))}))))

(defn stopped?
  "True when the cart moved less than 0.01 blocks over the last `window` game ticks."
  [samples window]
  (let [v (vec samples)
        last-s (peek v)
        before (last (filter #(>= (- (:t last-s) (:t %)) window) v))]
    (boolean (and before (< (distance (:pos last-s) (:pos before)) 0.01)))))

;; ---- arguments

(defn num-arg [s what]
  (when-not (re-matches number-pattern (str s)) (fail what " must be a plain number, got " (pr-str s)))
  (js/parseFloat s))

(defn parse-args
  "argv -> {:select {:uuid|:rider|:near} :secs :window :threshold :cell :until-stop :out}."
  [argv]
  (loop [args (vec argv)
         opts {:secs 30 :window 10 :threshold 0.3 :select {}}]
    (if (empty? args)
      (if (= 1 (count (:select opts)))
        opts
        (fail "give exactly one of --uuid <uuid>, --rider <player>, --near <x> <y> <z>"))
      (let [[flag & more] args
            take1 (fn [] (when (empty? more) (fail flag " needs a value")) (first more))]
        (case flag
          "--uuid" (let [u (take1)]
                     (when-not (re-matches uuid-pattern u) (fail "--uuid must be a hyphenated lower-case UUID"))
                     (recur (subvec args 2) (assoc-in opts [:select :uuid] u)))
          "--rider" (let [n (take1)]
                      (when-not (re-matches name-pattern n) (fail "--rider needs a valid player name"))
                      (recur (subvec args 2) (assoc-in opts [:select :rider] n)))
          "--near" (do (when (< (count more) 3) (fail "--near needs x y z"))
                       (recur (subvec args 4) (assoc-in opts [:select :near] (mapv #(num-arg % "--near") (take 3 more)))))
          "--secs" (recur (subvec args 2) (assoc opts :secs (num-arg (take1) "--secs")))
          "--window" (recur (subvec args 2) (assoc opts :window (num-arg (take1) "--window")))
          "--threshold" (recur (subvec args 2) (assoc opts :threshold (num-arg (take1) "--threshold")))
          "--cell" (do (when (< (count more) 2) (fail "--cell needs x z"))
                       (recur (subvec args 3) (assoc opts :cell (mapv #(num-arg % "--cell") (take 2 more)))))
          "--until-stop" (recur (subvec args 1) (assoc opts :until-stop true))
          "--out" (recur (subvec args 2) (assoc opts :out (take1)))
          (fail "unknown option " flag))))))

;; ---- I/O

(defn selector
  "[the entity selector for `data get entity`, the NBT path prefix] for a resolved UUID or a rider."
  [{:keys [uuid rider]}]
  (if rider [rider "RootVehicle.Entity."] [uuid ""]))

(defn nearest-selector [[x y z]]
  (str "@e[type=minecart,x=" x ",y=" y ",z=" z ",sort=nearest,limit=1]"))

(defn resolve-target!
  "Resolves to {:uuid} or {:rider} for the selection; --near is pinned to the nearest minecart's UUID once."
  [{:keys [near] :as select}]
  (if-not near
    (js/Promise.resolve select)
    (-> (rcon/send-commands! {} [(str "data get entity " (nearest-selector near) " UUID")])
        (.then (fn [[reply]] {:uuid (int-array->uuid reply)})))))

(defn sample-commands [[sel prefix]]
  [(str "time query gametime")
   (str "data get entity " sel " " prefix "Pos")
   (str "data get entity " sel " " prefix "Motion")])

(defn replies->sample [[time pos motion]]
  {:t (parse-gametime time) :pos (parse-triple pos) :motion (parse-triple motion)})

(defn sample-batch! [target]
  (-> (rcon/send-commands! {} (vec (mapcat identity (repeat batch-samples (sample-commands (selector target))))))
      (.then #(mapv replies->sample (partition 3 %)))))

(defn collect!
  "Resolves to the deduplicated samples: batches until the time is up (or, with :until-stop, the cart has stopped after
  moving). Each new sample goes to (on-sample sample)."
  [target {:keys [secs until-stop window]} on-sample]
  (let [deadline (+ (js/Date.now) (* 1000 secs))]
    (letfn [(step [samples]
              (-> (sample-batch! target)
                  (.then (fn [batch]
                           (let [fresh (filterv #(> (:t %) (or (:t (peek samples)) -1)) (dedupe-ticks batch))
                                 samples (into samples fresh)]
                             (run! on-sample fresh)
                             (cond
                               (>= (js/Date.now) deadline) samples
                               (and until-stop (stopped? samples window)
                                    (some #(> (distance (:pos %) (:pos (first samples))) 1) samples)) samples
                               :else (step samples)))))))]
      (step []))))

(defn fmt [x] (if (nil? x) "-" (.toFixed x 3)))

(defn summary-lines [samples {:keys [window threshold cell]}]
  (let [w (windowed-speeds samples window)
        low (min-window samples window)
        rec (when cell (recovery w (mapv int cell) threshold))]
    (concat
     [(str "samples " (count samples) " over " (if (seq samples) (- (:t (peek samples)) (:t (first samples))) 0) " ticks; window " window " ticks")]
     (if low
       [(str "min speed " (fmt (:speed low)) " b/tick at tick " (:t low) " pos " (str/join " " (map #(.toFixed % 2) (:pos low)))
             " (cell " (str/join " " (cell-of (:pos low))) ")")
        (str "max speed " (fmt (apply max (map :speed w))) " b/tick")]
       ["too few samples for a window"])
     (when cell
       [(cond
          (nil? rec) (str "cell " (str/join " " cell) ": never reached")
          (nil? (:distance rec)) (str "cell " (str/join " " cell) ": speed never back to " threshold)
          :else (str "cell " (str/join " " cell) ": back to >= " threshold " b/tick after "
                     (fmt (:distance rec)) " blocks, " (:ticks rec) " ticks"))])
     ["cell x z: n, min, mean b/tick"]
     (map (fn [{:keys [cell n min mean]}] (str "  " (str/join " " cell) ": " n ", " (fmt min) ", " (fmt mean)))
          (cell-speeds w)))))

(defn main [argv]
  (let [opts (try (parse-args argv) (catch :default e (js/console.error (.-message e)) nil))]
    (if-not opts
      (js/Promise.resolve 2)
      (-> (resolve-target! (:select opts))
          (.then (fn [target]
                   (let [out (when (:out opts) (.openSync fs (:out opts) "w"))
                         on-sample (fn [s] (if out (.writeSync fs out (str (js/JSON.stringify (clj->js s)) "\n")) (js/console.log (js/JSON.stringify (clj->js s)))))]
                     (-> (collect! target opts on-sample)
                         (.then (fn [samples]
                                  (when out (.closeSync fs out))
                                  (run! js/console.log (summary-lines samples opts))
                                  0))))))
          (.catch (fn [e] (js/console.error (str "cart-sample failed: " (.-message e))) 1))))))
