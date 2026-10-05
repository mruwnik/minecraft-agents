(ns engine.jobs.watch
  "Looking round while a job stares at its work (card 943cac28). A body that digs or builds faces one way, and perception
  sees only inside the view cone, so a creeper (silent) walking up behind it stays unknown. A job calls `watch!` at the
  seams between its acts; when the place is risky it turns the head to the open headings round the body and takes a mob
  sample after each look, so the danger checks and the hostile reflex see what a player who glanced round would.

  Risky means: the feet cell is dark (effective light under 8), a hostile is known within 24 blocks in the last 30 s
  (alert), or the job says so (:risky? true; a strip tunnel is). A quiet, lit place never scans. Scans run every
  :every-ms (3 s; :alert-ms, 2 s, while alert), before a dig of 2 s or longer, and when more headings have opened
  than at the last scan (a tunnel breaking into a cave). A mob heard but not seen is turned to once per 5 s.
  The clock is body memory (kind :watched), so a parent and its child share it. Only headings with a clear line at
  feet and head height are looked at (the body's own neighbours: no x-ray); no sight pass is run, only mob samples.
  The helper never turns back: the job's next act aims itself. A body with no perception does nothing."
  (:require [engine.ctx :as ctx]
            [engine.perception :as perception]))

(def dark-light "A feet cell under this effective light is dark: hostiles spawn and walk in from it." 8)
(def alert-radius 24)
(def alert-ms 30000)
(def default-every-ms 3000)
(def default-alert-every-ms 2000)
(def long-dig-ms 2000)
(def dig-gap-ms 1000)
(def turn-gap-ms 5000)
(def hearing-radius 16)
(def probe-distance 3)

(def memory-policy {:cap 1 :ttl 600000})
(def turn-policy {:cap 20 :ttl turn-gap-ms})

(defn perception-of [c] (aget (:primitives c) "perception"))

(defn effective-light
  "Light at the cell (x y z) of raw: the brighter of block light and sky light less the sky darkening of the hour."
  [raw x y z]
  (let [packed (.lightAt raw x y z)
        sky (bit-shift-right packed 4)
        block (bit-and packed 15)
        s (.sky raw)
        darken (perception/sky-darken (.-timeOfDay s) (.-rain s) (.-thunder s))
        subtract (js/Math.round (* 11 (/ (- 1 darken) 0.8)))]
    (max block (- sky subtract))))

(defn dark-here? [c]
  (let [per (perception-of c)
        raw (:raw per)
        ^js pos (.-pos (.self (:primitives c)))]
    (and raw
         (< (effective-light raw (js/Math.floor (.-x pos)) (js/Math.floor (.-y pos)) (js/Math.floor (.-z pos)))
            dark-light))))

(defn known-mobs [c]
  (when-let [f (aget (:primitives c) "knownMobs")]
    (array-seq (f))))

(defn alert? [c]
  (boolean (some #(and (<= (.-distance ^js %) alert-radius) (<= (.-ageMs ^js %) alert-ms)) (known-mobs c))))

(defn risk
  "The risk of this place now: :alert, :risky or :quiet. opts: :risky? the job says it is risky whatever the light."
  [c {:keys [risky?]}]
  (cond
    (nil? (perception-of c)) :quiet
    (alert? c) :alert
    (or risky? (dark-here? c)) :risky
    :else :quiet))

;; ---- where to look

(defn eye-of [c] (.eye ^js (:raw (perception-of c))))

(defn forward
  "The unit [dx dz] the eye faces (yaw 0 faces -z)."
  [^js eye]
  [(- (js/Math.sin (.-yaw eye))) (- (js/Math.cos (.-yaw eye)))])

(defn headings
  "The four turns from the facing, back first: [dx dz] for 180, +90 and -90 degrees (the forward view is the job's own)."
  [[fx fz]]
  [[(- fx) (- fz)] [(- fz) fx] [fz (- fx)]])

(defn open-heading?
  "Whether the eye has a clear line three blocks along [dx dz] at eye and at feet height."
  [raw table ^js eye [dx dz]]
  (let [ox (.-x eye) oy (.-y eye) oz (.-z eye)
        clear? (fn [y] (perception/line-clear? raw table ox y oz (+ ox (* probe-distance dx)) y (+ oz (* probe-distance dz))))]
    (and (clear? oy) (clear? (- oy 1.1)))))

(defn open-headings [c]
  (let [per (perception-of c)
        raw (:raw per)
        table (.sightTable raw)
        eye (eye-of c)]
    (when (and eye table)
      (filterv #(open-heading? raw table eye %) (headings (forward eye))))))

(defn look-point [^js eye [dx dz]]
  {:x (+ (.-x eye) (* 4 dx)) :y (.-y eye) :z (+ (.-z eye) (* 4 dz))})

;; ---- when to look

(defn last-scan [c] (ctx/latest c :watched))

(defn dig-ms [c pos]
  (when pos
    (let [f (aget (:primitives c) "digTime")]
      (or (some-> f (.call (:primitives c) (clj->js pos)) js/Number) 0))))

(defn heard-only
  "Known mobs heard now and not seen, not yet turned to in the last 5 s."
  [c]
  (let [turned (set (map (comp :id :data) (ctx/entries c :watch-turned)))]
    (filter #(and (.-heard ^js %) (not (.-seen ^js %)) (<= (.-distance ^js %) hearing-radius)
                  (not (turned (.-id ^js %))))
            (known-mobs c))))

(defn due?
  "Whether a scan is due at level: the interval is up, a long dig is next, or more headings are open than last time."
  [c level {:keys [every-ms alert-ms before-dig]} open]
  (let [last (last-scan c)
        age (if last (- (ctx/now c) (:t last)) js/Infinity)
        interval (if (= :alert level) (or alert-ms default-alert-every-ms) (or every-ms default-every-ms))]
    (boolean
     (or (>= age interval)
         (and (> (count open) (get-in last [:data :open] 0)) (> age dig-gap-ms))
         (and before-dig (> age dig-gap-ms) (>= (or (dig-ms c before-dig) 0) long-dig-ms))))))

(defn sample!
  "One mob sample (what a glance takes in); the known mobs not in before (a set of ids), nearest first."
  [c before]
  (filterv #(not (before (.-id ^js %))) (known-mobs c)))

(defn ^:async scan!
  "Look along each open heading, a mob sample after each; :saw (and watch.saw) when a new hostile became known."
  [c open]
  (let [before (set (map #(.-id ^js %) (known-mobs c)))
        started (ctx/now c)]
    (ctx/remember! c :watched {:open (count open)} memory-policy)
    (loop [hs open]
      (if-let [h (first hs)]
        (do (await (ctx/act c :look (clj->js {:pos (look-point (eye-of c) h)})))
            (if-let [^js m (first (sample! c before))]
              (do (ctx/emit! c :watch.saw :info {:name (.-name m) :pos (js->clj (.-pos m) :keywordize-keys true)
                                                 :distance (.-distance m) :how (if (.-heard m) :heard :seen)
                                                 :after-ms (- (ctx/now c) started)})
                  :saw)
              (recur (rest hs))))
        :scanned))))

(defn ^:async turn-to!
  "Turn once toward a mob heard and not seen, then sample."
  [c ^js m]
  (let [before (set (map #(.-id ^js %) (known-mobs c)))]
    (ctx/remember! c :watch-turned {:id (.-id m)} turn-policy)
    (ctx/remember! c :watched {:open (get-in (last-scan c) [:data :open] 0)} memory-policy)
    (await (ctx/act c :look (clj->js {:pos (js->clj (.-pos m) :keywordize-keys true)})))
    (sample! c before)
    :turned))

(defn ^:async watch!
  "Look round if this place is risky and a scan is due. opts: :risky? (the job says so), :every-ms, :alert-ms,
  :before-dig {x y z} (the cell the job digs next). Returns :skipped, :scanned, :turned (to a heard mob) or :saw."
  [c opts]
  (let [level (risk c opts)]
    (if (nil? (perception-of c))
      :skipped
      (if-let [m (first (heard-only c))]
        (await (turn-to! c m))
        (if (= :quiet level)
          :skipped
          (let [open (or (open-headings c) [])]
            (if (due? c level opts open)
              (await (scan! c open))
              :skipped)))))))
