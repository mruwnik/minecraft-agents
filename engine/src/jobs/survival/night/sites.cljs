(ns jobs.survival.night.sites
  "The night's sites: failed sites tonight, where a pit can be dug, roofed places, and the retry rules of an exposed hold."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.look :as look]
            [jobs.lib.shelter :as sh]
            [jobs.lib.solid :as solid]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in :as dig-in]
            [jobs.survival.dig-in-cells :as dig-cells]
            [clojure.string :as str]))

(def site-policy
  "The :night-site entries: failed sites, cleared at the morning the night job sees; a day at most."
  {:cap 8 :ttl sh/ms-per-day})

(a/defargs settings
  {:jobs.survival.night/max-sites {:default 4 :doc "Failed sites a night tries before holding exposed." :spec (a/int-in 0 nil)}
   :jobs.survival.night/relocate-reach {:default 16 :doc "How far (blocks) a night looks for a spot to dig in." :spec (a/int-in 0 nil)}
   :jobs.survival.night/retry-after-ms {:default 660000 :doc "How long a body holds exposed before it tries to dig in again (past dig-in's own 10-minute give-up)." :spec (a/int-in 0 nil)}
   :jobs.survival.night/max-retries {:default 2 :doc "Dig-in retries after a night went exposed." :spec (a/int-in 0 nil)}
   :jobs.survival.night/max-flees {:default 3 :doc "Walks to somewhere safer a night tries." :spec (a/int-in 0 nil)}
   :jobs.survival.night/ground-candidates {:default 16 :doc "Seen ground cells costed per flee: the nearest ones." :spec (a/int-in 0 nil)}
   :jobs.survival.night/cave-candidates {:default 16 :doc "Seen cave cells costed per flee: the nearest ones." :spec (a/int-in 0 nil)}
   :jobs.survival.night/cave-scan {:default 2048 :doc "Solid blocks one cave search reads: nearest first, so it reaches about 16 blocks around the body." :spec (a/int-in 0 nil)}})
(defn max-sites [] (settings/get settings :jobs.survival.night/max-sites))

(defn relocate-reach [] (settings/get settings :jobs.survival.night/relocate-reach))

(defn failed-sites
  "Tonight's failed sites [{:pos :reason}]."
  [c]
  (mapv :data (ctx/entries c :night-site)))

(defn site-failed!
  "Remember that the site at pos failed tonight, and why."
  [c pos reason]
  (ctx/remember! c :night-site {:pos pos :reason reason} site-policy))

(defn failed-here?
  "Whether a site that failed tonight lies within dig-in's futile radius of the feet."
  [c]
  (let [here (sh/feet (:primitives c))]
    (boolean (some #(<= (u/dist here %) (dig-in/futile-radius))
                   (map :pos (failed-sites c))))))

(defn pit-site?
  "Whether feet cell f looks like a place a pit can be dug, by what a player sees from the surface: standing room, dry
  ground the carried tools dig, no fluid beside it. How deep the ground goes is left to dig-in, which stops a bad site."
  [p {:keys [x y z] :as f}]
  (let [at (fn [dy] (u/seen-name p {:x x :y (+ y dy) :z z}))
        below {:x x :y (dec y) :z z}]
    (and (solid/solid? (at -1)) (tools/can-harvest? p (at -1)) (not (solid/solid? (at 0))) (not (solid/solid? (at 1)))
         (not (dig-cells/wet? p f)) (not (dig-cells/wet? p {:x x :y (inc y) :z z}))
         (not (dig-cells/wet? p below))
         (not (dig-cells/lateral-fluid p below)))))

(defn clear-of-failed?
  "Whether feet cell f is at least futile-radius+1 from every failed site (tonight's and dig-in's :dig-in-futile entries)."
  [c f]
  (let [failed (concat (map :pos (failed-sites c)) (map (comp :pos :data) (ctx/entries c :dig-in-futile)))]
    (every? #(>= (u/dist % f) (inc (dig-in/futile-radius))) failed)))

(defn relocation-site
  "The nearest feet cell within relocate-reach of the failed site the body is at (its feet when none: a body in its
  pit looks from the surface), at least futile-radius+1 from every failed site (tonight's and dig-in's
  :dig-in-futile entries), where a pit can be dug (pit-site?), or nil."
  [c]
  (let [p (:primitives c)
        feet (sh/feet p)
        here (first (filter #(<= (u/dist feet %) (dig-in/futile-radius)) (map :pos (failed-sites c))))
        {:keys [x y z]} (or here feet)
        offsets (sort-by (fn [[dx dz]] (+ (* dx dx) (* dz dz)))
                         (for [dx (range (- (relocate-reach)) (inc (relocate-reach)))
                               dz (range (- (relocate-reach)) (inc (relocate-reach)))
                               :when (<= (+ (* dx dx) (* dz dz)) (* (relocate-reach) (relocate-reach)))]
                           [dx dz]))]
    (some (fn [[dx dz]]
            (some (fn [dy] (let [f {:x (+ x dx) :y (+ y dy) :z (+ z dz)}]
                             (when (and (clear-of-failed? c f) (pit-site? p f)) f)))
                  [0 -1 1 -2 2]))
          offsets)))


(defn route-lit?
  "Whether every sample (every 2 blocks along the straight line from a to b, over the cells around that height) has some
  cell the body has seen (look/seen-block) with block light >= min-light; an unseen cell is not lit. The straight line
  approximates the route go-to walks (one planner call per candidate is not worth it)."
  [p a b min-light]
  (let [raw (some-> (aget p "perception") :raw)
        n (max 1 (js/Math.ceil (/ (u/dist a b) 2)))
        lit-at (fn [x y z] (and (not (:unknown (look/seen-block p {:x x :y y :z z})))
                                (<= 0 (.stateAt ^js raw x y z))
                                (<= min-light (bit-and (.lightAt ^js raw x y z) 15))))]
    (boolean
     (and raw
          (every? (fn [i]
                    (let [t (/ i n)
                          at (fn [k] (js/Math.floor (+ (k a) (* t (- (k b) (k a))))))]
                      (some #(lit-at (at :x) (+ (at :y) %) (at :z)) [-1 0 1 2])))
                  (range 0 (inc n)))))))

(defn roofed-places-in
  "The remembered places of :roofed-places within radius, roofed by blocks the body has seen (within :roof-height),
  and with lit? also reached over a lit route, as [{:name :pos}] nearest first."
  [c radius lit?]
  (let [p (:primitives c)
        {:keys [roofed-places roof-height lit-light]} (:args c)
        here (sh/feet p)
        roofed? (fn [pos] (some #(sh/solid-at? p (update pos :y + %)) (range 1 (inc roof-height))))]
    (->> roofed-places
         (keep (fn [nm] (when-let [pos (mem/place (ctx/view c) nm)] {:name nm :pos pos})))
         (filter (fn [{:keys [pos]}] (and (<= (u/dist here pos) radius) (roofed? pos) (or (not lit?) (route-lit? p here pos lit-light)))))
         (sort-by #(u/dist here (:pos %))))))

(defn roofed-place
  "The nearest remembered place of :roofed-places within :walk-radius, roofed by blocks the body has seen and reached
  over a lit route, as {:name :pos}, or nil."
  [c]
  (first (roofed-places-in c (:walk-radius (:args c)) true)))

(defn roof-walk-wanted?
  "Whether to walk to a roofed place before digging in: none failed tonight, no dig-in in flight, no walk failed."
  [c]
  (let [m (ctx/mem c)]
    (and (not (:roof-walk-failed m)) (not (:digging m)) (empty? (failed-sites c)))))

(defn exposed-why
  "A short why for a night with no shelter, from the failed sites' reasons: counts per reason, and a hint to move when
  every site was a place the server refused (spawn protection)."
  [sites]
  (let [reasons (map :reason sites)
        counts (frequencies reasons)]
    (cond
      (empty? sites) "no site tried"
      :else (str (str/join ", " (map (fn [[r n]] (str n " " (name r))) (sort-by (comp name key) counts)))
                 (when (every? #{:walls-server-refused} reasons)
                   "; the server refused every block placed (spawn protection?): move well away from here and try again")))))

(defn pit-start
  "The failed site tonight whose pit the body stands in (below it, within the futile radius), or nil."
  [c]
  (let [feet (sh/feet (:primitives c))]
    (first (filter #(and (< (:y feet) (:y %)) (<= (u/dist feet %) (dig-in/futile-radius))) (map :pos (failed-sites c))))))

(defn retry-after-ms [] (settings/get settings :jobs.survival.night/retry-after-ms))

(defn max-retries [] (settings/get settings :jobs.survival.night/max-retries))

(defn retry-due?
  "Whether the exposed hold digs in again: under max-retries, and the body has moved off the spot it went exposed on
  or retry-after-ms passed."
  [c]
  (let [{:keys [pos at retries]} (:exposed (ctx/mem c))]
    (and pos (< (or retries 0) (max-retries))
         (or (>= (u/dist (sh/feet (:primitives c)) pos) (inc (dig-in/futile-radius)))
             (>= (- (ctx/now c) at) (retry-after-ms))))))

(defn max-flees [] (settings/get settings :jobs.survival.night/max-flees))

(defn ground-candidates [] (settings/get settings :jobs.survival.night/ground-candidates))

(defn cave-candidates [] (settings/get settings :jobs.survival.night/cave-candidates))

(defn cave-scan [] (settings/get settings :jobs.survival.night/cave-scan))

