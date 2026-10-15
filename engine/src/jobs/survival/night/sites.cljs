(ns jobs.survival.night.sites
  "The night's sites: failed sites tonight, where a pit can be dug, roofed places, and the retry rules of an exposed hold."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.sense :as sense]
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
  {:jobs.survival.night.sites/max-sites {:default 4 :doc "Failed sites a night tries before holding exposed." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/relocate-reach {:default 16 :doc "How far (blocks) a night looks for a spot to dig in." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/retry-after-ms {:default 660000 :doc "How long a body holds exposed before it tries to dig in again (past dig-in's own 10-minute give-up)." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/max-retries {:default 2 :doc "Dig-in retries after a night went exposed." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/max-flees {:default 3 :doc "Walks to somewhere safer a night tries." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/ground-candidates {:default 16 :doc "Seen ground cells costed per flee: the nearest ones." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/cave-candidates {:default 16 :doc "Seen cave cells costed per flee: the nearest ones." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/max-dark-cells {:default 0 :doc "Dark cells (seen dark, or unseen at night) go-to's planned route to a roofed place may cross for the night to walk there instead of digging in." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/cave-scan {:default 2048 :doc "Solid blocks one cave search reads: nearest first, so it reaches about 16 blocks around the body." :spec (a/int-in 0 nil)}})
(defn max-dark-cells [] (settings/get settings :jobs.survival.night.sites/max-dark-cells))
(defn max-sites [] (settings/get settings :jobs.survival.night.sites/max-sites))

(defn relocate-reach [] (settings/get settings :jobs.survival.night.sites/relocate-reach))

(defn failed-sites
  "Tonight's failed sites [{:pos :reason}]."
  [c]
  (mapv :data (ctx/entries c :night-site)))

(defn site-failed!
  "Remember that the site at pos failed tonight, and why."
  [c pos reason]
  (ctx/remember! c :night-site {:pos pos :reason reason} site-policy))

(defn forget-failed!
  "The night is over: forget tonight's failed sites and dig-in's own (:dig-in-futile), so the next night tries them again."
  [c]
  (ctx/forget-where! c :night-site (constantly true))
  (ctx/forget-where! c :dig-in-futile (constantly true)))

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

(defn zone-refused?
  "Whether another's zone, claim or plan refuses the pit under feet cell f (three cells down) or the block at f."
  [c {:keys [x y z]}]
  (let [in (access/rules-input c)]
    (boolean (or (some #(access/trespass-refusal in :dig {:x x :y (- y %) :z z}) [1 2 3])
                 (access/trespass-refusal (assoc in :feet nil) :place {:x x :y y :z z})))))

(defn relocation-site
  "The nearest feet cell within relocate-reach of the failed site the body is at (its feet when none: a body in its
  pit looks from the surface), at least futile-radius+1 from every failed site (tonight's and dig-in's
  :dig-in-futile entries), where a pit can be dug (pit-site?), or nil. With :free?, only cells no zone, claim or plan refuses."
  [c & [{:keys [free?]}]]
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
                             (when (and (clear-of-failed? c f) (pit-site? p f) (not (and free? (zone-refused? c f)))) f)))
                  [0 -1 1 -2 2]))
          offsets)))


(defn roofed-places-in
  "The remembered places of :roofed-places within radius and roofed within :roof-height, as [{:name :pos}] nearest
  first. A roof cell the body has not seen is guessed solid (u/block-name-or): from the ground the cell over a home
  hides behind the roof's near edge; a column seen open is no roof."
  [c radius]
  (let [p (:primitives c)
        {:keys [roofed-places roof-height]} (:args c)
        here (sh/feet p)
        roofed? (fn [pos] (some #(solid/solid? (u/block-name-or p (update pos :y + %) "stone")) (range 1 (inc roof-height))))]
    (->> roofed-places
         (keep (fn [nm] (when-let [pos (mem/place (ctx/view c) nm)] {:name nm :pos pos})))
         (filter (fn [{:keys [pos]}] (and (<= (u/dist here pos) radius) (roofed? pos))))
         (sort-by #(u/dist here (:pos %))))))

(defn ^:async route-dark
  "The dark cells (seen dark, or unseen at night) on the route go-to's quote plans to place, or nil with no route or
  no perception to read the light with."
  [c {:keys [name]}]
  (let [r (:route (await (ctx/quote c 'jobs.movement.go-to {:place name :range 0})))]
    (when (:found r) (:dark r))))

(defn ^:async lit-route?
  "Whether go-to's route to place crosses at most max-dark-cells dark cells; a route too dark is quoted again after
  one look round per body cell (cells it has not seen read dark at night)."
  [c place]
  (let [lit? #(some-> % (<= (max-dark-cells)))
        d (await (route-dark c place))
        cell (sh/feet (:primitives c))]
    (cond
      (lit? d) true
      (or (nil? d) (= cell (:route-looked (ctx/mem c)))) false
      :else (do (ctx/update-mem! c assoc :route-looked cell)
                (await (sense/look-round! c []))
                (boolean (lit? (await (route-dark c place))))))))

(defn ^:async roofed-place!
  "The nearest of places (roofed-places-in, nearest first) reached over a lit route (lit-route?), as {:name :pos}, or
  nil."
  [c places]
  (loop [[place & more] places]
    (when place
      (if (await (lit-route? c place)) place (recur more)))))

(defn near-roofed-home
  "The nearest roofed place of :roofed-places within :home-radius, lit or not, as {:name :pos}, or nil."
  [c]
  (first (roofed-places-in c (:home-radius (:args c)))))

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

(defn retry-after-ms [] (settings/get settings :jobs.survival.night.sites/retry-after-ms))

(defn max-retries [] (settings/get settings :jobs.survival.night.sites/max-retries))

(defn retry-due?
  "Whether the exposed hold digs in again: under max-retries, and the body has moved off the spot it went exposed on
  or retry-after-ms passed."
  [c]
  (let [{:keys [pos at retries]} (:exposed (ctx/mem c))]
    (and pos (< (or retries 0) (max-retries))
         (or (>= (u/dist (sh/feet (:primitives c)) pos) (inc (dig-in/futile-radius)))
             (>= (- (ctx/now c) at) (retry-after-ms))))))

(defn max-flees [] (settings/get settings :jobs.survival.night.sites/max-flees))

(defn ground-candidates [] (settings/get settings :jobs.survival.night.sites/ground-candidates))

(defn cave-candidates [] (settings/get settings :jobs.survival.night.sites/cave-candidates))

(defn cave-scan [] (settings/get settings :jobs.survival.night.sites/cave-scan))

