(ns jobs.survival.night.sites
  "The night's sites: failed sites tonight, where a pit can be dug, roofed places, and the retry rules of an exposed hold."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.solid :as solid]
            [jobs.lib.threats :as threats]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.shut-in-night :as shut-night]
            [jobs.survival.dig-in-cells :as dig-cells]
            [clojure.string :as str]))

(def site-policy
  "The :night-site entries: failed sites, cleared at the morning the night job sees; a day at most."
  {:cap 8 :ttl sh-clock/ms-per-day})

(a/defargs settings
  {:jobs.survival.night.sites/home-radius {:default 12 :doc "A remembered roofed place (:roofed-places) this close is walked into, whatever the light, before a bed is used or put down; a carried bed is never put down in the open while one is." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/max-sites {:default 4 :doc "Failed sites a night tries before holding exposed." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/relocate-reach {:default 16 :doc "How far (blocks) a night looks for a spot to dig in." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/retry-after-ms {:default 660000 :doc "How long a body holds exposed before it tries to dig in again (past shut-in's own 10-minute give-up)." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/max-retries {:default 2 :doc "Dig-in retries after a night went exposed." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/max-flees {:default 3 :doc "Walks to somewhere safer a night tries." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/ground-candidates {:default 16 :doc "Seen ground cells costed per flee: the nearest ones." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/cave-candidates {:default 16 :doc "Seen cave cells costed per flee: the nearest ones." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/dig-in-seconds {:default 8 :doc "What digging in costs (seconds, as go-to prices a route): a roofed place is walked to when go-to's planned route costs no more." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/cave-scan {:default 2048 :doc "Solid blocks one cave search reads: nearest first, so it reaches about 16 blocks around the body." :spec (a/int-in 0 nil)}})
(defn home-radius [] (settings/get settings :jobs.survival.night.sites/home-radius))
(defn dig-in-seconds [] (settings/get settings :jobs.survival.night.sites/dig-in-seconds))
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
  "The night is over: forget tonight's failed sites and shut-in's own (:shut-in-failed), so the next night tries them again."
  [c]
  (ctx/forget-where! c :night-site (constantly true))
  (ctx/forget-where! c :shut-in-failed (constantly true)))

(defn failed-here?
  "Whether a site that failed tonight lies within shut-in's futile radius of the feet."
  [c]
  (let [here (sh-sense/feet (:primitives c))]
    (boolean (some #(<= (u/dist here %) (shut-night/futile-radius))
                   (map :pos (failed-sites c))))))

(defn pit-site?
  "Whether feet cell f looks like a place a pit can be dug, by what a player sees from the surface: standing room, dry
  ground the carried tools dig, no fluid beside it. How deep the ground goes is left to shut-in, which stops a bad site."
  [p {:keys [x y z] :as f}]
  (let [at (fn [dy] (u/seen-name p {:x x :y (+ y dy) :z z}))
        below {:x x :y (dec y) :z z}]
    (and (solid/solid? (at -1)) (tools/can-harvest? p (at -1)) (not (solid/solid? (at 0))) (not (solid/solid? (at 1)))
         (not (dig-cells/wet? p f)) (not (dig-cells/wet? p {:x x :y (inc y) :z z}))
         (not (dig-cells/wet? p below))
         (not (dig-cells/lateral-fluid p below)))))

(defn clear-of-failed?
  "Whether feet cell f is at least futile-radius+1 from every failed site (tonight's and shut-in's :shut-in-failed entries)."
  [c f]
  (let [failed (concat (map :pos (failed-sites c)) (map (comp :pos :data) (ctx/entries c :shut-in-failed)))]
    (every? #(>= (u/dist % f) (inc (shut-night/futile-radius))) failed)))

(defn zone-refused?
  "Whether another's zone, claim or plan refuses the pit under feet cell f (three cells down) or the block at f."
  [c {:keys [x y z]}]
  (let [in (access/rules-input c)]
    (boolean (or (some #(access/trespass-refusal in :dig {:x x :y (- y %) :z z}) [1 2 3])
                 (access/trespass-refusal (assoc in :feet nil) :place {:x x :y y :z z})))))

(defn relocation-site
  "The nearest feet cell within relocate-reach of the failed site the body is at (its feet when none: a body in its
  pit looks from the surface), at least futile-radius+1 from every failed site (tonight's and shut-in's
  :shut-in-failed entries), where a pit can be dug (pit-site?), outside the full-cost radius of every known mob the body would flee (threats/danger-near-fn: a
  site by a zombie only walks the body back into it), or nil. With :free?, only cells no zone, claim or plan refuses."
  [c & [{:keys [free?]}]]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        here (first (filter #(<= (u/dist feet %) (shut-night/futile-radius)) (map :pos (failed-sites c))))
        danger-near (threats/danger-near-fn c)
        {:keys [x y z]} (or here feet)
        offsets (sort-by (fn [[dx dz]] (+ (* dx dx) (* dz dz)))
                         (for [dx (range (- (relocate-reach)) (inc (relocate-reach)))
                               dz (range (- (relocate-reach)) (inc (relocate-reach)))
                               :when (<= (+ (* dx dx) (* dz dz)) (* (relocate-reach) (relocate-reach)))]
                           [dx dz]))]
    (some (fn [[dx dz]]
            (some (fn [dy] (let [f {:x (+ x dx) :y (+ y dy) :z (+ z dz)}]
                             (when (and (clear-of-failed? c f) (pit-site? p f) (not (danger-near f)) (not (and free? (zone-refused? c f)))) f)))
                  [0 -1 1 -2 2]))
          offsets)))


(defn roofed-places-in
  "The remembered places of :roofed-places within radius and roofed within :roof-height, as [{:name :pos}] nearest
  first. A roof cell the body has not seen is guessed solid (u/block-name-or): from the ground the cell over a home
  hides behind the roof's near edge; a column seen open is no roof."
  [c radius]
  (let [p (:primitives c)
        {:keys [roofed-places roof-height]} (:args c)
        here (sh-sense/feet p)
        roofed? (fn [pos] (some #(solid/solid? (u/block-name-or p (update pos :y + %) "stone")) (range 1 (inc roof-height))))]
    (->> roofed-places
         (keep (fn [nm] (when-let [pos (mem/place (ctx/view c) nm)] {:name nm :pos pos})))
         (filter (fn [{:keys [pos]}] (and (<= (u/dist here pos) radius) (roofed? pos))))
         (sort-by #(u/dist here (:pos %))))))

(defn ^:async route-cost
  "Seconds go-to's quote plans for the route to place, dark cells priced in (the planner's own cost), or nil with no route."
  [c {:keys [name]}]
  (let [r (:route (await (ctx/quote c 'jobs.movement.go-to {:place name :range 0})))]
    (when (:found r) (+ (:seconds r) (or (:dark-seconds r) 0)))))

(defn ^:async cheap-route?
  "Whether go-to's planned route to place costs no more than digging in (dig-in-seconds). Cells the body has not seen read
  dark at night, so a route dearer than that is quoted again after one look round per body cell. A dear route says so
  (info night.roof-walk-declined {:place :seconds :limit}; :seconds nil: no route)."
  [c place]
  (let [cheap? #(some-> % (<= (dig-in-seconds)))
        cost (await (route-cost c place))
        cell (sh-sense/feet (:primitives c))
        cost (if (or (cheap? cost) (nil? cost) (= cell (:route-looked (ctx/mem c))))
               cost
               (do (ctx/update-mem! c assoc :route-looked cell)
                   (await (sense/look-round! c []))
                   (await (route-cost c place))))]
    (or (cheap? cost)
        (do (ctx/emit! c :night.roof-walk-declined :info
                       {:place (:name place) :seconds cost :limit (dig-in-seconds)
                        :text (str "the walk to " (name (:name place)) (if cost (str " costs " (js/Math.round cost) " s") " has no route")
                                   ", more than digging in (" (dig-in-seconds) " s)")})
            false))))

(defn ^:async roofed-place!
  "The nearest of places (roofed-places-in, nearest first) reached over a route no dearer than digging in (cheap-route?), as {:name :pos}, or
  nil."
  [c places]
  (loop [[place & more] places]
    (when place
      (if (await (cheap-route? c place)) place (recur more)))))

(defn near-roofed-home
  "The nearest roofed place of :roofed-places within the ::home-radius setting, lit or not, as {:name :pos}, or nil."
  [c]
  (first (roofed-places-in c (home-radius))))

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
  (let [feet (sh-sense/feet (:primitives c))]
    (first (filter #(and (< (:y feet) (:y %)) (<= (u/dist feet %) (shut-night/futile-radius))) (map :pos (failed-sites c))))))

(defn retry-after-ms [] (settings/get settings :jobs.survival.night.sites/retry-after-ms))

(defn max-retries [] (settings/get settings :jobs.survival.night.sites/max-retries))

(defn retry-due?
  "Whether the exposed hold digs in again: under max-retries, and the body has moved off the spot it went exposed on
  or retry-after-ms passed."
  [c]
  (let [{:keys [pos at retries]} (:exposed (ctx/mem c))]
    (and pos (< (or retries 0) (max-retries))
         (or (>= (u/dist (sh-sense/feet (:primitives c)) pos) (inc (shut-night/futile-radius)))
             (>= (- (ctx/now c) at) (retry-after-ms))))))

(defn max-flees [] (settings/get settings :jobs.survival.night.sites/max-flees))

(defn ground-candidates [] (settings/get settings :jobs.survival.night.sites/ground-candidates))

(defn cave-candidates [] (settings/get settings :jobs.survival.night.sites/cave-candidates))

(defn cave-scan [] (settings/get settings :jobs.survival.night.sites/cave-scan))

