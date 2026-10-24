(ns jobs.survival.dig-pit-site
  "Where dig-pit digs a night pit when the feet are no site: failed sites (shut-in's :shut-in-failed entries) and the nearest ground a pit can be dug at."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.rooms :as rooms]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.solid :as solid]
            [jobs.lib.threats :as threats]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.shut-in-night :as shut-night]))

(a/defargs settings
  {::reach {:default 16 :doc "How far (blocks) a night pit looks for ground to dig in when the feet are no site." :spec (a/int-in 0 nil)}
   ::water-radius {:default 3 :doc "Blocks around a pit that failed for fluid beside it that count as the same site: a player steps a little way from the water, not out of the room." :spec (a/int-in 0 nil)}})

(defn failed
  "Tonight's failed sites [{:pos :way :reason}]."
  [c]
  (mapv :data (ctx/entries c :shut-in-failed)))

(defn site-radius
  "How far around a failed site (an entry of failed) another pit counts as the same site."
  [entry]
  (min (shut-night/futile-radius) (if (= :fluid-adjacent (:reason entry)) (settings/get settings ::water-radius) (shut-night/futile-radius))))

(defn failed-here?
  "Whether a site that failed tonight (for way, when given) lies within shut-in's futile radius of the feet."
  [c & [way]]
  (let [here (sh-sense/feet (:primitives c))]
    (boolean (some #(and (or (nil? way) (= way (:way %))) (<= (u/dist here (:pos %)) (site-radius %))) (failed c)))))

(def exposed-policy
  "The :night-exposed entries {:pos}: where a night ended with no shelter; forgotten with the failed sites when the night ends."
  {:cap 3 :ttl :forever})

(defn exposed-here?
  "Whether the night ended exposed within shut-in's futile radius of the feet: every way failed there, so it is not tried again."
  [c]
  (let [here (sh-sense/feet (:primitives c))]
    (boolean (some #(<= (u/dist here (:pos (:data %))) (shut-night/futile-radius)) (ctx/entries c :night-exposed)))))

(defn clear-of-failed?
  "Whether feet cell f is at least site-radius+1 from every failed site."
  [c f]
  (every? #(>= (u/dist (:pos %) f) (inc (site-radius %))) (failed c)))

(defn pit-site?
  "Whether feet cell f looks like a place a pit can be dug, by what a player sees from the surface: standing room, dry
  ground the carried tools dig, no fluid beside it. How deep the ground goes is left to the pit, which stops a bad site."
  [p {:keys [x y z] :as f}]
  (let [at (fn [dy] (u/seen-name p {:x x :y (+ y dy) :z z}))
        below {:x x :y (dec y) :z z}]
    (and (solid/solid? (at -1)) (tools/can-harvest? p (at -1)) (not (solid/solid? (at 0))) (not (solid/solid? (at 1)))
         (not (dig-cells/wet? p f)) (not (dig-cells/wet? p {:x x :y (inc y) :z z}))
         (not (dig-cells/wet? p below))
         (not (dig-cells/lateral-fluid p below)))))

(defn zone-refused?
  "Whether another's zone, claim or plan refuses the pit under feet cell f (three cells down) or the block at f."
  [c {:keys [x y z]}]
  (let [in (access/rules-input c)]
    (boolean (or (some #(access/trespass-refusal in :dig {:x x :y (- y %) :z z}) [1 2 3])
                 (access/trespass-refusal (assoc in :feet nil) :place {:x x :y y :z z})))))

(defn pit-start
  "The top of the pit the body stands in (a failed site's :start above the feet, within the futile radius), or nil."
  [c]
  (let [feet (sh-sense/feet (:primitives c))]
    (some #(when (and (:start %) (< (:y feet) (:y (:start %))) (<= (u/dist feet (:start %)) (site-radius %))) (:start %))
          (failed c))))

(defn relocation-site
  "The nearest feet cell within ::reach of the top of the pit the body is in, else of the failed site it is at, else of its feet, clear of every failed site, where a pit can be dug (pit-site?), outside the full-cost radius of every known
  mob the body would flee (threats/danger-near-fn), or nil. With :free?, only cells no zone, claim or plan refuses."
  [c & [{:keys [free?]}]]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        reach (settings/get settings ::reach)
        here (:pos (first (filter #(and (= :pit (:way %)) (<= (u/dist feet (:pos %)) (site-radius %))) (failed c))))
        danger-near (threats/danger-near-fn c)
        start (pit-start c)
        {:keys [x z]} (or start here feet)
        y (:y (or start feet))
        offsets (sort-by (fn [[dx dz]] (+ (* dx dx) (* dz dz)))
                         (for [dx (range (- reach) (inc reach))
                               dz (range (- reach) (inc reach))
                               :when (<= (+ (* dx dx) (* dz dz)) (* reach reach))]
                           [dx dz]))]
    (some (fn [[dx dz]]
            (some (fn [dy] (let [f {:x (+ x dx) :y (+ y dy) :z (+ z dz)}]
                             (when (and (clear-of-failed? c f) (pit-site? p f) (not (danger-near f)) (not (and free? (zone-refused? c f)))) f)))
                  [0 -1 1 -2 2]))
          offsets)))

(defn ^:async keeps?
  "Whether a site cell m chosen earlier is still the one to walk to, given fresh (site, from the feet now): the same cell; or
  go-to's quote for m is finite and no dearer than its quote for the fresh cell (none: m stays if it can be walked to), unless
  the feet are a pit site themselves (a walk that left the failed site behind on good ground digs there, for nothing)."
  [c m fresh]
  (cond
    (and (= :here (:at fresh)) (pit-site? (:primitives c) (sh-sense/feet (:primitives c)))) false
    (= m (:cell fresh)) true
    :else (let [qm (await (rooms/route-cost c {:pos m :range 0}))
                qf (when (:cell fresh) (await (rooms/route-cost c {:pos (:cell fresh) :range 0})))]
            (boolean (and qm (or (nil? qf) (<= qm qf)))))))

(defn site
  "Where a night pit is dug from: {:at :here}, {:at :there :cell f} (walk there first: a pit failed at the feet, or a zone
  refuses them and a free site is near), or nil with none."
  [c]
  (let [feet (sh-sense/feet (:primitives c))
        failed? (failed-here? c :pit)]
    (if (or failed? (zone-refused? c feet))
      (if-let [f (relocation-site c {:free? (not failed?)})]
        {:at :there :cell f}
        (when-not failed? {:at :here}))
      {:at :here})))
