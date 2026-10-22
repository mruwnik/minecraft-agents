(ns jobs.survival.night.flee
  "Fleeing: when no shelter can be dug here, walk to somewhere safer (a bed, a roofed place, a cave or overhang, or natural ground)."
  (:require [jobs.lib.cost.health :as cost-health]
            [jobs.lib.choose :as choose]
            [clojure.set :as set]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.look :as look]
            [jobs.lib.rooms :as room-lib]
            [jobs.lib.seal :as seal]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.solid :as solid]
            [jobs.lib.targets :as targets]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.bed-down :as bed-down]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-pit-site :as pit-site]
            [jobs.survival.night.hold :as hold]
            [jobs.survival.night.sites :as sites]))

(defn ground-sites
  "Natural ground the body has seen within radius that the carried tools dig and that is no sand or gravel, as feet
  cells ({:kind :ground :pos}) a pit can be dug at (pit-site/pit-site?), clear of failed sites, the nearest sites/ground-candidates."
  [c radius]
  (let [p (:primitives c)]
    (->> (look/seen-blocks p {:match #(and (solid/solid? %) (rules/natural? %) (not (rules/falling? %)) (tools/can-harvest? p %))
                              :radius (min (sites/scan-radius) radius) :max (sites/scan-radius)})
         (map #(update (:pos %) :y inc))
         (filter #(and (pit-site/pit-site? p %) (pit-site/clear-of-failed? c %)))
         (take (sites/ground-candidates))
         (mapv (fn [pos] {:kind :ground :pos pos})))))

(defn cave-sites
  "Feet cells the body has seen under a roof (a seen solid block 2..roof-height above), over a seen solid floor, with
  two seen free dry cells, clear of failed sites, that dig-in's :enclose can wall in with the blocks carried: each as
  {:kind :cave :pos :needed} (the wall cells not yet sealed; an unseen cell counts open). Nothing needing a block a
  zone or claim refuses. The nearest sites/cave-candidates. Reads seen blocks only: its reach is the nearest sites/cave-scan solids."
  [c radius]
  (let [p (:primitives c)
        roof-height (sh-sense/roof-height)
        solids (look/seen-blocks p {:match solid/solid? :radius (min (sites/scan-radius) radius) :max (sites/cave-scan)})
        key-of (juxt :x :y :z)
        solid? (into #{} (map (comp key-of :pos)) solids)
        seen-free? (fn [cell] (and (not (solid? (key-of cell)))
                                   (not (:unknown (look/seen-block p cell)))))
        have (reduce + (map :count (lb/carried c (choose/cheap-blocks (choose/filler-names c :shelter) (sites/wall-value)))))
        up (fn [{:keys [x y z]} dy] {:x x :y (+ y dy) :z z})
        sealed? (fn [cell] (and (solid? (key-of cell)) (seal/sealed? p cell)))
        stand-on? #(dig-cells/stand-on? p %)]
    (->> solids
         (map #(up (:pos %) 1))
         (filter (fn [f]
                   (and (some #(solid? (key-of (up f %))) (range 2 (inc roof-height)))
                        (seen-free? f) (seen-free? (up f 1))
                        (not (dig-cells/wet? p f)) (not (dig-cells/wet? p (up f 1)))
                        (pit-site/clear-of-failed? c f))))
         (keep (fn [f]
                 (let [cells (dig-cells/fill-cells sealed? f nil stand-on?)]
                   (when (and (<= (count cells) have)
                              (not-any? #(access/trespass-refusal c :place %) cells))
                     {:kind :cave :pos f :needed (count cells)}))))
         (take (sites/cave-candidates))
         vec)))

(defn ^:async flee-target
  "The place not yet tried tonight, somewhere safer than here (see the doc), that go-to reaches soonest (jobs.lib.targets/
  nearest!: one planner search over the candidates, costed as go-to plans: known dangers and darkness), as the candidate
  with its :distance and :cost; :searching while that search goes on; nil when there is none (none with the flee-radius setting 0
  or after sites/max-flees). Candidates: the bed, :roofed-places, caves, ground, the nearest max-targets of them."
  [c]
  (let [m (ctx/mem c)
        p (:primitives c)
        flee-radius (sites/flee-radius)
        tried (set (:flee-failed m))]
    (when (and (pos? flee-radius) (< (or (:flees m) 0) (sites/max-flees)))
      (let [here (sh-sense/feet p)
            bed (when-let [pos (sh-beds/bed-to-use p (ctx/view c) flee-radius (bed-down/bed-permit c))]
                  (when (> (u/dist here pos) (bed-down/radius c)) [{:kind :bed :pos pos}]))
            places (map #(assoc % :kind :roofed-place) (room-lib/roofed-places-in c (:args c) flee-radius))
            cands (->> (concat bed places (cave-sites c flee-radius) (ground-sites c flee-radius))
                       (remove #(contains? tried (:pos %)))
                       (sort-by #(u/dist here (:pos %)))
                       (take targets/max-targets)
                       vec)]
        (cond
          (and (empty? cands) (not (look/surveyed? c)))
          (do (await (look/survey! c)) ; a cave or roofed place behind the body is not seen until it looks
              :searching)

          (seq cands)
          (let [a (await (targets/nearest! c (mapv (fn [{:keys [kind pos]}] (cond-> pos (= :bed kind) (assoc :range 2))) cands) 0
                                           {:tag :night-flee}))]
            (case (:status a)
              :searching :searching
              :found (let [cand (nth cands (:index a))]
                       (cond-> (assoc cand :distance (u/dist here (:pos cand))) (:cost a) (assoc :cost (:cost a))))
              nil)))))))

(defn flee-args [{:keys [kind name pos]}]
  (case kind
    :roofed-place {:place name :range 0}
    :bed {:pos pos :range 2}
    {:pos pos :range 0}))

(defn ^:async flee!
  "Walk (go-to :flee) to target t, a flee-target; :again. Resumes after a cut (:fleeing). A walk that ends writes the
  target into :flee-failed (never the same twice), counts a flee and, arrived or not, emits shelter.fled or
  shelter.flee_failed; the body moved, so the niche may be tried again."
  [c t]
  (hold/busy! c)
  (ctx/update-mem! c assoc :fleeing t)
  (let [w (await (ctx/call c :flee 'jobs.movement.go-to (assoc (flee-args t) :floor cost-health/spend-down)))]
    (if (= :continue w)
      :again
      (let [arrived (:arrived (:data w))]
        (ctx/update-mem! c #(-> % (dissoc :fleeing :niche :no-site :shut-ins) (update :flees (fnil inc 0)) (update :flee-failed (fnil conj []) (:pos t))))
        (when (and arrived (= :cave (:kind t))) (ctx/update-mem! c assoc :enclose (:pos t)))
        (if arrived
          (ctx/emit! c :shelter.fled :info (assoc (set/rename-keys (select-keys t [:kind :pos :name :distance :cost :needed]) {:kind :target})
                                                  :text (str "could not shelter here; moved somewhere safer (" (name (:kind t)) ")")))
          (ctx/emit! c :shelter.flee_failed :info (assoc (set/rename-keys (select-keys t [:kind :pos :name]) {:kind :target})
                                                         :text "could not shelter here and could not reach somewhere safer")))
        :again))))

