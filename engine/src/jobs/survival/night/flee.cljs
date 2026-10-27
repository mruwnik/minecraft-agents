(ns jobs.survival.night.flee
  "Fleeing: when no shelter can be dug here, walk to somewhere safer (a bed, a roofed place, a cave or overhang, or natural ground)."
  (:require [jobs.lib.cost.health :as cost-health]
            [jobs.lib.choose :as choose]
            [clojure.set :as set]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.look :as look]
            [jobs.lib.rooms :as room-lib]
            [jobs.lib.seal :as seal]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.solid :as solid]
            [jobs.lib.walk-pick :as pick]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.bed-down :as bed-down]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-pit-site :as pit-site]
            [jobs.survival.night.hold :as hold]
            [jobs.survival.night.sites :as sites]))

(defn ground-sites
  "Natural ground among the seen solids [{:name :pos}] that the carried tools dig and that is no sand or gravel, as the
  feet cells over it ({:kind :ground :pos}); whether a pit can be dug there (ground-ok?) is asked only when its turn comes."
  [p solids]
  (let [^js by-name (js/Map.)
        ground? (fn [name]
                  (when-not (.has by-name name)
                    (.set by-name name (and (rules/natural? name) (not (rules/falling? name)) (tools/can-harvest? p name))))
                  (.get by-name name))]
    (->> solids
         (filter #(ground? (:name %)))
         (map (fn [b] {:kind :ground :pos (update (:pos b) :y inc)})))))

(defn ground-ok?
  "Whether a pit can be dug at ground site x (pit-site/pit-site?) and it is clear of failed sites."
  [c {:keys [pos]}]
  (and (pit-site/pit-site? (:primitives c) pos) (pit-site/clear-of-failed? c pos)))

(defn cave-sites
  "Feet cells over the seen solids [{:name :pos}] under a roof (a seen solid block 2..roof-height above), with two seen
  free cells, as {:kind :cave :pos}; whether they are dry and dig-in's :enclose walls one in (cave-needed) is asked only
  when its turn comes."
  [p solids]
  (let [roof-height (sh-sense/roof-height)
        ;; cells less than 1024 apart on each axis (a flee-radius sphere and the roofs over it) key apart
        key-of (fn [x y z] (bit-or (bit-shift-left (bit-and x 1023) 20) (bit-shift-left (bit-and y 1023) 10) (bit-and z 1023)))
        ^js solid (js/Set.)
        _ (doseq [{{:keys [x y z]} :pos} solids] (.add solid (key-of x y z)))
        seen-free? (fn [{:keys [x y z] :as cell}] (and (not (.has solid (key-of x y z)))
                                                       (not (:unknown (look/seen-block p cell)))))]
    (->> solids
         (keep (fn [{{:keys [x y z]} :pos}]
                 (let [f {:x x :y (inc y) :z z}]
                   (when (and (loop [dy 2] (and (<= dy roof-height) (or (.has solid (key-of x (+ y 1 dy) z)) (recur (inc dy)))))
                              (seen-free? f) (seen-free? (update f :y inc)))
                     {:kind :cave :pos f})))))))

(defonce ^:private site-lists (js/WeakMap.))

(defn sites
  "cave-sites then ground-sites of the seen solids within radius of the body, kept while what it remembers
  (sense/known-memory), where it stands and what it carries hold: memory changes far less often than flee-target asks."
  [p radius]
  (let [^js st (some-> (aget p "perception") :st)
        memory (sense/known-memory p)
        ^js pos (.-pos (.self p))
        k (when memory [(aget p "seenBlocks") (.-dim st) memory [(.-x pos) (.-y pos) (.-z pos)] radius (sh-sense/roof-height)
                        (into #{} (map :name) (u/inventory p))])
        hit (when k (.get site-lists st))]
    (if (and k (= k (:k hit)))
      (:sites hit)
      (let [solids (look/seen-blocks p {:match solid/solid? :radius radius :max ##Inf})
            v (into (vec (cave-sites p solids)) (ground-sites p solids))]
        (when k (.set site-lists st {:k k :sites v}))
        v))))

(defn cave-needed
  "The wall cells dig-in's :enclose still has to fill to wall in the cave cell f (an unseen cell counts open), when they
  are dry, clear of failed sites, no more than the blocks carried, and none a zone or claim refuses; else nil."
  [c f]
  (let [p (:primitives c)
        have (reduce + (map :count (lb/carried c (choose/cheap-blocks (choose/filler-names c :shelter) (sites/wall-value)))))
        up (fn [{:keys [x y z]} dy] {:x x :y (+ y dy) :z z})
        sealed? (fn [cell] (and (dig-cells/known-solid? p cell) (seal/sealed? p cell)))]
    (when (and (not (dig-cells/wet? p f)) (not (dig-cells/wet? p (up f 1))) (pit-site/clear-of-failed? c f))
      (let [cells (dig-cells/fill-cells sealed? f nil #(dig-cells/stand-on? p %))]
        (when (and (<= (count cells) have) (not-any? #(access/trespass-refusal c :place %) cells))
          (count cells))))))

(defn flee-args [{:keys [kind name pos]}]
  (case kind
    :roofed-place {:place name :range 0}
    :bed {:pos pos :range 2}
    {:pos pos :range 0}))

(defn ^:async flee-target
  "The place not yet tried tonight, somewhere safer than here (see the doc), whose walk go-to quotes cheapest (jobs.lib.walk-pick/
  cheapest!: every candidate within flee-radius is ranked by the least its straight walk takes and quoted best-first, as the flight itself
  will walk, until the best quoted is no dearer than the next bound; a site's own tests, ground-ok? and cave-needed, asked only when its
  turn comes; quotes shared with the pass, pick/quote-once!), as the candidate with its :distance and :cost (seconds); :searching
  while a quote's search goes on, or while the body looks round first; nil when there is none (none with the flee-radius setting 0 or
  after sites/max-flees). Candidates: the bed, :roofed-places, caves, ground."
  [c]
  (let [m (ctx/mem c)
        p (:primitives c)
        flee-radius (sites/flee-radius)
        tried (set (:flee-failed m))]
    (when (and (pos? flee-radius) (< (or (:flees m) 0) (sites/max-flees)))
      (let [here (sh-sense/feet p)
            bed (when-let [pos (sh-beds/bed-to-use p (ctx/view c) flee-radius (bed-down/bed-permit c))]
                  (when (< (bed-down/radius c) (u/dist here pos) (inc flee-radius)) [{:kind :bed :pos pos}]))
            places (map #(assoc % :kind :roofed-place) (room-lib/roofed-places-in c (:args c) flee-radius))
            cands (->> (concat bed places (sites p flee-radius))
                       (remove #(contains? tried (:pos %))))
            needed (atom {})
            ok? (fn [{:keys [kind pos] :as x}]
                  (case kind
                    :ground (ground-ok? c x)
                    :cave (some? (get (swap! needed #(if (contains? % pos) % (assoc % pos (cave-needed c pos)))) pos))
                    true))
            walk #(assoc (flee-args %) :floor cost-health/spend-down)
            best (await (pick/cheapest! c cands {:target (fn [{:keys [kind pos]}] {:pos pos :range (:range (flee-args {:kind kind}))})
                                                 :walk walk :ok? ok? :quote pick/quote-once!}))]
        (cond
          (map? best) (let [{:keys [kind pos] :as t} (:pick best)]
                        (cond-> (assoc t :distance (u/dist here pos) :cost (:price best))
                          (= :cave kind) (assoc :needed (get @needed pos))))
          best best
          (not (look/surveyed? c)) (do (await (look/survey! c)) ; a cave or roofed place behind the body is not seen until it looks
                                       :searching))))))

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
          (let [stranded? (= :stranded (contract/cause-key w))]
            (ctx/emit! c :shelter.flee_failed :info (assoc (set/rename-keys (select-keys t [:kind :pos :name]) {:kind :target})
                                                           :stranded stranded?
                                                           :text (if stranded?
                                                                   "could not shelter here; the walk to somewhere safer costs more than staying, so the body stays and seals in again"
                                                                   "could not shelter here and could not reach somewhere safer")))))
        :again))))

