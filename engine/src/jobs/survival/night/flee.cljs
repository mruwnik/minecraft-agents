(ns jobs.survival.night.flee
  "Fleeing: when no shelter can be dug here, walk to somewhere safer (a bed, a roofed place, a cave or overhang, or natural ground)."
  (:require [jobs.lib.shelter.fill :as fill]
            [jobs.lib.cost.health :as cost-health]
            [jobs.lib.choose :as choose]
            [clojure.set :as set]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.clock :as clock]
            [jobs.lib.cost :as cost]
            [jobs.lib.expiry :as expiry]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.look :as look]
            [jobs.lib.rooms :as room-lib]
            [jobs.lib.seal :as seal]
            [jobs.lib.sense :as sense]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.solid :as solid]
            [jobs.lib.walk-pick :as pick]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-pit-site :as pit-site]
            [jobs.survival.night.hold :as hold]
            [jobs.survival.night.sites :as sites]
            [jobs.survival.pit-cells :as pit]
            [jobs.survival.respond.state :as flight]))

(defn ground-sites
  "Natural ground among the seen solids [{:name :pos}] that the carried tools dig and that is no sand or gravel, as the
  feet cells over it ({:kind :ground :pos}); whether a pit can be dug there (ground-ok?) is asked only when its turn comes."
  [p solids]
  (let [^js by-name (js/Map.)
        ground? (fn [name]
                  (when-not (.has by-name name)
                    (.set by-name name (and (rules/natural? name) (not (rules/falling? name)) (not (bs/bad-floor name)) (tools/can-harvest? p name))))
                  (.get by-name name))]
    (->> solids
         (filter #(ground? (:name %)))
         (map (fn [b] {:kind :ground :pos (update (:pos b) :y inc)})))))

(defn ground-ok?
  "Whether a pit can be dug at ground site x (pit-site/pit-site?), clear of failed sites, and a pit fits there by what the
  body has sensed (pit-cells/pit-plan, else a stop pit-cells/stop-of gives only for a cell not sensed)."
  [c {:keys [pos]}]
  (and (pit-site/pit-site? (:primitives c) pos) (pit-site/clear-of-failed? c pos)
       (let [blocks (flight/blocks c)]
         (boolean (or (pit/pit-plan c pos blocks) (:unseen (pit/stop-of c blocks pos)))))))

(defn site-open?
  "The cheap tests of site x, asked of every site before the search (the dear ones, ground-ok? and cave-needed, only of
  its pick): clear of failed sites, and ground where a pit looks diggable (pit-site/pit-site?), a cave dry at both cells."
  [c {:keys [kind pos]}]
  (let [p (:primitives c)]
    (and (pit-site/clear-of-failed? c pos)
         (case kind
           :ground (pit-site/pit-site? p pos)
           :cave (and (not (dig-cells/wet? p pos)) (not (dig-cells/wet? p (update pos :y inc))))
           true))))

(defn cave-sites
  "Feet cells over the seen solids [{:name :pos}] under a roof (a seen solid block 2..roof-height above), with two seen
  free cells, as {:kind :cave :pos}; whether they are dry and dig-in's :enclose walls one in (cave-needed) is asked only
  when its turn comes."
  [p solids]
  (let [roof-height (sh-sense/roof-height)
        ;; cells less than 1024 apart on each axis (a memory-radius sphere and the roofs over it) key apart
        key-of (fn [x y z] (bit-or (bit-shift-left (bit-and x 1023) 20) (bit-shift-left (bit-and y 1023) 10) (bit-and z 1023)))
        ^js solid (js/Set.)
        _ (doseq [{{:keys [x y z]} :pos} solids] (.add solid (key-of x y z)))
        seen-free? (fn [{:keys [x y z] :as cell}] (and (not (.has solid (key-of x y z)))
                                                       (let [b (look/seen-block p cell)] (and (not (:unknown b)) (solid/room? (:name b))))))]
    (->> solids
         (keep (fn [{{:keys [x y z]} :pos name :name}]
                 (let [f {:x x :y (inc y) :z z}]
                   (when (and (not (bs/bad-floor name))
                              (loop [dy 2] (and (<= dy roof-height) (or (.has solid (key-of x (+ y 1 dy) z)) (recur (inc dy)))))
                              (seen-free? f) (seen-free? (update f :y inc)))
                     {:kind :cave :pos f})))))))

(defonce ^:private site-lists (js/WeakMap.))

(defn- sites-key
  "What the seen sites within radius rest on: what the body remembers (sense/known-memory), where it stands and what it
  carries; nil without a memory."
  [p radius]
  (let [^js st (some-> (aget p "perception") :st)
        memory (sense/known-memory p)
        ^js pos (.-pos (.self p))]
    (when memory [(aget p "seenBlocks") (.-dim st) memory [(.-x pos) (.-y pos) (.-z pos)] radius (sh-sense/roof-height)
                  (into #{} (map :name) (u/inventory p))])))

(defn sites
  "cave-sites then ground-sites of the seen solids within radius of the body, kept while sites-key holds: memory changes
  far less often than flee-target asks."
  [p radius]
  (let [^js st (some-> (aget p "perception") :st)
        k (sites-key p radius)
        hit (when k (.get site-lists st))]
    (if (and k (= k (:k hit)))
      (:sites hit)
      (let [solids (look/seen-blocks p {:match solid/solid? :radius radius :max ##Inf})
            v (into (vec (cave-sites p solids)) (ground-sites p solids))]
        (when k (.set site-lists st {:k k :sites v}))
        v))))

(defonce ^:private rejections (js/WeakMap.))

(defn- rejections-of
  "The atom of the sites flee-target's searches picked and passed over (a site's own test failed, or go-to priced no
  walk) of the body p: {[kind pos] {:memory :until}}, each counting while what the body remembers near the site (sense/site-memory)
  holds, for wsearch/search-max-age-ms (the land it read may have changed)."
  [p]
  (or (.get rejections p) (let [a (atom {})] (.set rejections p a) a)))

(defn rejected?
  "Whether flee-target's search passed over site {:kind :pos} and that still counts (rejections-of)."
  [c {:keys [kind pos]}]
  (let [p (:primitives c)
        r (get @(rejections-of p) [kind pos])]
    (boolean (and r (= (:memory r) (sense/site-memory p pos)) (expiry/current? (ctx/now c) r)))))

(defn reject!
  "Pass over site {:kind :pos} in flee-target's searches while what the body remembers near it holds, for
  wsearch/search-max-age-ms; drops the rejections that ran out."
  [c {:keys [kind pos]}]
  (let [p (:primitives c)
        now (ctx/now c)]
    (swap! (rejections-of p) #(assoc (into {} (filter (fn [[_ r]] (expiry/current? now r))) %)
                                     [kind pos] (expiry/stamp {:memory (sense/site-memory p pos)} now wsearch/search-max-age-ms)))))

(defonce ^:private searches (js/WeakMap.))

(defn- search-state
  "The atom of walk-pick/searched!'s progress over the sites of the body p, kept while sites-key holds, for
  wsearch/search-max-age-ms."
  [c p]
  (let [k (sites-key p look/memory-radius)
        now (ctx/now c)
        hit (.get searches p)]
    (if (and k (= k (:k hit)) (expiry/current? now hit))
      (:state hit)
      (let [a (atom {})]
        (when k (.set searches p (expiry/stamp {:k k :state a} now wsearch/search-max-age-ms)))
        a))))

(defn cave-needed
  "The wall cells dig-in's :enclose still has to fill to wall in the cave cell f (an unseen cell counts open), when they
  are dry, clear of failed sites, no more than the blocks carried, and none a zone or claim refuses; else nil."
  [c f]
  (let [p (:primitives c)
        have (reduce + (map :count (lb/carried c (choose/cheap-blocks (choose/filler-names c :shelter) (sites/wall-value)))))
        up (fn [{:keys [x y z]} dy] {:x x :y (+ y dy) :z z})
        sealed? (fn [cell] (and (dig-cells/known-solid? p cell) (fill/sealed? p cell)))]
    (when (and (not (dig-cells/wet? p f)) (not (dig-cells/wet? p (up f 1))) (pit-site/clear-of-failed? c f))
      (let [cells (fill/fill-cells sealed? f nil #(fill/stand-on? p %))]
        (when (and (<= (count cells) have) (not-any? #(access/trespass-refusal c :place %) cells))
          (count cells))))))

(defn flee-args [{:keys [kind name pos]}]
  (case kind
    :roofed-place {:place name :range 0}
    {:pos pos :range 0}))

(defn ^:async flee-target
  "The place not yet tried tonight, somewhere safer than here (see the doc), the body walks to soonest by one search over
  them all, then priced by go-to's quote of that walk alone (jobs.lib.walk-pick/searched!; a site's own tests, ground-ok?
  and cave-needed, asked of the pick alone, a pick that fails them or that go-to prices no walk to is passed over
  (reject!) and the search goes on without it; quotes shared with the pass, pick/quote-once!), as the candidate with its
  :distance, :cost (seconds) and go-to's :route; :searching while the search or a quote goes on or this call's budget
  (cost/search-budget-s, Q141) is spent, or, with survey?, while the body looks round first; nil when there is none
  (none after sites/max-flees). Candidates: the home at any distance (quoted on its own once the search has answered,
  the cheaper of it and the searched pick taken), caves and ground seen within look/memory-radius (a bed is the night's
  bed way, priced there)."
  [c survey?]
  (let [m (ctx/mem c)
        p (:primitives c)
        tried (set (:flee-failed m))]
    (when (< (or (:flees m) 0) (sites/max-flees))
      (let [here (sh-sense/feet p)
            untried (partial remove #(contains? tried (:pos %)))
            places (untried (map #(assoc % :kind :roofed-place) (room-lib/roofed-places-in c [:home] ##Inf)))
            needed (atom {})
            ok? (fn [{:keys [kind pos] :as x}]
                  (case kind
                    :ground (ground-ok? c x)
                    :cave (some? (get (swap! needed #(if (contains? % pos) % (assoc % pos (cave-needed c pos)))) pos))
                    true))
            target (fn [{:keys [kind pos]}] {:pos pos :range (:range (flee-args {:kind kind}))})
            walk #(assoc (flee-args %) :floor cost-health/spend-down :worth ##Inf)
            t0 (clock/now)
            site (await (pick/searched! c (filter #(site-open? c %) (untried (sites p look/memory-radius)))
                                        {:target target :walk walk :ok? ok? :quote pick/quote-once! :state (search-state c p)
                                         :rejected? #(rejected? c %) :reject! #(reject! c %)}))
            ;; the home at any distance is quoted on its own (past a search's nodes go-to still plans the long walk), in
            ;; this call when a quote's round still fits its budget (Q141)
            home (cond
                   (or (= :searching site) (empty? places)) nil
                   (cost/search-pays? (/ (- (clock/now) t0) 1000) (/ wsearch/round-ms 1000) (cost/search-budget-s))
                   (await (pick/cheapest! c places {:target target :walk walk :quote pick/quote-once!}))
                   :else :searching)
            best (if (some #{:searching} [home site]) :searching (first (sort-by :price (filter map? [home site]))))]
        (cond
          (map? best) (let [{:keys [kind pos] :as t} (:pick best)]
                        (cond-> (assoc t :distance (u/dist here pos) :cost (:price best) :route (get-in best [:quote :route]))
                          (= :cave kind) (assoc :needed (get @needed pos))))
          best best
          (and survey? (not (look/surveyed? c))) (do (await (look/survey! c)) ; a cave or roofed place behind the body is not seen until it looks
                                       :searching))))))

(defn ^:async flee!
  "Walk (go-to :flee) to target t, a flee-target; :again. Resumes after a cut (:fleeing). A walk that ends writes the
  target into :flee-failed (never the same twice), counts a flee and, arrived or not, emits shelter.fled or
  shelter.flee_failed; the body moved, so the niche may be tried again."
  [c t]
  (hold/busy! c)
  (ctx/update-mem! c assoc :fleeing t)
  (let [w (await (ctx/call c :flee 'jobs.movement.go-to (cond-> (assoc (flee-args t) :floor cost-health/spend-down) (:worth t) (assoc :worth (:worth t)))))]
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

