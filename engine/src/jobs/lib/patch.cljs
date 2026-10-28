(ns jobs.lib.patch
  "Patch scan: where a w x h patch of ground would lie, ranked by flatness, water, open sky and nearness (jobs.farm.find-spot's
  doc says how a score is made). Reads blocks the body has seen; it never walks, digs or marks anything."
  (:require [clojure.string :as str]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.surface :as surface :refer [open?]]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.look :as look]
            [jobs.lib.world :as known]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.util :as u]
            [jobs.lib.cost :as cost]
            [jobs.lib.farm :as farm]))

(a/defargs settings
  {::sky-extra {:default 8 :doc "Extra blocks of sky checked above a spot." :spec (a/int-in 0 nil)}
   ::read-budget {:default 4096 :doc "blockAt reads after which a round stops scanning (at the end of the row it is in)." :spec (a/int-in 1 nil)}
   ::max-reads {:default 45000 :doc "blockAt reads one attempt may spend on scanning (a cut and restart keeps the count); then it settles for the best spots found." :spec (a/int-in 1 nil)}
   ::water-weight {:default 25 :doc "Score points for a patch wholly within reach of water." :spec (a/num-in 0 nil)}
   ::point-blocks {:default 4 :doc "Blocks walked (jobs.lib.cost/per-block each) that one score point is worth in seconds: the price of earthwork and of the walk from the centre in points." :spec (a/num-in 0.1 nil)}
   ::farmable-weight {:default 40 :doc "Score points lost when no cell of a patch can be made farmland." :spec (a/num-in 0 nil)}})

(defn commonest
  "The most frequent value of xs; ties go to the lowest."
  [xs]
  (->> (frequencies xs)
       (sort-by (fn [[v n]] [(- n) v]))
       ffirst))

(defn points
  "Score points a number of seconds costs."
  [seconds]
  (/ seconds (* (settings/get settings ::point-blocks) (cost/per-block))))

(defn away-points
  "Score points the walk of away blocks from the centre costs."
  [away]
  (points (:cost (cost/walk-cost {:distance away}))))

(defn score-patch
  "Score a patch {:tops [y ...] :water-share 0..1 :sky bool :away dist}, nil
  when any top is nil. Earthwork and the walk are priced in seconds (jobs.lib.cost) and not capped: a patch dearer to level than it is
  worth scores under 0. Open sky is reported, not scored: crops need light, which the scan checks cell by cell."
  [{:keys [tops water-share sky away farmable]}]
  (when-not (some nil? tops)
    (let [y (commonest tops)
          work (reduce + (map #(js/Math.abs (- % y)) tops))
          level (js/Math.round (/ (* 100 (count (filter #(= y %) tops))) (count tops)))
          away (or away 0)
          score (- (+ level (js/Math.round (* (settings/get settings ::water-weight) water-share)))
                   (js/Math.round (points (cost/earthwork-s work)))
                   (js/Math.round (away-points away))
                   (if farmable (js/Math.round (* (settings/get settings ::farmable-weight) (- 1 farmable))) 0))]
      (cond-> {:y y :level level :work work :water-share water-share :sky (boolean sky)
               :away (js/Math.round away) :score score}
        farmable (assoc :farmable (js/Math.round (* 100 farmable)))))))

(defn max-step
  "The largest height difference between neighbouring cells of a patch whose tops run x-major, h cells per column."
  [tops h]
  (let [v (vec tops)
        d (fn [i j] (js/Math.abs (- (v i) (v j))))
        n (count v)]
    (reduce max 0 (concat (for [i (range (dec n)) :when (pos? (mod (inc i) h))] (d i (inc i)))
                          (for [i (range (- n h))] (d i (+ i h)))))))

(defn blocked-fn
  "A fn [x y z] -> true for a cell under a plan's footprint (any y of its column) or in a zone or claim of another owner.
  in is {:footprints :zones :claims :self :now}."
  [{:keys [footprints zones claims self now]}]
  (let [cols (into #{} (map (fn [[x _ z]] [x z])) (keys footprints))]
    (fn [x y z]
      (boolean (or (contains? cols [x z])
                   (zones/foreign-holder {:zones zones :claims claims :self self :now now :cell [x y z]}))))))

(defn read-budget [] (settings/get settings ::read-budget))

(defn max-reads [] (settings/get settings ::max-reads))

(defn sky-extra [] (settings/get settings ::sky-extra))

(defn sky-above?
  "True when every cell above top up to (+ from-y depth sky-extra) is air or
  unloaded (nil)."
  [name-fn x z top from-y depth]
  (loop [y (inc top)]
    (or (> y (+ from-y depth (sky-extra)))
        (let [n (name-fn {:x x :y y :z z})]
          (when (or (nil? n) (open? n))
            (recur (inc y)))))))

(def crop-light "The light a crop needs (block light, or sky light undarkened: the clock does not decide)." 9)

(def dark-penalty "Score a spot loses when it has to be lit with torches first." 15)

(defn crop-lit-fn
  "(fn [x y z]) whether the cell over the top y of a column gets crop-light; true where the body has no light data there."
  [p]
  (let [raw (look/raw-of p)]
    (fn [x y z]
      (let [y (inc y)]
        (or (nil? raw)
            (not (look/light-known? raw x y z))
            (let [packed (.lightAt ^js raw x y z)]
              (<= crop-light (max (bit-and packed 15) (bit-shift-right packed 4)))))))))

(defn patch-ring
  "How many cells out from the centre of the patch with NW corner x z lies (the larger of its x and z offsets, rounded up)."
  [{:keys [w h]} from x z]
  (long (js/Math.ceil (max (js/Math.abs (- (+ x (/ (dec w) 2)) (:x from)))
                           (js/Math.abs (- (+ z (/ (dec h) 2)) (:z from)))))))

(defn coarse-step
  "The gap between tried corners: big patches overlap so much that every fourth corner finds the same ground."
  [{:keys [w h]}]
  (max 1 (quot (min w h) 4)))

(defn corners-by-ring
  "{ring [[x z] ...]} for every tried NW corner of a patch within :range of from."
  [{:keys [w h range] :as a} from]
  (let [s (coarse-step a)
        fx (:x from) fz (:z from)]
    (->> (for [x (clojure.core/range (- fx range) (inc (- (+ fx range) w)) s)
               z (clojure.core/range (- fz range) (inc (- (+ fz range) h)) s)]
           [x z])
         (group-by (fn [[x z]] (patch-ring a from x z))))))

(defn best-possible
  "The most score a patch whose centre lies ring cells from the centre can have: level 100, all the water share when water is in
  range, no earthwork, every cell farmable, less the walk to its ring."
  [wet-needed? ring]
  (- (+ 100 (if wet-needed? (settings/get settings ::water-weight) 0))
     (js/Math.round (away-points ring))))

(defn nearest-miss
  "Of misses (nils skipped), the one with the fewest cells short, then the nearest."
  [misses]
  (first (sort-by (juxt :short :away) (remove nil? misses))))

(defn scan-rows
  "Scan patches ring by ring (nearest the centre first) from (:next-ring state)
  until the read count reaches budget, the last ring is done, or :limit good
  spots are in hand (the ring it is in is finished first). Returns {:next-ring
  :found :miss :done :reads :cache}; found is the best :limit spots so far, miss the patch with
  the fewest cells short of one (no seen ground, too dark, another's), nil when none. The column
  caches (:cache) are passed back in the state, so a later slice reads no column twice."
  [p {:keys [w h range depth limit blocked? farmland? dark-ok?] limit-step :max-step :or {farmland? true} :as a} from {:keys [next-ring found miss cache]} budget]
  (let [reads (volatile! 0)
        name-fn (fn [pos] (vswap! reads inc) (u/seen-name p pos))
        waters (->> (look/seen-blocks p {:names ["water"] :radius (+ range w h 4) :max 4096 :live? true})
                    (into #{} (map (fn [{{:keys [x y z]} :pos}] [x y z]))))
        tops (volatile! (:tops cache {}))
        skies (volatile! (:skies cache {}))
        memo (fn [store f]
               (fn [& k] (if-let [e (find @store k)]
                           (val e)
                           (let [r (apply f k)] (vswap! store assoc k r) r))))
        top-of (memo tops (fn [x z] (surface/column-top name-fn x z (:y from) depth)))
        sky-of (memo skies (fn [x z t] (sky-above? name-fn x z t (:y from) depth)))
        lit? (crop-lit-fn p)
        wet? (fn [x z y] (farm/hydrated? waters x y z))
        fx (:x from) fz (:z from)
        by-ring (corners-by-ring a from)
        rings (sort (keys by-ring))
        cand (fn [[x z]]
               (when (some? (top-of (+ x (quot w 2)) (+ z (quot h 2))))
                 (let [cols (for [dx (clojure.core/range w) dz (clojure.core/range h)] [(+ x dx) (+ z dz)])
                       tops (mapv (fn [[cx cz]] (top-of cx cz)) cols)
                       bare (keep (fn [[col t]] (when (nil? t) col)) (map vector cols tops))
                       dark (if dark-ok? 0 (count (filter (fn [[[cx cz] t]] (and t (not (lit? cx t cz)))) (map vector cols tops))))
                       foreign (count (filter (fn [[[cx cz] t]] (and t blocked? (blocked? cx t cz))) (map vector cols tops)))]
                   (if (pos? (+ (count bare) dark foreign))
                     {:miss true :pos {:x x :y (or (some-> (seq (remove nil? tops)) commonest) (:y from)) :z z} :w w :h h
                      :short (+ (count bare) dark foreign) :bare bare :dark dark :foreign foreign
                      :away (js/Math.round (js/Math.hypot (- (+ x (/ (dec w) 2)) fx) (- (+ z (/ (dec h) 2)) fz)))}
                     (let [wetn (count (filter true? (map (fn [[cx cz] t] (wet? cx cz t)) cols tops)))
                           sky (every? true? (map (fn [[cx cz] t] (sky-of cx cz t)) cols tops))
                           farm (when farmland? (count (filter true? (map (fn [[cx cz] t] (contains? bs/farmable-ground (name-fn {:x cx :y t :z cz}))) cols tops))))
                           s (score-patch {:tops tops :water-share (/ wetn (count cols)) :sky sky :farmable (some-> farm (/ (count cols)))
                                           :away (js/Math.hypot (- (+ x (/ (dec w) 2)) fx) (- (+ z (/ (dec h) 2)) fz))})]
                       (when s
                         (let [step (max-step tops h)]
                           (when (or (nil? limit-step) (<= step limit-step))
                             (let [dark? (not (every? true? (map (fn [[cx cz] t] (lit? cx t cz)) cols tops)))
                                   spot (cond-> (assoc s :step step :w w :h h :pos {:x x :y (:y s) :z z} :dark? dark?)
                                          dark? (update :score - dark-penalty))]
                               (if (neg? (:score spot))
                                 {:miss true :pos (:pos spot) :w w :h h :short (count (remove #(= (:y s) %) tops)) :steep (count (remove #(= (:y s) %) tops))
                                  :bare [] :dark 0 :foreign 0 :away (:away spot)}
                                 spot))))))))))]
    (loop [rs (drop-while #(< % next-ring) rings) found found miss miss]
      (if (empty? rs)
        {:next-ring ##Inf :found found :miss miss :done true :reads @reads}
        (let [r (first rs)
              {spots false misses true} (group-by (comp boolean :miss) (keep cand (by-ring r)))
              found (->> (concat found spots)
                         (sort-by (juxt (comp - :score) :away))
                         (take limit)
                         vec)
              miss (nearest-miss (cons miss misses))]
          (cond
            (and (= limit (count found)) (<= (best-possible (seq waters) (inc r)) (:score (peek found)))) {:next-ring (inc r) :found found :miss miss :done true :reads @reads}
            (>= @reads budget) {:next-ring (inc r) :found found :miss miss :done false :reads @reads :cache {:tops @tops :skies @skies}}
            :else (recur (rest rs) found miss)))))))

(defn scan
  "The best :limit spots [{:pos :score :level ...}] for the args around from, in
  one unbounded pass."
  [p a from]
  (:found (scan-rows p a from {:next-ring 0 :found []} ##Inf)))


(defn lacks
  "A miss of scan-rows with its columns of no seen ground split into :no-floor (water, lava or ice on top) and :unseen."
  [p {:keys [bare] :as miss} from-y depth]
  (let [floorless? (fn [[x z]] (some #(contains? surface/no-floor (u/seen-name p {:x x :y % :z z}))
                                     (range (+ from-y depth) (- from-y depth) -1)))
        n (count (filter floorless? bare))]
    (-> (dissoc miss :bare :miss)
        (assoc :no-floor n :unseen (- (count bare) n)))))

(defn lacks-text
  "What a miss of lacks lacks, e.g. \"40 of 256 cells short: 30 unseen, 6 water/lava/ice, 4 dark\"."
  [{:keys [w h short unseen no-floor dark foreign steep]}]
  (str short " of " (* w h) " cells short: "
       (str/join ", " (keep (fn [[n what]] (when (pos? n) (str n " " what)))
                            [[unseen "unseen"] [no-floor "water/lava/ice"] [dark "too dark for crops"] [foreign "another's plan, zone or claim"] [steep "off the patch's height, dearer to level than the patch is worth"]]))))

(defn scan-step!
  "One bounded slice of a scan of the patch args a round from (the state is kept in memory, so a cut resumes it, and its centre stays
  the first one). :again while rows remain, else the best spots found (nil for none)."
  [c a from]
  (let [saved (:patch-scan (ctx/mem c))
        from (or (:from saved) from)
        state (or saved {:next-ring 0 :found []})
        a (assoc a :blocked? (blocked-fn {:footprints (known/footprints c {:except (:plan a)}) :zones (known/zones c) :claims (known/claims c)
                                          :self (ctx/self-name c) :now (ctx/now c)}))
        r (scan-rows (:primitives c) a from state (read-budget))
        spent (+ (:reads state 0) (:reads r))]
    (if (or (:done r) (>= spent (max-reads)))
      (do (ctx/update-mem! c (fn [m] (cond-> (dissoc m :patch-scan)
                                       (and (empty? (:found r)) (:miss r))
                                       (update :patch-miss #(nearest-miss [% (lacks (:primitives c) (:miss r) (:y from) (:depth a))])))))
          (not-empty (:found r)))
      (do (ctx/update-mem! c assoc :patch-scan (assoc (select-keys r [:next-ring :found :miss :cache]) :from from :reads spent))
          :again))))

(defn water-unseen?
  "Whether a scan would run with no water in sight, so a look around could still show some."
  [c {:keys [range w h]}]
  (empty? (look/seen-blocks (:primitives c) {:names ["water"] :radius (+ range w h 4) :max 1})))
