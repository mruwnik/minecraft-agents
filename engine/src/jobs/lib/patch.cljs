(ns jobs.lib.patch
  "Patch scan: where a w x h patch of ground would lie, priced in seconds: its crops' worth less the earthwork, light, water and
  walk it takes (jobs.farm.find-spot's doc says how). Reads blocks the body has seen; it never walks, digs or marks anything."
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
            [jobs.lib.crops :as crops]
            [jobs.lib.farm :as farm]
            [jobs.lib.torch-plan :as torch-plan]))

(a/defargs settings
  {::sky-extra {:default 8 :doc "Extra blocks of sky checked above a spot." :spec (a/int-in 0 nil)}
   ::read-budget {:default 4096 :doc "blockAt reads after which a round stops scanning (at the end of the row it is in)." :spec (a/int-in 1 nil)}
   ::max-reads {:default 45000 :doc "blockAt reads one attempt may spend on scanning (a cut and restart keeps the count); then it settles for the best spots found." :spec (a/int-in 1 nil)}})

(defn commonest
  "The most frequent value of xs; ties go to the lowest."
  [xs]
  (->> (frequencies xs)
       (sort-by (fn [[v n]] [(- n) v]))
       ffirst))

(def field-crop "The crop a patch is priced for: wheat, the one whose seed is had without a field." "wheat")

(defn cell-worth
  "Seconds one lit field cell is worth: cost/sown-worth of field-crop on wet or dry farmland."
  [wet?]
  (cost/sown-worth field-crop (crops/stages-per-s wet? nil)))

(defn torch-cells
  "Crop cells one torch lights to crops/grow-light: those within the steps its light falls before it is under it."
  []
  (let [s (- (inc (torch-plan/reach nil)) crops/grow-light)]
    (inc (* 2 s (inc s)))))

(defn water-s
  "Seconds bringing water to n dry cells costs: for each bucket of water their sources need (one wets the farm
  water-reach square round it) a trip and fetch-s, the walk to the water and back, the irrigate step's measured work, and
  once bucket-s, the price of the bucket when none is carried."
  [n fetch-s bucket-s]
  (let [r (farm/water-reach)
        sources (js/Math.ceil (/ n (dec (* (inc (* 2 r)) (inc (* 2 r))))))]
    (+ bucket-s (* sources (+ (cost/trip) fetch-s)) (cost/field-step-s :irrigate sources))))

(defn field-price
  "The dearest-paying way to grow field-crop on cells [{:lit? :wet? :farmable?}] (a cell may carry :p-farm, the one chance its soil, room and light let it be planted, counted as that share of each case): {:worth :costs {:light :water :replace}
  :net :lit? :watered?}. Dark cells grow only when torched (torch-cells a torch, cost/light-cost with torches carried), dry
  ones grow slower unless water is brought (water-s, fetch-s the walk to water and back, bucket-s a bucket's price; nil fetch-s: none is known, so none is brought), a cell a hoe cannot till only when its top is dug out and filled with
  dirt (two blocks of cost/earthwork-s); each is done when it pays. w-wet is a wet cell's worth, a dry one that share of
  it cell-worth gives."
  ([cells torches w-wet] (field-price cells torches w-wet 0 0))
  ([cells torches w-wet fetch-s] (field-price cells torches w-wet fetch-s 0))
  ([cells torches w-wet fetch-s bucket-s]
  (let [either (fn [v p] (if p [[true p] [false (- 1 p)]] [[v 1]]))
        counts (reduce (fn [m {:keys [lit? wet? farmable? p-farm]}]
                         (reduce (fn [m [f pf]] (update m [lit? wet? f] (fnil + 0) pf)) m (either farmable? p-farm)))
                       {} cells)
        n (fn [pred] (reduce + 0 (keep (fn [[k v]] (when (apply pred k) v)) counts)))
        w-dry (* w-wet (/ (cell-worth false) (cell-worth true)))
        option (fn [light? water? replace?]
                 (let [grows? (fn [l f] (and (or l light?) (or f replace?)))
                       wet (n (fn [l w f] (and (grows? l f) w)))
                       dry (n (fn [l w f] (and (grows? l f) (not w))))
                       dark (n (fn [l _ f] (and (grows? l f) (not l))))
                       bare (n (fn [l _ f] (and (grows? l f) (not f))))
                       costs {:light (* (js/Math.ceil (/ dark (torch-cells))) (:cost (cost/light-cost {:carried torches})))
                              :water (if (and water? (pos? dry)) (water-s dry fetch-s bucket-s) 0)
                              :replace (cost/earthwork-s (* 2 bare))}
                       worth (if water? (* w-wet (+ wet dry)) (+ (* w-wet wet) (* w-dry dry)))]
                   {:worth worth :costs costs :net (- worth (reduce + (vals costs))) :lit? (pos? dark) :watered? (and water? (pos? dry))}))]
    (reduce (fn [best o] (if (> (:net o) (:net best)) o best))
            (for [light? [false true] water? (if fetch-s [false true] [false]) replace? [false true]] (option light? water? replace?))))))

(defn walk-s
  "Seconds the walk of away blocks from the centre costs."
  [away]
  (:cost (cost/walk-cost {:distance away})))

(defn score-patch
  "Price a patch {:tops [y ...] :cells [{:lit? :wet? :farmable?} ...] :away dist :sky bool :torches n :worth s :fetch-s s :bucket-s s :fill n :unseen n}, nil when any top is
  nil: :fill is the blocks of water to fill under its cells (earthwork-s on top of the levelling), :unseen the cells not seen (their tops are y, their cells weighed by the chance each can be planted); :score is its :worth (field-price, each wet cell :worth over the cells, nil: cell-worth; with no :cells, a patch for no crops, :worth whole) less the earthwork of levelling it
  (cost/earthwork-s) and the walk to it, in seconds. A patch for no crops and no :worth is an order with no cap: :worth nil, scored by its costs alone, refused only
  when they are ##Inf. Any other patch scoring 0 or less has :why: :far (its worth pays for the levelling,
  not the walk too), :steep (it pays for no levelling) or :barren (it pays for nothing: dark or untillable). Open sky is
  reported, not priced: crops need light, which the cells say."
  [{:keys [tops cells sky away torches fetch-s bucket-s fill unseen] whole :worth}]
  (when-not (some nil? tops)
    (let [y (commonest tops)
          work (reduce + (map #(js/Math.abs (- % y)) tops))
          level (js/Math.round (/ (* 100 (count (filter #(= y %) tops))) (count tops)))
          away (or away 0)
          field (when (seq cells) (field-price cells (or torches 0) (if whole (/ whole (count cells)) (cell-worth true)) fetch-s (or bucket-s 0)))
          costs (merge (:costs field) {:earthwork (cost/earthwork-s (+ work (or fill 0))) :walk (walk-s away)})
          worth (if field (:worth field) whole)
          net (if field (:net field) worth)
          score (- (or worth 0) (reduce + (vals costs)))
          n (count tops)]
      (cond-> {:y y :level level :work work :water-share (if field (/ (count (filter :wet? cells)) n) 0) :sky (boolean sky)
               :away (js/Math.round away) :worth (some-> worth js/Math.round) :costs (update-vals costs js/Math.round)
               :score (js/Math.round score) :dark? (boolean (:lit? field))}
        field (assoc :farmable (js/Math.round (/ (* 100 (count (filter :farmable? cells))) n)))
        (pos? (or unseen 0)) (assoc :unseen unseen)
        (pos? (or fill 0)) (assoc :fill fill)
        (if worth (<= score 0) (= ##-Inf score)) (assoc :why (cond (pos? (- net (:earthwork costs))) :far
                                       (pos? net) :steep
                                       :else :barren))))))

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

(defn crop-lit-fn
  "(fn [x y z]) whether the cell over the top y of a column gets crops/grow-light (block light, or sky light undarkened: the clock does not decide); true without a world; an unseen cell is dark (cand counts an unseen cell's light inside its plantable chance)."
  [p]
  (let [raw (look/raw-of p)]
    (fn [x y z]
      (or (nil? raw)
          (when-let [packed (look/known-light-at p x (inc y) z)]
            (<= crops/grow-light (max (bit-and packed 15) (bit-shift-right packed 4))))))))

(defn column-lack
  "What the column x z has in place of ground when surface/column-top finds none: {:kind :water :y surface :depth blocks of
  water seen down from it}, {:kind :ice :y}, {:kind :hard} (lava, magma, or open air seen all the way down: no spot) or
  {:kind :unseen} (a cell of it not seen, or rock with no air seen over it)."
  [name-fn x z from-y depth]
  (let [low (inc (- from-y depth))]
    (loop [y (+ from-y depth) gap? false]
      (let [n (when (>= y low) (name-fn {:x x :y y :z z}))]
        (cond
          (< y low) {:kind (if gap? :unseen :hard)}
          (nil? n) (recur (dec y) true)
          (or (open? n) (surface/canopy? n)) (recur (dec y) gap?)
          (= "water" n) {:kind :water :y y
                         :depth (count (take-while #(= "water" (name-fn {:x x :y % :z z})) (range y (dec low) -1)))}
          (contains? #{"ice" "frosted_ice"} n) {:kind :ice :y y}
          (surface/no-floor n) {:kind :hard}
          :else {:kind :unseen})))))

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
  "The most score a patch of n cells (whole: its :worth, as score-patch) whose centre lies ring or more cells from the centre can have: every cell lit and
  farmable (none when it grows no crops) with no earthwork; wet from wet-ring on (the first such ring with seen water
  in reach, nil for none), else the better of water brought (from beside the patch; none when no water is known) and dry; less the walk to the ring."
  [n farmland? whole wet-ring ring water?]
  (let [walk #(walk-s (max 0 (dec %)))
        w-wet (if whole (/ whole n) (cell-worth true))]
    (if-not farmland?
      (- (or whole 0) (walk ring))
      (max (- (:net (field-price (repeat n {:lit? true :wet? false :farmable? true}) 0 w-wet (when water? 0))) (walk ring))
           (if wet-ring (- (* n w-wet) (walk wet-ring)) ##-Inf)))))

(defn wet-ring-fn
  "(fn [ring]) the first ring from ring on whose patches of the larger side size could have water of waters (cells) in
  reach, nil for none."
  [waters size from]
  (let [out (+ (farm/water-reach) (quot (inc size) 2) 1)
        dists (map (fn [[x _ z]] (max (js/Math.abs (- x (:x from))) (js/Math.abs (- z (:z from))))) waters)]
    (fn [ring]
      (some->> (seq (keep (fn [d] (when (>= (+ d out) ring) (max ring (- d out)))) dists))
               (apply min)))))

(defn nearest-miss
  "Of misses (nils skipped), the one with the fewest cells short, then the nearest."
  [misses]
  (first (sort-by (juxt :short :away) (remove nil? misses))))

(defn scan-rows
  "Scan patches ring by ring (nearest the centre first) from (:next-ring state)
  until the read count reaches budget, the last ring is done, or :limit good
  spots are in hand (the ring it is in is finished first). Returns {:next-ring
  :found :miss :done :reads :cache}; found is the best :limit spots so far, miss the patch with
  the fewest cells short of one (lava or magma on top, another's, off height, too dark or untillable; a :far one none), nil when none. The column
  caches (:cache) are passed back in the state, so a later slice reads no column twice."
  [p {:keys [w h range depth limit blocked? farmland? torches worth bucket-s] limit-step :max-step :or {farmland? true} :as a} from {:keys [next-ring found miss cache]} budget]
  (let [reads (volatile! 0)
        name-fn (fn [pos] (vswap! reads inc) (u/seen-name p pos))
        waters (->> (look/seen-blocks p {:names ["water"] :radius (+ range w h 4) :max 4096 :live? true})
                    (into #{} (map (fn [{{:keys [x y z]} :pos}] [x y z]))))
        tops (volatile! (:tops cache {}))
        skies (volatile! (:skies cache {}))
        grounds (volatile! (:grounds cache {}))
        lackss (volatile! (:lacks cache {}))
        memo (fn [store f]
               (fn [& k] (if-let [e (find @store k)]
                           (val e)
                           (let [r (apply f k)] (vswap! store assoc k r) r))))
        top-of (memo tops (fn [x z] (surface/column-top name-fn x z (:y from) depth)))
        lack-of (memo lackss (fn [x z] (column-lack name-fn x z (:y from) depth)))
        sky-of (memo skies (fn [x z t] (sky-above? name-fn x z t (:y from) depth)))
        farmable-at? (memo grounds (fn [x z t] (contains? bs/farmable-ground (name-fn {:x x :y t :z z}))))
        lit? (crop-lit-fn p)
        wet-ring (wet-ring-fn waters (max w h) from)
        wet? (memo (volatile! {}) (fn [x z y] (farm/hydrated? waters x y z)))
        fetch-s (fn [x z] (when (seq waters)
                            (let [cx (+ x (/ (dec w) 2)) cz (+ z (/ (dec h) 2))]
                              (* 2 (walk-s (reduce min (map (fn [[wx _ wz]] (js/Math.hypot (- wx cx) (- wz cz))) waters)))))))
        fx (:x from) fz (:z from)
        by-ring (corners-by-ring a from)
        rings (sort (keys by-ring))
        away-of (fn [x z] (js/Math.hypot (- (+ x (/ (dec w) 2)) fx) (- (+ z (/ (dec h) 2)) fz)))
        cand (fn [[x z]]
               (when (let [cx (+ x (quot w 2)) cz (+ z (quot h 2))]
                 (or (some? (top-of cx cz)) (contains? #{:water :ice} (:kind (lack-of cx cz)))))
                 (let [cols (for [dx (clojure.core/range w) dz (clojure.core/range h)] [(+ x dx) (+ z dz)])
                       seen (mapv (fn [[cx cz]] (top-of cx cz)) cols)
                       lacks (mapv (fn [[cx cz] t] (when (nil? t) (lack-of cx cz))) cols seen)
                       hard (keep (fn [[col l]] (when (= :hard (:kind l)) col)) (map vector cols lacks))
                       ys (remove nil? (map (fn [t l] (or t (:y l))) seen lacks))
                       base (commonest ys)
                       tops (mapv (fn [t l] (or t (:y l) base)) seen lacks)
                       fill (reduce + 0 (keep #(when (= :water (:kind %)) (:depth %)) lacks))
                       unseen (count (filter #(= :unseen (:kind %)) lacks))
                       foreign (count (filter (fn [[[cx cz] t]] (and blocked? (blocked? cx t cz))) (map vector cols tops)))
                       miss (fn [m] (merge {:miss true :pos {:x x :y base :z z} :w w :h h
                                            :bare [] :dark 0 :foreign 0 :away (js/Math.round (away-of x z))}
                                           m))]
                   (if (pos? (+ (count hard) foreign))
                     (miss {:short (+ (count hard) foreign) :bare (vec hard) :foreign foreign})
                     (let [cells (when farmland?
                                   (mapv (fn [[cx cz] t l]
                                           (case (:kind l)
                                             :unseen {:lit? true :wet? false :farmable? false :p-farm (cost/spot-plantable-chance)}
                                             :ice {:lit? (lit? cx t cz) :wet? false :farmable? false}
                                             {:lit? (lit? cx t cz) :wet? (and (nil? l) (wet? cx cz t))
                                              :farmable? (or (some? l) (farmable-at? cx cz t))}))
                                         cols tops lacks))
                           sky (every? true? (map (fn [[cx cz] t] (sky-of cx cz t)) cols tops))
                           s (score-patch {:tops tops :cells cells :sky sky :torches torches :worth worth :bucket-s bucket-s :fill fill :unseen unseen :away (away-of x z) :fetch-s (when farmland? (fetch-s x z))})
                           step (max-step tops h)]
                       (if (and limit-step (> step limit-step))
                         (miss {:short (count (remove #(= (:y s) %) tops)) :over-step step})
                         (case (:why s)
                           nil (assoc s :step step :w w :h h :pos {:x x :y (:y s) :z z})
                           :far (miss {:short 0 :far (:away s)})
                           :steep (let [off (count (remove #(= (:y s) %) tops))] (miss {:short off :steep off}))
                           :barren (let [dark (count (remove :lit? cells)) poor (count (remove :farmable? cells))]
                                     (miss {:short (if cells (max dark poor) (count tops)) :dark dark :unfarmable poor})))))))))]
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
            (and (= limit (count found)) (<= (js/Math.round (best-possible (* w h) farmland? worth (wet-ring (inc r)) (inc r) (seq waters))) (:score (peek found)))) {:next-ring (inc r) :found found :miss miss :done true :reads @reads}
            (>= @reads budget) {:next-ring (inc r) :found found :miss miss :done false :reads @reads :cache {:tops @tops :skies @skies :grounds @grounds :lacks @lackss}}
            :else (recur (rest rs) found miss)))))))

(defn scan
  "The best :limit spots [{:pos :score :level ...}] for the args around from, in
  one unbounded pass."
  [p a from]
  (:found (scan-rows p a from {:next-ring 0 :found []} ##Inf)))


(defn lacks
  "A miss of scan-rows with its :bare columns (lava, magma and the like on top) counted as :no-floor."
  [miss]
  (-> (dissoc miss :bare :miss)
      (assoc :no-floor (count (:bare miss)))))

(defn lacks-text
  "What a miss of lacks lacks, e.g. \"40 of 256 cells short: 6 lava or magma, 4 another's plan, zone or claim\"."
  [{:keys [w h short no-floor dark unfarmable foreign steep over-step far]}]
  (if far
    (str "the best patch lies " far " blocks off: its crops do not pay for the walk")
    (str short " of " (* w h) " cells short: "
         (str/join ", " (keep (fn [[n what]] (when (pos? n) (str n " " what)))
                              [[no-floor "lava or magma"] [foreign "another's plan, zone or claim"]
                               [(if over-step short 0) (str "off the patch's height, with a step of " over-step " over the most allowed")]
                               [steep "off the patch's height, dearer to level than the crops are worth"]
                               [dark "too dark, dearer to light than the crops are worth"]
                               [unfarmable "untillable, dearer to replace than the crops are worth"]])))))

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
                                       (update :patch-miss #(nearest-miss [% (lacks (:miss r))])))))
          (not-empty (:found r)))
      (do (ctx/update-mem! c assoc :patch-scan (assoc (select-keys r [:next-ring :found :miss :cache]) :from from :reads spent))
          :again))))

(defn water-unseen?
  "Whether a scan would run with no water in sight, so a look around could still show some."
  [c {:keys [range w h]}]
  (empty? (look/seen-blocks (:primitives c) {:names ["water"] :radius (+ range w h 4) :max 1})))
