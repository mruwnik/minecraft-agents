(ns jobs.lib.patch
  "Patch scan: where a w x h patch of ground would lie, ranked by flatness, water, open sky and nearness (jobs.farm.find-spot's
  doc says how a score is made). Reads blocks the body has seen; it never walks, digs or marks anything."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.surface :as surface :refer [open?]]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.look :as look]
            [jobs.lib.world :as known]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.util :as u]
            [jobs.lib.farm :as farm]))

(a/defargs settings
  {::sky-extra {:default 8 :doc "Extra blocks of sky checked above a spot." :spec (a/int-in 0 nil)}
   ::read-budget {:default 4096 :doc "blockAt reads after which a round stops scanning (at the end of the row it is in)." :spec (a/int-in 1 nil)}
   ::max-reads {:default 45000 :doc "blockAt reads one attempt may spend on scanning (a cut and restart keeps the count); then it settles for the best spots found." :spec (a/int-in 1 nil)}
   ::water-weight {:default 25 :doc "Score points for a patch wholly within reach of water." :spec (a/num-in 0 nil)}
   ::sky-weight {:default 15 :doc "Score points for open sky over a patch." :spec (a/num-in 0 nil)}
   ::work-cap {:default 40 :doc "Most score points the earthwork (blocks the cells are off the commonest height) costs." :spec (a/num-in 0 nil)}
   ::away-cap {:default 30 :doc "Most score points the distance from the centre costs." :spec (a/num-in 0 nil)}
   ::farmable-weight {:default 40 :doc "Score points lost when no cell of a patch can be made farmland." :spec (a/num-in 0 nil)}})

(defn commonest
  "The most frequent value of xs; ties go to the lowest."
  [xs]
  (->> (frequencies xs)
       (sort-by (fn [[v n]] [(- n) v]))
       ffirst))

(defn score-patch
  "Score a patch {:tops [y ...] :water-share 0..1 :sky bool :away dist}, nil
  when any top is nil."
  [{:keys [tops water-share sky away farmable]}]
  (when-not (some nil? tops)
    (let [y (commonest tops)
          work (reduce + (map #(js/Math.abs (- % y)) tops))
          level (js/Math.round (/ (* 100 (count (filter #(= y %) tops))) (count tops)))
          away (or away 0)
          score (- (+ level (js/Math.round (* (settings/get settings ::water-weight) water-share)) (if sky (settings/get settings ::sky-weight) 0))
                   (min (settings/get settings ::work-cap) work)
                   (min (settings/get settings ::away-cap) (js/Math.round (/ away 4)))
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

(defn good?
  "A spot that needs no earthwork under open sky, and water within 4 when any water is in range: nothing nearer can improve much on it."
  [wet-needed? {:keys [level sky water-share]}]
  (and sky (= 100 level) (or (not wet-needed?) (pos? water-share))))

(defn scan-rows
  "Scan patches ring by ring (nearest the centre first) from (:next-ring state)
  until the read count reaches budget, the last ring is done, or :limit good
  spots are in hand (the ring it is in is finished first). Returns {:next-ring
  :found :done :reads :cache}; found is the best :limit spots so far. The column
  caches (:cache) are passed back in the state, so a later slice reads no column twice."
  [p {:keys [w h range depth limit blocked? farmland?] limit-step :max-step :or {farmland? true} :as a} from {:keys [next-ring found cache]} budget]
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
                       tops (mapv (fn [[cx cz]] (top-of cx cz)) cols)]
                   (when (and (not-any? nil? tops)
                              (every? true? (map (fn [[cx cz] t] (lit? cx t cz)) cols tops))
                              (not (some true? (map (fn [[cx cz] t] (and blocked? (blocked? cx t cz))) cols tops))))
                     (let [wetn (count (filter true? (map (fn [[cx cz] t] (wet? cx cz t)) cols tops)))
                           sky (every? true? (map (fn [[cx cz] t] (sky-of cx cz t)) cols tops))
                           farm (when farmland? (count (filter true? (map (fn [[cx cz] t] (contains? bs/farmable-ground (name-fn {:x cx :y t :z cz}))) cols tops))))
                           s (score-patch {:tops tops :water-share (/ wetn (count cols)) :sky sky :farmable (some-> farm (/ (count cols)))
                                           :away (js/Math.hypot (- (+ x (/ (dec w) 2)) fx) (- (+ z (/ (dec h) 2)) fz))})]
                       (when s
                         (let [step (max-step tops h)]
                           (when (or (nil? limit-step) (<= step limit-step))
                             (assoc s :step step :w w :h h :pos {:x x :y (:y s) :z z})))))))))]
    (loop [rs (drop-while #(< % next-ring) rings) found found]
      (if (empty? rs)
        {:next-ring ##Inf :found found :done true :reads @reads}
        (let [r (first rs)
              found (->> (concat found (keep cand (by-ring r)))
                         (sort-by (juxt (comp - :score) :away))
                         (take limit)
                         vec)]
          (cond
            (and (= limit (count found)) (every? (partial good? (seq waters)) found)) {:next-ring (inc r) :found found :done true :reads @reads}
            (>= @reads budget) {:next-ring (inc r) :found found :done false :reads @reads :cache {:tops @tops :skies @skies}}
            :else (recur (rest rs) found)))))))

(defn scan
  "The best :limit spots [{:pos :score :level ...}] for the args around from, in
  one unbounded pass."
  [p a from]
  (:found (scan-rows p a from {:next-ring 0 :found []} ##Inf)))


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
      (do (ctx/update-mem! c dissoc :patch-scan)
          (not-empty (:found r)))
      (do (ctx/update-mem! c assoc :patch-scan (assoc (select-keys r [:next-ring :found :cache]) :from from :reads spent))
          :again))))

(defn water-unseen?
  "Whether a scan would run with no water in sight, so a look around could still show some."
  [c {:keys [range w h]}]
  (empty? (look/seen-blocks (:primitives c) {:names ["water"] :radius (+ range w h 4) :max 1})))
