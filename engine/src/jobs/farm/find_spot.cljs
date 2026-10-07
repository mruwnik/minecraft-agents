(ns jobs.farm.find-spot
  (:require [engine.ctx :as ctx]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]))

(def doc
  "Pick where a :w x :h farm would go. Ranks patches by flatness first, then water within 4,
  open sky and nearness to :center. It tries patches nearest-first and stops once :limit flat open-sky ones are found (with water within 4, when water is in range).
  A patch of 8 or more is tried at every fourth corner. Reads blocks only: it never digs or marks anything.
  A scan reads at most about 30000 blocks, then settles for the best spots so far.
  With :walk true it then walks to the best spot.
  Result: {:spot pos-or-nil :spots [...] :walked bool}. A pos is the north-west corner at ground level.
  :reason is :none (no patch found) or :unreachable (walk failed).")

(def args
  {:w {:doc "patch width (x), 1 to 16" :type :int :min 1 :max 16 :default 5}
   :h {:doc "patch height (z), 1 to 16" :type :int :min 1 :max 16 :default 5}
   :range {:doc "patches lie within this many blocks of :center in x and z, 1 to 48" :type :int :min 1 :max 48 :default 24}
   :center {:doc "where to search from {:x :y :z}; nil is the body's cell" :type :pos :default nil}
   :depth {:doc "how far above and below the centre's y to look for ground, 1 to 16" :type :int :min 1 :max 16 :default 12}
   :limit {:doc "how many spots to keep, 1 to 8" :type :int :min 1 :max 8 :default 3}
   :walk {:doc "walk to the best spot (finding and walking are separate; a parent can walk)" :default false}})

(def air #{"air" "cave_air" "void_air"})
(def no-floor #{"water" "lava" "bubble_column" "ice" "frosted_ice" "magma_block" "powder_snow"})
(def water-reach 4)

(defn column-top
  "The y of the surface of column x z: the highest non-air block scanning from
  (+ from-y depth) down to (- (+ from-y 1) depth), when air lies above it. nil
  for an unloaded cell on the way, a fluid, ice or magma surface, nothing, or a
  covered top. name-fn takes {:x :y :z} and returns a block name or nil."
  [name-fn x z from-y depth]
  (let [low (inc (- from-y depth))]
    (loop [y (+ from-y depth)]
      (when (>= y low)
        (let [n (name-fn {:x x :y y :z z})]
          (cond
            (nil? n) nil
            (air n) (recur (dec y))
            (no-floor n) nil
            :else (when-let [above (name-fn {:x x :y (inc y) :z z})]
                    (when (air above) y))))))))

(defn commonest
  "The most frequent value of xs; ties go to the lowest."
  [xs]
  (->> (frequencies xs)
       (sort-by (fn [[v n]] [(- n) v]))
       ffirst))

(defn score-patch
  "Score a patch {:tops [y ...] :water-share 0..1 :sky bool :away dist}, nil
  when any top is nil."
  [{:keys [tops water-share sky away]}]
  (when-not (some nil? tops)
    (let [y (commonest tops)
          work (reduce + (map #(js/Math.abs (- % y)) tops))
          level (js/Math.round (/ (* 100 (count (filter #(= y %) tops))) (count tops)))
          away (or away 0)
          score (- (+ level (js/Math.round (* 25 water-share)) (if sky 15 0))
                   (min 40 work)
                   (min 30 (js/Math.round (/ away 4))))]
      {:y y :level level :work work :water-share water-share :sky (boolean sky)
       :away (js/Math.round away) :score score})))

(defn hydrated?
  "True when a water cell of water-set (a set of [x y z]) lies within 4 in x and
  z of the farmland at x y z, at its y or one above."
  [water-set x y z]
  (boolean (some (fn [[wx wy wz]]
                   (and (<= (js/Math.abs (- wx x)) water-reach)
                        (<= (js/Math.abs (- wz z)) water-reach)
                        (or (= wy y) (= wy (inc y)))))
                 water-set)))

(def read-budget
  "blockAt reads after which a round stops scanning (at the end of the row it is in)."
  4096)

(def max-reads
  "blockAt reads one attempt may spend on scanning (a cut and restart keeps the count); then it settles for the best spots found."
  30000)

(def sky-extra 8)

(defn sky-above?
  "True when every cell above top up to (+ from-y depth sky-extra) is air or
  unloaded (nil)."
  [name-fn x z top from-y depth]
  (loop [y (inc top)]
    (or (> y (+ from-y depth sky-extra))
        (let [n (name-fn {:x x :y y :z z})]
          (when (or (nil? n) (air n))
            (recur (inc y)))))))

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
  :found :done :reads}; found is the best :limit spots so far. Caches live within
  this call."
  [p {:keys [w h range depth limit] :as a} from {:keys [next-ring found]} budget]
  (let [reads (volatile! 0)
        name-fn (fn [pos] (vswap! reads inc) (u/seen-name p pos))
        waters (->> (look/seen-blocks p {:names ["water"] :radius (+ range w h 4) :max 4096 :live? true})
                    (into #{} (map (fn [{{:keys [x y z]} :pos}] [x y z]))))
        memo (fn [f] (let [cache (volatile! {})]
                       (fn [& k] (if-let [e (find @cache k)]
                                   (val e)
                                   (let [r (apply f k)] (vswap! cache assoc k r) r)))))
        top-of (memo (fn [x z] (column-top name-fn x z (:y from) depth)))
        sky-of (memo (fn [x z t] (sky-above? name-fn x z t (:y from) depth)))
        wet? (memo (fn [x z y] (hydrated? waters x y z)))
        fx (:x from) fz (:z from)
        by-ring (corners-by-ring a from)
        rings (sort (keys by-ring))
        cand (fn [[x z]]
               (when (some? (top-of (+ x (quot w 2)) (+ z (quot h 2))))
                 (let [cols (for [dx (clojure.core/range w) dz (clojure.core/range h)] [(+ x dx) (+ z dz)])
                       tops (mapv (fn [[cx cz]] (top-of cx cz)) cols)]
                   (when (not-any? nil? tops)
                     (let [wetn (count (filter true? (map (fn [[cx cz] t] (wet? cx cz t)) cols tops)))
                           sky (every? true? (map (fn [[cx cz] t] (sky-of cx cz t)) cols tops))
                           s (score-patch {:tops tops :water-share (/ wetn (count cols)) :sky sky
                                           :away (js/Math.hypot (- (+ x (/ (dec w) 2)) fx) (- (+ z (/ (dec h) 2)) fz))})]
                       (when s (assoc s :pos {:x x :y (:y s) :z z})))))))]
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
            (>= @reads budget) {:next-ring (inc r) :found found :done false :reads @reads}
            :else (recur (rest rs) found)))))))

(defn scan
  "The best :limit spots [{:pos :score :level ...}] for the args around from, in
  one unbounded pass."
  [p a from]
  (:found (scan-rows p a from {:next-ring 0 :found []} ##Inf)))

(defn check [_c] true)

(defn finish-scan!
  "Remember and announce the found spots in memory (dropping the scan state);
  nil (after a warn) when there are none."
  [c a from found]
  (ctx/update-mem! c dissoc :scan)
  (if (empty? found)
    (do (ctx/emit! c :find-spot.none :warn
                   {:text (str "no " (:w a) "x" (:h a) " patch of open ground within " (:range a) " of " (pr-str from))})
        nil)
    (do (ctx/update-mem! c assoc :spots found)
        (ctx/emit! c :find-spot.found :info
                   {:spots (count found) :best (:pos (first found)) :score (:score (first found))
                    :text (str (count found) " spots, best " (pr-str (:pos (first found))) " score " (:score (first found)))})
        found)))

(defn scan-step!
  "One bounded scan slice. :again while rows remain (state kept in :scan),
  else the finished spots (or nil for none)."
  [c a]
  (let [saved (:scan (ctx/mem c))
        from (or (:center a) (:from saved) (into {} (map (fn [[k v]] [k (js/Math.floor v)])) (u/self-pos c)))
        state (or saved {:next-ring 0 :found []})
        r (scan-rows (:primitives c) a from state read-budget)
        spent (+ (:reads state 0) (:reads r))]
    (if (or (:done r) (>= spent max-reads))
      (finish-scan! c a from (:found r))
      (do (ctx/update-mem! c assoc :scan (assoc (select-keys r [:next-ring :found]) :from from :reads spent))
          :again))))

(defn ^:async step
  "Scan (a bounded slice per step, remembering the spots), then walk to the
  best one if :walk."
  [c]
  (let [a (merge (into {} (map (fn [[k v]] [k (:default v)])) args) (:args c))
        found (or (:spots (ctx/mem c)) (scan-step! c a))]
    (cond
      (= :again found) :again
      (nil? found) (do (ctx/result! c {:spot nil :reason :none}) :done)
      :else
      (let [pos (:pos (first found))
            result {:spot pos :spots found}]
        (if-not (:walk a)
          (do (ctx/result! c (assoc result :walked false)) :done)
          (case (await (near/go-near! c (update pos :y inc) 2 {:zone-tolls true}))
            :partial :continue
            :blocked (do (ctx/result! c (assoc result :walked false :reason :unreachable)) :done)
            (do (ctx/result! c (assoc result :walked true)) :done)))))))

(defn ^:async round
  "The whole attempt: scan slice after slice, then the walk; :continue only while the walk waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
