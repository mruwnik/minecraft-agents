(ns jobs.farm.find-spot
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.surface :as surface :refer [open?]]
            [jobs.farm.till :as till]
            [jobs.lib.look :as look]
            [jobs.lib.child :as child]
            [jobs.explore.search :as search]
            [jobs.lib.ready :as ready]
            [jobs.lib.world :as known]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.farm :as farm]))

(def doc
  "Pick where a :w x :h farm would go. Ranks patches by flatness first, then water within 4,
  open sky and nearness to :center. It tries patches nearest-first and stops once ::limit flat open-sky ones are found (with water within 4, when water is in range).
  A patch of 8 or more is tried at every fourth corner. Reads blocks only: it never digs or marks anything.
  A scan reads at most ::max-reads blocks, then settles for the best spots so far. It never walks to the spot: a parent does.
  When the scan finds no patch it explores: it walks (jobs.movement.go-to) to the nearest untried leg of explore.search's spiral round
  the start (legs ::leg-spacing apart, within ::max-distance, at most ::max-legs, each on standable seen ground), looks, and scans again
  centred there, until a patch is found or the call's :job-time is spent.
  Ends with data {:spot pos :spots [...]}: done with the spots, or nothing-possible :none with :why :distance (no leg left), :legs
  (::max-legs walked) or :stuck (three walks in a row failed).
  :spots best first up to ::limit, each {:pos :score :level :work :step :water-share :sky :away :y :farmable}; :step is
  the largest height difference between two neighbouring cells of the patch. A pos is the north-west corner at ground level.
  Score = level (% of the patch's cells at its commonest ground height y, 0-100) + ::water-weight x water-share + ::sky-weight for open sky,
  less :work (sum of blocks the other cells are off y, at most ::work-cap), :away / 4 (blocks from :center, at most ::away-cap)
  and up to ::farmable-weight for cells a hoe cannot make farmland of. A spot with level under 100 has cells to level or build up.
  :farmable is the % of its cells whose ground is farmland or a hoe makes farmland of (dirt, grass, path, coarse or rooted dirt).
  A patch is left out when a column of it is unseen, lies under a plan's footprint, or in a zone or claim of another owner (the body's own zones are fine);
  :max-step drops patches with a bigger step.
  Needs nothing; it reads only what the body has seen and looks around once when no water is in sight.")

(a/defargs args
  {:w {:doc "patch width (x), 1 to 16" :spec (a/int-in 1 16) :default 5}
   :h {:doc "patch height (z), 1 to 16" :spec (a/int-in 1 16) :default 5}
   :range {:doc "patches lie within this many blocks of :center in x and z, 1 to 48" :spec (a/int-in 1 48) :default 24}
   :center {:doc "where to search from {:x :y :z}; nil is the body's cell" :spec ::a/pos :default nil}
   :max-step {:doc "drop patches whose largest step between neighbouring cells is over this; nil: keep all" :spec (a/int-in 0 16) :default nil}})

(a/defargs settings
  {::depth {:default 12 :doc "How far above and below the centre's y to look for ground." :spec (a/int-in 1 16)}
   ::limit {:default 3 :doc "How many spots to keep; the scan stops once that many good ones are found." :spec (a/int-in 1 8)}
   ::job-time-s {:default 300 :doc "Seconds the call may take, exploring included (a guess until measured)." :spec (a/num-in 0 nil)}
   ::max-legs {:default 8 :doc "Legs walked while exploring before it gives up." :spec (a/int-in 0 nil)}
   ::leg-spacing {:default 24 :doc "Blocks between exploring legs." :spec (a/int-in 4 64)}
   ::max-distance {:default 96 :doc "How far (XZ) from the start an exploring leg may go." :spec (a/int-in 1 128)}
   ::max-steps {:default 6 :doc "Steps toward one column not yet seen before it is given up." :spec (a/int-in 1 nil)}
   ::max-failed-in-row {:default 3 :doc "Exploring walks in a row that fail before it gives up." :spec (a/int-in 1 nil)}
   ::sky-extra {:default 8 :doc "Extra blocks of sky checked above a spot." :spec (a/int-in 0 nil)}
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

(def farmable-ground
  "The grounds that are farmland or a hoe makes farmland of (coarse and rooted dirt in two passes)."
  (conj till/tillable "farmland"))

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
  [p {:keys [w h range depth limit blocked?] limit-step :max-step :as a} from {:keys [next-ring found cache]} budget]
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
        wet? (fn [x z y] (farm/hydrated? waters x y z))
        fx (:x from) fz (:z from)
        by-ring (corners-by-ring a from)
        rings (sort (keys by-ring))
        cand (fn [[x z]]
               (when (some? (top-of (+ x (quot w 2)) (+ z (quot h 2))))
                 (let [cols (for [dx (clojure.core/range w) dz (clojure.core/range h)] [(+ x dx) (+ z dz)])
                       tops (mapv (fn [[cx cz]] (top-of cx cz)) cols)]
                   (when (and (not-any? nil? tops)
                              (not (some true? (map (fn [[cx cz] t] (and blocked? (blocked? cx t cz))) cols tops))))
                     (let [wetn (count (filter true? (map (fn [[cx cz] t] (wet? cx cz t)) cols tops)))
                           sky (every? true? (map (fn [[cx cz] t] (sky-of cx cz t)) cols tops))
                           farm (count (filter true? (map (fn [[cx cz] t] (contains? farmable-ground (name-fn {:x cx :y t :z cz}))) cols tops)))
                           s (score-patch {:tops tops :water-share (/ wetn (count cols)) :sky sky :farmable (/ farm (count cols))
                                           :away (js/Math.hypot (- (+ x (/ (dec w) 2)) fx) (- (+ z (/ (dec h) 2)) fz))})]
                       (when s
                         (let [step (max-step tops h)]
                           (when (or (nil? limit-step) (<= step limit-step))
                             (assoc s :step step :pos {:x x :y (:y s) :z z})))))))))]
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

(def target [:center :w :h])

(def defaults {:spend {:job-time ::job-time-s}})
(def safe?
  "Scanning needs only no danger near; the exploring walks are go-to children, which check their own light."
  (with-meta ready/threatened
    {::ready/exposure (fn [_] (contract/ok :survey "the scan walks nowhere; a leg's go-to child judges its own exposure"))}))
(def fit? ready/fit?)

(defn done?
  "Ok once spots are found; else later."
  [c]
  (let [spots (:spots (ctx/mem c))
        data [:units :spots :done (count spots) :left (if (seq spots) 0 1)
              :result (if (seq spots) {:spot (:pos (first spots)) :spots spots} {})]]
    (if (seq spots)
      (apply contract/ok :found (str "find-spot: " (count spots) " spots found") data)
      (apply contract/later :searching (str "find-spot: " (count spots) " spots found") data))))

(defn quote-of [_c]
  {:uses {:job-time [0.1 1 20]}})

(defn finish-scan!
  "Remember the found spots in memory (dropping the scan state); nil when there are none."
  [c found]
  (ctx/update-mem! c dissoc :scan)
  (when (seq found)
    (ctx/update-mem! c assoc :spots found)
    found))

(defn scan-step!
  "One bounded scan slice. :again while rows remain (state kept in :scan),
  else the finished spots (or nil for none)."
  [c a]
  (let [saved (:scan (ctx/mem c))
        from (or (:at (ctx/mem c)) (:center a) (:from saved) (into {} (map (fn [[k v]] [k (js/Math.floor v)])) (u/self-pos c)))
        state (or saved {:next-ring 0 :found []})
        a (assoc a :blocked? (blocked-fn {:footprints (known/footprints c) :zones (known/zones c) :claims (known/claims c)
                                          :self (ctx/self-name c) :now (ctx/now c)}))
        r (scan-rows (:primitives c) a from state (read-budget))
        spent (+ (:reads state 0) (:reads r))]
    (if (or (:done r) (>= spent (max-reads)))
      (finish-scan! c (:found r))
      (do (ctx/update-mem! c assoc :scan (assoc (select-keys r [:next-ring :found :cache]) :from from :reads spent))
          :again))))

(defn water-unseen?
  "Whether the first scan would run with no water in sight, so a look around could still show some."
  [c a]
  (and (not (:spots (ctx/mem c))) (not (:scan (ctx/mem c))) (not (look/surveyed? c))
       (empty? (look/seen-blocks (:primitives c) {:names ["water"] :radius (+ (:range a) (:w a) (:h a) 4) :max 1}))))

(defn next-leg
  "The nearest untried exploring leg round m's origin: {:leg [x y z] :step column?} (a step toward a column not yet seen is walked
  and looked at again, no leg) and :dead, the columns with no ground to stand on, which are not tried again."
  [p m [bx bz] y]
  (let [tried (set (:tried m))
        legs (->> (search/spiral-points (:origin m) (settings/get settings ::leg-spacing) (settings/get settings ::max-distance))
                  (remove tried)
                  (sort-by #(search/xz-dist % [bx bz])))
        at (search/seen-at p)]
    (loop [[pt & more] legs dead []]
      (if (nil? pt)
        {:dead dead}
        (let [{sy :y fail :fail} (search/stand-cell at (pt 0) (pt 1) y)
              step (when (and (= :unseen fail) (< (get (:steps m) pt 0) (settings/get settings ::max-steps))) (search/step-toward at [bx y bz] (pt 0) (pt 1)))]
          (cond
            sy {:dead dead :leg [(pt 0) sy (pt 1)]}
            step {:dead dead :leg step :step pt}
            :else (recur more (conj dead pt))))))))

(defn ^:async walk-leg!
  "One go-to toward the leg in memory: :continue while it goes on; on its end book the arrival (re-centre, forget the scan) or the failure."
  [c]
  (let [[x y z] (:leg (ctx/mem c))
        r (await (child/call-walk* c :leg {:pos {:x x :y y :z z} :range 2 :settings {:jobs.movement.go-to.tuning/goal-danger :end}}))]
    (if (= :continue r)
      :continue
      (let [arrived (and (:arrived (:data r)) (not (:in-water (:data r))))
            in-row (if arrived 0 (inc (:failed-in-row (ctx/mem c) 0)))]
        (ctx/update-mem! c (fn [m] (cond-> (-> m (dissoc :leg :leg-step :scan) (assoc :failed-in-row in-row))
                                     (not (:leg-step m)) (-> (update :tried (fnil conj []) [x z]) (update :legs (fnil inc 0)))
                                     (and arrived (not (:leg-step m))) (assoc :at {:x x :y y :z z}))))
        (when arrived (await (look/survey! c :run (look/search-effort))))
        (if (>= in-row (settings/get settings ::max-failed-in-row))
          (ctx/end c :nothing-possible :none {:spot nil :why :stuck})
          :again)))))

(defn explore!
  "No patch in the area scanned: choose the next leg and start walking, or end for want of one. :again when a leg was set."
  [c]
  (let [m (ctx/mem c)
        origin (or (:origin m) (let [{:keys [x z]} (or (:center (:args c)) (into {} (map (fn [[k v]] [k (js/Math.floor v)])) (u/self-pos c)))] [x z]))
        m (assoc m :origin origin)
        self (u/self-pos c)
        {:keys [leg dead] stepping :step} (next-leg (:primitives c) m [(:x self) (:z self)] (:y self))
        _ (ctx/update-mem! c #(-> % (assoc :origin origin) (update :tried into dead)))]
    (cond
      (>= (:legs m 0) (settings/get settings ::max-legs)) (ctx/end c :nothing-possible :none {:spot nil :why :legs})
      (nil? leg) (ctx/end c :nothing-possible :none {:spot nil :why :distance})
      :else (do (ctx/update-mem! c #(cond-> (assoc % :leg leg :leg-step (boolean stepping)) stepping (update-in [:steps stepping] (fnil inc 0)))) :again))))

(defn ^:async step
  "Look around when no water is in sight, then scan (a bounded slice per step, remembering the spots); with no patch, explore."
  [c]
  (let [a (assoc (merge (settings/job-defaults (quote jobs.farm.find-spot) args) (:args c))
                 :depth (settings/get settings ::depth) :limit (settings/get settings ::limit))]
    (if (:leg (ctx/mem c))
      (await (walk-leg! c))
      (let [found (if (water-unseen? c a)
                    (do (await (look/survey! c :run (look/search-effort))) :again)
                    (or (:spots (ctx/mem c)) (scan-step! c a)))]
        (cond
          (= :again found) :again
          (nil? found) (explore! c)
          :else (ctx/end c :done nil {:spot (:pos (first found)) :spots found}))))))

(defn ^:async round
  "The whole attempt: scan slice after slice."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
