(ns jobs.explore.search
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [clojure.string :as str]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.area :as area]
            [jobs.lib.breath :as breath]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [engine.notes :as notes]))

(def doc
  "Look for :count things named :target (a block or entity name, or several) by walking legs from where the job
  starts (the origin), or from :center when given.

  Each step looks around (blocks and entities with those names within :scan-radius; not again within 3 blocks of the
  last look round, nor when what it has seen already makes up :count), notes what it saw (:seen) and
  the spot (:searched) (engine.notes). Blocks the body saw earlier anywhere within :max-distance of the origin count as
  found too, nearest first. It ends when :count are found. Otherwise it walks the next leg with
  jobs.movement.go-to as a child (range 2). One call is the whole search; it yields (:continue) only while go-to waits
  on the world or for leg columns to load (:chunks-loading).

  Legs are :spacing apart: :spiral rings round the origin, or :outward ahead along :heading (a :heading alone means :outward), then the sides, and on further ahead once those are tried or searched. A
  leg is never tried twice and never goes beyond :max-distance (XZ) of the origin. It is skipped (:skipped)
  when a live :searched note of any body, this one included, listing every target name lies within half its
  radius.

  A leg needs a standing cell in its column from 12 above the feet down to :descent (40) below, read top down through leaves. An unloaded,
  wet or standless column is recorded failed (:not-loaded, :wet, :no-surface, :unseen) and the next candidate is tried
  in the same round. A loaded column not yet seen (:unseen) is walked toward in steps of up to 12 blocks (not
  counted in :max-legs) and looked at again; within 3 blocks of it, it is recorded failed. An unloaded column is not tried but looked at again later. When only unloaded ones are
  left the round pauses 2 s (jobs.time.noop, holding :chunks-loading) and yields, until they load or the call's time is spent (a body just logged in has no chunks
  yet). A walk go-to gives up on is recorded :unreachable. Three of those in
  a row end the search (:stuck); until a leg arrives, a leg within 60 degrees of the way one failed from where the
  body stands waits (the same obstacle likely blocks it), and the search ends :stuck when only such legs are left.
  A leg whose goal lies by a mob the body would flee is not walked (go-to :goal-danger
  :end): it is recorded :goal-dangerous, no stuck walk; a step toward an unseen column given up so leaves that column.

  With :use-notes, live :seen notes of a target within :max-distance count as found (:noted true, :by), unless the body
  sees another block at that cell now, and a noted entity standing in a zone or plan of the body's own (its pens) never counts. The
  ending text lists the positions.

  The ending's data: {:found [{:what :pos :id? :noted? :by?}] :coverage {:legs :scans
  :failed [{:pos :reason}] :skipped :farthest} :why :text} and the ending:
  - :done: :count found.
  - :nothing-possible :distance (no leg left), :legs (:max-legs walked) or :stuck (three failed walks in a row).
  - :limit-reached :job-time (the call's :spend :job-time, default ::job-time-s, pauses not counted) or :wall-time (default ::wall-time-s); the engine ends it, with the data of done? ({:found :coverage :why}) and neither event; with :least targets found (default 1) the engine ends it :done instead.
  - A call with no :target never runs (:bad-args).

  Memory: :origin :legs :scans :found :tried :failed :skipped :farthest :failed-in-row :blocked (the ways legs
  failed since the last arrival) :deferred (unloaded legs set aside while a sweep goes on), and :leg while
  a walk is under way, so a cut or restart resumes it.")

(def diagonal 0.7071)
(def headings {:north [0 -1] :east [1 0] :south [0 1] :west [-1 0]
               :north-east [diagonal (- diagonal)] :south-east [diagonal diagonal]
               :south-west [(- diagonal) diagonal] :north-west [(- diagonal) (- diagonal)]})

(a/defargs args
  {:target {:doc "a block or entity name, or several (a vector or set)" :spec (a/or-of a/name? (a/coll-of a/name?)) :default nil}
   :count {:doc "how many targets end the search" :spec (a/int-in 1 nil) :default 1}
   :center {:doc "the centre of the area the job works, a cell [x y z] or {:x :y :z}; nil: where the body stands when the job first runs (jobs.lib.area)" :spec a/position? :default nil}
   :max-distance {:doc "how far (XZ) from the origin a leg may go, 1 to 128" :spec (a/int-in 1 128) :default 96}
   :pattern {:doc ":spiral (rings round the origin, the default) or :outward (ahead along :heading); :outward when a :heading is given" :spec #{:spiral :outward} :default nil}
   :heading {:doc "a compass heading (:north, :north-east, :east ...), for :outward (default :north)" :spec (set (keys headings)) :default nil}
   :spacing {:doc "blocks between legs, 4 to 64" :spec (a/int-in 4 64) :default 16}
   :scan-radius {:doc "how far round the body each look reaches" :spec (a/num-in 0 nil) :default 24}
   :max-legs {:doc "legs walked before it gives up" :spec (a/int-in 0 nil) :default 32}
   :takeable {:doc "leave out blocks a zone, claim or plan footprint bars this body from taking" :spec boolean? :default false}
   :avoid {:doc "columns [x z] whose blocks do not count as found" :spec (a/coll-of (a/pred "an [x z] column" vector?)) :default nil}
   :avoid-boxes {:doc "boxes [[x1 z1] [x2 z2]] (corners inclusive, any order) of columns whose blocks do not count as found: a pen is one box, not its columns (a job expression is size-limited)" :spec (a/coll-of (a/pred "two [x z] corners" #(and (vector? %) (= 2 (count %)) (every? vector? %)))) :default nil}
   :use-notes {:doc "targets noted (by any body) within :max-distance count as found" :spec boolean? :default true}})

(a/defargs settings
  {::seen-ttl-s {:default 259200 :doc "Seconds a note of a block seen lasts." :spec (a/num-in 0 nil)}
   ::entity-ttl-s {:default 600 :doc "Seconds a note of an entity seen lasts." :spec (a/num-in 0 nil)}
   ::searched-ttl-s {:default 86400 :doc "Seconds a note of searched ground lasts." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 600 :doc "Seconds from the first round before a search gives up." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 3600 :doc "Seconds from the first round before a search gives up, waits included (a pause is no job time)." :spec (a/num-in 0 nil)}
   ::max-failed-in-row {:default 3 :doc "Failed attempts in a row before the search gives up." :spec (a/int-in 0 nil)}
   ::wait-ms {:default 2000 :doc "How long the search waits for the world to settle, in ms." :spec (a/int-in 0 nil)}
   ::descent {:default 40 :doc "How far below its feet a leg's ground is looked for, in blocks (go-to does the descent)." :spec (a/int-in 0 nil)}
   ::reach {:default 12 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}
   ::held-angle {:default 60 :doc "A leg lies the way a failed leg went when the angle between them, seen from the body, is under this many degrees." :spec (a/num-in 0 180)}
   ::held-near {:default 4 :doc "Blocks from where a leg failed within which its way still holds others back." :spec (a/num-in 0 nil)}
   ::near-sight {:default 6 :doc "Within this many blocks a look misses no animal: a noted animal not seen there is taken as gone." :spec (a/num-in 0 nil)}
   ::stand-cap {:default 48 :doc "Stand cells one choice reads; past it :more says candidates are left for the next step." :spec (a/int-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))
(defn max-failed-in-row [] (settings/get settings ::max-failed-in-row))
(defn wait-ms [] (settings/get settings ::wait-ms))
(defn reach [] (settings/get settings ::reach))
(defn descent [] (settings/get settings ::descent))
(def target [:target :center])

(def safe? ready/safe-if-lit)
(def fit? ready/fit?)

(def defaults {:spend {:job-time ::job-time-s :wall-time ::wall-time-s}})


;; ------------------------------------------------------------------ pure

(defn targets
  "The target names as a sorted vector."
  [c]
  (let [t (:target (:args c))]
    (if (string? t) [t] (vec (sort (set t))))))

(defn xz-dist [[ax az] [bx bz]]
  (js/Math.hypot (- ax bx) (- az bz)))

(defn spiral-points
  "[x z] points spacing apart in rings round the origin [x z], nearest ring first, within max-distance."
  [[ox oz] spacing max-distance]
  (let [ring (fn [r] (concat (for [x (range (- r) r)] [x (- r)])
                             (for [z (range (- r) r)] [r z])
                             (for [x (range r (- r) -1)] [x r])
                             (for [z (range r (- r) -1)] [(- r) z])))]
    (->> (range 1 (inc (js/Math.floor (/ max-distance spacing))))
         (mapcat ring)
         (filter (fn [[x z]] (<= (js/Math.hypot (* x spacing) (* z spacing)) max-distance)))
         (mapv (fn [[x z]] [(+ (js/Math.floor ox) (* x spacing)) (+ (js/Math.floor oz) (* z spacing))])))))

(def outward-offsets
  "[ahead across] fractions of the spacing: straight on, then ever more to the sides."
  [[1 0] [0.75 0.5] [0.75 -0.5] [0.25 0.75] [0.25 -0.75] [0 1] [0 -1]])

(defn outward-points
  "[x z] points spacing away from [x z] along heading, then to its sides."
  [[x z] heading spacing]
  (let [[dx dz] (headings heading)
        [sx sz] [(- dz) dx]]
    (mapv (fn [[f a]] [(js/Math.floor (+ x (* spacing (+ (* dx f) (* sx a)))))
                       (js/Math.floor (+ z (* spacing (+ (* dz f) (* sz a)))))])
          outward-offsets)))

(def open-names #{"air" "cave_air" "void_air" "short_grass" "grass" "tall_grass" "fern" "large_fern" "dead_bush"
                  "snow" "dandelion" "poppy" "blue_orchid" "allium" "azure_bluet" "oxeye_daisy" "cornflower"
                  "lily_of_the_valley" "torch" "leaf_litter" "short_dry_grass" "tall_dry_grass" "bush" "firefly_bush"})

(defn open? [n]
  (boolean (and n (or (open-names n) (str/ends-with? n "_sapling") (str/ends-with? n "_tulip")))))

(defn leaves? [n] (str/ends-with? n "_leaves"))
(def wet-names #{"water" "lava" "bubble_column"})

(defn seen-at
  "The block-at of stand-cell for primitives p: [x y z] -> the name seen or remembered, :unseen for a loaded cell
  never seen, nil when the chunk is not loaded."
  [p]
  (fn [[bx by bz]]
    (let [pos {:x bx :y by :z bz}]
      (or (u/seen-name p pos) (when (u/sensed p pos) :unseen)))))

(defn stand-cell
  "Where a body could stand in column x z near feet height y, read top down from y+reach to y-descent through open cells and
  leaves to the first other block: {:y feet} when the two cells above it are open, else {:fail :wet} (a fluid),
  {:fail :not-loaded} (an unloaded cell on the way), {:fail :unseen} (a loaded cell not yet seen) or {:fail :no-surface}.
  block-at: [x y z] -> name, :unseen or nil."
  [block-at x z y]
  (let [y0 (js/Math.floor y)]
    (loop [h (+ y0 (reach))]
      (if (< h (- y0 (descent)))
        {:fail :no-surface}
        (let [n (block-at [x h z])]
          (cond
            (nil? n) {:fail :not-loaded}
            (= :unseen n) {:fail :unseen}
            (or (open? n) (leaves? n)) (recur (dec h))
            (wet-names n) {:fail :wet}
            (and (open? (block-at [x (inc h) z])) (open? (block-at [x (+ h 2) z]))) {:y (inc h)}
            :else {:fail :no-surface}))))))

(defn finding-key [{:keys [what id pos]}] [what (or id pos)])

(defn add-found
  "found with the new findings whose key it lacks, at most want of them."
  [found new want]
  (vec (take want (reduce (fn [v f] (if (some #(= (finding-key f) (finding-key %)) v) v (conj v f))) found new))))

(defn outdated?
  "Whether the body's own sight holds a different block at the cell of block note n (a cell never seen proves nothing)."
  [p n]
  (let [[x y z] (:pos n)
        seen (when-not (:id n) (u/seen-name p {:x x :y y :z z}))]
    (boolean (and seen (not= seen (:what n))))))

(defn departed
  "The ids of entity notes lying within near blocks of the body at [x y z] whose animal is not among the entities seen
  there now (killed or gone), since a look this close misses none."
  [ns seen [x y z] near]
  (let [here (set (keep :id seen))]
    (into #{} (comp (filter #(and (= :seen (:kind %)) (:id %) (not (here (:id %)))
                                  (<= (js/Math.hypot (- ((:pos %) 0) x) (- ((:pos %) 1) y) (- ((:pos %) 2) z)) near)))
                    (map :id))
          ns)))

(defn noted
  "Findings from live :seen notes of names within max-distance (XZ) of the origin [x y z], leaving out a block note the
  body now sees something else at and the entity notes of the departed ids."
  [p ns names [ox _ oz] max-distance gone]
  (->> ns
       (remove #(gone (:id %)))
       (filter #(and (= :seen (:kind %)) (some #{(:what %)} names)
                     (<= (xz-dist [((:pos %) 0) ((:pos %) 2)] [ox oz]) max-distance)
                     (not (outdated? p %))))
       (map (fn [n] (cond-> {:what (:what n) :pos (:pos n) :noted true :by (:by n)} (:id n) (assoc :id (:id n)))))))

;; ------------------------------------------------------------------ the body

(defn no-target
  "Never :bad-args while no target is named."
  [c]
  (if (empty? (targets c))
    (contract/never :bad-args "no target is named" :text "no target is named")
    (contract/ok :target "a target is named")))

(def can-start? no-target)

(defn coverage [m]
  {:legs (:legs m 0) :scans (:scans m 0) :failed (:failed m []) :skipped (:skipped m 0) :farthest (:farthest m 0)})

(defn done?
  "The targets found against :count; done once all are found."
  [c]
  (let [n (count (:found (ctx/mem c)))
        left (max 0 (- (:count (:args c)) n))
        data [:units :found :done n :left left
              :result (cond-> {:reason :found :found (:found (ctx/mem c) []) :coverage (coverage (ctx/mem c))} (pos? left) (assoc :why :time))]]
    (if (zero? left)
      (apply contract/ok :found (str "found " n) data)
      (apply contract/later :searching (str "found " n " of " (:count (:args c))) data))))

(defn time-limit "Seconds the call may search." [c]
  (or (get-in (ctx/frozen-call c) [:spend :job-time]) (time-s)))

(defn quote-of [c]
  (let [most (time-limit c)]
    {:uses {:job-time [0 (min 60 most) most]}}))

(defn cell [p] (mapv js/Math.floor [(.-x p) (.-y p) (.-z p)]))
(defn here [c] (cell (.-pos (.self (:primitives c)))))

(defn cell-of [{:keys [x y z]}] (mapv js/Math.floor [x y z]))

(defn sense
  "The targets the body has seen (never through walls): blocks {:what :pos}, entities {:what :pos :id}."
  [p names radius]
  (concat (map (fn [b] {:what (:name b) :pos (cell-of (:pos b))})
               (mapcat #(look/seen-blocks p {:names [%] :radius radius :max 64 :live? true}) names))
          (map (fn [e] (cond-> {:what (:name e) :pos (cell-of (:pos e))} (:uuid e) (assoc :id (:uuid e))))
               (look/seen-entities p {:names names :radius radius :max 32}))))

(defn remembered
  "Findings from blocks the body saw earlier anywhere within max-distance (XZ) of the origin [x y z], beyond what a look
  reaches, nearest the body first. The memory read is capped after the sort by distance from the body, so the cap is
  generous: a dense target near the body must not cut those in range.
  Read once per look (not again when choosing a leg resumes): the seen memory only grows when the body sees more."
  [p names [ox _ oz] max-distance]
  (->> (look/seen-blocks p {:names names :radius (* 2 max-distance) :max 512 :live? true})
       (map (fn [b] {:what (:name b) :pos (cell-of (:pos b))}))
       (filter #(<= (xz-dist [((:pos %) 0) ((:pos %) 2)] [ox oz]) max-distance))))

(defn finish!
  "End the call: done when found (or nothing was left to find), else the ending of why; the text and findings are the ending's data."
  [c reason why]
  (let [m (ctx/mem c)
        what (str/join "/" (targets c))
        found (:found m [])
        result (cond-> {:reason reason :found found :coverage (coverage m)
                        :text (if (= :found reason)
                                (str "found " (count found) " " what " at " (str/join ", " (map #(str/join "," (:pos %)) found)))
                                (str "no " what " found (" (name why) ") after " (:legs m 0) " legs, "
                                     (:scans m 0) " looks of " (:scan-radius (:args c)) " blocks round, " (:farthest m 0) " blocks out"
                                     (when (pos? (:skipped m 0)) (str ", " (:skipped m) " legs skipped as already searched"))))}
                 why (assoc :why why))]
    (case why
      nil (ctx/end c :done nil result)
      (ctx/end c :nothing-possible why result))))

(defn takeable
  "found without the blocks in :avoid columns and :avoid-boxes, the blocks on the footprint of a plan the body made (its
  posts and walls are not material to cut), the noted entities standing in a zone or plan of the body's own (its pens),
  and (when :takeable is on) those a zone, claim or plan footprint bars taking."
  [c found]
  (let [{:keys [takeable avoid avoid-boxes]} (:args c)
        avoid (set (map vec avoid))
        boxes (map (fn [[[x1 z1] [x2 z2]]] [(min x1 x2) (min z1 z2) (max x1 x2) (max z1 z2)]) avoid-boxes)
        avoided? (fn [[x z]] (or (avoid [x z]) (some (fn [[x1 z1 x2 z2]] (and (<= x1 x x2) (<= z1 z z2))) boxes)))
        in (when takeable (access/rules-input c))
        own (access/rules-input c {:spare-own? true})]
    (cond->> found
      true (remove #(and (not (:id %)) (access/own-keep own (zipmap [:x :y :z] (:pos %)))))
      true (remove #(and (:noted %) (:id %) (access/in-own-area? c (zipmap [:x :y :z] (:pos %)))))
      (or (seq avoid) (seq boxes)) (remove #(and (not (:id %)) (avoided? [((:pos %) 0) ((:pos %) 2)])))
      takeable (filter #(or (:id %) (:ok (access/may? in :harvest (:pos %))))))))

(defn ^:async scan!
  "Look round (every heading, once per spot: not again within look/near-blocks of the last look round), unless what is
  seen already makes up :count; note what was seen and the spot as searched, book the findings."
  [c names]
  (let [{:keys [scan-radius] want :count} (:args c)
        seen-ttl-s (settings/get settings ::seen-ttl-s)
        entity-ttl-s (settings/get settings ::entity-ttl-s)
        searched-ttl-s (settings/get settings ::searched-ttl-s)
        seen #(vec (takeable c (sense (:primitives c) names scan-radius)))
        _ (when (< (count (add-found (:found (ctx/mem c) []) (seen) want)) want)
            (when-not (await (look/survey! c :near (look/search-effort))) (look/see! c (look/search-effort))))
        sighted (seen)
        [x y z] (here c)
        [ox _ oz] (:origin (ctx/mem c))]
    (notes/note! c (conj (mapv #(assoc % :kind :seen :ttl-ms (* 1000 (if (:id %) entity-ttl-s seen-ttl-s))) sighted)
                         {:kind :searched :what names :pos [x y z] :r scan-radius :ttl-ms (* 1000 searched-ttl-s)}))
    (ctx/update-mem! c (fn [m] (-> m
                                   (update :scans (fnil inc 0))
                                   (update :farthest (fnil max 0) (js/Math.round (xz-dist [x z] [ox oz])))
                                   (update :found #(add-found (or % []) sighted want)))))))

(defn candidates [c m]
  (let [{:keys [pattern heading spacing max-distance]} (:args c)
        [ox _ oz] (:origin m)
        [x _ z] (here c)]
    (if (or (= :outward (keyword pattern)) (and heading (not pattern)))
      (let [heading (keyword (or heading :north))
            [dx dz] (headings heading)]
        (mapcat (fn [k] (outward-points [(+ x (* dx spacing k)) (+ z (* dz spacing k))] heading spacing))
                (range (inc (js/Math.ceil (/ max-distance spacing))))))
      (spiral-points [ox oz] spacing max-distance))))

(defn stand-cap [] (settings/get settings ::stand-cap))
(defn near-sight [] (settings/get settings ::near-sight))

(defn step-toward
  "Where to walk toward the unseen column [x z] from the body at [bx y bz]: the seen stand cell on the line 12, 6 or 3
  blocks out, else the 6 or 3 block point at feet height (far ground is seldom in view), nil when within 3. A player
  walks toward ground out of view and looks again."
  [block-at [bx y bz] x z]
  (let [d (xz-dist [bx bz] [x z])
        at (fn [k] (let [f (/ k d)] [(js/Math.round (+ bx (* f (- x bx)))) (js/Math.round (+ bz (* f (- z bz))))]))
        near (filter #(< % d) [12 6 3])]
    (or (some (fn [k] (let [[sx sz] (at k) {sy :y} (stand-cell block-at sx sz y)] (when sy [sx sy sz]))) near)
        (when-let [k (first (filter #(<= % 6) near))] (let [[sx sz] (at k)] [sx y sz])))))

(defn held?
  "Whether point [x z] lies the way a leg failed from near the body at [hx hz] (blocked, [{:at [x z] :leg [x z]}] since
  the last arrival): the same obstacle likely stands in its way."
  [blocked [hx hz] [x z]]
  (let [v [(- x hx) (- z hz)]]
    (boolean (some (fn [{[ax az] :at [lx lz] :leg}]
                     (let [w [(- lx hx) (- lz hz)]
                           n (* (js/Math.hypot (v 0) (v 1)) (js/Math.hypot (w 0) (w 1)))]
                       (and (<= (xz-dist [hx hz] [ax az]) (settings/get settings ::held-near))
                            (pos? n)
                            (> (/ (+ (* (v 0) (w 0)) (* (v 1) (w 1))) n) (js/Math.cos (* js/Math.PI (/ (settings/get settings ::held-angle) 180)))))))
                   blocked))))

(defn choose-leg
  "{:leg [x y z] (or nil when none is left) :tried [points looked at] :failed [{:pos :reason}] :skipped n
  :unloaded [{:pos :reason :not-loaded}] :step true when :leg is only a step toward an unseen column (it is no leg
  walked; :column that column) :more true when stand-cap cells were read and candidates may be left :held n, the
  points passed over for now (held?, m's :blocked)}; an unloaded column is not tried, so a later round looks again (m's
  :deferred ones are kept out of this sweep)."
  [c m names ns]
  (let [{:keys [max-distance]} (:args c)
        [ox y oz] (here c)
        origin [((:origin m) 0) ((:origin m) 2)]
        tried (into (set (:tried m)) (map (fn [{[x _ z] :pos}] [x z])) (:deferred m))
        p (:primitives c)
        block-at (seen-at p)]
    (loop [[pt & more] (candidates c m) reads 0 acc {:tried [] :failed [] :skipped 0 :held 0 :unloaded (vec (:deferred m))}]
      (cond
        (nil? pt) acc
        (>= reads (stand-cap)) (assoc acc :more true)
        (or (tried pt) (> (xz-dist pt origin) max-distance) (= pt [ox oz])) (recur more reads acc)
        (held? (:blocked m) [ox oz] pt) (recur more reads (update acc :held inc))
        (some #(notes/covers? % pt names) ns) (recur more reads (-> acc (update :tried conj pt) (update :skipped inc)))
        :else (let [{sy :y fail :fail} (stand-cell block-at (pt 0) (pt 1) y)
                    failed {:pos [(pt 0) y (pt 1)] :reason fail}
                    step (when (= :unseen fail) (step-toward block-at (here c) (pt 0) (pt 1)))]
                (cond
                  step (assoc acc :leg step :step true :column pt)
                  (= :not-loaded fail) (recur more (inc reads) (update acc :unloaded conj failed))
                  fail (recur more (inc reads) (-> acc (update :tried conj pt) (update :failed conj failed)))
                  :else (assoc (update acc :tried conj pt) :leg [(pt 0) sy (pt 1)])))))))

(def interrupted #{:cut-again :moved-while-searching})

(defn interrupted?
  "Whether go-to's result res is an interruption (the walk was cut or the body pushed about), not a verdict on the leg."
  [{:keys [why reason]}]
  (boolean (or (interrupted why) (interrupted reason))))

(defn ^:async walk!
  "One go-to call toward the leg (the last one before the failed legs in a row end the search may dig out); on its end book the leg arrived or failed. :continue when go-to yields, else :again
  (or the ending when the failed legs in a row end the search)."
  [c]
  (let [[x y z] (:leg (ctx/mem c))
        r (await (child/call-walk* c :leg (cond-> {:pos {:x x :y y :z z} :range 2 :goal-danger :end}
                                                           (< (:failed-in-row (ctx/mem c) 0) (dec (max-failed-in-row))) (assoc :ways #{:walk :clear-seal}))))]
    (if (= :continue r)
      :continue
      (let [{:keys [in-water why] :as data} (:data r)
            cause (contract/cause-key r)
            res (assoc data :reason cause)
            arrived (and (:arrived data) (not in-water))
            dangerous? (= :goal-dangerous (or why cause))
            in-row (cond arrived 0 dangerous? (:failed-in-row (ctx/mem c) 0) :else (inc (:failed-in-row (ctx/mem c) 0)))
            [hx _ hz] (here c)]
        (ctx/update-mem! c (fn [m] (cond-> (assoc (dissoc m :leg :leg-column) :failed-in-row in-row)
                                     arrived (dissoc :blocked)
                                     (not (or arrived dangerous?)) (update :blocked (fnil conj []) {:at [hx hz] :leg [x z]})
                                     (not arrived) (update :failed (fnil conj []) {:pos [x y z] :reason (cond dangerous? :goal-dangerous in-water :wet :else :unreachable)})
                                     (and (not arrived) (not (interrupted? res)) (:leg-column m)) (-> (update :tried (fnil conj []) (:leg-column m))
                                                                              (cond-> (not dangerous?) (update :legs (fnil inc 0)))))))
        (if (>= in-row (max-failed-in-row)) (finish! c :not-found :stuck) :again)))))

(defn ^:async step!
  "Look round; end when enough is found or the legs are used up, else start the next leg."
  [c]
  (let [{:keys [max-legs use-notes max-distance] want :count} (:args c)
        names (targets c)
        resumed (:choosing (ctx/mem c))
        _ (when-not (or resumed (breath/in-water? (:primitives c))) (await (scan! c names)))
        ns (notes/notes c)
        m (ctx/mem c)
        near (min (:scan-radius (:args c)) (near-sight))
        gone (if (pos? near) (departed ns (sense (:primitives c) names near) (here c) near) #{})
        found (cond-> (cond-> (:found m)
                        (not resumed) (add-found (takeable c (remembered (:primitives c) names (:origin m) max-distance)) want))
                use-notes (add-found (takeable c (noted (:primitives c) ns names (:origin m) max-distance gone)) want))]
    (ctx/update-mem! c assoc :found found)
    (cond
      (>= (count found) want) (finish! c :found nil)
      (>= (:legs m 0) max-legs) (finish! c :not-found :legs)
      :else (let [{:keys [leg step column tried failed skipped unloaded more held]} (choose-leg c m names ns)]
              (ctx/update-mem! c (fn [m] (-> m
                                             (update :tried (fnil into []) tried)
                                             (update :failed (fnil into []) failed)
                                             (update :skipped (fnil + 0) skipped)
                                             (assoc :choosing (boolean (and more (not leg))))
                                             (assoc :deferred (when (and more (not leg)) unloaded)))))
              (cond
                leg (do (ctx/update-mem! c (fn [m] (-> m (assoc :leg leg :leg-column column)
                                                       (update :legs (fnil + 0) (if step 0 1)))))
                        (await (walk! c)))
                more :again
                (and (empty? unloaded) (pos? held)) (do (ctx/update-mem! c dissoc :blocked) :again)
                (empty? unloaded) (finish! c :not-found :distance)
                :else (do (await (child/pause! c (/ (wait-ms) 1000) :chunks-loading))
                          :continue))))))

(defn ^:async search-step!
  "One pass: end, walk the leg under way, or look and start the next leg. :again goes on, else a round result."
  [c]
  (ctx/update-mem! c (fn [m] (cond-> m (nil? (:origin m)) (assoc :origin (or (some-> (area/center c) cell-of) (here c))))))
  (if (:leg (ctx/mem c))
    (await (walk! c))
    (await (step! c))))

(defn ^:async round [c]
  (await (pace/steps! c #(search-step! c))))
