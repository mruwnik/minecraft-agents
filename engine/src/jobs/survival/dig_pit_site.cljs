(ns jobs.survival.dig-pit-site
  "Where dig-pit digs a night pit when the feet are no site: failed sites (shut-in's :shut-in-failed entries) and the nearest ground a pit can be dug at."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.cost :as cost]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.rooms :as rooms]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.solid :as solid]
            [jobs.lib.threats :as threats]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.walk-pick :as walk-pick]
            [jobs.lib.walk-risk :as walk-risk]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.plug-way :as way]
            [jobs.survival.shut-in-night :as shut-night]))

(a/defargs settings
  {::reach {:default 16 :doc "How far (blocks) a night pit looks for ground to dig in when the feet are no site." :spec (a/int-in 0 nil)}
   ::water-radius {:default 3 :doc "Blocks around a pit that failed for fluid beside it that count as the same site: a player steps a little way from the water, not out of the room." :spec (a/int-in 0 nil)}})

(defn priced-name
  "The block name a dig of cell is priced by: what is seen there, else the nearest seen solid block above it (the ground the pit is
  dug into: dirt stays dirt below the surface), else the rock guess."
  [p cell]
  (or (u/seen-name p cell)
      (->> (range 1 (inc (dig-look/rock-depth))) (keep #(u/seen-name p (update cell :y + %))) (filter solid/solid?) first)
      (dig-look/rock-name p cell)))

(defn dig-s
  "Seconds digging cells takes (tools/clear-ms with the best carried tool or the hand): stone for one nothing is known of, nothing for one seen open."
  [c cells]
  (let [p (:primitives c)]
    (/ (reduce + (map #(let [n (or (priced-name p %) "stone")] (if (solid/solid? n) (tools/clear-ms p n) 0)) cells)) 1000)))

(defn plan-s
  "Seconds a pit plan {:prep :steps :plugs} takes before it is dug: dig-s of its cells and a placement (way/place-s of c's call) for each plug and the roof. dig-pit's quote and pit-s both."
  [c {:keys [prep steps plugs]}]
  (+ (dig-s c (concat prep (mapcat :dig steps))) (* (way/place-s c) (inc (count plugs)))))

(defn pit-s
  "Seconds a pit dug at feet cell f takes before a plan is made: plan-s of a straight pit, 2 deep (the least a pit is). The walk in is priced apart (walk-pick, walk-toll-s)."
  [c f]
  (plan-s c (dig-cells/pit-steps f nil 2)))

(defn stay-toll-s
  "Seconds the known mobs the body would flee cost a pit dug at feet cell f (danger-near, threats/danger-near-fn of c):
  their toll there over pit-s."
  [c danger-near f]
  (let [toll (:toll (danger-near f) 0)]
    (if (zero? toll) 0 (* toll (pit-s c f)))))

(defn walk-toll-s
  "Seconds the known mobs the body would flee cost a walk from `from` to feet cell f: their toll at each cell of walk-risk/way
  (the straight line, no plan) over the least seconds a block takes."
  [danger-near from f]
  (* cost/least-block-s (transduce (map #(:toll (danger-near %) 0)) + 0 (rest (walk-risk/way from f nil)))))

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

(defn clear-of?
  "Whether feet cell f is at least site-radius+1 from every one of the failed sites (entries of failed)."
  [fails f]
  (every? #(>= (u/dist (:pos %) f) (inc (site-radius %))) fails))

(defn clear-of-failed?
  "Whether feet cell f is at least site-radius+1 from every failed site."
  [c f]
  (clear-of? (failed c) f))

(defn standing?
  "Whether the three block names (below, at, over the feet; each a fn of no args) leave standing room over ground the carried tools dig."
  [p below at over]
  (and (solid/solid? (below)) (not (solid/solid? (at))) (not (solid/solid? (over))) (tools/can-harvest? p (below))))

(defn pit-site?
  "Whether feet cell f looks like a place a pit can be dug, by what a player sees from the surface: standing room, dry
  ground the carried tools dig, no fluid beside it. How deep the ground goes is left to the pit, which stops a bad site."
  [p {:keys [x y z] :as f}]
  (let [below {:x x :y (dec y) :z z}]
    (and (standing? p #(u/seen-name p below) #(u/seen-name p f) #(u/seen-name p {:x x :y (inc y) :z z}))
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
  "The cheapest feet cell within ::reach of the top of the pit the body is in, else of the failed site it is at, else of
  its feet, clear of every failed site, where a pit can be dug (pit-site?): the least walk-pick/walk-s from there plus
  walk-toll-s and stay-toll-s, the nearer first on a tie; or nil. With :free?, only cells no zone, claim or plan refuses.
  Best first: each column where a pit stands (standing?) gets the lower bound of its price without the walk's toll, the
  nearest columns first; the cheapest bound is priced whole, then tested whole (pit-site?, zones) once no column not yet
  read can undercut it, so the dear wet and zone tests run on the winner (and the winners that fail them) only."
  [c & [{:keys [free?]}]]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        reach (settings/get settings ::reach)
        here (:pos (first (filter #(and (= :pit (:way %)) (<= (u/dist feet (:pos %)) (site-radius %))) (failed c))))
        danger-near (threats/danger-near-fn c)
        start (pit-start c)
        {:keys [x z] :as from} (or start here feet)
        y (:y (or start feet))
        columns (->> (for [dx (range (- reach) (inc reach))
                           dz (range (- reach) (inc reach))
                           :when (<= (+ (* dx dx) (* dz dz)) (* reach reach))]
                       [dx dz])
                     (sort-by (fn [[dx dz]] (+ (* dx dx) (* dz dz)))))
        fails (failed c)
        walk #(walk-pick/walk-s from {:pos %})
        lower-bounds (fn [[dx dz] n]
                       (let [cx (+ x dx)
                             cz (+ z dz)
                             w (walk {:x cx :z cz})
                             names (into {} (map (fn [d] [d (delay (u/seen-name p {:x cx :y (+ y d) :z cz}))])) (range -3 4))]
                         (keep-indexed (fn [i dy]
                                         (let [f {:x cx :y (+ y dy) :z cz}
                                               at #(deref (names (+ dy %)))]
                                           (when (and (clear-of? fails f) (standing? p #(at -1) #(at 0) #(at 1)))
                                             [(+ w (stay-toll-s c danger-near f)) n i f false])))
                                       [0 -1 1 -2 2])))]
    (loop [[col & more :as left] columns n 0 pool (sorted-set)]
      (let [[price _ _ f exact? :as best] (first pool)
            next-w (when col (walk {:x (+ x (first col)) :z (+ z (second col))}))]
        (cond
          (not (and best (or (empty? left) (<= price next-w))))
          (when (seq left) (recur more (inc n) (into pool (lower-bounds col n))))

          (not exact?)
          (recur left n (-> pool (disj best) (conj (assoc best 0 (+ price (walk-toll-s danger-near from f)) 4 true))))

          (and (pit-site? p f) (not (and free? (zone-refused? c f)))) f

          :else (recur left n (disj pool best)))))))

(defn ^:async keeps?
  "Whether a site cell m chosen earlier is still the one to walk to, given fresh (site, from the feet now): the same cell; or
  go-to's quote for m is finite and, plus its stay-toll-s, no dearer than the fresh cell's (none: m stays if it can be walked
  to), unless the feet are a pit site themselves (a walk that left the failed site behind on good ground digs there, for nothing)."
  [c m fresh]
  (cond
    (and (= :here (:at fresh)) (pit-site? (:primitives c) (sh-sense/feet (:primitives c)))) false
    (= m (:cell fresh)) true
    :else (let [danger-near (threats/danger-near-fn c)
                priced (fn ^:async priced [f] (when-let [q (await (rooms/route-cost c {:pos f :range 0}))] (+ q (stay-toll-s c danger-near f))))
                qm (await (priced m))
                qf (when (:cell fresh) (await (priced (:cell fresh))))]
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
