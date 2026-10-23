(ns jobs.explore.search
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.reach :as reach]
            [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [clojure.string :as str]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.area :as area]
            [jobs.lib.food-animals :as animals]
            [jobs.lib.crops :as crops]
            [jobs.lib.breath :as breath]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [jobs.lib.food-animals :as food-animals]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.surface-up :as surface-up]
            [jobs.lib.look :as look]
            [jobs.lib.patch :as patch]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [jobs.lib.vantage :as vantage]
            [engine.notes :as notes]))

(def doc
  "Look for :count things named :target (a block or entity name, or several) by walking legs from where the job
  starts (the origin), or from :center when given.

  A look round that finds too few and hears target animals it cannot see walks up to 4 short steps toward the sound, looking round after each.

  Each step looks around (blocks and entities with those names within :scan-radius; not again within 3 blocks of the
  last look round, nor when what it has seen already makes up :count), notes what it saw (:seen) and
  the spot (:searched) (engine.notes). Blocks the body saw earlier anywhere within the max-distance setting of the origin count as
  found too, nearest first. It ends when :count are found. Otherwise it walks the next leg with
  jobs.movement.go-to as a child (range 2). One call is the whole search; it yields (:continue) only while go-to waits
  on the world or for leg columns to load (:chunks-loading).

  Under rock a step toward an unseen column is only taken to a seen open cell (a tunnel or cave), never at feet height into unseen rock: the search then ends :distance when no tunnel leads on.

  For farm animals (cow, pig, sheep, chicken, rabbit) a body under rock first climbs out (jobs.lib.surface-up), and a leg whose stand is under rock (no sky light) is skipped (:underground), as is one whose ground lies more than ::surface-dip (8) below the feet (a pit, ravine or cave mouth): they live on the open surface.

  Legs are ::spacing apart: :spiral points on rings round the origin, the one nearest the body first (so a body in a corridor walks on), or :outward ahead along :heading (a :heading alone means :outward), then the sides, and on further ahead once those are tried or searched. A
  leg is never tried twice and never goes beyond the max-distance setting (XZ) of the origin. It is skipped (:skipped)
  when a live :searched note of any body, this one included, listing every target name lies within half its
  radius (a note for farm animals lasts only ::animal-searched-ttl-s, they wander); with :use-notes false only a note this job made counts.

  A leg needs a standing cell in its column from 12 above the feet down to :descent (40) below, read top down through leaves. An unloaded,
  wet or standless column is recorded failed (:not-loaded, :wet, :no-surface, :unseen) and the next candidate is tried
  in the same round. A loaded column not yet seen (:unseen) is walked toward in steps of up to 12 blocks (not
  counted in max-legs) and looked at again (over water the step swims, glancing ahead as it goes, so a far shore comes into sight; a step ending in water is progress, not a failure); within 3 blocks of it, it is recorded failed. An unloaded column is not tried but looked at again later. When only unloaded ones are
  left the round pauses 2 s (jobs.time.noop, holding :chunks-loading) and yields, until they load or the call's time is spent (a body just logged in has no chunks
  yet). Unloaded ones farther than ::load-reach (64) from the body (a far :center) are walked toward instead, once the ground under it is loaded. A walk go-to gives up on is recorded :unreachable. Three of those in
  a row end the search (:stuck); until a leg arrives, a leg within 60 degrees of the way one failed from where the
  body stands waits (the same obstacle likely blocks it), and the search ends :stuck when only such legs are left.
  A leg whose goal lies by a mob the body would flee is not walked (go-to :goal-danger
  :end): it is recorded :goal-dangerous, no stuck walk; a step toward an unseen column given up so leaves that column.

  With :use-notes, live :seen notes of a target within the max-distance setting count as found (:noted true, :by), an entity
  only if noted within the ::stale-s setting (it moves; an older one is looked for again), unless the body
  sees another block at that cell now, and a noted entity standing in a zone or plan of the body's own (its pens) never counts. The
  ending text lists the positions.

  The ending's data: {:found [{:what :pos :id? (the uuid) :entity-id? (the number entities and combat.attack use) :noted? :by?}] :coverage {:legs :scans
  :failed [{:pos :reason}] :skipped :farthest} :why :text} and the ending:
  - :done: :count found.
  - :nothing-possible :distance (no leg left), :no-way-up (animals that live at the surface, the body under rock and no stair up), :legs (::max-legs walked), :stuck (three failed walks in a row) or :price (with :worth, the next walk costs more than is left of it).
  - :limit-reached :job-time (the call's :spend :job-time, default ::job-time-s) or :wall-time (default ::wall-time-s); the engine ends it, with the data of done? ({:found :coverage :why}) and neither event; with :least targets found (default 1) the engine ends it :done instead.
  - A call with no :target and no :patch never runs (:bad-args).

  With :patch it looks for ground, not things: each look round (the origin first, then every leg's arrival) scans for a patch
  of the footprint (jobs.lib.patch, in slices) and ends :done at the first that has one, :found [{:what \"patch\" :pos :spots}]; notes are neither read nor written.

  Memory: :origin :legs :scans :found :tried :failed :skipped :farthest :failed-in-row :spent and :leg-price (with :worth: the prices of the walks done, and of the one under way) :stepped (the nearest a step has come to each unseen column: a step must end nearer) :blocked (the ways legs
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
   :pattern {:doc ":spiral (rings round the origin, the default) or :outward (ahead along :heading); :outward when a :heading is given" :spec #{:spiral :outward} :default nil}
   :heading {:doc "a compass heading (:north, :north-east, :east ...), for :outward (default :north)" :spec (set (keys headings)) :default nil}
   :scan-radius {:doc "how far round the body each look reaches" :spec (a/num-in 0 nil) :default 24}
   :takeable {:doc "leave out blocks a zone, claim or plan footprint bars this body from taking" :spec boolean? :default false}
   :skip-ids {:doc "entity ids (the numbers entities lists, or uuids) that do not count as found (animals the caller cannot use, e.g. a baby)" :spec (a/coll-of (a/pred "an entity id" some?)) :default nil}
   :avoid {:doc "columns [x z] whose blocks do not count as found" :spec (a/coll-of (a/pred "an [x z] column" vector?)) :default nil}
   :patch {:doc "look for a patch of ground instead of named things: {:w :h :range :depth :limit :max-step :plan :farmland?} as jobs.lib.patch scans (jobs.farm.find-spot builds it); the search ends at the first look round with one, and :found is then [{:what \"patch\" :pos :spots}]" :spec map? :default nil}
   :forage {:doc "whether the search is for food (the body hunts or picks it to eat); nil: every target is a food animal or food block. A search for wool or leather from a food animal says false" :spec boolean? :default nil}
   :use-notes {:doc "targets noted (by any body) within the max-distance setting count as found" :spec boolean? :default true}
   :worth {:doc "seconds the find is worth: before each walk its price (jobs.lib.cost/leg-price of go-to's quote, the body as it is now) is weighed against what is left of it after the walks so far; the search ends :price at the first that costs more, and ::max-legs does not bind. nil: unpriced" :spec (a/num-in 0 nil) :default nil}})

(a/defargs settings
  {::max-distance {:default 96 :doc "How far (XZ) from the origin a leg may go and a noted or remembered target still counts." :spec (a/int-in 1 nil)}
   ::max-legs {:default 32 :doc "Legs walked before the search gives up." :spec (a/int-in 0 nil)}
   ::seen-ttl-s {:default 259200 :doc "Seconds a note of a block seen lasts." :spec (a/num-in 0 nil)}
   ::stale-s {:default 60 :doc "Seconds after which an animal noted no longer counts as found by a note, and one seen earlier in this search is reported with how long ago." :spec (a/num-in 0 nil)}
   ::entity-ttl-s {:default 600 :doc "Seconds a note of an entity seen lasts." :spec (a/num-in 0 nil)}
   ::searched-ttl-s {:default 86400 :doc "Seconds a note of searched ground lasts." :spec (a/num-in 0 nil)}
   ::animal-searched-ttl-s {:default 120 :doc "Seconds a note of ground searched for farm animals lasts: they wander, so old ground is looked at again." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 600 :doc "Seconds from the first round before a search gives up." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 3600 :doc "Seconds from the first round before a search gives up, waits included." :spec (a/num-in 0 nil)}
   ::max-failed-in-row {:default 3 :doc "Failed attempts in a row before the search gives up." :spec (a/int-in 0 nil)}
   ::load-reach {:default 64 :doc "Columns still unloaded this far (XZ) from the body are walked toward, since chunks load only near it; nearer ones are waited for." :spec (a/num-in 0 nil)}
   ::wait-ms {:default 2000 :doc "How long the search waits for the world to settle, in ms." :spec (a/int-in 0 nil)}
   ::descent {:default 40 :doc "How far below its feet a leg's ground is looked for, in blocks (go-to does the descent)." :spec (a/int-in 0 nil)}
   ::surface-dip {:default 8 :doc "Farm animals live on open ground: a leg whose ground lies deeper than this below the feet (a pit, ravine or cave mouth) is skipped." :spec (a/int-in 0 nil)}
   ::rise {:default 12 :doc "How far above its feet a leg's ground is looked for, in blocks." :spec (a/int-in 0 nil)}
   ::spacing {:default 16 :doc "Blocks between legs." :spec (a/int-in 4 64)}
   ::held-angle {:default 60 :doc "A leg lies the way a failed leg went when the angle between them, seen from the body, is under this many degrees." :spec (a/num-in 0 180)}
   ::held-near {:default 4 :doc "Blocks from where a leg failed within which its way still holds others back." :spec (a/num-in 0 nil)}
   ::near-sight {:default 6 :doc "Within this many blocks a look misses no animal: a noted animal not seen there is taken as gone." :spec (a/num-in 0 nil)}
   ::leg-tries {:default 2 :doc "Walks toward one spot (any job or body) that may start before the spot is dropped as tried; counted in notes, so a walk cut off by a reflex (a drowning) counts too." :spec (a/int-in 1 nil)}
   ::leg-tried-ttl-s {:default 3600 :doc "Seconds a note of a walk started toward a spot lasts." :spec (a/num-in 0 nil)}
   ::roam-min-health {:default 10 :doc "A search that is not for food waits :low-health below this health while the body can not heal (cannot-heal?: food low, nothing edible carried), so a hurt, foodless body does not roam for wool or wood." :spec (a/num-in 0 20)}
   ::stand-cap {:default 48 :doc "Stand cells one choice reads; past it :more says candidates are left for the next step." :spec (a/int-in 0 nil)}})
(defn distance-limit
  "How far (XZ) from the origin legs go and findings count: the ::max-distance setting, or with :worth cost/search-radius of it."
  [c]
  (if-let [worth (:worth (:args c))]
    (js/Math.ceil (cost/search-radius worth (settings/get settings ::spacing)))
    (settings/get settings ::max-distance)))
(defn time-s [] (settings/get settings ::job-time-s))
(defn max-failed-in-row [] (settings/get settings ::max-failed-in-row))
(defn wait-ms [] (settings/get settings ::wait-ms))
(defn rise [] (settings/get settings ::rise))
(defn leg-spacing [] (settings/get settings ::spacing))
(defn descent [] (settings/get settings ::descent))
(def target [:target :center])

(def safe? ready/threatened)
(def defaults {:spend {:job-time ::job-time-s :wall-time ::wall-time-s}})


;; ------------------------------------------------------------------ pure

(defn targets
  "The target names as a sorted vector."
  [c]
  (let [t (:target (:args c))]
    (if (string? t) [t] (vec (sort (set t))))))

(def foraged
  "The target names of a search for food: the food animals and the food blocks."
  (into animals/food-animals (concat (keys crops/food-ripe-age) (keys crops/wild-food))))

(defn forage?
  "Whether the search is for food: the :forage arg, else every target is a food animal or food block."
  [c]
  (let [f (:forage (:args c))]
    (if (some? f)
      f
      (boolean (and (seq (targets c)) (every? foraged (targets c)))))))

(defn fit?
  "ready/fit-to-forage while the search is for food; else ready/fit?, with the higher ::roam-min-health floor while the body can not heal."
  [c]
  (cond (forage? c) (ready/fit-to-forage c)
        (ready/cannot-heal? c) (ready/fit-min (settings/get settings ::roam-min-health) c)
        :else (ready/fit? c)))

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

(defn leaves? [n] (and (string? n) (str/ends-with? n "_leaves")))

(defn open? [n]
  (and (string? n) (= :open (reach/kind-of #js {:name n}))))

(def wet-names bs/fluids)

(defn seen-at
  "The block-at of stand-cell for primitives p: [x y z] -> the name seen or remembered, :unseen for a loaded cell
  never seen, nil when the chunk is not loaded."
  [p]
  (fn [[bx by bz]]
    (let [pos {:x bx :y by :z bz}]
      (or (u/seen-name p pos) (when (u/sensed p pos) :unseen)))))

(defn stand-cell
  "Where a body could stand in column x z near feet height y, read top down from y+rise to y-descent through cells a walker passes
  (jobs.lib.reach/kind-of) and leaves (a canopy is no ground to walk to) to the first other block: {:y feet} when the two cells above it are open, else {:fail :wet} (a fluid),
  {:fail :not-loaded} (an unloaded cell on the way), {:fail :unseen} (a loaded cell not yet seen) or {:fail :no-surface}.
  block-at: [x y z] -> name, :unseen or nil."
  [block-at x z y]
  (let [y0 (js/Math.floor y)]
    (loop [h (+ y0 (rise))]
      (if (< h (- y0 (descent)))
        {:fail :no-surface}
        (let [n (block-at [x h z])]
          (cond
            (nil? n) {:fail :not-loaded}
            (= :unseen n) {:fail :unseen}
            (wet-names n) {:fail :wet}
            (or (open? n) (leaves? n)) (recur (dec h))
            (and (= :solid (reach/kind-of #js {:name n})) (open? (block-at [x (inc h) z])) (open? (block-at [x (+ h 2) z]))) {:y (inc h)}
            :else {:fail :no-surface}))))))

(defn surface-only?
  "Whether every name is an animal that spawns on lit grass at the surface: nothing is looked for underground."
  [names]
  (boolean (and (seq names) (every? food-animals/food-animals names))))

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
  body now sees something else at and the entity notes of the departed ids. Entity notes older than fresh-ms (they
  move) are left out, or with stale true are the only ones kept, minus the visited ids."
  [p ns names [ox _ oz] max-distance gone now fresh-ms visited stale]
  (let [old? #(and (:id %) (> (- now (:t %)) fresh-ms))]
    (->> ns
         (remove #(gone (:id %)))
         (filter #(and (= :seen (:kind %)) (some #{(:what %)} names)
                       (<= (xz-dist [((:pos %) 0) ((:pos %) 2)] [ox oz]) max-distance)
                       (not (outdated? p %))
                       (if stale (and (old? %) (not (visited (:id %)))) (not (old? %)))))
         (map (fn [n] (cond-> {:what (:what n) :pos (:pos n) :noted true :by (:by n)} (:id n) (assoc :id (:id n)) (:entity-id n) (assoc :entity-id (:entity-id n))))))))

;; ------------------------------------------------------------------ the body

(defn no-target
  "Never :bad-args while no target (or :patch) is named."
  [c]
  (if (and (empty? (targets c)) (not (:patch (:args c))))
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
               (when (seq names) (look/seen-blocks p {:names names :radius radius :max (* 64 (count names)) :live? true})))
          (map (fn [e] (cond-> {:what (:name e) :pos (cell-of (:pos e))} (:uuid e) (assoc :id (:uuid e)) (:id e) (assoc :entity-id (:id e))))
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

(defn aged
  "found with :ago-s (seconds) on each entity last seen more than the ::stale-s setting ago (by its notes)."
  [c found]
  (let [ns (notes/notes c)
        now (ctx/now c)
        stale-ms (* 1000 (settings/get settings ::stale-s))
        seen-at (fn [{:keys [id]}] (when id (some->> ns (filter #(and (= :seen (:kind %)) (= id (:id %)))) (map :t) seq (apply max))))]
    (mapv (fn [f] (let [t (seen-at f)] (cond-> f (and t (> (- now t) stale-ms)) (assoc :ago-s (quot (- now t) 1000))))) found)))

(defn finding-text [{:keys [pos ago-s]}]
  (str (str/join "," pos) (when ago-s (str " (last seen " (max 1 (js/Math.round (/ ago-s 60))) " min ago)"))))

(defn refused-text
  "When the legs that ended a :stuck search were all refused by the call's own floor, the sentence saying so, else nil."
  [c m]
  (let [row (take-last (:failed-in-row m 0) (:failed m))
        floor (:floor (first row))]
    (when (and floor (seq row) (every? #(= floor (:floor %)) row))
      (let [line (name (:floor floor))]
        (str "every walk was refused: the body's " line " " (js/Math.round (aget (.self (:primitives c)) line))
             " is under the call's floor " (:min floor) " (its :floor " line ")")))))

(defn finish!
  "End the call: done when found (or nothing was left to find), else the ending of why; the text and findings are the ending's data."
  [c reason why]
  (let [m (ctx/mem c)
        what (str/join "/" (targets c))
        found (aged c (:found m []))
        result (cond-> {:reason reason :found found :coverage (coverage m)
                        :text (if (= :found reason)
                                (str "found " (count found) " " what " at " (str/join ", " (map finding-text found)))
                                (str "no " what " found (" (name why) ") after " (:legs m 0) " legs, "
                                     (:scans m 0) " looks of " (:scan-radius (:args c)) " blocks round, " (:farthest m 0) " blocks out"
                                     (cond
                                       (= :price why) (str ", the next walk costs " (js/Math.round (:next-price m)) " s, more than the "
                                                           (js/Math.round (- (:worth (:args c)) (:spent m 0))) " s left of what the find is worth")
                                       (and (= :stuck why) (refused-text c m)) (str ", " (refused-text c m))
                                       (pos? (:skipped m 0)) (str ", " (:skipped m) " legs skipped as already searched"))))}
                 why (assoc :why why))]
    (case why
      nil (ctx/end c :done nil result)
      (ctx/end c :nothing-possible why result))))

(defn takeable
  "found without the blocks in :avoid columns, the blocks on the footprint of a plan the body made (its
  posts and walls are not material to cut), the noted entities standing in a zone or plan of the body's own (its pens),
  the entities of :skip-ids, and (when :takeable is on) those a zone, claim or plan footprint bars taking and the
  entities standing in a closed fenced enclosure that is not the body's own (kept animals)."
  [c found]
  (let [{:keys [takeable avoid skip-ids]} (:args c)
        avoid (set (map vec avoid))
        skip (set skip-ids)
        pens (atom [])
        kept? (fn [{:keys [pos]}]
                (and (not (access/in-own-area? c (zipmap [:x :y :z] pos)))
                     (spare/cell-enclosed? c pens (vec pos))))
        in (when takeable (access/rules-input c))
        own (access/rules-input c {:spare-own? true})]
    (cond->> found
      (seq skip) (remove #(or (skip (:id %)) (skip (:entity-id %))))
      true (remove #(and (not (:id %)) (access/own-keep own (zipmap [:x :y :z] (:pos %)))))
      true (remove #(and (:noted %) (:id %) (access/in-own-area? c (zipmap [:x :y :z] (:pos %)))))
      (seq avoid) (remove #(and (not (:id %)) (avoid [((:pos %) 0) ((:pos %) 2)])))
      takeable (remove #(and (:id %) (kept? %)))
      takeable (filter #(:ok (access/may? in (if (:id %) :take :harvest) (:pos %)))))))

(defn ^:async hear-lead!
  "Still short after a look round: walk a few blocks toward the nearest target animal the body hears but cannot see and look
  round there (jobs.lib.vantage), as a player walks toward a sound. Ends when found, when nothing is heard or after the steps."
  [c names short?]
  (let [rows (when (short?) (vantage/heard c names (:scan-radius (:args c))))]
    (when (and (seq rows) (= :next (await (vantage/step! c rows #(not (short?))))))
      (await (hear-lead! c names short?)))))

(defn ^:async scan!
  "Look round (every heading, once per spot: not again within look/near-blocks of the last look round), unless what is
  seen already makes up :count; note what was seen and the spot as searched, book the findings. In water only a glance at what it
  faces (turning to look round would leave it sinking)."
  [c names]
  (let [{:keys [scan-radius] want :count} (:args c)
        seen-ttl-s (settings/get settings ::seen-ttl-s)
        entity-ttl-s (settings/get settings ::entity-ttl-s)
        searched-ttl-s (settings/get settings (if (surface-only? names) ::animal-searched-ttl-s ::searched-ttl-s))
        seen #(vec (takeable c (sense (:primitives c) names scan-radius)))
        short-by? #(< (count (add-found (:found (ctx/mem c) []) % want)) want)
        short? #(short-by? (seen))
        first-look (seen)
        looking? (short-by? first-look)
        _ (when looking?
            (if (breath/in-water? (:primitives c))
              (look/see! c (look/search-effort) :search)
              (when-not (await (look/survey! c :near (look/search-effort))) (look/see! c (look/search-effort) :search))))
        _ (do (ctx/update-mem! c dissoc :vantage) (when looking? (await (hear-lead! c names short?))))
        sighted (if looking? (seen) first-look)
        [x y z] (here c)
        [ox _ oz] (:origin (ctx/mem c))]
    (notes/note! c (conj (mapv #(assoc % :kind :seen :ttl-ms (* 1000 (if (:id %) entity-ttl-s seen-ttl-s))) sighted)
                         {:kind :searched :what names :pos [x y z] :r scan-radius :ttl-ms (* 1000 searched-ttl-s)}))
    (ctx/update-mem! c (fn [m] (-> m
                                   (update :scans (fnil inc 0))
                                   (update :farthest (fnil max 0) (js/Math.round (xz-dist [x z] [ox oz])))
                                   (update :found #(add-found (or % []) sighted want)))))))

(defn candidates [c m]
  (let [{:keys [pattern heading]} (:args c)
        max-distance (distance-limit c)
        spacing (leg-spacing)
        [ox _ oz] (:origin m)
        [x _ z] (here c)]
    (if (or (= :outward (keyword pattern)) (and heading (not pattern)))
      (let [heading (keyword (or heading :north))
            [dx dz] (headings heading)]
        (mapcat (fn [k] (outward-points [(+ x (* dx spacing k)) (+ z (* dz spacing k))] heading spacing))
                (range (inc (js/Math.ceil (/ max-distance spacing))))))
      (sort-by #(xz-dist [x z] %) (spiral-points [ox oz] spacing max-distance)))))

(defn leg-tries
  "How many walks toward the XZ point [x z] were started and not followed by a look there (:leg-tried notes within the leg spacing's third)."
  [ns names [x z]]
  (transduce (comp (filter #(and (= :leg-tried (:kind %)) (every? (set (:what %)) names)
                                 (<= (xz-dist [x z] [((:pos %) 0) ((:pos %) 2)]) 3)))
                   (map :n))
             (completing max) 0 ns))

(defn fresh-ms [] (* 1000 (settings/get settings ::stale-s)))
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
  (let [max-distance (distance-limit c)
        [ox y oz] (here c)
        origin [((:origin m) 0) ((:origin m) 2)]
        tried (into (set (:tried m)) (map (fn [{[x _ z] :pos}] [x z])) (:deferred m))
        p (:primitives c)
        block-at (seen-at p)
        surface? (surface-only? names)
        buried? (sh-sense/buried? p)
        covering (if (false? (:use-notes (:args c))) (filter #(>= (:t %) (:t0 m 0)) ns) ns)
        underground? (fn [sy pt] (and surface? sy (or (< sy (- y (settings/get settings ::surface-dip)))
                                                      (sh-sense/buried-at? p {:x (pt 0) :y sy :z (pt 1)}))))]
    (loop [[pt & more] (candidates c m) reads 0 acc {:tried [] :failed [] :skipped 0 :held 0 :unloaded (vec (:deferred m))}]
      (cond
        (nil? pt) acc
        (>= reads (stand-cap)) (assoc acc :more true)
        (or (tried pt) (> (xz-dist pt origin) max-distance) (= pt [ox oz])) (recur more reads acc)
        (held? (:blocked m) [ox oz] pt) (recur more reads (update acc :held inc))
        (some #(notes/covers? % pt names) covering) (recur more reads (-> acc (update :tried conj pt) (update :skipped inc)))
        (>= (leg-tries ns names pt) (settings/get settings ::leg-tries)) (recur more reads (-> acc (update :tried conj pt) (update :failed conj {:pos [(pt 0) y (pt 1)] :reason :tried-before})))
        :else (let [{sy :y fail :fail} (let [r (stand-cell block-at (pt 0) (pt 1) y)] (if (underground? (:y r) pt) {:fail :underground} r))
                    failed {:pos [(pt 0) y (pt 1)] :reason fail}
                    step (when (= :unseen fail)
                          (let [s (step-toward block-at (here c) (pt 0) (pt 1))]
                            (when (and s (< (xz-dist [(s 0) (s 2)] pt) (dec (get-in m [:stepped pt] ##Inf))) (or (not buried?) (open? (block-at s)))) s)))]
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

(defn note-leg-start!
  "Note a walk toward the spot started, counting those before it: the walk may end in a reflex that drops the whole job
  (a drowning), and the next job's search then knows the spot was tried."
  [c [x y z]]
  (let [names (targets c)
        n (inc (leg-tries (notes/notes c) names [x z]))]
    (notes/note! c [{:kind :leg-tried :what (vec names) :pos [x y z] :n n :ttl-ms (* 1000 (settings/get settings ::leg-tried-ttl-s))}])))

(defn leg-args
  "The go-to args of the walk toward the leg [x y z]."
  [c [x y z]]
  (cond-> {:pos {:x x :y y :z z} :range 2 :settings {:jobs.movement.go-to.tuning/goal-danger :end}}
    (forage? c) (->> (ready/forage-walk c))
    (< (:failed-in-row (ctx/mem c) 0) (dec (max-failed-in-row))) (assoc :ways #{:walk :clear-seal})))

(defn ^:async price-leg!
  "With :worth, the price of a walk toward leg before it starts (jobs.lib.cost/leg-price of go-to's quote, the death it
  risks under rock or in the dark where the body stands), booked as :leg-price: nil when it is affordable (or unpriced,
  or no route is known: the walk fails that leg alone), :continue while go-to's quote search goes on (it resumes at the
  next call), else the :price ending."
  [c leg]
  (when-let [worth (:worth (:args c))]
    (let [p (:primitives c)
          [x y z] leg
          q (await (ctx/quote c 'jobs.movement.go-to (leg-args c leg)))
          price (cost/leg-price (cost/death-body p (ctx/view c)) q
                                {:underground? (sh-sense/buried? p) :night? (look/dark-here? p)} {:x x :y y :z z})]
      (cond
        (:estimate q) :continue
        (infinite? price) (do (ctx/update-mem! c dissoc :leg-price) nil)
        (> price (- worth (:spent (ctx/mem c) 0))) (do (ctx/update-mem! c assoc :next-price price)
                                                       (finish! c :not-found :price))
        :else (do (ctx/update-mem! c assoc :leg-price price) nil)))))

(defn ^:async walk!
  "One go-to call toward the leg (the last one before the failed legs in a row end the search may dig out); on its end book the leg arrived or failed. :continue when go-to yields, else :again
  (or the ending when the failed legs in a row end the search)."
  [c]
  (let [leg (:leg (ctx/mem c))
        [x y z] leg
        r (await (child/call-walk* c :leg (leg-args c leg)))]
    (if (= :continue r)
      :continue
      (let [{:keys [in-water why] :as data} (:data r)
            cause (contract/cause-key r)
            floor (when (and (map? (:cause r)) (:floor (:cause r))) (select-keys (:cause r) [:floor :min]))
            res (assoc data :reason cause)
            step? (some? (:leg-column (ctx/mem c)))
            arrived (and (:arrived data) (or step? (not in-water)))
            dangerous? (= :goal-dangerous (or why cause))
            in-row (cond arrived 0 dangerous? (:failed-in-row (ctx/mem c) 0) :else (inc (:failed-in-row (ctx/mem c) 0)))
            [hx _ hz] (here c)]
        (ctx/update-mem! c (fn [m] (cond-> (assoc (dissoc m :leg :leg-column :leg-price) :failed-in-row in-row)
                                     (:leg-price m) (update :spent (fnil + 0) (:leg-price m))
                                     arrived (dissoc :blocked)
                                     (and arrived (:leg-column m)) (update-in [:stepped (:leg-column m)] (fnil min ##Inf) (xz-dist [hx hz] (:leg-column m)))
                                     (not (or arrived dangerous?)) (update :blocked (fnil conj []) {:at [hx hz] :leg [x z]})
                                     (not arrived) (update :failed (fnil conj []) (cond-> {:pos [x y z] :reason (cond dangerous? :goal-dangerous in-water :wet :else :unreachable)}
                                                                                    floor (assoc :floor floor)))
                                     (and (not arrived) (not (interrupted? res)) (:leg-column m)) (-> (update :tried (fnil conj []) (:leg-column m))
                                                                              (cond-> (not dangerous?) (update :legs (fnil inc 0)))))))
        (if (>= in-row (max-failed-in-row)) (finish! c :not-found :stuck) :again)))))

(defn ^:async patch-step!
  "Patch mode: look round and scan for a patch (the origin first, then where the body stands). :again while the scan goes on,
  true when spots are found (booked as the finding), nil when this place has none."
  [c]
  (let [a (:patch (:args c))
        _ (when-not (:patch-scan (ctx/mem c)) (await (look/survey! c :near (look/search-effort))))
        [x y z] (if (zero? (:scans (ctx/mem c) 0)) (:origin (ctx/mem c)) (here c))
        far? (> (xz-dist [x z] (let [[ox _ oz] (:origin (ctx/mem c))] [ox oz])) (distance-limit c))
        spots (if far? nil (patch/scan-step! c a {:x x :y y :z z}))]
    (when-not (= :again spots)
      (let [[ox _ oz] (:origin (ctx/mem c))]
        (ctx/update-mem! c (fn [m] (cond-> (-> m (update :scans (fnil inc 0)) (update :farthest (fnil max 0) (js/Math.round (xz-dist [x z] [ox oz]))))
                                     spots (assoc :found [{:what "patch" :pos (:pos (first spots)) :spots spots}]))))))
    (if (= :again spots) :again (boolean spots))))

(defn revisit
  "The nearest takeable animal noted over the stale setting ago and not yet visited, within reach of the origin: the body
  walks there and looks before the search counts it found, nil when none."
  [c m names ns]
  (when-not (false? (:use-notes (:args c)))
    (let [p (:primitives c)
          me (here c)
          near (min (:scan-radius (:args c)) (near-sight))
          gone (if (pos? near) (departed ns (sense p names near) me near) #{})]
      (->> (noted p ns names (:origin m) (distance-limit c) gone (ctx/now c) (fresh-ms) (set (:revisited m)) true)
           (takeable c)
           (sort-by #(xz-dist [(me 0) (me 2)] [((:pos %) 0) ((:pos %) 2)]))
           first))))

(defn far-unloaded
  "The cell at the body's height under the nearest unloaded column [{:pos [x y z]}] farther than ::load-reach from the body, nil when none
  or while the ground under the body is itself unloaded (a body just logged in: its own chunks come first)."
  [c unloaded]
  (let [[bx y bz] (here c)
        reach (settings/get settings ::load-reach)]
    (when (u/sensed (:primitives c) {:x (js/Math.floor bx) :y (dec (js/Math.floor y)) :z (js/Math.floor bz)})
      (some->> unloaded
               (map (fn [{[x _ z] :pos}] [(xz-dist [bx bz] [x z]) x z]))
               (filter #(> (first %) reach))
               (sort-by first)
               first
               (#(vector (% 1) y (% 2)))))))

(defn ^:async advance!
  "Start the next leg, or end the search for want of one."
  [c names ns]
  (let [m (ctx/mem c)
        stale (revisit c m names ns)]
    (cond
      (and (surface-only? names) (sh-sense/buried? (:primitives c)))
      (case (await (surface-up/step! c))
        :continue :continue
        :out :again
        (finish! c :not-found :no-way-up))
      stale
      (or (await (price-leg! c (:pos stale)))
          (do (ctx/update-mem! c (fn [m] (-> m (update :revisited (fnil conj #{}) (:id stale)) (assoc :leg (:pos stale) :leg-column nil))))
              (await (walk! c))))
      (and (nil? (:worth (:args c))) (>= (:legs m 0) (settings/get settings ::max-legs)))
      (finish! c :not-found :legs)
      :else
      (let [{:keys [leg step column tried failed skipped unloaded more held]} (choose-leg c m names ns)
            priced (when leg (await (price-leg! c leg)))]
        (if (= :continue priced)
          :continue
          (do
            (ctx/update-mem! c (fn [m] (-> m
                                           (update :tried (fnil into []) tried)
                                           (update :failed (fnil into []) failed)
                                           (update :skipped (fnil + 0) skipped)
                                           (assoc :choosing (boolean (and more (not leg))))
                                           (assoc :deferred (when (and more (not leg)) unloaded)))))
            (cond
              leg (or priced
                      (do (ctx/update-mem! c (fn [m] (-> m (assoc :leg leg :leg-column column)
                                                         (update :legs (fnil + 0) (if step 0 1)))))
                          (note-leg-start! c leg)
                          (await (walk! c))))
              more :again
              (and (empty? unloaded) (pos? held)) (do (ctx/update-mem! c dissoc :blocked) :again)
              (empty? unloaded) (finish! c :not-found :distance)
              (far-unloaded c unloaded) (or (await (price-leg! c (far-unloaded c unloaded)))
                                            (do (ctx/update-mem! c assoc :leg (far-unloaded c unloaded) :leg-column nil)
                                                (await (walk! c))))
              :else (do (await (child/pause! c (/ (wait-ms) 1000) :chunks-loading))
                        :continue))))))))

(defn ^:async patch-round!
  "One pass of a :patch search: scan here; with none, the next leg."
  [c]
  (let [found (if (:choosing (ctx/mem c)) false (await (patch-step! c)))]
    (case found
      :again :again
      true (finish! c :found nil)
      (await (advance! c [] [])))))

(defn ^:async step!
  "Look round; end when enough is found or the legs are used up, else start the next leg."
  [c]
  (let [{:keys [use-notes] want :count} (:args c)
        max-distance (distance-limit c)
        names (targets c)
        resumed (:choosing (ctx/mem c))
        _ (when-not resumed (await (scan! c names)))
        ns (notes/notes c)
        m (ctx/mem c)
        near (min (:scan-radius (:args c)) (near-sight))
        gone (if (pos? near) (departed ns (sense (:primitives c) names near) (here c) near) #{})
        found (cond-> (cond-> (:found m)
                        (not resumed) (add-found (takeable c (remembered (:primitives c) names (:origin m) max-distance)) want))
                use-notes (add-found (takeable c (noted (:primitives c) ns names (:origin m) max-distance gone (ctx/now c) (fresh-ms) #{} false)) want))]
    (ctx/update-mem! c assoc :found found)
    (if (>= (count found) want)
      (finish! c :found nil)
      (await (advance! c names ns)))))

(defn ^:async search-leg!
  "One pass: end, walk the leg under way, or look and start the next leg. :again goes on, else a round result."
  [c]
  (ctx/update-mem! c (fn [m] (cond-> m (nil? (:origin m)) (assoc :origin (or (some-> (area/center c) cell-of) (here c)) :t0 (ctx/now c)))))
  (cond
    (:leg (ctx/mem c)) (await (walk! c))
    (:patch (:args c)) (await (patch-round! c))
    :else (await (step! c))))

(defn ^:async search-step!
  "One pass: end, walk the leg under way, or look and start the next leg. :again goes on, else a round result."
  [c]
  (let [w (when-not (:origin (ctx/mem c)) (await (area/walk-within! c (distance-limit c))))]
    (cond
      (= :there w) :again
      (= :continue w) :continue
      w w
      :else (await (search-leg! c)))))

(defn ^:async round [c]
  (await (pace/steps! c #(search-step! c))))
