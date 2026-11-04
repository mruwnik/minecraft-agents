(ns jobs.lib.ledger
  "The scaffold ledger: every temporary block this body placed (pillars, tunnel torches, retreat plugs and the
  like), so a cleanup can take back exactly its own blocks, even after a cut, a discarded job or a restart.

  Storage: body memory, not job memory (a discarded job's memory is gone, its blocks are not). One entry of
  kind :scaffold (cap 1, forever) with data {:entries [entry ...]}. An entry is
    {:cell [x y z] :item \"dirt\" :before \"air\" :job \"j4\" :purpose :pillar :state :intent|:placed|:removing :until ms}
  with at most one entry per cell. Every entry stays open until a cleanup drops it.

  Placing: a job writes the entry as :intent before the placing act (act saves memory before it calls the
  primitive) and confirms it (:placed) once it sees the item in the cell. An :intent left by a cut or a
  restart is decided from the cell (decide, reconcile):
  - it holds the item: placed
  - anything else: not placed, dropped
  - not loaded: undecided, kept

  Cleanup (jobs.access.cleanup): marks an entry :removing before its dig, then settles each entry it works on
  from the cell (settle-entry):
  - the item is there (dirt grown to grass_block, podzol or mycelium counts): kept as :placed
  - a :removing cell that is now air: removed
  - anything else: dropped, it is not ours any more
  - not loaded: kept
  Cells a cleanup could not take are held (kind :scaffold-held, 10 minutes) so the standing offer does not
  repeat them every tick.

  Expiry: remember! stamps each new intent with :until, ::ttl-s on (jobs.lib.expiry). A placer takes its own blocks back
  when it ends (a cleanup :of its id); a root job releases its tree's entries in its cleanup (release-root!) and keeps them
  alive while it runs and whenever a job reads them (keep-alive!, entries), so ::ttl-s is the backstop for a cleanup that never ran; past :until an entry is an orphan, whoever placed it (no :until: one already).

  The lifecycle functions are pure over the entry vector. open-entries reads it from a memory view, write!
  and remember! store it."
  (:require [jobs.lib.block-sets :as bs]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.death-spot :as death-spot]
            [jobs.lib.expiry :as expiry]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::max-below {:default 4 :doc "Blocks below the body a scaffold may lie to be walked to; the scaffold-left trigger ignores a deeper one (a cave, a pit)." :spec (a/int-in 0 nil)}
   ::roof-height {:default 4 :doc "Blocks above a scaffold block within which a solid block (not scaffold) roofs it; the scaffold-left trigger does not offer a roofed one from the open." :spec (a/int-in 0 nil)}
   ::reach {:default u/bucket-reach :doc "Cells whose centre is this close to the eye are dug from where the body stands, in blocks (jobs.access.cleanup digs them; the scaffold-left trigger walks to the rest)." :spec (a/num-in 0 nil)}
   ::ttl-s {:default 3600 :doc "Seconds a scaffold entry stays its placer's before it is an orphan (the scaffold-left trigger takes it back). Twice the longest placing call's job time (go-to, stair, build from-plan: 1800 s), its cuts allowed for." :spec (a/num-in 1 nil)}})

(defn max-below [] (settings/get settings ::max-below))
(defn roof-height [] (settings/get settings ::roof-height))
(defn dig-reach [] (settings/get settings ::reach))

(defn centre [[x y z]] [(+ x 0.5) (+ y 0.5) (+ z 0.5)])

(defn distance [a b] (apply js/Math.hypot (map - a b)))

(defn eye-of [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [x (+ y game/eye-height) z]))

(defn roofed?
  "Whether a solid block that is not a ledger cell lies within roof-height above cell."
  [{:keys [block-at ledger roof-height]} cell]
  (boolean (some (fn [dy] (let [c (rules/offset cell 0 dy 0)
                                n (block-at c)]
                            (and n (not (bs/replaceable n)) (not (contains? ledger c)))))
                 (range 1 (inc roof-height)))))

(defn submerged?
  "Whether the cell above cell holds water: the block lies under water, and a walk to within reach of it is a dive."
  [block-at cell]
  (= "water" (block-at (rules/offset cell 0 1 0))))

(defn walk-blocker
  "Why the body should not walk to the entry at cell, as {:reason ...}, or nil: :far-below (more than max-below under
  the feet), :died-near (died-near? of the cell), :submerged (water over it), :roofed (under cover where the body is not)."
  [{:keys [feet max-below died-near? block-at] :as in} {:keys [cell]}]
  (cond
    (submerged? block-at cell) {:reason :submerged}
    (> (- (get feet 1) (get cell 1)) max-below) {:reason :far-below}
    (and died-near? (died-near? cell)) {:reason :died-near}
    (and (roofed? in cell) (not (roofed? in (update feet 1 dec)))) {:reason :roofed}))

(defn died-near-of
  "A fn of a cell: whether a remembered death of c, not seen clear since, counts for it (death-spot/covers?: its killer's field, or the terrain radius)."
  [c]
  (let [spots (death-spot/uncleared c)]
    (fn [[x y z]] (boolean (some #(death-spot/covers? c % {:x x :y y :z z}) spots)))))

(def seal-clear-radius "Blocks round a goal or the feet within which go-to and leave-shelter take back the body's own seals." 3)

(def kind :scaffold)

(def policy {:cap 1 :ttl :forever :durable? true})

(defn entry-at [l cell] (first (filter #(= cell (:cell %)) l)))

(defn drop-cell [l cell] (filterv #(not= cell (:cell %)) l))

(defn intend
  "l with e as an :intent, replacing any entry at its cell."
  [l e]
  (conj (drop-cell l (:cell e)) (assoc e :state :intent)))

(defn confirm
  "l with the entry at cell marked :placed."
  [l cell]
  (mapv #(if (= cell (:cell %)) (assoc % :state :placed) %) l))

(defn decide
  "What became of an :intent entry, from the cell's block now (block-at: [x y z] -> name, nil when not loaded):
  :placed, :not-placed or :unknown."
  [{:keys [cell item]} block-at]
  (let [n (block-at cell)]
    (cond
      (nil? n) :unknown
      (= item n) :placed
      :else :not-placed)))

(defn reconcile
  "l with every :intent decided against the world: placed ones confirmed, not-placed ones dropped, unknown ones kept.
  :placed entries are left alone (a block gone since is the cleanup's business)."
  [l block-at]
  (into []
        (keep (fn [e]
                (if (not= :intent (:state e))
                  e
                  (case (decide e block-at)
                    :placed (assoc e :state :placed)
                    :not-placed nil
                    :unknown e))))
        l))

(defn cells [l] (set (map :cell l)))

(defn of-job [l job purpose] (filterv #(and (= job (:job %)) (= purpose (:purpose %))) l))

(defn respawn-bed-entry?
  "Whether e is a bed at the live respawn bed of view (either half): it is the body's spawn, not scaffold."
  [view {:keys [cell item]}]
  (boolean (and (.endsWith (str item) "_bed")
                (sh-spawn/spawn-bed-at? view (u/cell-pos cell)))))

(defn open-entries
  "The ledger in a body-memory view {:data :now} (ctx/view in a job): every entry not yet cleaned up, oldest first.
  The live respawn bed is never listed, so no cleanup or take-back digs it."
  [view]
  (into [] (remove #(respawn-bed-entry? view %))
        (:entries (:data (mem/latest view kind)))))

(defn without-cells
  "The ledger of a body-memory view, the respawn bed too, less the entries at cells; nil when none is there."
  [view cells]
  (let [all (:entries (:data (mem/latest view kind)))]
    (when (some #(contains? cells (:cell %)) all)
      (filterv #(not (contains? cells (:cell %))) all))))

(defn write!
  "Store l in body-memory store."
  [store l]
  (mem/write! store kind {:entries (vec l)} policy))

(defn remember!
  "Store l in body memory from a job's round, a new intent stamped with :until (expiry) and the :root of the job tree
  placing it. Any other entry with no :until or :root keeps the one memory holds for its cell (a caller confirms its
  own copy of the intent), or none (a cleanup's rewrite of an old entry). A root's timed entries share its latest
  :until (a placer's own, for an entry with no root), so its scaffold runs out as one, and a root still placing keeps
  what it placed before."
  [c l]
  (let [now (ctx/now c) ttl (* 1000 (settings/get settings ::ttl-s))
        stored (into {} (map (juxt :cell identity)) (:entries (:data (mem/latest (ctx/view c) kind))))
        timed (mapv (fn [e]
                      (let [s (stored (:cell e))]
                        (cond-> e
                          (and (not (:until e)) (= :intent (:state e))) (expiry/stamp now ttl)
                          (and (not (:until e)) (not= :intent (:state e)) (:until s)) (assoc :until (:until s))
                          (and (not (:root e)) (= :intent (:state e))) (assoc :root (:root c))
                          (and (not (:root e)) (not= :intent (:state e)) (:root s)) (assoc :root (:root s)))))
                    l)
        owner #(or (:root %) (:job %))
        latest (reduce (fn [m {:keys [until] :as e}] (cond-> m until (update (owner e) (fnil max until) until))) {} timed)]
    (ctx/remember! c kind {:entries (mapv #(cond-> % (:until %) (assoc :until (latest (owner %)))) timed)} policy)))

;; ------------------------------------------------------------------ cleanup

(defn begin-removal
  "l with the entry at cell marked :removing (written before the dig)."
  [l cell]
  (mapv #(if (= cell (:cell %)) (assoc % :state :removing) %) l))

(defn same-block?
  "Whether block n is the item an entry recorded: dirt left in the open turns to grass, podzol or mycelium, and still digs as dirt."
  [item n]
  (or (= item n) (and (= "dirt" item) (contains? #{"grass_block" "podzol" "mycelium"} n))))

(defn settle-entry
  "[what entry] for an entry, from its cell now: [:keep e] (the item is there, e :placed; or not loaded, e as it was),
  [:removed e] (a :removing cell that is air) or [:dropped e] (anything else, e with :found, the block there)."
  [{:keys [cell item state] :as e} block-at]
  (let [n (block-at cell)]
    (cond
      (nil? n) [:keep e]
      (same-block? item n) [:keep (assoc e :state :placed)]
      (and (= :removing state) (bs/air n)) [:removed e]
      :else [:dropped (assoc e :found n)])))

(defn settle
  "{:ledger :removed :dropped}: l with every entry for which pick? holds settled from the world (settle-entry); the
  removed and dropped entries are out of :ledger."
  [l pick? block-at]
  (reduce (fn [acc e]
            (if-not (pick? e)
              (update acc :ledger conj e)
              (let [[what e'] (settle-entry e block-at)]
                (update acc (if (= :keep what) :ledger what) conj e'))))
          {:ledger [] :removed [] :dropped []}
          l))

(defn of-instance?
  "Whether e was placed by instance id or one of its children."
  [id e]
  (let [j (str (:job e))]
    (or (= id j) (str/starts-with? j (str id "/")))))

(defn placed-by?
  "Whether e was placed by the job tree rooted at ctx c's instance, or by c's instance or its children."
  [c e]
  (or (= (:id c) (:root e)) (of-instance? (:id c) e)))

(defn release!
  "End the time of the entries c's instance and its children placed, and, when c is a root, of its whole tree
  (placed-by?): orphans now, for the scaffold-left trigger. A root calls it in its cleanup."
  [c]
  (let [l (:entries (:data (mem/latest (ctx/view c) kind))) now (ctx/now c)]
    (when (some #(placed-by? c %) l)
      (ctx/remember! c kind {:entries (mapv #(cond-> % (placed-by? c %) (expiry/expire now)) l)} policy))))

(defn release-root!
  "release! when c is the root of its tree; a job's cleanup calls it."
  [c]
  (when (= (:id c) (:root c)) (release! c)))

(defn keep-alive!
  "Stamp the entries of c's root that run out within half the ttl anew (expiry/stamp): a root still running keeps the
  scaffold its tree uses past the ttl, which is only the backstop for a cleanup that never ran."
  [c]
  (let [now (ctx/now c) ttl (* 1000 (settings/get settings ::ttl-s))
        l (:entries (:data (mem/latest (ctx/view c) kind)))
        mine? #(and (= (:root c) (:root %)) (:until %) (expiry/current? now %) (expiry/due? now ttl %))]
    (when (some mine? l)
      (ctx/remember! c kind {:entries (mapv #(cond-> % (mine? %) (expiry/stamp now ttl)) l)} policy))))

(defn entries
  "open-entries of c's body memory, after keep-alive!: a job reading the ledger for the floor or scaffold it relies on
  keeps it from running out under it."
  [c]
  (keep-alive! c)
  (open-entries (ctx/view c)))

(def held-kind :scaffold-held)

(def held-policy {:cap 1 :ttl 600000})

(defn held-cells [view] (set (:cells (:data (mem/latest view held-kind)))))

(defn near-of
  "The entries of l whose cell (its centre in x and z, at its feet height) lies within the :radius of the :pos of near ({:pos {:x :y :z} :radius n}); all of l when near is nil."
  [l near]
  (if-not near
    l
    (let [{:keys [x y z]} (:pos near)]
      (filterv (fn [{[cx cy cz] :cell}] (<= (js/Math.hypot (- (+ cx 0.5) x) (- cy y) (- (+ cz 0.5) z)) (:radius near))) l))))

(defn select
  "The entries of l a cleanup with :of of works on: :orphans, those past their :until (expiry) and not held (a dig-in's :shelter blocks stay: the dig-in's leave takes them back); :all,
  every one; an instance id, those of that instance and its children. near narrows them to a pos and radius (near-of); purposes (a set), to those of
  that :purpose."
  ([l view of] (select l view of nil nil))
  ([l view of near] (select l view of near nil))
  ([l view of near purposes]
   (cond->> (near-of (cond
                       (or (nil? of) (= :orphans of)) (let [held (held-cells view)]
                                    (filterv #(and (not= :shelter (:purpose %)) (not (expiry/current? (:now view) %)) (not (held (:cell %)))) l))
                       (= :all of) (vec l)
                       :else (filterv #(of-instance? of %) l))
                     near)
     (seq purposes) (filterv #(contains? (set purposes) (:purpose %))))))

(defn offered
  "The entries a cleanup with :of of (and :near near, :purpose purposes) would work on now: selected and loaded."
  ([view block-at of] (offered view block-at of nil nil))
  ([view block-at of near] (offered view block-at of near nil))
  ([view block-at of near purposes]
   (filterv #(some? (block-at (:cell %))) (select (open-entries view) view of near purposes))))

(defn hold!
  "Hold cells in body-memory store."
  [store cells]
  (mem/write! store held-kind {:cells (vec cells)} held-policy))

(defn remember-held!
  "Hold cells from a job's round."
  [c cells]
  (ctx/remember! c held-kind {:cells (vec cells)} held-policy))

(defn ways-of
  "The set of floors of the stairs this body cut earlier in its current dimension (memory :stair-way)."
  [c]
  (let [dim (.-dimension (.self (:primitives c)))]
    (set (mapcat #(when (= dim (:dim (:data %))) (:floors (:data %))) (ctx/entries c :stair-way)))))

(defn stair-stands
  "The set of feet cells [x y z] of the steps of the stairs this body cut (ways-of)."
  [c]
  (set (map (fn [[x y z]] [x (inc y) z]) (ways-of c))))

(def tunnel-way-policy {:cap 30 :ttl :forever})

(defn record-tunnel!
  "Remember a level tunnel this body cut (memory :tunnel-way: dim, origin feet cell [x y z], delta [dx dz], steps), so a
  later stay-level run does not take its air for a cave and go-to's own way up links it."
  [c origin delta steps]
  (when (pos? steps)
    (ctx/remember! c :tunnel-way {:dim (.-dimension (.self (:primitives c))) :origin (vec origin) :delta (vec delta) :steps steps}
                   tunnel-way-policy)))

(defn tunnel-stands
  "The set of feet cells [x y z] of the level tunnels this body cut in its current dimension (memory :tunnel-way)."
  [c]
  (let [dim (.-dimension (.self (:primitives c)))]
    (set (for [{{[x y z] :origin [dx dz] :delta :keys [steps] :as data} :data} (ctx/entries c :tunnel-way)
               :when (= dim (:dim data))
               k (range (inc steps))]
           [(+ x (* k dx)) y (+ z (* k dz))]))))

(defn own-cells
  "The cells of the passages this body cut itself in its dimension: feet, head and the cell over the head (a step cuts it) of each step of its stairs (memory :stair-way) and of its tunnels (:tunnel-way), as [x y z]."
  [c]
  (let [dim (.-dimension (.self (:primitives c)))
        here? #(= dim (:dim (:data %)))
        stairs (for [e (ctx/entries c :stair-way) :when (here? e)
                     [x y z] (:floors (:data e)) dy [1 2 3]]
                 [x (+ y dy) z])
        tunnels (for [e (ctx/entries c :tunnel-way) :when (here? e)
                      :let [{[x y z] :origin [dx dz] :delta :keys [steps]} (:data e)]
                      k (range (inc steps)) dy [0 1]]
                  [(+ x (* k dx)) (+ y dy) (+ z (* k dz))])]
    (set (concat stairs tunnels))))

(defn way-ends
  "The own cells (own-cells) the body leaves its passages by: those of the highest step of each stair and of the first
  cell of each tunnel, as [x y z]."
  [c]
  (let [dim (.-dimension (.self (:primitives c)))
        here? #(= dim (:dim (:data %)))
        stairs (for [e (ctx/entries c :stair-way) :when (here? e)
                     :let [top (last (sort-by second (:floors (:data e))))]
                     :when top dy [1 2 3]]
                 (update (vec top) 1 + dy))
        tunnels (for [e (ctx/entries c :tunnel-way) :when (here? e) dy [0 1]]
                  (update (vec (:origin (:data e))) 1 + dy))]
    (set (concat stairs tunnels))))

(defn near-of-args
  "The :near the entries are narrowed to, {:pos :radius}: the arg (its :pos the body's position when left out), else for :orphans the body's position and
  32, so a saved trigger register whose job spec has no :near stays bounded, else nil."
  [c {:keys [near of]}]
  (cond
    near {:pos (or (:pos near) (u/self-pos c)) :radius (:radius near)}
    (or (nil? of) (= :orphans of)) {:pos (u/self-pos c) :radius 32}))
