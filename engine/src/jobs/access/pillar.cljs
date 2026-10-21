(ns jobs.access.pillar
  (:require [jobs.lib.choose :as choose]
            [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.ready :as ready]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.contract :as contract]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.need :as need]
            [jobs.lib.access :as access]
            [jobs.lib.dig-look :as look]
            [jobs.lib.look :as sight]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.pace :as pace]
            [jobs.lib.spent :as spent]
            [jobs.lib.reach :as reach]
            [jobs.lib.walk :as walk]
            [jobs.lib.pillar :as pl]
            [jobs.lib.util :as u]))

(def doc
  "Pillar up :height blocks (or until the feet are at :y) from the cell the body stands in, by jump-placing one block at a time under itself.
  Done (:already before the start) when the body stands on the ground with its feet at :y or above.
  A roof over the head (one of the two cells above the feet) is dug out first; a sand or gravel column that falls into the body's cells is dug out at once, and more than max-landings landings end :ceiling with the cells clear. One call builds the whole pillar; it yields :continue only while a recentring go-to or a roof dig waits on the world.

  Every block is written to the scaffold ledger (jobs.lib.ledger, body memory) as an intent before its
  jump and confirmed when the cell is seen holding it, so a cleanup can take the pillar back after a cut or
  restart. A restart between intent and placement is decided from the cell.

  Needs: blocks for the :use {:blocks [...]} list of the call (default: the carried fillers for a pillar, dirt then
  cobblestone, jobs.lib.choose). A call starts only with some of them
  carried unless the call's :ways are only #{:carried} (then condition :too-few-blocks, with :short; a fetch that fails ends the same, the
  fetch's ending its :because) (checked at the call's start only). Quote: height x the time of a block, that many
  blocks of the first carried one, ending height up.

  Before each block it checks:
  - the whole rest of the column, feet up to the top, is permitted (no zone that bars :place, no other plan's
    footprint, a zone list loaded)
  - the body stands on a solid block
  - the two cells above the head are clear
  - a block is carried: :item, or without it dirt while any is carried, then cobblestone
  - the cell passes jobs.lib.access.rules/may-place?
  - a seen mob other than a hostile stands in the cell: it waits for it to move on (a failed jump while one does is not counted), up to
    ::mob-waits looks (an escape from danger sets it 0 in the call's :settings), then gives up :mob-in-cell (:mob its name); a hostile there is not waited on

  A knockback (a failed jump with the body airborne or more than 0.2 off its start point) is not a refusal: it waits to land,
  walks back to where it began (go-to child if off the column, then a short centring nudge) and tries again; up to 8 per pillar, then :off-column.

  Run short part way, it gives up :too-few-blocks. Every give-up ends the job, so a parent running this as a child
  reads the ending.

  Ends :done, or with a cause {:reason r ...}: :condition-failed for :too-few-blocks (:short still owed), :mob-in-cell, :ceiling (a roof that cannot be dug, or more than max-ceiling-digs blocks of it; :dig the outcome),
  :not-on-solid, :zone, :footprint, :no-zones, :not-loaded, :not-replaceable and :bad-args; :nothing-possible for
  :off-column (the body left the column it started in) and :place-failed (3 failed jumps in a row with the body in
  the column, :detail the primitive's reason). Data {:cells [[x y z] ...] :built n :height n}: :cells are this job's
  confirmed blocks, lowest first. The give-up data holds by reason
  :at :block :zone :short :detail :mob.")

(a/defargs args
  {:height {:doc "blocks to rise, 1 to 64 (the short form of :y: the feet's y at the start plus this)" :spec (a/int-in 1 64) :default 1}
   :y {:doc "the y the feet end at; nil: the start's y plus :height" :spec (a/int-in -64 320) :default nil}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::max-height {:default 64 :doc "Tallest pillar a call may ask for, in blocks." :spec (a/int-in 0 nil)}
   ::max-failures {:default 3 :doc "Failed placements before the pillar gives up." :spec (a/int-in 0 nil)}
   ::max-ceiling-digs {:default 6 :doc "Blocks a pillar digs out of the roof over its head before it gives up :ceiling (sand and gravel, which fall back, are counted by ::max-landings instead)." :spec (a/int-in 0 nil)}
   ::max-landings {:default 3 :doc "Falling blocks (sand, gravel) that may land in the pillar's own cells, each dug out at once, before it gives up :ceiling." :spec (a/int-in 0 nil)}
   ::fall-wait-ms {:default 1000 :doc "Milliseconds a pillar waits after digging a roof cell for the falling block over it to drop." :spec (a/int-in 0 nil)}
   ::mob-waits {:default 20 :doc "Looks a pillar waits for a mob to leave the cell it places into before it gives up :mob-in-cell." :spec (a/int-in 0 nil)}
   ::max-displacements {:default 8 :doc "Knockbacks (a failed jump with the body airborne or off its column) a pillar rides out before it gives up :off-column." :spec (a/int-in 0 nil)}
   ::mob-step-ms {:default 500 :doc "Wait between looks for a mob in the pillar's cell, in ms." :spec (a/int-in 1 nil)}
   ::land-step-ms {:default 50 :doc "Wait between looks for the ground after a pillar, in ms." :spec (a/int-in 0 nil)}
   ::land-max-steps {:default 20 :doc "Looks for the ground after a pillar before it goes on." :spec (a/int-in 0 nil)}
   ::block-s {:default 0.6 :doc "Mean seconds one pillar block takes (jump, place, land), for the quote." :spec (a/num-in 0 nil)}})
(defn max-height [] (settings/get settings ::max-height))

(def purpose :pillar)

(def safe? ready/safe-here-by-night)

(def fit? ready/fit?)

(def ways need/fetch-ways)

(def target [])

(def defaults {:use {:blocks :pillar} :spend {:job-time ::job-time-s}})

(defn max-failures [] (settings/get settings ::max-failures))

(defn max-displacements [] (settings/get settings ::max-displacements))

(defn land-step-ms [] (settings/get settings ::land-step-ms))

(defn land-max-steps [] (settings/get settings ::land-max-steps))

(defn access-inputs
  "The zones, claims, footprints, the body's name, the clock and the job's :ignore-zones? arg, as jobs.lib.access.rules
  takes them (jobs.lib.access/zone-input)."
  [c]
  (access/zone-input c {:ignore-zones? (:ignore-zones? (:args c)) :own-plans-ok? true}))

(def zone-keys [:zones :footprints :claims :self :now :ignore-zones?])

(defn up [[x y z] n] [x (+ y n) z])

(defn item-to-use
  "The block to place next: item when carried, without one the first of items (default: choose/fillers :pillar) carried; nil when none."
  ([item carried] (item-to-use item carried nil))
  ([item carried items]
   (if item
     (when (pos? (get carried item 0)) item)
     (first (filter #(pos? (get carried % 0)) (or items (choose/fillers :pillar)))))))

(defn permission-refusal
  "The first refusal among cells for permission alone (zone, footprint, no zone list), as the rules' refusal with :at."
  [cells {:keys [ledger] :as in}]
  (some (fn [cell]
          (let [v (rules/may-place? (merge (select-keys in zone-keys)
                                           {:block-at (constantly "air") :cell cell :feet (up cell 1) :ledger ledger}))]
            (when-not (:ok v) (assoc v :at cell))))
        cells))

(defn give-up [reason & {:as more}] (merge {:step :give-up :reason reason} more))

(defn refusal->give-up [v] (merge (dissoc v :ok) {:step :give-up}))

(defn next-step
  "The next step of a pillar, from {:feet :base :height :block-at :carried {name count} :item :zones :footprints
  :ledger #{cells}}: {:step :done}, {:step :place :cell :item} or {:step :give-up :reason ...}."
  [{:keys [feet base height block-at carried item items] :as in}]
  (let [[bx by bz] base
        [fx fy fz] feet
        top (+ by height)]
    (if-not (and (int? height) (<= 1 height (max-height)))
      (give-up :bad-args :text (str ":height must be 1 to " (max-height) ", not " (pr-str height)))
      (if (or (not= [bx bz] [fx fz]) (< fy by))
        (give-up :off-column :at feet)
        (if (>= fy top)
          {:step :done}
          (let [refusal (permission-refusal (map #(up feet %) (range (- top fy))) in)
                floor (up feet -1)
                ceiling (first (remove #(pl/clear? (block-at %)) [(up feet 1) (up feet 2)]))
                use (item-to-use item carried items)
                place (rules/may-place? (-> in (dissoc :body-cols) (assoc :cell feet :feet (up feet 1))))]
            (cond
              refusal (refusal->give-up refusal)
              (not (rules/solid-floor? block-at floor)) (give-up :not-on-solid :at floor :block (block-at floor))
              ceiling (give-up :ceiling :at ceiling :block (block-at ceiling))
              (nil? use) (give-up :too-few-blocks :short (- top fy))
              (not (:ok place)) (refusal->give-up (assoc place :at feet))
              :else {:step :place :cell feet :item use})))))))

;; ------------------------------------------------------------------ the round

(defn built
  "This job's confirmed pillar cells in ledger l, lowest first."
  [c l]
  (->> (ledger/of-job l (:id c) purpose)
       (filter #(= :placed (:state %)))
       (map :cell)
       (sort-by second)
       vec))

(defn spent-log
  "jobs.lib.spent log of this job's confirmed pillar blocks in ledger l."
  [c l]
  (->> (ledger/of-job l (:id c) purpose)
       (filter #(= :placed (:state %)))
       (sort-by (comp second :cell))
       (reduce (fn [log {:keys [item cell]}] (spent/add log item cell)) nil)))

(def nothing-possible
  "Give-ups that are no fact about the world but a pillar that could not be made: the rest end :condition-failed."
  #{:off-column :place-failed})

(defn height-of
  "The blocks to rise: :y less the base's y (the feet's before the first block), else :height."
  [c]
  (if-let [y (:y (:args c))]
    (- y (second (or (:base (ctx/mem c)) (pl/feet-cell c))))
    (:height (:args c))))

(defn result-of
  "The result data of the pillar as ledger l holds it: {:status :cells :built :height} plus its spent summary."
  [c l status]
  (let [cells (built c l)]
    (merge {:status status :cells cells :built (count cells) :height (height-of c)}
           (some-> (spent-log c l) spent/summary))))

(defn finish!
  "End the job: a give-up has a kind by reason and the step's cause; a pillar made ends :done."
  [c l step]
  (let [result (merge (result-of c l (if (= :done (:step step)) :done :stopped)) (dissoc step :step))]
    (if (= :done (:step step))
      (ctx/end c :done nil result)
      (ctx/end c (cond (= :bad-args (:reason step)) :impossible (nothing-possible (:reason step)) :nothing-possible :else :condition-failed)
               (dissoc step :step) result))))

(defn blocks-of
  "The block names the call lists for the pillar: its :use {:blocks}, before the call has started (the engine freezes
  it at the first round) the one it was given or the carried fillers."
  [c]
  (or (ctx/use c :blocks) (get-in c [:args :use :blocks]) (mapv :item (:list (choose/fillers-for c :pillar 1)))))

(defn step-now
  "next-step for the body as it stands, with ledger l, its feet and the base of its column."
  [c l feet base]
  (let [p (:primitives c)]
    (next-step (merge (access-inputs c)
                      {:feet feet :base base :height (height-of c) :block-at (access/sensed-at p blocks/hidden-guess)
                       :carried (pl/carried p) :item nil :items (blocks-of c) :ledger (ledger/cells l)}))))

(defn blocks-short
  "How many blocks a pillar of height lacks when none of items is carried, else nil. Asked of the carried list alone: the
  cells above the head are not looked at yet at the call's start, and an unseen one reads as rock."
  [carried items height]
  (when-not (item-to-use nil carried items) height))

(defn too-few-blocks
  "Later :too-few-blocks (:short blocks missing) when no listed block is carried and the call's ways allow no fetch. Run short
  part way, the round gives up with its pillar and ledger settled."
  [c]
  (let [items (blocks-of c)
        short (when (empty? (need/fetch-how c)) (blocks-short (pl/carried (:primitives c)) items (height-of c)))]
    (if short
      (contract/later :too-few-blocks (str short " blocks short") :short short :items items)
      (contract/ok :blocks "blocks enough or fetched"))))

(def can-start? too-few-blocks)

(defn on-top?
  "Whether the body stands on the ground with its feet at the pillar's top or above."
  [c]
  (let [base (or (:base (ctx/mem c)) (pl/feet-cell c))]
    (and (not (false? (.-onGround (.self (:primitives c)))))
         (>= (second (pl/feet-cell c)) (+ (second base) (height-of c))))))

(defn done?
  "Ok once the body stands with its feet at the top (:y, else the base's y plus :height); else the blocks of this job in
  the ledger against the height."
  [c]
  (let [l (ledger/open-entries (ctx/view c))
        n (count (built c l))]
    (if (on-top? c)
      (contract/ok :on-top "the feet are at the top" :units :blocks :done n :left 0 :result (result-of c l :done))
      (contract/later :building "the pillar is being built" :units :blocks :done n :left (max 0 (- (height-of c) n))))))

(defn quote-of [c]
  (let [h (max 0 (height-of c))
        s (settings/get settings ::block-s)
        carried (pl/carried (:primitives c))
        item (first (filter #(pos? (get carried % 0)) (blocks-of c)))
        [x y z] (pl/feet-cell c)]
    (cond-> {:uses {:job-time [(* h s 0.5) (* h s) (* h s 3)]} :moves-to [x (+ y h) z]}
      item (assoc-in [:uses :items item] [h h h]))))

(defn ^:async land!
  "Wait in short steps until the body is on the ground, up to land-max-steps."
  [c]
  (loop [i 0]
    (when (and (< i (land-max-steps)) (false? (.-onGround (.self (:primitives c)))))
      (await (child/pause! c (/ (land-step-ms) 1000) :landing))
      (recur (inc i)))))

(defn centred?
  "True when the body is in the cell [x z] and within walk/centre-tolerance of the point [hx hz] (where the pillar began)."
  [c [x z] [hx hz]]
  (let [{px :x pz :z} (u/self-pos c)]
    (and (= [x z] [(js/Math.floor px) (js/Math.floor pz)])
         (<= (js/Math.abs (- px hx)) (walk/centre-tolerance))
         (<= (js/Math.abs (- pz hz)) (walk/centre-tolerance)))))

(defn displaced?
  "True when the body is outside the column cell [bx _ bz] or off the point where the pillar began (a knockback). A
  body still over its column, even mid-air after a refused jump, is merely refused."
  [c [bx _ bz] home]
  (let [[fx _ fz] (pl/feet-cell c)]
    (not (and (= [bx bz] [fx fz]) (or (nil? home) (centred? c [bx bz] home))))))

(defn ^:async recentre!
  "Walk back to [hx hz], the point the pillar began at, after a shove. :in when the body is over its column again,
  :continue while a go-to is still going, :failed otherwise. A body still standing on the column (its edge) is only
  nudged to the centre; a body off it goes back by go-to first."
  [c [x y z] [hx hz]]
  (if (centred? c [x z] [hx hz])
    :in
    (let [{sx :x sz :z} (reach/standing-cell (:primitives c))
          on-column? (= [x z] [sx sz])
          r (when-not on-column?
              (await (child/call-walk* c :recentre (ready/forage-walk c {:pos {:x hx :y y :z hz} :range 0 :ways #{:walk :clear-seal}}))))]
      (if (= :continue r)
        :continue
        (do (await (walk/centre! c hx hz))
            (if (centred? c [x z] [hx hz]) :in :failed))))))

(defn mob-in-cell
  "A seen mob other than a hostile in cell: one to wait for. A hostile there is not waited on while it hits; the
  failed places end the pillar, for its caller to deal with the mob."
  [c cell]
  (pl/mob-in (remove #(= "hostile" (:kind %)) (sight/seen-entities (:primitives c) {:radius 4})) cell))

(defn mob-wait!
  "Count one wait for a mob in the cell of the next block (reset by a placed block); false once they add up to
  mob-waits."
  [c]
  (let [n (inc (:mob-waits (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :mob-waits n)
    (<= n (settings/get settings ::mob-waits))))

(defn ^:async wait-out-mob!
  "A mob stands in the cell of the next block: wait mob-step-ms for it to move on, :again; after mob-waits of it,
  give up :mob-in-cell."
  [c l {:keys [cell]} mob]
  (if (mob-wait! c)
    (do (await (child/pause! c (/ (settings/get settings ::mob-step-ms) 1000) :mob-in-cell))
        :again)
    (finish! c l (give-up :mob-in-cell :at cell :mob (:name mob)))))

(defn ^:async place!
  "Write the intent, jump-place one block, confirm it when the jump reports it (the sensed cell may lag). Three failed jumps in a row give up;
  a failure with the body shoved off or airborne is a knockback and does not count towards the three, nor does one
  with a mob in the cell (until mob-waits of such waits)."
  [c l {:keys [cell item]}]
  (let [block-at (access/sensed-at (:primitives c) nil)
        l (ledger/intend l {:cell cell :item item :before (block-at cell) :job (:id c) :purpose purpose})
        _ (ledger/remember! c l)
        r (await (ctx/act c :jumpPlace #js {:item item :count 1}))]
    (if (pos? (or (.-placed r) 0))
      (do (ledger/remember! c (ledger/confirm l cell))
          (ctx/update-mem! c #(-> % (assoc :failures 0) (dissoc :mob-waits)))
          :again)
      (let [shoved? (displaced? c (:base (ctx/mem c)) (:home (ctx/mem c)))
            _ (when shoved? (ctx/update-mem! c #(-> % (update :displaced (fnil inc 0)) (assoc :counted true))))
            failures (if (or shoved? (and (mob-in-cell c cell) (mob-wait! c))) (:failures (ctx/mem c) 0) (inc (:failures (ctx/mem c) 0)))]
        (ctx/update-mem! c assoc :failures failures)
        (if (and (< failures (max-failures)) (<= (:displaced (ctx/mem c) 0) (max-displacements)))
          :again
          (let [settled (ledger/reconcile l block-at)]
            (ledger/remember! c settled)
            (finish! c settled (if (< failures (max-failures))
                                 (give-up :off-column :at (pl/feet-cell c) :detail (.-reason r))
                                 (give-up :place-failed :at cell :detail (.-reason r))))))))))

(defn ^:async shoved-back!
  "Before a round: a body off the base column (whatever the last place did) counts one knockback and walks back
  (up to the max-displacements setting). :continue while the walk goes on, else nil."
  [c]
  (let [{:keys [stand base counted walking displaced home]} (ctx/mem c)
        [bx _ bz] base
        [fx _ fz] (pl/feet-cell c)
        off? (and base home (not (centred? c [bx bz] home)))
        displaced (cond-> (or displaced 0) (and off? (not counted) (not walking)) inc)]
    (ctx/update-mem! c #(-> % (assoc :displaced displaced) (dissoc :counted) (assoc :walking (boolean (and off? (<= displaced (max-displacements)))))))
    (when (and off? stand (<= displaced (max-displacements)))
      (let [[x y z] stand
            block-at (access/sensed-at (:primitives c) "air")
            top (or (first (filter #(pl/clear? (block-at [x % z])) (range y (+ y 3)))) y)
            r (await (recentre! c [x top z] home))]
        (when (= :continue r) :continue)))))

(defn ^:async look-up!
  "Look at each unsensed cell of the two above the feet (once), so an unseen ceiling is not taken for rock."
  [c]
  (let [feet (pl/feet-cell c)]
    (doseq [cell [(up feet 1) (up feet 2)]]
      (when (look/unknown? (:primitives c) cell)
        (await (look/look-at! c cell))))))

(defn ^:async dig-one!
  "Dig the cell at (a jobs.blocks.dig child, falling blocks accepted): the dig's outcome keyword."
  [c [x y z]]
  (blocks/dig-outcome
   (await (blocks/dig-cell-ending! c {:x x :y y :z z}
                                   (cond-> {:accept #{:falling-block} :walk false :collect false :need-drop false}
                                     (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))))

(defn ^:async dig-ceiling!
  "A diggable roof over the head is dug out (a jobs.blocks.dig child, by hand when no tool harvests it), up to
  max-ceiling-digs blocks per call (a falling block does not count: its column is one obstacle, see clear-landed!); after a dig
  with a falling block over the cell it waits for it to drop. :again, :continue while the child waits, or the :ceiling give-up
  with the dig's outcome."
  [c l {:keys [at block] :as step}]
  (let [n (:ceiling-digs (ctx/mem c) 0)
        falling? (rules/falling? block)]
    (if (and (not falling?) (>= n (settings/get settings ::max-ceiling-digs)))
      (finish! c l step)
      (let [outcome (await (dig-one! c at))
            block-at (access/sensed-at (:primitives c) nil)]
        (when-not falling? (ctx/update-mem! c update :ceiling-digs (fnil inc 0)))
        (case outcome
          :continue :continue
          (:dug :missing) (do (when (rules/falling? (block-at (up at 1)))
                                (await (child/pause! c (/ (settings/get settings ::fall-wait-ms) 1000) :falling-block)))
                              :again)
          (finish! c l (assoc step :dig outcome)))))))

(defn ^:async clear-landed!
  "A falling block (sand, gravel) that landed in the head or feet cell is dug out at once, head first, so the body never
  stands in a filled cell. Each landing counts; after max-landings it gives up :ceiling once the cells are clear. Nil when both
  cells are open, else :again, :continue or the ending."
  [c l feet]
  (let [block-at (access/sensed-at (:primitives c) nil)]
    (when-let [cell (first (filter #(rules/falling? (block-at %)) [(up feet 1) feet]))]
      (let [landed (inc (:landed (ctx/mem c) 0))]
        (ctx/update-mem! c assoc :landed landed)
        (if (> landed (settings/get settings ::max-landings))
          (do (await (dig-one! c cell))
              (finish! c l (give-up :ceiling :at cell :block (block-at cell) :dig :landed)))
          (if (= :continue (await (dig-one! c cell))) :continue :again))))))

(defn ^:async step!
  "One block of the pillar (or its end): :again, :continue while a recentring go-to waits, or the ending."
  [c]
  (await (land! c))
  (if (= :continue (await (shoved-back! c)))
    :continue
    ;; look from the column, after any walk back: cells seen from the shoved cell are not the column's
    (let [_ (await (look-up! c))
          p (:primitives c)
          seen (ledger/open-entries (ctx/view c))
          l (ledger/reconcile seen (access/sensed-at p nil))
          feet (pl/feet-cell c)
          base (or (:base (ctx/mem c)) feet)
          landed (await (clear-landed! c l feet))
          step (when-not landed (step-now c l feet base))]
      (when (not= l seen) (ledger/remember! c l))
      (ctx/update-mem! c #(-> % (assoc :base base :stand feet) (update :home (fn [h] (or h (let [{:keys [x z]} (u/self-pos c)] [x z]))))))
      (cond
        landed landed

        (= :place (:step step))
        (if-let [mob (mob-in-cell c (:cell step))]
          (await (wait-out-mob! c l step mob))
          (await (place! c l step)))

        (= :ceiling (:reason step)) (await (dig-ceiling! c l step))
        :else (finish! c l step)))))

(defn ^:async fetch-first!
  "Before the first block, outside the urgent section: walk back after a fetch, and with no listed block carried get
  them (jobs.lib.need). :continue while that goes on, the give-up when it fails, else nil. Cut mid-pillar it is skipped."
  [c]
  (let [back (await (need/return! c))
        short (when (and (seq (need/fetch-how c)) (not (:base (ctx/mem c))))
                (blocks-short (pl/carried (:primitives c)) (blocks-of c) (height-of c)))]
    (cond
      back back
      (nil? short) nil
      :else (let [e (await (need/blocks! c (or (seq (blocks-of c)) (choose/filler-names c :pillar)) short {:return? true :how (need/fetch-how c)}))]
              (cond
                (= :continue e) :continue
                (= :done (:kind e)) nil
                :else (finish! c (ledger/open-entries (ctx/view c)) (give-up :too-few-blocks :short short)))))))

(defn ^:async round
  "The whole pillar: step! until it ends, a pace between blocks, as one urgent section (ctx/atomic :pillar): a night or
  low-food reflex waits for the climb to end, up to its cap."
  [c]
  (or (await (fetch-first! c))
      (await (ctx/atomic c :urgent :pillar #(pace/steps! c (fn [] (step! c)))))))

(def meta {:position-bound? true})
