(ns jobs.survival.breathe
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.checks :as checks]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.breath :as breath]
            [jobs.lib.choose :as choose]
            [jobs.lib.escape :as escape]
            [jobs.lib.land :as land]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.pace :as pace]
            [jobs.lib.sense :as sense]
            [jobs.lib.shore :as shore]
            [jobs.lib.util :as u]
            [jobs.survival.breathe.air :as air]))

(def doc
  "Get air when drowning or stuck inside a block, then out of the water, in one run (a reflex; never yields).
  Each pass reads the world again:
  - Drowning (in water, head under, oxygen below :min-oxygen): swim up the own column when it reaches air within
    the reach setting (10) blocks, else step sideways, at the feet's height, to a nearby column that does; else a go-to
    child to the nearest air-reaching surface within the air-radius setting (8), else to the nearest seen air below the feet within the depth setting (24)
    (the way the body came; a failed target is not tried again); else dig up through a cap within 3 blocks over the head
    when it is natural (jobs.lib.escape/natural?), not protected and not sand or gravel, one cell a pass, at most 3 digs
    (zones respected, trespass only as a last resort), with the best tool, and only when the dig fits jobs.survival.breathe.air/cap-dig-share of the air left.
    With no air seen near (or after a failed try) and :pocket, a dry pocket comes before the swim up an unseen column and
    the cap: standing on solid ground with a carried pillar block, after the go-to to seen air further out, it digs the
    two natural wall cells beside it, steps in and plugs the opening (jobs.survival.breathe-pocket; ledger purpose
    :breathe-plug; one a run, and none within 16 blocks of one made in the last 5 minutes). Afloat with every way spent it places a block under the feet (purpose :breathe-step) to
    make the pocket from, and at the last starts a cap dig that fits all the air left. Kelp, seagrass and upward bubble columns
    count as water; a downward (drag) bubble column is never swum into or crossed by a side step or a straight walk (nor
    its corner cut); a go-to child's planner rides one to its bottom and swims back up, so it routes round one when it can. Air is looked for only in cells the body has seen or looked at (perception memory), never through
    stone: it looks up the own column first, and when no block is seen over it (a dark column learns nothing) swims
    up it at once; only a seen block over the own column sends the looks to the neighbour columns.
  - Enclosed (head cell holds a suffocating block, see jobs.lib.breath): step to a side cell with room
    to stand, once; then dig the head block, the block above it if solid, and step up.
  - Surfaced and still in water: the ways out are read from what is seen; when none shows, the unseen cells that read touched are looked at (jobs.lib.sense/decide!, bounded) before the run gives up. Each pass takes the first way left: swim to the nearest shore cell (land with its
    rim at most one block above the water) within the shore-radius setting (6), another direction after each failed swim; a go-to
    child onto land within the search-radius setting (48) (at most 3 targets per spot, a failed target excludes its direction);
    then a step block: with a carried pillar block (jobs.lib.choose/fillers :climb), one placed (jobs.blocks.place child,
    zones respected) in the nearest top water cell within the shore-radius setting that has support and two air cells above,
    written to the scaffold ledger (purpose :breathe-step, for jobs.access.cleanup), at most 2 per spot; the next
    pass's shore swim climbs onto it; then a swim leg, a go-to to the farthest loaded surface water 8..leg-length (32) blocks out, outward from the run's
    start only, inside swim-range (96) of it, at most max-legs (6) legs (a leg that moved starts afresh at its end).
  Done at once when the body is neither drowning, enclosed nor surfaced but still in water. A reflex
  (level danger): it exhausts its ways before it stops and never yields :continue (only a cut round returns it, unseen). Ends :done when the head is clear
  and, after a swim, the body stands on solid ground out of the water. Else nothing-possible:
  :no-land-in-range (data :searched :swum :legs :headings-failed :step, one of :no-wall :no-blocks :refused :failed) when
  every way out of the water is spent or the run has gone 3 x the swim-range setting blocks; the searched area (the start and the
  failed headings) is remembered (:breathe-afloat, 5 min), so a refire there swims no leg the run already did;
  :no-blocks when only a step cell was left and no pillar block is carried;
  :no-air (data :air-radius, :cap why no cap was dug: :not-tried :no-cap :unseen :not-natural :protected :gravity :too-slow :dig-failed,
  :pocket why no pocket gave air: :off :made-near :no-block :afloat :no-wall :too-slow :dig-failed :step-failed :made :not-tried;
  the text names both) or :no-way-out after three failed tries in the run, or many passes. It never holds still while afloat.
  The air search is jobs.survival.breathe.air. Quote: a few seconds to a minute.
  Memory: one :breathe entry per run; :breathe-pocket per pocket made.")

(a/defargs args
  {:min-oxygen {:doc "oxygen (of 20) below which being in water with the head submerged is drowning (nil: the jobs.lib.breath/min-oxygen setting)"
                :spec (a/num-in 0 20) :default nil}
   :pocket {:doc "with no air seen, make a dry pocket: dig into the wall beside the body, step in and plug the opening with a carried block" :spec boolean? :default true}
   :ignore-zones? :shared})

(a/defargs settings
  {::shore-radius {:default 6 :doc "After surfacing, land this many blocks sideways is swum to." :spec (a/num-in 0 nil)}
   ::search-radius {:default 48 :doc "Land beyond shore-radius up to this many blocks sideways is gone to with go-to." :spec (a/num-in 0 nil)}
   ::leg-length {:default 32 :doc "Blocks one swim leg goes out at most." :spec (a/num-in 0 nil)}
   ::swim-range {:default 96 :doc "Blocks from the run's start no swim leg goes beyond." :spec (a/num-in 0 nil)}
   ::max-legs {:default 6 :doc "Swim legs one run makes at most." :spec (a/int-in 1 nil)}
   ::job-time-s {:default 120 :doc "Seconds getting air and out of the water may take." :spec (a/num-in 0 nil)}
   ::land-tries {:default 3 :doc "Go-to targets one spot tries before it swims a leg." :spec (a/int-in 0 nil)}
   ::min-leg {:default 8 :doc "Blocks a swim leg must move to count as moved." :spec (a/int-in 0 nil)}
   ::max-passes {:default 120 :doc "Passes one run makes before it stops :no-way-out, a safety net against a world that never changes." :spec (a/int-in 0 nil)}
   ::afloat-near {:default 16 :doc "Blocks around a spot in which a later run takes over the start and failed headings of an earlier stop." :spec (a/int-in 0 nil)}
   ::max-steps {:default 2 :doc "Step blocks one spot places at most." :spec (a/int-in 0 nil)}})
(defn shore-radius [] (settings/get settings ::shore-radius))
(defn search-radius [] (settings/get settings ::search-radius))
(defn leg-length [] (settings/get settings ::leg-length))
(defn swim-range [] (settings/get settings ::swim-range))
(defn max-legs [] (settings/get settings ::max-legs))
(defn land-tries [] (settings/get settings ::land-tries))
(defn min-leg [] (settings/get settings ::min-leg))
(defn max-passes [] (settings/get settings ::max-passes))
(defn afloat-near [] (settings/get settings ::afloat-near))
(def afloat-policy {:cap 5 :ttl (* 5 60 1000)})

(def breathe-policy {:cap 20 :ttl (* 60 60 1000)})


(defn land-footing?
  "The support under the feet is solid ground: not air, water, lava, fire or magma, and loaded. It is the cell below
  the feet cell, or the feet cell itself on a slab, farmland, path or carpet (feet above the cell's floor). A body
  pressed against a wall over water is onGround for a tick without it."
  [p self]
  (let [y (.. self -pos -y) fy (js/Math.floor y)
        below (u/feel-name p {:x (js/Math.floor (.. self -pos -x)) :y (if (> (- y fy) 0.001) fy (dec fy))
                               :z (js/Math.floor (.. self -pos -z))})]
    (boolean (and below (not (breath/air? below)) (not (contains? land/unsafe-below below))))))

(defn on-land?
  "Out of the water and standing on solid ground. A swim or walk that ends on a jump crest against a wall over water
  is out of the water for that moment, without a footing."
  [p]
  (let [self (.self p)]
    (and (not (.-inWater self)) (.-onGround self) (land-footing? p self))))

(defn surfaced-in-water?
  "A swim surfaced earlier in this job and the body is still in water, or not standing on land yet: a body bobbing
  at the surface or climbing out is out of the water for a moment on every crest, and one pressed against a wall
  over water is onGround without a footing. A ctx without job memory (no :view) has not surfaced."
  [c]
  (boolean (and (:view c)
                (:surfaced (ctx/mem c))
                (not (on-land? (:primitives c))))))

(defn needs-air? [c]
  (or (some? (breath/situation (:primitives c) (:min-oxygen (:args c))))
      (boolean (surfaced-in-water? c))))

(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s}})

(def meta {:category :reflex})

(def safe? ready/ok)

(def fit? ready/ok)


(defn done?
  "Air at the head and, after a swim, solid ground under the feet."
  [c]
  (let [needs (needs-air? c)]
    (checks/goal (not needs) [:air "air at the head, on solid ground"] [:no-air "still short of air or ground"]
                 {:units :cells :left (if needs 1 0)})))

(defn quote-of [_c] {:uses {:job-time [1 10 60]}})

(defn surface-pos
  "pos with y raised to the feet cell at the top of the own water column (breath/surface-in-column), so a body that has
  sunk a few blocks below the surface still finds a bank flush with it; pos itself when the column has no surface."
  [p pos reach]
  (let [cell (breath/surface-in-column p (js/Math.floor (:x pos)) (js/Math.floor (:z pos)) (js/Math.floor (:y pos)) reach)]
    (if cell (assoc pos :y (:y cell)) pos)))

(defn same-way?
  "cell lies within 45 degrees of a failed target as seen from the offset (dx, dz): a swim toward that way already
  failed (a wall), so its neighbours would too."
  [dx dz failed fx fz]
  (some (fn [t]
          (let [tx (- (:x t) fx) tz (- (:z t) fz)
                dot (+ (* dx tx) (* dz tz))]
            (and (pos? dot)
                 (>= (* dot dot) (* 0.5 (+ (* dx dx) (* dz dz)) (+ (* tx tx) (* tz tz)))))))
        failed))

(defn shore-cell?
  "A land cell a swimming body can climb onto: a side neighbour has room (feet and head cells air or water) over water
  one or two blocks below the land cell's feet, so its rim is at most one block above the water. Land behind a wall,
  or on a wall two above the water, is not one."
  [p cell]
  (some (fn [[dx dz]]
          (let [n (-> cell (update :x + dx) (update :z + dz))
                at (fn [dy] (breath/seen-water-name p (update n :y + dy)))]
            (and (breath/passable-water-or-air? (at 0)) (breath/passable-water-or-air? (at 1))
                 (or (= "water" (at -1)) (= "water" (at -2))))))
        [[1 0] [-1 0] [0 1] [0 -1]]))

(defn nearest-land
  "The nearest land cell within radius sideways of self-pos, feet y from one below to two above, that keep? accepts;
  nil if none. A failed target (a seq of cells) and the cells in its direction are skipped."
  ([p self-pos radius] (nearest-land p self-pos radius nil (constantly true)))
  ([p self-pos radius failed keep?]
   (let [fx (js/Math.floor (:x self-pos))
         fy (js/Math.floor (:y self-pos))
         fz (js/Math.floor (:z self-pos))]
     (->> (for [[dx dz] (breath/columns radius)
                :when (not (same-way? dx dz failed fx fz))
                dy [0 -1 1 2]]
            {:x (+ fx dx) :y (+ fy dy) :z (+ fz dz)})
          (remove (set failed))
          (filter #(and (shore/standable? p %) (keep? p %)))
          first))))

(def headings
  "Swim headings [dx dz], east first, then south, west, north, then the diagonals."
  [[1 0] [0 1] [-1 0] [0 -1] [1 1] [-1 1] [-1 -1] [1 -1]])

(defn hdist [a b] (js/Math.hypot (- (:x a) (:x b)) (- (:z a) (:z b))))

(defn legal-headings
  "The headings not in failed and not back toward start from pos: once the body has moved, only outward ones."
  [pos start failed]
  (let [ox (- (:x pos) (:x start)) oz (- (:z pos) (:z start))]
    (filterv (fn [[dx dz :as h]] (and (not (contains? failed h)) (>= (+ (* dx ox) (* dz oz)) 0))) headings)))

(defn after-leg
  "Job memory after a leg: one that moved starts afresh at its end (a way blocked before may be open from here), one
  that did not fails its heading."
  [m heading moved?]
  (if moved?
    (-> m (dissoc :failed-shores :failed-land :land-tries :failed-headings) (update :legs (fnil inc 0)))
    (update m :failed-headings (fnil conj #{}) heading)))

(defn initial-search
  "{:start :failed} from the latest :breathe-afloat entry within afloat-near of pos: the start of the run that stopped
  there (its swim-range still bounds the swim) and the headings it found failed; {:start nil :failed #{}} for none."
  [entries pos]
  (let [near (last (filter #(<= (hdist pos (:pos (:data %))) (afloat-near)) entries))
        d (:data near)]
    {:start (:start d) :failed (into #{} (map vec) (:headings d))}))

(defn leg-target
  "The farthest loaded surface water cell along heading, min-leg to the leg-length setting blocks out, inside swim-range of
  start; nil if none."
  [p pos start [hx hz]]
  (let [fx (js/Math.floor (:x pos)) fy (js/Math.floor (:y pos)) fz (js/Math.floor (:z pos))]
    (some (fn [k]
            (let [x (+ fx (* hx k)) z (+ fz (* hz k))
                  cell (when (<= (hdist {:x x :z z} start) (swim-range)) (breath/surface-in-column p x z fy (air/reach)))]
              (when (and cell (= "water" (u/seen-name p cell))) cell)))
          (range (leg-length) (dec (min-leg)) -1))))

(defn stop-afloat!
  "Every way out of the water is spent: remember the spot's failed headings and stop with reason (:no-land-in-range, or
  :no-blocks when a step cell was left and no block to place)."
  [c reason]
  (let [m (ctx/mem c)
        pos (u/self-pos c)
        failed (:failed-headings m #{})]
    (ctx/remember! c :breathe-afloat {:pos pos :start (:start m pos) :reason reason :headings (vec failed)} afloat-policy)
    (ctx/end c :nothing-possible reason
             {:text (str "afloat; no land within " (search-radius) " blocks or a swim of " (swim-range)
                         (when (= :no-blocks reason) ", and no block to step out on"))
              :searched (search-radius) :swum (js/Math.round (hdist pos (:start m pos))) :legs (:legs m 0)
              :headings-failed (count failed)
              :step (cond (= :no-blocks reason) :no-blocks (:step-refused m) :refused (seq (:failed-steps m)) :failed :else :no-wall)})))

(defn max-steps [] (settings/get settings ::max-steps))

(defn step-cell
  "The nearest top water cell within radius of the body that a step block can go in: water with two air cells above it
  and a neighbour to place against; not the body's own cell, not a failed one (a seq of cells). nil if none."
  [p pos radius failed]
  (let [fx (js/Math.floor (:x pos)) fy (js/Math.floor (:y pos)) fz (js/Math.floor (:z pos))
        self (u/pos-of (.-pos (.self p)))
        own {:x (js/Math.floor (:x self)) :y (js/Math.floor (:y self)) :z (js/Math.floor (:z self))}]
    (->> (for [[dx dz] (breath/columns radius) dy [0 -1]] {:x (+ fx dx) :y (+ fy dy) :z (+ fz dz)})
         (remove #(or (= own %) (contains? (set failed) %)))
         (filter #(and (= "water" (u/seen-name p %))
                       (breath/air? (u/seen-name p (update % :y inc)))
                       (breath/air? (u/seen-name p (update % :y + 2)))
                       (blocks/support? p %)))
         first)))

(defn ^:async step!
  "Place a pillar block in cell with a jobs.blocks.place child; the scaffold ledger gets the intent first and the
  settled entry after. :again either way; a refused or failed cell is not tried again."
  [c cell]
  (let [p (:primitives c)
        block-at (escape/block-at-of p)
        at [(:x cell) (:y cell) (:z cell)]
        fillers (choose/filler-names c :climb)
        args {:pos at :use {:blocks fillers} :fetch false}
        item (blocks/chosen c fillers)
        l (ledger/reconcile (ledger/open-entries (ctx/view c)) block-at)
        intended (ledger/intend l {:cell at :item item :before (block-at at) :job (:id c) :purpose :breathe-step})
        _ (ledger/remember! c intended)
        e (await (ctx/call c :step 'jobs.blocks.place args))]
    (ledger/remember! c (ledger/reconcile intended block-at))
    (cond
      (= :not-allowed (contract/cause-key e))
      (ctx/update-mem! c #(-> % (assoc :step-refused true) (update :failed-steps (fnil conj []) cell)))
      (and (= :done (:kind e)) (= item (block-at at))) (ctx/update-mem! c update :steps (fnil inc 0))
      :else (ctx/update-mem! c update :failed-steps (fnil conj []) cell))
    :again))

(defn ^:async land-by-go-to!
  "A go-to child onto land at target. :done when the body then stands on land, else :again (target excluded)."
  [c target]
  (ctx/update-mem! c (fn [m] (-> m (update :land-tries (fnil inc 0)) (update :travelled (fnil + 0) (hdist (u/self-pos c) target)))))
  (await (air/go! c :land target 0))
  (if (on-land? (:primitives c))
    :done
    (do (ctx/update-mem! c update :failed-land (fnil conj []) target)
        :again)))

(defn ^:async swim-leg!
  "One swim leg with a go-to child to target along heading; the run's memory is updated by after-leg."
  [c heading target]
  (let [before (u/self-pos c)]
    (ctx/update-mem! c update :travelled (fnil + 0) (hdist before target))
    (await (air/go! c :leg target 2))
    (ctx/update-mem! c after-leg heading (>= (hdist before (u/self-pos c)) (min-leg)))
    (if (on-land? (:primitives c)) :done :again)))

(defn ^:async head-for-land!
  "Surfaced and still in water: the first way left of shore swim (nearest shore cell within the shore-radius setting (6), another
  direction after each failure; the swim primitive with toward climbs out onto a rim the planner cannot path to), a
  go-to onto land within the search-radius setting, a swim leg, then stopped :no_land_in_range. :done when out of the water."
  [c]
  (let [p (:primitives c)
        pos (surface-pos p (u/self-pos c) (air/reach))
        _ (when-not (:start (ctx/mem c))
            (let [{:keys [start failed]} (initial-search (ctx/entries c :breathe-afloat) pos)]
              (ctx/update-mem! c assoc :start (or start (u/self-pos c)) :failed-headings failed)))
        m (ctx/mem c)
        carried? (boolean (escape/pillar-item c))
        in-budget? (< (:travelled m 0) (* 3 (swim-range)))
        ways (fn []
               (let [shore (nearest-land p pos (shore-radius) (:failed-shores m) shore-cell?)
                     land (when (and in-budget? (< (:land-tries m 0) (land-tries)))
                            (nearest-land p pos (search-radius) (:failed-land m) (constantly true)))
                     stepcell (when (< (:steps m 0) (max-steps)) (step-cell p pos (shore-radius) (:failed-steps m)))
                     legs (when (and in-budget? (< (:legs m 0) (max-legs)))
                            (some (fn [h] (when-let [t (leg-target p pos (:start m) h)] [h t]))
                                  (legal-headings pos (:start m) (:failed-headings m #{}))))]
                 {:shore shore :land land :stepcell stepcell :legs legs}))
        ;; a way out is looked for in the cells the decision read and the body never saw before the run gives up
        {:keys [shore land stepcell legs]} (await (sense/decide! c :breathe-land ways
                                                                 {:retry? (fn [w] (not (or (:shore w) (:land w)
                                                                                           (and carried? (:stepcell w)))))}))]
    (cond
      shore
      (let [r (await (ctx/act c :swim (clj->js {:toward shore})))]
        (if (or (= "landed" (air/status r)) (on-land? p))
          :done
          (do (ctx/update-mem! c update :failed-shores (fnil conj []) shore)
              :again)))

      land (await (land-by-go-to! c land))
      (and stepcell carried?) (await (step! c stepcell))
      legs (await (swim-leg! c (first legs) (second legs)))
      stepcell (stop-afloat! c :no-blocks)
      :else (stop-afloat! c :no-land-in-range))))

(defn note!
  "Write the :breathe entry once per job instance."
  [c why]
  (when-not (:noted (ctx/mem c))
    (let [self (.self (:primitives c))]
      (ctx/remember! c :breathe {:why why :pos (u/pos-of (.-pos self)) :oxygen (.-oxygen self)} breathe-policy)
      (ctx/update-mem! c assoc :noted true))))

(defn fail!
  "One failed try in this run: :again until u/max-failures, then nothing possible with cause cause."
  [c cause text]
  (if-not (u/count-fail! c)
    :again
    (ctx/end c :nothing-possible cause {:text text :tries (u/max-failures)})))

(defn air-stop-fields
  "The :no-air data from the run's memory m: a cap or pocket the run never reached is :not-tried, not :no-cap."
  [m]
  {:air-radius (air/air-radius) :cap (:cap m :not-tried) :pocket (:pocket m :not-tried)})

(defn fail-air!
  "fail! for :no-air: the stop says how far air was searched and why no pocket and no cap gave air."
  [c]
  (if-not (u/count-fail! c)
    :again
    (let [fields (air-stop-fields (ctx/mem c))
          text (str "drowning and no air within reach (none seen within " (:air-radius fields) "; pocket: " (name (:pocket fields))
                    "; cap: " (name (:cap fields)) ")")]
      (ctx/end c :nothing-possible :no-air (assoc fields :tries (u/max-failures) :text text)))))

(defn ^:async pass!
  "One look at the world and one try. :again or :held for another pass, else :done (perhaps stopped)."
  [c]
  (let [p (:primitives c)
        min-oxygen (:min-oxygen (:args c))
        why (breath/situation p min-oxygen)]
    (cond
      (and (nil? why) (surfaced-in-water? c)) (await (head-for-land! c))
      (nil? why) :done

      :else
      (let [side (when (and (= :enclosed why) (not (:side-tried (ctx/mem c))))
                   (breath/side-cell p (.self p)))]
        (ctx/hold-still! c nil)
        (note! c why)
        (if side
          (do (ctx/update-mem! c assoc :side-tried true)
              ;; raw moveTo kept: an emergency step to air or out of water (range 0), where the planner may have no standable cell; no time for a plan.
              (await (ctx/act c :moveTo (clj->js {:pos side :range 0})))
              :again)
          (let [drowning? (= :drowning why)
                ok (await (if drowning? (air/drown-out! c) (air/dig-out! c)))]
            (cond
              (nil? (breath/situation p min-oxygen)) :again
              (and drowning? ok) :again
              (= :dug ok) (do (ctx/update-mem! c dissoc :failures) :again)
              drowning? (fail-air! c)
              :else (fail! c :no-way-out "could not dig out of the block"))))))))

(defn ^:async round [c]
  (loop [i 0]
    (cond
      (not (ctx/alive? c)) :continue
      (<= (max-passes) i) (ctx/end c :nothing-possible :no-way-out {:text "still not out after many tries"})
      :else (let [r (await (pass! c))]
              (case r
                :held (recur (inc i))
                :again (do (await (pace/pace!)) (recur (inc i)))
                :done (ctx/end c :done nil)
                r)))))
