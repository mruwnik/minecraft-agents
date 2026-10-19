(ns jobs.survival.extinguish
  (:require [clojure.string :as str]
            [jobs.lib.ready :as ready]
            [jobs.lib.checks :as checks]
            [jobs.lib.choose :as choose]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.look :as look]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.child :as child]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [jobs.survival.eat :as eat]
            [jobs.lib.body :as body]))

(def doc
  "Put the body out when it is on fire or in lava. One run, re-reading the world before each pass, until the body is
  neither burning nor in lava (:done). Done at once when it is not burning and no poured water is left to scoop. Each pass, in this order:
  1. With a water bucket carried and the body not in lava, pours water at the feet and notes the cell in body memory
     (:extinguish-pour, ten minutes; a jobs.blocks.use-on child), so a run cut before the scoop leaves it for the next run.
     A cell in another's zone or claim is skipped while another way exists. As a last resort it pours there anyway,
     with one extinguish.trespass-last-resort warning.
  2. Once the fire is out, scoops the water back up with the empty bucket so no source block stays.
     It looks at the poured cell, then waits (a declared :burning-wait hold) up to 10 quarter seconds for it to read as water, and up to
     8 while still burning. A pour left more than 4 blocks away is walked back to (one go-to) when within 16 blocks, else dropped.
     A scoop that fails, a pour dropped as too far, or a pour that never shows as water, is dropped and listed in the ending as :scoop-failed [{:pos :status :text}].
  3. With no bucket, on fire and with water within :water-radius, walks into the nearest water.
  4. In lava, or with no water in reach, walks to the best nearby cell within the ::step setting (4) blocks.
     It must be passable, solid underfoot and not fire, lava, magma or a campfire.
     Scoring favours distance from those hazards (up to 4 blocks) and height, and charges a small cost per block walked.
  Water also counts powder snow. Not in lava (or in lava once a walk was blocked), with a cover block carried (cobblestone, stone, dirt, ...) it first
  places one (a jobs.blocks.place child) on a lava cell next to the feet (side or below) with open air above it, so the body does not step past it. Never sand, gravel or
  other falling blocks, never in another's zone or claim, and at most 3 covers per run.
  Before each decision from a new cell it looks round (level and down) so the fire at the feet and the lava beside them are seen.
  5. On fire, not in lava, with no water in reach and no hazard within 1.5 blocks, there is nothing useful to do.
     With a real danger near (jobs.lib.danger, the hostile-near radii) it ends :condition-failed :hostile-near instead, never standing still beside it.
     Otherwise it holds still (:burning-wait): eats (a jobs.survival.eat child) when food is under 18 and food is carried
     (to keep regenerating), and waits a second per pass; still burning after 20 seconds ends :limit-reached :still-burning.
  A walk that is blocked (boxed in, perhaps by its own covers) goes through a go-to from then on (it pillars, stairs or digs
  out when shut in); a go-to still working is not a failure. Three failures (no safe cell, or a go-to that fails, with its reason) end :nothing-possible :stuck.
  Still burning after max-passes passes ends :limit-reached :still-burning. Never yields :continue (only a cut round returns it, unseen).
  The cover blocks are the call's :use :blocks (default the carried :cover fillers of jobs.lib.choose).
  Memory: writes :extinguish {:pos :cause} each pass (cap 20, one hour),
  and lava seen within the ::scan-radius setting (8) as :hazard entries (cap 50, six hours) for retreat logic.")

(a/defargs args
  {:water-radius {:doc "on fire, water within this many blocks is walked into" :spec (a/num-in 0 nil) :default 6}
   :ignore-zones? :shared})

(def hazards #{"lava" "fire" "soul_fire" "magma_block" "campfire" "soul_campfire"})
(def passable #{"air" "cave_air" "water" "short_grass" "tall_grass" "grass" "snow"})
(def unsafe-floor
  #{"air" "cave_air" "void_air" "water" "lava" "fire" "soul_fire" "magma_block" "campfire" "soul_campfire"
    "cactus" "sweet_berry_bush" "powder_snow"})

(def extinguish-policy {:cap 20 :ttl (* 60 60 1000)})
(def hazard-policy {:cap 50 :ttl (* 6 60 60 1000)})
(a/defargs settings
  {::step {:default 4 :doc "Candidate cells to walk to lie within this many blocks (horizontally) of the body." :spec (a/num-in 0 nil)}
   ::scan-radius {:default 8 :doc "Fire, lava and magma within this many blocks count as hazards." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 60 :doc "Seconds putting the body out may take." :spec (a/num-in 0 nil)}
   ::max-hazards-per-round {:default 16 :doc "Hazard cells costed per round, the nearest first." :spec (a/int-in 0 nil)}
   ::hazard-reach {:default 4 :doc "Blocks around the body in which hazards are scanned." :spec (a/int-in 0 nil)}
   ::height-bonus {:default 2 :doc "Cost credit per block a hazard cell lies above the body." :spec (a/int-in 0 nil)}
   ::walk-cost {:default 0.5 :doc "Cost per block of the walk to a hazard cell." :spec (a/num-in 0 nil)}
   ::hazard-touch {:default 1.5 :doc "Distance from the feet at which a hazard counts as touching." :spec (a/num-in 0 nil)}
   ::max-stand-waits {:default 20 :doc "One-second waits standing still before the run stops :still-burning." :spec (a/int-in 0 nil)}
   ::stand-wait-ms {:default 1000 :doc "One wait while standing still off the fire, in ms." :spec (a/int-in 0 nil)}
   ::eat-below {:default 18 :doc "Food level below which the body eats while it burns." :spec (a/int-in 0 nil)}
   ::max-covers {:default 3 :doc "Lava cells covered per run." :spec (a/int-in 0 nil)}
   ::max-return-distance {:default 16 :doc "A pour left farther than this is dropped with a warn, not walked back to." :spec (a/int-in 0 nil)}
   ::max-scoop-return-range {:default 3 :doc "go-to range for the walk back to a pour." :spec (a/int-in 0 nil)}
   ::max-pour-waits {:default 8 :doc "Waits for the fire to go out after a pour." :spec (a/int-in 0 nil)}
   ::max-water-waits {:default 10 :doc "Waits for poured water to show before it is scooped back." :spec (a/int-in 0 nil)}
   ::pour-wait-ms {:default 250 :doc "A wait for the server to put the fire out or show the poured water." :spec (a/int-in 0 nil)}
   ::max-passes {:default 60 :doc "Passes in one run before it stops :still-burning." :spec (a/int-in 0 nil)}})
(defn step-radius [] (settings/get settings ::step))
(defn scan-radius [] (settings/get settings ::scan-radius))
(defn max-hazards-per-round [] (settings/get settings ::max-hazards-per-round))
(defn hazard-reach [] (settings/get settings ::hazard-reach))
(defn height-bonus [] (settings/get settings ::height-bonus))
(defn walk-cost [] (settings/get settings ::walk-cost))
(defn hazard-touch [] (settings/get settings ::hazard-touch))

(defn max-stand-waits [] (settings/get settings ::max-stand-waits))
(defn stand-wait-ms [] (settings/get settings ::stand-wait-ms))
(defn eat-below [] (settings/get settings ::eat-below))
(def water-like ["water" "powder_snow"])
(defn max-covers [] (settings/get settings ::max-covers))

(defn max-return-distance [] (settings/get settings ::max-return-distance))
(defn max-scoop-return-range [] (settings/get settings ::max-scoop-return-range))
(defn max-pour-waits [] (settings/get settings ::max-pour-waits))
(defn max-water-waits [] (settings/get settings ::max-water-waits))

(defn body-burning? [c] (body/burning? (:primitives c)))

(def pour-policy "Body memory of the cell poured: it outlives a cut run so the next run scoops it." {:cap 1 :ttl (* 10 60 1000)})

(defn poured
  "The cell a run poured water at and has not scooped yet, or nil."
  [c]
  (some-> (ctx/latest c :extinguish-pour) :data :pos))

(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s} :use {:blocks :cover}})

(def meta {:category :reflex})

(def safe? ready/ok)

(def fit? ready/ok)


(defn done?
  "Out: not burning and no poured water left to scoop."
  [c]
  (let [out? (not (or (body-burning? c) (poured c)))]
    (checks/goal out? [:out "not burning, no water left to scoop"] [:burning "still burning or water to scoop"]
                 {:units :fires :left (if out? 0 1)})))

(defn quote-of [_c] {:uses {:job-time [0 5 60]}})

(defn floor-cell [pos] (into {} (map (fn [[k v]] [k (js/Math.floor v)])) pos))

(defn offset [pos dx dy dz] {:x (+ (:x pos) dx) :y (+ (:y pos) dy) :z (+ (:z pos) dz)})


(defn scan
  "The blocks of names the body has seen within radius, nearest first, as {:name :pos}."
  [p radius names max]
  (mapv #(select-keys % [:name :pos]) (look/seen-blocks p {:radius radius :names names :max max})))

(defn feel-feet
  "scanned plus the fire at the feet cell when the scan lacks it: the body feels the flames it stands in, in view or not."
  [p pos scanned]
  (let [feet (u/feel-name p pos)]
    (if (and (contains? hazards feet) (not-any? #(= pos (:pos %)) scanned))
      (conj scanned {:name feet :pos pos})
      scanned)))

(defn standable?
  "Feet cell and head cell passable, and the cell below solid, or the feet in water; all three seen (never a cell behind a wall)."
  [p pos]
  (let [feet (u/seen-name p pos)
        head (u/seen-name p (offset pos 0 1 0))
        below (u/seen-name p (offset pos 0 -1 0))]
    (and (contains? passable feet)
         (contains? passable head)
         (or (= "water" feet)
             (and (some? below) (not (contains? unsafe-floor below)))))))

(defn score
  "Higher is better: away from hazards, up, and close."
  [from hazard-poss cell]
  (let [near (transduce (map #(u/dist cell %)) min (hazard-reach) hazard-poss)]
    (- (+ near (* (height-bonus) (- (:y cell) (:y from))))
       (* (walk-cost) (u/dist from cell)))))

(defn best-cell
  "The standable cell within step blocks of from with the best score, never
  from itself or a cell within a block of it."
  [p from step hazard-poss]
  (let [cells (for [dx (range (- step) (inc step))
                    dz (range (- step) (inc step))
                    dy (range 0 3)
                    :let [cell (offset from dx dy dz)]
                    :when (>= (u/dist from cell) 2)
                    :when (standable? p cell)]
                cell)]
    (when (seq cells)
      (apply max-key #(score from hazard-poss %) cells))))

(defn better-cell?
  "Whether cell scores higher than staying at from."
  [from cell hazard-poss]
  (> (score from hazard-poss cell) (score from hazard-poss from)))

(defn remember-hazards!
  "Write the lava among scanned hazards that memory does not know yet."
  [c scanned]
  (let [known (into #{} (map (comp :pos :data)) (ctx/entries c :hazard))
        fresh (->> scanned
                   (filter #(= "lava" (:name %)))
                   (remove #(contains? known (:pos %)))
                   (take (max-hazards-per-round)))]
    (doseq [{:keys [pos]} fresh]
      (ctx/remember! c :hazard {:kind :lava :pos pos} hazard-policy))))

(defn hazard-near?
  "Whether any scanned hazard lies within hazard-touch blocks of the feet cell."
  [pos scanned]
  (boolean (some #(<= (u/dist pos (:pos %)) (hazard-touch)) scanned)))

(defn adjacent-lava
  "A scanned lava cell next to the feet cell (side, or the cell below), or nil."
  [pos scanned]
  (->> scanned
       (filter #(= "lava" (:name %)))
       (filter (fn [{p :pos}]
                 (and (<= (js/Math.abs (- (:x p) (:x pos))) 1)
                      (<= (js/Math.abs (- (:z p) (:z pos))) 1)
                      (<= 0 (- (:y pos) (:y p)) 1)
                      (not= p pos)
                      (<= (+ (js/Math.abs (- (:x p) (:x pos))) (js/Math.abs (- (:z p) (:z pos)))) 1))))
       first))

(defn exposed-lava
  "The scanned lava cells without a block over them; lava below the feet level under a block, such as an earlier
  cover, is no candidate. Lava at feet level is one even under an overhang: the body stands beside it."
  [p feet scanned]
  (remove (fn [{:keys [name pos]}]
            (and (= "lava" name)
                 (< (:y pos) (:y feet))
                 (let [above (u/block-name p (offset pos 0 1 0))]
                   (not (or (nil? above) (contains? passable above) (= "lava" above))))))
          scanned))

(defn cover-item
  "The cover block to place: the first of the call's :use :blocks carried, else the first :cover filler carried."
  [c]
  (let [carried (into #{} (map :name) (u/inventory (:primitives c)))]
    (or (first (filter carried (ctx/use c :blocks))) (choose/filler c :cover))))

(defn lava-refusals
  "Why each scanned lava cell is no cover candidate, as [{:pos :reason}]: a run-wide reason (:no-cover-item, :max-covers,
  :in-lava) first, else :covered-over, :not-adjacent or :social. Empty when some cell is a candidate."
  [c p pos scanned lava?]
  (let [lavas (filter #(= "lava" (:name %)) scanned)
        exposed (set (map :pos (exposed-lava p pos scanned)))
        adjacent (:pos (adjacent-lava pos (exposed-lava p pos scanned)))
        run-wide (cond
                   (and lava? (not (:blocked (ctx/mem c)))) :in-lava
                   (<= (max-covers) (:covers (ctx/mem c) 0)) :max-covers
                   (nil? (cover-item c)) :no-cover-item)]
    (mapv (fn [{lava :pos}]
            {:pos lava
             :reason (cond
                       run-wide run-wide
                       (not (contains? exposed lava)) :covered-over
                       (not= lava adjacent) :not-adjacent
                       (some? (access/trespass-refusal c :place lava)) :social)})
          lavas)))

(defn clear? [c] (not (body-burning? c)))

(defn pour-wait-ms [] (settings/get settings ::pour-wait-ms))
(defn max-passes [] (settings/get settings ::max-passes))

(defn ^:async pour-water!
  "Pour a carried water bucket at the feet. True when the water was placed (then noted in body memory)."
  [c pos]
  ;; the zone choice is made above (trespass-refusal, last resort), and the scoop must undo the pour: the child must not refuse again
  (ctx/hold-still! c nil)
  (let [placed (= :done (:kind (await (child/run!* c :bucket 'jobs.blocks.use-on {:pos pos :item "water_bucket" :ignore-zones? true}))))]
    (when placed (ctx/remember! c :extinguish-pour {:pos pos} pour-policy))
    placed))

(defn has-bucket? [p] (boolean (some #(= "water_bucket" (:name %)) (u/inventory p))))

(defn clear-pour! [c]
  (ctx/forget-where! c :extinguish-pour (constantly true))
  (ctx/update-mem! c dissoc :pour-waits :water-waits :returned))

(defn scoop-failed!
  "Notes a poured cell left in place for the ending (the pass goes on)."
  [c cell status text]
  (ctx/update-mem! c update :scoop-failed (fnil conj []) {:pos cell :status status :text text}))

(defn ^:async burning-wait!
  "Pause on purpose for ms (a noop child holding :burning-wait)."
  [c ms]
  (await (child/pause! c (/ ms 1000) :burning-wait)))

(defn ^:async scoop-pass
  "The water was poured at cell. Wait while burning (up to max-pour-waits), nil after that so the pass tries
  another way; once out, wait for the water to show (up to max-water-waits), scoop it back up and end."
  [c cell]
  (let [p (:primitives c)]
    (cond
      (body-burning? c)
      (let [waits (inc (:pour-waits (ctx/mem c) 0))]
        (if (> waits (max-pour-waits))
          (do (clear-pour! c) nil)
          (do (ctx/update-mem! c assoc :pour-waits waits)
              (await (burning-wait! c (pour-wait-ms)))
              :again)))

      (not= "water" (do (await (dig-look/look-unknown! c [[(:x cell) (:y cell) (:z cell)]])) (u/block-name p cell)))
      (let [waits (inc (:water-waits (ctx/mem c) 0))]
        (if (> waits (max-water-waits))
          (do (clear-pour! c)
              (scoop-failed! c cell "unseen" (str "the poured water at " (pr-str cell) " never showed: not scooped"))
              :done)
          (do (ctx/update-mem! c assoc :water-waits waits)
              (await (burning-wait! c (pour-wait-ms)))
              :again)))

      (and (> (u/eye-dist (u/self-pos c) cell) u/bucket-reach)
           (<= (u/dist (u/self-pos c) cell) (max-return-distance))
           (not (:returned (ctx/mem c))))
      (do (ctx/update-mem! c assoc :returned true)
          (await (child/call-walk* c :go {:pos cell :range (max-scoop-return-range) :ways #{:walk :clear-seal}}))
          :again)

      (> (u/eye-dist (u/self-pos c) cell) u/bucket-reach)
      (do (clear-pour! c)
          (scoop-failed! c cell "far" (str "left the poured water at " (pr-str cell) ": too far to scoop"))
          :done)

      :else
      (do (ctx/hold-still! c nil)
          (let [r (await (child/run!* c :bucket 'jobs.blocks.use-on {:pos cell :item "bucket" :ignore-zones? true}))
                status (if (= :continue r) "continue" (name (or (get-in r [:data :reason]) :failed)))]
            (clear-pour! c)
            (when-not (= :done (:kind r))
              (scoop-failed! c cell status (str "could not scoop the poured water at " (pr-str cell) ": " status)))
            :done)))))

(defn ^:async cover!
  "Place a cover block on the lava cell (a jobs.blocks.place child, slot :cover): true when placed, else false (a reflex
  never yields: a child that waits is no cover this pass)."
  [c lava]
  (let [r (await (child/run!* c :cover 'jobs.blocks.place {:pos lava :item (cover-item c) :fetch false :clear false}))]
    (and (not= :continue r) (= :done (:kind r)))))

(defn ^:async pour-last-resort!
  "No permitted way out is left and the feet cell is refused: pour there anyway, with the warn. :again when poured,
  else nil."
  [c pos refusal]
  (access/trespass! c "extinguish" refusal)
  (when (await (pour-water! c pos))
    :again))

(defn still-burning [c text]
  (ctx/end c :limit-reached {:line :job-time :reason :still-burning} {:text text}))

(defn ^:async stand-wait!
  "One wait off the fire: eat to keep regenerating, tell the agent once, wait a second. Stopped :still-burning after
  max-stand-waits."
  [c]
  (let [p (:primitives c)
        waits (inc (:stand-waits (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :stand-waits waits)
    (when (and (eat/carried-best c) (< (.-food (.self p)) (eat-below)))
      (await (child/run!* c :eat 'jobs.survival.eat {:until (eat-below) :max-bites 1})))
    (await (burning-wait! c (stand-wait-ms)))
    (cond
      (clear? c) :done
      (>= waits (max-stand-waits)) (still-burning c "still burning after standing still off the fire")
      :else :again)))

(defn ^:async stand-pass!
  "On fire, nothing to do but wait it out off the fire (stand-wait!). Never beside a real danger: that ends stopped
  :hostile-near, so the hostile reflex can act."
  [c]
  (if (danger-q/danger-near? (:primitives c) (danger-q/hostile-radius) (danger-q/ranged-radius))
    (ctx/end c :condition-failed :hostile-near {:text "a hostile is near: not standing still"})
    (await (stand-wait! c))))

(defn stuck!
  "One failed walk: :again until u/max-failures in a row, then stopped :stuck."
  [c text]
  (if-not (u/count-fail! c)
    :again
    (ctx/end c :nothing-possible :stuck {:text text :tries (u/max-failures)})))

(defn ^:async move!
  "An emergency step to pos (range 0). The walk's status."
  [c pos]
  (ctx/hold-still! c nil)
  ;; raw moveTo kept: an emergency step into water or out of fire (range 0), a few blocks, no time for a plan.
  (.-status (await (ctx/act c :moveTo (clj->js {:pos pos :range 0})))))

(defn ^:async escape!
  "The emergency step is blocked (boxed in, perhaps by own covers): one go-to to pos, which plans the way and, when
  shut in, pillars, stairs or digs out. :again while it works or when it arrives; a go-to that fails or is declined
  counts toward stuck!, which carries its reason; an arrival resets that count."
  [c pos]
  (ctx/hold-still! c nil)
  (ctx/update-mem! c assoc :blocked true)
  (let [e (await (child/call-walk* c :go {:pos pos :range 0}))]
    (cond
      (clear? c) :done
      (= :continue e) :again
      (and (= :done (:kind e)) (:arrived (:data e))) (do (u/progress! c) :again)
      :else (stuck! c (str "the way to a safe cell is blocked: "
                           (or (:text (:data e)) (name (contract/cause-key e))))))))

(defn ^:async pass!
  "One try at putting the body out. :again for another pass, else :done (perhaps stopped)."
  [c]
  (let [p (:primitives c)
        me (.self p)
        cell (poured c)
        scooped (when cell (await (scoop-pass c cell)))]
    (cond
      scooped scooped

      (not (body/burning? p))
      :done

      :else
      (let [{:keys [water-radius]} (:args c)
            pos (floor-cell (u/pos-of (.-pos me)))
            lava? (boolean (.-inLava me))
            _ (await (look/survey! c :cell))
            ;; lava beside the feet under an overhang shows to no glance of the survey: turn to each side cell not yet sensed
            _ (await (dig-look/look-unknown! c (for [[dx dy dz] [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 -1 0]]]
                                                 [(+ (:x pos) dx) (+ (:y pos) dy) (+ (:z pos) dz)])))
            scanned (feel-feet p pos (scan p (scan-radius) hazards 128))
            water (when-not lava? (first (scan p water-radius water-like 1)))
            pour? (and (not lava?) (has-bucket? p))
            refusal (when pour? (access/trespass-refusal c :place pos))]
        (ctx/remember! c :extinguish {:pos pos :cause (if lava? :lava :fire)} extinguish-policy)
        (remember-hazards! c scanned)
        (let [refused (lava-refusals c p pos scanned lava?)]
          (when (and (seq refused) (every? :reason refused))
            (ctx/emit! c :extinguish.no_cover :debug
                       {:text "lava scanned, no cover chosen"
                        :feet pos :in-lava lava?
                        :scanned (mapv :pos scanned)
                        :lava (mapv :pos (filter #(= "lava" (:name %)) scanned))
                        :hazards (mapv (comp :pos :data) (ctx/entries c :hazard))
                        :refused refused})))
        (cond
          (and pour? (nil? refusal) (await (pour-water! c pos)))
          :again

          water
          (do (await (move! c (:pos water))) :again)

          (and (or (not lava?) (:blocked (ctx/mem c))) (< (:covers (ctx/mem c) 0) (max-covers)) (cover-item c)
               (let [lava (:pos (adjacent-lava pos (exposed-lava p pos scanned)))]
                 (and lava
                      (nil? (access/trespass-refusal c :place lava))
                      (do (ctx/hold-still! c nil)
                          (await (cover! c lava))))))
          (do (ctx/update-mem! c update :covers (fnil inc 0))
              :again)

          (and (not lava?) (not (hazard-near? pos scanned)))
          (or (when refusal (await (pour-last-resort! c pos refusal)))
              (await (stand-pass! c)))

          :else
          (let [hazard-poss (map :pos scanned)
                target (best-cell p pos (step-radius) hazard-poss)]
            (cond
              (not target)
              (or (when refusal (await (pour-last-resort! c pos refusal)))
                  (stuck! c "no safe cell within reach"))

              ;; out of the lava a move is a must; otherwise a cell that is no better is not worth the walk back and forth
              (and (not lava?) (not (better-cell? pos target hazard-poss)))
              (await (stand-pass! c))

              :else
              (let [blocked? (:blocked (ctx/mem c))
                    status (if blocked? "blocked" (await (move! c target)))]
                (cond
                  (clear? c) :done
                  ;; in lava the first blocked step covers the lava around first (next passes): the go-to needs a step
                  (and lava? (not blocked?) (= "blocked" status)) (do (ctx/update-mem! c assoc :blocked true) :again)
                  (= "blocked" status) (await (escape! c target))
                  :else :again)))))))))

(defn ^:async round [c]
  (loop [i 0]
    (cond
      (not (ctx/alive? c)) :continue
      (<= (max-passes) i) (still-burning c "still burning after many tries")
      :else (let [r (await (pass! c))]
              (case r
                :again (do (await (pace/pace!)) (recur (inc i)))
                :done (let [failed (:scoop-failed (ctx/mem c))]
                        (ctx/end c :done nil (when (seq failed) {:scoop-failed failed :text (str/join "; " (map :text failed))})))
                r)))))
