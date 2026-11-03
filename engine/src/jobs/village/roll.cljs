(ns jobs.village.roll
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.blocks :as b]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.villager :as villager]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.step-off :as step-off]
            [jobs.village.trade :as trade]))

(def doc
  "Re-roll the offers of an untraded villager (:villager uuid; nil: the nearest adult, not a nitwit, in sight)
  until one gives :want (with :enchant, an enchanted book of that enchantment, e.g. \"mending\"; :enchant may be a map
  name -> minimum level, {\"mending\" 1 \"unbreaking\" 3}: the first offer with any of them stops the roll, and the done text names it), by breaking and placing
  again the workstation of :profession at :pos.
  Needs: a profession, a wanted item (or enchantment) and a cell the body may break and place at (else impossible, :bad-args); a
  cell not yet seen is looked at, then walked to. An offer matches when it gives :want, is not sold out and costs at
  most :max-price (nil: no limit). A villager seen before that steps out of view is waited for (three fruitless looks, then :gone). Says which villager it picked (info roll.villager). Waits (:continue) for the villager to lose
  and take its job until the call's :wall-time; villagers take jobs only by day, so at night the wait's reason is
  :night-villagers-take-no-job. :ignore-zones? goes to the dig and place children; :ways (default all) goes to the place child, so #{:carried} places only a workstation already carried (else it waits :need, nothing is fetched or crafted). Re-reads the world every step, so a restart finds a missing
  workstation and places it first; the cleanup places one the call broke however it ends.
  Returns {:found true|false :rolls n :item workstation :offer {:index :cost :gives ..}}: done when an offer
  matches, else
  - condition-failed: :gone, :baby, :locked (an offer was already used, or the villager is above level 1: it has traded), :occupied (another block at :pos),
    :cell-unseen.
  - impossible: :nitwit.
  - nothing-possible: :no-match (::max-rolls, 20, rolls without a match), :unreachable, :window (offers could not be
    read three times), :place-failed (no room to step off the cell), :workstation-lost (the broken workstation's drop is not carried after the body walked to it and to its far side and waited a few polls; nothing is placed, the data names :drop-cell, the cell the drop lies in, and no block above the cell is ever dug), or the ending of the dig or place child.")

(a/defargs args
  {:villager {:doc "the villager's entity uuid or numeric entity id" :spec villager/id? :default nil}
   :pos {:doc "the workstation block, [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :profession {:doc "villager profession whose workstation block is broken and placed again, e.g. librarian" :spec string? :default nil}
   :want {:doc "item name an offer is to give; nil with :enchant: enchanted_book" :spec a/item? :default nil}
   :enchant {:doc "enchantment an offer's item is to carry: a name (e.g. mending, any level) or a map name -> minimum level, e.g. {\"mending\" 1 \"unbreaking\" 3}; an offer with any one of them matches" :spec (a/or-of string? (a/map-of string? (a/int-in 1 nil))) :default nil}
   :max-price {:doc "highest price (first cost stack); nil for no limit" :spec (a/int-in 0 nil) :default nil}
   :ignore-zones? :shared})

(def ways need/fetch-ways)

(def workstations
  {"librarian" "lectern" "farmer" "composter" "cleric" "brewing_stand" "armorer" "blast_furnace" "butcher" "smoker"
   "cartographer" "cartography_table" "fisherman" "barrel" "fletcher" "fletching_table" "leatherworker" "cauldron"
   "mason" "stonecutter" "shepherd" "loom" "toolsmith" "smithing_table" "weaponsmith" "grindstone"})

(a/defargs settings
  {::max-rolls {:default 20 :doc "Rolls (breaks of the workstation) without a match before the call gives up (:no-match)." :spec (a/int-in 1 200)}
   ::poll-s {:default 2 :doc "Seconds between looks while waiting for the villager to lose or take its job." :spec (a/int-in 1 60)}
   ::away-s {:default 30 :doc "Seconds a villager seen before may stay out of view (after the workstation is placed) before the call ends :gone." :spec (a/int-in 1 nil)}
   ::wall-time-s {:default 3600 :doc "Seconds one call may last from its start, the waits for the villager's job included (:limit-reached)." :spec (a/int-in 1 nil)}
   ::reach {:default 2 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}
   ::leg-s {:default 15 :doc "Seconds one walk leg may take." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 1800 :doc "Seconds one search may take." :spec (a/int-in 1 nil)}})
(defn max-rolls [] (settings/get settings ::max-rolls))
(defn poll-s [] (settings/get settings ::poll-s))
(defn away-s [] (settings/get settings ::away-s))
(defn reach [] (settings/get settings ::reach))
(defn leg-s [] (settings/get settings ::leg-s))

(defn item-of
  "The workstation item of :profession."
  [args]
  (workstations (:profession args)))

(defn want-of [args] (or (:want args) (when (:enchant args) "enchanted_book")))

(def target [:villager :pos])

(defn unfinished?
  "Whether the call has broken the workstation or lifted the slab and not yet put it back."
  [c]
  (let [m (ctx/mem c)] (boolean (or (:broken m) (:slab m)))))

(defn safe-roll
  "safe-at-by-night the cell, but a sound alone holds nothing (threatened-seen: nobody judges it while the reflex is muted) and
  nothing holds while the workstation or slab is out: the call puts it back first."
  [base]
  (with-meta
    (fn [c]
      (let [r (base c)]
        (cond
          (= :ok (first r)) r
          (unfinished? c) (contract/ok :unfinished "the workstation is put back first")
          (and (= :hostile-near (:reason (nth r 2))) (= :ok (first (ready/threatened-seen c))))
          ((::ready/exposure (meta base)) c)
          :else r)))
    (meta base)))

(def safe? (price/staying :work (safe-roll (ready/safe-at-by-night (fn [c] (a/cell (:pos (:args c))))))))
(def fit? ready/fed-fit?)


(def defaults {:spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(defn bad-args
  "Never :bad-args when a cell, a profession with a workstation and a wanted item (or enchantment) are not given."
  [c]
  (let [{:keys [villager]} (:args c)
        {:keys [error]} (b/parse (:args c))]
    (cond
      (not (and (or (nil? villager) (villager/id? villager)) (string? (item-of (:args c))) (string? (want-of (:args c)))))
      (contract/never :bad-args "needs a known profession and a wanted item (or enchantment)"
                      :why "needs a known profession and a wanted item (or enchantment)")
      error (contract/never :bad-args error :why error)
      :else (contract/ok :args "a cell, a profession and a wanted item"))))

(defn start-facts
  "With a named villager in view: never :nitwit (never a job), later :baby for a baby and :occupied when another block
  stands at :pos. The rest is found out by the rounds."
  [c]
  (let [{:keys [villager]} (:args c)
        p (:primitives c)
        e (when (villager/id? villager) (villager/find-by-uuid p villager))
        block (when e (some->> (:pos (b/parse (:args c))) (u/seen-name p)))
        item (item-of (:args c))]
    (cond
      (nil? e) (contract/ok :villager "no villager in view to judge")
      (= "nitwit" (.-profession e)) (contract/never :nitwit "a nitwit has no job")
      (.-baby e) (contract/later :baby "the villager is a baby")
      (and block (not (bs/air block)) (not= item block)) (contract/later :occupied (str block " stands at the workstation cell") :block block)
      :else (contract/ok :villager "the villager can take a job"))))

(defn cell-unseen
  "Look :cell-unseen while the workstation cell is not in view or memory (after a look still unseen is the round's to
  find out)."
  [c]
  (let [pos (:pos (b/parse (:args c)))]
    (if (and pos (nil? (u/seen-name (:primitives c) pos)) (not (look/surveyed? c :near)))
      (contract/look :cell-unseen "the workstation cell is not in view or memory")
      (contract/ok :cell "the workstation cell is seen or looked at"))))

(def can-start? (cond/and bad-args start-facts cell-unseen))

(defn rolls [c] (:rolls (ctx/mem c) 0))

(defn data-of
  "The call's data: found with the offer once one matched, the rolls and the item."
  [c extra]
  (let [offer (:offer (ctx/mem c))]
    (merge {:found (boolean offer) :rolls (rolls c) :item (item-of (:args c))}
           (when offer {:offer offer})
           extra)))

(defn done?
  "Ok once an offer matched (found! books it), with the offer as its result; later while the rolls go on."
  [c]
  (let [data [:units :offers :done (if (:offer (ctx/mem c)) 1 0) :left (if (:offer (ctx/mem c)) 0 1) :result (data-of c nil)]]
    (if (:offer (ctx/mem c))
      (apply contract/ok :found (str "found " (or (:matched (ctx/mem c)) (want-of (:args c))) " after " (rolls c) " rolls") data)
      (apply contract/later :rolling (str (rolls c) " rolls without " (want-of (:args c))) data))))

(defn quote-of [c]
  {:uses {:job-time [0 (* 30 (max-rolls)) ##Inf]}})

(def failed-conditions "Causes that end condition-failed: the villager or the cell is not right now. The rest are nothing-possible."
  #{:gone :baby :locked :occupied :cell-unseen})

(defn stop!
  "End with reason (a keyword) as the cause: condition-failed for those in failed-conditions, else nothing-possible."
  [c reason]
  (ctx/end c (cond (= :nitwit reason) :impossible (failed-conditions (:reason reason reason)) :condition-failed :else :nothing-possible)
           reason (data-of c nil)))

(defn found!
  "Book the matching offer; done? ends the job."
  [c offer]
  (let [e (some->> (:enchant (:args c)) trade/enchant-wants (#(trade/carries % (:gives offer))))]
    (ctx/update-mem! c assoc :offer (select-keys offer [:index :cost :gives])
                     :matched (when e (str (:name e) " " (:level e)))))
  :again)

(defn not-nitwit? [e] (not= "nitwit" (.-profession e)))

(defn pick!
  "The villager rolled: :villager, else the nearest adult that is no nitwit (saying which one it picked, once)."
  [c]
  (let [before (villager/chosen c)
        id (villager/pick! c not-nitwit?)]
    (when (and id (nil? before))
      (let [e (villager/find-by-uuid (:primitives c) id)]
        (ctx/emit! c :roll.villager :info
                   {:villager id :pos (some-> e (.-pos) u/pos-of) :profession (some-> e (.-profession))
                    :text (str "rolls villager " id ": the nearest adult that is no nitwit")})))
    id))

(defn ^:async wait-job!
  "Hold still for a poll, saying why the villager's job is awaited; :continue. At night the reason says villagers
  claim workstations only in the day."
  [c why]
  (await (child/pause! c (poll-s) (if (and (= why :villager-takes-no-job) (sh-sense/night? (:primitives c)))
                                    :night-villagers-take-no-job
                                    why)))
  :continue)

(defn ^:async walk!
  "One go-to leg toward the villager at pos. :again, :continue or the stop."
  [c pos]
  (let [r (await (child/call-chase* c :walk
                                   {:pos pos :range (reach) :leg-s (leg-s) :walk {:ways #{:walk :clear-seal}}}))]
    (cond
      (= :continue r) :continue
      (let [res (:data r)] (or (:arrived res) (:nearer res))) :again
      :else (stop! c :unreachable))))

(defn ^:async approach!
  "The workstation cell is not in view or memory: walk to it (go-to child, slot :cell) once; if it is still unseen
  there, end :cell-unseen. :again, :continue or the stop."
  [c pos]
  (if (:approached (ctx/mem c))
    (stop! c :cell-unseen)
    (let [r (await (child/call-walk* c :cell {:pos pos :range (reach) :ways #{:walk :clear-seal}}))]
      (cond
        (= :continue r) :continue
        (:arrived (:data r)) (do (ctx/update-mem! c assoc :approached true) :again)
        :else (stop! c :unreachable)))))

(defn ^:async read-offers!
  "In reach of the villager: read its offers and decide. :found, :locked, :miss (no match), or :again/:continue/stop."
  [c]
  (let [{:keys [max-price enchant]} (:args c)
        villager (villager/chosen c)
        want (want-of (:args c))
        r (await (ctx/act c :trade (clj->js {:villager villager :op "offers"})))
        status (.-status r)]
    (case status
      "ok" (let [offers (js->clj (.-offers r) :keywordize-keys true)
                 {:keys [offer]} (trade/choose offers {:buy want :max-price max-price :enchant enchant})]
             (u/progress! c)
             (trade/emit-offers! c :roll.offers villager r offers)
             (cond
               (or (some #(pos? (:uses % 0)) offers) (< 1 (or (.-level r) 1))) :locked
               offer {:found offer}
               :else :miss))
      "gone" (stop! c (villager/gone-cause c))
      "out-of-reach" :again
      (if (u/count-fail! c)
        (ctx/end c :nothing-possible :window (data-of c nil))
        :continue))))

(defn ending-or-wait?
  "Whether a child! result is :continue or a stop, not the child's result."
  [r]
  (or (keyword? r) (contract/ending? r)))

(defn ^:async back-off!
  "Step three blocks back from the workstation cell along the side the body stands on, so a mob that wants out of the cell
  is not held in by the body at its mouth. :continue or nil."
  [c pos]
  (let [{:keys [x z]} (u/self-pos c)
        dx (- x (+ (:x pos) 0.5))
        dz (- z (+ (:z pos) 0.5))
        [sx sz] (if (>= (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0] [0 (js/Math.sign dz)])
        r (await (child/call-walk* c :back-off {:pos {:x (+ (:x pos) (* 3 sx)) :y (:y pos) :z (+ (:z pos) (* 3 sz))}
                                               :range 1 :ways #{:walk}}))]
    (when (= :continue r) :continue)))

(defn ^:async child!
  "Run a dig or place child in a slot of its own per roll; its result when done, :continue, or a stop. A place refused
  with a mob in the cell (the villager stepped in) waits a poll and is placed again: :continue."
  [c slot job cargs]
  (let [r (await (ctx/call c slot job (assoc cargs :checks (ready/unforced c {:safe? '(not (threatened-seen))}))))]
    (cond
      (= :continue r) :continue
      (= :done (:kind r)) (:data r)
      (and (= 'jobs.blocks.place job) (seq (:mobs (:data r))))
      (do (ctx/update-mem! c update :place-waits (fnil inc 0))
          (or (await (back-off! c (:pos cargs)))
              (do (await (child/pause! c (poll-s) :mob-in-the-cell))
                  :continue)))
      :else (ctx/end c (:kind r) (:cause r) (data-of c nil)))))

(defn ^:async flush-to-cell!
  "A body in the cell beside the workstation steps to its edge nearest the workstation, so the pickup reach (hitbox grown by a block)
  covers the whole workstation cell and the drop is taken where it falls, even when a slab keeps the body out of the cell."
  [c pos]
  (let [[x y z] (b/feet-cell c)
        dx (- (:x pos) x)
        dz (- (:z pos) z)]
    (when (and (= y (:y pos)) (= 1 (+ (Math/abs dx) (Math/abs dz))))
      (await (ctx/act c :moveTo #js {:pos #js {:x (+ x 0.5 (* 0.2 dx)) :y y :z (+ z 0.5 (* 0.2 dz))} :range 0.1})))))

(defn ^:async back-to-view!
  "The villager was seen but is out of view (the body moved, or the villager shifted in its cell): step to the spot it
  was first seen from, then one and two blocks farther from the workstation, where a slit shows more of the cell, as
  a player shifts to look again. :again when it set out (three tries a sighting), else nil."
  [c pos]
  (let [{:keys [x y z] :as view} (:view (ctx/mem c))
        n (:view-tries (ctx/mem c) 0)]
    (when (and view (< n 3))
      (let [dx (- x (+ (:x pos) 0.5))
            dz (- z (+ (:z pos) 0.5))
            [sx sz] (if (>= (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0] [0 (js/Math.sign dz)])]
        (ctx/update-mem! c update :view-tries (fnil inc 0))
        (await (ctx/act c :moveTo #js {:pos #js {:x (+ x (* n sx)) :y y :z (+ z (* n sz))} :range 0.1}))
        :again))))

(defn ^:async dig!
  "Break the workstation; the break is booked before the dig child runs, so a cut leaves it for the cleanup."
  [c]
  (let [{:keys [pos]} (:args c)
        slot (keyword (str "dig-" (rolls c)))
        _ (await (flush-to-cell! c pos))
        _ (ctx/update-mem! c assoc :broken true :stand (u/self-pos c))
        r (await (child! c slot 'jobs.blocks.dig (cond-> {:pos pos :collect true} (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))]
    (if-not (ending-or-wait? r)
      (do (ctx/update-mem! c #(-> % (update :rolls (fnil inc 0))))
          :again)
      r)))

(defn on-cell?
  "Whether the body's feet or head fill the cell pos."
  [c pos]
  (let [[x y z] (b/feet-cell c)]
    (and (= x (:x pos)) (= z (:z pos)) (contains? #{(:y pos) (dec (:y pos))} y))))

(defn ^:async clear-cell!
  "Walk off the workstation cell so it can be placed (jobs.lib.step-off), to the side the dig was made from, never into a dead end behind it. :again, :continue or the stop."
  [c pos]
  (let [r (await (step-off/step-off-zoned! c pos (if-let [stand (:stand (ctx/mem c))] {:near stand} {})))]
    (cond
      (= :continue r) :continue
      (= :arrived r) :again
      :else (stop! c :place-failed))))

(defn ^:async to-stand!
  "Back to where the dig was made from when the body picked up the drop beyond the cell: placing from there would shut it
  in behind the workstation. :again, :continue, or nil when it is there (or not known)."
  [c]
  (let [stand (:stand (ctx/mem c))]
    (when (and stand (not (u/within? (u/self-pos c) stand 1.5)))
      (let [r (await (child/call-walk* c :to-stand {:pos stand :range 1 :ways #{:walk}}))]
        (cond (= :continue r) :continue
              (:arrived (:data r)) :again
              :else nil)))))

(defn drop-spot
  "Where the broken workstation's drop lies, for the body to reach: the nearest seen item of it within 4 blocks of the cell, else the
  cell beyond the workstation from where the dig was made (a drop slides on, away from the digger). Nil when neither is known."
  [c pos item]
  (let [seen (->> (look/seen-items (:primitives c) {:radius 4 :max 32})
                  (filter #(and (= item (.-name (.-item %))) (u/within? (u/pos-of (.-pos %)) pos 4))))
        stand (:stand (ctx/mem c))]
    (cond
      (seq seen) (u/pos-of (.-pos (first (sort-by #(u/dist (u/pos-of (.-pos %)) pos) seen))))
      stand (let [dx (- (+ (:x pos) 0.5) (:x stand))
                  dz (- (+ (:z pos) 0.5) (:z stand))
                  [sx sz] (if (>= (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0] [0 (js/Math.sign dz)])]
              {:x (+ (:x pos) 0.5 sx) :y (:y pos) :z (+ (:z pos) 0.5 sz)}))))

(defn far-side
  "The point 2 blocks beyond spot on the line from the body through it, along the longer axis: the other side of the wall or cell the drop lies in."
  [c spot]
  (let [{:keys [x z]} (u/self-pos c)
        dx (- (:x spot) x)
        dz (- (:z spot) z)]
    (if (>= (js/Math.abs dx) (js/Math.abs dz))
      (assoc spot :x (+ (:x spot) (* 2 (js/Math.sign dx))))
      (assoc spot :z (+ (:z spot) (* 2 (js/Math.sign dz)))))))

(defn ^:async reach-drop!
  "Take the drop through a gap in a wall (a back passage behind a sealed cell): the first try walks (go-to, walk only) within pickup
  range of the drop's spot, later tries to the far side of it from where the body stood, then presses up against whatever keeps the body
  out, so the pickup box reaches through. :again, :continue, or nil when it was tried four times or no spot is known."
  [c]
  (let [{:keys [pos]} (:args c)
        n (:reach-drops (ctx/mem c) 0)
        spot (when (< n 4) (drop-spot c pos (item-of (:args c))))]
    (when spot
      (ctx/update-mem! c update :reach-drops (fnil inc 0))
      (let [r (await (child/call-walk* c (keyword (str "reach-drop-" n)) (if (zero? n)
                                                       {:pos spot :range 2.5 :ways #{:walk}}
                                                       {:pos (far-side c spot) :range 1.2 :ways #{:walk}})))]
        (cond
          (= :continue r) :continue
          :else (do (when (:arrived (:data r))
                      (await (ctx/act c :moveTo #js {:pos #js {:x (:x spot) :y (:y spot) :z (:z spot)} :range 0.1})))
                    (await (child/pause! c 1 :picking-up-the-drop))
                    :again))))))

(defn ^:async recover!
  "The broken workstation is not carried (its drop slid out of reach, e.g. into the sealed cell beside it): walk to where its drop
  lies and take it through a gap (reach-drop!); failing that stand on its cell when the body can enter it, else stay at the cell's
  edge, and wait a few polls for the drop to come; then end :workstation-lost naming the cell the drop lies in (:drop-cell). Nothing
  above the cell is ever dug: the cell stays shut. :again or :continue."
  [c]
  (let [{:keys [pos]} (:args c)
        tries (:recovers (ctx/mem c) 0)]
    (cond
      (>= tries 5) (let [spot (or (drop-spot c pos (item-of (:args c))) pos)]
                     (ctx/end c :nothing-possible :workstation-lost
                              (data-of c {:drop-cell {:x (js/Math.floor (:x spot)) :y (js/Math.floor (:y spot)) :z (js/Math.floor (:z spot))}})))
      :else (or (await (reach-drop! c))
                (if (on-cell? c pos)
                  (do (ctx/update-mem! c update :recovers (fnil inc 0))
                      (await (child/pause! c 1 :picking-up-the-drop))
                      :continue)
                  (let [r (await (child/call-walk* c :to-drop {:pos pos :range 0 :ways #{:walk}}))]
                    (cond (= :continue r) :continue
                          (:arrived (:data r)) :again
                          :else (do (ctx/update-mem! c update :recovers (fnil inc 0))
                                    (await (child/pause! c 1 :picking-up-the-drop))
                                    :continue))))))))

(defn ^:async place!
  "Place the workstation again, and the book of the break closed."
  [c]
  (let [{:keys [pos]} (:args c)
        item (item-of (:args c))
        slot (keyword (str "place-" (rolls c) "-" (:place-waits (ctx/mem c) 0)))
        ways (:ways (:args c))
        r (await (child! c slot 'jobs.blocks.place (cond-> {:pos pos :item item :through false}
                                                     (:ignore-zones? (:args c)) (assoc :ignore-zones? true)
                                                     (some? ways) (assoc :ways ways))))]
    (if-not (ending-or-wait? r)
      (do (ctx/update-mem! c #(-> % (dissoc :broken)))
          :again)
      r)))

(defn ^:async place-or-return!
  "place!, after a walk back to the dig's stand when the body is elsewhere (to-stand!)."
  [c]
  (or (await (to-stand! c)) (await (place! c))))

(defn ^:async step!
  "One piece of the search, from what the world and the saved count say now. :again, :continue or :done."
  [c]
  (let [{:keys [pos]} (:args c)
        item (item-of (:args c))
        p (:primitives c)
        villager (pick! c)
        e (when villager (villager/find-by-uuid p villager))
        block (u/seen-name p pos)
        _ (when e (ctx/update-mem! c #(-> % (update :view (fn [v] (or v (u/self-pos c)))) (assoc :view-tries 0))))
        profession (when e (.-profession e))
        out-of-tries? (>= (rolls c) (max-rolls))]
    (cond
      (nil? block) (await (approach! c pos))
      (bs/air block)
      (cond
        (not (:broken (ctx/mem c))) (do (ctx/update-mem! c assoc :broken true) :again)
        ;; the workstation is gone: wait for the villager to lose the job, then place it back
        (and e (not= "unemployed" profession)) (await (wait-job! c :villager-keeps-job))
        (zero? (u/carried-count p item)) (await (recover! c))
        (on-cell? c pos) (await (clear-cell! c pos))
        :else (await (place-or-return! c)))
      (not= item block) (stop! c :occupied)
      ;; villagers count only while in view; one seen before may have stepped away while it lost its job
      (nil? e) (cond
                 (await (back-to-view! c pos)) :again
                 (await (villager/find-in-view! c not-nitwit? look/hazards-only)) :again
                 (and (:seen (ctx/mem c)) (< (- (ctx/now c) (:away-since (ctx/mem c) (ctx/now c))) (* 1000 (away-s))))
                 (do (ctx/update-mem! c update :away-since #(or % (ctx/now c)))
                     (await (wait-job! c :villager-out-of-view)))
                 :else (stop! c (villager/gone-cause c)))
      (and e (not (and (:seen (ctx/mem c)) (nil? (:away-since (ctx/mem c)))))) (do (ctx/update-mem! c #(-> % (assoc :seen true) (dissoc :away-since))) :again)
      (.-baby e) (stop! c :baby)
      (= "nitwit" profession) (stop! c :nitwit)
      (= "unemployed" profession) (await (wait-job! c :villager-takes-no-job))
      (not (u/within? (u/self-pos c) (u/pos-of (.-pos e)) (reach))) (await (walk! c (u/pos-of (.-pos e))))
      :else
      (let [r (await (read-offers! c))]
        (cond
          (:found r) (found! c (:found r))
          (= :locked r) (stop! c :locked)
          (= :miss r) (if out-of-tries?
                        (stop! c :no-match)
                        (await (dig! c)))
          :else r)))))

(defn ^:async cleanup
  "However the call ends: a workstation this call broke and did not place back is placed (after stepping off its cell first).
  Done, or the place child's failure."
  [c _]
  (let [{:keys [pos]} (:args c)
        block (u/seen-name (:primitives c) pos)
        r (cond
            (not (and (:broken (ctx/mem c)) block (bs/air block) (pos? (u/carried-count (:primitives c) (item-of (:args c))))))
            (ctx/end c :done nil)
            (on-cell? c pos) (await (clear-cell! c pos))
            :else (await (place-or-return! c)))]
    (when (and (contract/ending? r) (not= :done (:kind r)) (:broken (ctx/mem c)) (some-> (u/seen-name (:primitives c) pos) bs/air))
      (ctx/emit! c :roll.cell-open :warn {:pos pos :text (str "CELL OPEN: the " (item-of (:args c)) " at " (:x pos) "," (:y pos) "," (:z pos) " is not placed back")}))
    (if (= :again r) (await (cleanup c nil)) r)))

(defn ^:async round
  "The whole search: steps (a pace between) until found or stopped; it yields while the villager or a child waits."
  [c]
  (await (pace/steps! c #(step! c))))
