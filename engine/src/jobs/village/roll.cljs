(ns jobs.village.roll
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.cond :as cond]
            [jobs.lib.blocks :as b]
            [jobs.lib.look :as look]
            [jobs.lib.villager :as villager]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.step-off :as step-off]
            [jobs.village.trade :as trade]))

(def doc
  "Re-roll the offers of an untraded villager (:villager uuid; nil: the nearest adult, not a nitwit, in sight)
  until one gives :want (with :enchant, an enchanted book of that enchantment, e.g. \"mending\"), by breaking and placing
  again the workstation of :profession at :pos.
  Needs: a profession, a wanted item (or enchantment) and a cell the body may break and place at (else impossible, :bad-args); a
  cell not yet seen is looked at, then walked to. An offer matches when it gives :want, is not sold out and costs at
  most :max-price (nil: no limit). Waits (:continue) for the villager to lose
  and take its job until the call's :wall-time. Re-reads the world every step, so a restart finds a missing
  workstation and places it first; the cleanup places one the call broke however it ends.
  Returns {:found true|false :rolls n :item workstation :offer {:index :cost :gives ..}}: done when an offer
  matches, else
  - condition-failed: :gone, :baby, :locked (an offer was already used), :occupied (another block at :pos),
    :cell-unseen.
  - impossible: :nitwit.
  - nothing-possible: :no-match (::max-rolls, 20, rolls without a match), :unreachable, :window (offers could not be
    read three times), :place-failed (no room to step off the cell), or the ending of the dig or place child.")

(a/defargs args
  {:villager {:doc "the villager's entity uuid" :spec string? :default nil}
   :pos {:doc "the workstation block, [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :profession {:doc "villager profession whose workstation block is broken and placed again, e.g. librarian" :spec string? :default nil}
   :want {:doc "item name an offer is to give; nil with :enchant: enchanted_book" :spec a/item? :default nil}
   :enchant {:doc "enchantment name an offer's item is to carry, e.g. mending" :spec string? :default nil}
   :max-price {:doc "highest price (first cost stack); nil for no limit" :spec (a/int-in 0 nil) :default nil}})

(def workstations
  {"librarian" "lectern" "farmer" "composter" "cleric" "brewing_stand" "armorer" "blast_furnace" "butcher" "smoker"
   "cartographer" "cartography_table" "fisherman" "barrel" "fletcher" "fletching_table" "leatherworker" "cauldron"
   "mason" "stonecutter" "shepherd" "loom" "toolsmith" "smithing_table" "weaponsmith" "grindstone"})

(a/defargs settings
  {::max-rolls {:default 20 :doc "Rolls (breaks of the workstation) without a match before the call gives up (:no-match)." :spec (a/int-in 1 200)}
   ::poll-s {:default 2 :doc "Seconds between looks while waiting for the villager to lose or take its job." :spec (a/int-in 1 60)}
   ::wall-time-s {:default 3600 :doc "Seconds one call may last from its start, the waits for the villager's job included (:limit-reached)." :spec (a/int-in 1 nil)}
   ::reach {:default 2 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}
   ::leg-s {:default 15 :doc "Seconds one walk leg may take." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 1800 :doc "Seconds one search may take." :spec (a/int-in 1 nil)}})
(defn max-rolls [] (settings/get settings ::max-rolls))
(defn poll-s [] (settings/get settings ::poll-s))
(defn reach [] (settings/get settings ::reach))
(defn leg-s [] (settings/get settings ::leg-s))

(defn item-of
  "The workstation item of :profession."
  [args]
  (workstations (:profession args)))

(defn want-of [args] (or (:want args) (when (:enchant args) "enchanted_book")))

(def target [:villager :pos])

(def safe? (ready/safe-at-by-night (fn [c] (a/cell (:pos (:args c))))))
(def fit? ready/fit?)


(def defaults {:spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(defn bad-args
  "Never :bad-args when a cell, a profession with a workstation and a wanted item (or enchantment) are not given."
  [c]
  (let [{:keys [villager]} (:args c)
        {:keys [error]} (b/parse (:args c))]
    (cond
      (not (and (or (nil? villager) (string? villager)) (string? (item-of (:args c))) (string? (want-of (:args c)))))
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
        e (when (string? villager) (villager/find-by-uuid p villager))
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
      (apply contract/ok :found (str "found " (want-of (:args c)) " after " (rolls c) " rolls") data)
      (apply contract/later :rolling (str (rolls c) " rolls without " (want-of (:args c))) data))))

(defn quote-of [c]
  {:uses {:job-time [0 (* 30 (max-rolls)) ##Inf]}})

(def failed-conditions "Causes that end condition-failed: the villager or the cell is not right now. The rest are nothing-possible."
  #{:gone :baby :locked :occupied :cell-unseen})

(defn stop!
  "End with reason (a keyword) as the cause: condition-failed for those in failed-conditions, else nothing-possible."
  [c reason]
  (ctx/end c (cond (= :nitwit reason) :impossible (failed-conditions reason) :condition-failed :else :nothing-possible)
           reason (data-of c nil)))

(defn found!
  "Book the matching offer; done? ends the job."
  [c offer]
  (ctx/update-mem! c assoc :offer (select-keys offer [:index :cost :gives]))
  :again)

(defn ^:async wait-job!
  "Hold still for a poll, saying why the villager's job is awaited; :continue."
  [c why]
  (await (child/pause! c (poll-s) why))
  :continue)

(defn pick!
  "The villager rolled: :villager, else the nearest adult that is no nitwit."
  [c]
  (villager/pick! c #(not= "nitwit" (.-profession %))))

(defn ^:async walk!
  "One go-to leg toward the villager at pos. :again, :continue or the stop."
  [c pos]
  (let [r (await (child/call-chase* c :walk
                                   {:pos pos :range (reach) :leg-s (leg-s) :walk {:ways #{:walk :dig} :may-dig #{:own}}}))]
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
    (let [r (await (child/call-walk* c :cell {:pos pos :range (reach) :ways #{:walk :dig} :may-dig #{:own}}))]
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
             (cond
               (some #(pos? (:uses % 0)) offers) :locked
               offer {:found offer}
               :else :miss))
      "gone" (stop! c :gone)
      "out-of-reach" :again
      (if (u/count-fail! c)
        (ctx/end c :nothing-possible :window (data-of c nil))
        :continue))))

(defn ending-or-wait?
  "Whether a child! result is :continue or a stop, not the child's result."
  [r]
  (or (keyword? r) (contract/ending? r)))

(defn ^:async child!
  "Run a dig or place child in a slot of its own per roll; its result when done, :continue, or a stop."
  [c slot job cargs]
  (let [r (await (ctx/call c slot job cargs))]
    (cond
      (= :continue r) :continue
      (= :done (:kind r)) (:data r)
      :else (ctx/end c (:kind r) (:cause r) (data-of c nil)))))

(defn ^:async dig!
  "Break the workstation; the break is booked before the dig child runs, so a cut leaves it for the cleanup."
  [c]
  (let [{:keys [pos]} (:args c)
        slot (keyword (str "dig-" (rolls c)))
        _ (ctx/update-mem! c assoc :broken true)
        r (await (child! c slot 'jobs.blocks.dig {:pos pos :collect true}))]
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
  "Walk off the workstation cell so it can be placed (jobs.lib.step-off). :again, :continue or the stop."
  [c pos]
  (let [r (await (step-off/step-off-zoned! c pos {}))]
    (cond
      (= :continue r) :continue
      (= :arrived r) :again
      :else (stop! c :place-failed))))

(defn ^:async place!
  "Place the workstation again, and the book of the break closed."
  [c]
  (let [{:keys [pos]} (:args c)
        item (item-of (:args c))
        slot (keyword (str "place-" (rolls c)))
        r (await (child! c slot 'jobs.blocks.place {:pos pos :item item}))]
    (if-not (ending-or-wait? r)
      (do (ctx/update-mem! c #(-> % (dissoc :broken)))
          :again)
      r)))

(defn ^:async step!
  "One piece of the search, from what the world and the saved count say now. :again, :continue or :done."
  [c]
  (let [{:keys [pos]} (:args c)
        item (item-of (:args c))
        p (:primitives c)
        villager (pick! c)
        e (when villager (villager/find-by-uuid p villager))
        block (u/seen-name p pos)
        profession (when e (.-profession e))
        out-of-tries? (>= (rolls c) (max-rolls))]
    (cond
      (nil? block) (await (approach! c pos))
      (bs/air block)
      (cond
        (not (:broken (ctx/mem c))) (do (ctx/update-mem! c assoc :broken true) :again)
        ;; the workstation is gone: wait for the villager to lose the job, then place it back
        (and e (not= "unemployed" profession)) (await (wait-job! c :villager-keeps-job))
        (on-cell? c pos) (await (clear-cell! c pos))
        :else (await (place! c)))
      (not= item block) (stop! c :occupied)
      (and (nil? e) (not (look/surveyed? c))) (await (look/survey! c)) ; villagers count only while in view
      (nil? e) (stop! c :gone)
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
  "However the call ends: a workstation this call broke and did not place back is placed (stepping off its cell first).
  Done, or the place child's failure."
  [c _]
  (let [{:keys [pos]} (:args c)
        block (u/seen-name (:primitives c) pos)]
    (if-not (and (:broken (ctx/mem c)) block (bs/air block))
      (ctx/end c :done nil)
      (let [r (if (on-cell? c pos) (await (clear-cell! c pos)) (await (place! c)))]
        (if (= :again r) (await (cleanup c nil)) r)))))

(defn ^:async round
  "The whole search: steps (a pace between) until found or stopped; it yields while the villager or a child waits."
  [c]
  (await (pace/steps! c #(step! c))))
