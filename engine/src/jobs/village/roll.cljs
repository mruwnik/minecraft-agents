(ns jobs.village.roll
  (:require [engine.contract :as contract]
            [engine.settings :as settings]
            [engine.args :as a] [jobs.lib.need :as need]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.blocks :as b]
            [jobs.lib.look :as look]
            [jobs.lib.started :as started]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.step-off :as step-off]
            [jobs.village.trade :as trade]))

(def doc
  "Re-roll the offers of the untraded villager with uuid :villager (nil: the nearest adult, not a nitwit, within :radius)
  until one gives :want (:trade is the same). The villager works at the workstation :item (nil: that of :profession)
  standing at :pos (a block the body may break and place there). One call is the whole search: read the
  offers (walking within 2 of the villager with go-to legs of 15 s); when none gives :want, break the workstation
  (jobs.blocks.dig child, the drop collected), wait until the villager has lost its profession, place it again
  (jobs.blocks.place child) and wait until the villager claims it, then read the offers again. It yields (:continue) while
  it waits for the villager or a child waits on the world.
  An offer matches when it gives :want, is not sold out and its price (the adjusted count of the first cost stack) is at
  most :max-price (nil: no limit). Offers do not name the enchantment of a book: :want is an item name only.
  Ends with the data {:found true|false :rolls n :item workstation :offer {:index :cost :gives ..}}: :done (info
  roll.done) when an offer matches, else (warn roll.gave-up) with the cause:
  - condition-failed: :gone (not seen within :radius), :baby (takes no job yet), :locked (an offer was already used:
    breaking the workstation changes nothing), :occupied (another block stands at :pos). :baby, :occupied and a named
    :villager's :gone are checked at the start and then wait (a skipped job); a nitwit takes no job ever: impossible,
    :nitwit, also checked at the start and found mid-run (impossible).
  - nothing-possible: :still-employed (it kept its job), :no-claim (the villager took no profession within :claim-s after the workstation was placed: night, no path, a
    taken workstation), :no-match (:tries rolls without a match), :unreachable, :window (offers could not be read
    three times), :dig-failed, :place-failed (a child stopped, with its reason).
  A workstation item, wanted item or cell that is missing or bad is impossible (:bad-args). A cell the body has not seen
  asks for a look at the start; the round then walks to it (go-to child) and ends condition-failed :cell-unseen when it
  still cannot be seen there.
  Every step re-reads the world and the saved count: a restart that finds the workstation missing places it first, so a cut
  between the break and the placing leaves nothing behind. The workstation stands at :pos when the job ends, except after
  :place-failed or :dig-failed (a child stopped).")

(a/defargs args
  {:villager {:doc "the villager's entity uuid" :spec string? :default nil}
   :pos {:doc "the workstation block, [x y z] or {:x :y :z}" :spec ::a/pos :default nil}
   :item {:doc "the workstation block item name, e.g. lectern; nil: the one of :profession" :spec a/item? :default nil}
   :profession {:doc "villager profession whose workstation :item defaults to" :spec string? :default nil}
   :want {:doc "item name an offer is to give" :spec a/item? :default nil}
   :trade {:doc "item name an offer is to give (alias of :want)" :spec a/item? :default nil}
   :radius {:doc "how far to look for a villager" :spec (a/int-in 1 96) :default 48}
   :max-price {:doc "highest price (first cost stack); nil for no limit" :spec (a/int-in 0 nil) :default nil}
   :tries {:doc "rolls (breaks) before giving up" :spec (a/int-in 1 200) :default 20}
   :claim-s {:doc "seconds to wait for the villager to take the placed workstation" :spec (a/int-in 1 600) :default 60}
   :fetch {:doc "get the workstation item or a tool when missing (jobs.lib.need)" :spec need/option? :default true}})

(def workstations
  {"librarian" "lectern" "farmer" "composter" "cleric" "brewing_stand" "armorer" "blast_furnace" "butcher" "smoker"
   "cartographer" "cartography_table" "fisherman" "barrel" "fletcher" "fletching_table" "leatherworker" "cauldron"
   "mason" "stonecutter" "shepherd" "loom" "toolsmith" "smithing_table" "weaponsmith" "grindstone"})

(a/defargs settings
  {::reach {:default 2 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}
   ::leg-s {:default 15 :doc "Seconds one walk leg may take." :spec (a/int-in 0 nil)}
   ::time-s {:default 1800 :doc "Seconds one search may take." :spec (a/int-in 1 nil)}
   ::unemploy-s {:default 20 :doc "Seconds to wait for the villager to lose its job after the break, then place anyway." :spec (a/int-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn leg-s [] (settings/get settings ::leg-s))
(defn unemploy-s [] (settings/get settings ::unemploy-s))

(defn item-of
  "The workstation item: :item, else the one of :profession."
  [args]
  (or (:item args) (workstations (:profession args))))

(defn want-of [args] (or (:want args) (:trade args)))

(def target [:villager :pos])

(def defaults {:spend {:time ::time-s}})

(defn bad-args
  "Impossible :bad-args when a cell, a workstation item (or profession) and a wanted item (or trade) are not given."
  [c]
  (let [{:keys [villager]} (:args c)
        {:keys [error]} (b/parse (:args c))]
    (cond
      (not (and (or (nil? villager) (string? villager)) (string? (item-of (:args c))) (string? (want-of (:args c)))))
      (contract/impossible {:reason :bad-args :why "needs a workstation item (or profession) and a wanted item"})
      error (contract/impossible {:reason :bad-args :why error}))))

(defn start-facts
  "At the call's start, with a named villager in view: impossible for a nitwit (never a job), the cause :baby for a baby
  and :occupied when another block stands at :pos (not now). The rest is found out by the rounds."
  [c]
  (let [{:keys [villager]} (:args c)
        p (:primitives c)
        e (when (and (string? villager) (not (ctx/frozen-call c))) (trade/find-villager p villager))
        block (when e (some->> (:pos (b/parse (:args c))) (u/seen-name p)))
        item (item-of (:args c))]
    (cond
      (nil? e) nil
      (= "nitwit" (.-profession e)) (contract/impossible {:reason :nitwit})
      (.-baby e) {:reason :baby}
      (and block (not (b/air block)) (not= item block)) {:reason :occupied :block block})))

(def cell-unseen
  "At the start, the cause :cell-unseen (after a look still unseen is the round's to find out) while the workstation
  cell is not in view or memory."
  (started/at-start
   (fn [c]
     (let [pos (:pos (b/parse (:args c)))]
       (when (and pos (nil? (u/seen-name (:primitives c) pos)) (not (look/surveyed? c :near)))
         (contract/look-first {:reason :cell-unseen}))))))

(def conditions [[:fn bad-args] [:fn start-facts] [:fn cell-unseen]])

(defn rolls [c] (:rolls (ctx/mem c) 0))

(defn done?
  "The rolls against :tries (searching an offer has no state to be done before it runs)."
  [c]
  {:done? false :units :rolls :done (rolls c) :left (max 0 (- (:tries (:args c)) (rolls c)))})

(defn quote-of [c]
  {:uses {:time [0 (* 30 (:tries (:args c) 1)) ##Inf]}})

(defn data-of [c extra]
  (merge {:found false :rolls (rolls c) :item (item-of (:args c))} extra))

(def failed-conditions "Causes that end condition-failed: the villager or the cell is not right now. The rest are nothing-possible."
  #{:gone :baby :locked :occupied :cell-unseen})

(defn stop!
  "Warn and end with reason (a keyword) as the cause: condition-failed for those in failed-conditions, else nothing-possible."
  [c reason text]
  (ctx/emit! c :roll.gave-up :warn {:reason reason :text text :rolls (rolls c)})
  (ctx/end c (cond (= :nitwit reason) :impossible (failed-conditions reason) :condition-failed :else :nothing-possible)
           reason (data-of c nil)))

(defn found!
  [c offer]
  (ctx/emit! c :roll.done :info {:rolls (rolls c) :text (str "found " (want-of (:args c)) " after " (rolls c) " rolls")})
  (ctx/end c :done nil (data-of c {:found true :offer (select-keys offer [:index :cost :gives])})))

(defn stamp! [c k] (ctx/update-mem! c assoc k (ctx/now c)))
(defn waited-s [c k] (/ (- (ctx/now c) (get (ctx/mem c) k (ctx/now c))) 1000))

(defn chosen
  "The uuid of the villager rolled: :villager, else the one picked first (kept in memory), else nil."
  [c]
  (or (:villager (:args c)) (:chosen (ctx/mem c))))

(defn pick!
  "Without :villager, choose the nearest adult villager within :radius that is no nitwit and keep it. The uuid or nil."
  [c]
  (or (chosen c)
      (when-let [e (->> (array-seq (.entities (:primitives c) #js {:radius (:radius (:args c)) :names #js ["villager"]}))
                        (remove #(or (.-baby %) (= "nitwit" (.-profession %))))
                        first)]
        (ctx/update-mem! c assoc :chosen (.-uuid e))
        (.-uuid e))))

(defn ^:async walk!
  "One go-to leg toward the villager at pos. :again, :continue or the stop."
  [c pos]
  (let [r (await (child/call-walk* c :walk
                                  {:pos pos :range (reach) :leg-s (leg-s) :escalate false :warn false}))]
    (cond
      (= :continue r) :continue
      (let [res (:data r)] (or (:arrived res) (:leg res))) :again
      :else (stop! c :unreachable "cannot reach the villager"))))

(defn ^:async approach!
  "The workstation cell is not in view or memory: walk to it (go-to child, slot :cell) once; if it is still unseen
  there, end :cell-unseen. :again, :continue or the stop."
  [c pos]
  (if (:approached (ctx/mem c))
    (stop! c :cell-unseen "the workstation cell cannot be seen")
    (let [r (await (child/call-walk* c :cell {:pos pos :range (reach) :escalate false :warn false}))]
      (cond
        (= :continue r) :continue
        (:arrived (:data r)) (do (ctx/update-mem! c assoc :approached true) :again)
        :else (stop! c :unreachable "cannot reach the workstation cell")))))

(defn ^:async read-offers!
  "In reach of the villager: read its offers and decide. :found, :locked, :miss (no match), or :again/:continue/stop."
  [c]
  (let [{:keys [max-price]} (:args c)
        villager (chosen c)
        want (want-of (:args c))
        r (await (ctx/act c :trade (clj->js {:villager villager :op "offers"})))
        status (.-status r)]
    (case status
      "ok" (let [offers (js->clj (.-offers r) :keywordize-keys true)
                 {:keys [offer]} (trade/choose offers want max-price)]
             (u/progress! c)
             (cond
               (some #(pos? (:uses % 0)) offers) :locked
               offer {:found offer}
               :else :miss))
      "gone" (stop! c :gone "the villager is gone")
      "out-of-reach" :again
      (if (= :done (u/fail! c :roll.gave-up (str "roll gave up: offers " status)))
        (ctx/end c :nothing-possible :window (data-of c nil))
        :continue))))

(defn ending-or-wait?
  "Whether a child! result is :continue or a stop, not the child's result."
  [r]
  (or (keyword? r) (contract/ending? r)))

(defn ^:async child!
  "Run a dig or place child in a slot of its own per roll; its result when done, :continue, or a stop."
  [c slot job cargs failed]
  (let [r (await (ctx/call c slot job cargs))]
    (cond
      (= :continue r) :continue
      (= :done (:kind r)) (:data r)
      :else (stop! c failed (str (name slot) " ended " (name (:kind r)) ": " (name (contract/cause-key r)))))))

(defn ^:async dig!
  "Break the workstation; the break is booked with its time."
  [c]
  (let [{:keys [pos fetch]} (:args c)
        slot (keyword (str "dig-" (rolls c)))
        r (await (child! c slot 'jobs.blocks.dig {:pos pos :collect true :fetch fetch} :dig-failed))]
    (if-not (ending-or-wait? r)
      (do (ctx/update-mem! c #(-> % (update :rolls (fnil inc 0)) (assoc :broken (ctx/now c))))
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
      :else (stop! c :place-failed "the body cannot leave the workstation cell"))))

(defn ^:async place!
  "Place the workstation again; the time is booked for the claim wait."
  [c]
  (let [{:keys [pos fetch]} (:args c)
        item (item-of (:args c))
        slot (keyword (str "place-" (rolls c)))
        r (await (child! c slot 'jobs.blocks.place {:pos pos :item item :fetch fetch} :place-failed))]
    (if-not (ending-or-wait? r)
      (do (ctx/update-mem! c #(-> % (dissoc :broken) (assoc :placed (ctx/now c))))
          :again)
      r)))

(defn ^:async step!
  "One piece of the search, from what the world and the saved count say now. :again, :continue or :done."
  [c]
  (let [{:keys [tries claim-s pos]} (:args c)
        item (item-of (:args c))
        p (:primitives c)
        villager (pick! c)
        e (when villager (trade/find-villager p villager))
        block (u/seen-name p pos)
        profession (when e (.-profession e))
        out-of-tries? (>= (rolls c) tries)]
    (cond
      (nil? block) (await (approach! c pos))
      (b/air block)
      (cond
        (not (:broken (ctx/mem c))) (do (stamp! c :broken) :again)
        ;; the workstation is gone: wait for the villager to lose the job, then place it back
        (and e (not= "unemployed" profession) (< (waited-s c :broken) (unemploy-s))) :continue
        (and e (not= "unemployed" profession) (not (:stale (ctx/mem c)))) (do (ctx/update-mem! c assoc :stale true) :again)
        (on-cell? c pos) (await (clear-cell! c pos))
        :else (await (place! c)))
      (not= item block) (stop! c :occupied (str block " stands where the workstation goes"))
      (nil? e) (stop! c :gone "the villager is not here")
      (.-baby e) (stop! c :baby "a baby takes no job")
      (= "nitwit" profession) (stop! c :nitwit "a nitwit takes no job")
      (and (:stale (ctx/mem c)) (not= "unemployed" profession))
      (stop! c :still-employed "the villager kept its job after the break: its offers cannot change")
      (= "unemployed" profession)
      (do (ctx/update-mem! c dissoc :stale)
          (when-not (:placed (ctx/mem c)) (stamp! c :placed))
          (if (> (waited-s c :placed) claim-s)
            (stop! c :no-claim "the villager took no job")
            :continue))
      (not (u/within? (u/self-pos c) (u/pos-of (.-pos e)) (reach))) (await (walk! c (u/pos-of (.-pos e))))
      :else
      (let [r (await (read-offers! c))]
        (cond
          (:found r) (found! c (:found r))
          (= :locked r) (stop! c :locked "the villager already traded: breaking the workstation changes nothing")
          (= :miss r) (if out-of-tries?
                        (stop! c :no-match (str "no match in " (rolls c) " rolls"))
                        (do (ctx/update-mem! c dissoc :placed)
                            (await (dig! c))))
          :else r)))))

(defn ^:async round
  "The whole search: steps (a pace between) until found or stopped; it yields while the villager or a child waits."
  [c]
  (await (pace/steps! c #(step! c))))
