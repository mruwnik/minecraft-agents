(ns jobs.village.roll
  (:require [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.access :as access]
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
  Result: {:found true|false :rolls n :item workstation :offer {:index :cost :gives ..}}. Success is info roll.done.
  Giving up ends stopped with :reason and warns roll.gave-up. :reason is one of:
  - \"gone\" (not seen within 48), \"baby\", \"nitwit\" (they take no job), \"locked\" (an offer was already used: breaking
    the workstation changes nothing), \"occupied\" (another block stands at :pos).
  - \"no-claim\" (the villager took no profession within :claim-s after the workstation was placed: night, no path, a
    taken workstation), \"no-match\" (:tries rolls without a match), \"unreachable\", \"window\" (offers could not be read
    three times), \"dig-failed\", \"place-failed\" (a child stopped, with its reason).
  Every step re-reads the world and the saved count: a restart that finds the workstation missing places it first, so a cut
  between the break and the placing leaves nothing behind. The workstation stands at :pos when the job ends, except after
  \"place-failed\" or \"dig-failed\" (a child stopped).")

(def args
  {:villager {:doc "the villager's entity uuid" :type :string :default nil}
   :pos {:doc "the workstation block, [x y z] or {:x :y :z}" :type :pos :default nil}
   :item {:doc "the workstation block item name, e.g. lectern; nil: the one of :profession" :type :item :default nil}
   :profession {:doc "villager profession whose workstation :item defaults to" :type :string :default nil}
   :want {:doc "item name an offer is to give" :type :item :default nil}
   :trade {:doc "item name an offer is to give (alias of :want)" :type :item :default nil}
   :radius {:doc "how far to look for a villager" :type :int :min 1 :max 96 :default 48}
   :max-price {:doc "highest price (first cost stack); nil for no limit" :default nil}
   :tries {:doc "rolls (breaks) before giving up" :type :int :min 1 :max 200 :default 20}
   :claim-s {:doc "seconds to wait for the villager to take the placed workstation" :type :int :min 1 :max 600 :default 60}
   :fetch {:doc "get the workstation item or a tool when missing (jobs.lib.fetch)" :default true}})

(def workstations
  {"librarian" "lectern" "farmer" "composter" "cleric" "brewing_stand" "armorer" "blast_furnace" "butcher" "smoker"
   "cartographer" "cartography_table" "fisherman" "barrel" "fletcher" "fletching_table" "leatherworker" "cauldron"
   "mason" "stonecutter" "shepherd" "loom" "toolsmith" "smithing_table" "weaponsmith" "grindstone"})

(def reach 2)
(def leg-s 15)
(def unemploy-s "Seconds to wait for the villager to lose its job after the break, then place anyway." 20)

(defn item-of
  "The workstation item: :item, else the one of :profession."
  [args]
  (or (:item args) (workstations (:profession args))))

(defn want-of [args] (or (:want args) (:trade args)))

(defn check
  "A cell, a workstation item (or a profession) and a wanted item (or trade) are given; the villager may be left out."
  [c]
  (let [{:keys [villager]} (:args c)
        {:keys [error]} (b/parse (:args c))]
    (cond
      (not (and (or (nil? villager) (string? villager)) (string? (item-of (:args c))) (string? (want-of (:args c)))))
      (ctx/wait c {:reason :bad-args :why "needs a workstation item (or profession) and a wanted item"})
      error (ctx/wait c {:reason :bad-args :why error})
      :else true)))

(defn rolls [c] (:rolls (ctx/mem c) 0))

(defn finish!
  [c extra]
  (ctx/result! c (merge {:found false :rolls (rolls c) :item (item-of (:args c))} extra))
  :done)

(defn stop!
  "Warn and end stopped with a reason."
  [c reason text]
  (ctx/emit! c :roll.gave-up :warn {:reason reason :text text :rolls (rolls c)})
  (finish! c {:status :stopped :reason reason}))

(defn found!
  [c offer]
  (ctx/emit! c :roll.done :info {:rolls (rolls c) :text (str "found " (want-of (:args c)) " after " (rolls c) " rolls")})
  (finish! c {:found true :offer (select-keys offer [:index :cost :gives])}))

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
  (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to
                                 {:pos pos :range reach :leg-s leg-s :escalate false :warn false}))]
    (cond
      (= :continue r) :continue
      (let [res (ctx/child-result c :walk)] (and (= :done r) (or (:arrived res) (:leg res)))) :again
      :else (stop! c "unreachable" "cannot reach the villager"))))

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
      "gone" (stop! c "gone" "the villager is gone")
      "out-of-reach" :again
      (if (= :done (u/fail! c :roll.gave-up (str "roll gave up: offers " status)))
        (finish! c {:status :stopped :reason "window"})
        :continue))))

(defn ^:async child!
  "Run a dig or place child in a slot of its own per roll; its result when done, :continue, or a stop."
  [c slot job cargs failed]
  (let [r (await (ctx/call-child c slot job cargs))]
    (cond
      (= :continue r) :continue
      (= :done r) (let [res (ctx/child-result c slot)]
                    (if (= :stopped (:status res))
                      (stop! c failed (str (name slot) " stopped: " (name (:reason res :unknown))))
                      res))
      :else (stop! c failed (str (name slot) " declined")))))

(defn ^:async dig!
  "Break the workstation; the break is booked with its time."
  [c]
  (let [{:keys [pos fetch]} (:args c)
        slot (keyword (str "dig-" (rolls c)))
        r (await (child! c slot 'jobs.blocks.dig {:pos pos :collect true :fetch fetch} "dig-failed"))]
    (if (map? r)
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
  (let [r (await (step-off/step-off! c pos {:ok? (step-off/zone-ok (access/rules-input c))}))]
    (cond
      (= :continue r) :continue
      (= :arrived r) :again
      :else (stop! c "place-failed" "the body cannot leave the workstation cell"))))

(defn ^:async place!
  "Place the workstation again; the time is booked for the claim wait."
  [c]
  (let [{:keys [pos fetch]} (:args c)
        item (item-of (:args c))
        slot (keyword (str "place-" (rolls c)))
        r (await (child! c slot 'jobs.blocks.place {:pos pos :item item :fetch fetch} "place-failed"))]
    (if (map? r)
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
        block (u/block-name p pos)
        profession (when e (.-profession e))
        out-of-tries? (>= (rolls c) tries)]
    (cond
      (nil? block) :continue
      (b/air block)
      (cond
        (not (:broken (ctx/mem c))) (do (stamp! c :broken) :again)
        ;; the workstation is gone: wait for the villager to lose the job, then place it back
        (and e (not= "unemployed" profession) (< (waited-s c :broken) unemploy-s)) :continue
        (and e (not= "unemployed" profession) (not (:stale (ctx/mem c)))) (do (ctx/update-mem! c assoc :stale true) :again)
        (on-cell? c pos) (await (clear-cell! c pos))
        :else (await (place! c)))
      (not= item block) (stop! c "occupied" (str block " stands where the workstation goes"))
      (nil? e) (stop! c "gone" "the villager is not here")
      (.-baby e) (stop! c "baby" "a baby takes no job")
      (= "nitwit" profession) (stop! c "nitwit" "a nitwit takes no job")
      (and (:stale (ctx/mem c)) (not= "unemployed" profession))
      (stop! c "still-employed" "the villager kept its job after the break: its offers cannot change")
      (= "unemployed" profession)
      (do (ctx/update-mem! c dissoc :stale)
          (when-not (:placed (ctx/mem c)) (stamp! c :placed))
          (if (> (waited-s c :placed) claim-s)
            (stop! c "no-claim" "the villager took no job")
            :continue))
      (not (u/within? (u/self-pos c) (u/pos-of (.-pos e)) reach)) (await (walk! c (u/pos-of (.-pos e))))
      :else
      (let [r (await (read-offers! c))]
        (cond
          (map? r) (found! c (:found r))
          (= :locked r) (stop! c "locked" "the villager already traded: breaking the workstation changes nothing")
          (= :miss r) (if out-of-tries?
                        (stop! c "no-match" (str "no match in " (rolls c) " rolls"))
                        (do (ctx/update-mem! c dissoc :placed)
                            (await (dig! c))))
          :else r)))))

(defn ^:async round
  "The whole search: steps (a pace between) until found or stopped; it yields while the villager or a child waits."
  [c]
  (await (pace/steps! c #(step! c))))
