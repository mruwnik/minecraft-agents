(ns jobs.items.enchant
  (:require [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.need :as need]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.look :as look]
            [clojure.string :as str]))

(def doc
  "Enchant one :item at an enchanting table. Walks to the table (:table, else the nearest within the table-radius setting), reads
  the three offers with the item lying in the table, chooses one and enchants. Needs the item, lapis and levels; a missing item or
  lapis is fetched (jobs.lib.need) unless :fetch is false. Quote: A, a walk and a visit.
  An offer costs its slot number in lapis, and in levels at least its level cost and its slot number. The
  choice: :slot 1-3 takes that offer alone. Otherwise :choice \"best\" takes the dearest offer the body can pay and
  \"cheapest\" the lowest level cost it can pay (the lower slot on a tie). Offers dearer than :max-level-cost
  are ignored.
  Ends :done with {:enchanted true :item :slot :level-cost :levels-spent :lapis-spent :enchants [{:name
  :level}] :xp-level :hint {:enchant :level}|nil}, measured from what is carried and the body's level after the
  window closed, and a :text.
  A give-up ends with the :text and {:enchanted false :item :levels-spent n :lapis-spent n}.
  Impossible: :bad-args (no item name). Conditions (a listed root job waits): :no-item and :no-lapis (not carried, :fetch false), :too-few-levels
  (no level at all). A failed fetch ends as {:reason :need} with its ending as the :because. Condition-failed in the
  round: :no-table, :not-a-table, :no-item, :already-enchanted (every copy carried is enchanted), :not-enchantable,
  :no-lapis, :too-few-levels (the offers cost more), :no-offer (the :slot has none), :no-offer-within-cost,
  :inventory-full. Nothing-possible: :enchant-failed (an answer not understood after the call), :window (did not open,
  three times), :window-stalled, :not-confirmed (the item came back unenchanted), :offer-changed (three times),
  :unreachable, :timeout.
  One call is the whole enchant (it yields :continue only while go-to waits on the world).
  A failed enchant is never repeated, as it may have taken the price. The attempt is written to memory before the
  call. After a restart that finds the item enchanted, the job reports it (:resumed true, enchants unknown).
  done? is ok once the item is enchanted.")

(a/defargs args
  {:item {:doc "name of the item to enchant, from the inventory" :spec a/item? :default nil}
   :table {:doc "the enchanting table position {:x :y :z}; the nearest within the table-radius setting when nil" :spec ::a/pos :default nil}
   :max-level-cost {:doc "highest level cost of an offer to take; nil for no limit" :spec (a/int-in 0 nil) :default nil}
   :slot {:doc "take exactly offer 1, 2 or 3; nil to choose by :choice" :spec #{1 2 3} :default nil}
   :choice {:doc "\"best\" (dearest affordable offer) or \"cheapest\" (lowest level cost affordable)" :spec #{"best" "cheapest"} :default "best"}
   :fetch {:doc "get a missing item or lapis (jobs.lib.need): true, a set of kinds or a map of limits; false stops with :no-item / :no-lapis" :spec need/option? :default true}})

(a/defargs settings
  {::table-radius {:default 16 :doc "How far to look for a table, in blocks." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 120 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}
   ::reach {:default 3 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}
   ::walk-s {:default 10 :doc "Seconds the walk to the table takes (the quote)." :spec (a/num-in 0 nil)}
   ::visit-s {:default 5 :doc "Seconds the offers and the enchant take at the table (the quote)." :spec (a/num-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn table-radius [] (settings/get settings ::table-radius))

(def target [:item :table :slot :choice])

(def safe? ready/safe-here-by-night)
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(defn problem
  "The cause {:reason :bad-args} when no item name is given."
  [c]
  (let [i (:item (:args c))]
    (when-not (and (string? i) (not (str/blank? i)))
      {:reason :bad-args :why "no item name"})))

(def lapis "lapis_lazuli")

(defn lapis-wanted
  "How much lapis the visit may spend: the slot asked for, else the dearest offer."
  [c]
  (or (:slot (:args c)) 3))

(defn under-way?
  "An attempt was written before the call: the facts changed with it, the round reports what it finds."
  [c]
  (or (:attempt (ctx/mem c)) (:enchanted (ctx/mem c))))

(defn carries [c name] (storage/carried (u/inventory (:primitives c)) name))

(defn lacking
  "The cause :no-item or :no-lapis for what is not carried when nothing fetches it."
  [c]
  (let [{:keys [item fetch]} (:args c)]
    (when-not (or (under-way? c) (need/parse-arg fetch) (not (string? item)))
      (cond
        (zero? (carries c item)) {:reason :no-item :item item}
        (zero? (carries c lapis)) {:reason :no-lapis}))))

(defn unaffordable
  "The cause :too-few-levels when the body has no level: the cheapest offer costs one."
  [c]
  (when-not (under-way? c)
    (when (zero? (or (some-> (.self (:primitives c)) .-experience .-level) 0))
      {:reason :too-few-levels :need 1})))

(defn can-start? [c]
  (contract/all [(checks/cause-status :never (problem c)) (checks/cause-status :later (lacking c)) (checks/cause-status :later (unaffordable c))]))

(defn done?
  "Ok once the offer is bought (the round ends the call with the data)."
  [c]
  (if (:enchanted (ctx/mem c))
    (contract/ok :enchanted "enchanted" :units :items :done 1 :left 0 :result {:enchanted true})
    (contract/later :enchanting "enchant: the round ends the call" :units :items :done 0 :left 1 :result {:enchanted false})))

(defn quote-of [_c]
  (let [walk (settings/get settings ::walk-s)
        visit (settings/get settings ::visit-s)]
    {:uses {:job-time [visit (+ walk visit) (+ walk (* 3 visit))]
            :items {"lapis_lazuli" [1 2 3]}}}))

;; ------------------------------------------------------------------ the choice

(defn need-of
  "The levels an offer needs: its level cost, and at least its slot number."
  [{:keys [level-cost lapis-cost]}]
  (max level-cost lapis-cost))

(defn choose-offer
  "Pick an offer from the read ones ({:index :level-cost :lapis-cost :hint}): {:choice index :offer o}, or {:reason r}
  (with :need, the least levels any offer in budget needs, for too-few-levels). m: :offers :xp :lapis :max-level-cost
  :slot (1-3 or nil) :rule (\"best\" or \"cheapest\")."
  [{:keys [offers xp lapis max-level-cost slot rule]}]
  (let [open (filter #(pos? (:level-cost %)) offers)
        pool (if slot (filter #(= (dec slot) (:index %)) open) open)
        in-budget (filter #(or (nil? max-level-cost) (<= (:level-cost %) max-level-cost)) pool)
        paid (filter #(<= (:lapis-cost %) lapis) in-budget)
        able (filter #(<= (need-of %) xp) paid)
        order (if (= "cheapest" rule)
                (juxt :level-cost :index)
                (juxt (comp - :level-cost) :index))]
    (cond
      (empty? open) {:reason "not-enchantable"}
      (empty? pool) {:reason "no-offer"}
      (empty? in-budget) {:reason "no-offer-within-cost"}
      (empty? paid) {:reason "no-lapis"}
      (empty? able) {:reason "too-few-levels" :need (apply min (map need-of paid))}
      :else (let [o (first (sort-by order able))] {:choice (:index o) :offer o}))))

;; ------------------------------------------------------------------ the round

(def condition-reasons
  "Give-ups that are a fact about the call; any other give-up is a failed try."
  #{:no-table :not-a-table :no-item :already-enchanted :not-enchantable :no-lapis :too-few-levels :no-offer
    :no-offer-within-cost :inventory-full})

(defn finish!
  "The ending of kind with cause; the data defaults to nothing spent."
  [c kind cause extra]
  (ctx/end c kind cause (merge {:enchanted false :item (:item (:args c)) :levels-spent 0 :lapis-spent 0} extra)))

(defn give-up!
  "Warn and end with a reason (and what was spent, when anything was)."
  [c reason extra]
  (let [reason (keyword reason)]
    (finish! c (if (condition-reasons reason) :condition-failed :nothing-possible) reason
             (assoc extra :text (str "enchant gave up: " (name reason))))))

(defn fail-up!
  "Count a failed attempt in a row: :again until u/max-failures, then give up with the reason."
  [c reason]
  (if (u/count-fail! c)
    (give-up! c reason {})
    :again))

(defn find-table
  "The position of the nearest enchanting table the body has seen within radius, or nil."
  [p radius]
  (:pos (first (look/seen-blocks p {:names ["enchanting_table"] :radius radius :max 8 :live? true}))))

(defn ^:async visit!
  "One table visit through act, as a cljs map."
  [c pos op extra]
  (let [r (await (ctx/act c :enchant (clj->js (merge {:pos pos :op op :item (:item (:args c))} extra))))]
    (js->clj r :keywordize-keys true)))

(defn offer-rows
  "The offers of a read as {:index :level-cost :lapis-cost :hint}."
  [r]
  (mapv (fn [o] {:index (:index o) :level-cost (:levelCost o) :lapis-cost (:lapisCost o) :hint (:hint o)}) (:offers r)))

(defn done!
  "Info and finish with the measured result."
  [c r offer]
  (let [result {:enchanted true :slot (inc (:choice r)) :level-cost (:level-cost offer) :levels-spent (:levelsSpent r)
                :lapis-spent (:lapisSpent r) :enchants (:enchants r) :xp-level (:xpLevel r) :hint (:hint offer)}]
    (ctx/update-mem! c assoc :enchanted true)
    (finish! c :done nil (assoc result :text (str "enchanted " (:item (:args c)))))))

(defn resumed!
  "The attempt noted before the call found its item enchanted: report it, spending unknown."
  [c]
  (let [{:keys [slot level-cost]} (:attempt (ctx/mem c))
        result {:enchanted true :resumed true :slot slot :level-cost level-cost :enchants nil}]
    (ctx/update-mem! c assoc :enchanted true)
    (finish! c :done nil (assoc result :text (str "enchanted " (:item (:args c)) " (found done after a restart)")))))

(def failed-reasons {"enchant-stalled" "window-stalled" "not-confirmed" "not-confirmed" "item-not-returned" "not-confirmed"})

(defn spent-of [r] {:levels-spent (or (:levelsSpent r) 0) :lapis-spent (or (:lapisSpent r) 0)})

(defn ^:async buy!
  "Enchant with the chosen offer and handle the outcome."
  [c pos {:keys [choice offer]} read]
  (ctx/update-mem! c assoc :attempt {:slot (inc choice) :level-cost (:level-cost offer) :xp (:xpLevel read) :lapis (:lapis read)})
  (let [r (await (visit! c pos "enchant" {:choice choice :levelCost (:level-cost offer)}))]
    (case (:status r)
      "enchanted" (done! c r offer)
      "no-lapis" (give-up! c "no-lapis" {})
      "no-levels" (give-up! c "too-few-levels" {})
      "full" (give-up! c "inventory-full" {})
      "cannot" (case (:reason r)
                 "not-enchantable" (give-up! c "not-enchantable" {})
                 "already-enchanted" (give-up! c "already-enchanted" {})
                 (fail-up! c (str (:reason r))))
      "timeout" (if (:inventoryChange r) :again (fail-up! c "timeout"))
      "failed" (give-up! c (get failed-reasons (:reason r) "enchant-failed") (spent-of r))
      "no-item" (give-up! c "no-item" {})
      "missing" (give-up! c "no-table" {})
      "unreachable" (fail-up! c "unreachable")
      (give-up! c "enchant-failed" {}))))

(defn ^:async consider!
  "Read the offers and choose, or give up."
  [c pos]
  (let [{:keys [max-level-cost slot choice]} (:args c)
        r (await (visit! c pos "offers" {}))]
    (case (:status r)
      "ok" (let [picked (choose-offer {:offers (offer-rows r) :xp (:xpLevel r) :lapis (:lapis r) :max-level-cost max-level-cost :slot slot :rule choice})]
             (if (:choice picked)
               (await (buy! c pos picked r))
               (give-up! c (:reason picked) {})))
      "missing" (give-up! c "no-table" {})
      "cannot" (case (:reason r)
                 "not-a-table" (give-up! c "not-a-table" {})
                 "already-enchanted" (if (:attempt (ctx/mem c)) (resumed! c) (give-up! c "already-enchanted" {}))
                 (give-up! c (str (:reason r)) {}))
      "no-item" (give-up! c "no-item" {})
      "unreachable" (fail-up! c "unreachable")
      (fail-up! c "window"))))

(defn ^:async step!
  "One piece: find and reach the table (a go-to child), then read the offers and enchant. :again, :continue (go-to
  waits) or :done."
  [c]
  (let [table (:table (:args c))
        pos (or table (find-table (:primitives c) (table-radius)))]
    (if-not pos
      (if (look/surveyed? c)
        (give-up! c "no-table" {})
        (do (await (look/survey! c)) :again)) ; a table behind the body is not seen until it looks
      (let [r (if (u/within? (u/self-pos c) pos (reach))
                :done
                (await (child/call-walk* c :walk {:pos pos :range (reach) :ways #{:walk :clear-seal} :spend {:tries 1}})))]
        (cond
          (= :continue r) :continue
          (not (or (u/within? (u/self-pos c) pos (reach)) (:arrived (:data r)))) (fail-up! c "unreachable")
          :else (await (consider! c pos)))))))

(defn ^:async supply!
  "Fetch the item, then the lapis, when one is not carried and :fetch allows: nil when both are, :continue while a
  fetch waits, else the fetch's ending."
  [c]
  (let [{:keys [item]} (:args c)
        opts (need/opts c 'jobs.items.enchant)
        lacks (when-not (under-way? c)
                (cond (zero? (carries c item)) [item 1]
                      (zero? (carries c lapis)) [lapis (lapis-wanted c)]))
        e (when (and lacks opts) (await (need/item! c (first lacks) (second lacks) opts)))]
    (when-not (or (nil? e) (= :done (:kind e))) e)))

(defn ^:async round
  "The whole enchant in one call: step! again until it is done or stopped."
  [c]
  (let [e (await (supply! c))]
    (cond
      (= :continue e) :continue
      e (finish! c (:kind e) (:cause e) {})
      :else (await (pace/steps! c (fn ^:async enchant-step [] (await (step! c))))))))
