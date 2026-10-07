(ns jobs.items.enchant
  (:require [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.look :as look]))

(def doc
  "Enchant one :item at an enchanting table. Walks to the table (:table, else the nearest within :radius), reads
  the three offers with the item lying in the table, chooses one and enchants.
  An offer costs its slot number in lapis, and in levels at least its level cost and its slot number. The
  choice: :slot 1-3 takes that offer alone. Otherwise :choice \"best\" takes the dearest offer the body can pay and
  \"cheapest\" the lowest level cost it can pay (the lower slot on a tie). Offers dearer than :max-level-cost
  are ignored.
  Result on success: {:enchanted true :item :slot :level-cost :levels-spent :lapis-spent :enchants [{:name
  :level}] :xp-level :hint {:enchant :level}|nil}, measured from what is carried and the body's level after the
  window closed. Info enchant.done.
  It gives up with {:enchanted false :reason r :levels-spent n :lapis-spent n} and warn enchant.gave-up.
  Reasons: no-table, not-a-table, no-item, already-enchanted (every copy carried is enchanted),
  not-enchantable, no-lapis, too-few-levels, no-offer (the :slot has none), no-offer-within-cost,
  inventory-full, enchant-failed (an answer not understood after the call), window (did not open, three times),
  window-stalled, not-confirmed (the item came back unenchanted), offer-changed (three times), unreachable.
  One call is the whole enchant (it yields :continue only while go-to waits on the world).
  A failed enchant is never repeated, as it may have taken the price. The attempt is written to memory before the
  call. After a restart that finds the item enchanted, the job reports it (:resumed true, enchants unknown).")

(def args
  {:item {:doc "name of the item to enchant, from the inventory" :default nil}
   :table {:doc "the enchanting table position {:x :y :z}; the nearest within :radius when nil" :type :pos :default nil}
   :radius {:doc "how far to look for a table" :default 16}
   :max-level-cost {:doc "highest level cost of an offer to take; nil for no limit" :default nil}
   :slot {:doc "take exactly offer 1, 2 or 3; nil to choose by :choice" :default nil}
   :choice {:doc "\"best\" (dearest affordable offer) or \"cheapest\" (lowest level cost affordable)" :default "best"}})

(def reach 3)

(defn check
  "An item name is given."
  [c]
  (string? (:item (:args c))))

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

(defn finish!
  "Hand the parent the result and return :done. extra is merged over the nothing-spent default."
  [c extra]
  (ctx/result! c (merge {:enchanted false :item (:item (:args c)) :levels-spent 0 :lapis-spent 0} extra))
  :done)

(defn give-up!
  "Warn and finish with a reason (and what was spent, when anything was)."
  [c reason extra]
  (ctx/emit! c :enchant.gave-up :warn {:reason reason :text (str "enchant gave up: " reason)})
  (finish! c (assoc extra :reason reason)))

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
    (ctx/emit! c :enchant.done :info (assoc result :item (:item (:args c)) :text (str "enchanted " (:item (:args c)))))
    (finish! c result)))

(defn resumed!
  "The attempt noted before the call found its item enchanted: report it, spending unknown."
  [c]
  (let [{:keys [slot level-cost]} (:attempt (ctx/mem c))
        result {:enchanted true :resumed true :slot slot :level-cost level-cost :enchants nil}]
    (ctx/emit! c :enchant.done :info (assoc result :item (:item (:args c)) :text (str "enchanted " (:item (:args c)) " (found done after a restart)")))
    (finish! c result)))

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
  (let [{:keys [table radius]} (:args c)
        pos (or table (find-table (:primitives c) radius))]
    (if-not pos
      (give-up! c "no-table" {})
      (let [r (if (u/within? (u/self-pos c) pos reach)
                :done
                (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos pos :range reach :escalate false :warn false :retry false :zone-tolls true})))]
        (cond
          (= :continue r) :continue
          (not (or (u/within? (u/self-pos c) pos reach) (:arrived (ctx/child-result c :walk)))) (fail-up! c "unreachable")
          :else (await (consider! c pos)))))))

(defn ^:async round
  "The whole enchant in one call: step! again until it is done or stopped."
  [c]
  (await (pace/steps! c (fn ^:async enchant-step [] (await (step! c))))))
