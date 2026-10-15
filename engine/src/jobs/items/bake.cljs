(ns jobs.items.bake
  (:require [engine.settings :as settings]
            [jobs.lib.game-facts :as facts]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.need :as getting]
            [jobs.lib.started :as started]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.look :as look]
            [jobs.lib.storage :as storage]
            [jobs.storage.least :as least]))

(def doc
  "Turn the store chest's wheat into bread. Walks to the chest, finds a crafting table within :table-radius of it,
  takes the wheat out, crafts it (jobs.items.craft), puts the bread back (jobs.storage.deposit) and carries
  :keep loaves. A shortfall of bread is taken from the chest.
  Only whole loaves are made. At most three stacks of wheat are taken per trip, and empty slots are left for the
  bread. Bread is never tossed: what cannot be put away stays carried.
  One call is the whole bake (it yields :continue only while a child waits on the world); every step starts from the inventory and the chest, so a cut loses nothing.
  Needs (condition, at the start only): a known chest, else :no-chest. Ends done with data {:baked n :deposited n} (also at any other ending when at least the call's :least loaves were baked); with
  nothing baked it is done only when :keep loaves are carried or were taken from the chest, else nothing-possible
  {:reason :no-wheat} (info bake.nothing). A crafting table not near the chest is fetched (jobs.lib.need) when :fetch is
  on. Every other ending has the same data, after a warn (bake.no-table, bake.gave-up,
  bake.deposit-failed, bake.withdraw-failed, bake.craft-failed, bake.refused):
  - condition-failed :no-table (before anything was taken), :unreachable, :inventory-full, :refused {:zones :claims}
    (a zone or claim refuses the chest), or the cause of the child that stopped (deposit, withdraw, craft; a craft that
    came short of wheat after making bread is progress);
  - nothing-possible {:reason :chest :status s}: the chest could not be read three times in a row.
  Quote: a walk, the craft and the chest trips; yields bread. done? counts the loaves baked.")

(a/defargs args
  {:chest {:doc "store chest position [x y z] or {:x :y :z}; the known :chest place when nil" :spec ::a/pos :default nil}
   :keep {:doc "loaves to carry when done" :spec (a/int-in 0 nil) :default 16}
   :table-radius {:doc "how far from the chest the table may be" :spec (a/num-in 0 nil) :default 8}
   :fetch {:doc "get a crafting table (jobs.lib.need) when none is near the chest; false ends no-table" :spec boolean? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(a/defargs settings
  {::per-trip-max {:default 192 :doc "Items carried to the furnace in one trip." :spec (a/int-in 0 nil)}
   ::time-s {:default 300 :doc "Seconds a bake may last: the walks, the trips to the chest and the crafts." :spec (a/num-in 0 nil)}
   ::loaf-s {:default 2 :doc "Seconds a quote counts for each loaf: the chest trips and the craft share." :spec (a/num-in 0 nil)}})
(defn per-trip-max [] (settings/get settings ::per-trip-max))

(def target [:chest])

(def defaults {:least :all :spend {:time ::time-s}})

(defn no-chest
  "A chest is known."
  [c]
  (when-not (storage/chest-of (ctx/view c) (:args c))
    {:reason :no-chest}))

(def conditions [[:fn (started/at-start no-chest)]])

(defn chest-of!
  "The chest of this call, kept in memory at the first look so a place forgotten meanwhile does not lose it."
  [c]
  (or (:chest (ctx/mem c))
      (when-let [chest (storage/chest-of (ctx/view c) (:args c))]
        (ctx/update-mem! c assoc :chest chest)
        chest)))

(defn carried
  "How many of name the inventory holds over all stacks."
  [p name]
  (storage/carried (u/inventory p) name))

(defn counts
  "The :baked and :deposited counts so far. Baked is derived from the bread:
  carried now plus deposited, less what was carried at the start and what the
  top-up took out, so a cut or resumed craft child cannot lose a loaf."
  [c]
  (let [m (ctx/mem c)
        deposited (:deposited m 0)
        bread (carried (:primitives c) "bread")]
    {:baked (max 0 (- (+ bread deposited) (:bread0 m 0) (:topped m 0)))
     :deposited deposited}))

(defn done?
  "The loaves baked so far (the round ends the call with the counts)."
  [c]
  {:done? false :units :loaves :done (:baked (counts c)) :left 0})

(defn quote-of
  "A walk and the loaves the chest holds (at most the time default); yields bread."
  [c]
  (let [loaf (settings/get settings ::loaf-s)
        t (settings/get settings ::time-s)]
    {:uses {:time [loaf (* 10 loaf) t]}
     :yields {:items {"bread" [0 (:keep (:args c) 0) (* 64 3)]}}}))

(defn end!
  "End with kind and cause; the data is the counts (and the extra fields)."
  ([c kind cause] (end! c kind cause nil))
  ([c kind cause more] (let [data (merge (counts c) more)]
                         (least/end! c kind cause data (:baked data) nil))))

(defn stop!
  "Warn of event and end condition-failed with cause."
  [c event text cause]
  (ctx/emit! c event :warn {:text text :reason (name cause)})
  (end! c :condition-failed cause))

(defn child-stop!
  "End like the child that stopped (event names the warn, text starts it): a refusal keeps its zones and claims."
  [c event what e]
  (let [cause (contract/cause-key e)
        fields (select-keys (:data e) [:zones :claims])]
    (ctx/emit! c event :warn (assoc fields :reason cause :text (if (= :refused cause)
                                                                  "cannot take from or put into the chest: refused by a zone or claim"
                                                                  (str what ": " (name cause)))))
    (end! c (:kind e) (:cause e) fields)))

(defn child-ok?
  "Whether the child ending e is a success: done, or a chest that merely lacks part of what was asked."
  [e]
  (or (= :done (:kind e)) (false? (get-in e [:data :gave-up]))))

(defn give-up!
  "u/fail!, and when it gives up end nothing-possible with cause."
  [c text cause]
  (if (= :done (u/fail! c :bake.gave-up text))
    (end! c :nothing-possible cause)
    :again))

(defn nearest-table
  "The position of the crafting table nearest the chest within radius of it
  (one the body has seen within 16 of it), or nil."
  [p chest radius]
  (->> (look/seen-blocks p {:names ["crafting_table"] :radius 16 :max 8 :live? true})
       (map :pos)
       (filter #(<= (u/dist % chest) radius))
       (sort-by #(u/dist % chest))
       first))

(defn wheat-target
  "How much wheat to carry after the next trip: whole loaves of what the chest
  and the hands hold, at most three stacks, and no more than the inventory has
  room for. Crafting at a table needs empty slots left for the bread (two when
  no bread is carried yet, one when a stack of it is), so those stay free; the
  stack already carried fills first. Under that no wheat is taken."
  [p chest-wheat]
  (let [cw (carried p "wheat")
        free (u/free-slots p)
        reserved (if (pos? (carried p "bread")) 1 2)
        room (+ (* facts/stack-size (- free reserved)) (mod (- cw) facts/stack-size))]
    (if (< free reserved)
      0
      (min (* 3 (quot (+ chest-wheat cw) 3)) (per-trip-max) (+ cw room)))))

(defn ^:async deposit-spare!
  "Bread above :keep goes back into the chest through the deposit child."
  [c chest bread]
  (let [{:keys [keep]} (:args c)
        dargs (merge (select-keys (:args c) [:ignore-zones?]) {:chest chest :items ["bread"] :keep {"bread" keep}})
        e (await (ctx/call c :deposit 'jobs.storage.deposit dargs))]
    (cond
      (= :continue e) :continue
      (not (child-ok? e)) (child-stop! c :bake.deposit-failed "cannot put the bread away" e)
      :else (do (ctx/update-mem! c update :deposited (fnil + 0) (max 0 (- bread (carried (:primitives c) "bread"))))
                :again))))

(defn ^:async craft-carried!
  "Craft the carried wheat into bread through the craft child (no table: the carried one is put down)."
  [c table wheat]
  (let [cargs (cond-> {:item "bread" :count (quot wheat 3) :fetch false} table (assoc :table table))
        e (await (ctx/call c :craft 'jobs.items.craft cargs))]
    (cond
      (= :continue e) :continue
      (or (= :done (:kind e)) (and (= :need (contract/cause-key e)) (pos? (get-in e [:data :made] 0))))
      (do (u/progress! c) :again)
      :else (child-stop! c :bake.craft-failed "cannot craft the bread" e))))

(defn ^:async withdraw!
  "Take the wheat out through the withdraw child, carrying target in all."
  [c chest target]
  (let [wargs (merge (select-keys (:args c) [:ignore-zones?]) {:chest chest :items {"wheat" target}})
        e (await (ctx/call c :withdraw 'jobs.storage.withdraw wargs))]
    (cond
      (= :continue e) :continue
      (not (child-ok? e)) (child-stop! c :bake.withdraw-failed "cannot take the wheat out" e)
      :else (do (u/progress! c) :again))))

(defn finish-done!
  "Emit bake.done with the counts and end done; with no loaf baked or taken and fewer than :keep carried, bake.nothing
  and nothing-possible."
  [c]
  (let [{:keys [baked]} (counts c)]
    (if (or (pos? baked) (pos? (:topped (ctx/mem c) 0)) (>= (carried (:primitives c) "bread") (:keep (:args c))))
      (do (ctx/emit! c :bake.done :info (assoc (counts c) :text (str "baked " baked)))
          (end! c :done nil))
      (do (ctx/emit! c :bake.nothing :info {:text "no whole loaf of wheat in the chest"})
          (end! c :nothing-possible {:reason :no-wheat})))))

(defn ^:async top-up!
  "No loaf of wheat is left: carry :keep bread from the chest, then finish."
  [c chest chest-items]
  (let [{:keys [keep]} (:args c)
        p (:primitives c)
        held (reduce + 0 (map #(.-count %) (filter #(= "bread" (.-name %)) (array-seq chest-items))))]
    (if-not (and (pos? held) (< (carried p "bread") keep))
      (finish-done! c)
      (let [before (carried p "bread")
            targs (merge (select-keys (:args c) [:ignore-zones?]) {:chest chest :items {"bread" keep}})
            e (await (ctx/call c :topup 'jobs.storage.withdraw targs))]
        (ctx/update-mem! c update :topped (fnil + 0) (max 0 (- (carried p "bread") before)))
        (cond
          (= :continue e) :continue
          (not (child-ok? e)) (child-stop! c :bake.withdraw-failed "cannot take the bread out" e)
          :else (finish-done! c))))))

(defn ^:async inspect!
  "Read the chest and take wheat out, top up the bread, or finish. Wheat for a
  loaf in the chest but no room for it ends as inventory-full, taking nothing."
  [c chest]
  (let [p (:primitives c)
        seen (await (ctx/act c :inspectContainer (clj->js {:pos chest})))]
    (if (not= "ok" (.-status seen))
      (give-up! c (str "chest " (.-status seen)) {:reason :chest :status (.-status seen)})
      (let [items (.-items seen)
            wheat (reduce + 0 (map #(.-count %) (filter #(= "wheat" (.-name %)) (array-seq items))))
            target (wheat-target p wheat)
            whole (* 3 (quot (+ wheat (carried p "wheat")) 3))]
        (cond
          (>= target 3) (await (withdraw! c chest target))

          (>= whole 3)
          (do (ctx/emit! c :bake.inventory-full :info {:text "no room for the wheat and the bread it becomes"})
              (end! c :condition-failed :inventory-full))

          :else (await (top-up! c chest items)))))))

(defn ^:async step!
  "One piece of the bake: find the table (walking to the chest to see it, once), put spare bread away, craft carried
  wheat, else walk to the chest and look in it. The deposit and craft children walk themselves, so the body is not
  drawn back to the chest between the table and the bread. The table is looked up again at every step, so one broken
  meanwhile is noticed. :again, :continue (a child waits) or :done."
  [c]
  (let [p (:primitives c)
        chest (chest-of! c)
        _ (when-not (contains? (ctx/mem c) :bread0)
            (ctx/update-mem! c assoc :bread0 (carried p "bread")))
        bread (carried p "bread")
        wheat (carried p "wheat")
        spare? (> bread (:keep (:args c)))
        craft? (and (not spare?) (>= wheat 3))
        table (nearest-table p chest (:table-radius (:args c)))
        walk? (if (and table (or spare? craft?)) false (not (u/within? (u/self-pos c) chest 3)))
        w (when walk?
            (await (child/call-walk* c :walk {:pos chest :range 3 :escalate false :warn false :retry false :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))})))]
    (cond
      (= :continue w) :continue
      (and w (not (:arrived (:data w)))) (stop! c :bake.gave-up "cannot reach the chest" :unreachable)
      :else
      (let [table (or table (nearest-table p chest (:table-radius (:args c))))]
        (cond
          (and (nil? table) (not (look/surveyed? c)))
          (do (await (look/survey! c)) :again) ; a table behind the body is not seen until it looks

          (and (nil? table) (not (:fetch (:args c))))
          (stop! c :bake.no-table "no crafting table near the chest" :no-table)

          spare? (await (deposit-spare! c chest bread))
          craft? (let [t (when (nil? table) (await (getting/table! c false)))]
                   (cond
                     (= :continue t) :continue
                     (and t (not= :done (:kind t))) (stop! c :bake.no-table "no crafting table near the chest" :no-table)
                     :else (await (craft-carried! c table wheat))))
          :else (await (inspect! c chest)))))))

(defn ^:async round
  "The whole bake in one call: step! again until it is done or stopped."
  [c]
  (await (pace/steps! c (fn ^:async bake-step [] (await (step! c))))))
