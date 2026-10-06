(ns jobs.items.bake
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]
            [engine.path.near :as near]
            [jobs.items.smelt :as smelt]
            [jobs.storage.deposit :as deposit]))

(def doc
  "Turn the store chest's wheat into bread. Walks to the chest, finds a crafting table within :table-radius of it,
  takes the wheat out, crafts it (jobs.items.craft), puts the bread back (jobs.storage.deposit) and carries
  :keep loaves. A shortfall of bread is taken from the chest.
  Only whole loaves are made. At most three stacks of wheat are taken per trip, and empty slots are left for the
  bread. Bread is never tossed: what cannot be put away stays carried.
  Every round starts from the inventory and the chest, so a cut loses nothing.
  Ends with {:baked n :deposited n}. No wheat for a loaf is a success (info bake.nothing).
  A stop adds :reason, after a warn (bake.no-table, bake.gave-up, bake.deposit-failed, bake.withdraw-failed,
  bake.craft-failed). :reason is \"no-table\" (before anything was taken), \"unreachable\", \"inventory-full\",
  \"chest <status>\", \"deposit <reason>\", \"withdraw <reason>\", \"craft <reason>\" or :refused (a zone or claim
  refuses the chest; bake.refused).")

(def args
  {:chest {:doc "store chest position; the known :chest place when nil" :default nil}
   :keep {:doc "loaves to carry when done" :default 16}
   :table-radius {:doc "how far from the chest the table may be" :default 8}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def per-trip-max 192)
(def stack-size 64)

(defn check
  "A chest is known."
  [c]
  (boolean (deposit/chest-of (ctx/view c) (:args c))))

(defn carried
  "How many of name the inventory holds over all stacks."
  [p name]
  (deposit/carried (u/inventory p) name))

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

(defn finish!
  "Hand the parent the counts plus extra and return :done."
  [c extra]
  (ctx/result! c (merge (counts c) extra))
  :done)

(defn stop!
  "Warn of kind and finish with the counts and a reason."
  [c kind text reason]
  (ctx/emit! c kind :warn {:text text :reason reason})
  (finish! c {:reason reason}))

(defn refused!
  "Finish with the withdraw or deposit child's refusal out ({:reason :refused :zones :claims}): no retry."
  [c kind out]
  (let [fields (select-keys out [:zones :claims])]
    (ctx/emit! c kind :warn (assoc fields :reason :refused :text "cannot take from or put into the chest: refused by a zone or claim"))
    (finish! c (assoc fields :reason :refused))))

(defn give-up!
  "u/fail!, and when it gives up finish with the reason."
  [c text reason]
  (let [r (u/fail! c :bake.gave-up text)]
    (when (= :done r) (finish! c {:reason reason}))
    r))

(defn nearest-table
  "The position of the crafting table nearest the chest within radius of it
  (one the body has seen within 16 of it), or nil."
  [p chest radius]
  (->> (smelt/seen-blocks p ["crafting_table"] 16 8)
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
        room (+ (* stack-size (- free reserved)) (mod (- cw) stack-size))]
    (if (< free reserved)
      0
      (min (* 3 (quot (+ chest-wheat cw) 3)) per-trip-max (+ cw room)))))

(defn ^:async deposit-spare!
  "Bread above :keep goes back into the chest through the deposit child."
  [c chest bread]
  (let [{:keys [keep]} (:args c)
        r (await (ctx/call-child c :deposit 'jobs.storage.deposit
                                 (merge (select-keys (:args c) [:ignore-zones?])
                                        {:chest chest :items ["bread"] :keep {"bread" keep}})))
        out (ctx/child-result c :deposit)]
    (cond
      (and (= :done r) (= :refused (:reason out))) (refused! c :bake.refused out)

      (and (= :done r) (:gave-up out))
      (stop! c :bake.deposit-failed (str "cannot put the bread away: " (:reason out))
             (str "deposit " (:reason out)))

      (contains? #{:done :continue} r)
      (do (ctx/update-mem! c update :deposited (fnil + 0) (max 0 (- bread (carried (:primitives c) "bread"))))
          :continue)

      :else :continue)))

(defn ^:async craft-carried!
  "Craft the carried wheat into bread through the craft child."
  [c table wheat]
  (ctx/update-mem! c assoc :started true)
  (let [r (await (ctx/call-child c :craft 'jobs.items.craft
                                 {:item "bread" :count (quot wheat 3) :table table}))
        out (ctx/child-result c :craft)]
    (if-not (= :done r)
      :continue
      (do (if (:reason out)
            (stop! c :bake.craft-failed (str "cannot craft the bread: " (:reason out))
                   (str "craft " (:reason out)))
            :continue)))))

(defn ^:async withdraw!
  "Take the wheat out through the withdraw child, carrying target in all."
  [c chest target]
  (ctx/update-mem! c assoc :started true)
  (let [r (await (ctx/call-child c :withdraw 'jobs.storage.withdraw
                                 (merge (select-keys (:args c) [:ignore-zones?])
                                        {:chest chest :items {"wheat" target}})))
        out (ctx/child-result c :withdraw)]
    (cond
      (and (= :done r) (= :refused (:reason out))) (refused! c :bake.refused out)
      (and (= :done r) (:gave-up out)) (stop! c :bake.withdraw-failed (str "cannot take the wheat out: " (:reason out))
                                              (str "withdraw " (:reason out)))
      :else :continue)))

(defn finish-done!
  "Emit bake.done with the counts and finish."
  [c]
  (ctx/emit! c :bake.done :info (assoc (counts c) :text (str "baked " (:baked (counts c)))))
  (finish! c {}))

(defn ^:async top-up!
  "No loaf of wheat is left: carry :keep bread from the chest, then finish."
  [c chest chest-items]
  (let [{:keys [keep]} (:args c)
        p (:primitives c)
        held (reduce + 0 (map #(.-count %) (filter #(= "bread" (.-name %)) (array-seq chest-items))))]
    (if-not (and (pos? held) (< (carried p "bread") keep))
      (finish-done! c)
      (let [before (carried p "bread")
            r (await (ctx/call-child c :topup 'jobs.storage.withdraw
                                     (merge (select-keys (:args c) [:ignore-zones?])
                                            {:chest chest :items {"bread" keep}})))
            out (ctx/child-result c :topup)]
        (ctx/update-mem! c update :topped (fnil + 0) (max 0 (- (carried p "bread") before)))
        (cond
          (and (= :done r) (= :refused (:reason out))) (refused! c :bake.refused out)
          (and (= :done r) (:gave-up out)) (stop! c :bake.withdraw-failed (str "cannot take the bread out: " (:reason out))
                                                  (str "withdraw " (:reason out)))
          (= :done r) (finish-done! c)
          :else :continue)))))

(defn ^:async inspect!
  "Read the chest and take wheat out, top up the bread, or finish. Wheat for a
  loaf in the chest but no room for it ends as inventory-full, taking nothing."
  [c chest]
  (let [p (:primitives c)
        seen (await (ctx/act c :inspectContainer (clj->js {:pos chest})))]
    (if (not= "ok" (.-status seen))
      (let [r (u/fail! c :bake.gave-up (str "chest " (.-status seen)))]
        (when (= :done r) (finish! c {:reason (str "chest " (.-status seen))}))
        r)
      (let [items (.-items seen)
            wheat (reduce + 0 (map #(.-count %) (filter #(= "wheat" (.-name %)) (array-seq items))))
            target (wheat-target p wheat)
            whole (* 3 (quot (+ wheat (carried p "wheat")) 3))]
        (cond
          (>= target 3) (await (withdraw! c chest target))

          (>= whole 3)
          (do (ctx/emit! c :bake.inventory-full :info {:text "no room for the wheat and the bread it becomes"})
              (finish! c {:reason "inventory-full"}))

          (not (:started (ctx/mem c)))
          (do (ctx/emit! c :bake.nothing :info {:text "no whole loaf of wheat in the chest"})
              (finish! c {}))

          :else (await (top-up! c chest items)))))))

(defn ^:async round
  "One bounded step: reach the chest, find the table, put spare bread away,
  craft carried wheat, else look in the chest."
  [c]
  (let [p (:primitives c)
        chest (deposit/chest-of (ctx/view c) (:args c))
        _ (when-not (contains? (ctx/mem c) :bread0)
            (ctx/update-mem! c assoc :bread0 (carried p "bread")))
        w (await (near/walk-near! c chest 3))]
    (case w
      :partial :continue
      :blocked (give-up! c "cannot reach the chest" "unreachable")
      (let [table (or (:table (ctx/mem c)) (nearest-table p chest (:table-radius (:args c))))
            bread (carried p "bread")
            wheat (carried p "wheat")]
        (cond
          (nil? table)
          (stop! c :bake.no-table "no crafting table near the chest" "no-table")

          :else
          (do (ctx/update-mem! c assoc :table table)
              (cond
                (> bread (:keep (:args c))) (await (deposit-spare! c chest bread))
                (>= wheat 3) (await (craft-carried! c table wheat))
                :else (await (inspect! c chest)))))))))
