(ns jobs.lib.hunting
  "The kill-and-collect steps combat.hunt and animals.cull share: the drops table, the attack child call, the
  drop collection and the search wait. Memory keys: :target, :killed, :skipped, :skips, :collecting, :misses."
  (:require [engine.ctx :as ctx]
            [jobs.lib.util :as u]))

(def raw-meats
  {"beef" "cooked_beef" "porkchop" "cooked_porkchop" "mutton" "cooked_mutton"
   "chicken" "cooked_chicken" "rabbit" "cooked_rabbit"})

(def wool-colors
  ["white" "orange" "magenta" "light_blue" "yellow" "lime" "pink" "gray"
   "light_gray" "cyan" "purple" "blue" "brown" "green" "red" "black"])

(def drops
  "What each huntable kind drops, raw and cooked meat included, by mob name."
  (let [raw {"cow" ["beef" "leather"]
             "mooshroom" ["beef" "leather"]
             "pig" ["porkchop"]
             "sheep" (into ["mutton"] (map #(str % "_wool")) wool-colors)
             "chicken" ["chicken" "feather"]
             "rabbit" ["rabbit" "rabbit_hide" "rabbit_foot"]}]
    (update-vals raw (fn [items] (into items (keep raw-meats) items)))))

(def max-skips
  "Animals skipped in a row before the hunt or cull gives up."
  3)

(def collect-radius
  "Blocks around the last kill in which its drops are collected."
  8)

(defn note-target!
  "Remember the passive target as last seen (:animal, :entity id, :uuid, :cell, :by job and the rule that allowed the kill, from (rule-fn c entity)),
  for the kill event: the entity is gone once it is dead."
  [c target {:keys [radius job rule-fn]}]
  (when-let [e (->> (array-seq (.entities (:primitives c) #js {:radius radius :names #js [(:mob (:args c))] :max 64}))
                    (filter #(and (= target (.-id %)) (= "passive" (.-kind %))))
                    first)]
    (let [p (u/pos-of (.-pos e))]
      (ctx/update-mem! c assoc :target-seen
                       {:animal (.-name e) :entity target :uuid (.-uuid e) :by job :rule (rule-fn c e)
                        :cell (mapv #(js/Math.floor %) [(:x p) (:y p) (:z p)])}))))

(defn kill-event!
  "Info animal.killed for the passive target last seen, when it died."
  [c target]
  (when-let [seen (:target-seen (ctx/mem c))]
    (when (= target (:entity seen))
      (ctx/emit! c :animal.killed :info
                 (assoc seen :text (str (:by seen) " killed a " (:animal seen) " at " (pr-str (:cell seen)) " (" (name (:rule seen)) ")"))))))

(defn book-outcome!
  "Book the attack child's ending e for target: a kill, or a skipped animal. Then start collecting."
  [c target e]
  (let [killed? (some #{target} (get-in e [:data :killed]))]
    (when killed? (kill-event! c target))
    (ctx/update-mem! c (fn [m]
                         (-> (if killed?
                               (-> m (update :killed (fnil inc 0)) (assoc :skips 0))
                               (-> m (update :skipped (fnil conj []) target) (update :skips (fnil inc 0))))
                             (dissoc :target)
                             (assoc :collecting true))))))

(defn ^:async attack!
  "Call the attack child on the target (a whole fight) with the call's weapons; books its end. :again, :yield while the
  child waits on the world, or the ending after too many skips in a row (opts :give-up, a fn of c).
  opts: :radius (search radius of the child), :job and :rule-fn (see note-target!).
  Shared by combat.hunt and animals.cull."
  [c {:keys [radius give-up] :as opts}]
  (let [target (:target (ctx/mem c))
        _ (note-target! c target opts)
        tools (ctx/use c :tools)
        e (await (ctx/call c :attack 'jobs.combat.attack
                           (cond-> {:targets [target] :radius radius :lost-s 1 :absent :done}
                             tools (assoc :use {:tools tools}))))]
    (if (= :continue e)
      :yield
      (do (book-outcome! c target e)
          (if (>= (:skips (ctx/mem c) 0) max-skips)
            (give-up c)
            :again)))))

(defn collect-ended?
  "Whether the collect-drops child's answer e ends the collecting: any ending but one that declines (condition-failed:
  its call is held by a floor) waits and is tried again while the drops last."
  [e]
  (not (or (= :continue e) (= :condition-failed (:kind e)))))

(defn ^:async collect!
  "Call the collect-drops child; done collecting when it ends, else waiting (what to return while it waits on the world
  or its call is held by a floor)."
  [c waiting]
  (let [{:keys [mob] :as a} (:args c)
        e (await (ctx/call c :collect 'jobs.forestry.collect-drops
                                 {:radius collect-radius :filter (or (:drops a) (get drops mob))}))]
    (if (collect-ended? e)
      (do (ctx/update-mem! c dissoc :collecting) :again)
      waiting)))

(defn ^:async search!
  "Nothing to attack: wait and look once more, then end with (end! c)."
  [c end!]
  (let [misses (inc (:misses (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :misses misses)
    (if (>= misses 2)
      (end! c)
      (do (await (ctx/act c :wait #js {:ms 1000}))
          :again))))
