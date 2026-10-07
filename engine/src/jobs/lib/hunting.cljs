(ns jobs.lib.hunting
  "The kill-and-collect steps combat.hunt and animals.cull share: the drops table, the attack child call, the
  drop collection and the search wait. Memory keys: :target, :killed, :skipped, :skips, :collecting, :misses."
  (:require [engine.ctx :as ctx]))

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

(defn book-outcome!
  "Book the attack child's result for target: a kill, or a skipped animal. Then
  start collecting."
  [c target]
  (let [killed? (some #{target} (:killed (ctx/child-result c :attack)))]
    (ctx/update-mem! c (fn [m]
                         (-> (if killed?
                               (-> m (update :killed (fnil inc 0)) (assoc :skips 0))
                               (-> m (update :skipped (fnil conj []) target) (update :skips (fnil inc 0))))
                             (dissoc :target)
                             (assoc :collecting true))))))

(defn ^:async attack!
  "Call the attack child on the target (a whole fight); books its end. opts: :radius (search radius of the child),
  :waiting (what to return while it waits on the world), :give-up (fn of c, called after too many skips in a row).
  Shared by combat.hunt and animals.cull."
  [c {:keys [radius waiting give-up]}]
  (let [target (:target (ctx/mem c))
        r (await (ctx/call-child c :attack 'jobs.combat.attack
                                 {:targets [target] :radius radius :weapons (:weapons (:args c)) :lost-s 1 :absent :done}))]
    (cond
      (not= :done r) waiting
      :else (do (book-outcome! c target)
                (if (>= (:skips (ctx/mem c) 0) (:max-skips (:args c)))
                  (give-up c)
                  :again)))))

(defn ^:async collect!
  "Call the collect-drops child; done collecting when it is, else waiting (what to return while it waits on the world)."
  [c waiting]
  (let [{:keys [mob collect-radius] :as a} (:args c)
        r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                 {:radius collect-radius :filter (or (:drops a) (get drops mob))}))]
    (if (= :done r)
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
