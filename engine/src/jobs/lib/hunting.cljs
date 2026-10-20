(ns jobs.lib.hunting
  "The kill-and-collect steps combat.hunt and animals.cull share: the drops table, the attack child call, the
  drop collection and the search wait. Memory keys: :target, :killed, :skipped, :skips, :collecting, :misses."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [engine.settings :as settings]
            [jobs.lib.animals :as animals]
            [jobs.lib.foods :as foods]
            [jobs.lib.look :as look]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.waits :as waits]
            [jobs.lib.util :as u]))

(def raw-drops
  "What each huntable kind drops, by mob name."
  {"cow" ["beef" "leather"]
   "mooshroom" ["beef" "leather"]
   "pig" ["porkchop"]
   "sheep" (into ["mutton"] (map #(str % "_wool")) game/dye-colors)
   "chicken" ["chicken" "feather"]
   "rabbit" ["rabbit" "rabbit_hide" "rabbit_foot"]
   "zombie" ["rotten_flesh"]
   "skeleton" ["bone" "arrow"]
   "spider" ["string" "spider_eye"]
   "creeper" ["gunpowder"]
   "enderman" ["ender_pearl"]})

(def drops
  "raw-drops with the cooked meat of each raw meat added."
  (update-vals raw-drops (fn [items] (into items (keep foods/cooked-of) items))))

(def sources
  "The mob names that drop an item, by item name (raw drops only)."
  (reduce-kv (fn [m mob items] (reduce #(update %1 %2 (fnil conj []) mob) m items)) {} raw-drops))

(defn wool-color
  "The dye colour of a wool item name (\"white\" for white_wool), else nil."
  [item]
  (some->> item (re-matches #"(.+)_wool") second))

(defn mobs
  "The mob names a call hunts or culls: those that drop its :item, else its :mob."
  [c]
  (let [{:keys [mob item]} (:args c)]
    (if item (get sources item []) [mob])))

(defn drops-wanted
  "The item names collected after a kill: the call's :drops, else the drops of its mobs (nil: every item)."
  [c]
  (or (:drops (:args c)) (not-empty (vec (distinct (mapcat drops (mobs c)))))))

(a/defargs settings
  {::max-skips {:default 3 :doc "Animals skipped in a row before the hunt or cull gives up." :spec (a/int-in 0 nil)}
   ::collect-radius {:default 8 :doc "Blocks around the last kill in which its drops are collected." :spec (a/num-in 0 nil)}})

(defn note-target!
  "Remember the passive target as last seen (:animal, :entity id, :uuid, :cell, :by job and the rule that allowed the kill, from (rule-fn c entity)),
  for the kill event: the entity is gone once it is dead."
  [c target {:keys [radius job rule-fn]}]
  (when-let [e (->> (look/seen-objs (:primitives c) {:radius radius :names (mobs c) :max 64})
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
  "Call the attack child on the target (a whole fight) with the call's weapons and :checks (the child's own default :checks would
  otherwise hide the caller's); books its end. :again, :yield while the
  child waits on the world, or the ending after too many skips in a row (opts :give-up, a fn of c).
  opts: :radius (search radius of the child), :job and :rule-fn (see note-target!).
  Shared by combat.hunt and animals.cull."
  [c {:keys [radius give-up] :as opts}]
  (let [target (:target (ctx/mem c))
        _ (note-target! c target opts)
        tools (ctx/use c :tools)
        checks (:checks (ctx/frozen-call c))
        e (waits/waiting-out c (await (ctx/call c :attack 'jobs.combat.attack
                                                (cond-> {:targets [target] :radius radius :absent :done}
                                                  tools (assoc :use {:tools tools})
                                                  checks (assoc :checks checks)))))]
    (if (= :continue e)
      :yield
      (do (book-outcome! c target e)
          (if (>= (:skips (ctx/mem c) 0) (settings/get settings ::max-skips))
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
  (let [e (waits/waiting-out c (await (ctx/call c :collect 'jobs.items.collect-drops
                                                {:radius (settings/get settings ::collect-radius) :items (drops-wanted c)})))]
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

(defn corners
  "The eight corners of a box of cells: the far corners are the outer edges of the max cells."
  [{lo :min hi :max}]
  (for [x [(:x lo) (inc (:x hi))] y [(:y lo) (inc (:y hi))] z [(:z lo) (inc (:z hi))]]
    {:x x :y y :z z}))

(defn reach
  "How far from the body the entity query must look to cover the bound."
  [c]
  (let [{:keys [center radius]} (:args c)
        box (pen-read/arg-box c)
        here (u/self-pos c)]
    (cond
      box (apply max (map #(u/dist here %) (corners box)))
      center (+ radius (js/Math.hypot (- (:x center) (:x here)) (- (:z center) (:z here))))
      :else radius)))

(defn in-bound?
  "True when the position lies inside the bound (a box, else around the center, else around the body)."
  [c pos]
  (let [{:keys [center radius]} (:args c)
        box (pen-read/arg-box c)
        {:keys [x y z]} pos]
    (cond
      box (animals/in-box? box pos)
      center (<= (js/Math.hypot (- x (:x center)) (- z (:z center))) radius)
      :else (<= (u/dist (u/self-pos c) pos) radius))))

(defn herd
  "The entities of the call's mobs in the bound, never players or items."
  [c]
  (->> (look/seen-objs (:primitives c) {:radius (reach c) :names (mobs c) :max 64})
       (remove #(contains? #{"player" "item"} (.-kind %)))
       (filter #(in-bound? c (u/pos-of (.-pos %))))
       vec))

(defn census
  "{:adults [...] :babies [...]} of the herd in the bound."
  [c]
  (let [{babies true adults false} (group-by #(true? (.-baby %)) (herd c))]
    {:adults (vec adults) :babies (vec babies)}))
