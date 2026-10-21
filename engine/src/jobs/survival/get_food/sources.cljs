(ns jobs.survival.get-food.sources
  "Where get-food finds food it knows of: remembered farms and chests, ripe crops in sight."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.child :as child]
            [jobs.lib.chests :as chests]
            [jobs.lib.crops :as crops]
            [jobs.lib.stock :as stock]
            [jobs.lib.foods :as foods]
            [jobs.lib.look :as look]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]))

(a/defargs settings
  {::reach {:default 3 :doc "How near a food source the body stands to use it, in blocks." :spec (a/int-in 0 nil)}
   ::source-radius {:default 64 :doc "How far away a remembered food source still counts, in blocks." :spec (a/num-in 0 nil)}
   ::take {:default 16 :doc "Most items to withdraw from a chest in one go." :spec (a/int-in 1 nil)}
   ::stock-radius {:default 256 :doc "Blocks within which a chest the body booked food in (memory :fetch/stock, what it saw when it opened it) counts as a food source." :spec (a/int-in 0 nil)}})

(defn reach [] (settings/get settings ::reach))

(defn carried-count [c name] (storage/carried (u/inventory (:primitives c)) name))

(defn foreign-chest?
  "A chest source in another's zone or claim: survival jobs never take other people's stuff, in any mode and even
  starving, so it is skipped (one get-food.skipped warn per chest). :ignore-zones? lifts it."
  [c {:keys [pos kind]}]
  (let [v (when (= :chest kind) (access/container-refusal c :take pos))]
    (when v
      (let [fields (access/refusal-fields [v])]
        (ctx/warn-once! c [:skipped pos] :get-food.skipped
                        (assoc fields :pos pos :text (str "skipped the chest at " (pr-str pos) ": " (access/refusal-text fields))))))
    (boolean v)))

(defn stock-entries
  "The chests the body booked food in (memory :fetch/stock, latest per chest) within ::stock-radius, as :food-source-shaped
  entries {:t :data {:pos {:x :y :z} :kind :chest}}: its own stores count as sources though nothing wrote a :food-source."
  [c]
  (let [here (u/self-pos c)]
    (->> (ctx/entries c stock/stock-kind)
         (reduce (fn [m e] (assoc m (:pos (:data e)) e)) {})
         vals
         (filter (fn [{:keys [data]}] (and (some foods/edible? (keys (:items data))) (<= (u/dist here (zipmap [:x :y :z] (:pos data))) (settings/get settings ::stock-radius)))))
         (map (fn [{:keys [t data]}] {:t t :data {:pos (zipmap [:x :y :z] (:pos data)) :kind :chest}})))))

(defn usable-entries
  "The :food-source entries within ::source-radius and the chests booked with food within ::stock-radius, newest first,
  not tried in this job and not another's chest."
  [c]
  (let [tried (set (:tried-sources (ctx/mem c)))
        noted (filter (fn [{:keys [data]}] (<= (u/dist (u/self-pos c) (:pos data)) (settings/get settings ::source-radius))) (ctx/entries c :food-source))
        known (set (map (comp :pos :data) noted))]
    (->> (concat noted (remove #(known (:pos (:data %))) (stock-entries c)))
         (sort-by :t >)
         (filter (fn [{:keys [data]}]
                   (and (not (tried (:pos data)))
                        (not (foreign-chest? c data))))))))

(defn known-source
  "The newest usable :food-source entry's data (see usable-entries)."
  [c]
  (:data (first (usable-entries c))))

(defn fresh-source
  "known-source, when its entry is strictly newer than the latest :hungry
  entry: a source the last fruitless search never had in front of it."
  [c]
  (let [{source :data learned :t} (first (usable-entries c))
        gave-up (:t (ctx/latest c :hungry))]
    (when (and source learned gave-up (> learned gave-up))
      source)))

(defn box-middle [[lo hi]]
  (zipmap [:x :y :z] (map #(js/Math.floor (/ (+ %1 %2) 2)) lo hi)))

(defn own-boxes
  "The [min max] cells of the zones this body owns and of the plans it made (their footprints), within ::source-radius
  of their middle, not in this job's memory list tried-key, nearest first: its own pens and farms, which it knows without
  seeing them."
  [c tried-key]
  (let [self (ctx/self-name c)
        here (u/self-pos c)
        tried (set (tried-key (ctx/mem c)))
        zoned (->> (known/zones c)
                   (filter #(zones/same-owner? (:owner %) self))
                   (map (juxt :min :max)))
        planned (->> (known/footprints c)
                     (filter (comp (access/own-plans c) val))
                     (group-by val)
                     vals
                     (map (fn [cells] (let [cs (map key cells)]
                                        [(apply mapv min cs) (apply mapv max cs)]))))]
    (->> (concat zoned planned)
         distinct
         (remove (comp tried box-middle))
         (filter #(<= (u/dist here (box-middle %)) (settings/get settings ::source-radius)))
         (sort-by #(u/dist here (box-middle %))))))

(defn box-around
  "The [min max] cells of the own zone or plan footprint that holds pos, else nil."
  [c pos]
  (let [cell (mapv #(js/Math.floor %) [(:x pos) (:y pos) (:z pos)])]
    (first (filter (fn [[lo hi]] (every? true? (map (fn [v a b] (<= a v b)) cell lo hi))) (own-boxes c ::none)))))

(defn own-areas
  "The middles {:x :y :z} of own-boxes."
  [c]
  (map box-middle (own-boxes c :tried-sources)))

(defn pass-over!
  "Not try the source at pos again in this job."
  [c pos]
  (ctx/update-mem! c update :tried-sources (fnil conj []) pos))

(defn bury-source!
  "Give up on the source at pos: not tried again this job, and forgotten. :again, for the next way."
  [c pos]
  (pass-over! c pos)
  (ctx/forget-where! c :food-source #(= pos (:pos %)))
  :again)

(defn ^:async go-near!
  "Walk within reach of pos by one go-to call: :there, :far (it gave up) or :continue (go-to waits on the world)."
  [c pos]
  (let [r (await (child/run!* c :goto 'jobs.movement.go-to {:pos pos :range (reach)} {:max-calls 1}))]
    (cond
      (= :continue r) :continue
      (<= (u/dist (u/self-pos c) pos) (inc (reach))) :there
      :else :far)))

(defn ripe-blocks
  "The ripe blocks named in ripe-ages within radius of center, nearest to the
  body first, minus the ones that proved undiggable (a lazy seq)."
  [c ripe-ages center radius]
  (let [skipped (set (:skipped-blocks (ctx/mem c)))]
    (->> (crops/seen-crops (:primitives c) (keys ripe-ages) radius 64)
         (filter #(let [need (ripe-ages (:name %))] (or (zero? need) (some-> (:age %) (>= need)))))
         (map :pos)
         (filter #(<= (u/dist % center) radius))
         (remove skipped))))

(defn last-resort?
  "Whether the body may take food from another's area: truly hungry (below the hungry line) and nothing edible carried."
  [c]
  (let [self (.self (:primitives c))]
    (and (foods/hungry? (.-food self) (.-health self) (:args c))
         (not (foods/carries-food? self)))))

(defn ^:async dig-ripe!
  "Dig the nearest ripe block in a permitted place (blocks.dig walks there and collects the drops): :again, or nil
  when there is none. Another's area counts only when last-resort? and then only after every permitted block. A block
  not dug is skipped from then on."
  [c ripe-ages center radius last-resort?]
  (when (empty? (ripe-blocks c ripe-ages center radius))
    (await (look/survey! c :cell)))
  (let [{pos :option trespass :trespass} (access/choose c :dig (ripe-blocks c ripe-ages center radius) vector)]
    (when (and pos (or last-resort? (not trespass)))
      (access/trespass! c "get-food" trespass)
      ;; access/choose made the zone call (a missing zone list never blocks survival); the dig notes a trespass for restore-broken
      (let [e (await (child/run!* c :dig 'jobs.blocks.dig {:pos pos :ignore-zones? true}))]
        (cond
          (= :continue e) :continue
          (and (= :done (:kind e)) (:dug (:data e))) :again
          :else (do (ctx/update-mem! c update :skipped-blocks (fnil conj []) pos) :again))))))

(def farm-radius
  "Blocks around a known farm in which ripe crops are harvested."
  6)

(defn ^:async farm!
  [c {:keys [pos]}]
  (case (await (go-near! c pos))
    :continue :continue
    :far (bury-source! c pos)
    (or (await (dig-ripe! c crops/food-ripe-age pos farm-radius (last-resort? c)))
        (do (pass-over! c pos) nil))))

(defn loaves-wanted
  "How many loaves fill the body to full."
  [c]
  (int (js/Math.ceil (/ (- 20 (.-food (.self (:primitives c)))) 5))))

(defn best-in-chest [items]
  (->> items
       (filter #(foods/edible? (.-name %)))
       (sort-by #(- (foods/points (.-name %))))
       first))

(defn wheat-to-take
  "How much wheat to withdraw: the loaves wanted worth, no more than the chest
  holds or ::take allows, all in multiples of 3; 0 when under 3."
  [c wheat]
  (let [down3 #(* 3 (quot % 3))]
    (min (* 3 (loaves-wanted c)) (down3 wheat) (down3 (settings/get settings ::take)))))

(defn chest-wheat [items]
  (transduce (comp (filter #(= "wheat" (.-name %))) (map #(.-count %))) + 0 items))

(defn ^:async withdraw!
  "Withdraw n of item from the chest at pos (storage.withdraw): :again; a chest that gave nothing is forgotten."
  [c pos item n]
  (let [before (carried-count c item)
        st (await (child/run!* c :withdraw 'jobs.storage.withdraw
                              {:chest pos :items {item (+ before n)} :ignore-zones? (:ignore-zones? (:args c))}))]
    (cond
      (= :continue st) :continue
      (> (carried-count c item) before)
      (do (ctx/emit! c :food.took-from-chest :info {:pos pos :item item :count (- (carried-count c item) before)
                                                    :text (str "took " (- (carried-count c item) before) " " item " from the chest at " (pr-str pos))})
          :again)
      :else (bury-source! c pos))))

(defn ^:async chest!
  "Look in the chest (the inventory may have changed since it was learned) and withdraw its best food, or wheat."
  [c {:keys [pos]}]
  (let [seen (await (chests/look-in! c pos))
        status (:status seen)]
    (cond
      (= :continue seen) :continue
      (= "unreachable" status) (bury-source! c pos)
      (not (#{"ok" "missing"} status)) (do (pass-over! c pos) :again)
      :else
      (let [items (when (= "ok" status) (array-seq (:items seen)))
            best (best-in-chest items)
            wheat (wheat-to-take c (chest-wheat items))]
        (cond
          best (await (withdraw! c pos (.-name best) (min (.-count best) (settings/get settings ::take))))
          (pos? wheat) (await (withdraw! c pos "wheat" wheat))
          :else (bury-source! c pos))))))
