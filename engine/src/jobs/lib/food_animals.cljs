(ns jobs.lib.food-animals
  "The food animals get-food may hunt: which are in sight and why one is spared, and what a kill says."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [engine.game :as game]
            [engine.notes :as notes]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.animal-spare :as spare]
            [jobs.lib.foods :as foods]
            [jobs.lib.home-food :as home-food]
            [jobs.lib.look :as look]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.util :as u]
            [jobs.lib.walk-pick :as pick]))

(a/defargs settings
  {::note-ttl-s {:default 600 :doc "Seconds a note of a food animal seen lasts: it counts toward the pair a hunt keeps." :spec (a/num-in 0 nil)}
   ::meat-pen-adults {:default 2 :doc "Penned adults of an animal kind in sight that make a working breeding pen: the body's raw meat of that kind is then valued at its cooking gain only." :spec (a/int-in 1 nil)}
   ::meat-wild-premium {:default 3 :doc "Extra worth of raw meat from wild animals (times its cooking gain) when none of the kind is near and no pen breeds it: every hunt leaves fewer." :spec (a/num-in 0 nil)}
   ::meat-wild-relief {:default 4 :doc "Wild adults of a kind in sight that halve the wild premium." :spec (a/num-in 0.1 nil)}
   ::meat-radius {:default 32 :doc "Blocks within which the animals of a kind count toward the replacement cost of its meat." :spec (a/int-in 1 nil)}
   ::pen-look-range {:default 4 :doc "Blocks from a food animal the body walks to when the cells round it were out of sight and may be a fenced pen." :spec (a/num-in 0 nil)}})

(def food-animals #{"cow" "pig" "sheep" "chicken" "rabbit"})

(def meat "The raw meat each food animal drops." {"cow" "beef" "pig" "porkchop" "sheep" "mutton" "chicken" "chicken" "rabbit" "rabbit"})

(defn carry
  "The food points get-food fills the pack to: its :carry, else the body's reserve (home-food/carry-points)."
  [c]
  (or (:carry (:args c)) (home-food/carry-points c)))

(def noted-range "Blocks from an animal another body noted that the walk toward it ends; its place is then looked at." 12)

(defn noted-cost
  "Seconds the walk from `me` toward the noted animal n takes at the least (walk-pick/walk-s, within noted-range): a lower
  bound (its danger is go-to's to price)."
  [me n]
  (pick/walk-s me {:pos (zipmap [:x :y :z] (:pos n)) :range noted-range}))

(defn noted-price
  "Seconds per hunger point of the walk toward the :seen note n (noted-cost) over the points its kind's meat gives: a lower bound."
  [me n]
  (/ (noted-cost me n) (max 1 (or (foods/points (meat (:what n))) 1))))

(defn meat-opts [] {:pen-adults (settings/get settings ::meat-pen-adults)
                    :wild-premium (settings/get settings ::meat-wild-premium)
                    :wild-relief (settings/get settings ::meat-wild-relief)})

(defn animals-seen
  "{:wild n :penned n}: the adult animals of a kind in sight within ::meat-radius, outside and inside a zone or enclosure of the body's own."
  [c animal]
  (let [pens (atom [])
        adults (->> (look/seen-objs (:primitives c) {:radius (settings/get settings ::meat-radius) :kind "passive"
                                                        :names [animal] :max 32})
                    (remove #(true? (.-baby %))))
        penned (count (filter #(spare/penned? c pens %) adults))]
    {:wild (- (count adults) penned) :penned penned}))

(defn meat-benefit
  "Seconds the meat item is worth the body: what reaching food is worth (cost/food-worth), or the item-value of one kill's mean
  drops that the food reserve lacks, times cost/meat-factor."
  [c item]
  (let [p (:primitives c)
        pts (or (foods/points item) 1)
        drops (or (cost/kill-drops (cost/meat-animal item) item :version (game/version-of p)) cost/expected-drops)
        short (max 0 (- (carry c) (foods/stock-points (u/inventory p))))
        n (min drops (/ short pts))
        stock (* (cost/meat-factor (animals-seen c (cost/meat-animal item)) (meat-opts))
                 (:value (cost/item-value [{:name item :count n}])))]
    (max (cost/food-worth (.-food (.self p)) (.-health (.self p)) (cost/death-fn p (ctx/view c))) stock)))

(defn noted-benefit
  "Seconds the meat of the noted animal n is worth the body (meat-benefit), less the walk back (cost/walk-cost) from its place to where the body stands."
  [c n]
  (let [back (:cost (cost/walk-cost {:distance (u/dist (u/self-pos c) (zipmap [:x :y :z] (:pos n)))}))]
    (- (meat-benefit c (meat (:what n))) back)))

(defn search-benefit
  "Seconds a search for food is worth the body: what reaching food is worth (cost/food-worth), or cost/food-search-worth of the
  points the pack lacks of the :carry reserve (those the bar lacks past the carried food as hunger, the rest at what carrying
  the chance each is eaten before the next easy food, cost/reserve-worth over home-food/away-s of the :trip-s arg), less the walk back (cost/walk-cost) from the search's reach
  (cost/search-radius of that worth) where the hunt ends."
  [c]
  (let [p (:primitives c)
        food (.-food (.self p))
        stock (foods/stock-points (u/inventory p))
        lack (max 0 (- foods/full-food food))
        short (max 0 (- (carry c) stock))
        hunger (min short (max 0 (- lack stock)))
        held (max 0 (- stock lack))
        reserve (if (> short hunger) (cost/reserve-worth (home-food/away-s c (:trip-s (:args c))) held (+ held (- short hunger))) 0)
        worth (max (cost/food-worth food (.-health (.self p)) (cost/death-fn p (ctx/view c)))
                   (cost/food-search-worth hunger reserve))
        back (:cost (cost/walk-cost {:distance (cost/search-radius worth (cost/leg-spacing))}))]
    (max 0 (- worth back))))

(defn noted-here?
  "Whether the note n is not proven gone: its cell in view within the scan its kind's reach covers (reach: kind -> blocks)."
  [c n reach]
  (not (spare/gone? c n (reach (:what n)))))

(defn noted-targets
  "The food animals a :seen note (any body's) places anywhere whose meat may be worth the walk (noted-benefit over
  noted-cost), as the notes, cheapest first by noted-price, skipping the ids in `done`, babies, the animals tried, those proven gone (spare/gone? within the walk `reach` of its kind, kind -> blocks); not cut at a distance."
  [c done reach]
  (let [c (assoc-in c [:args :carry] (carry c))
        me (u/self-pos c)
        skipped (set (:skipped-animals (ctx/mem c)))]
    (->> (notes/notes c)
         (filter #(and (= :seen (:kind %)) (contains? food-animals (:what %)) (:id %) (:pos %) (not (:baby %))
                       (not (done (:id %))) (not (skipped (:id %))) (noted-here? c % reach)
                       (> (noted-benefit c %) (noted-cost me %))))
         (sort-by #(noted-price me %)))))

(defn noted-target "The first of noted-targets, nil when none." [c done reach] (first (noted-targets c done reach)))

(defn note-seen!
  "Write the adults and babies of food animals in sight as :seen notes, so a later look at one of a pair counts the other."
  [c {:keys [radius]}]
  (let [seen (look/seen-objs (:primitives c) {:radius radius :kind "passive" :names food-animals :max 16})]
    (when (seq seen)
      (notes/note! c (mapv (fn [e] (let [p (u/pos-of (.-pos e))]
                                     {:kind :seen :what (.-name e) :id (.-uuid e) :baby (true? (.-baby e))
                                      :pos (mapv #(js/Math.floor %) [(:x p) (:y p) (:z p)])
                                      :ttl-ms (* 1000 (settings/get settings ::note-ttl-s))}))
                           seen)))))

(defn replacement-cost
  "Seconds to find a breeding pair again after a kill of an adult of `kind`: 0 while the kill leaves `keep` adults in sight, else
  the walk to the nearest noted adult out of sight (noted-cost), which the floor guarantees exists."
  [c kind sight-n seen-uuids radius near]
  (if (>= (dec sight-n) (spare/keep-adults))
    0
    (let [me (u/self-pos c)]
      (or (some->> (spare/noted-adult-notes c kind seen-uuids radius near) (map #(noted-cost me %)) seq (apply min)) 0))))

(defn kept-by-floor
  "Whether the kill of an adult of `kind` is left alone to keep a breeding pair: :kept when it would leave fewer than ::keep-adults
  known adults (n counts those in sight and noted within the keep radius; starving lifts nothing), or when the meat is worth less than
  the walk to find a pair again (replacement-cost)."
  [c kind n sight-n seen-uuids radius near]
  (cond (<= n (spare/keep-adults)) :kept
        :else (let [price (replacement-cost c kind sight-n seen-uuids radius near)]
                (when (and (pos? price) (< (meat-benefit c (meat kind)) price)) :kept))))

(defn in-sight
  "The food animals the entity scan lists within radius, nearest first, as {:entity e :why r :adults n :rule k}: r is nil for one
  to hunt, else :tried (attacked this job), :baby, :refused (another's zone, claim or plan), or what jobs.lib.animal-spare/why
  says (:off-limits, :pen: a zone or plan of its own or a closed enclosure), or :kept (kept-by-floor: a kill that leaves fewer than ::keep-adults adults known within keep-radius, or priced above its meat); n counts the adults of its kind in sight and noted within keep-radius and k is the spare rule that allowed the hunt.
  reach (kind -> blocks), when given, drops the animals of a kind farther than it after the adults of each kind in sight are counted (a pair in view past its reach is still a pair); animals in a zone or plan of the body's own under own? are kept.
  own? (the :pen way): an animal in a zone or plan of its own is no pen, and its rule is :own-pen."
  [c {:keys [radius reach keep-radius own?]}]
  (let [skipped (set (:skipped-animals (ctx/mem c)))
        me (u/self-pos c)
        seen (look/seen-objs (:primitives c) {:radius radius :kind "passive" :names food-animals :max 16})
        adult-es (remove #(true? (.-baby %)) seen)
        seen-uuids (set (map #(.-uuid %) seen))
        near (or keep-radius radius)
        adults (into {} (map (fn [k] [k (spare/adults-of-kind c k adult-es seen-uuids radius near)])) (distinct (map #(.-name %) adult-es)))
        sight (frequencies (map #(.-name %) adult-es))
        pens (atom [])]
    (->> seen
         (filter (fn [e] (or (nil? reach) (and own? (access/in-own-area? c (u/pos-of (.-pos e))))
                             (<= (u/dist me (u/pos-of (.-pos e))) (reach (.-name e))))))
         (mapv (fn [e]
                 (let [n (adults (.-name e))
                       mine? (and own? (access/in-own-area? c (u/pos-of (.-pos e))))
                       penned? (and (not mine?) (not (skipped (.-id e))) (not (true? (.-baby e)))
                                    (spare/penned? c pens e))
                       refusal (when-not (or (skipped (.-id e)) (true? (.-baby e)))
                                 (access/trespass-refusal c :take (u/pos-of (.-pos e))))
                       why (cond (skipped (.-id e)) :tried
                                 (true? (.-baby e)) :baby
                                 refusal :refused
                                 :else (or (spare/why 0 (.-name e) n penned?) (when-not mine? (kept-by-floor c (.-name e) n (sight (.-name e)) seen-uuids radius near))))]
                   {:entity e :why why :adults n
                    :by (when refusal (access/refusal-text (access/refusal-fields [refusal])))
                    :rule (when-not why (if mine? :own-pen (spare/allowed-by (spare/keep-adults) n penned?)))}))))))

(defn nearest
  "The nearest adult food animal in-sight with opts finds to hunt, as an in-sight entry."
  [c opts]
  (->> (in-sight c opts) (remove :why) first))

(declare look-for-animals! pen-looked)

(defn ^:async resighted
  "The nearest entry to hunt after the pen read; when the read turned the body so that `target` left the view cone, the body looks
  round again from where it stands (out of sight is no verdict on it, and a kept target would skip the spare and pen rules).
  An animal that look finds gets its own pen read, once: `relooked?` stops a second look round."
  [c opts target relooked?]
  (let [uuid (.-uuid (:entity target))]
    (cond
      (some #(= uuid (.-uuid (:entity %))) (in-sight c opts)) (nearest c opts)
      relooked? (nearest c opts)
      :else (do (look/forget-survey! c)
                (when-let [found (await (look-for-animals! c opts))]
                  (if (= :own-pen (:rule found)) found (await (pen-looked c opts found true))))))))

(defn ^:async pen-looked
  "The nearest entry to hunt (or :continue while the walk is not done) after the body has looked at the cells of any
  enclosure round the target that a first read left unseen, walking within sight range of it when they lie beyond:
  a fenced pen is no wild surplus, and the spare check reads seen cells only. `relooked?`: the target came from a look round already."
  [c opts target relooked?]
  (let [{:keys [x y z] :as pos} (u/pos-of (.-pos (:entity target)))
        undecided? (fn ^:async undecided []
                     (= :unloaded (:reason (await (pen-read/read! c {:at {:x (js/Math.floor x) :y (js/Math.floor (+ y 0.01)) :z (js/Math.floor z)}
                                                                     :max-cells (settings/get spare/settings ::spare/pen-max-cells)}
                                                                  #(= :unloaded (:reason %)))))))]
    (if-not (await (undecided?))
      (await (resighted c opts target relooked?))
      (let [e (await (child/run!* c :pen-look 'jobs.movement.go-to {:pos pos :range (settings/get settings ::pen-look-range) :ways #{:walk}}))]
        (if (= :continue e)
          :continue
          (do (await (undecided?)) (await (resighted c opts target relooked?))))))))

(defn cell-of
  "The rounded cell [x y z] of a position map or JS object, or nil when the sighting gave none."
  [pos]
  (when pos
    (let [p (if (map? pos) pos (u/pos-of pos))]
      (mapv #(js/Math.round %) [(:x p) (:y p) (:z p)]))))

(defn spared-sightings
  "The animals in sight that were not hunted, as {:name :id :why :pos}."
  [c opts]
  (->> (in-sight c opts) (filter :why)
       (mapv (fn [{:keys [entity why by]}] (cond-> {:name (.-name entity) :id (.-id entity) :uuid (.-uuid entity) :why why :pos (cell-of (.-pos entity))}
                                              by (assoc :by by))))))

(defn note-spared!
  "Remember the animals spared now by id: entities are sensed only in the view cone, so a later look at the end of a survey no longer lists them."
  [c opts]
  (let [seen (spared-sightings c opts)]
    (when (seq seen)
      (ctx/update-mem! c update :spared-seen #(merge % (into {} (map (juxt :id identity)) seen))))))

(defn ^:async look-for-animals!
  "The nearest food animal after one look around from this cell for animals: another job's look (collect's) is no such look, since entities
  count only while in the view cone and its last heading points away. Each look notes what it saw."
  [c opts]
  (let [cell (cell-of (u/self-pos c))]
    (when (not= cell (:animals-looked (ctx/mem c)))
      (look/forget-survey! c))
    (let [found (await (look/survey-until! c :cell (fn [] (note-seen! c opts) (note-spared! c opts) (nearest c opts))
                                           (look/glance-effort) look/hazards-only))]
      (ctx/update-mem! c assoc :animals-looked cell)
      (or found (do (note-seen! c opts) (nearest c opts))))))

(defn ^:async nearest-or-look
  "The nearest food animal in sight (an in-sight entry), its surroundings looked at first when they may be a pen (see pen-looked); with none, the body looks around once from where it stands and tries again, remembering what it spared on the way."
  [c opts]
  (note-seen! c opts)
  (when-let [target (or (nearest c opts) (await (look-for-animals! c opts)))]
    (if (= :own-pen (:rule target)) target (await (pen-looked c opts target false)))))

(defn note-kill!
  "Say which animal the hunt killed and where, with the adults of its kind counted, the keep threshold, the food level
  and the rule that allowed it, and keep it for the result."
  [c {animal :entity :keys [adults rule]}]
  (let [p (u/pos-of (.-pos animal))
        cell (mapv #(js/Math.floor %) [(:x p) (:y p) (:z p)])
        food (.-food (.self (:primitives c)))
        why {:adults adults :keep (spare/keep-adults) :food food :rule rule}]
    (ctx/update-mem! c update :killed (fnil conj []) (merge {:name (.-name animal) :pos cell} why))
    (ctx/emit! c :food.killed :info (merge {:name (.-name animal) :pos cell
                                            :text (str "hunted a " (.-name animal) " at " (pr-str cell)
                                                       " (" adults " adults, keep " (:keep why) ", food " food ", " (name rule) ")")}
                                           why))))

(defn passed-over
  "The animals not hunted, as {:name :id :why :pos}: those in sight now and those spared earlier in a look."
  [c opts]
  (let [now (spared-sightings c opts)
        ids (set (map :id now))]
    (into now (remove #(ids (:id %))) (vals (:spared-seen (ctx/mem c))))))

(defn nearby
  "The mobs of any kind the body sees or heard within radius, as {:name :kind :distance :pos}: tells a hunt filter from
  an empty scan when no animal qualified."
  [c radius]
  (->> (look/seen-entities (:primitives c) {:radius radius :max 16})
       (remove #(#{"item" "player"} (:kind %)))
       (mapv (fn [e] {:name (:name e) :kind (:kind e) :distance (/ (js/Math.round (* 10 (:distance e))) 10)
                     :pos (cell-of (:pos e))}))))
