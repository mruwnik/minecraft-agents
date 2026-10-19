(ns jobs.village.feed
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.cond :as cond]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]))

(def doc
  "Toss :count food to the villager with uuid :villager and confirm that villager took it. One call is the whole
  feeding: find the villager within :radius, walk to a cell 2 to 3 blocks from it (jobs.movement.go-to legs of 5 s, it
  wanders; nearer, the body's own pickup reach takes the drop back before the villager's does), look at its feet and
  toss with a pickup receipt (the toss primitive's :watchS reports who collected the drop, by uuid). Only what that
  villager collected counts as fed; the rest of the drop lying near the toss is collected back (a jobs.items.collect-drops child), never left as litter.
  :item is the food to give (carried), nil for the first carried of bread, carrot, potato, beetroot. Villagers pick
  up only bread, potato, carrot, wheat, beetroot and seeds: any other :item is impossible (:bad-args).
  When none is carried it is fetched (jobs.lib.need, :fetch true; condition-failed :no-food when :fetch is off or the fetch ended).
  Ends with the data {:fed n :item name}. :done when the villager took :count (done? holds), or the call's :least
  (default :all) when it gave up after that many. Otherwise with the cause:
  - condition-failed :no-food: none carried and none fetched (a listed call with :fetch off waits for food).
  - condition-failed :gone: the villager is not seen within :radius (also a non-villager under the uuid).
  - nothing-possible :unreachable: three blocked walks in a row, or eight legs without getting within reach.
  - nothing-possible :not-taken: three tosses in a row that the villager did not collect (full inventory, not
    wanted), or the toss status as a keyword when the toss is refused three times, or :litter when the drop
    cannot be collected back.
  A :villager that is no uuid, or an :item villagers do not pick up, is impossible (:bad-args). What was fed is booked after each toss, so a cut and restart does not feed twice; a
  toss cut before its receipt is settled on restart, back at its spot (what left the inventory, was not eaten and
  does not lie there counts as taken; none taken counts as refused) and its drop is collected back. Drops lying near
  before the toss are never collected.
  One call is the whole attempt; it yields :continue only while a walk or fetch child waits on the world.")

(a/defargs args
  {:villager {:doc "the villager's entity uuid" :spec string? :default nil}
   :item {:doc "food to give; nil: the first carried of bread, carrot, potato, beetroot" :spec a/item? :default nil}
   :count {:doc "how many items the villager is to take" :spec (a/int-in 1 64) :default 1}
   :radius {:doc "how far to look for the villager" :spec (a/int-in 1 96) :default 48}
   :fetch {:doc "get food when none is carried (jobs.lib.need): true, a set of kinds or a map of limits; false ends :no-food" :spec need/option? :default true}})

(def foods ["bread" "carrot" "potato" "beetroot"])
(def wanted (into (set foods) ["wheat" "wheat_seeds" "beetroot_seeds" "torchflower_seeds" "pitcher_pod"]))
(a/defargs settings
  {::min-gap {:default 2 :doc "Fewest blocks along x or z between body and villager at a toss: nearer, the drop at the villager's feet is in the body's own pickup reach." :spec (a/num-in 0 nil)}
   ::max-gap {:default 3 :doc "Most blocks along x and z between body and villager at a toss." :spec (a/num-in 0 nil)}
   ::leg-s {:default 5 :doc "Seconds one walk leg may take." :spec (a/int-in 0 nil)}
   ::max-refused {:default 3 :doc "Refusals before the job gives up on the animal." :spec (a/int-in 0 nil)}
   ::max-walks {:default 8 :doc "Failed walks of one call before it stops :unreachable." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 120 :doc "Seconds one feeding may take." :spec (a/int-in 1 nil)}
   ::watch-s {:default 4 :doc "Seconds the body watches the animal after acting." :spec (a/int-in 0 nil)}
   ::foot-height {:default 0.3 :doc "Height of an animal's feet above its position, in blocks." :spec (a/num-in 0 nil)}
   ::settle-reach {:default 4 :doc "Reach once the animal has settled, in blocks." :spec (a/int-in 0 nil)}})
(defn min-gap [] (settings/get settings ::min-gap))
(defn max-gap [] (settings/get settings ::max-gap))
(defn leg-s [] (settings/get settings ::leg-s))
(defn max-refused [] (settings/get settings ::max-refused))
(defn max-walks [] (settings/get settings ::max-walks))
(defn watch-s [] (settings/get settings ::watch-s))
(defn foot-height [] (settings/get settings ::foot-height))
(defn settle-reach [] (settings/get settings ::settle-reach))
(def job 'jobs.village.feed)

(defn fed [c] (:fed (ctx/mem c) 0))

(defn food
  "The item to toss: :item when carried, else the first carried of foods when :item is nil, else nil."
  [c]
  (let [have (into #{} (map :name) (u/inventory (:primitives c)))
        item (:item (:args c))]
    (if item (have item) (first (filter have foods)))))

(defn problem
  "The need wait for food while the villager still has to take some and none is carried, else nil."
  [c]
  (let [{:keys [item count]} (:args c)
        left (- count (fed c))]
    (when (and (pos? left) (nil? (food c)))
      (if item
        {:reason :need :item item :count left}
        {:reason :need :any-of foods :count left}))))

(def target [:villager])

(def meta {:category :phase :part-of '[jobs.village.breed]})

(def safe? ready/safe-if-lit)
(def fit? ready/fit?)


(def defaults {:least :all :spend {:job-time ::job-time-s}})

(defn bad-args
  "Never :bad-args when no villager uuid is given or :item is not food a villager picks up."
  [c]
  (let [{:keys [villager item]} (:args c)]
    (cond
      (not (string? villager)) (contract/never :bad-args "needs a villager uuid" :why "needs a villager uuid")
      (and item (not (contains? wanted item)))
      (let [why (str "villagers do not pick up " item)] (contract/never :bad-args why :why why))
      :else (contract/ok :args "a villager uuid and food they pick up"))))

(defn no-food
  "Later :need when none is carried and the call will not fetch it."
  [c]
  (let [w (problem c)]
    (if (and w (not (need/opts c job)))
      (apply contract/later :need "no food carried and no fetch for it" :why "no food carried and no fetch for it" (mapcat identity (dissoc w :reason)))
      (contract/ok :food "food carried or fetched, or none left to give"))))

(def can-start? (cond/and bad-args no-food))

(defn result "{:fed n :item name} so far." [c]
  {:fed (fed c) :item (or (:item (ctx/mem c)) (:item (:args c)))})

(defn done?
  "Ok once the villager took :count and no toss or clean-up of a drop is left; the result is the data of the ending."
  [c]
  (let [m (ctx/mem c)
        data [:units :items :done (fed c) :left (max 0 (- (:count (:args c)) (fed c))) :result (result c)]]
    (if (and (>= (fed c) (:count (:args c))) (not (:tossing m)) (not (:cleanup m)))
      (apply contract/ok :fed (str "villager took " (fed c) " " (:item (result c))) data)
      (apply contract/later :feeding (str "villager took " (fed c) " of " (:count (:args c))) data))))

(defn quote-of [c]
  {:uses {:job-time [0 (* 10 (:count (:args c) 1)) ##Inf]}})

(defn ending-of
  "The ending of a feeding that stopped for reason: :done when the call's :least was taken, condition-failed :no-food or :gone, else
  nothing-possible with reason as the cause."
  [c reason]
  (let [least (or (:least (ctx/frozen-call c)) :all)
        need (if (= :all least) (:count (:args c)) least)
        data (result c)]
    (cond
      (>= (fed c) need) (ctx/end c :done nil data)
      (= :no-food reason) (ctx/end c :condition-failed {:reason :no-food :need (problem c)} data)
      (= :gone reason) (ctx/end c :condition-failed :gone data)
      :else (ctx/end c :nothing-possible reason data))))

(defn stop!
  "End with the ending of reason (a keyword)."
  [c reason]
  (ending-of c reason))

(defn find-villager
  "The villager entity with uuid within radius, or nil."
  [p uuid radius]
  (->> (look/seen-objs p {:radius radius :names ["villager"]})
       (filter #(= uuid (.-uuid %)))
       first))

(defn ^:async fetch-food!
  "No food carried: fetch it when :fetch allows, else end no-food. :again once it arrived, :continue, or the stop."
  [c]
  (let [opts (need/opts c job)
        {:keys [item count any-of] :as w} (problem c)]
    (if-not (and opts w)
      (stop! c :no-food)
      (let [e (await (if item (need/item! c item count opts) (need/blocks! c any-of count opts)))]
        (cond
          (= :continue e) :continue
          (and (= :done (:kind e)) (not (problem c))) :again
          :else (stop! c :no-food))))))

(defn gap
  "Blocks between a and b along x or z, whichever is more: a drop is in a body's pickup reach while it is within about
  1.4 along both."
  [a b]
  (max (js/Math.abs (- (:x a) (:x b))) (js/Math.abs (- (:z a) (:z b)))))

(defn tossing-gap?
  "Within min-gap to max-gap, with a quarter block of slack: the stand cell's centre is exactly min-gap out for a
  villager standing on a block centre, and a body that arrives drifts a little in."
  [me v]
  (<= (- (min-gap) 0.25) (gap me v) (+ (max-gap) 0.25)))

(defn stand
  "The cell to toss from at the villager at v: on the body's side of it along the axis the body is farther along (x on a
  tie), its centre min-gap to max-gap out; each blocked walk in a row turns it a quarter round the villager."
  [me v blocked]
  (let [sign #(if (neg? %) -1 1)
        dx (- (:x me) (:x v)) dz (- (:z me) (:z v))
        sides (if (>= (js/Math.abs dx) (js/Math.abs dz))
                [[(sign dx) 0] [0 (sign dz)] [(- (sign dx)) 0] [0 (- (sign dz))]]
                [[0 (sign dz)] [(sign dx) 0] [0 (- (sign dz))] [(- (sign dx)) 0]])
        [sx sz] (nth sides (mod blocked 4))
        out (/ (+ (min-gap) (max-gap)) 2)]
    {:x (js/Math.floor (+ (:x v) (* sx out))) :y (js/Math.floor (:y v)) :z (js/Math.floor (+ (:z v) (* sz out)))}))

(defn ^:async walk!
  "One go-to leg to within range of pos. A leg or arrival resets the blocked count, anything else counts (the third in a
  row ends unreachable); the legs since the last toss are bounded by max-walks. :again, :continue or :done."
  [c pos range]
  (let [r (await (child/call-walk* c :walk {:pos pos :range range :spend {:job-time (leg-s) :tries 1} :doors :shut
                                            :ways #{:walk :clear-seal}}))]
    (if (= :continue r)
      :continue
      (let [res (:data r)
            ok? (or (:arrived res) (:leg res))
            _ (ctx/update-mem! c #(-> % (assoc :blocked (if ok? 0 (inc (:blocked % 0)))) (update :walks (fnil inc 0))))
            m (ctx/mem c)]
        (if (or (>= (:blocked m 0) 3) (>= (:walks m 0) (max-walks)))
          (stop! c :unreachable)
          :again)))))

(defn lying
  "The drops of item near at (the toss spot), nearest first, with only the body's share counted: skip maps the id of a
  drop lying there before the toss to its count then, so a thrown stack that merged into it counts by what it grew."
  [c item at skip]
  (keep (fn [d]
          (let [more (- (:count d) (get skip (:id d) 0))]
            (when (pos? more) (assoc d :count more))))
        (look/drops (:primitives c) item (:radius (:args c)) at)))

(defn held
  "How many of item the body carries."
  [c item]
  (transduce (comp (filter #(= item (:name %))) (map :count)) + 0 (u/inventory (:primitives c))))

(defn took
  "What the villager took of a toss cut before its receipt: the items gone from the inventory since the intent was
  booked (:before), less those eaten meanwhile and those still lying as the body's own drops."
  [{:keys [before]} now eaten own]
  (max 0 (- before now eaten (transduce (map :count) + 0 own))))

(defn eaten
  "How many bites of item the body ate since t (the :fed entries jobs.survival.eat writes to body memory)."
  [c item t]
  (count (filter #(= item (:item (:data %))) (ctx/since c :fed t))))

(defn ^:async clean-up!
  "Collect back the drop of the last toss that lies near its spot (a jobs.items.collect-drops child for those drops);
  each round that gathers nothing counts through u/count-fail! (the third ends as litter). :again, :continue while the
  child waits, or the ending."
  [c {:keys [item at skip]}]
  (let [drops (lying c item at skip)]
    (if (empty? drops)
      (do (ctx/update-mem! c dissoc :cleanup) :again)
      (let [r (await (child/run!* c :collect 'jobs.items.collect-drops {:ids (mapv :id drops) :near (select-keys at [:x :y :z]) :radius (:radius (:args c))}))]
        (cond
          (= :continue r) :continue
          (pos? (:collected (:data r) 0)) (do (u/progress! c) :again)
          (u/count-fail! c) (ending-of c :litter)
          :else :again)))))

(defn tossed!
  "Book the receipt of a toss of n: what the villager collected is fed; a short receipt leaves a clean-up of the
  drop, and a toss it took none of counts as refused."
  [c e item n r]
  (let [by (js->clj (.-takenBy r))
        got (min n (get by (.-uuid e) 0))
        {:keys [at skip]} (:tossing (ctx/mem c))]
    (ctx/update-mem! c #(-> %
                            (dissoc :tossing :walks)
                            (assoc :item item)
                            (update :fed (fnil + 0) got)
                            (assoc :refused (if (pos? got) 0 (inc (:refused % 0))))
                            (cond-> (< got n) (assoc :cleanup {:item item :at at :skip skip}))))
    (u/progress! c)
    :again))

(defn ^:async toss!
  "In reach: intent first (a cut before the receipt is cleaned up on restart), look at the villager's feet, toss with a
  receipt and book it. :again or the stop."
  [c e item]
  (let [p (:primitives c)
        n (min (- (:count (:args c)) (fed c)) (get (into {} (map (juxt :name :count)) (u/inventory p)) item 0))
        pos (u/pos-of (.-pos e))]
    (ctx/update-mem! c assoc :tossing {:item item :at (u/self-pos c) :before (held c item)
                                       :t (ctx/now c)
                                       :skip (into {} (map (juxt :id :count)) (lying c item (u/self-pos c) {}))})
    (await (ctx/act c :look (clj->js {:pos (update pos :y + (foot-height))})))
    (let [r (await (ctx/act c :toss (clj->js {:item item :count n :watchS (watch-s)})))
          status (.-status r)]
      (if (= "tossed" status)
        (tossed! c e item (.-count r) r)
        (do (ctx/update-mem! c dissoc :tossing)
            (if (u/count-fail! c)
              (ending-of c (keyword status))
              :again))))))

(defn settle!
  "A restart found a toss without its receipt: what left the inventory and was neither eaten nor lies there as the
  body's own drop was taken by the villager, so it counts as fed (none taken counts as refused); the rest is cleaned
  up."
  [c {:keys [item at skip t] :as intent}]
  (let [got (took intent (held c item) (eaten c item t) (lying c item at skip))]
    (ctx/update-mem! c #(-> %
                            (dissoc :tossing :walks)
                            (assoc :item item)
                            (update :fed (fnil + 0) got)
                            (assoc :refused (if (pos? got) 0 (inc (:refused % 0))))
                            (assoc :cleanup {:item item :at at :skip skip})))
    :again))

(defn ^:async step!
  "One piece: take back a drop, find the villager, fetch food, walk or toss. :again, :continue or an ending."
  [c]
  (let [m (ctx/mem c)
        {:keys [villager radius]} (:args c)
        item (food c)
        e (when-not (or (:tossing m) (:cleanup m)) (find-villager (:primitives c) villager radius))]
    (cond
      (:tossing m) (if (u/within? (u/self-pos c) (:at (:tossing m)) (settle-reach))
                     (settle! c (:tossing m))
                     (await (walk! c (:at (:tossing m)) (settle-reach))))
      (:cleanup m) (await (clean-up! c (:cleanup m)))
      (>= (:refused m 0) (max-refused)) (stop! c :not-taken)
      (and (nil? e) (not (look/surveyed? c))) (await (look/survey! c)) ; villagers count only while in view
      (nil? e) (stop! c :gone)
      (nil? item) (await (fetch-food! c))
      (tossing-gap? (u/self-pos c) (u/pos-of (.-pos e))) (await (toss! c e item))
      :else (await (walk! c (stand (u/self-pos c) (u/pos-of (.-pos e)) (:blocked m 0)) 0)))))

(defn ^:async round
  "The whole attempt: steps (a pace between) until fed or stopped; it yields only while a walk or fetch waits."
  [c]
  (await (pace/steps! c #(step! c))))
