(ns jobs.lib.give-villager
  "Tossing food to a villager with a pickup receipt: the villager half of jobs.items.give (its :to is the villager's uuid)."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.game :as game]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.lib.villager :as villager]))

(def foods ["bread" "carrot" "potato" "beetroot"])
(def wanted (into (set foods) ["wheat" "wheat_seeds" "beetroot_seeds" "torchflower_seeds" "pitcher_pod"]))
(a/defargs settings
  {::drop-radius {:default 48 :doc "Blocks within which the body collects its tossed food back." :spec (a/int-in 1 96)}
   ::min-gap {:default 2.5 :doc "Fewest blocks along x or z between body and villager at a toss: a level toss lands about 2.5 to 3 ahead; nearer, the drop is in the body's own pickup reach." :spec (a/num-in 0 nil)}
   ::max-gap {:default 3.5 :doc "Most blocks along x and z between body and villager at a toss: farther, the drop falls short of the villager's pickup reach." :spec (a/num-in 0 nil)}
   ::crowd-gap {:default 2 :doc "Blocks around the spot a toss lands (::toss-reach ahead of the body) that must hold no other villager: one nearer takes the drop, and it is not counted." :spec (a/num-in 0 nil)}
   ::toss-reach {:default 2.6 :doc "Blocks ahead of the body, along the toss, where a level toss comes to rest: it flies on past a villager nearer than this." :spec (a/num-in 0 nil)}
   ::line-gap {:default 1.5 :doc "Blocks a villager must keep from the line of the toss: nearer, it picks up the flying drop." :spec (a/num-in 0 nil)}
   ::crowd-s {:default 30 :doc "Seconds to wait for other villagers to move off before the call stops :crowded." :spec (a/int-in 0 nil)}
   ::leg-s {:default 5 :doc "Seconds one walk leg may take." :spec (a/int-in 0 nil)}
   ::max-refused {:default 3 :doc "Tosses in a row that are refused or not collected before the call stops :not-taken." :spec (a/int-in 0 nil)}
   ::max-walks {:default 8 :doc "Failed walks of one call before it stops :unreachable." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 120 :doc "Seconds one feeding may take." :spec (a/int-in 1 nil)}
   ::watch-s {:default 4 :doc "Seconds the toss primitive watches for who collects the drop (the pickup receipt)." :spec (a/int-in 0 nil)}
   ::settle-reach {:default 4 :doc "Blocks from the toss spot within which a restart settles a toss cut before its receipt; farther, it walks back there first." :spec (a/int-in 0 nil)}})
(defn drop-radius [] (settings/get settings ::drop-radius))
(defn min-gap [] (settings/get settings ::min-gap))
(defn max-gap [] (settings/get settings ::max-gap))
(defn crowd-gap [] (settings/get settings ::crowd-gap))
(defn toss-reach [] (settings/get settings ::toss-reach))
(defn line-gap [] (settings/get settings ::line-gap))
(defn crowd-s [] (settings/get settings ::crowd-s))
(defn leg-s [] (settings/get settings ::leg-s))
(defn max-refused [] (settings/get settings ::max-refused))
(defn max-walks [] (settings/get settings ::max-walks))
(defn watch-s [] (settings/get settings ::watch-s))
(defn settle-reach [] (settings/get settings ::settle-reach))
(def job 'jobs.items.give)

(defn fed [c] (:fed (ctx/mem c) 0))

(defn want-count "How many the villager is to take: the call's :count, else 1." [c] (or (:count (:args c)) 1))

(defn food
  "The item to toss: :item when carried, else the first carried of foods when :item is nil, else nil."
  [c]
  (let [have (into #{} (map :name) (u/inventory (:primitives c)))
        item (:item (:args c))]
    (if item (have item) (first (filter have foods)))))

(defn problem
  "The need wait for food while the villager still has to take some and none is carried, else nil."
  [c]
  (let [item (:item (:args c))
        left (- (want-count c) (fed c))]
    (when (and (pos? left) (nil? (food c)))
      (if item
        {:reason :need :item item :count left}
        {:reason :need :any-of foods :count left}))))

(defn bad-args
  "Never :bad-args when :item is not food a villager picks up."
  [c]
  (let [{:keys [item]} (:args c)]
    (if (and item (not (contains? wanted item)))
      (let [why (str "villagers do not pick up " item)] (contract/never :bad-args why :why why))
      (contract/ok :args "food a villager picks up"))))

(defn no-food
  "Later :need when none is carried and the call will not fetch it."
  [c]
  (let [w (problem c)]
    (if (and w (not (need/fetch-opts c)))
      (apply contract/later :need "no food carried and no fetch for it" :why "no food carried and no fetch for it" (mapcat identity (dissoc w :reason)))
      (contract/ok :food "food carried or fetched, or none left to give"))))

(defn result "{:given n :item name} so far." [c]
  {:given (fed c) :item (or (:item (ctx/mem c)) (:item (:args c)))})

(defn done?
  "Ok once the villager took :count and no toss or clean-up of a drop is left; the result is the data of the ending."
  [c]
  (let [m (ctx/mem c)
        data [:units :items :done (fed c) :left (max 0 (- (want-count c) (fed c))) :result (result c)]]
    (if (and (>= (fed c) (want-count c)) (not (:tossing m)) (not (:cleanup m)))
      (apply contract/ok :given (str "villager took " (fed c) " " (:item (result c))) data)
      (apply contract/later :feeding (str "villager took " (fed c) " of " (want-count c)) data))))

(defn quote-of [c]
  {:uses {:job-time [0 (* 10 (want-count c)) ##Inf]}})

(defn ending-of
  "The ending of a feeding that stopped for reason: :done when the call's :least was taken, condition-failed :no-food or :gone, else
  nothing-possible with reason as the cause."
  [c reason]
  (let [least (or (:least (ctx/frozen-call c)) :all)
        need (if (= :all least) (want-count c) least)
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

(defn ^:async fetch-food!
  "No food carried: fetch it when the ways allow, else end no-food. :again once it arrived, :continue, or the stop."
  [c]
  (let [opts (need/fetch-opts c)
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

(defn sides
  "The four sides of the villager at v as [sx sz] unit steps, the body's own side (along the axis it is farther along,
  x on a tie) first."
  [me v]
  (let [sign #(if (neg? %) -1 1)
        dx (- (:x me) (:x v)) dz (- (:z me) (:z v))]
    (if (>= (js/Math.abs dx) (js/Math.abs dz))
      [[(sign dx) 0] [0 (sign dz)] [(- (sign dx)) 0] [0 (- (sign dz))]]
      [[0 (sign dz)] [(sign dx) 0] [0 (- (sign dz))] [(- (sign dx)) 0]])))

(defn side-of
  "The side of the villager at v the body at me stands on."
  [me v]
  (first (sides me v)))

(defn stand
  "The cell to toss from at the villager at v on side [sx sz]: its centre min-gap to max-gap out."
  [v [sx sz]]
  (let [out (/ (+ (min-gap) (max-gap)) 2)]
    {:x (js/Math.floor (+ (:x v) (* sx out))) :y (js/Math.floor (:y v)) :z (js/Math.floor (+ (:z v) (* sz out)))}))

(defn seg-dist
  "Distance from point o to the segment a-b on x and z."
  [a b o]
  (let [abx (- (:x b) (:x a)) abz (- (:z b) (:z a))
        l2 (+ (* abx abx) (* abz abz))
        t (if (zero? l2) 0 (max 0 (min 1 (/ (+ (* (- (:x o) (:x a)) abx) (* (- (:z o) (:z a)) abz)) l2))))]
    (js/Math.hypot (- (:x o) (+ (:x a) (* t abx))) (- (:z o) (+ (:z a) (* t abz))))))

(defn clear-side?
  "Whether a toss thrown from `from` at the villager on side [sx sz] meets none of the other villagers: the drop comes to
  rest toss-reach ahead of the body (nobody within crowd-gap of that spot) and flies over a line that passes no other
  villager within pickup reach."
  [others [sx sz] from]
  (let [land {:x (- (:x from) (* sx (toss-reach))) :z (- (:z from) (* sz (toss-reach)))}]
    (every? #(and (> (gap land %) (crowd-gap)) (> (seg-dist from land %) (line-gap))) others)))

(defn others
  "Positions of the villagers seen other than e."
  [c e]
  (->> (look/seen-objs (:primitives c) {:radius (+ 8 (crowd-gap)) :names ["villager"]})
       (remove #(= (.-uuid e) (.-uuid %)))
       (map #(u/pos-of (.-pos %)))))

(defn from-stand [v side] (let [{:keys [x z]} (stand v side)] (assoc v :x (+ x 0.5) :z (+ z 0.5))))

(defn clear-stand
  "The first side of v, turned round by blocked, that is clear of the other villagers, or nil."
  [others v me blocked]
  (let [ss (sides me v)
        turned (take 4 (drop (mod blocked 4) (cycle ss)))]
    (first (filter #(clear-side? others % (from-stand v %)) turned))))

(defn ^:async wait-room!
  "Hold 0.5 s for the crowd round the villager to move off; the held time is booked, and the call stops :crowded after
  crowd-s. :again, :continue or the stop."
  [c]
  (if (>= (:crowded-ms (ctx/mem c) 0) (* 1000 (crowd-s)))
    (stop! c :crowded)
    (let [r (await (child/pause! c 0.5 :waiting-for-room))]
      (ctx/update-mem! c dissoc :walks) ; it got within reach
      (when (= :done (:kind r)) (ctx/update-mem! c update :crowded-ms (fnil + 0) 500))
      (if (= :continue r) :continue :again))))

(defn ^:async walk!
  "One go-to leg to within range of pos. A leg or arrival resets the blocked count, anything else counts (the third in a
  row ends unreachable); the legs since the last toss are bounded by max-walks. :again, :continue or :done."
  [c pos range]
  (let [r (await (child/call-chase* c :walk {:pos pos :range range :leg-s (leg-s)
                                             :walk {:doors :shut :ways #{:walk :clear-seal}}}))]
    (if (= :continue r)
      :continue
      (let [res (:data r)
            ok? (or (:arrived res) (:nearer res))
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
        (look/drops (:primitives c) item (drop-radius) at)))

(defn held [c item] (u/carried-count (:primitives c) item))

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
      (let [r (await (child/run!* c :collect 'jobs.items.collect-drops {:ids (mapv :id drops) :near (select-keys at [:x :y :z]) :radius (drop-radius)}))]
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
                            (dissoc :tossing :walks :crowded-ms)
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
        n (min (- (want-count c) (fed c)) (get (into {} (map (juxt :name :count)) (u/inventory p)) item 0))
        pos (u/pos-of (.-pos e))]
    (ctx/update-mem! c assoc :tossing {:item item :at (u/self-pos c) :before (held c item)
                                       :t (ctx/now c)
                                       :skip (into {} (map (juxt :id :count)) (lying c item (u/self-pos c) {}))})
    (await (ctx/act c :look (clj->js {:pos (assoc pos :y (+ (:y (u/self-pos c)) game/eye-height))})))
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
        item (food c)
        e (when-not (or (:tossing m) (:cleanup m)) (villager/find-by-uuid (:primitives c) (:to (:args c))))]
    (cond
      (:tossing m) (if (u/within? (u/self-pos c) (:at (:tossing m)) (settle-reach))
                     (settle! c (:tossing m))
                     (await (walk! c (:at (:tossing m)) (settle-reach))))
      (:cleanup m) (await (clean-up! c (:cleanup m)))
      (>= (:refused m 0) (max-refused)) (stop! c :not-taken)
      (and (nil? e) (not (look/surveyed? c))) (await (look/survey! c)) ; villagers count only while in view
      (nil? e) (stop! c :gone)
      (nil? item) (await (fetch-food! c))
      :else
      (let [me (u/self-pos c) v (u/pos-of (.-pos e)) os (others c e)
            side (clear-stand os v me (:blocked m 0))]
        (cond
          (nil? side) (await (wait-room! c))
          (and (tossing-gap? me v) (clear-side? os (side-of me v) me)) (await (toss! c e item))
          :else (await (walk! c (stand v side) 0)))))))

(defn ^:async run-rounds
  "The whole attempt: steps (a pace between) until fed or stopped; it yields only while a walk or fetch waits."
  [c]
  (await (pace/steps! c #(step! c))))
