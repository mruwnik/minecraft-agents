(ns jobs.village.feed
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]))

(def doc
  "Toss :count food to the villager with uuid :villager and confirm that villager took it. One call is the whole
  feeding: find the villager within :radius, walk within 2 of it (jobs.movement.go-to legs of 5 s, it wanders),
  look at its feet and toss with a pickup receipt (the toss primitive's :watchS reports who collected the drop, by
  uuid). Only what that villager collected counts as fed; the rest of the drop lying near the toss is collected back,
  never left as litter.
  :item is the food to give (carried), nil for the first carried of bread, carrot, potato, beetroot. Villagers pick
  up only bread, potato, carrot, wheat, beetroot and seeds: any other :item is refused at the check (:bad-args).
  When none is carried it is fetched (jobs.lib.fetch, :fetch true; a need wait when :fetch is off or failed).
  Result: {:fed n :item name}; when n is below :count also :status :stopped and :reason, with warn feed.gave-up:
  - \"gone\": the villager is not seen within :radius (also a non-villager under the uuid).
  - \"no-food\": none carried and none fetched.
  - \"unreachable\": three blocked walks in a row, or eight legs without getting within reach.
  - \"not-taken\": three tosses in a row that the villager did not collect (full inventory, not wanted), or the
    toss status when the toss is refused three times, or \"litter\" when the drop cannot be collected back.
  Success is info feed.done. What was fed is booked after each toss, so a cut and restart does not feed twice; a
  toss cut before its receipt is settled on restart, back at its spot (what left the inventory, was not eaten and
  does not lie there counts as taken; none taken counts as refused) and its drop is collected back. Drops lying near
  before the toss are never collected.
  One call is the whole attempt; it yields :continue only while a walk or fetch child waits on the world.")

(a/defargs args
  {:villager {:doc "the villager's entity uuid" :spec string? :default nil}
   :item {:doc "food to give; nil: the first carried of bread, carrot, potato, beetroot" :spec a/item? :default nil}
   :count {:doc "how many items the villager is to take" :spec (a/int-in 1 64) :default 1}
   :radius {:doc "how far to look for the villager" :spec (a/int-in 1 96) :default 48}
   :fetch {:doc "get food when none is carried (jobs.lib.fetch): true, a set of kinds or a map of limits; false ends :no-food" :spec fetch/option? :default true}})

(def foods ["bread" "carrot" "potato" "beetroot"])
(def wanted (into (set foods) ["wheat" "wheat_seeds" "beetroot_seeds" "torchflower_seeds" "pitcher_pod"]))
(def reach 2)
(def leg-s 5)
(def max-refused 3)
(def max-walks 8)
(def watch-s 4)
(def foot-height 0.3)
(def settle-reach 4)
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

(defn check
  "A villager uuid is given and :item, when given, is food a villager picks up; a missing food is fetched or waits."
  [c]
  (let [{:keys [villager item]} (:args c)]
    (cond
      (not (string? villager)) (ctx/wait c {:reason :bad-args :why "needs a villager uuid"})
      (and item (not (contains? wanted item))) (ctx/wait c {:reason :bad-args :why (str "villagers do not pick up " item)})
      :else (if-let [w (problem c)] (fetch/check c job w) true))))

(defn finish!
  "Hand the parent the result and return :done."
  [c extra]
  (ctx/result! c (merge {:fed (fed c) :item (or (:item (ctx/mem c)) (:item (:args c)))} extra))
  :done)

(defn stop!
  "Warn and end stopped with a reason."
  [c reason text]
  (ctx/emit! c :feed.gave-up :warn {:reason reason :text text :fed (fed c)})
  (finish! c {:status :stopped :reason reason}))

(defn done!
  "Info and end: the villager took :count."
  [c]
  (ctx/emit! c :feed.done :info {:fed (fed c) :text (str "villager took " (fed c) " " (:item (ctx/mem c)))})
  (finish! c {}))

(defn find-villager
  "The villager entity with uuid within radius, or nil."
  [p uuid radius]
  (->> (array-seq (.entities p #js {:radius radius :names #js ["villager"]}))
       (filter #(= uuid (.-uuid %)))
       first))

(defn ^:async fetch-food!
  "No food carried: fetch it when :fetch allows, else end no-food. :again once it arrived, :continue, or :done."
  [c]
  (if-not (and (fetch/opts c job) (problem c))
    (stop! c "no-food" "no food to give")
    (let [r (await (fetch/fetch-untimed! c job problem))]
      (cond
        r r
        (problem c) (stop! c "no-food" "no food to give")
        :else :again))))

(defn ^:async walk!
  "One go-to leg to the villager at pos. A leg or arrival resets the blocked count, anything else counts (the third in a
  row ends unreachable); the legs since the last toss are bounded by max-walks. :again, :continue or :done."
  [c pos]
  (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos pos :range reach :leg-s leg-s :doors :shut
                                                               :escalate false :warn false :retry false :zone-tolls true}))]
    (if (= :continue r)
      :continue
      (let [res (ctx/child-result c :walk)
            ok? (or (:arrived res) (:leg res))
            _ (ctx/update-mem! c #(-> % (assoc :blocked (if ok? 0 (inc (:blocked % 0)))) (update :walks (fnil inc 0))))
            m (ctx/mem c)]
        (if (or (>= (:blocked m 0) 3) (>= (:walks m 0) max-walks))
          (stop! c "unreachable" "cannot get within reach of the villager")
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
  "Collect back the drop of the last toss that lies near its spot; each collect that gathers nothing counts through
  u/fail! (the third ends as litter). :again, or :done when it ended."
  [c {:keys [item at skip]}]
  (let [drops (lying c item at skip)]
    (if (empty? drops)
      (do (ctx/update-mem! c dissoc :cleanup) :again)
      (let [r (await (ctx/act c :collect (clj->js {:id (:id (first drops))})))]
        (if (= "collected" (.-status r))
          (do (u/progress! c) :again)
          (if (= :done (u/fail! c :feed.gave-up "feed gave up: litter"))
            (finish! c {:status :stopped :reason "litter"})
            :again))))))

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
    (await (ctx/act c :look (clj->js {:pos (update pos :y + foot-height)})))
    (let [r (await (ctx/act c :toss (clj->js {:item item :count n :watchS watch-s})))
          status (.-status r)]
      (if (= "tossed" status)
        (tossed! c e item (.-count r) r)
        (do (ctx/update-mem! c dissoc :tossing)
            (if (= :done (u/fail! c :feed.gave-up (str "feed gave up: toss " status)))
              (finish! c {:status :stopped :reason status})
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
  "One piece: take back a drop, end, find the villager, fetch food, walk or toss. :again, :continue or :done."
  [c]
  (let [m (ctx/mem c)
        {:keys [villager count radius]} (:args c)
        item (food c)
        e (when-not (or (:tossing m) (:cleanup m) (>= (fed c) count)) (find-villager (:primitives c) villager radius))]
    (cond
      (:tossing m) (if (u/within? (u/self-pos c) (:at (:tossing m)) settle-reach)
                     (settle! c (:tossing m))
                     (await (walk! c (:at (:tossing m)))))
      (:cleanup m) (await (clean-up! c (:cleanup m)))
      (>= (fed c) count) (done! c)
      (>= (:refused m 0) max-refused) (stop! c "not-taken" "the villager does not take the food")
      (nil? e) (stop! c "gone" "the villager is not here")
      (nil? item) (await (fetch-food! c))
      (u/within? (u/self-pos c) (u/pos-of (.-pos e)) reach) (await (toss! c e item))
      :else (await (walk! c (u/pos-of (.-pos e)))))))

(defn ^:async round
  "The whole attempt: steps (a pace between) until fed or stopped; it yields only while a walk or fetch waits."
  [c]
  (await (pace/steps! c #(step! c))))
