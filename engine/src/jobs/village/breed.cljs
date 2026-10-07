(ns jobs.village.breed
  (:require [engine.ctx :as ctx]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]))

(def doc
  "Grow the villagers seen within :radius to :target (adults and babies count). One call is the whole attempt. Each
  attempt takes the two nearest adults not yet tried in this call, feeds each enough to be willing (jobs.village.feed
  child, fetched unless :fetch is off: 3 bread, else 12 of carrot, potato or beetroot), then stays near them for
  :wait-s seconds (jobs.movement.linger-near child) and counts again. A rise in the count is a birth.
  Needs adult villagers in sight and beds the villagers can claim (the body cannot see which are free): it only counts
  what it sees, so a villager out of sight is not counted. Zones are the walks' (go-to tolls), never claimed here.
  Result: {:population n :target t :births b}; short of the target also :status :stopped and :reason, warn
  breed.gave-up; success is info breed.done (an already met target is done with 0 births):
  - \"no-pair\": fewer than two adult villagers in sight that were not tried yet, and none tried.
  - \"no-births\": three attempts in a row without a birth, or no untried pair left after an attempt.
  - \"no-food\": a feed child ended without food.
  A pair a feed could not complete (villager gone, unreachable, would not take food) counts as an attempt without a
  birth. What was tried, the birth baseline and the wait are kept in job memory, so a cut and restart re-checks the
  count and goes on. It yields :continue while a feed, walk or the wait is under way.")

(def args
  {:target {:doc "villagers wanted in sight (adults and babies)" :type :int :min 2 :max 64 :default nil}
   :radius {:doc "how far to look for villagers" :type :int :min 1 :max 96 :default 48}
   :wait-s {:doc "seconds to stay near a fed pair for a birth" :type :int :min 1 :max 600 :default 45}
   :fetch {:doc "get food when none is carried (jobs.lib.fetch): true, a set of kinds or a map of limits; false waits for food" :default true}})

(def max-fruitless 3)
(def wait-range 6)
(def job 'jobs.village.breed)

(defn check
  "A target of at least 2 is given."
  [c]
  (let [t (:target (:args c))]
    (or (and (number? t) (>= t 2))
        (ctx/wait c {:reason :bad-args :why "needs a :target of at least 2"}))))

(defn villagers
  "The villagers in sight within radius."
  [p radius]
  (array-seq (.entities p #js {:radius radius :names #js ["villager"]})))

(defn adult? [e] (not (.-baby e)))

(defn held
  "How many of item the body carries."
  [c item]
  (transduce (comp (filter #(= item (:name %))) (map :count)) + 0 (u/inventory (:primitives c))))

(defn meal
  "[item count] to feed one villager: carried bread (3), else a carried 12 of carrot, potato or beetroot, else bread
  (fetched)."
  [c]
  (or (first (filter (fn [[item n]] (>= (held c item) n)) [["bread" 3] ["carrot" 12] ["potato" 12] ["beetroot" 12]]))
      ["bread" 3]))

(defn births
  "The births so far: the booked ones plus the rise in the count since the pair now being fed or waited for was chosen."
  [c pop]
  (let [m (ctx/mem c)]
    (+ (:births m 0) (if (:pair m) (max 0 (- pop (:baseline m pop))) 0))))

(defn finish!
  "Hand the parent the counts and return :done."
  [c pop extra]
  (ctx/result! c (merge {:population pop :target (:target (:args c)) :births (births c pop)} extra))
  :done)

(defn stop!
  "Warn and end stopped with a reason."
  [c pop reason text]
  (ctx/emit! c :breed.gave-up :warn {:reason reason :text text :population pop :births (births c pop)})
  (finish! c pop {:status :stopped :reason reason}))

(defn done!
  "Info and end: the target is met."
  [c pop]
  (ctx/emit! c :breed.done :info {:population pop :births (births c pop) :text (str pop " villagers in sight")})
  (finish! c pop {}))

(defn choose!
  "Pick the two nearest untried adults and book them with the baseline count, or end without a pair. :again or :done."
  [c vs pop]
  (let [m (ctx/mem c)
        me (u/self-pos c)
        tried (set (:tried m))
        pair (->> vs (filter adult?) (remove #(tried (.-uuid %)))
                  (sort-by #(u/dist me (u/pos-of (.-pos %)))) (take 2))]
    (cond
      (< (count pair) 2) (if (seq tried)
                           (stop! c pop "no-births" "no untried pair left")
                           (stop! c pop "no-pair" "fewer than two adult villagers in sight"))
      :else (do (ctx/update-mem! c #(assoc % :pair (mapv (fn [e] (.-uuid e)) pair) :fed [] :baseline pop
                                           :tried (into (vec (:tried %)) (map (fn [e] (.-uuid e))) pair)))
                :again))))

(defn failed-pair!
  "The pair could not be fed: an attempt without a birth."
  [c]
  (ctx/update-mem! c #(-> % (dissoc :pair :fed) (update :fruitless (fnil inc 0))))
  :again)

(defn ^:async feed-pair!
  "Feed the next villager of the pair; both fed starts the wait near them. :again, :continue or :done."
  [c vs pop]
  (let [m (ctx/mem c)
        next (first (remove (set (:fed m)) (:pair m)))
        {:keys [radius fetch]} (:args c)]
    (if-not next
      (let [at (some #(when (= (first (:pair m)) (.-uuid %)) (u/pos-of (.-pos %))) vs)]
        (ctx/update-mem! c assoc :waiting true :at (or at (u/self-pos c)))
        :again)
      (let [[item n] (meal c)
            r (await (ctx/call-child c (keyword (str "feed-" next)) 'jobs.village.feed
                                     {:villager next :item item :count n :radius radius :fetch fetch}))]
        (if (not= :done r)
          :continue
          (let [res (ctx/child-result c (keyword (str "feed-" next)))]
            (cond
              (= "no-food" (:reason res)) (stop! c pop "no-food" "no food to feed the villagers")
              (:status res) (failed-pair! c)
              :else (do (ctx/update-mem! c update :fed conj next) :again))))))))

(defn ^:async wait!
  "Stay near the fed pair; when the wait ends count the births. :again or :continue."
  [c pop]
  (let [m (ctx/mem c)
        r (await (ctx/call-child c :wait 'jobs.movement.linger-near {:pos (:at m) :range wait-range :wait-s (:wait-s (:args c))}))]
    (if (not= :done r)
      :continue
      (let [born (max 0 (- pop (:baseline m pop)))]
        (ctx/update-mem! c #(-> % (dissoc :waiting :pair :fed :at)
                                (update :births (fnil + 0) born)
                                (assoc :fruitless (if (pos? born) 0 (inc (:fruitless % 0))))))
        :again))))

(defn ^:async step!
  "Count what is in sight, then: end at the target, wait near a fed pair, give up after three fruitless attempts, feed the
  pair, or choose one. :again, :continue or :done."
  [c]
  (let [m (ctx/mem c)
        vs (villagers (:primitives c) (:radius (:args c)))
        pop (count vs)]
    (cond
      (>= pop (:target (:args c))) (done! c pop)
      (:waiting m) (await (wait! c pop))
      (>= (:fruitless m 0) max-fruitless) (stop! c pop "no-births" "three attempts without a birth")
      (:pair m) (await (feed-pair! c vs pop))
      :else (choose! c vs pop))))

(defn ^:async round
  "The whole attempt: steps (a pace between) until the target is met or it gives up; yields only while a child waits."
  [c]
  (await (pace/steps! c #(step! c))))
