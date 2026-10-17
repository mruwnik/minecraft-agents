(ns jobs.village.breed
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a] [jobs.lib.need :as need]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.cond :as cond]
            [jobs.lib.util :as u]))

(def doc
  "Grow the villagers seen within :radius to :target (adults and babies count). One call is the whole attempt. Each
  attempt takes the two nearest adults not yet tried in this call, feeds each enough to be willing (jobs.village.feed
  child, fetched unless :fetch is off: 3 bread, else 12 of carrot, potato or beetroot), then stays near them for
  :wait-s seconds (jobs.movement.linger-near child) and counts again. A rise in the baby villagers in sight is a birth
  (an adult walking into view is not).
  Needs adult villagers in sight and beds the villagers can claim (the body cannot see which are free): it only counts
  what it sees, so a villager out of sight is not counted. Zones are the walks' (go-to tolls), never claimed here.
  Conditions at the start (a listed call waits on them): :no-pair (below :target, fewer than two adult villagers in
  sight; the engine looks round once first), :no-food (the food is not carried and a fetch is off or failed lately). A :target under 2 is impossible
  (:bad-args). Quote: time from :wait-s and the births still missing.
  Ends with the data {:population n :target t :births b}: done when the target is met (done? holds; an already
  met target is done with 0 births), else with the cause:
  - condition-failed :no-food: a feed child ended without food.
  - nothing-possible :no-births: max-fruitless (3) attempts in a row without a birth, or no untried pair left after an attempt.
  A pair a feed could not complete (villager gone, unreachable, would not take food) counts as an attempt without a
  birth. What was tried, the birth baseline and the wait are kept in job memory, so a cut and restart re-checks the
  count and goes on. It yields :continue while a feed, walk or the wait is under way.")

(def default-wait-s 45)

(a/defargs args
  {:target {:doc "villagers wanted in sight (adults and babies)" :spec (a/int-in 2 64) :default nil}
   :radius {:doc "how far to look for villagers" :spec (a/int-in 1 96) :default 48}
   :wait-s {:doc "seconds to stay near a fed pair for a birth" :spec (a/int-in 1 600) :default default-wait-s}
   :fetch {:doc "get food when none is carried (jobs.lib.need): true, a set of kinds or a map of limits; false ends :no-food" :spec need/option? :default true}})

(a/defargs settings
  {::max-fruitless {:default 3 :doc "Rounds without progress before the job gives up." :spec (a/int-in 0 nil)}
   ::wait-range {:default 6 :doc "Blocks within which the body waits for a pair to breed." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 3600 :doc "Seconds one breeding may take." :spec (a/int-in 1 nil)}
   ::feed-s {:default 30 :doc "Seconds one pair's feeding takes, for the quote." :spec (a/num-in 0 nil)}})
(defn max-fruitless [] (settings/get settings ::max-fruitless))
(defn wait-range [] (settings/get settings ::wait-range))
(defn feed-s [] (settings/get settings ::feed-s))
(def job 'jobs.village.breed)

(defn villagers
  "The villagers in sight within radius."
  [p radius]
  (look/seen-objs p {:radius radius :names ["villager"]}))

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

(defn sight
  "{:pop n :kids k}: the villagers in sight, and how many of them are babies."
  [vs]
  {:pop (count vs) :kids (count (remove adult? vs))})

(def target [:target])

(def safe? ready/safe-if-lit)
(def fit? ready/fit?)


(def defaults {:spend {:job-time ::job-time-s}})

(defn bad-args
  "Never :bad-args: no :target of at least 2."
  [c]
  (let [t (:target (:args c))]
    (if (and (number? t) (>= t 2))
      (contract/ok :args "a :target of at least 2")
      (contract/never :bad-args "needs a :target of at least 2" :why "needs a :target of at least 2"))))

(defn no-pair
  "Look :no-pair (later once a look round found nothing) while the target is not met and fewer than two adult villagers are in sight."
  [c]
  (let [vs (villagers (:primitives c) (:radius (:args c)))]
    (cond
      (not (and (< (:pop (sight vs)) (:target (:args c))) (< (count (filter adult? vs)) 2))) (contract/ok :pair "two adult villagers in sight, or the target met")
      (look/surveyed? c :near) (contract/later :no-pair "fewer than two adult villagers in sight")
      :else (contract/look :no-pair "fewer than two adult villagers in sight"))))

(defn no-food
  "Later :no-food while the meal is not carried and :fetch is off."
  [c]
  (let [[item n] (meal c)
        need {:reason :need :item item :count (- n (held c item))}]
    (if (and (pos? (:count need)) (not (need/opts c job)))
      (contract/later :no-food "no food carried and no fetch for it" :need need)
      (contract/ok :food "the meal is carried or fetched"))))

(defn pair-lost
  "Later :no-pair mid-run while no pair was ever chosen (or is being fed or waited for) and fewer than two adults are in
  sight: they may come back; once a pair was tried the round ends :no-births instead."
  [c]
  (let [m (ctx/mem c)]
    (if (or (:pair m) (:waiting m) (seq (:tried m)))
      (contract/ok :pair "a pair was chosen")
      (no-pair c))))

(def can-run? pair-lost)
(def can-start? (cond/and bad-args no-pair no-food))

(declare data-of)

(defn done?
  "Ok once the villagers in sight reach :target and no pair is being fed or waited for (step! books that pair's
  births first), with the population, target and births as its result."
  [c]
  (let [target (:target (:args c))
        seen (sight (villagers (:primitives c) (:radius (:args c))))
        m (ctx/mem c)
        data [:units :villagers :done (:pop seen) :left (max 0 (- (or target 0) (:pop seen))) :result (data-of c seen)]]
    (if (and (number? target) (>= (:pop seen) target) (not (:pair m)) (not (:waiting m)))
      (apply contract/ok :target (str (:pop seen) " villagers in sight") data)
      (apply contract/later :growing (str (:pop seen) " of " target " villagers in sight") data))))

(defn quote-of [c]
  (let [left (max 0 (- (:target (:args c)) (:pop (sight (villagers (:primitives c) (:radius (:args c)))))))
        w (:wait-s (:args c) default-wait-s)]
    {:uses {:job-time [(if (pos? left) w 0) (* left (+ w (feed-s))) ##Inf]}}))

(defn born
  "The babies that came into sight since the pair now being fed or waited for was chosen."
  [c seen]
  (if (:pair (ctx/mem c)) (max 0 (- (:kids seen) (:baseline (ctx/mem c) (:kids seen)))) 0))

(defn births
  "The births so far: the booked ones plus those since the pair was chosen."
  [c seen]
  (+ (:births (ctx/mem c) 0) (born c seen)))

(defn data-of
  "The call's data: the villagers in sight, the target and the births so far."
  [c seen]
  {:population (:pop seen) :target (:target (:args c)) :births (births c seen)})

(defn stop!
  "End with kind and the cause reason."
  [c seen kind reason]
  (ctx/end c kind reason (data-of c seen)))

(defn choose!
  "Pick the two nearest untried adults and book them with the baseline count; none left after an attempt ends :no-births, none at all yields (can-run? answers). :again, :continue or an ending."
  [c vs seen]
  (let [m (ctx/mem c)
        me (u/self-pos c)
        tried (set (:tried m))
        pair (->> vs (filter adult?) (remove #(tried (.-uuid %)))
                  (sort-by #(u/dist me (u/pos-of (.-pos %)))) (take 2))]
    (cond
      (< (count pair) 2) (if (seq tried) (stop! c seen :nothing-possible :no-births) :continue)
      :else (do (ctx/update-mem! c #(assoc % :pair (mapv (fn [e] (.-uuid e)) pair) :fed [] :baseline (:kids seen)
                                           :tried (into (vec (:tried %)) (map (fn [e] (.-uuid e))) pair)))
                :again))))

(defn failed-pair!
  "The pair could not be fed: an attempt without a birth."
  [c]
  (ctx/update-mem! c #(-> % (dissoc :pair :fed) (update :fruitless (fnil inc 0))))
  :again)

(defn ^:async feed-pair!
  "Feed the next villager of the pair; both fed starts the wait near them. :again, :continue or an ending."
  [c vs seen]
  (let [m (ctx/mem c)
        next (first (remove (set (:fed m)) (:pair m)))
        {:keys [radius fetch]} (:args c)]
    (if-not next
      (let [at (some #(when (= (first (:pair m)) (.-uuid %)) (u/pos-of (.-pos %))) vs)]
        (ctx/update-mem! c assoc :waiting true :at (or at (u/self-pos c)))
        :again)
      (let [[item n] (meal c)
            r (await (ctx/call c (keyword (str "feed-" next)) 'jobs.village.feed
                               {:villager next :item item :count n :radius radius :fetch fetch}))]
        (cond
          (= :continue r) :continue
          (= :no-food (contract/cause-key r)) (stop! c seen :condition-failed :no-food)
          (not= :done (:kind r)) (failed-pair! c)
          :else (do (ctx/update-mem! c update :fed conj next) :again))))))

(defn ^:async wait!
  "Stay near the fed pair; when the wait ends (however it ends) count the births. :again or :continue."
  [c seen]
  (let [m (ctx/mem c)
        r (await (ctx/call c :wait 'jobs.movement.linger-near {:pos (:at m) :range (wait-range) :wait-s (:wait-s (:args c))}))]
    (if (= :continue r)
      :continue
      (let [born (born c seen)]
        (ctx/update-mem! c #(-> % (dissoc :waiting :pair :fed :at)
                                (update :births (fnil + 0) born)
                                (assoc :fruitless (if (pos? born) 0 (inc (:fruitless % 0))))))
        :again))))

(defn ^:async step!
  "Count what is in sight, then: end at the target, wait near a fed pair, give up after three fruitless attempts, feed the
  pair, or choose one. :again, :continue or an ending."
  [c]
  (let [m (ctx/mem c)
        vs (villagers (:primitives c) (:radius (:args c)))
        seen (sight vs)]
    (cond
      (>= (:pop seen) (:target (:args c)))
      (do (ctx/update-mem! c #(-> % (dissoc :waiting :pair :fed :at) (update :births (fnil + 0) (born c seen)))) :again)
      (:waiting m) (await (wait! c seen))
      (>= (:fruitless m 0) (max-fruitless)) (stop! c seen :nothing-possible :no-births)
      (:pair m) (await (feed-pair! c vs seen))
      :else (choose! c vs seen))))

(defn ^:async round
  "The whole attempt: steps (a pace between) until the target is met or it gives up; yields only while a child waits."
  [c]
  (await (pace/steps! c #(step! c))))
