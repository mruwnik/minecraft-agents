(ns jobs.strip.smelt
  (:require [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.waits :as waits]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.pace :as pace]
            [jobs.lib.stock :as stock]
            [jobs.lib.util :as u]
            [jobs.lib.work-place :as work-place]
            [jobs.strip.site-plan :refer [chests furnaces loads ores]]
            [jobs.strip.steps :as steps :refer [reason-of run-all!]]))

(def doc
  "Smelt the raw ore (raw_iron raw_gold raw_copper) at a field base: take the raw ore the base chests are known to hold (body
  memory stock) into the bag (storage.withdraw, up to 64 a free furnace; a failed withdraw is not a stop), load the raw ore carried
  into the base furnaces (items.smelt, :ways #{:carried :chest}; one kind split over both furnaces, several one kind each), then
  wait while it cooks and collect the output into the bag (the same children again). The furnaces and their args are kept in body
  memory :strip/smelting {base {slot args}}, so a restart or a death collects what was loaded. The ingots are left in the bag
  for jobs.strip.haul. A smelt that gives up is listed in :smelt-failed and its ore stays raw; that is not a failed call.
  Needs the :base plan to be readable and to hold a furnace, else waits :bad-plan / :no-furnace (a called child ends
  condition-failed with it). Done at once when no ore is carried or known in the base chests and nothing cooks.
  Ends :done {:smelted {raw-item n} :smelt-failed [{:furnace :reason}]}.")

(a/defargs args
  {:base {:doc "id of the field-base plan whose furnaces and chests are used" :spec a/name? :default nil}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds a smelt may take: the loads and the cooking." :spec (a/num-in 0 nil)}})

(def kind :strip/smelting)
(def policy {:cap 1 :ttl :forever})

(defn base [c] (:base (:args c)))
(defn cooking-of [c] (get (:data (ctx/latest c kind)) (base c) {}))
(defn save-cooking! [c m]
  (ctx/remember! c kind (assoc (:data (ctx/latest c kind)) (base c) m) policy))

(defn bag [c] (into {} (map (juxt :name :count)) (u/inventory (:primitives c))))

(defn load-plan [c]
  (let [cooking (cooking-of c)]
    (loads (remove #(contains? cooking (first %)) (furnaces c (base c))) (bag c) (:ignore-zones? (:args c)))))

(defn withdraw-plan
  "[[slot job args] ..] taking the raw ore the base chests are known to hold (body memory :fetch/stock) into the bag, up to
  what the free furnaces take (64 each), the kinds in one call per chest."
  [c]
  (let [free (- (count (furnaces c (base c))) (count (cooking-of c)))
        stock (stock/stock-of (ctx/view c))]
    (when (pos? free)
      (loop [[chest & more] (chests c (base c)) want (select-keys (bag c) ores) out []]
        (if-not chest
          out
          (let [have (select-keys (get stock (stock/cell-of chest)) ores)
                want' (reduce (fn [m [k n]] (update m k #(min (* 64 free) (+ (or % 0) n)))) want have)]
            (recur more want' (cond-> out (seq have) (conj [(keyword (str "withdraw-" (count out))) 'jobs.storage.withdraw
                                                           (merge {:chest chest :items (select-keys want' (keys have))}
                                                                  (select-keys (:args c) [:ignore-zones?]))])))))))))

(defn ^:async smelt-run!
  "Each [slot args] smelt child once: {:cooking {slot args} :smelted {item n} :failed [{:furnace :reason}]}. One that waits on its
  cooking stays in :cooking; one that gave up is dropped."
  [c entries]
  (loop [[[slot args] & more] entries acc {:cooking {} :smelted {} :failed []}]
    (if-not slot
      acc
      (let [e (waits/waiting-out c (await (ctx/call c slot 'jobs.items.smelt args)))]
        (recur more (cond (= :continue e) (assoc-in acc [:cooking slot] args)
                          (not= :done (:kind e)) (update acc :failed conj {:furnace (:furnace args) :reason (reason-of e)})
                          :else (update acc :smelted #(merge-with + % {(:item args) (:smelted (:data e) 0)}))))))))

(defn book!
  "Keep what cooks (kept plus r's) and add r's output and failures to the call's totals."
  [c r kept]
  (save-cooking! c (merge kept (:cooking r)))
  (ctx/update-mem! c #(-> %
                          (update :smelted (fn [m] (merge-with + (or m {}) (:smelted r))))
                          (update :smelt-failed (fnil into []) (:failed r)))))

(defn ^:async step! [c]
  (let [cooking (cooking-of c)
        m (ctx/mem c)]
    (cond
      (seq cooking) (let [r (await (smelt-run! c cooking))]
                      (book! c r {})
                      (if (seq (:cooking r)) :continue :again))
      (and (not (:withdrawn m)) (seq (withdraw-plan c))) (let [f (await (run-all! c (withdraw-plan c)))]
                                                           (if (= :continue f)
                                                             :continue
                                                             (do (ctx/update-mem! c assoc :withdrawn true) :again)))
      (and (not (:loaded m)) (seq (load-plan c))) (let [r (await (smelt-run! c (load-plan c)))]
                                                    (book! c r {})
                                                    (ctx/update-mem! c assoc :loaded true)
                                                    :again)
      :else (or (ctx/done c) :continue))))

(defn ^:async round [c]
  (await (pace/steps! c #(step! c))))

(def target [:base])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? (price/staying :long (ready/safe-at-by-night #(work-place/plan-spot {:plan (base %)} %))))

(def fit? (ready/fit-for :long))

(defn can-start? [c]
  (cond (not (base c)) (contract/never :bad-args "needs :base")
        (empty? (furnaces c (base c))) (contract/later :no-furnace "the base plan holds no furnace (or is unreadable)")
        :else (contract/ok :args "base with a furnace")))

(defn done?
  "Nothing cooks, and no ore is left to load or to take out of the base chests (a load or take already tried counts as done)."
  [c]
  (let [m (ctx/mem c)
        left (+ (count (cooking-of c))
                (if (:loaded m) 0 (count (load-plan c)))
                (if (:withdrawn m) 0 (count (withdraw-plan c))))
        data [:units :loads :done (reduce + 0 (vals (:smelted m))) :left left :result {:smelted (:smelted m {}) :smelt-failed (:smelt-failed m [])}]]
    (if (zero? left)
      (apply contract/ok :smelted "smelt: nothing left to smelt" data)
      (apply contract/later :smelting "smelt: ore to load or cooking" data))))

(defn quote-of [_] {:uses {:job-time [1 600 (settings/get settings ::job-time-s)]}})

(def meta {:tags [:mine]})
