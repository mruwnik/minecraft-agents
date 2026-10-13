(ns engine.contract
  "The job contract (docs/goals-and-actions.md), pure parts. A frame's :contract state in its job memory is
  {:call {call keys, frozen at call start} :tally {:uses {:items {name n} :wear {tool uses} :health hp}
  :yields {:items {name n}}} :ran-ms n}."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [engine.args :as a]))

(def health-causes
  "engine.hurt causes charged to the act they happen in; a mob's hit is the mob's doing, not the act's."
  #{"fall" "cactus" "berry bush" "drowning"})

(def report-policy {:cap 256 :ttl :forever})

(def worn-parts
  "The equipment parts not also in the bag (mainHand is a hotbar slot)."
  [:head :torso :legs :feet :offHand])

(defn carried-stacks [{:keys [inventory equipment]}]
  (concat inventory (keep #(get equipment %) worn-parts)))

(defn counts [stacks] (reduce (fn [m {:keys [name count]}] (update m name (fnil + 0) count)) {} stacks))

(defn durabilities [stacks]
  (reduce (fn [m {:keys [name durability]}] (cond-> m durability (update name (fnil + 0) durability))) {} stacks))

(defn positive [m] (into {} (filter (comp pos? val)) m))

(defn minus
  "a - b key by key, a missing key counting 0."
  [a b]
  (into {} (map (fn [k] [k (- (get a k 0) (get b k 0))])) (distinct (concat (keys a) (keys b)))))

(defn prune
  "m without nil, zero and empty-map values, nested."
  [m]
  (into {} (keep (fn [[k v]]
                   (let [v (if (map? v) (prune v) v)]
                     (when-not (or (nil? v) (and (number? v) (zero? v)) (and (map? v) (empty? v))) [k v]))))
        m))

(defn act-delta
  "What one act used and yielded, from body snapshots {:inventory :equipment :hurt {cause total}} before and after it,
  and change, the act's own {item delta} where its result gives one. Uses and yields are not netted."
  [before after change]
  (if-not (and (:inventory before) (:inventory after))
    {}
    (let [stacks-before (carried-stacks before)
          stacks-after (carried-stacks after)
          count-before (counts stacks-before)
          count-after (counts stacks-after)
          items (or change (minus count-after count-before))
          kept? #(= (get count-after %) (get count-before %))
          wear (into {} (filter (comp kept? key))
                     (positive (minus (durabilities stacks-before) (durabilities stacks-after))))
          hurt (minus (:hurt after) (:hurt before))]
      (prune {:uses {:items (positive (update-vals items -))
                     :wear wear
                     :health (reduce + (vals (select-keys hurt health-causes)))}
              :yields {:items (positive items)}}))))

(defn add [a b] (merge-with (fn [x y] (if (map? x) (add x y) (+ x y))) a b))

(defn report
  "The report of a frame's :contract state: its tally with the time its rounds ran, in seconds, among the uses."
  [{:keys [tally ran-ms]}]
  (assoc-in tally [:uses :time] (/ (or ran-ms 0) 1000)))

;; ------------------------------------------------------------------ endings

(def kinds
  "The kinds of an ending other than :done, in the order that decides when two hold in one round."
  [:cut-off :limit-reached :condition-failed :nothing-possible])

(def all-kinds (conj (set kinds) :done))

(defn ending?
  "Whether x is an ending {:kind :cause :because :report :data}."
  [x]
  (and (map? x) (contains? all-kinds (:kind x))))

(defn ending
  "The ending of kind with cause, plus job-specific data when given."
  [kind cause data]
  (when-not (all-kinds kind)
    (throw (ex-info (str "an ending's kind is one of " (str/join ", " (sort all-kinds)) ", not " (pr-str kind))
                    {:kind kind})))
  (cond-> {:kind kind :cause cause} (seq data) (assoc :data data)))

(defn prevailing
  "The ending among endings whose kind comes first in kinds, :done last."
  [endings]
  (first (sort-by #(let [i (.indexOf kinds (:kind %))] (if (neg? i) (count kinds) i)) endings)))

(defn cause-key
  "The keyword an ending's cause reads as (an event's :reason): the cause itself, a map's :reason, :fact or :line,
  else the kind."
  [{:keys [kind cause]}]
  (cond
    (keyword? cause) cause
    (map? cause) (or (some #(when (keyword? (get cause %)) (get cause %)) [:reason :fact :line]) kind)
    :else kind))

(defn with-because
  "e with child, a child's ending, as its :because, unless e is :done or says its own."
  [e child]
  (cond-> e (and child (not= :done (:kind e)) (not (contains? e :because))) (assoc :because child)))

(def event-chain "How many endings of a :because chain an event carries." 4)

(defn for-event
  "e as an event carries it: no :report on the endings it was because of, the chain cut after event-chain endings."
  ([e] (for-event e event-chain))
  ([e n]
   (cond-> e
     (:because e) (update :because #(when (> n 1) (dissoc (for-event % (dec n)) :report)))
     (and (contains? e :because) (<= n 1)) (dissoc :because))))

(defn done?
  "What job def's done? export says now, {:done? :done :left :units}, or nil when it has none."
  [def c]
  (when-let [f (:done? def)] (f c)))

(defn progress "The report fields of a done? answer." [d] (select-keys d [:units :done :left :left-items]))

;; ------------------------------------------------------------------ the call

(defn amounts? [m] (and (map? m) (every? (fn [[k v]] (and (a/ok? a/item? k) (a/ok? (a/num-in 0 nil) v))) m)))

(defn lines-problem
  "Why m is no map of limit lines (numbers >= 0 for numbers, {name n} for amounts), or nil."
  [numbers amounts m]
  (cond
    (not (map? m)) "not a map"
    (seq (remove (into numbers amounts) (keys m))) "an unknown line"
    (some #(and (contains? m %) (not (a/ok? (a/num-in 0 nil) (get m %)))) numbers) "a line not a number >= 0"
    (some #(and (contains? m %) (not (amounts? (get m %)))) amounts) "a line not {name n}"))

(a/defargs call-keys
  "The keys of a call that every job takes beside its own args (docs/job-contract-plan.md 1.1)."
  {:use {:doc "what the job may use, {purpose [item ...]}, each list in order" :default nil
         :spec (a/map-of keyword? (a/coll-of (a/or-of a/item? map?)))}
   :ways {:doc "the ways allowed, a set of the job's way keywords; default the job's defaults, else all" :default nil
          :spec (a/set-of keyword?)}
   :spend {:doc "what the job may use up: {:time s :health hp :food pts :items {name n} :wear {tool uses}}"
           :default nil
           :spec (a/valid-by "a map {:time s :health hp :food pts :items {name n} :wear {tool uses}} of numbers >= 0"
                             (partial lines-problem #{:time :health :food} #{:items :wear}))}
   :floor {:doc "levels the job does not go below: {:health hp :food pts}" :default nil
           :spec (a/valid-by "a map {:health hp :food pts} of numbers >= 0" (partial lines-problem #{:health :food} #{}))}
   :deadline {:doc "the wall-clock ms by which the call must have ended; default the caller's" :default nil
              :spec (a/num-in 0 nil)}
   :least {:doc "the least work accepted, in the job's units, or :all; default 1" :default nil
           :spec (a/or-of a/pos-num? #{:all})}})

(defn call-problem
  "Why the call keys in args do not fit, as text naming the job sym, or nil. ways is the job's ways export."
  [sym ways args]
  (let [given (select-keys args (keys call-keys))
        {:keys [path want value]} (a/first-problem 'engine.contract call-keys given)
        unknown (remove (set ways) (:ways given))]
    (cond
      path (str sym " " (a/path-text path) " must be " want ", got " (pr-str value))
      (seq unknown) (str sym " has no way " (str/join ", " (sort unknown)) "; "
                         (if (seq ways) (str "its ways are " (str/join ", " ways)) "it has no ways")))))

(defn resolve-settings
  "defaults with each qualified keyword value read with (lookup k), a settings key."
  [defaults lookup]
  (walk/postwalk #(if (qualified-keyword? %) (lookup %) %) defaults))

(def merged-keys "Call keys a call merges over the defaults line by line." #{:spend :floor :use})

(defn freeze
  "The call keys of a call to a job with ways and defaults (settings resolved): given (the args) over the defaults,
  :spend, :floor and :use line by line; :ways all of the job's when neither says; :least 1; :deadline the caller's
  call's. :use from the defaults names a chooser, filled later, so only given lists count."
  [{:keys [ways defaults]} given caller]
  (let [defaults (cond-> (dissoc defaults :use) (seq ways) (update :ways #(set (or % ways))))
        call (reduce (fn [m k]
                       (let [d (get defaults k) g (get given k)]
                         (cond
                           (and (merged-keys k) (map? d) (map? g)) (assoc m k (merge d g))
                           (some? g) (assoc m k g)
                           (some? d) (assoc m k d)
                           :else m)))
                     {:least 1} (keys call-keys))]
    (cond-> call (and (nil? (:deadline call)) (:deadline caller)) (assoc :deadline (:deadline caller)))))
