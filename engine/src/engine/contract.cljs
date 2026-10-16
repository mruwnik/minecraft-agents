(ns engine.contract
  "The job contract (docs/goals-and-actions.md), pure parts. A frame's :contract state in its job memory is
  {:call {call keys, frozen at call start} :tally {:uses {:items {name n} :wear {tool uses} :health hp}
  :yields {:items {name n}}} :ran-ms n :started-ms wall-clock ms its call froze}."
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
  "What one act used and yielded, from body snapshots {:inventory :equipment :hurt {cause total}} before and after it.
  Uses and yields are not netted."
  [before after]
  (if-not (and (:inventory before) (:inventory after))
    {}
    (let [stacks-before (carried-stacks before)
          stacks-after (carried-stacks after)
          count-before (counts stacks-before)
          count-after (counts stacks-after)
          items (minus count-after count-before)
          kept? #(= (get count-after %) (get count-before %))
          wear (into {} (filter (comp kept? key))
                     (positive (minus (durabilities stacks-before) (durabilities stacks-after))))
          hurt (minus (:hurt after) (:hurt before))]
      (prune {:uses {:items (positive (update-vals items -))
                     :wear wear
                     :health (reduce + (vals (select-keys hurt health-causes)))}
              :yields {:items (positive items)}}))))

(defn add [a b] (merge-with (fn [x y] (if (map? x) (add x y) (+ x y))) a b))

(defn used
  "What a frame's :contract state has used of each spend line at wall-clock now: :job-time the s its rounds ran (plus
  extra-ms, the round in flight's), :wall-time the s since its call started, yields and waits included."
  [{:keys [tally ran-ms started-ms]} now extra-ms]
  (cond-> (assoc (:uses tally) :job-time (/ (+ (or ran-ms 0) extra-ms) 1000))
    started-ms (assoc :wall-time (/ (- now started-ms) 1000))))

(defn report
  "The report of a frame's :contract state at now: its tally with what used gives among the uses."
  ([state now] (report state now 0))
  ([state now extra-ms] (assoc (:tally state) :uses (used state now extra-ms))))

;; ------------------------------------------------------------------ endings

(def kinds
  "The kinds of an ending other than :done, in the order that decides when two hold in one round."
  [:cut-off :limit-reached :impossible :condition-failed :nothing-possible])

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

(defn cause-key
  "The keyword an ending's cause reads as (an event's :reason): the cause itself, a map's :reason, :fact, :line or
  :floor, else the kind."
  [{:keys [kind cause]}]
  (cond
    (keyword? cause) cause
    (map? cause) (or (some #(when (keyword? (get cause %)) (get cause %)) [:reason :fact :line :floor]) kind)
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

(declare status?)

(defn done?
  "What job def's done? export says now, a status, data {:units :done :left :result}; nil when it has none. Any other
  return is an error naming the job."
  [def c]
  (when-let [f (:done? def)]
    (let [s (f c)]
      (when-not (status? s)
        (throw (js/Error. (str "done? of " (or (:sym def) (:label def)) " returned " (pr-str s) ", not a status [kind text {:reason ..}]"))))
      s)))

(defn progress "The report fields of a done? status." [[_ _ data]] (select-keys data [:units :done :left :left-items]))

;; ------------------------------------------------------------------ statuses

(declare passes-through?)

(def status-kinds
  "A check's status [kind text data]: :ok holds, :later not now, :never can never hold, :look fails unless a look round
  finds otherwise."
  #{:ok :later :never :look})

(defn status?
  "Whether x is a status [kind text data], data a map with a keyword :reason."
  [x]
  (and (vector? x) (= 3 (count x)) (contains? status-kinds (first x)) (string? (second x))
       (map? (nth x 2)) (keyword? (:reason (nth x 2)))))

(defn status [kind reason text data] [kind text (assoc data :reason reason)])

(defn ok "[:ok text {:reason reason ...data}]" [reason text & {:as data}] (status :ok reason text data))
(defn later "[:later text {:reason reason ...data}]" [reason text & {:as data}] (status :later reason text data))
(defn never "[:never text {:reason reason ...data}]" [reason text & {:as data}] (status :never reason text data))
(defn look "[:look text {:reason reason ...data}]" [reason text & {:as data}] (status :look reason text data))

(defn status-cause
  "The cause an ending of status s carries, and a check's wait reason (job.waiting, observe): (assoc data :text text), without the :parts of a composite."
  [[_ text data]]
  (assoc (dissoc data :parts) :text text))

(defn with-slot "Status s with :slot in its data." [s slot] (assoc-in s [2 :slot] slot))

(defn all
  "Statuses ss as one: any :never, else the first :later, else the first :look, else :ok (texts joined by \"; \")."
  [ss]
  (let [first-of (fn [k] (some #(when (= k (first %)) %) ss))]
    (or (first-of :never) (first-of :later) (first-of :look)
        (if (seq ss)
          [:ok (str/join "; " (map second ss)) (nth (first ss) 2)]
          (ok :holds "holds")))))

(defn fn-name [f] (or (some-> (.-name f) not-empty demunge) "an anonymous fn"))

(defn run-condition
  "(f c); a throw is rethrown naming the condition fn."
  [c f]
  (try (f c)
       (catch :default e
         (if (passes-through? e)
           (throw e)
           (throw (js/Error. (str "condition " (fn-name f) ": " (or (.-message e) e))))))))

(defn asked
  "The status check f (export what, :can-start? or :can-run?, of def; both nil for a library check) gives for c; any other return is an error naming
  the export and the job (its symbol or combinator label)."
  ([c f] (asked c nil nil f))
  ([c def what f]
   (let [s (run-condition c f)]
     (when-not (status? s)
       (throw (js/Error. (str (if what (name what) "check") " of " (or (:sym def) (:label def) (fn-name f)) " returned " (pr-str s)
                              ", not a status [kind text {:reason ..}]"))))
     s)))

(defn verdict
  "What def's checks say now, as one status (all): its can-start? and can-run?, each that it has."
  [c def]
  (all (for [k [:can-start? :can-run?] :let [f (get def k)] :when f] (asked c def k f))))

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
   :spend {:doc "what the job may use up: {:job-time s (its rounds ran) :wall-time s (since it started) :tries n (children ended other than :cut-off, this frame and below, across rounds) :health hp :food pts :items {name n} :wear {tool uses}}"
           :default nil
           :spec (a/valid-by "a map {:job-time s :wall-time s :tries n :health hp :food pts :items {name n} :wear {tool uses}} of numbers >= 0"
                             (partial lines-problem #{:job-time :wall-time :tries :health :food} #{:items :wear}))}
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
      (and (contains? given :ways) (empty? (:ways given))) (str sym " :ways is empty, so no way may run")
      (seq unknown) (str sym " has no way " (str/join ", " (sort unknown)) "; "
                         (if (seq ways) (str "its ways are " (str/join ", " ways)) "it has no ways")))))

(defn resolve-settings
  "defaults with each qualified keyword value read with (lookup k), a settings key, and each fn value called with
  args, the call's args (a spend that grows with the work asked)."
  [defaults lookup args]
  (walk/postwalk #(cond (qualified-keyword? %) (lookup %) (fn? %) (% args) :else %) defaults))

(def merged-keys "Call keys a call merges over the defaults line by line." #{:spend :floor :use})

(defn freeze
  "The call keys of a call to a job with ways and defaults (settings resolved): given (the args) over the defaults,
  :spend, :floor and :use line by line; :ways all of the job's when neither says; :least 1; :deadline the caller's
  call's. :use from the defaults names a chooser, filled by engine.core.ending/fill-use, so only given lists count."
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

;; ------------------------------------------------------------------ limits

(a/defargs settings
  {::default-time-s {:default 30 :spec (a/num-in 0 nil)
                     :doc "The mean time a job with no quote-of and no reports quotes."}
   ::quote-s {:default 2 :spec (a/num-in 0 nil)
              :doc "The wall-clock budget of a quote asked with no quote deadline; past it the quote's time is ##Inf."}})

(defn remainder
  "What is left of each spend line the call of a frame's :contract state sets, now and extra-ms as for used."
  [state now extra-ms]
  (let [u (used state now extra-ms)
        left #(max 0 (- %2 (or %1 0)))]
    (into {} (map (fn [[k v]]
                    [k (if (map? v) (into {} (map (fn [[n x]] [n (left (get-in u [k n]) x)])) v) (left (get u k) v))]))
          (get-in state [:call :spend]))))

(defn left
  "What is left of the call of a frame's :contract state: {:spend (remainder) :floor :deadline}, now and extra-ms as for
  used."
  [state now extra-ms]
  (let [{:keys [floor deadline]} (:call state)]
    (cond-> {:spend (remainder state now extra-ms)} floor (assoc :floor floor) deadline (assoc :deadline deadline))))

(defn least-left
  "The least of lefts (left of each frame of a chain): each spend line's least remainder, each floor's highest level,
  the earliest deadline."
  [lefts]
  (reduce (fn [a b]
            (cond-> {:spend (merge-with #(if (map? %1) (merge-with min %1 %2) (min %1 %2)) (:spend a) (:spend b))}
              (or (:floor a) (:floor b)) (assoc :floor (merge-with max (:floor a) (:floor b)))
              (or (:deadline a) (:deadline b)) (assoc :deadline (apply min (keep :deadline [a b])))))
          {:spend {}}
          lefts))

(defn crossings
  "Each line of a frame's :contract state that is crossed, as [limit cause, amount]: {:used n}, the body's {:health n}
  or {:food n}, or {:now t}. levels is the body's {:health :food}, now the wall clock, extra-ms as for used. A spend is
  crossed past it, a floor below it."
  [{:keys [call] :as state} levels now extra-ms]
  (let [{:keys [spend floor deadline]} call
        u (used state now extra-ms)]
    (concat (when (and deadline (> now deadline)) [[{:line :deadline :deadline deadline} {:now now}]])
            (for [k [:health :food]
                  :let [f (get floor k) l (get levels k)]
                  :when (and f (number? l) (< l f))]
              [{:line k :floor f} {k l}])
            (for [k [:job-time :wall-time :tries :health :food]
                  :let [s (get spend k) x (get u k 0)]
                  :when (and s (> x s))]
              [(cond-> {:line k :spend s} (and (= :tries k) (:last-try state)) (assoc :last (:last-try state)))
               {:used x}])
            (for [[k what] [[:items :item] [:wear :tool]]
                  [n s] (get spend k)
                  :let [x (get-in u [k n] 0)]
                  :when (> x s)]
              [{:line k what n :spend s} {:used x}]))))

(defn most-crossings
  "Each line of a frame's :contract state an act quoted q could cross before it can be stopped, as [cause amount]:
  cause as crossings names the line, amount {:most m :left room}; lines crossings names already are left out. levels,
  now and extra-ms as for crossings."
  [{:keys [call] :as state} q levels now extra-ms]
  (let [{:keys [spend floor deadline]} call
        u (used state now extra-ms)
        uses (:uses q)
        most #(when (vector? %) (nth % 2))
        crossed (set (map first (crossings state levels now extra-ms)))
        over (fn [cause m room] (when (and m (> m room) (not (crossed cause))) [cause {:most m :left room}]))]
    (keep identity
          (concat [(when deadline (over {:line :deadline :deadline deadline} (most (:job-time uses)) (/ (- deadline now) 1000)))]
                  (for [k [:health :food]
                        :let [f (get floor k) l (get levels k)]
                        :when (and f (number? l))]
                    (over {:line k :floor f} (most (get uses k)) (- l f)))
                  (for [k [:job-time :wall-time :health :food]
                        :let [s (get spend k)]
                        :when s]
                    (over {:line k :spend s} (if (= :wall-time k) (some->> [(:job-time uses) (:wall-time uses)] (keep most) seq (reduce +)) (most (get uses k))) (- s (get u k 0))))
                  (for [[k what] [[:items :item] [:wear :tool]]
                        [n s] (get spend k)]
                    (over {:line k what n :spend s} (most (get-in uses [k n])) (- s (get-in u [k n] 0))))))))

(defn below-floor
  "The cause {:floor line :min f} of the first floor of call the body's levels are below, or nil."
  [call levels]
  (some (fn [k] (let [f (get-in call [:floor k]) l (get levels k)]
                  (when (and f (number? l) (< l f)) {:floor k :min f})))
        [:health :food]))

(defn clamp
  "A child's frozen call held to its caller's: each spend line it sets no more than left (the caller's remainder) has,
  the deadline no later than the caller's, each floor at least the caller's. [call lines], lines the paths changed."
  [call caller left]
  (let [spend (reduce-kv (fn [m k v]
                           (let [l (get left k)]
                             (cond
                               (nil? l) m
                               (map? v) (assoc m k (into {} (map (fn [[n x]] [n (if-let [y (get l n)] (min x y) x)])) v))
                               :else (assoc m k (min v l)))))
                         (:spend call) (:spend call))
        deadline (cond->> (:deadline call) (:deadline caller) (min (:deadline caller)))
        floor (merge-with max (:floor call) (:floor caller))
        out (cond-> call
              (seq spend) (assoc :spend spend)
              deadline (assoc :deadline deadline)
              (seq floor) (assoc :floor floor))
        lines (concat (for [k (keys spend) :when (not= (get spend k) (get-in call [:spend k]))] [:spend k])
                      (when (not= deadline (:deadline call)) [[:deadline]])
                      (for [k (keys floor) :when (not= (get floor k) (get-in call [:floor k]))] [:floor k]))]
    [out (vec lines)]))

(defn limit-error
  "The error a crossed line unwinds with to the call boundary of frame, the job id it ends."
  [frame cause]
  (doto (js/Error. (str "limit: " frame " reached " (pr-str cause)))
    (aset "code" "limit")
    (aset "limit" {:frame frame :cause cause})))

(defn limit-of "{:frame :cause} of a limit error, else nil." [e]
  (when (and (instance? js/Error e) (= "limit" (.-code e))) (aget e "limit")))

(defn passes-through?
  "Whether error e must pass a job's catch-all on up: a cut or a limit."
  [e]
  (and (instance? js/Error e) (contains? #{"cut" "limit"} (.-code e))))

;; ------------------------------------------------------------------ quotes

(def too-expensive "The quote of a quote that ran past its deadline." {:uses {:job-time [##Inf ##Inf ##Inf]}})

(defn figure? [x] (and (vector? x) (= 3 (count x)) (every? number? x) (apply <= x)))

(defn quote-problem
  "Why q is no quote {:uses {:job-time :health :food fig :items {name fig} :wear {tool fig}} :yields {:items {name fig}}
  :done fig :moves-to [x y z]}, each fig [least mean most], or nil."
  [q]
  (let [{:keys [uses yields]} q
        figs (concat (keep #(get uses %) [:job-time :health :food]) (vals (:items uses)) (vals (:wear uses))
                     (vals (:items yields)) (keep :done [q]))]
    (cond
      (not (map? q)) "not a map"
      (seq (remove figure? figs)) "a figure not [least mean most] with least <= mean <= most")))

(defn default-quote
  "The quote of a job with no quote-of: time (0, the mean time of reports, else guess-s, ##Inf)."
  [reports guess-s]
  (let [ts (keep #(get-in % [:uses :job-time]) reports)]
    {:uses {:job-time [0 (if (seq ts) (/ (reduce + ts) (count ts)) guess-s) ##Inf]}}))

(defn least-cause
  "{:reason :least :least :most} when quote q's most :done is short of call's :least (:all: done?'s :left, d), else
  nil; a quote with no :done passes."
  [call q d]
  (let [most (some-> (:done q) (nth 2))
        least (:least call)
        want (if (= :all least) (:left d) least)]
    (when (and most (number? want) (< most want)) {:reason :least :least least :most most})))

(defn breaks
  "The first line quote q's least crosses, given left ({:spend :floor :deadline}, ctx/left), the body's levels and
  now: {:line :least :left}, or nil. A least of ##Inf crosses a line that is not set too."
  [q {:keys [spend floor deadline]} levels now]
  (let [u (:uses q)
        over (fn [line fig room]
               (let [l (first fig)]
                 (when (and l (or (= ##Inf l) (and room (> l room)))) {:line line :least l :left room})))
        room (fn [k] (let [s (get spend k)
                           f (get floor k)
                           above (when (and f (number? (get levels k))) (- (get levels k) f))]
                       (if (and s above) (min s above) (or s above))))]
    (or (over :job-time (:job-time u) (:job-time spend))
        (when (:wall-time spend) (over :wall-time (:job-time u) (:wall-time spend)))
        (when deadline (over :deadline (:job-time u) (/ (- deadline now) 1000)))
        (over :health (:health u) (room :health))
        (over :food (:food u) (room :food))
        (some (fn [[k what]] (some (fn [[n fig]] (some-> (over k fig (get-in spend [k n])) (assoc what n))) (get u k)))
              [[:items :item] [:wear :tool]]))))
