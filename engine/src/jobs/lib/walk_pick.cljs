(ns jobs.lib.walk-pick
  "The one rule for choosing among places to walk to: rank the candidates by a lower bound of their price (the least
  seconds of the walk, walk-s, plus what does not hang on the way), quote the best through go-to (its plan, priced by
  jobs.lib.walk-risk: the one danger figure), and go on best-first until the quoted best is no dearer than every bound
  left. Danger has no straight-line bound: it comes from the quote alone."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.clock :as clock]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.planner :as cost-planner]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.targets :as targets]
            [jobs.lib.threat-field :as field]
            [jobs.lib.util :as u]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk-risk :as walk-risk]
            [jobs.lib.walk.world :as wworld]))

(defn walk-s
  "Seconds a walk from `from` into :range of :pos (go-to args) takes at the least: the flat blocks between their cells
  less the range (arrival is within the range of the goal cell), cost/least-route-s. A lower bound of any plan's."
  [from {:keys [pos range]}]
  (let [f js/Math.floor]
    (cost/least-route-s (js/Math.hypot (- (f (:x from)) (f (:x pos))) (- (f (:z from)) (f (:z pos)))) (or range 0))))

(defn not-worth
  "{:reason :not-worth :worth :walk-s} while even the least walk from the body into walk (go-to args, nil: none) costs
  more than the pass there is worth (worth: (fn [] seconds), asked only for a walk to make), else nil."
  [c walk worth]
  (let [s (if walk (walk-s (u/self-pos c) walk) 0)]
    (when (pos? s)
      (let [w (worth)]
        (when-not (cost/affords? s w) {:reason :not-worth :worth w :walk-s s})))))

(defn risk-s
  "Seconds the risk of go-to's quote q costs the body (cost/risk-s): the hp its plan takes (its :uses :health) and
  extra-hurt hp besides, with its :risk's hurt and death chance."
  ([c q] (risk-s c q 0))
  ([c q extra-hurt]
   (walk-risk/risk-s c (:risk q) (+ (second (get-in q [:uses :health] [0 0 0])) extra-hurt))))

(defn ev-worth
  "Seconds a find worth `worth` is worth to the body that walks go-to's quote q to it: worth times the chance the walk
  does not end in death (cost/survived-worth); the walk's price (quoted-s) carries the death's own cost."
  [worth q]
  (cost/survived-worth worth (get-in q [:risk :death-chance])))

(defn quoted-s
  "Seconds go-to's quote q of a walk costs the body: its mean time and its risk-s; 0 for a body already there (a :done
  ending), ##Inf for another ending or a walk with no finite time."
  [c q]
  (let [mean (second (get-in q [:uses :job-time]))]
    (cond
      (= :done (:kind q)) 0
      (or (contract/ending? q) (not (number? mean)) (not (js/isFinite mean))) ##Inf
      :else (+ mean (risk-s c q)))))

(defn ^:async walk-back-worth?
  "Whether walking back to cell ([x y z]) to go on with a job there costs less than redo-s seconds of its work done again
  where the body stands: go-to's quote of a route there (quoted-s: its mean job-time, a dig priced in, and its risk) is no
  dearer, by the ways of the walk's own args (jobs.lib.child/walk-back-args); a route not found counts only when those ways
  change the world (a dig finds it). true / false, or :searching while go-to's quote search goes on (ask again next round).
  Quoted at worth redo-s, what it is weighed against; false unquoted when even the least walk there (walk-s) is dearer."
  [c walk-args redo-s]
  (let [args (-> (select-keys walk-args [:range :ways]) (assoc :worth redo-s) (assoc :pos (zipmap [:x :y :z] (:pos walk-args))))]
    (if (> (walk-s (u/self-pos c) args) redo-s)
      false
      (let [q (await (ctx/quote c 'jobs.movement.go-to args))
            route (:route q)]
        (if (= :searching (:reason route))
          :searching
          (boolean (and (<= (quoted-s c q) redo-s) (or (:found route) (:escalating route)))))))))

(defn ^:async quote!
  "go-to's quote of the walk args (a go-to call's args), at the worth they name, else the calling job's: a walk with the same args then judges by the same worth."
  [c args]
  (await (ctx/quote c 'jobs.movement.go-to args)))

(defn ^:async once!
  "(f)'s value, asked once in this pass (::pass in c, an atom a caller's pass sets, keeps the answers; none: asked each time):
  the quotes and sweeps one pass's choices share."
  [c k f]
  (let [memo (::pass c)]
    (if-not memo
      (await (f))
      (let [pending (or (get @memo k) (let [pr (f)] (swap! memo assoc k pr) pr))]
        (await pending)))))

(defn ^:async kept-quote!
  "go-to's quote of the walk args, kept per body (threat-field/shared) while its world epoch, walk-risk/body-inputs,
  health, carried items and the walk's worth (the args', else the caller's) hold, so a later pass asks no unchanged walk again. A search still going on (:estimate) or a
  quote past its deadline is not kept: asked again, it goes on."
  [c args]
  (let [p (:primitives c)
        box (field/shared c [::quote args (or (:worth args) (ctx/worth c)) (hash (walk-risk/body-inputs c)) (.-health (.self p)) (mapv (juxt :name :count) (u/inventory p))]
                          #(volatile! nil) (constantly 1))]
    (or @box
        (let [q (await (quote! c args))]
          (when-not (or (:estimate q) (= contract/too-expensive q)) (vreset! box q))
          q))))

(defn ^:async quote-once!
  "go-to's quote of the walk args, asked once in this pass (once!) and kept across passes (kept-quote!)."
  [c args]
  (await (once! c [:quote args] #(kept-quote! c args))))

(defn by-bound
  "xs as (sort-by bound xs) orders them, each bound asked once: a sort asks its key at every comparison."
  [bound xs]
  (let [a (to-array (map (fn [x] #js [(bound x) x]) xs))]
    (.sort a (fn [^js l ^js r] (let [x (aget l 0) y (aget r 0)] (cond (< x y) -1 (< y x) 1 :else 0))))
    (map #(aget ^js % 1) a)))

(defn ^:async best!
  "{:pick :price :quote} the cheapest of candidates, nil when none has a finite price. bound (fn [x]) seconds no more
  than x's price; args (fn [x]) the go-to args of the walk to x; price (fn [x q]) x's seconds with q, go-to's quote of
  that walk; ok? (fn [x], default all) is asked of a candidate only when its turn comes, so a costly test is paid for just the
  candidates reached; quote (fn [c args], default quote!) asks go-to, so a caller can reuse one quote. Quoted best-first
  by bound until the best quoted is no dearer than the next bound, each one whose quote-gain over the ratios of the
  quoted ones is more than a quote costs (the seconds
  on the clock the quotes so far took each, their ok? tests included, or, when more, their route searches' expansions
  at cost/search-cell-s); the others are passed over.
  rebound (fn [x q left]), asked after each quote until it answers non-nil: a fn is a tighter bound of the candidates left, ranked by it from then on."
  [c candidates {:keys [bound args price ok? quote rebound]}]
  (let [t0 (clock/now)
        ratios (volatile! [])
        quoted (volatile! 0)
        searched (volatile! 0)
        ;; a search's CPU by its expansions too: the :test build's clock stands still
        quote-s #(if (zero? @quoted) 0 (/ (max (/ (- (clock/now) t0) 1000) (* @searched (cost/search-cell-s))) @quoted))]
    (loop [[x & more :as left] (by-bound bound candidates) best nil bound bound rebound rebound]
      (cond
        (or (empty? left) (not (js/isFinite (bound x))) (and best (<= (:price best) (bound x)))) best
        (or (and best (<= (cost/quote-gain (:price best) (bound x) @ratios) (quote-s)))
            (and ok? (not (ok? x)))) (recur more best bound rebound)
        :else (let [b (bound x)
                    q (await ((or quote quote!) c (args x)))
                    s (price x q)
                    _ (vswap! quoted inc)
                    _ (vswap! searched + (or (get-in q [:route :nodes]) 0))
                    _ (when (and (pos? b) (not (:estimate q))) (vswap! ratios conj (/ s b)))
                    best (if (and (js/isFinite s) (or (nil? best) (< s (:price best)))) {:pick x :price s :quote q} best)
                    r (when rebound (await (rebound x q more)))]
                (cond
                  (fn? r) (recur (by-bound r more) best r nil)
                  (some? r) (recur more best bound nil)
                  :else (recur more best bound rebound)))))))

(defn ^:async pick!
  "The candidate best! picks, else the one of least bound: a quote that cannot price the walk (its plan cannot start where
  the body stands, say) leaves the walk to say whether it gets there."
  [c candidates opts]
  (or (:pick (await (best! c candidates opts))) (first (by-bound (:bound opts) candidates))))

(defn ^:async pick-walk!
  "The one of xs the body walks to cheapest by pick!: (to x) the go-to args of the walk, (on x) seconds after it that do
  not hang on the way (default 0). nil for no xs; a lone one is taken unquoted."
  ([c xs to] (pick-walk! c xs to (constantly 0)))
  ([c xs to on]
   (if (next xs)
     (let [me (u/self-pos c)]
       (await (pick! c xs {:bound #(+ (walk-s me (to %)) (on %)) :args to :price (fn [x q] (+ (quoted-s c q) (on x)))})))
     (first xs))))

(defn no-way?
  "Whether go-to's quote q found no way because its search ran out of land: the body's land is closed under that walk's
  policy, so another goal is reached only from within it."
  [q]
  (and (false? (get-in q [:route :found])) (contains? #{:exhausted :start-enclosed} (get-in q [:route :reason]))
       (nil? (get-in q [:route :partial])) (not (:estimate q))))

(defn reach-policy
  "go-to's walk policy at its most lenient (doors opened, no damage budget), for a search whose finding no way proves
  that no go-to quote finds one."
  [c]
  (-> (wworld/body-policy c) (update :moves conj :open) (assoc :damage-budget ##Inf)))

(defn ^:async none-reachable!
  "true when one search over the goals ({:x :y :z :range}) proves the body walks to none of them however go-to plans
  (targets/nearest! over what the body has seen, every goal and go-to's own stands spared the rock guess, reach-policy,
  no danger or darkness costs); :searching while that search goes on; false when it reaches one or proves nothing."
  [c goals]
  (let [policy (reach-policy c)
        cells (mapv (juxt :x :y :z) goals)
        unknown (wworld/with-seeds (wworld/unknown-spec c) nil (-> cells (into (ledger/stair-stands c)) (into (ledger/tunnel-stands c))))
        a (await (targets/nearest! c goals 0 {:tag ::reach :all true :budget targets/max-nodes :policy policy :box (wplan/box-of policy)
                                               :unknown unknown :dangers false :dark false}))]
    (case (:status a)
      :searching :searching
      :none (true? (:proved a))
      false)))

(defn ^:async cheapest!
  "{:pick x :price seconds :quote q} of the one of xs whose walk go-to quotes cheapest, within limit-s seconds (those whose
  straight walk alone is over it are left out, the rest quoted best-first, best!). target (fn [x]) {:pos :range} bounds the
  straight walk, walk (fn [x]) are the go-to args; ok? and quote as best!. A quote whose search still goes on prices nothing
  (its estimate is no price): x is left out this call; with none priced and one searching, :searching (ask again later); a priced pick wins over a search still going on unless one of the candidates it left out could
  still be cheaper (its straight walk under the pick's price): then :searching.
  Nil when no walk is priced. price (fn [x s]), default s, adds to a walk's seconds s what x costs besides.
  The first quote that finds no way (no-way?) asks once whether the others (cells with a y, walks over what the body has
  seen) are out of reach too (none-reachable!): then each
  of their quotes would be go-to's guess with no route, ##Inf, or with a way that changes the world (the quote's mean is finite)
  cost-planner/left-s of its straight blocks, so that is their bound; :searching while that search goes on."
  [c xs {:keys [target walk limit-s ok? quote price] :or {price (fn [_ s] s) limit-s ##Inf}}]
  (let [feet (u/self-pos c)
        cell #(update-vals (select-keys % [:x :z]) js/Math.floor)
        bound #(walk-s feet (target %))
        searching (atom false)
        reaching (atom nil)
        guess-s (fn [x] (cost-planner/left-s (u/flat-dist (cell feet) (cell (:pos (target x)))) (:back (walk x))))
        rebound (fn ^:async tighter [_ q left]
                  (when (no-way? q)
                    (let [goals (map #(let [{:keys [pos range]} (target %)] (assoc (cell pos) :y (some-> (:y pos) js/Math.floor) :range (or range 0))) left)
                          r (when (and (seq left) (every? :y goals) (not-any? #(false? (:unknown (walk %))) left))
                              (await (none-reachable! c goals)))
                          mean (second (get-in q [:uses :job-time]))]
                      (cond
                        (= :searching r) (do (reset! reaching (apply min (map bound left))) (constantly ##Inf))
                        (not (true? r)) false
                        (and (number? mean) (js/isFinite mean)) (fn [x] (max (bound x) (guess-s x)))
                        :else (constantly ##Inf)))))
        best (await (best! c (cond->> xs (js/isFinite limit-s) (filter #(<= (bound %) limit-s)))
                           {:bound bound :args walk :ok? ok? :quote quote :rebound rebound
                            :price (fn [x q] (if (:estimate q)
                                               (do (reset! searching true) ##Inf)
                                               (price x (quoted-s c q))))}))]
    (cond
      (and best (or (nil? @reaching) (<= (:price best) @reaching))) best
      (or @reaching @searching) :searching)))

(defn ^:async searched!
  "{:pick x :price seconds :quote q} of the one of xs the body walks to soonest by one search over all their targets
  (targets/nearest! :all, go-to's world over what the body has seen: dangers and darkness priced, the planner's own price
  of a route), then go-to's quote of that walk alone (Q141: one search ranks them, not a quote each; the quote's risk
  past the planner's, walk-risk's, does not rank them). target (fn [x]) {:pos :range}, walk (fn [x]) the go-to args;
  ok? (fn [x]), asked of the pick alone, or a quote with no finite price reject! the pick (fn [x]) and the search runs
  again without it; rejected? (fn [x]) leaves out the ones rejected before. state, an atom of {} the caller keeps while
  what the search rests on holds, keeps a pick whose quote goes on and the answer once there is one. :searching while a
  search or a quote goes on, or when this call's next step (a search, a test or a quote, at the mean of those so far)
  would take it past cost/search-budget-s (Q141): the first step of a call always runs, a search stops at what the
  budget has left; nil when no target is reached. price (fn [x s]), default s, adds to a walk's seconds s what x costs
  besides; quote as best!."
  [c xs {:keys [target walk ok? quote price state rejected? reject!] :or {price (fn [_ s] s)}}]
  (let [t0 (clock/now)
        goal (fn [x] (let [{:keys [pos range]} (target x)] (assoc (update-vals (select-keys pos [:x :y :z]) js/Math.floor) :range (or range 0))))
        quote-pick! (fn ^:async quote-pick [x]
                      (let [q (await ((or quote quote!) c (walk x)))
                            s (when-not (:estimate q) (price x (quoted-s c q)))]
                        (cond
                          (nil? s) (do (swap! state assoc :pending x) :searching)
                          (js/isFinite s) (do (swap! state assoc :answer {:pick x :price s :quote q}) :done)
                          :else (do (swap! state dissoc :pending) (reject! x) :again))))]
    ;; a kept pick or answer no longer among xs (the caller tried it since) is forgotten
    (let [in (set xs)]
      (swap! state #(cond-> %
                      (and (:pending %) (not (in (:pending %)))) (dissoc :pending)
                      (some-> (:answer %) :pick in not) (dissoc :answer))))
    (loop []
      (let [t (clock/now)
            {:keys [pending]} @state
            r (cond
                (contains? @state :answer) :done
                pending (await (quote-pick! pending))
                :else (let [left (vec (remove rejected? xs))
                            goals (mapv goal left)
                            unknown (wworld/with-seeds (wworld/unknown-spec c) nil (-> (mapv (juxt :x :y :z) goals) (into (ledger/stair-stands c)) (into (ledger/tunnel-stands c))))
                            a (when (seq left) (await (targets/nearest! c goals 0 {:tag ::searched :all true :unknown unknown
                                                                                    :ms (max 0 (- (* 1000 (cost/search-budget-s)) (- t t0)))})))
                            x (when (= :found (:status a)) (nth left (:index a)))]
                        (cond
                          (= :searching (:status a)) :searching
                          (nil? x) (do (swap! state assoc :answer nil) :done)
                          (and ok? (not (ok? x))) (do (reject! x) :again)
                          :else (await (quote-pick! x)))))]
        (swap! state #(-> % (update :ms (fnil + 0) (- (clock/now) t)) (update :steps (fnil inc 0))))
        (case r
          :again (let [{:keys [ms steps]} @state]
                   (if (cost/search-pays? (/ (- (clock/now) t0) 1000) (/ ms steps 1000) (cost/search-budget-s)) (recur) :searching))
          :done (:answer @state)
          r)))))
