(ns jobs.lib.walk-pick
  "The one rule for choosing among places to walk to: rank the candidates by a lower bound of their price (the least
  seconds of the walk, walk-s, plus what does not hang on the way), quote the best through go-to (its plan, priced by
  jobs.lib.walk-risk: the one danger figure), and go on best-first until the quoted best is no dearer than every bound
  left. Danger has no straight-line bound: it comes from the quote alone."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.targets :as targets]
            [jobs.lib.util :as u]
            [jobs.lib.walk.plan :as wplan]
            [jobs.lib.walk.world :as wworld]
            [jobs.movement.go-to.preview :as preview]))

(defn walk-s
  "Seconds a walk from `from` into :range of :pos (go-to args) takes at the least: the flat blocks between their cells
  less the range (arrival is within the range of the goal cell), at cost/least-block-s. A lower bound of any plan's."
  [from {:keys [pos range]}]
  (let [cell #(update-vals (select-keys % [:x :z]) js/Math.floor)]
    (* (max 0 (- (u/flat-dist (cell from) (cell pos)) (or range 0))) cost/least-block-s)))

(defn risk-s
  "Seconds the risk of go-to's quote q costs the body (cost/risk-s): the hp its plan takes (its :uses :health) and
  extra-hurt hp besides, with its :risk's hurt and death chance."
  ([c q] (risk-s c q 0))
  ([c q extra-hurt]
   (let [{:keys [hp lethal-hp] :or {hp 0 lethal-hp 0}} (:risk q)]
     (cost/risk-s (.-health (.self (:primitives c)))
                  (+ (- hp lethal-hp) (second (get-in q [:uses :health] [0 0 0])) extra-hurt)
                  lethal-hp))))

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

(defn ^:async quote!
  "go-to's quote of the walk args (a go-to call's args)."
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

(defn ^:async quote-once!
  "go-to's quote of the walk args, asked once in this pass (once!)."
  [c args]
  (await (once! c [:quote args] #(quote! c args))))

(defn ^:async best!
  "{:pick :price :quote} the cheapest of candidates, nil when none has a finite price. bound (fn [x]) seconds no more
  than x's price; args (fn [x]) the go-to args of the walk to x; price (fn [x q]) x's seconds with q, go-to's quote of
  that walk; ok? (fn [x], default all) is asked of a candidate only when its turn comes, so a costly test is paid for just the
  candidates reached; quote (fn [c args], default quote!) asks go-to, so a caller can reuse one quote. Quoted best-first by bound, until the best quoted is no dearer than the next bound.
  rebound (fn [x q left]), asked after each quote until it answers non-nil: a fn is a tighter bound of the candidates left, ranked by it from then on."
  [c candidates {:keys [bound args price ok? quote rebound]}]
  (loop [[x & more :as left] (sort-by bound candidates) best nil bound bound rebound rebound]
    (cond
      (or (empty? left) (not (js/isFinite (bound x))) (and best (<= (:price best) (bound x)))) best
      (and ok? (not (ok? x))) (recur more best bound rebound)
      :else (let [q (await ((or quote quote!) c (args x)))
                  s (price x q)
                  best (if (and (js/isFinite s) (or (nil? best) (< s (:price best)))) {:pick x :price s :quote q} best)
                  r (when rebound (await (rebound x q more)))]
              (cond
                (fn? r) (recur (sort-by r more) best r nil)
                (some? r) (recur more best bound nil)
                :else (recur more best bound rebound))))))

(defn ^:async pick!
  "The candidate best! picks, else the one of least bound: a quote that cannot price the walk (its plan cannot start where
  the body stands, say) leaves the walk to say whether it gets there."
  [c candidates opts]
  (or (:pick (await (best! c candidates opts))) (first (sort-by (:bound opts) candidates))))

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
  (its estimate is no price): x is left out this call; with none priced and one searching, :searching (ask again later).
  Nil when no walk is priced. price (fn [x s]), default s, adds to a walk's seconds s what x costs besides.
  The first quote that finds no way (no-way?) asks once whether the others (cells with a y, walks over what the body has
  seen) are out of reach too (none-reachable!): then each
  of their quotes would be go-to's guess with no route, ##Inf, or with a way that changes the world (the quote's mean is finite)
  quote-of-route's rest-s of its straight blocks, so that is their bound; :searching while that search goes on."
  [c xs {:keys [target walk limit-s ok? quote price] :or {price (fn [_ s] s) limit-s ##Inf}}]
  (let [feet (u/self-pos c)
        cell #(update-vals (select-keys % [:x :z]) js/Math.floor)
        bound #(walk-s feet (target %))
        searching (atom false)
        reaching (atom false)
        guess-s (fn [x] (* (if (:back (walk x)) 2 1) (preview/rest-s (js/Math.floor (u/flat-dist (cell feet) (cell (:pos (target x))))))))
        rebound (fn ^:async tighter [_ q left]
                  (when (no-way? q)
                    (let [goals (map #(let [{:keys [pos range]} (target %)] (assoc (cell pos) :y (some-> (:y pos) js/Math.floor) :range (or range 0))) left)
                          r (when (and (seq left) (every? :y goals) (not-any? #(false? (:unknown (walk %))) left))
                              (await (none-reachable! c goals)))
                          mean (second (get-in q [:uses :job-time]))]
                      (cond
                        (= :searching r) (do (reset! reaching true) (constantly ##Inf))
                        (not (true? r)) false
                        (and (number? mean) (js/isFinite mean)) (fn [x] (max (bound x) (guess-s x)))
                        :else (constantly ##Inf)))))
        best (await (best! c (filter #(<= (bound %) limit-s) xs)
                           {:bound bound :args walk :ok? ok? :quote quote :rebound rebound
                            :price (fn [x q] (if (:estimate q)
                                               (do (reset! searching true) ##Inf)
                                               (price x (quoted-s c q))))}))]
    (cond
      @reaching :searching
      best best
      @searching :searching)))
