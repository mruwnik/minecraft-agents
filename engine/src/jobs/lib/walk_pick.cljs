(ns jobs.lib.walk-pick
  "The one rule for choosing among places to walk to: rank the candidates by a lower bound of their price (the least
  seconds of the walk, walk-s, plus what does not hang on the way), quote the best through go-to (its plan, priced by
  jobs.lib.walk-risk: the one danger figure), and go on best-first until the quoted best is no dearer than every bound
  left. Danger has no straight-line bound: it comes from the quote alone."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.util :as u]))

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

(defn ^:async best!
  "{:pick :price :quote} the cheapest of candidates, nil when none has a finite price. bound (fn [x]) seconds no more
  than x's price; args (fn [x]) the go-to args of the walk to x; price (fn [x q]) x's seconds with q, go-to's quote of
  that walk; ok? (fn [x], default all) is asked of a candidate only when its turn comes, so a costly test is paid for just the
  candidates reached; quote (fn [c args], default quote!) asks go-to, so a caller can reuse one quote. Quoted best-first by bound, until the best quoted is no dearer than the next bound."
  [c candidates {:keys [bound args price ok? quote]}]
  (loop [[x & more :as left] (sort-by bound candidates) best nil]
    (cond
      (or (empty? left) (and best (<= (:price best) (bound x)))) best
      (and ok? (not (ok? x))) (recur more best)
      :else (let [q (await ((or quote quote!) c (args x)))
                  s (price x q)]
              (recur more (if (and (js/isFinite s) (or (nil? best) (< s (:price best)))) {:pick x :price s :quote q} best))))))

(defn ^:async pick!
  "The candidate best! picks, else the one of least bound: a quote that cannot price the walk (its plan cannot start where
  the body stands, say) leaves the walk to say whether it gets there."
  [c candidates opts]
  (or (:pick (await (best! c candidates opts))) (first (sort-by (:bound opts) candidates))))

(defn ^:async cheapest!
  "{:pick x :price seconds :quote q} of the one of xs whose walk go-to quotes cheapest, within limit-s seconds (those whose
  straight walk alone is over it are left out, the rest quoted best-first, best!). target (fn [x]) {:pos :range} bounds the
  straight walk, walk (fn [x]) are the go-to args; ok? and quote as best!. A quote whose search still goes on prices nothing
  (its estimate is no price): x is left out this call; with none priced and one searching, :searching (ask again later).
  Nil when no walk is priced. price (fn [x s]), default s, adds to a walk's seconds s what x costs besides."
  [c xs {:keys [target walk limit-s ok? quote price] :or {price (fn [_ s] s) limit-s ##Inf}}]
  (let [feet (u/self-pos c)
        bound #(walk-s feet (target %))
        searching (atom false)
        best (await (best! c (filter #(<= (bound %) limit-s) xs)
                           {:bound bound :args walk :ok? ok? :quote quote
                            :price (fn [x q] (if (:estimate q)
                                               (do (reset! searching true) ##Inf)
                                               (price x (quoted-s c q))))}))]
    (or best (when @searching :searching))))
