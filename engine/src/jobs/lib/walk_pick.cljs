(ns jobs.lib.walk-pick
  "The one rule for choosing among places to walk to: rank the candidates by a lower bound of their price (the straight
  walk's seconds, walk-s, plus what does not hang on the way), quote the best through go-to (its plan, priced by
  jobs.lib.walk-risk: the one danger figure), and go on best-first until the quoted best is no dearer than every bound
  left. Danger has no straight-line bound: it comes from the quote alone."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.cost :as cost]
            [jobs.lib.util :as u]))

(defn walk-s
  "Seconds of the straight walk from the body to pos: a lower bound of any walk there."
  [c pos]
  (/ (u/dist (u/self-pos c) pos) (cost/walk-blocks-per-s)))

(defn risk-s
  "Seconds the risk of go-to's quote q costs the body (cost/risk-s): the hp its plan takes (its :uses :health) and
  extra-hurt hp besides, with its :risk's hurt and death chance."
  ([c q] (risk-s c q 0))
  ([c q extra-hurt]
   (let [{:keys [hp lethal-hp] :or {hp 0 lethal-hp 0}} (:risk q)]
     (cost/risk-s (.-health (.self (:primitives c)))
                  (+ (- hp lethal-hp) (second (get-in q [:uses :health] [0 0 0])) extra-hurt)
                  lethal-hp))))

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
  that walk. Quoted best-first by bound, until the best quoted is no dearer than the next bound."
  [c candidates {:keys [bound args price]}]
  (loop [[x & more :as left] (sort-by bound candidates) best nil]
    (if (or (empty? left) (and best (<= (:price best) (bound x))))
      best
      (let [q (await (quote! c (args x)))
            s (price x q)]
        (recur more (if (and (js/isFinite s) (or (nil? best) (< s (:price best)))) {:pick x :price s :quote q} best))))))

(defn ^:async pick!
  "The candidate best! picks, else the one of least bound: a quote that cannot price the walk (its plan cannot start where
  the body stands, say) leaves the walk to say whether it gets there."
  [c candidates opts]
  (or (:pick (await (best! c candidates opts))) (first (sort-by (:bound opts) candidates))))
