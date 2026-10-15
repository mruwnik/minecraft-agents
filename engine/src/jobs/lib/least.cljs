(ns jobs.lib.least
  "A partial outcome against the call's :least (a count of units, or :all). These jobs default to :all, so a stop short
  of everything is the stop's own ending unless the caller asks for less."
  (:require [engine.ctx :as ctx]))

(defn enough?
  "Whether n of total units meet the call's :least; false without one. A nil total never meets :all."
  [c n total]
  (let [least (:least (ctx/frozen-call c))]
    (boolean (and least (pos? n) (>= n (if (= :all least) (or total ##Inf) least))))))

(defn end!
  "ctx/end with kind and cause, but done when n of total units meet the call's :least (a :gave-up in data turns false)."
  [c kind cause data n total]
  (if (and (not= :done kind) (enough? c n total))
    (ctx/end c :done nil (cond-> data (contains? data :gave-up) (assoc :gave-up false)))
    (ctx/end c kind cause data)))
