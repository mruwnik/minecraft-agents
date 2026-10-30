(ns world-test.swim-times
  "tools/event-counts.mjs swim-times: counts the measured prior behind go-to ::swim-ms from the bodies' logs alone: the duration of
  each go-to swim (a swim action ending with the status of a :go-to.unstuck event of the same job, within match-ms), a landed one
  as seen exactly, a timeout (the hold ran out at its cap) as right-censored there; other statuses are left out. report fits them
  (world-test.censored): a quantile past the cap is unobservable, so then :lower-bound? is true and the quantile is the cap.
  Pure parts: swims, report."
  (:require [clojure.string :as str]
            [world-test.censored :as censored]))

(def match-ms "Longest gap between a swim action's end and its :go-to.unstuck event." 100)

(defn- field [re line] (second (re-find re line)))
(defn- time-of [line] (some-> (field #":time-ms (\d+)" line) js/parseInt))

(defn swims
  "[{:status :ms}] of the go-to swims in lines (one body's logs, any order): each swim action that ended with an unstuck
  event's status in the same job."
  [lines]
  (let [acts (filter #(and (str/includes? % ":source :action") (str/includes? % ":name \"swim\"")) lines)
        id-of (partial field #":action-id \"([^\"]*)\"")
        starts (into {} (for [l acts :when (str/includes? l ":kind :started")] [(id-of l) (time-of l)]))
        ends (vals (into {} (for [l acts :when (str/includes? l ":kind :done") :let [s (starts (id-of l))] :when s]
                              [(id-of l) {:job (field #":job-id \"([^\"]*)\"" l) :status (field #":status \"([^\"]*)\"" l)
                                          :t (time-of l) :ms (- (time-of l) s)}])))
        unstuck (for [l lines :when (str/includes? l ":kind :go-to.unstuck")]
                  {:job (field #":job-id \"([^\"]*)\"" l) :status (field #":status \"([^\"]*)\"" l) :t (time-of l)})]
    (vec (for [u unstuck
               :let [e (first (filter #(and (= (:job u) (:job %)) (= (:status u) (:status %)) (<= (js/Math.abs (- (:t u) (:t %))) match-ms)) ends))]
               :when e]
           (select-keys e [:status :ms])))))

(defn report
  "{:n :landed :capped :other :p :quantile :lower-bound?} of the swims (swims' rows) at quantile q: landed ones exact, timeouts
  censored at their length; nil with none of either."
  [rows q]
  (let [landed (filter #(= "landed" (:status %)) rows)
        capped (filter #(= "timeout" (:status %)) rows)
        xs (concat (map (fn [{:keys [ms]}] [ms ms]) landed) (map (fn [{:keys [ms]}] [ms nil]) capped))]
    (when (seq xs)
      (let [f (censored/fit xs)
            x (censored/quantile f q)
            cum (reduce + (map second (filter #(<= (first %) x) (map vector (:at f) (:p f)))))]
        {:n (count xs) :landed (count landed) :capped (count capped) :other (- (count rows) (count xs))
         :q q :quantile x :lower-bound? (< cum (- q 1e-9))}))))
