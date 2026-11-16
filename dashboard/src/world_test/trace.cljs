(ns world-test.trace
  "The trace of a failed case: the body events from its setup on (first and last when too many) and the tail of body.log and, when the body ran with the step trace, the tail of its steps, kept next to the results."
  (:require [clojure.string :as str]
            ["fs" :as fs]
            ["path" :as path]))

(def max-events-bytes "Cap on the printed events of one trace." 200000)
(def max-log-bytes "Cap on the body.log tail of one trace." 20000)
(def max-steps-bytes "Cap on the step-trace tail of one trace." 400000)

(defn file-name
  "cases/ file of run `run`, attempt `attempt` (0 = first) of case id \"fixture/case\"."
  [id run attempt]
  (let [[fixture case-name] (if (str/includes? id "/") (str/split id #"/" 2) [id id])
        clean #(str/replace % #"[^A-Za-z0-9._-]" "_")]
    (str (clean fixture) "__" (clean case-name) "__r" run "-a" attempt ".edn")))

(defn tail-text [text max-bytes]
  (let [n (count text)] (if (<= n max-bytes) text (subs text (- n max-bytes)))))

(defn fit-count
  "How many of sized leading entries fit in max-bytes."
  [sizes max-bytes]
  (count (take-while #(<= % max-bytes) (rest (reductions + 0 sizes)))))

(defn cap-events
  "{:events the first and the latest events whose printed form fits max-bytes (half each, the decisions are at the start, the outcome at the end), :dropped how many in between were left out}. A stream that fits whole is kept whole."
  [events max-bytes]
  (let [size #(inc (count (pr-str %)))
        n (count events)
        whole (fit-count (map size events) (- max-bytes 2))]
    (if (= whole n)
      {:events (vec events) :dropped 0}
      (let [head (fit-count (map size events) (quot max-bytes 2))
            tail (min (- n head) (fit-count (map size (reverse events)) (- max-bytes (quot max-bytes 2) 2)))]
        {:events (into (vec (take head events)) (take-last tail events)) :dropped (- n head tail)}))))

(defn trace-doc
  [{:keys [id run attempt status why events log steps start-ticks]}]
  (let [{:keys [events dropped]} (cap-events events max-events-bytes)]
    (cond-> {:id id :run run :attempt attempt :status status}
      why (assoc :why why)
      start-ticks (assoc :start-ticks start-ticks)
      steps (assoc :steps-tail (tail-text steps max-steps-bytes))
      :always (assoc :events events :events-dropped dropped :body-log-tail (tail-text (or log "") max-log-bytes)))))

(defn write!
  "Writes doc as EDN into dir (created); returns the file path."
  [dir id run attempt doc]
  (let [file (path/join dir (file-name id run attempt))]
    (fs/mkdirSync dir #js {:recursive true})
    (fs/writeFileSync file (pr-str doc))
    file))
