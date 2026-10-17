(ns world-test.trace
  "The trace of a failed case: the body events from its setup on and the tail of body.log and, when the body ran with the step trace, the tail of its steps, kept next to the results."
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

(defn cap-events
  "{:events the latest events whose printed form fits max-bytes, :dropped how many older ones were left out}."
  [events max-bytes]
  (let [sizes (map #(inc (count (pr-str %))) (reverse events))
        kept (count (take-while #(<= % max-bytes) (rest (reductions + 2 sizes))))]
    {:events (vec (take-last kept events)) :dropped (- (count events) kept)}))

(defn trace-doc
  [{:keys [id run attempt status why events log steps]}]
  (let [{:keys [events dropped]} (cap-events events max-events-bytes)]
    (cond-> {:id id :run run :attempt attempt :status status}
      why (assoc :why why)
      steps (assoc :steps-tail (tail-text steps max-steps-bytes))
      :always (assoc :events events :events-dropped dropped :body-log-tail (tail-text (or log "") max-log-bytes)))))

(defn write!
  "Writes doc as EDN into dir (created); returns the file path."
  [dir id run attempt doc]
  (let [file (path/join dir (file-name id run attempt))]
    (fs/mkdirSync dir #js {:recursive true})
    (fs/writeFileSync file (pr-str doc))
    file))
