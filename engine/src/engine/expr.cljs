(ns engine.expr
  "Job expressions: the EDN form of a job spec, parsed (never evaluated)
  against the job registry into a node. See README.md, Job expressions.

    (jobs.survival.eat {:until 18})   a leaf; args merged over the defaults
    (seq e1 e2 ...)                     children in order
    (any e1 e2 ...)                     the first child whose check passes
    (repeat e)                          e again, fresh, each time it is done
    (repeat n e)                        the same, done after n runs (n a positive integer)
    (until g e)                         e repeated, done once g's check passes

  Nodes: {:op :leaf :job sym :args map}, {:op :seq|:any :children [node]},
  {:op :repeat :child node :times n?}, {:op :until :children [guard child]}. They are plain EDN, so they persist."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.settings :as settings]))

(def combinators #{'seq 'any 'repeat 'until})

(defn fail [msg form]
  (throw (ex-info msg {:form form})))

(defn leaf
  "[def args] for job symbol sym with args merged over its defaults (a declared group key by key) and conformed by
  its spec (engine.args: positions become {:x :y :z}); the call keys (engine.contract/call-keys) are checked apart.
  Throws when sym is not in the registry, is no contract job (a round and can-start? or can-run?, or no old check) or a value does not fit."
  [registry sym args]
  (let [entry (get registry sym)
        data (:args entry)]
    (cond
      (nil? entry) (throw (ex-info (str "unknown job " sym) {:job sym}))
      (not (and (or (some some? ((juxt :can-start? :can-run?) entry)) (nil? (:check entry)) (:engine.core.ending/wrapped entry)) (fn? (:round entry))))
      (throw (ex-info (str "job " sym " needs a round and no old check, or can-start? or can-run?") {:job sym}))
      :else (if-let [problem (contract/call-problem sym (:ways entry) args)]
              (fail problem args)
              (try [entry (a/conform sym data (a/merge-args data (settings/job-defaults sym data) args))]
                   (catch :default e (fail (ex-message e) args)))))))

(defn unknown-keys-message
  "Why args holds keys the job does not declare (the call keys aside), at any level, or nil: each unknown key of the first level holding one
  named, then the keys of that level. The job registry always carries :args (nil for a job with none); a hand-built
  entry without the key is not checked."
  [sym entry args]
  (when (contains? entry :args)
    (when-let [[[path known] :as all] (seq (a/unknown (:args entry) (apply dissoc args
                                                                             (keys contract/call-keys))))]
      (let [level (vec (butlast path))
            ks (sort-by str (keep (fn [[p _]] (when (= level (vec (butlast p))) (last p))) all))
            listing (if (seq known) (str "its " (if (seq level) "keys" "args") " are " (str/join ", " (sort-by str known)))
                        "it takes no args")]
        (str sym (when (seq level) (str " " (a/path-text level))) " has no " (if (seq level) "key " "arg ")
             (str/join ", " ks) "; " listing)))))

(defn parse-form
  "The node for job spec form; throws with a message naming the problem."
  [registry form]
  (let [head (when (and (seq? form) (seq form)) (first form))
        parts (when (seq? form) (rest form))
        n (count parts)]
    (cond
      (not (and (seq? form) (symbol? head)))
      (fail (str "a job spec is a list such as (jobs.survival.eat) or (seq ...), not " (pr-str form)) form)

      (and (= 'repeat head) (= 1 n))
      {:op :repeat :child (parse-form registry (first parts))}

      (and (= 'repeat head) (= 2 n))
      (if (pos-int? (first parts))
        {:op :repeat :times (first parts) :child (parse-form registry (second parts))}
        (fail "repeat takes an optional count (a positive integer) and a job spec" form))

      (= 'repeat head)
      (fail "repeat takes exactly one job spec, after an optional count" form)

      (= 'until head)
      (if (= 2 n)
        {:op :until :children (mapv #(parse-form registry %) parts)}
        (fail "until takes a guard job spec and a job spec" form))

      (#{'seq 'any} head)
      (if (pos? n)
        {:op (keyword head) :children (mapv #(parse-form registry %) parts)}
        (fail (str head " takes at least one job spec") form))

      (not (contains? registry head))
      (fail (str "unknown job or combinator " head "; jobs are namespaces under jobs/,"
                 " combinators are seq, any, repeat, until")
            form)

      (> n 1) (fail (str head " takes at most one args map") form)

      (and (= 1 n) (not (map? (first parts)))) (fail (str "args of " head " must be a map") form)

      (unknown-keys-message head (get registry head) (first parts))
      (fail (unknown-keys-message head (get registry head) (first parts)) form)

      :else {:op :leaf :job head :args (second (leaf registry head (first parts)))})))

(defn parse
  "The node for a job spec form; throws with a message naming the problem and the whole spec."
  [registry form]
  (try
    (parse-form registry form)
    (catch :default e
      (throw (ex-info (str (ex-message e) ", in " (pr-str form)) {:form form} e)))))

(defn problem
  "The message of what is wrong with form, or nil."
  [registry form]
  (try (parse registry form) nil
       (catch :default e (ex-message e))))

(defn label
  "A short printable name of a node: the job symbol, or the combinator form."
  [{:keys [op job children child times]}]
  (case op
    :leaf (str job)
    :repeat (str "(repeat " (when times (str times " ")) (label child) ")")
    (str "(" (name op) " " (str/join " " (map label children)) ")")))
