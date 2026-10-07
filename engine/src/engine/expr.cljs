(ns engine.expr
  "Job expressions: the EDN form of a job spec, parsed (never evaluated)
  against the job registry into a node. See README.md, Job expressions.

    (jobs.survival.eat {:until 18})   a leaf; args merged over the defaults
    (seq e1 e2 ...)                     children in order
    (any e1 e2 ...)                     the first child whose check passes
    (repeat e)                          e again, fresh, each time it is done
    (repeat n e)                        the same, done after n runs (n a positive integer)
    (until g e)                         e repeated, done once g's check passes
    (hold e)                            e, holding the body; top level only

  Nodes: {:op :leaf :job sym :args map}, {:op :seq|:any :children [node]},
  {:op :repeat :child node :times n?}, {:op :until :children [guard child]}. They are plain EDN, so they persist."
  (:require [clojure.string :as str]
            [engine.args :as a]))

(def combinators #{'seq 'any 'repeat 'until 'hold})

(defn fail [msg form]
  (throw (ex-info msg {:form form})))

(defn leaf
  "[def args] for job symbol sym with args merged over its defaults (a declared group key by key) and conformed by
  its spec (engine.args: positions become {:x :y :z}); throws when sym is not in the registry or a value does not fit."
  [registry sym args]
  (let [entry (get registry sym)
        data (:args entry)]
    (cond
      (nil? entry) (throw (ex-info (str "unknown job " sym) {:job sym}))
      (not (and (fn? (:check entry)) (fn? (:round entry))))
      (throw (ex-info (str "job " sym " needs a check and a round") {:job sym}))
      :else (try [entry (a/conform sym data (a/merge-args data (a/defaults data) args))]
                 (catch :default e (fail (ex-message e) args))))))

(defn unknown-keys-message
  "Why args holds keys the job does not declare, at any level, or nil: each unknown key of the first level holding one
  named, then the keys of that level. The job registry always carries :args (nil for a job with none); a hand-built
  entry without the key is not checked."
  [sym entry args]
  (when (contains? entry :args)
    (when-let [[[path known] :as all] (seq (a/unknown (:args entry) args))]
      (let [level (vec (butlast path))
            ks (sort-by str (keep (fn [[p _]] (when (= level (vec (butlast p))) (last p))) all))
            listing (if (seq known) (str "its " (if (seq level) "keys" "args") " are " (str/join ", " (sort-by str known)))
                        "it takes no args")]
        (str sym (when (seq level) (str " " (a/path-text level))) " has no " (if (seq level) "key " "arg ")
             (str/join ", " ks) "; " listing)))))

(defn parse-form
  "The node for form (the wrapper hold is peeled off before)."
  [registry form]
  (let [head (when (and (seq? form) (seq form)) (first form))
        parts (when (seq? form) (rest form))
        n (count parts)]
    (cond
      (not (and (seq? form) (symbol? head)))
      (fail (str "a job spec is a list such as (jobs.survival.eat) or (seq ...), not " (pr-str form)) form)

      (= 'hold head)
      (fail "hold is only allowed around a whole job spec, not inside one" form)

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
                 " combinators are seq, any, repeat, until and hold")
            form)

      (> n 1) (fail (str head " takes at most one args map") form)

      (and (= 1 n) (not (map? (first parts)))) (fail (str "args of " head " must be a map") form)

      (unknown-keys-message head (get registry head) (first parts))
      (fail (unknown-keys-message head (get registry head) (first parts)) form)

      :else {:op :leaf :job head :args (second (leaf registry head (first parts)))})))

(defn peel
  "Unwrap the top-level (hold e) of form: {:form inner :hold? bool}."
  [form]
  (let [head (when (seq? form) (first form))
        parts (when (seq? form) (rest form))]
    (cond
      (= 'hold head)
      (if (= 1 (count parts))
        (assoc (peel (first parts)) :hold? true)
        (fail "hold takes exactly one job spec" form))

      :else {:form form :hold? false})))

(defn parse-spec
  "{:node node :hold? bool} for a job spec form; throws with a message naming the problem and the whole spec."
  [registry form]
  (try
    (let [{:keys [form hold?]} (peel form)]
      {:node (parse-form registry form) :hold? hold?})
    (catch :default e
      (throw (ex-info (str (ex-message e) ", in " (pr-str form)) {:form form} e)))))

(defn parse
  "The node for a job spec form that must not hold."
  [registry form]
  (let [{:keys [node hold?]} (parse-spec registry form)]
    (when hold? (fail (str "hold is not allowed here, in " (pr-str form)) form))
    node))

(defn problem
  "The message of what is wrong with form, or nil."
  [registry form]
  (try (parse-spec registry form) nil
       (catch :default e (ex-message e))))

(defn label
  "A short printable name of a node: the job symbol, or the combinator form."
  [{:keys [op job children child times]}]
  (case op
    :leaf (str job)
    :repeat (str "(repeat " (when times (str times " ")) (label child) ")")
    (str "(" (name op) " " (str/join " " (map label children)) ")")))
