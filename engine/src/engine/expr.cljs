(ns engine.expr
  "Job expressions: the EDN form of a job spec, parsed (never evaluated)
  against the job registry into a node. See README.md, Job expressions.

    (jobs.survival.eat {:min-food 6})   a leaf; args merged over the defaults
    (seq e1 e2 ...)                     children in order
    (any e1 e2 ...)                     the first child whose check passes
    (repeat e)                          e again, fresh, each time it is done
    (hold e)                            e, holding the body; top level only

  Nodes: {:op :leaf :job sym :args map}, {:op :seq|:any :children [node]},
  {:op :repeat :child node}. They are plain EDN, so they persist."
  (:require [clojure.string :as str]))

(def combinators #{'seq 'any 'repeat 'hold})

(defn defaults
  "The default args of a registry entry: {k default} from its :args spec."
  [entry]
  (into {} (map (fn [[k spec]] [k (:default spec)])) (:args entry)))

(defn leaf
  "[def args] for job symbol sym with args merged over its defaults; throws
  when sym is not in the registry."
  [registry sym args]
  (let [entry (get registry sym)]
    (cond
      (nil? entry) (throw (ex-info (str "unknown job " sym) {:job sym}))
      (not (and (fn? (:check entry)) (fn? (:round entry))))
      (throw (ex-info (str "job " sym " needs a check and a round") {:job sym}))
      :else [entry (merge (defaults entry) args)])))

(defn fail [msg form]
  (throw (ex-info msg {:form form})))

(defn parse-form
  "The node for form; top? says whether hold is allowed here."
  [registry form top?]
  (let [head (when (and (seq? form) (seq form)) (first form))
        parts (when (seq? form) (rest form))
        n (count parts)]
    (cond
      (not (and (seq? form) (symbol? head)))
      (fail (str "a job spec is a list such as (jobs.survival.eat) or (seq ...), not " (pr-str form)) form)

      (= 'hold head)
      (cond
        (not top?) (fail "hold is only allowed around a whole job spec, not inside one" form)
        (not= 1 n) (fail "hold takes exactly one job spec" form)
        :else (parse-form registry (first parts) false))

      (= 'repeat head)
      (if (= 1 n)
        {:op :repeat :child (parse-form registry (first parts) false)}
        (fail "repeat takes exactly one job spec" form))

      (#{'seq 'any} head)
      (if (pos? n)
        {:op (keyword head) :children (mapv #(parse-form registry % false) parts)}
        (fail (str head " takes at least one job spec") form))

      (not (contains? registry head))
      (fail (str "unknown job or combinator " head "; jobs are namespaces under jobs/,"
                 " combinators are seq, any, repeat and hold")
            form)

      (> n 1) (fail (str head " takes at most one args map") form)

      (and (= 1 n) (not (map? (first parts)))) (fail (str "args of " head " must be a map") form)

      :else {:op :leaf :job head :args (second (leaf registry head (first parts)))})))

(defn parse-spec
  "{:node node :hold? bool} for a job spec form; throws with a message naming
  the problem and the whole spec."
  [registry form]
  (let [hold? (and (seq? form) (= 'hold (first form)))
        node (try
               (parse-form registry form true)
               (catch :default e
                 (throw (ex-info (str (ex-message e) ", in " (pr-str form)) {:form form} e))))]
    {:node node :hold? (boolean hold?)}))

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
  [{:keys [op job children child]}]
  (case op
    :leaf (str job)
    :repeat (str "(repeat " (label child) ")")
    (str "(" (name op) " " (str/join " " (map label children)) ")")))
