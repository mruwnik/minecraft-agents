(ns engine.expr
  "Job expressions: the EDN form of a job spec, parsed (never evaluated)
  against the job registry into a node. See README.md, Job expressions.

    (jobs.survival.eat {:until 18})   a leaf; args merged over the defaults
    (seq e1 e2 ...)                     children in order
    (any e1 e2 ...)                     the first child whose check passes
    (repeat e)                          e again, fresh, each time it is done
    (hold e)                            e, holding the body; top level only
    (backoff cfg e)                     e with its own backoff config (a map, or
                                        false for none); top level only, and it
                                        nests with hold in either order

  Nodes: {:op :leaf :job sym :args map}, {:op :seq|:any :children [node]},
  {:op :repeat :child node}. They are plain EDN, so they persist."
  (:require [clojure.string :as str]
            [engine.backoff :as backoff]))

(def combinators #{'seq 'any 'repeat 'hold 'backoff})

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

(defn unknown-keys-message
  "Why args holds keys the job does not declare, or nil: each unknown key named, then the known ones.
  The job registry always carries :args (nil for a job with none); a hand-built entry without the key is not checked."
  [sym entry args]
  (let [known (set (keys (:args entry)))
        unknown (sort-by str (remove known (keys args)))
        listing (if (seq known) (str "its args are " (str/join ", " (sort-by str known))) "it takes no args")]
    (when (and (contains? entry :args) (seq unknown))
      (str sym " has no arg " (str/join ", " unknown) "; " listing))))

(defn parse-form
  "The node for form (the wrappers hold and backoff are peeled off before)."
  [registry form]
  (let [head (when (and (seq? form) (seq form)) (first form))
        parts (when (seq? form) (rest form))
        n (count parts)]
    (cond
      (not (and (seq? form) (symbol? head)))
      (fail (str "a job spec is a list such as (jobs.survival.eat) or (seq ...), not " (pr-str form)) form)

      (= 'hold head)
      (fail "hold is only allowed around a whole job spec, not inside one" form)

      (= 'backoff head)
      (fail "backoff is only allowed around a whole job spec, not inside one" form)

      (= 'repeat head)
      (if (= 1 n)
        {:op :repeat :child (parse-form registry (first parts))}
        (fail "repeat takes exactly one job spec" form))

      (#{'seq 'any} head)
      (if (pos? n)
        {:op (keyword head) :children (mapv #(parse-form registry %) parts)}
        (fail (str head " takes at least one job spec") form))

      (not (contains? registry head))
      (fail (str "unknown job or combinator " head "; jobs are namespaces under jobs/,"
                 " combinators are seq, any, repeat and hold")
            form)

      (> n 1) (fail (str head " takes at most one args map") form)

      (and (= 1 n) (not (map? (first parts)))) (fail (str "args of " head " must be a map") form)

      (unknown-keys-message head (get registry head) (first parts))
      (fail (unknown-keys-message head (get registry head) (first parts)) form)

      :else {:op :leaf :job head :args (second (leaf registry head (first parts)))})))

(defn peel
  "Unwrap the top-level (hold e) and (backoff cfg e) wrappers of form, in any
  order: {:form inner :hold? bool} plus :backoff (a map or false) when given."
  [form]
  (let [head (when (seq? form) (first form))
        parts (when (seq? form) (rest form))]
    (cond
      (= 'hold head)
      (if (= 1 (count parts))
        (assoc (peel (first parts)) :hold? true)
        (fail "hold takes exactly one job spec" form))

      (= 'backoff head)
      (let [[cfg inner] parts]
        (when-not (and (= 2 (count parts)) (or (map? cfg) (false? cfg)))
          (fail "backoff takes a config map (or false) and one job spec" form))
        (backoff/validate! cfg)
        (assoc (peel inner) :backoff cfg))

      :else {:form form :hold? false})))

(defn parse-spec
  "{:node node :hold? bool} for a job spec form, plus :backoff when it is
  wrapped in (backoff cfg ...); throws with a message naming the problem and
  the whole spec."
  [registry form]
  (try
    (let [{:keys [form hold?] :as peeled} (peel form)]
      (-> peeled
          (select-keys [:backoff])
          (assoc :node (parse-form registry form) :hold? hold?)))
    (catch :default e
      (throw (ex-info (str (ex-message e) ", in " (pr-str form)) {:form form} e)))))

(defn parse
  "The node for a job spec form that must not hold."
  [registry form]
  (let [{:keys [node hold? backoff]} (parse-spec registry form)]
    (when hold? (fail (str "hold is not allowed here, in " (pr-str form)) form))
    (when (some? backoff) (fail (str "backoff is not allowed here, in " (pr-str form)) form))
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
