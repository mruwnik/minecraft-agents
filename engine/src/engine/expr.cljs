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
  (:require [clojure.string :as str]))

(def combinators #{'seq 'any 'repeat 'until 'hold})

(defn defaults
  "The default args of a registry entry: {k default} from its :args spec."
  [entry]
  (into {} (map (fn [[k spec]] [k (:default spec)])) (:args entry)))

(defn fail [msg form]
  (throw (ex-info msg {:form form})))

(defn cell
  "{:x :y :z} for a position given as [x y z] or {:x :y :z} of numbers, else nil."
  [v]
  (cond
    (and (map? v) (every? #(number? (get v %)) [:x :y :z])) (select-keys v [:x :y :z])
    (and (sequential? v) (= 3 (count v)) (every? number? v)) (zipmap [:x :y :z] v)))

(def arg-types
  "Arg :type -> [human name, predicate]; :pos and :enum are handled apart. A spec without :type is not checked."
  {:keyword ["a keyword" keyword?]
   :int ["a whole number" #(and (number? %) (== % (js/Math.round %)))]
   :number ["a number" #(and (number? %) (js/isFinite %))]
   :bool ["true or false" boolean?]
   :string ["a string" string?]
   :item ["an item name (non-empty string)" #(and (string? %) (seq %))]})

(defn type-problem
  "Why v does not fit spec's :type (with :values for :enum, :min/:max for numbers), or nil. A nil v is unset and fits."
  [spec v]
  (let [t (:type spec)
        [human ok?] (get arg-types t)
        {:keys [min max values]} spec]
    (cond
      (or (nil? v) (nil? t) (= :pos t)) nil
      (= :enum t) (when-not (some #(= v %) values) (str "one of " (pr-str (vec values))))
      (nil? ok?) (str "a known :type, but the spec says " (pr-str t))
      (not (ok? v)) human
      (and min (number? v) (< v min)) (str human " >= " min)
      (and max (number? v) (> v max)) (str human " <= " max))))

(defn normalize-positions
  "args checked against their specs' :type (see arg-types; throws naming the job, arg, expected type and the value,
  so a bad value is refused at submit rather than failing inside the job); a :pos arg becomes {:x :y :z}, a nil one is left."
  [sym entry args]
  (reduce (fn [acc [k spec]]
            (let [v (get acc k)]
              (cond
                (and (= :pos (:type spec)) (cell v)) (assoc acc k (cell v))
                (and (= :pos (:type spec)) (some? v))
                (fail (str sym " " k " must be [x y z] or {:x :y :z} of numbers, got " (pr-str v)) args)
                :else (if-let [want (type-problem spec v)]
                        (fail (str sym " " k " must be " want ", got " (pr-str v)) args)
                        acc))))
          args (:args entry)))

(defn leaf
  "[def args] for job symbol sym with args merged over its defaults; throws
  when sym is not in the registry."
  [registry sym args]
  (let [entry (get registry sym)]
    (cond
      (nil? entry) (throw (ex-info (str "unknown job " sym) {:job sym}))
      (not (and (fn? (:check entry)) (fn? (:round entry))))
      (throw (ex-info (str "job " sym " needs a check and a round") {:job sym}))
      :else [entry (normalize-positions sym entry (merge (defaults entry) args))])))

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
