(ns engine.condition
  "Trigger conditions: an EDN list in the look of a job expression, read (never
  evaluated) and walked against a fixed table of operators and facts. See
  README.md, Conditions.

    (and (< (inventory \"bread\") 8)
         (< (distance-to (place :home)) 16))

  compile validates a form once, at registration, into a node or a refusal
  {:ok false :reason :at :allowed}. evaluate walks a node against the world
  and memory: pure, with the held-for timers and the scan cache threaded
  through as state. when-fn wraps a node as a register :when with its own
  state. Values are numbers, strings, keywords, positions, booleans or
  unknown; unknown propagates and and/or are three-valued, and only
  (known? x) turns an unknown into a definite boolean."
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]
            [cljs.tools.reader.edn :as edn]
            [cljs.tools.reader.reader-types :as rt]
            [engine.condition.facts :as facts]))

(def unknown facts/unknown)

(defn unknown? [v] (= unknown v))

;; ------------------------------------------------------------------ reader

(defn refuse [reason at message & {:as more}]
  (merge {:ok false :reason reason :at at :message message} more))

(def quoted-message "drop the leading quote: a condition is data already, as a job spec is")

(defn refusal? [r] (false? (:ok r)))

(defn quoted? [form]
  (and (seq? form) (= 'quote (first form))))

(defn read-string-form [s]
  (let [r (rt/string-push-back-reader s)
        read-one #(try {:form (edn/read {:eof ::eof} r)}
                       (catch :default e {:error (ex-message e)}))
        {:keys [form error]} (read-one)]
    (cond
      error (refuse :unreadable s (str "unreadable EDN: " error))
      (= ::eof form) (refuse :unreadable s "the condition is empty")
      (not= ::eof (:form (read-one))) (refuse :trailing s "one condition form only; text follows it")
      :else {:ok true :form form})))

(defn read-condition
  "{:ok true :form form} for a condition given as a string (read as one EDN
  form) or as a form already read (from a scenario file); a refusal for a
  leading quote, unreadable text or text after the form."
  [x]
  (cond
    (and (string? x) (str/starts-with? (str/triml x) "'")) (refuse :quoted x quoted-message)
    (string? x) (let [r (read-string-form x)]
                  (if (refusal? r) r (read-condition (:form r))))
    (quoted? x) (refuse :quoted x quoted-message)
    :else {:ok true :form x}))

;; ------------------------------------------------------------------ the vocabulary

(def ops
  "Operators by symbol. :args is the type every argument must have (:any for
  =), :arity an exact count or :min a least one."
  {'and {:sig "(and boolean ...)" :min 1 :args :boolean}
   'or {:sig "(or boolean ...)" :min 1 :args :boolean}
   'not {:sig "(not boolean)" :arity 1 :args :boolean}
   '< {:sig "(< number number)" :arity 2 :args :number}
   '> {:sig "(> number number)" :arity 2 :args :number}
   '<= {:sig "(<= number number)" :arity 2 :args :number}
   '>= {:sig "(>= number number)" :arity 2 :args :number}
   '= {:sig "(= a a), a a number, string, keyword or boolean" :arity 2 :args :any}
   'held-for {:sig "(held-for seconds boolean)" :arity 2}
   'known? {:sig "(known? form)" :arity 1}})

(def comparable #{:number :string :keyword :boolean})

(defn fact-sig [sym {:keys [args type]}]
  (str "(" (str/join " " (cons sym (map name args))) ") -> " (name type)))

(defn sig [sym]
  (if-let [op (get ops sym)]
    (:sig op)
    (fact-sig sym (get facts/table sym))))

(def vocabulary
  "Every signature, operators first: what a refusal for an unknown symbol lists."
  (into (mapv :sig (vals ops)) (map (fn [[sym f]] (fact-sig sym f))) (sort-by key facts/table)))

;; ------------------------------------------------------------------ validator

(defn literal-type [x]
  (cond
    (number? x) :number
    (string? x) :string
    (keyword? x) :keyword
    (boolean? x) :boolean))

(declare check)

(defn check-args
  "The checked children of form's arguments, or the first refusal."
  [args path]
  (reduce (fn [acc [i a]]
            (let [r (check a (conj path i))]
              (if (refusal? r) (reduced r) (conj acc r))))
          [] (map-indexed vector args)))

(defn arity-ok? [{:keys [arity min]} n]
  (if arity (= arity n) (>= n min)))

(defn type-refusal [sym child expected]
  (refuse :type (:form child)
          (str (pr-str (:form child)) " is a " (name (:type child)) "; " sym " wants a " (name expected) " here")
          :allowed [(sig sym)]))

(defn check-held-for [form [secs inner] path]
  (cond
    (not (and (number? secs) (>= secs 0)))
    (refuse :bad-duration form "held-for takes a literal number of seconds (0 or more), then a boolean"
            :allowed [(sig 'held-for)])

    :else
    (let [c (check inner (conj path 1))]
      (cond
        (refusal? c) c
        (not= :boolean (:type c)) (type-refusal 'held-for c :boolean)
        :else {:op :held-for :form form :type :boolean :ms (* 1000 secs) :path path :children [c]}))))

(defn check-known
  "known? takes a fact or a call, of any type: a literal is always known, so
  it is refused rather than answered."
  [form [inner] path]
  (let [c (check inner (conj path 0))]
    (cond
      (refusal? c) c
      (= :literal (:op c)) (refuse :type inner
                                   (str (pr-str inner) " is a literal and always known; known? wants a fact or a call here")
                                   :allowed [(sig 'known?)])
      :else {:op :known? :form form :type :boolean :children [c]})))

(defn check-op [form sym args path]
  (let [op (get ops sym)
        args-type (:args op)
        children (check-args args path)]
    (cond
      (refusal? children) children

      (= :any args-type)
      (let [[a b] (map :type children)]
        (if (and (= a b) (comparable a))
          {:op sym :form form :type :boolean :children children}
          (refuse :incomparable form (str "= compares two values of one type, not a " (name a) " and a " (name b))
                  :allowed [(:sig op)])))

      :else
      (if-let [bad (first (remove #(= args-type (:type %)) children))]
        (type-refusal sym bad args-type)
        {:op sym :form form :type :boolean :children children}))))

(defn check-fact [form sym args path]
  (let [{:keys [type] :as fact} (get facts/table sym)
        children (check-args args path)
        mismatch (when-not (refusal? children)
                   (first (remove (fn [[c t]] (= t (:type c))) (map vector children (:args fact)))))]
    (cond
      (refusal? children) children
      mismatch (type-refusal sym (first mismatch) (second mismatch))
      :else {:op :fact :fact sym :form form :type type :children children})))

(defn check
  "The checked node of form at path, or a refusal naming the sub-form."
  [form path]
  (let [head (when (seq? form) (first form))
        args (when (seq? form) (rest form))
        lit (literal-type form)]
    (cond
      lit {:op :literal :form form :type lit :value form}

      (quoted? form) (refuse :quoted form quoted-message)

      (not (and (seq? form) (symbol? head)))
      (refuse :not-a-form form
              (str (pr-str form) " is not a value or a call; facts are called like (health) and literals are"
                   " numbers, strings, keywords, true or false")
              :allowed vocabulary)

      (not (or (contains? ops head) (contains? facts/table head)))
      (refuse :unknown-symbol form
              (str head " is not in the condition language: no variables, arithmetic or functions of your own;"
                   " when nothing here says it, ask for a new fact")
              :allowed vocabulary)

      (not (arity-ok? (or (get ops head) {:arity (count (:args (get facts/table head)))}) (count args)))
      (refuse :arity form (str head " takes " (sig head) ", not " (count args) " argument(s)") :allowed [(sig head)])

      (= 'held-for head) (check-held-for form args path)
      (= 'known? head) (check-known form args path)
      (contains? ops head) (check-op form head args path)
      :else (check-fact form head args path))))

(def boolean-forms
  (filterv #(or (str/ends-with? % "boolean") (not (str/includes? % "->"))) vocabulary))

(defn compile
  "{:ok true :node node} for a condition form, or a refusal {:ok false
  :reason :at :message :allowed}. Reasons: :quoted :not-a-form
  :unknown-symbol :arity :type :incomparable :bad-duration :not-boolean."
  [form]
  (let [r (check form [])]
    (cond
      (refusal? r) r
      (not= :boolean (:type r))
      (refuse :not-boolean form (str "a condition must be true or false; " (pr-str form) " is a " (name (:type r)))
              :allowed boolean-forms)
      :else {:ok true :node r})))

;; ------------------------------------------------------------------ evaluator

(defn k-and [vs]
  (cond (some false? vs) false (some unknown? vs) unknown :else true))

(defn k-or [vs]
  (cond (some true? vs) true (some unknown? vs) unknown :else false))

(def compare-fns {'< < '> > '<= <= '>= >= '= =})

(defn combine [op vs]
  (cond
    (= 'and op) (k-and vs)
    (= 'or op) (k-or vs)
    (some unknown? vs) unknown
    (= 'not op) (not (first vs))
    :else (apply (compare-fns op) vs)))

(defn read-fact
  "[value cache'] of fact sym with arg values vs: unknown args give unknown
  unread; a :scan fact is served from cache while younger than :refresh-s."
  [sym vs env now cache cache']
  (let [{:keys [read cost refresh-s]} (get facts/table sym)
        k [sym vs]
        hit (get cache k)]
    (cond
      (some unknown? vs) [unknown cache']
      (not= :scan cost) [(apply read env vs) cache']
      (and hit (< (- now (:t hit)) (* 1000 refresh-s))) [(:value hit) (assoc cache' k hit)]
      :else (let [v (apply read env vs)] [v (assoc cache' k {:t now :value v})]))))

(defn walk
  "[tree state'] for node: tree is {:form :value :children [tree]}. Every
  child is evaluated, so held-for timers do not depend on their siblings.
  state' holds only the timers and cache entries this walk used."
  [node env now state state']
  (let [[kids st] (reduce (fn [[ts st] c] (let [[t st2] (walk c env now state st)] [(conj ts t) st2]))
                          [[] state'] (:children node))
        vs (mapv :value kids)
        done (fn [v st] [{:form (:form node) :value v :children kids} st])]
    (case (:op node)
      :literal (done (:value node) st)
      :fact (let [[v cache] (read-fact (:fact node) vs env now (:cache state) (:cache st))]
              (done v (assoc st :cache cache)))
      :held-for (let [inner (first vs)
                      since (get-in state [:since (:path node)] now)]
                  (cond
                    (unknown? inner) (done unknown st)
                    (false? inner) (done false st)
                    :else (let [remaining-ms (max 0 (- (+ since (:ms node)) now))
                                value (zero? remaining-ms)]
                            [{:form (:form node) :value value :children kids :remaining-ms remaining-ms}
                             (assoc-in st [:since (:path node)] since)])))
      :known? (done (not (unknown? (first vs))) st)
      (done (combine (:op node) vs) st))))

(defn run [node env state]
  (walk node env (or (:now env) (:now (:memory env))) state {:since {} :cache {}}))

(defn evaluate
  "{:value v :state state'} of a compiled node now. env is {:world primitives
  :memory view} plus an optional :now (ms; default the view's :now). state is
  {} at first, then the :state of the previous call for the same instance."
  [node env state]
  (let [[tree st] (run node env state)]
    {:value (:value tree) :state st}))

(defn explain
  "Every sub-term of node with its value now, depth first, root first:
  [{:form f :value v} ...]. Active held-for terms also include :remaining-ms.
  It reads state but does not advance it."
  [node env state]
  (->> (tree-seq (comp seq :children) :children (first (run node env state)))
       (mapv #(select-keys % [:form :value :remaining-ms]))))

(defn when-fn
  "A register :when, (fn [world memory args & _]) -> boolean, for a compiled
  node, with its own held-for timers and scan cache. Make one per registered
  trigger instance. It holds only on a definite true."
  [node]
  (let [state (atom {})]
    (fn [world memory _args & _]
      (let [{:keys [value] :as r} (evaluate node {:world world :memory memory} @state)]
        (reset! state (:state r))
        (true? value)))))
