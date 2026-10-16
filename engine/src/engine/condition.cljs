(ns engine.condition
  "Agent conditions: an EDN list in the look of a job expression, read (never evaluated) and compiled against the
  library the default trigger set names (engine.triggers/library): its names and literals only, so nothing an agent
  writes runs code of its own. See README.md, Conditions.

    (and (< (inventory \"bread\") 8)
         (because :far-from-home (< (distance-to (place :home)) 16)))

  compile gives {:ok true :check f}, f a fn of a ctx {:primitives :view} returning a status (engine.contract), or a
  refusal {:ok false :reason :at :message :allowed}."
  (:refer-clojure :exclude [compile])
  (:require [clojure.string :as str]
            [cljs.tools.reader.edn :as edn]
            [cljs.tools.reader.reader-types :as rt]
            [engine.triggers :as triggers]))

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
  "The operators' grammar by symbol (their fns are in the library). :args is the type every argument must have (:any
  for =), :arity an exact count, else :min and :max."
  {'and {:sig "(and status ...)" :min 1 :args :status}
   'or {:sig "(or status ...)" :min 1 :args :status}
   'not {:sig "(not status)" :arity 1 :args :status}
   '< {:sig "(< number number)" :arity 2 :args :number}
   '> {:sig "(> number number)" :arity 2 :args :number}
   '<= {:sig "(<= number number)" :arity 2 :args :number}
   '>= {:sig "(>= number number)" :arity 2 :args :number}
   '= {:sig "(= a a), a a number, string or keyword" :arity 2 :args :any}
   'because {:sig "(because keyword string? status)" :min 2 :max 3}
   'known? {:sig "(known? form)" :arity 1}})

(def comparable #{:number :string :keyword})

(def ^:dynamic *extras*
  "{sym entry} of the entries a job-side compile adds to the trigger library (the :ready/vocabulary hook): set by
  compile and allowed, so a trigger's own condition never sees them."
  nil)

(defn entry "The library entry sym names (an operator's has no :type), or nil." [sym]
  (let [e (or (get *extras* sym) (get triggers/library sym))] (when (:type e) e)))

(defn entry-sig [sym {:keys [args type]}]
  (str "(" (str/join " " (cons sym (map name args))) ") -> " (name type)))

(defn sig [sym]
  (if-let [op (get ops sym)]
    (:sig op)
    (entry-sig sym (entry sym))))

(def vocabulary
  "Every signature, operators first: what a refusal for an unknown symbol lists."
  (into (mapv :sig (vals ops)) (keep (fn [[sym e]] (when (:type e) (entry-sig sym e)))) (sort-by key triggers/library)))

(defn op-fn [sym] (:fn (or (get *extras* sym) (get triggers/library sym))))

;; ------------------------------------------------------------------ compiler

(defn literal-type [x]
  (cond
    (number? x) :number
    (string? x) :string
    (keyword? x) :keyword
    (boolean? x) :boolean))

(declare check allowed)

(defn check-args
  "The checked children of form's arguments, or the first refusal."
  [args]
  (reduce (fn [acc a]
            (let [r (check a)]
              (if (refusal? r) (reduced r) (conj acc r))))
          [] args))

(defn arity-ok? [{:keys [arity min max]} n]
  (if arity (= arity n) (and (>= n min) (or (nil? max) (<= n max)))))

(defn type-refusal [sym child expected]
  (refuse :type (:form child)
          (str (pr-str (:form child)) " is a " (name (:type child)) "; " sym " wants a " (name expected) " here")
          :allowed [(sig sym)]))

(defn typed
  "The node of (sym children...) when each child has its type in types, else the first mismatch's refusal."
  [form sym children types]
  (if-let [[c t] (first (remove (fn [[c t]] (= t (:type c))) (map vector children types)))]
    (type-refusal sym c t)
    {:form form :type (if (entry sym) (:type (entry sym)) :status) :value (apply (op-fn sym) (map :value children))}))

(defn check-known
  "known? takes an entry or a call, of any type: a literal is always known, so it is refused rather than answered."
  [form [inner]]
  (let [c (check inner)]
    (cond
      (refusal? c) c
      (literal-type inner) (refuse :type inner
                                   (str (pr-str inner) " is a literal and always known; known? wants a fact or a call here")
                                   :allowed [(sig 'known?)])
      :else {:form form :type :status :value ((op-fn 'known?) (:value c))})))

(defn check-op [form sym args]
  (let [args-type (:args (get ops sym))
        children (check-args args)
        [a b] (map :type (when-not (refusal? children) children))]
    (cond
      (refusal? children) children
      (= 'because sym) (typed form sym children (if (= 2 (count args)) [:keyword :status] [:keyword :string :status]))
      (not= :any args-type) (typed form sym children (repeat args-type))
      (and (= a b) (comparable a)) (typed form sym children [a a])
      :else (refuse :incomparable form (str "= compares two values of one type, not a " (name a) " and a " (name b))
                    :allowed [(sig sym)]))))

(defn check
  "The checked node {:form :type :value} of form, value a literal or a checker, or a refusal naming the sub-form."
  [form]
  (let [head (when (seq? form) (first form))
        args (when (seq? form) (rest form))
        lit (literal-type form)]
    (cond
      lit {:form form :type lit :value form}

      (quoted? form) (refuse :quoted form quoted-message)

      (not (and (seq? form) (symbol? head)))
      (refuse :not-a-form form
              (str (pr-str form) " is not a value or a call; facts are called like (health) and literals are"
                   " numbers, strings or keywords")
              :allowed (allowed *extras*))

      (not (or (contains? ops head) (entry head)))
      (refuse :unknown-symbol form
              (str head " is not in the condition language: no variables, arithmetic or functions of your own;"
                   " when nothing here says it, ask for a new fact")
              :allowed (allowed *extras*))

      (not (arity-ok? (or (get ops head) {:arity (count (:args (entry head)))}) (count args)))
      (refuse :arity form (str head " takes " (sig head) ", not " (count args) " argument(s)") :allowed [(sig head)])

      (= 'known? head) (check-known form args)
      (contains? ops head) (check-op form head args)
      :else (let [children (check-args args)]
              (if (refusal? children) children (typed form head children (:args (entry head))))))))

(defn allowed
  "Every signature of the library and of the extra entries (default none), as a refusal lists them."
  ([] vocabulary)
  ([extras] (into vocabulary (keep (fn [[sym e]] (when (:type e) (entry-sig sym e)))) (sort-by key extras))))

(defn status-forms []
  (filterv #(or (str/ends-with? % "status") (not (str/includes? % "->"))) (allowed *extras*)))

(defn compile*
  [form]
  (let [r (check form)]
    (cond
      (refusal? r) r
      (not= :status (:type r))
      (refuse :not-a-status form (str "a condition must be a status; " (pr-str form) " is a " (name (:type r)))
              :allowed (status-forms))
      :else {:ok true :check (:value r)})))

(defn compile
  "{:ok true :check f} for a condition form, f a fn of a ctx {:primitives :view} returning a status, or a refusal
  {:ok false :reason :at :message :allowed}. Reasons: :quoted :not-a-form :unknown-symbol :arity :type :incomparable
  :not-a-status. extras: {sym entry} added to the library (a job's checks)."
  ([form] (compile form nil))
  ([form extras]
   (binding [*extras* extras] (compile* form))))
