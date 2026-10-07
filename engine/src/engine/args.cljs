(ns engine.args
  "Job args and settings as cljs.spec. A job declares (defargs args {k entry}); a namespace's tuning numbers
  (defargs settings {::k entry}). An entry is {:doc :default :spec pred}, or a group {:doc :keys {k entry}} (a nested
  map of args). pred is a predicate of this namespace (int-in, item?, coll-of ...), a set of the values allowed, a
  core predicate (boolean?, string?, keyword?, map? ...) or ::pos (a position, given as [x y z] or {:x :y :z}, conformed
  to {:x :y :z}). Each pred knows the text a refusal says it wants (want). See README.md, Jobs.

  defargs registers one spec per leaf (nil allowed: unset) and one s/keys per map level: a job's args under
  :<job-ns>/args, its leaf k under :<job-ns>.args/k, a group g under :<job-ns>.args/g, a leaf of g under
  :<job-ns>.args.g/k. A setting registers under its own key (nil not allowed). The var keeps the data map with
  each :spec as its quoted form, for the catalog, the dashboard and the defaults."
  (:require-macros [engine.args])
  (:require [cljs.spec.alpha :as s]
            [clojure.string :as str]))

(defonce wants (atom {}))

(def core-wants
  {boolean? "true or false" string? "a string" keyword? "a keyword" symbol? "a symbol" map? "a map"
   number? "a number" false? "false" true? "true"})

(defn want
  "The text a refusal says pred wants, or nil when pred has none."
  [pred]
  (cond
    (set? pred) (str "one of " (str/join ", " (map pr-str (sort-by str pred))))
    (= ::pos pred) "[x y z] or {:x :y :z} of numbers"
    :else (or (:want (meta pred)) (get core-wants pred))))

(defn want!
  "want of pred; throws when it has none (a pred built from one without a want text is a load-time error)."
  [pred]
  (or (want pred) (throw (ex-info (str "no want text for spec " (pr-str pred)) {:pred pred}))))

(defn pred
  "f, a one-argument predicate, carrying the text a refusal says it wants."
  [want-text f]
  (with-meta f {:want want-text}))

(defn ok?
  "Does v fit pred (a fn or a set)? A set holding false or nil cannot say so: use or-of with false?."
  [pred v]
  (boolean (if (keyword? pred) (s/valid? pred v) (pred v))))

;; ------------------------------------------------------------------ leaf predicates

(defn finite? [v] (and (number? v) (js/isFinite v)))

(defn whole? [v] (and (finite? v) (== v (js/Math.round v))))

(defn bounds-text [lo hi]
  (cond (and lo hi) (str " from " lo " to " hi) lo (str " >= " lo) hi (str " <= " hi) :else ""))

(defn in-bounds? [lo hi v] (and (or (nil? lo) (>= v lo)) (or (nil? hi) (<= v hi))))

(defn int-in
  "A whole number (3.0 counts) from lo to hi, inclusive; a nil bound is open."
  [lo hi]
  (pred (str "a whole number" (bounds-text lo hi)) #(and (whole? %) (in-bounds? lo hi %))))

(defn num-in
  "A finite number from lo to hi, inclusive; a nil bound is open."
  [lo hi]
  (pred (str "a number" (bounds-text lo hi)) #(and (finite? %) (in-bounds? lo hi %))))

(def pos-num? (pred "a number above 0" #(and (finite? %) (pos? %))))

(def item? (pred "an item name (non-empty string)" #(and (string? %) (not (str/blank? %)))))

(def name? (pred "a name (non-empty string)" #(and (string? %) (not (str/blank? %)))))

(defn cell
  "{:x :y :z} for a position given as [x y z] or {:x :y :z} of numbers, else nil."
  [v]
  (cond
    (and (map? v) (every? #(number? (get v %)) [:x :y :z])) (select-keys v [:x :y :z])
    (and (sequential? v) (= 3 (count v)) (every? number? v)) (zipmap [:x :y :z] v)))

(s/def ::pos (s/conformer #(or (cell %) ::s/invalid)))

(def position? (pred "[x y z] or {:x :y :z} of numbers" #(some? (cell %))))

(def point? (pred "{:x :y :z} of numbers" #(and (map? %) (every? (fn [k] (number? (get % k))) [:x :y :z]))))

(defn coll-of
  "A list, vector or set (not a map) of values fitting p."
  [p]
  (pred (str "a list, vector or set of " (want! p)) #(and (coll? %) (not (map? %)) (every? (fn [x] (ok? p x)) %))))

(defn set-of
  "A set of values fitting p."
  [p]
  (pred (str "a set of " (want! p)) #(and (set? %) (every? (fn [x] (ok? p x)) %))))

(defn map-of
  "A map whose keys fit kp and values fit vp."
  [kp vp]
  (pred (str "a map of " (want! kp) " to " (want! vp))
        #(and (map? %) (every? (fn [[k v]] (and (ok? kp k) (ok? vp v))) %))))

(defn map-with
  "A map holding each key of m with a value fitting m's pred for it; other keys are free."
  [m]
  (pred (str "a map {" (str/join ", " (map (fn [[k p]] (str k " " (want! p))) m)) "}")
        #(and (map? %) (every? (fn [[k p]] (ok? p (get % k))) m))))

(defn or-of
  "A value fitting any of preds (no tag: the value conforms to itself)."
  [& preds]
  (pred (str/join " or " (map want! preds)) #(boolean (some (fn [p] (ok? p %)) preds))))

(defn valid-by
  "A value problem (a fn of the value, returning why it is bad or nil) finds no fault with; one problem throws on is bad."
  [want-text problem]
  (pred want-text #(try (nil? (problem %)) (catch :default _ false))))

(def box? (pred "a box {:min {:x :y :z} :max {:x :y :z}} of numbers" #(and (map? %) (ok? point? (:min %)) (ok? point? (:max %)))))

;; ------------------------------------------------------------------ registration (defargs expands to these)

(defn leaf!
  "Register spec key k for pred (form its quoted source), nil allowed when nilable?."
  [k form pred nilable?]
  (swap! wants assoc k (want! pred))
  (s/def-impl k form (if nilable? (s/nilable-impl form pred nil) pred)))

(defn keys-spec
  "s/keys :opt-un over the spec keys ks, as a value (the s/keys macro, without its per-key code)."
  [ks]
  (s/map-spec-impl {:req nil :opt nil :req-un nil :opt-un ks :req-keys [] :req-specs []
                    :opt-keys (mapv #(keyword (name %)) ks) :opt-specs ks
                    :pred-forms '[cljs.core/map?] :pred-exprs [map?] :keys-pred map? :gfn nil}))

(defn level!
  "Register spec key k for a map level holding the spec keys ks (all optional); nil allowed when nilable? (a group)."
  [k ks nilable?]
  (let [form (list 'keys :opt-un ks)]
    (s/def-impl k form (if nilable? (s/nilable-impl form (keys-spec ks) nil) (keys-spec ks)))))

(defn declare-level!
  "Register the entries of data under spec namespace level-ns; the data map back with each :spec as its :form."
  [level-ns data nilable?]
  (into {} (map (fn [[k e]]
                  (let [lk (if nilable? (keyword level-ns (name k)) k)]
                    (if-let [ks (:keys e)]
                      (let [sub (declare-level! (str level-ns "." (name k)) ks true)]
                        (level! lk (mapv #(keyword (str level-ns "." (name k)) (name %)) (keys sub)) true)
                        [k (assoc e :keys sub)])
                      (do (leaf! lk (:form e (:spec e)) (:spec e) nilable?)
                          [k (-> e (assoc :spec (:form e (:spec e))) (dissoc :form))])))))
        data))

(defn declare!
  "Register the specs of job ns-sym's args data ({k {:doc :default :spec pred :form quoted-source}}, groups {:doc
  :keys {...}}): one per leaf, one s/keys per level. The data map back, each :spec its :form. defargs calls it; a test
  may too, for a hand-built job."
  [ns-sym data]
  (let [d (declare-level! (str ns-sym ".args") data true)]
    (level! (keyword (str ns-sym) "args") (mapv #(keyword (str ns-sym ".args") (name %)) (keys d)) false)
    d))

(defn declare-settings!
  "Register each setting of data ({::k {:doc :default :spec pred :form quoted-source}}) under its own key, nil not
  allowed. The data map back, each :spec its :form."
  [data]
  (declare-level! nil data false))

;; ------------------------------------------------------------------ reading a data map

(defn args-key "The spec key of job ns-sym's whole args." [ns-sym] (keyword (str ns-sym) "args"))

(defn level-ns "The namespace of the spec keys at path (a vector of group keys) under job ns-sym."
  [ns-sym path]
  (str/join "." (cons (str ns-sym ".args") (map name path))))

(defn defaults
  "{k default} of a data map; a group's default is the map of its keys' defaults."
  [data]
  (into {} (map (fn [[k e]] [k (if (:keys e) (defaults (:keys e)) (:default e))])) data))

(defn merge-args
  "layers (maps of args) merged in order over each other, later wins; a declared group merges key by key (one level
  per group), any other value replaces."
  [data & layers]
  (reduce (fn [acc layer]
            (reduce-kv (fn [acc k v]
                         (let [ks (:keys (get data k))]
                           (if (and ks (map? v) (map? (get acc k)))
                             (assoc acc k (merge-args ks (get acc k) v))
                             (assoc acc k v))))
                       acc (or layer {})))
          {} layers))

(defn unknown
  "[[path known-keys] ...]: each key of args that data does not declare, at any level, as its path, with the keys
  that level declares."
  ([data args] (unknown data args []))
  ([data args path]
   (mapcat (fn [[k v]]
             (let [e (get data k)]
               (cond
                 (not (contains? data k)) [[(conj path k) (keys data)]]
                 (and (:keys e) (map? v)) (unknown (:keys e) v (conj path k))
                 :else nil)))
           (sort-by (comp str key) args))))

(defn path-text [path] (str/join " " (map str path)))

(defn first-problem
  "{:path :want :value} of the first value of args (by path) its spec refuses, or nil."
  ([ns-sym data args] (first-problem ns-sym data args []))
  ([ns-sym data args path]
   (some (fn [[k v]]
           (let [e (get data k)
                 lk (keyword (level-ns ns-sym path) (name k))]
             (cond
               (nil? e) nil
               (:keys e) (if (or (nil? v) (map? v))
                           (first-problem ns-sym (:keys e) v (conj path k))
                           {:path (conj path k) :want "a map" :value v})
               (and (s/get-spec lk) (not (s/valid? lk v))) {:path (conj path k) :want (get @wants lk) :value v})))
         (sort-by (comp str key) args))))

(defn conform
  "args of job ns-sym (merged with the defaults) conformed by its spec (positions become {:x :y :z}); throws naming
  the job, the key path, what it wants and the value. A job without args data (a stub) or a registered spec gets args
  back as they are."
  [ns-sym data args]
  (let [k (args-key ns-sym)]
    (if-not (and (seq data) (s/get-spec k))
      args
      (let [c (s/conform k args)]
        (if-not (s/invalid? c)
          c
          (let [{:keys [path want value]} (or (first-problem ns-sym data args) {:path [] :want "a map" :value args})]
            (throw (ex-info (str ns-sym " " (path-text path) " must be " want ", got " (pr-str value))
                            {:job ns-sym :path path :value value}))))))))

(defn problem
  "Why v does not fit the spec registered under setting key k, or nil. nil is never a value."
  [k v]
  (cond
    (nil? v) "a value, not nil"
    (and (s/get-spec k) (not (s/valid? k v))) (get @wants k)))

(defn described
  "The data map of job ns-sym with each :spec given as the text of what it wants (for the catalog)."
  ([ns-sym data] (described ns-sym data []))
  ([ns-sym data path]
   (into {} (map (fn [[k e]]
                   [k (if (:keys e)
                        (update e :keys #(described ns-sym % (conj path k)))
                        (cond-> e (contains? e :spec) (assoc :spec (get @wants (keyword (level-ns ns-sym path) (name k))))))]))
         data)))
