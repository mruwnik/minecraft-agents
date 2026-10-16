(ns engine.args
  "Compile time: defargs, the declaration of a job's args or a namespace's settings. See engine.args (cljs).")

(def shared
  "Args many jobs declare alike: an entry written :shared stands for the one here."
  {:ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec 'boolean? :default false}})

(defn expand
  "data with each :shared entry replaced by its entry in shared; groups recurse."
  [data]
  (into {} (map (fn [[k e]]
                  [k (cond
                       (= :shared e) (or (get shared k) (throw (ex-info (str "defargs " k ": no shared arg of that name") {:key k})))
                       (:keys e) (update e :keys expand)
                       :else e)]))
        data))

(defn with-forms
  "data with each entry's :form, the quoted source of its :spec, added; groups recurse."
  [data]
  (into {} (map (fn [[k e]]
                  (when-not (map? e)
                    (throw (ex-info (str "defargs " k ": an entry is a literal map {:doc :default :spec}, found " (pr-str e)) {:key k})))
                  (when-not (or (:keys e) (contains? e :spec))
                    (throw (ex-info (str "defargs " k ": no :spec") {:key k})))
                  [k (if (:keys e)
                       (update e :keys with-forms)
                       (assoc e :form (list 'quote (:spec e))))]))
        (expand data)))

(defmacro defargs
  "(defargs args doc? {:k {:doc .. :default .. :spec pred} :g {:doc .. :keys {...}}}): a job's args (unqualified keys), or
  (defargs settings doc? {::k {...}}): a namespace's settings (qualified keys, no groups). Registers the specs and defs
  name as the data map, each :spec its quoted source. An entry written :shared is the one in engine.args/shared."
  [name & doc+data]
  (let [data (last doc+data)
        doc (when (next doc+data) (first doc+data))
        ns-name (str (-> &env :ns :name))
        ks (keys data)
        settings? (and (seq ks) (every? qualified-keyword? ks))]
    (when-not (or settings? (every? simple-keyword? ks))
      (throw (ex-info (str "defargs " name ": keys are all unqualified (job args) or all qualified (settings)") {:name name})))
    (when (and settings? (some :keys (vals data)))
      (throw (ex-info (str "defargs " name ": a setting has no :keys group") {:name name})))
    (when (and settings? (some #(= (keyword ns-name "args") %) ks))
      (throw (ex-info (str "defargs " name ": a setting named args collides with the job's args spec") {:name name})))
    `(def ~name ~@(when doc [doc])
       ~(if settings?
          `(engine.args/declare-settings! ~(with-forms data))
          `(engine.args/declare! '~(symbol ns-name) ~(with-forms data))))))
