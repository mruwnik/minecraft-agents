(ns engine.args
  "Compile time: defargs, the declaration of a job's args or a namespace's settings. See engine.args (cljs).")

(def shared
  "Args many jobs declare alike (mine and dig-for share the dig ones; their specs use a/ and heading-name, which both have): an entry written :shared stands for the one here."
  {:ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec 'boolean? :default false}
   :block {:doc "narrow the target to this block (an ore with its deepslate kind); nil: every block that drops the item" :spec 'a/name? :default nil}
   :item {:doc "the item to carry, or several items (any of them counts toward :count); the blocks come from the drop table. Required unless :block is given, which then names the item by what it drops" :spec '(a/or-of a/item? (a/coll-of a/item?)) :default nil}
   :count {:doc "how many to carry at least (carried at the start count)" :spec '(a/int-in 0 nil) :default 8}
   :radius {:doc "seen blocks within this many blocks of the body count" :spec '(a/num-in 0 nil) :default 16}
   :center {:doc "the centre of the area the job works, a cell [x y z] or {:x :y :z}; nil: where the body stands when the job first runs (jobs.lib.area)" :spec 'a/position? :default nil}
   :wet {:doc "dig blocks that touch water, and tunnel beside water" :spec 'boolean? :default false}
   :heading {:doc "the strip tunnel's heading, over :direction: north, south, east or west (n/s/e/w), or a cell [x y z] (the dominant axis from the start toward it); nil: :direction" :spec '(a/pred "a heading name or a cell [x y z]" #(or (and (or (string? %) (keyword? %)) (heading-name %)) (and (sequential? %) (= 3 (count %)) (every? number? %)))) :default nil}
   :direction {:doc "the strip tunnel's heading: north, south, east or west (n/s/e/w); nil: the way the body faces when the job starts" :spec '(a/pred "north, south, east or west (n/s/e/w), a string or keyword" #(and (or (string? %) (keyword? %)) (heading-name %))) :default nil}
   :tunnel-length {:doc "the most blocks the strip tunnel runs in this job, at the body's level (the :strip-tunnel way)" :spec '(a/int-in 0 nil) :default 32}
   :level {:doc "the feet height the strip tunnel runs at (a whole number, or a cell [x y z] / {:x :y :z} whose y it takes): with no target seen the body first stairs down to it (jobs.access.stair) when higher; nil: the ore's level (setting mine.descent/ore-levels), none for other blocks; false: where the body stands"
           :spec '(a/pred "a whole number, a cell [x y z] or {:x :y :z}, nil or false" #(or (nil? %) (false? %) (integer? %) (and (sequential? %) (= 3 (count %)) (integer? (second %))) (and (map? %) (integer? (:y %))))) :default nil}
   :shaft {:doc "a mineshaft plan id for :descend-shaft to use; nil: the nearest usable one within reach, when a descent is due" :spec '(a/pred "a shaft plan id or nil" #(or (nil? %) (string? %))) :default nil}
   :descend-limit {:doc "the most steps of stair down through soil to find stone, when the block is stone-type and none is in sight (the :descend-stair way)" :spec '(a/int-in 0 nil) :default 12}
   :torches {:doc "hang torches where jobs.lib.torch-plan says a dug cell would stay dark; false: none" :spec 'boolean? :default true}
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent :falling-block), a set or list; the lava and :wet rules above still hold"
            :spec '(a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent :falling-block}}})

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
