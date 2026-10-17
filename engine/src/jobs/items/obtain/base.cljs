(ns jobs.items.obtain.base
  "Settings, argument helpers and the allowed ways that every part of items.obtain reads."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.storage :as storage]))

(a/defargs settings
  {:jobs.items.obtain.base/max-inspections {:default 4 :doc "Looks at a place before the search for it gives up." :spec (a/int-in 0 nil)}
   :jobs.items.obtain.base/craft-radius {:default 32 :doc "Blocks within which a crafting table is looked for." :spec (a/int-in 0 nil)}
   :jobs.items.obtain.base/seen-radius {:default 48 :doc "Blocks within which a seen raw block (ore, dirt, leaves) counts as a source; the gather gets the same radius." :spec (a/int-in 0 64)}
   :jobs.items.obtain.base/sheep-radius {:default 24 :doc "Blocks within which sheep are looked for when wool is gathered." :spec (a/num-in 0 nil)}
   :jobs.items.obtain.base/explore {:default {:max-distance 64 :max-legs 12 :spend {:job-time 300}} :doc "jobs.explore.search options of a search for a raw need (:explore true)." :spec map?}})
(defn max-inspections [] (settings/get settings :jobs.items.obtain.base/max-inspections))
(defn sheep-radius [] (settings/get settings :jobs.items.obtain.base/sheep-radius))
(defn seen-radius [] (settings/get settings :jobs.items.obtain.base/seen-radius))
(defn craft-radius [] (settings/get settings :jobs.items.obtain.base/craft-radius))
(defn explore-setting [] (settings/get settings :jobs.items.obtain.base/explore))

(defn names-of [{:keys [item any-of]}]
  (vec (if item [item] any-of)))

(defn wanted-fields
  "{:item name} or {:any-of [..]} of the args, for reasons and results."
  [a]
  (into {} (remove (comp nil? val)) (select-keys a [:item :any-of])))

(defn args-error [a]
  (let [names (names-of a)]
    (cond
      (not (and (seq names) (every? string? names))) (str "needs :item, or :any-of [names], got :item " (pr-str (:item a)) " :any-of " (pr-str (:any-of a)))
      (not (and (int? (:count a)) (pos? (:count a)))) (str ":count is a positive whole number, got " (pr-str (:count a))))))

(defn carried-of [inv names]
  (reduce + 0 (map #(storage/carried inv %) names)))

(def all-ways #{:chest :craft :smelt :gather})

(defn ways-of
  "The ways the call allows: the frozen call's; before it starts, the args' :ways, else all."
  [c]
  (set (or (:ways (ctx/frozen-call c)) (:ways (:args c)) all-ways)))

(defn way? [c k] (contains? (ways-of c) k))

(defn makes?
  "Whether a chain may be made: a craft or a smelt step is allowed."
  [c]
  (or (way? c :craft) (way? c :smelt)))

(defn step-way [step] (if (= :smelt (:op step)) :smelt :craft))

(defn plan-allowed?
  "Whether every step of a plan is a way the call allows."
  [c plan]
  (every? #(way? c (step-way %)) (:steps plan)))

(defn old-how
  "The ways as the :how sources of an old job's fetch option."
  [c]
  (set (filter #{:chest :craft :gather} (ways-of c))))

(defn holds [items names]
  (first (filter #(pos? (get items % 0)) names)))
(defn tried! [c k v] (ctx/update-mem! c assoc-in [:tried k] v) :again)
