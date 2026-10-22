(ns jobs.items.obtain.base
  "Settings, argument helpers and the allowed ways that every part of items.obtain reads."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(a/defargs settings
  {:jobs.items.obtain.base/max-inspections {:default 4 :doc "Looks at a place before the search for it gives up." :spec (a/int-in 0 nil)}
   :jobs.items.obtain.base/craft-radius {:default 32 :doc "Blocks within which a crafting table is looked for." :spec (a/int-in 0 nil)}
   :jobs.items.obtain.base/seen-radius {:default 48 :doc "Blocks within which a seen raw block (ore, dirt, leaves) counts as a source; the gather gets the same radius." :spec (a/int-in 0 64)}
   :jobs.items.obtain.base/animal-radius {:default 24 :doc "Blocks within which animals (sheep for wool, cow, chicken, spider, skeleton for their drops) are looked for." :spec (a/num-in 0 nil)}
   :jobs.items.obtain.base/near-chest {:default 10 :doc "Blocks within which a chest is worth the walk even when a craft from what is carried would do." :spec (a/num-in 0 nil)}
   :jobs.items.obtain.base/far-chest {:default 160 :doc "Blocks beyond which a remembered chest is not walked to while a craft or gather way could get the item instead (with only the chest way it is walked to however far)." :spec (a/num-in 0 nil)}
   :jobs.items.obtain.base/sapling-leaf-limit {:default 60 :doc "Leaves broken for one sapling at most (a drop is about 1 in 20); one run, no retries." :spec (a/int-in 1 nil)}
   :jobs.items.obtain.base/search-scan-radius {:default 16 :doc "How far round the body each look of a search for a raw need reaches." :spec (a/num-in 0 nil)}
   :jobs.items.obtain.base/craft-step-s {:default 5 :doc "Guess of the seconds one craft step takes, walks included (the quote)." :spec (a/num-in 0 nil)}
   :jobs.items.obtain.base/smelt-item-s {:default 15 :doc "Guess of the seconds one smelt step takes per item, the furnace's 10 s and its fetches (the quote)." :spec (a/num-in 0 nil)}
   :jobs.items.obtain.base/gather-need-s {:default 60 :doc "Guess of the seconds one raw need takes to gather: a walk, a fell or a dig (the quote)." :spec (a/num-in 0 nil)}
   :jobs.items.obtain.base/loose-radius {:default 16 :doc "Blocks within which a loose item of the wanted kind is picked up (the :loose way)." :spec (a/num-in 0 32)}
   :jobs.items.obtain.base/bill-item-s {:default 2700 :doc "Seconds each item of a bill may take (its obtain child's :spend :job-time)." :spec (a/num-in 0 nil)}
   :jobs.items.obtain.base/explore {:default {:max-distance 64 :max-legs 12 :spend {:job-time 300}} :doc "jobs.explore.search options of the first search for a raw need (:explore true): rings round the start." :spec map?}
   :jobs.items.obtain.base/explore-far {:default {:max-distance 192 :max-legs 16 :spend {:job-time 600}} :doc "jobs.explore.search options of every search after the first: it walks on outward along one heading (a body in a treeless desert or on an island goes further and swims over water)." :spec map?}
   :jobs.items.obtain.base/site-radius {:default 96 :doc "How far from the :center cell (a build's site) a search for a raw need goes at most." :spec (a/num-in 1 nil)}
   :jobs.items.obtain.base/barren-ore {:default 20 :doc "Ore blocks the rock a mine for an unseen ore cut in a row with no find would be expected to lay bare (jobs.lib.cost base rates at its height) before the gather counts its runs fruitless again." :spec (a/num-in 0 nil)}
   :jobs.items.obtain.base/explore-rounds {:default 6 :doc "Searches for one raw need before the gather source gives up; each after the first heads another way." :spec (a/int-in 1 nil)}})
(defn max-inspections [] (settings/get settings :jobs.items.obtain.base/max-inspections))
(defn animal-radius [] (settings/get settings :jobs.items.obtain.base/animal-radius))
(defn near-chest [] (settings/get settings :jobs.items.obtain.base/near-chest))
(defn far-chest [] (settings/get settings :jobs.items.obtain.base/far-chest))
(defn sapling-leaf-limit [] (settings/get settings :jobs.items.obtain.base/sapling-leaf-limit))
(defn search-scan-radius [] (settings/get settings :jobs.items.obtain.base/search-scan-radius))
(defn craft-step-s [] (settings/get settings :jobs.items.obtain.base/craft-step-s))
(defn smelt-item-s [] (settings/get settings :jobs.items.obtain.base/smelt-item-s))
(defn gather-need-s [] (settings/get settings :jobs.items.obtain.base/gather-need-s))
(defn seen-radius [] (settings/get settings :jobs.items.obtain.base/seen-radius))
(defn craft-radius [] (settings/get settings :jobs.items.obtain.base/craft-radius))
(defn loose-radius [] (settings/get settings :jobs.items.obtain.base/loose-radius))
(defn bill-item-s [] (settings/get settings :jobs.items.obtain.base/bill-item-s))
(defn explore-setting [] (settings/get settings :jobs.items.obtain.base/explore))
(defn explore-far-setting [] (settings/get settings :jobs.items.obtain.base/explore-far))
(defn site-radius [] (settings/get settings :jobs.items.obtain.base/site-radius))
(defn barren-ore [] (settings/get settings :jobs.items.obtain.base/barren-ore))
(defn explore-rounds [] (settings/get settings :jobs.items.obtain.base/explore-rounds))

(defn names-of [{:keys [item any-of]}]
  (vec (if item [item] any-of)))

(defn wanted-fields
  "{:item name} or {:any-of [..]} of the args, for reasons and results."
  [a]
  (into {} (remove (comp nil? val)) (select-keys a [:item :any-of])))

(defn bill-of
  "The bill's entries [item n], in order."
  [a]
  (vec (:bill a)))

(defn args-error [a]
  (let [names (names-of a)]
    (cond
      (:bill a) (cond (or (:item a) (:any-of a)) "a :bill replaces :item and :any-of: give one or the other"
                      (empty? (:bill a)) ":bill is {item count} with at least one item"
                      (not (every? (fn [[k n]] (and (string? k) (int? n) (pos? n))) (:bill a))) (str ":bill is {item count}, counts positive whole numbers, got " (pr-str (:bill a))))
      (not (and (seq names) (every? string? names))) (str "needs :item, or :any-of [names], got :item " (pr-str (:item a)) " :any-of " (pr-str (:any-of a)))
      (not (and (int? (:count a)) (pos? (:count a)))) (str ":count is a positive whole number, got " (pr-str (:count a))))))

(defn carried-of [inv names]
  (reduce + 0 (map #(storage/usable-carried inv %) names)))

(defn shortfall
  "How many more of names than are carried the call's :count asks for."
  [c names]
  (max 0 (- (:count (:args c)) (carried-of (u/inventory (:primitives c)) names))))

(def all-ways #{:chest :craft :smelt :loose :gather :hunt :crops :search})

(def field-ways "What a caller's :gather (the fetch ways of jobs.lib.need) stands for: every way that works the field." #{:loose :hunt :crops :search})

(defn ways-of
  "The ways the call allows: the frozen call's; before it starts, the args' :ways, else all. A :gather brings the field ways."
  [c]
  (let [ws (set (or (:ways (ctx/frozen-call c)) (:ways (:args c)) all-ways))]
    (cond-> ws (contains? ws :gather) (into field-ways))))

(defn way? [c k] (contains? (ways-of c) k))

(defn gathers?
  "Whether a way that fetches a raw item from the world is allowed."
  [c]
  (boolean (some #(way? c %) [:gather :hunt :crops])))

(defn makes?
  "Whether a chain may be made: a craft or a smelt step is allowed."
  [c]
  (or (way? c :craft) (way? c :smelt)))

(defn step-way [step] (if (= :smelt (:op step)) :smelt :craft))

(defn plan-allowed?
  "Whether every step of a plan is a way the call allows."
  [c plan]
  (every? #(way? c (step-way %)) (:steps plan)))

(defn child-ways
  "The ways of this call a fetching child job takes: :carried and the :chest :craft :gather it allows (the ways of jobs.lib.need)."
  [c]
  (into #{:carried} (filter #{:chest :craft :gather} (ways-of c))))

(defn holds [items names]
  (first (filter #(pos? (get items % 0)) names)))
(defn tried! [c k v] (ctx/update-mem! c assoc-in [:tried k] v) :again)
