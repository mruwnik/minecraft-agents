(ns jobs.build.hut
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.build.from-plan.walk :as fp-walk]
            [jobs.lib.build.plan :as plan]
            [jobs.lib.build.wood :as wood]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.checks :as checks]
            [jobs.lib.cost :as cost]
            [jobs.lib.need :as need]
            [jobs.lib.ready :as ready]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.world :as known]
            [jobs.lib.world-files :as world-files]
            [plan.shape :as shape]))

(def meta {:tags [:goal :build]})

(def doc
  "Build a hut from a blueprint: pick one of :blueprints, a site for it, place it there as the plan :plan (default
  hut-<body>) and build that plan.
  The blueprint is the one whose blocks cost least to have: what is carried is free, the rest is priced by the jobs.items.obtain
  each would start (need/item-price); with the call's ways only :carried, a block not carried has no price. Ties go to the first listed.
  The site is a jobs.farm.find-spot child sized to the blueprint (it avoids other plans, zones and claims, and prices levelling
  and the walk); the blueprint's bottom layer stands on the ground the spot names. The placed plan
  {:parts [{:id \"hut\" :blueprint :at}] :metadata {:by body}} is saved as a plan of the world (jobs.lib.world/create-plan!), so every
  job reads its cells as claimed, and a jobs.build.from-plan child builds it (with the call's ways: it fetches what is short).
  Resume: an existing plan :plan made by the body is built as it stands, with no new choice; one made by another is never
  (:taken); a plan id made by another meanwhile ends nothing-possible :taken. The plan stays when the call ends, built or not, as a plan the agent made would,
  except that a build ending nothing-possible with no cell built removes it: the next round chooses a site again, and the call ends with that ending when the
  search finds the same site (the :spend job-time prices the tries).
  can-start?: never :taken; without the plan, never :no-blueprint when no blueprint listed is readable, later
  :no-items {:needs} when no blueprint has a price (nothing to fetch with), later :not-worth {:price :worth} when the
  cheapest costs more than the call's worth. Done when the plan exists, is the body's and no cell of it is owed.
  Ends with from-plan's ending and data, with :plan; or find-spot's ending other than done.")

(a/defargs args
  {:blueprints {:doc "ids of the blueprints to choose from" :spec (a/coll-of a/name?) :default ["starter-hut"]}
   :plan {:doc "id of the plan the hut is placed as; nil: hut-<body>" :spec world-files/plan-id? :default nil}
   :ignore-zones? :shared})

(def ways need/fetch-ways)

(def target [:plan])

(a/defargs settings
  {::job-time-s {:default 2100 :doc "Seconds one hut may take: a site search (farm.find-spot ::job-time-s) and one build (build.from-plan ::job-time-s)." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:job-time ::job-time-s}})

(defn plan-id [c] (or (:plan (:args c)) (str "hut-" (ctx/self-name c))))

(defn own-plan
  "The answer for the call's plan (jobs.lib.world/plan), nil when there is none; {:taken by} when another body made it."
  [c]
  (when-let [answer (known/plan c (plan-id c))]
    (let [by (shape/author (:plan answer))]
      (if (and by (not= by (ctx/self-name c))) {:taken by} answer))))

(defn needs
  "{item n} blueprint bp's cells take beyond carried ({item n}): one item a cell (none for :clear or a two-block item's other half), a generic
  wood want in the species carried most."
  [bp carried]
  (let [sp (wood/choose carried nil)]
    (plan/shortage (keep (fn [{:keys [want]}]
                           (let [want (wood/resolve-bed (wood/resolve-want want sp))]
                             (when-not (plan/companion? want)
                               (some->> (plan/item-for want carried) (hash-map :item)))))
                         (shape/placed-cells bp {:at [0 0 0]} nil))
                   carried)))

(defn priced
  "[{:id :needs :price}] of the listed blueprints that can be read, cheapest first (stable: ties keep the listed order)."
  [c]
  (let [carried (storage/carried-totals (u/inventory (:primitives c)))
        o (need/fetch-opts c)]
    (->> (:blueprints (:args c))
         (keep (fn [id] (when-let [bp (known/blueprint c id)] {:id id :needs (needs bp carried)})))
         (map (fn [b] (assoc b :price (reduce + 0 (map (fn [[item n]] (if o (need/item-price c item n o nil) ##Inf)) (:needs b))))))
         (sort-by :price)
         vec)))

(defn choice
  "The cheapest blueprint (priced), judged once per check-cache window in a check."
  [c]
  (first (if (:token c) (priced c) (check-cache/cached c [:hut (:args c)] #(priced c)))))

(defn site-check
  "ok while the plan exists (it is built as it stands); else the blueprint choice (see doc)."
  [c]
  (let [own (own-plan c)
        b (when-not own (choice c))]
    (cond
      (:taken own) (contract/never :taken (str "plan " (plan-id c) " is " (:taken own) "'s") :by (:taken own))
      own (contract/ok :placed (str "plan " (plan-id c) " is placed"))
      (nil? b) (contract/never :no-blueprint (str "no blueprint of " (pr-str (:blueprints (:args c))) " can be read"))
      (= ##Inf (:price b)) (checks/cause-status :later {:reason :no-items :blueprint (:id b) :needs (:needs b)}
                                                (str "hut: " (:id b) " needs " (plan/shortage-text (:needs b)) ", none of it to be had"))
      (not (cost/affords? (:price b) (ctx/worth c)))
      (contract/later :not-worth (str "hut: " (:id b) "'s blocks cost " (js/Math.round (:price b)) " s, more than its worth " (js/Math.round (ctx/worth c)) " s")
                      :price (:price b) :worth (ctx/worth c))
      :else (contract/ok :blueprint (str "hut: " (:id b) " is to be had")))))

(defn built
  "{:cells judged} of the call's plan (jobs.lib.build.plan/planned), or {:trouble text}: a check reads it once per check-cache window."
  [c]
  (let [read #(plan/planned (assoc c :args {:plan (plan-id c) :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (if (:token c) (read) (check-cache/cached c [:hut-built (plan-id c)] read))))

(defn work-s "Mean seconds the owed cells of the placed plan take; 0 before it is placed." [c]
  (let [own (own-plan c)
        {:keys [cells trouble]} (when (and own (not (:taken own))) (built c))]
    (if (or (nil? cells) trouble) 0 (fp-walk/owed-s cells))))

(defn spot [c] (or (work-place/plan-spot {:plan (plan-id c)} c) (a/cell (u/self-pos c))))

(def safe? (ready/safe-at-build spot work-s))
(def fit? (ready/fed-fit-for :long work-s))

(def can-start? site-check)
(def can-run? site-check)

(defn done? [c]
  (let [own (own-plan c)
        {:keys [cells trouble]} (when (and own (not (:taken own))) (built c))
        left (if cells (count (plan/owed cells)) 1)
        data {:units :cells :left left :result {:plan (plan-id c)}}]
    (cond
      (or (nil? own) (:taken own)) (apply contract/later :no-hut (str "hut: plan " (plan-id c) " is not placed") (mapcat identity data))
      trouble (apply contract/later :plan-trouble (str "hut: plan " (plan-id c) ": " trouble) (mapcat identity data))
      :else (checks/goal (zero? left) [:built (str "hut: plan " (plan-id c) " is built")] [:owed (str "hut: plan " (plan-id c) ": " left " cells owed")] data))))

(defn passed-up [c e]
  (contract/with-because (ctx/end c (:kind e) (:cause e) (assoc (:data e) :plan (plan-id c))) e))

(defn nothing-built?
  "Whether no cell of the call's plan holds its block yet."
  [c]
  (let [{:keys [cells trouble]} (built c)]
    (and (not trouble) (not-any? #(and (= :match (:answer %)) (:item %) (not (:companion %))) cells))))

(defn site-at [c] (some-> (own-plan c) :plan :parts first :at))

(defn ^:async build!
  "Build the placed plan. A site that ends nothing-possible with no cell built is given up: the plan is removed, the site
  remembered, and the next round chooses again; the call's :spend job-time prices the tries."
  [c]
  (let [e (await (ctx/call c :build 'jobs.build.from-plan {:plan (plan-id c) :ignore-zones? (boolean (:ignore-zones? (:args c))) :ways (need/pass-ways c)}))]
    (cond
      (= :continue e) :continue
      (and (= :nothing-possible (:kind e)) (not (:taken (own-plan c))) (nothing-built? c))
      (do (ctx/update-mem! c assoc :failed {:at (site-at c) :end (select-keys e [:kind :cause :data])})
          (known/delete-plan! c (plan-id c))
          :continue)
      :else (passed-up c e))))

(defn placed-plan
  "The plan of blueprint id with its bottom layer on the ground top spot {:x :y :z}."
  [c id {:keys [x y z]}]
  {:id (plan-id c) :note "hut" :parts [{:id "hut" :blueprint id :at [x (inc y) z]}] :metadata {:by (ctx/self-name c)}})

(defn ^:async place!
  "Save the plan of the spot the search found, unless the id was taken meanwhile, or the spot is the one that just failed."
  [c id spot]
  (let [plan (placed-plan c id spot)
        failed (:failed (ctx/mem c))]
    (cond
      (= (:at failed) (get-in plan [:parts 0 :at]))
      (let [{:keys [kind cause data]} (:end failed)] (ctx/end c kind cause (assoc data :plan (plan-id c))))
      (= :exists (known/create-plan! c plan))
      (if (:taken (own-plan c))
        (ctx/end c :nothing-possible :taken {:plan (plan-id c) :by (:taken (own-plan c))})
        (await (build! c)))
      :else (await (build! c)))))

(defn ^:async round
  "Place the plan when it is missing (blueprint, then a find-spot child), then build it."
  [c]
  (if (own-plan c)
    (await (build! c))
    (let [id (or (:blueprint (ctx/mem c)) (:id (choice c)))
          _ (ctx/update-mem! c assoc :blueprint id)
          e (await (ctx/call c :site 'jobs.farm.find-spot {:blueprint id}))]
      (cond
        (= :continue e) :continue
        (not= :done (:kind e)) (passed-up c e)
        :else (await (place! c id (:spot (:data e))))))))
