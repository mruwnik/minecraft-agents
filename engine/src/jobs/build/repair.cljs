(ns jobs.build.repair
  "Mend what the body has seen broken in a plan, through jobs.build.from-plan."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.build.from-plan.walk :as fp-walk]
            [jobs.lib.build.diff :as diff]
            [jobs.lib.build.plan :as plan]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.cost :as cost]
            [jobs.lib.need :as need]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]))

(def meta {:tags [:goal]})

(def doc
  "Mend the cells of a plan (:plan, its :parts, nil: every part) the body has seen differ from their want
  (jobs.lib.build.diff/diff): missing, a wrong block, or a block where the plan wants clear. A cell never seen, or
  remembered so long it is likelier changed than not, is not listed: it is mended once seen. Crop, tree and :any cells,
  the upper half of a two-block item and a door or gate swung wrong are not this job's. A cell that was never built is
  mended like a broken one: the plan wants it there either way.
  Each round calls jobs.build.from-plan with :cells the cells listed (it builds them and the plan cells they stand on,
  fetching what it lacks), at the call's worth. Ends done when no cell is seen broken; the from-plan child's ending
  other than done is passed up with its data; nothing-possible :not-mended {:cells} when from-plan ended done but a cell
  it was given is still seen broken. New breaks seen meanwhile get a new from-plan call.
  can-start?: a missing or unreadable plan waits :plan-trouble, a part the plan lacks is never; later :not-worth
  {:price :worth} while from-plan's work over the listed cells (from-plan.walk/owed-s) costs more than the call's worth.
  Quote: from-plan's over the listed cells. No memory of its own but the cells of the from-plan call under way.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world" :spec a/name? :default nil}
   :parts {:doc "ids of the plan's parts to mend; nil: every part" :spec (a/coll-of a/name?) :default nil}
   :ignore-zones? :shared})

(def ways need/fetch-ways)

(def target [:plan :parts])

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one repair may take, its builds together: one build's (jobs.build.from-plan ::job-time-s)." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:job-time ::job-time-s}})

(defn broken-now
  "{:broken diff-cells} of the call's plan and parts, or {:trouble :later?}: a check (no token) reads it once per
  check-cache window, a round afresh."
  [c]
  (let [read #(let [{:keys [plan parts]} (:args c)
                    r (diff/plan-cells c plan parts)]
                (if (:trouble r) r {:broken (diff/diff c (:cells r))}))]
    (if (:token c) (read) (check-cache/cached c [:repair (:args c)] read))))

(defn child-args [c cells]
  {:plan (:plan (:args c)) :cells (vec cells) :ignore-zones? (boolean (:ignore-zones? (:args c))) :ways (need/pass-ways c)})

(defn price
  "Seconds from-plan's work over the broken cells takes, as its quote prices it (from-plan.walk/owed-s)."
  [c broken]
  (let [{:keys [cells trouble]} (when (seq broken) (plan/planned (assoc c :args (child-args c (map :pos broken)))))]
    (if (or trouble (empty? cells)) 0 (fp-walk/owed-s cells))))

(defn work-s "Seconds the cells seen broken take to mend (price)." [c] (price c (:broken (broken-now c))))

(def safe? (ready/safe-at-build work-place/spot work-s))

(declare quote-of)

(def fit? (ready/trip-fed (ready/fed-fit-for :long work-s) work-place/spot #(quote-of %)))

(defn plan-problem [c]
  (let [{:keys [trouble later?]} (broken-now c)]
    (cond
      (nil? trouble) (contract/ok :plan "the plan can be read")
      later? (contract/later :plan-trouble trouble :why trouble)
      :else (contract/never :plan-trouble trouble :why trouble))))

(defn worth-it [c]
  (let [{:keys [broken]} (broken-now c)
        s (price c broken)
        w (ctx/worth c)]
    (if (cost/affords? s w)
      (contract/ok :worth "the mend is worth its work")
      (contract/later :not-worth (str "mending " (count broken) " cells takes " (js/Math.round s) " s, more than its worth " (js/Math.round w) " s")
                      :price s :worth w))))

(def can-start? (cond/and plan-problem worth-it))
(def can-run? plan-problem)

(defn done? [c]
  (let [{:keys [trouble broken]} (broken-now c)
        n (count broken)
        data {:units :cells :left n :result {:broken (mapv :pos broken)}}
        plan (:plan (:args c))]
    (cond
      trouble (apply contract/later :plan-trouble trouble (mapcat identity data))
      (and (zero? n) (checks/child-under-way? c)) (apply contract/later :wrapping-up (str "plan " plan ": mended, the build wraps up") (mapcat identity data))
      :else (checks/goal (zero? n) [:sound (str "plan " plan ": no cell seen broken")] [:broken (str "plan " plan ": " n " cells seen broken")] data))))

(defn ^:async quote-of [c]
  (let [broken (:broken (broken-now c))]
    (assoc (select-keys (await (ctx/quote c 'jobs.build.from-plan (child-args c (map :pos broken)))) [:uses :death :estimate])
           :done [0 (count broken) (count broken)])))

(defn ^:async round [c]
  (let [cells (or (when (checks/child-under-way? c) (:cells (ctx/mem c)))
                  (mapv :pos (:broken (broken-now c))))
        _ (ctx/update-mem! c assoc :cells cells)
        e (await (ctx/call c :mend 'jobs.build.from-plan (child-args c cells)))]
    (cond
      (= :continue e) :continue
      (not= :done (:kind e)) (contract/with-because (ctx/end c (:kind e) (:cause e) (:data e)) e)
      :else (let [left (filterv (set (map :pos (:broken (broken-now c)))) cells)]
              (if (seq left)
                (contract/with-because
                  (ctx/end c :nothing-possible :not-mended
                           {:cells left :text (str "plan " (:plan (:args c)) ": the build ended done, but " (count left) " of the cells it was given are still seen broken: " (pr-str (take 3 left)))})
                  e)
                :continue)))))
