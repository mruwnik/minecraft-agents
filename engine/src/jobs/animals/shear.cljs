(ns jobs.animals.shear
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]))

(def doc
  "Shear the adult sheep within :radius and pick up the wool. A one-shot order that starts and ends itself; a cut job resumes.

  One call is the whole run. It takes the nearest adult sheep not yet shorn or given up on (babies and sheared sheep are
  skipped). The body walks to within 3 blocks (doors :shut) and uses the
  shears it carries (fetching them first when none are carried and sheep wait, jobs.lib.need, unless :fetch is false). Sheep are tracked by uuid, by id when it has none.

  A sheep is given up on when its walk is blocked or two shearings were out of reach (:unreachable), nothing
  happened (:no-effect: already sheared), it vanished (:gone), the server said it cannot (:cannot) or the use
  failed (:failed).

  Shearing stops when :count sheep are shorn (every one in radius when nil), none is left, the shears are gone
  or three sheep in a row were fruitless (no shears are fetched while nothing is left to shear). Unless :collect is false it then runs jobs.items.collect-drops for
  the wool in :radius and ends.

  The data {:reason :shorn [keys]
  :given-up {key reason} :collected n} and the ending by the reason:
  - :done :already (data reason :all-sheared) when adult sheep stand in radius and all are sheared at the start. :done :shorn: the count was reached. Any stop after at least :least sheep were shorn (default 1; :all =
    :count, every sheep in radius when nil) is done, with the stop as the data reason.
  - :nothing-possible :shears-broke (the shears were gone after some, fewer than :least, were shorn).
  - :condition-failed :no-shears (none carried and the shears went missing mid-run; a listed call with :fetch false waits for them), a failed fetch ends with the fetch's own ending,
    reason :need (its cause names the shears), or with nothing shorn :refused / :no-zones
    when the zone rules refused every candidate.
  - :nothing-possible with nothing shorn: :unreachable if one was given up as unreachable, else :all-sheared (adults present
    but all sheared) or :none.

  Zones: a sheep standing in another owner's zone or claim, or in a plan's footprint, is left alone (warn
  shear.declined once, :reason :refused, or :no-zones when no zone list was read). :ignore-zones? true skips the check.")

(a/defargs args
  {:count {:doc "sheep to shear; every one in radius when nil" :spec (a/int-in 1 nil) :default nil}
   :radius {:doc "sheep within this many blocks count" :spec (a/num-in 0 nil) :default 16}
   :color {:doc "shear only sheep of this wool colour (e.g. \"white\"); nil: any" :spec a/name? :default nil}
   :collect {:doc "pick up the wool afterwards" :spec boolean? :default true}
   :fetch {:doc "get shears when none are carried (jobs.lib.need): true, a set of kinds or a map of limits; false waits :no-shears; a failed fetch ends :need" :spec need/option? :default true}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds of working, fetching included, before shearing gives up." :spec (a/num-in 0 nil)}
   ::max-in-row {:default 3 :doc "Fruitless animals in a row that end the run." :spec (a/int-in 0 nil)}})
(defn max-in-row [] (settings/get settings ::max-in-row))

(def wool-names
  (mapv #(str % "_wool")
        ["white" "orange" "magenta" "light_blue" "yellow" "lime" "pink" "gray"
         "light_gray" "cyan" "purple" "blue" "brown" "green" "red" "black"]))

(defn shears-carried? [c]
  (some #(= "shears" (:name %)) (u/inventory (:primitives c))))

(def target [:color])

(def safe? ready/safe-if-lit)
(def fit? ready/fit?)


(def defaults {:spend {:job-time ::job-time-s}})

(defn can-start?
  "Later :no-shears: no shears carried and :fetch off."
  [c]
  (if (or (need/parse-arg (:fetch (:args c))) (shears-carried? c))
    (contract/ok :shears "shears carried or fetched")
    (contract/later :no-shears "no shears carried and :fetch is off")))

(defn quote-of [c]
  {:uses {:job-time [0 (* 10 (or (:count (:args c)) 1)) ##Inf]}})

(def ending-of
  "The kind and cause of a finish by reason."
  {:shorn [:done nil]
   :shears-broke [:nothing-possible :shears-broke]
   :no-shears [:condition-failed :no-shears]
   :refused [:condition-failed :refused]
   :no-zones [:condition-failed :no-zones]})

(defn least-shorn
  "Sheep that must be shorn for the run to be done: the call's :least (:all = :count, else what was shorn)."
  [c]
  (let [l (:least (ctx/frozen-call c) 1)]
    (if (= :all l) (or (:count (:args c)) (count (:shorn (ctx/mem c)))) l)))

(defn finish!
  "Emit the outcome and end the call: the ending of reason (ending-of, else nothing-possible), or ending when given
  (a failed fetch's)."
  [c reason & [ending]]
  (let [m (ctx/mem c)
        result {:reason reason
                :shorn (vec (:shorn m))
                :given-up (:given-up m {})
                :collected (:collected m 0)
                :text (str "shear done: " (name reason) ", shorn " (count (:shorn m)))}]
    (if ending
      (assoc ending :data result)
      (let [[kind cause] (if (and (pos? (count (:shorn m))) (>= (count (:shorn m)) (least-shorn c)))
                           [:done nil]
                           (get ending-of reason [:nothing-possible reason]))]
        (ctx/end c kind cause result)))))

(defn go-collect!
  "End with reason, after picking up the wool when :collect is set."
  [c reason]
  (if-not (:collect (:args c))
    (finish! c reason)
    (do (ctx/update-mem! c assoc :phase :collect :reason reason)
        :again)))

(defn sheep-in-radius [c]
  (let [color (:color (:args c))]
    (cond->> (animals/herd (:primitives c) "sheep" (:radius (:args c)))
      color (filterv #(= color (.-color %))))))

(defn candidates
  "The adult unsheared sheep within radius not shorn or given up on, nearest first."
  [c]
  (let [{:keys [shorn given-up]} (ctx/mem c)
        skip (into (set shorn) (keys given-up))]
    (->> (sheep-in-radius c)
         (filterv #(and (not (true? (.-baby %)))
                        (not (true? (.-sheared %)))
                        (not (contains? skip (animals/key-of %)))))
         (animals/allowed c :shear.declined "shear" :harvest))))

(defn done?
  "Ok before any work when adult sheep stand in radius and all are sheared; later with the sheep shorn and those still
  to shear: the run ends itself with its reason (a run that began shearing stays on until its wool is collected)."
  [c]
  (let [{:keys [shorn given-up phase]} (ctx/mem c)
        adults (filterv #(not (true? (.-baby %))) (sheep-in-radius c))]
    (if (and (empty? shorn) (empty? given-up) (nil? phase) (seq adults) (every? #(true? (.-sheared %)) adults))
      (contract/ok :all-sheared "every sheep in reach is sheared"
                   :result {:reason :all-sheared :shorn [] :given-up {} :collected 0})
      (contract/later :shearing (str (count shorn) " sheep shorn") :units :sheep
                      :done (count shorn) :left (count (candidates c))))))

(defn none-reason
  "Why the job ends with no sheep left to shear."
  [c]
  (cond
    (some #{:unreachable} (vals (:given-up (ctx/mem c)))) :unreachable
    (animals/refusal c) (animals/refusal c)
    (some #(not (true? (.-baby %))) (sheep-in-radius c)) :all-sheared
    :else :none))

(defn out-of-sheep!
  "End when no sheep is left: collect when any was shorn, else finish with the reason."
  [c]
  (if (seq (:shorn (ctx/mem c)))
    (go-collect! c (if (>= (count (:shorn (ctx/mem c))) (least-shorn c)) :shorn (none-reason c)))
    (finish! c (none-reason c))))

(defn ^:async shear!
  "Use the shears on sheep once (an items.interact child) and book the outcome. :continue while the child waits."
  [c sheep]
  (let [k (animals/key-of sheep)
        r (await (animals/use-on! c k sheep "shears"))]
    (cond
      (= :continue r) :continue
      (= :used (:status r)) (ctx/update-mem! c #(-> % (update :shorn (fnil conj []) k) (assoc :in-row 0)))
      :else (animals/book-failed! c k (:status r)))))

(defn ^:async engage!
  "Shear the sheep; end when fruitless :max-in-row times in a row."
  [c sheep]
  (cond
    (= :continue (await (shear! c sheep))) :continue
    (>= (:in-row (ctx/mem c) 0) (max-in-row)) (out-of-sheep! c)
    :else :again))

(defn ^:async collect!
  "One round of the pick-up phase; finish with the stored reason when it is done."
  [c]
  (let [{:keys [radius]} (:args c)
        e (await (ctx/call c :collect 'jobs.items.collect-drops {:radius radius :items wool-names}))]
    (if (= :continue e)
      :continue
      (do (ctx/update-mem! c assoc :collected (get-in e [:data :collected] 0))
          (finish! c (:reason (ctx/mem c)))))))

(defn fetchable?
  "Whether shears are to be fetched now: none carried, an unsheared sheep waits and :fetch allows it."
  [c]
  (and (need/parse-arg (:fetch (:args c))) (not (shears-carried? c)) (not= :collect (:phase (ctx/mem c))) (seq (candidates c))))

(defn ^:async no-shears!
  "No shears carried: fetch them when :fetch allows and a sheep waits, else end (collecting what was shorn). Resolves
  to :again once they arrived, else the round's result."
  [c]
  (let [end! #(if (seq (:shorn (ctx/mem c))) (go-collect! c :shears-broke) (finish! c :no-shears))]
    (if-not (fetchable? c)
      (end!)
      (let [limits (select-keys (need/parse-arg (:fetch (:args c))) [:how :explore])
            e (await (need/item! c "shears" 1 limits))]
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) :again
          (seq (:shorn (ctx/mem c))) (go-collect! c :shears-broke)
          :else (finish! c :no-shears e))))))

(defn ^:async step [c]
  (let [{n :count} (:args c)
        m (ctx/mem c)
        cands (candidates c)]
    (cond
      (= :collect (:phase m)) (await (collect! c))
      (and (empty? cands) (empty? (:shorn m)) (not (look/surveyed? c))) (await (look/survey! c)) ; animals count only while in view
      (not (shears-carried? c)) (await (no-shears! c))
      (and n (>= (count (:shorn m)) n)) (go-collect! c :shorn)
      (empty? cands) (out-of-sheep! c)
      :else (await (engage! c (animals/pick c cands))))))

(defn ^:async round
  "The whole attempt: steps (a pace between; done? and can-run? are asked between them) until one ends or yields."
  [c]
  (await (pace/steps! c #(step c))))
