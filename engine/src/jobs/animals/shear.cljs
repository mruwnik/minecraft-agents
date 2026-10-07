(ns jobs.animals.shear
  (:require [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.util :as u]))

(def doc
  "Shear the adult sheep within :radius and pick up the wool. A one-shot order that starts and ends itself. The
  check always passes, so a cut job resumes.

  One call is the whole run. It takes the nearest adult sheep not yet shorn or given up on (babies and sheared sheep are
  skipped). The body walks to within 3 blocks (doors :shut, each leg bounded by :walk-timeout-s) and uses the
  shears it carries (fetching them first when none are carried and sheep wait, jobs.lib.fetch, unless :fetch is false). Sheep are tracked by uuid, by id when it has none.

  A sheep is given up on when its walk is blocked or two shearings were out of reach (:unreachable), nothing
  happened (:no-effect: already sheared), it vanished (:gone), the server said it cannot (:cannot) or the use
  failed (:failed).

  Shearing stops when :count sheep are shorn (every one in radius when nil), none is left, the shears are gone
  or three sheep in a row were fruitless (no shears are fetched while nothing is left to shear). Unless :collect is false it then runs jobs.forestry.collect-drops for
  the wool in :radius and ends.

  Ends with info shear.done and a warn shear.gave-up unless the reason is :shorn. Result {:reason :shorn [keys]
  :given-up {key reason} :collected n}. Reasons:
  - :shorn: the count was reached, or the sheep ran out after some were shorn.
  - :shears-broke: the shears were gone after some were shorn.
  - :timeout: :timeout-s of work, not counting fetching (no collecting).
  - :no-shears: none carried (and none fetched) and none shorn.
  - With nothing shorn: :unreachable if one was given up as unreachable, else :refused (or :no-zones) when the
    zone rules refused every candidate, else :all-sheared (adults present but all sheared) or :none.

  Zones: a sheep standing in another owner's zone or claim, or in a plan's footprint, is left alone (warn
  shear.declined once, :reason :refused, or :no-zones when no zone list was read). :ignore-zones? true skips the check.")

(def args
  {:count {:doc "sheep to shear; every one in radius when nil" :default nil}
   :radius {:doc "sheep within this many blocks count" :default 16}
   :walk-timeout-s {:doc "bound of one walk towards a sheep" :default 5}
   :timeout-s {:doc "seconds of working with the animals (not fetching) before the job gives up" :default 120}
   :collect {:doc "pick up the wool afterwards" :default true}
   :fetch {:doc "get shears when none are carried (jobs.lib.fetch): true, a set of kinds or a map of limits; false ends :no-shears" :default true}
   :ignore-zones? animals/ignore-zones-arg})

(def reach 3)
(def max-in-row 3)

(def wool-names
  (mapv #(str % "_wool")
        ["white" "orange" "magenta" "light_blue" "yellow" "lime" "pink" "gray"
         "light_gray" "cyan" "purple" "blue" "brown" "green" "red" "black"]))

(defn check [_c] true)

(defn finish!
  "Emit the outcome, hand it to the parent and end the job."
  [c reason]
  (let [m (ctx/mem c)
        result {:reason reason
                :shorn (vec (:shorn m))
                :given-up (:given-up m {})
                :collected (:collected m 0)}]
    (ctx/emit! c :shear.done :info (assoc result :text (str "shear done: " (name reason) ", shorn " (count (:shorn m)))))
    (when (not= :shorn reason)
      (ctx/emit! c :shear.gave-up :warn {:reason reason :text (str "shearing stopped: " (name reason))}))
    (ctx/result! c result)
    :done))

(defn go-collect!
  "End with reason, after picking up the wool when :collect is set."
  [c reason]
  (if-not (:collect (:args c))
    (finish! c reason)
    (do (ctx/update-mem! c assoc :phase :collect :reason reason)
        :again)))

(defn sheep-in-radius [c]
  (animals/herd (:primitives c) "sheep" (:radius (:args c))))

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
    (go-collect! c :shorn)
    (finish! c (none-reason c))))

(defn ^:async shear!
  "Use the shears on sheep once and book the outcome."
  [c sheep]
  (let [k (animals/key-of sheep)
        r (await (ctx/act c :interact #js {:id (.-id sheep) :item "shears"}))]
    (case (.-status r)
      "used" (ctx/update-mem! c #(-> % (update :shorn (fnil conj []) k) (assoc :in-row 0)))
      "no-effect" (do (animals/give-up! c k :no-effect) (animals/bump-row! c))
      "gone" (animals/give-up! c k :gone)
      "out-of-reach" (animals/book-out-of-reach! c k)
      "cannot" (do (animals/give-up! c k :cannot) (animals/bump-row! c))
      "failed" (do (animals/give-up! c k :failed) (animals/bump-row! c))
      nil)))

(defn ^:async engage!
  "Walk to the sheep and shear it; end when fruitless three times in a row."
  [c sheep]
  (let [walked (await (animals/walk! c (animals/key-of sheep) sheep reach {:reset-row? true :doors :shut}))]
    (when (= :there walked)
      (await (shear! c sheep)))
    (if (>= (:in-row (ctx/mem c) 0) max-in-row)
      (out-of-sheep! c)
      :again)))

(defn ^:async collect!
  "One round of the pick-up phase; finish with the stored reason when it is done."
  [c]
  (let [{:keys [radius]} (:args c)
        r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius radius :filter wool-names}))]
    (if-not (= :done r)
      :continue
      (do (ctx/update-mem! c assoc :collected (:collected (ctx/child-result c :collect) 0))
          (finish! c (:reason (ctx/mem c)))))))

(defn shears-carried? [c]
  (some #(= "shears" (:name %)) (u/inventory (:primitives c))))

(defn problem
  "The need wait for shears while none are carried and an unsheared sheep waits, else nil."
  [c]
  (when (and (not (shears-carried? c)) (not= :collect (:phase (ctx/mem c))) (seq (candidates c)))
    {:reason :need :item "shears"}))

(defn ^:async no-shears!
  "No shears carried: fetch them when :fetch allows and a sheep waits, else end (collecting what was shorn). Resolves
  to :again once they arrived, else the round's result."
  [c]
  (let [end! #(if (seq (:shorn (ctx/mem c))) (go-collect! c :shears-broke) (finish! c :no-shears))]
    (if-not (and (fetch/opts c 'jobs.animals.shear) (problem c))
      (end!)
      (or (await (fetch/fetch-untimed! c 'jobs.animals.shear problem))
          (if (problem c) (end!) :again)))))

(defn ^:async step [c]
  (let [now (ctx/now c)
        {:keys [timeout-s] n :count} (:args c)]
    (ctx/update-mem! c update :started #(or % now))
    (let [m (ctx/mem c)
          cands (candidates c)]
      (cond
        (>= (- now (:started m)) (* 1000 timeout-s)) (finish! c :timeout)
        (= :collect (:phase m)) (await (collect! c))
        (not (shears-carried? c)) (await (no-shears! c))
        (and n (>= (count (:shorn m)) n)) (go-collect! c :shorn)
        (empty? cands) (out-of-sheep! c)
        :else (await (engage! c (first cands)))))))

(defn ^:async round
  "The whole attempt: loop the steps until one ends or yields."
  [c]
  (loop []
    (let [r (await (step c))]
      (if (= :again r) (recur) r))))
