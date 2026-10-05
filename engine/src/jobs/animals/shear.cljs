(ns jobs.animals.shear
  (:require [engine.ctx :as ctx]
            [engine.jobs.animals :as animals]
            [engine.jobs.util :as u]
            [engine.path.near :as near]))

(def doc
  "Shear the adult sheep within :radius and pick up the wool. A one-shot order that starts and ends itself. The
  check always passes, so a cut job resumes.

  Each round takes the nearest adult sheep not yet shorn or given up on (babies and sheared sheep are
  skipped). The body walks to within 3 blocks (doors :shut, each steer bounded by :walk-timeout-s) and uses the
  shears it carries. Sheep are tracked by uuid, by id when it has none.

  A sheep is given up on when its walk is blocked or two shearings were out of reach (:unreachable), nothing
  happened (:no-effect: already sheared), it vanished (:gone), the server said it cannot (:cannot) or the use
  failed (:failed).

  Shearing stops when :count sheep are shorn (every one in radius when nil), none is left, the shears are gone
  or three rounds in a row were fruitless. Unless :collect is false it then runs jobs.forestry.collect-drops for
  the wool in :radius and ends.

  Ends with info shear.done and a warn shear.gave-up unless the reason is :shorn. Result {:reason :shorn [keys]
  :given-up {key reason} :collected n}. Reasons:
  - :shorn: the count was reached, or the sheep ran out after some were shorn.
  - :shears-broke: the shears were gone after some were shorn.
  - :timeout: :timeout-s from the first round (no collecting).
  - :no-shears: none carried and none shorn.
  - With nothing shorn: :unreachable if one was given up as unreachable, else :all-sheared (adults present but
    all sheared or refused) or :none.")

(def args
  {:count {:doc "sheep to shear; every one in radius when nil" :default nil}
   :radius {:doc "sheep within this many blocks count" :default 16}
   :walk-timeout-s {:doc "bound of one walk towards a sheep" :default 5}
   :timeout-s {:doc "seconds from the first round before the job gives up" :default 120}
   :collect {:doc "pick up the wool afterwards" :default true}})

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
        :continue)))

(defn sheep-in-radius [c]
  (animals/herd (:primitives c) "sheep" (:radius (:args c))))

(defn candidates
  "The adult unsheared sheep within radius not shorn or given up on, nearest first."
  [c]
  (let [{:keys [shorn given-up]} (ctx/mem c)
        skip (into (set shorn) (keys given-up))]
    (filterv #(and (not (true? (.-baby %)))
                   (not (true? (.-sheared %)))
                   (not (contains? skip (animals/key-of %))))
             (sheep-in-radius c))))

(defn none-reason
  "Why the job ends with no sheep left to shear."
  [c]
  (cond
    (some #{:unreachable} (vals (:given-up (ctx/mem c)))) :unreachable
    (some #(not (true? (.-baby %))) (sheep-in-radius c)) :all-sheared
    :else :none))

(defn out-of-sheep!
  "End when no sheep is left: collect when any was shorn, else finish with the reason."
  [c]
  (if (seq (:shorn (ctx/mem c)))
    (go-collect! c :shorn)
    (finish! c (none-reason c))))

(defn give-up! [c k reason]
  (ctx/update-mem! c assoc-in [:given-up k] reason))

(defn bump-row! [c]
  (ctx/update-mem! c update :in-row (fnil inc 0)))

(defn reset-row! [c]
  (ctx/update-mem! c assoc :in-row 0))

(defn ^:async walk!
  "Walk within reach of sheep when further than reach. Resolves to :there,
  :partial or :blocked; a blocked walk gives the sheep up."
  [c sheep]
  (let [tpos (u/pos-of (.-pos sheep))]
    (if (<= (u/dist (u/self-pos c) tpos) reach)
      :there
      (let [r (await (near/walk-near! c tpos 2 {:doors :shut :timeout-s (:walk-timeout-s (:args c))}))]
        (case r
          :there (do (reset-row! c) :there)
          :partial :partial
          (do (give-up! c (animals/key-of sheep) :unreachable)
              (bump-row! c)
              :blocked))))))

(defn book-out-of-reach! [c k]
  (let [n (inc (get-in (ctx/mem c) [:fails k] 0))]
    (ctx/update-mem! c assoc-in [:fails k] n)
    (when (>= n 2) (give-up! c k :unreachable))
    (bump-row! c)))

(defn ^:async shear!
  "Use the shears on sheep once and book the outcome."
  [c sheep]
  (let [k (animals/key-of sheep)
        r (await (ctx/act c :interact #js {:id (.-id sheep) :item "shears"}))]
    (case (.-status r)
      "used" (ctx/update-mem! c #(-> % (update :shorn (fnil conj []) k) (assoc :in-row 0)))
      "no-effect" (do (give-up! c k :no-effect) (bump-row! c))
      "gone" (give-up! c k :gone)
      "out-of-reach" (book-out-of-reach! c k)
      "cannot" (do (give-up! c k :cannot) (bump-row! c))
      "failed" (do (give-up! c k :failed) (bump-row! c))
      nil)))

(defn ^:async engage!
  "Walk to the sheep and shear it; end when fruitless three times in a row."
  [c sheep]
  (let [walked (await (walk! c sheep))]
    (when (= :there walked)
      (await (shear! c sheep)))
    (if (>= (:in-row (ctx/mem c) 0) max-in-row)
      (out-of-sheep! c)
      :continue)))

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

(defn ^:async round [c]
  (let [now (ctx/now c)
        {:keys [timeout-s] n :count} (:args c)]
    (ctx/update-mem! c update :started #(or % now))
    (let [m (ctx/mem c)
          cands (candidates c)]
      (cond
        (>= (- now (:started m)) (* 1000 timeout-s)) (finish! c :timeout)
        (= :collect (:phase m)) (await (collect! c))
        (not (shears-carried? c)) (if (seq (:shorn m)) (go-collect! c :shears-broke) (finish! c :no-shears))
        (and n (>= (count (:shorn m)) n)) (go-collect! c :shorn)
        (empty? cands) (out-of-sheep! c)
        :else (await (engage! c (first cands)))))))
