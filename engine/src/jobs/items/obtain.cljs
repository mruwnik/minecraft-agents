(ns jobs.items.obtain
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.checks :as checks]
            [engine.contract :as contract]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.items.obtain.base :refer [args-error carried-of craft-step-s gather-need-s gathers? holds makes? max-inspections names-of near-chest smelt-item-s shortfall tried! wanted-fields way?]]
            [jobs.items.obtain.bill :as bill]
            [jobs.items.obtain.loose :as loose-way]
            [jobs.items.obtain.chest :refer [blocked-text candidates inspect! withdraw!]]
            [jobs.items.obtain.craft :refer [craft-step!]]
            [jobs.items.obtain.gather :refer [finish-tree! gather-step!]]
            [jobs.items.obtain.need :refer [explorable? gather-held gather-viable?]]
            [jobs.items.obtain.plan :refer [craft-plan gather-plan]]
            [jobs.items.obtain.why :refer [no-source-why tried-text]]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [clojure.string]))

(def doc
  "Carry at least :count of :item (or of any of :any-of, in that order of preference; carried names are added up), or every item of a :bill {item n}. A body that carries them is done at once. One call is the whole attempt: it yields (:continue) only while a child waits on the world.

  Ways (:ways, default all), tried in order, each only on what the body has seen (never x-ray); the README row has the detail. A :gather in :ways stands for the field ways below:
  - :loose: items of the wanted kind lying in sight, picked up by a jobs.items.collect-drops child.
  - :chest: seen containers that allow :take, known stock withdrawn, unknown stock looked into (nearest first, a few per call).
  - :craft and :smelt: a chain planned from the recipes over what is carried (a table is put down when needed); each step a craft or smelt child.
  - :gather, :hunt, :crops, :search: what a chain lacks, or a raw item itself, is felled, mined, fished, cut or scooped (:gather), hunted (:hunt: leather, raw meat, wool, shearing), cut ripe in a field of its own (:crops: wheat, carrot, potato, beetroot) by one child per round; with :search an unseen block or animal need is searched for (jobs.explore.search; :explore false drops it).
  A child that brings in nothing three times ends that way (:tried).
  A bill runs one obtain child per item still missing (not carried or worn), in order, after the furnace material (iron_ingot ..) the missing items need together, which is got in one go and reserved against deposits; each child gets a :spend :job-time of the ::bill-item-s setting. The first child that fails ends the bill with its kind and cause, data {:item the one it stopped at :why}.

  Needs: valid :item or :any-of and :count, or a :bill (else impossible :bad-args); a source (else condition-failed :no-source with :why naming what each way lacks).
  Returns :done {:got n :item name} only with the count carried (the call's :least, default :all, may name fewer) (a bill: {:have [items]}); nothing-possible :no-source, :table-unreachable or the gather's walk cause (:zone-exit ..) when every allowed way was tried, data {:got :tried :why}; limit-reached :timeout after :spend :job-time (default ::job-time-s).
  Quote: the count as yield, time a guess per step, no most for a gather; a bill two gathers' time per missing item.")

(a/defargs args
  {:item {:doc "the item to get" :spec a/item? :default nil}
   :any-of {:doc "items, any one will do, the first preferred (instead of :item)" :spec (a/coll-of a/item?) :default nil}
   :bill {:doc "{item count}: carry at least count of each item, one obtain child after the other (instead of :item, :any-of and :count); the furnace material the items need together is got first" :spec (a/map-of a/item? number?) :default nil}
   :count {:doc "how many to carry at least (carried at the start count)" :spec (a/int-in 1 nil) :default 1}
   :for-plan {:doc "id of the plan whose work this is: a crafting table may stand in its own cells; passed to the craft steps" :spec a/name? :default nil}
   :avoid {:doc "cells [x y z] a crafting table is never put in or dug for (a caller's dig line); passed to the craft steps" :spec (a/coll-of a/position?) :default nil}
   :explore {:doc "false drops the :search way (the base ::explore setting's search for a raw need not seen): fails :no-source instead" :spec boolean? :default true}})

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds an obtain may run (its default :spend :job-time)." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(def target [:item :any-of :bill])

(def safe? ready/threatened)
(def fit? ready/ok)

(def ways [:chest :craft :smelt :loose :gather :hunt :crops :search])

(def walk-causes
  "Gather endings where the body could not get to or from the source (docs/end-statuses.md no-path, and a zone it
  could not leave): the obtain ends with that cause, not :no-source, so a caller can try again later or from elsewhere."
  #{:unreachable :not-back :zone-exit :walk-failed :walk-in-failed :shut-in :exhausted :tree-unreachable
    :no-approach :out-of-reach :one-way :way-dives :detour :door-stuck :cut-again :replan-limit})

(def defaults {:least :all :ways (set ways) :spend {:job-time ::job-time-s}})

(defn source-cause
  "nil when a way would start: a usable seen chest that might hold one of the names, a chain made from what is
  carried, a gather of seen raw needs, or a search for unseen ones. Else the cause: the gather child's own decline,
  or {:reason :no-source :why ..}."
  [c]
  (let [a (:args c)
        names (names-of a)
        n (shortfall c names)]
    (cond
      (:bill a) nil
      (zero? n) nil
      (and (way? c :chest) (seq (candidates c names))) nil
      (and (makes? c) (craft-plan c names n)) nil
      (and (way? c :loose) (seq (loose-way/seen c names))) nil
      (and (gathers? c) (gather-viable? c names n)) nil
      (and (gathers? c) (explorable? c names n)) nil
      :else (or (when (gathers? c) (gather-held c names n))
                (merge {:reason :no-source :why (no-source-why c names n)} (wanted-fields a))))))

(defn bad-args [c]
  (when-let [e (args-error (:args c))]
    {:reason :bad-args :why e}))

(defn no-source
  "Source-cause; a plain no-source waits for the round's look around when chests are allowed. Bad args are bad-args'."
  [c]
  (when-not (args-error (:args c))
    (let [cause (source-cause c)]
      (when-not (and (= :no-source (:reason cause)) (way? c :chest) (not (look/surveyed? c)))
        cause))))

(defn can-start? [c]
  (let [cause (no-source c)]
    (contract/all [(checks/cause-status :never (bad-args c)) (checks/cause-status (:kind (meta cause) :later) cause)])))

(defn held-name
  "The name of names carried most of in inventory inv, else the first."
  [inv names]
  (or (holds (into {} (map (juxt :name :count)) inv) names) (first names)))

(defn done-of
  "The count carried, no tree a cut left half felled and no child under way (the round fells it whole first, a table in a plan cell
  dug up)."
  [c]
  (let [{:keys [start] :as m} (ctx/mem c)
        inv (u/inventory (:primitives c))
        names (names-of (:args c))
        have (carried-of inv names)
        got (max 0 (- have (:have start 0)))
        left (max 0 (- (:count (:args c)) have))
        data [:units :items :done (if start got 0) :left left]]
    (cond
      (pos? left)
      (apply contract/later :short (str "obtain: " left " " (clojure.string/join "/" names) " to get") (conj data :result
             (if start (merge {:got got :tried (:tried m)} (wanted-fields (:args c))) {:got 0})))

      (or (get-in m [:gather :tree]) (checks/child-under-way? c))
      (apply contract/later :wrapping-up "obtain: the count is carried, wrapping up" (conj data :result {:got got :item (held-name inv names)}))

      :else (apply contract/ok :carried (if (zero? got)
                                          (str "already carried " have " " (held-name inv names) "; pass :count N for more")
                                          (str "obtained " got " " (held-name inv names))) (conj data :result {:got got :item (held-name inv names)})))))

(defn done?
  "Ok when the count (or every item of the bill) is carried and no child is under way."
  [c]
  (if (:bill (:args c))
    (let [left (count (bill/todo c))]
      (if (pos? left)
        (contract/later :short (str "obtain: " left " item(s) of the bill to get") :units :items :done 0 :left left)
        (contract/ok :carried "obtained the bill" :units :items :done 0 :left 0 :result {:have (vec (keys (:bill (:args c))))})))
    (done-of c)))

(defn quote-item
  "The plan's figures: the count as yield; time guessed per craft and smelt step, a gather's most open."
  [c]
  (let [a (:args c)
        names (names-of a)
        n (max 1 (shortfall c names))
        pl (or (craft-plan c names n) (gather-plan c names n))
        steps (:steps pl)
        smelts (reduce + 0 (map :count (filter #(= :smelt (:op %)) steps)))
        gathers (count (:gather pl))
        step-s (craft-step-s)
        mean (+ (* step-s (count (remove #(= :smelt (:op %)) steps))) (* (smelt-item-s) smelts) (* (gather-need-s) gathers))
        item (or (:item pl) (first names))]
    {:uses {:job-time [0 (max step-s mean) (if (or (nil? pl) (pos? gathers)) ##Inf (* 3 (max step-s mean)))]}
     :yields {:items {item [0 n n]}}}))

(defn quote-of
  "A single item: quote-item. A bill: two gathers' time for each item still to get."
  [c]
  (if (:bill (:args c))
    {:uses {:job-time [0 (* (count (bill/todo c)) 2 (gather-need-s)) ##Inf]}}
    (quote-item c)))

(defn survey-first?
  "Before the first round, with chests allowed, none usable seen that might hold one of names and no look around yet:
  the round looks around first, so a chest behind the body is preferred to a craft or gather."
  [c names]
  (and (not (:start (ctx/mem c)))
       (not (look/surveyed? c))
       (way? c :chest)
       (empty? (candidates c names))))

(defn chests-to-try
  "The candidates, minus the ones further than near-chest when a chain from what is carried makes the need: crafting now
  is cheaper than a long walk, to a chest that may not hold it or to one that does."
  [c m names n]
  (let [cs (candidates c names)]
    (if (and (makes? c) (not (get-in m [:tried :craft])) (craft-plan c names n))
      (filter #(u/within? (u/self-pos c) (:pos %) (near-chest)) cs)
      cs)))

(defn ^:async step! [c]
  (let [a (:args c)
        names (names-of a)
        inv (u/inventory (:primitives c))
        have (carried-of inv names)]
    (cond
      (survey-first? c names)
      (do (await (look/survey-until! c #(nil? (source-cause c))))
          (ctx/update-mem! c assoc :looked (look/cell-of (u/self-pos c)))
          (if-let [cause (source-cause c)]
            (ctx/end c :condition-failed cause (merge {:got 0} (wanted-fields a) (when (map? cause) (select-keys cause [:why]))))
            :again))

      :else
      (let [_ (when-not (:start (ctx/mem c))
                (ctx/update-mem! c update :craft dissoc :table-unreachable)
                (ctx/update-mem! c assoc :start {:have have :target (:count a)}))
            {:keys [target] :as start} (:start (ctx/mem c))
            got (max 0 (- have (:have start)))
            m (ctx/mem c)
            data (merge {:got got} (wanted-fields a))]
        (cond
          (get-in m [:gather :tree]) (await (finish-tree! c))
          (>= have target) (ctx/end c :done nil {:got got :item (held-name inv names)})
          (and (way? c :loose) (not (get-in m [:tried :loose])) (seq (loose-way/seen c names)))
          (await (loose-way/step! c names))
          (and (way? c :chest) (not (get-in m [:tried :chest]))
               (or (seq (chests-to-try c m names (- target have))) (not (seq (candidates c names)))))
          (let [cs (chests-to-try c m names (- target have))
                known (first (filter :stock cs))
                unknown (first (remove :stock cs))]
            (cond
              known (await (withdraw! c (:pos known) (:stock known) names have (boolean (and (makes? c) (craft-plan c names (- target have))))))
              (and unknown (< (:inspected m 0) (max-inspections))) (await (inspect! c (:pos unknown)))
              :else (tried! c :chest (cond (seq (:blocked-chests m)) :blocked (or (seq (:done-chests m)) (pos? (:inspected m 0))) :lacking :else :none-seen))))
          (and (makes? c) (not (get-in m [:tried :craft]))
               (or (not (gathers? c)) (get-in m [:craft :step]) (get-in m [:craft :table-unreachable]) (craft-plan c names (- target have))))
          (await (craft-step! c names have target))
          (and (gathers? c) (not (get-in m [:tried :gather])))
          (await (gather-step! c names have target))
          (= :table-unreachable (get-in m [:tried :craft]))
          (ctx/end c :nothing-possible :table-unreachable
                   (assoc data :tried (:tried m)
                          :why (str "the seen crafting table cannot be reached and no other way makes it: "
                                    (no-source-why c names (shortfall c names)))))
          (walk-causes (get-in m [:tried :gather]))
          (let [k (get-in m [:tried :gather])
                why (str "could not get to a source of " (clojure.string/join "/" names) ": gathering ended " (pr-str k)
                         (some->> (get-in m [:gather :why]) (str ": ")))]
            (ctx/end c :nothing-possible (merge {:reason k :why why} (wanted-fields a)) (assoc data :tried (:tried m) :why why)))
          :else
          (let [tried (tried-text (:tried m) (get-in m [:gather :why]) (get-in m [:craft :detail]))
                why (str "no source for " (clojure.string/join "/" names) ": "
                         (some-> tried (subs 2) (str "; "))
                         (no-source-why c names (shortfall c names))
                         (blocked-text (:blocked-chests m)))]
            (ctx/end c :nothing-possible (merge {:reason :no-source :why why} (wanted-fields a))
                     (assoc data :tried (:tried m) :why why))))))))

(defn ^:async round
  "The whole attempt in one call: step! again until the count is carried or it ends; :continue only while a child waits on the world."
  [c]
  (await (if (:bill (:args c)) (bill/run! c) (pace/steps! c #(step! c)))))
