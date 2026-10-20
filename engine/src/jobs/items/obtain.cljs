(ns jobs.items.obtain
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.checks :as checks]
            [engine.contract :as contract]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.items.obtain.base :refer [args-error carried-of craft-step-s gather-need-s holds makes? max-inspections names-of near-chest smelt-item-s shortfall tried! wanted-fields way?]]
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
  "Carry at least :count of :item (or of any of :any-of, in that order of preference; carried names are added up). A body that carries them is done at once. One call is the whole attempt: it yields (:continue) only while a child waits on the world.

  Ways (:ways, default all), tried in order, each only on what the body has seen (never x-ray); the README row has the detail:
  - :chest: seen containers that allow :take, known stock withdrawn, unknown stock looked into (nearest first, a few per call).
  - :craft and :smelt: a chain planned from the recipes over what is carried (a table is put down when needed); each step a craft or smelt child.
  - :gather: what a chain lacks, or a raw item itself, is felled, mined, fished, cut, scooped, sheared or hunted by one child per round; with :explore an unseen need is searched for (jobs.explore.search).
  A child that brings in nothing three times ends that way (:tried).

  Needs: valid :item or :any-of and :count (else impossible :bad-args); a source (else condition-failed :no-source with :why naming what each way lacks).
  Returns :done {:got n :item name} only with the count carried; nothing-possible :no-source, :table-unreachable or the gather's walk cause (:zone-exit, :not-home ..) when every allowed way was tried, data {:got :tried :why}; limit-reached :timeout after :spend :job-time (default ::job-time-s).
  Quote: the count as yield, time a guess per step, no most for a gather.")

(a/defargs args
  {:item {:doc "the item to get" :spec a/item? :default nil}
   :any-of {:doc "items, any one will do, the first preferred (instead of :item)" :spec (a/coll-of a/item?) :default nil}
   :count {:doc "how many to carry at least (carried at the start count)" :spec (a/int-in 1 nil) :default 1}
   :for-plan {:doc "id of the plan whose work this is: a crafting table may stand in its own cells; passed to the craft steps" :spec a/name? :default nil}
   :avoid {:doc "cells [x y z] a crafting table is never put in or dug for (a caller's dig line); passed to the craft steps" :spec (a/coll-of a/position?) :default nil}
   :explore {:doc "walk and look for a raw need not seen (the base ::explore setting's search); false fails :no-source instead" :spec boolean? :default true}})

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds an obtain may run (its default :spend :job-time)." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(def target [:item :any-of])

(def safe? ready/threatened)
(def fit? ready/ok)

(def ways [:chest :craft :smelt :gather])

(def walk-causes
  "Gather endings where the body could not get to or from the source (docs/end-statuses.md no-path, and a zone it
  could not leave): the obtain ends with that cause, not :no-source, so a caller can try again later or from elsewhere."
  #{:unreachable :not-home :not-back :zone-exit :walk-failed :walk-in-failed :shut-in :exhausted :tree-unreachable
    :no-approach :out-of-reach :one-way :way-dives :detour :door-stuck :cut-again :replan-limit})

(def defaults {:ways (set ways) :spend {:job-time ::job-time-s}})

(defn source-cause
  "nil when a way would start: a usable seen chest that might hold one of the names, a chain made from what is
  carried, a gather of seen raw needs, or a search for unseen ones. Else the cause: the gather child's own decline,
  or {:reason :no-source :why ..}."
  [c]
  (let [a (:args c)
        names (names-of a)
        n (shortfall c names)]
    (cond
      (zero? n) nil
      (and (way? c :chest) (seq (candidates c names))) nil
      (and (makes? c) (craft-plan c names n)) nil
      (and (way? c :gather) (gather-viable? c names n)) nil
      (and (way? c :gather) (explorable? c names n)) nil
      :else (or (when (way? c :gather) (gather-held c names n))
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

(defn done?
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

      :else (apply contract/ok :carried (str "obtained " got " " (held-name inv names)) (conj data :result {:got got :item (held-name inv names)})))))

(defn quote-of
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

(defn survey-first?
  "Before the first round, with chests allowed, none usable seen that might hold one of names and no look around yet:
  the round looks around first, so a chest behind the body is preferred to a craft or gather."
  [c names]
  (and (not (:start (ctx/mem c)))
       (not (look/surveyed? c))
       (way? c :chest)
       (empty? (candidates c names))))

(defn chests-to-try
  "The candidates, minus the unlooked-in far ones when a chain from what is carried makes the need: crafting now is
  cheaper than a long walk to a chest that may not hold it."
  [c m names n]
  (let [cs (candidates c names)]
    (if (and (makes? c) (not (get-in m [:tried :craft])) (craft-plan c names n))
      (filter #(or (:stock %) (u/within? (u/self-pos c) (:pos %) (near-chest))) cs)
      cs)))

(defn ^:async step! [c]
  (let [a (:args c)
        names (names-of a)
        inv (u/inventory (:primitives c))
        have (carried-of inv names)]
    (cond
      (survey-first? c names)
      (do (await (look/survey! c))
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
          (and (way? c :chest) (not (get-in m [:tried :chest]))
               (or (seq (chests-to-try c m names (- target have))) (not (seq (candidates c names)))))
          (let [cs (chests-to-try c m names (- target have))
                known (first (filter :stock cs))
                unknown (first (remove :stock cs))]
            (cond
              known (await (withdraw! c (:pos known) (:stock known) names have))
              (and unknown (< (:inspected m 0) (max-inspections))) (await (inspect! c (:pos unknown)))
              :else (tried! c :chest (cond (seq (:blocked-chests m)) :blocked (or (seq (:done-chests m)) (pos? (:inspected m 0))) :lacking :else :none-seen))))
          (and (makes? c) (not (get-in m [:tried :craft]))
               (or (not (way? c :gather)) (get-in m [:craft :step]) (get-in m [:craft :table-unreachable]) (craft-plan c names (- target have))))
          (await (craft-step! c names have target))
          (and (way? c :gather) (not (get-in m [:tried :gather])))
          (await (gather-step! c names have target))
          (= :table-unreachable (get-in m [:tried :craft]))
          (ctx/end c :nothing-possible :table-unreachable (assoc data :tried (:tried m)))
          (walk-causes (get-in m [:tried :gather]))
          (let [k (get-in m [:tried :gather])
                why (str "could not get to a source of " (clojure.string/join "/" names) ": gathering ended " (pr-str k)
                         (some->> (get-in m [:gather :why]) (str ": ")))]
            (ctx/end c :nothing-possible (merge {:reason k :why why} (wanted-fields a)) (assoc data :tried (:tried m) :why why)))
          :else
          (let [why (str "no source for " (clojure.string/join "/" names) ": " (no-source-why c names (shortfall c names))
                         (blocked-text (:blocked-chests m))
                         (tried-text (:tried m) (get-in m [:gather :why]) (get-in m [:craft :detail])))]
            (ctx/end c :nothing-possible (merge {:reason :no-source :why why} (wanted-fields a))
                     (assoc data :tried (:tried m) :why why))))))))

(defn ^:async round
  "The whole attempt in one call: step! again until the count is carried or it ends; :continue only while a child waits on the world."
  [c]
  (await (pace/steps! c #(step! c))))
