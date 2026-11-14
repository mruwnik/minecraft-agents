(ns jobs.items.obtain
  (:require [jobs.lib.death-spot :as death-spot]
            [jobs.lib.ready :as ready]
            [jobs.lib.checks :as checks]
            [jobs.lib.cost :as cost]
            [jobs.lib.fetch-time :as fetch-time]
            [engine.contract :as contract]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.items.obtain.base :refer [args-error carried-of gathers? holds makes? max-inspections names-of own-worth shortfall tried! wanted-fields way?]]
            [jobs.items.obtain.bill :as bill]
            [jobs.items.obtain.loose :as loose-way]
            [jobs.items.obtain.chest :refer [blocked-text candidates cheapest! inspect! withdraw!]]
            [jobs.items.obtain.craft :refer [craft-step!]]
            [jobs.items.obtain.gather :refer [finish-tree! gather-step!]]
            [jobs.items.obtain.need :refer [explorable? explorable-needs gather-held gather-viable?]]
            [jobs.items.obtain.chain :refer [craft-plan smelt-s]]
            [jobs.items.obtain.plan :refer [craft-s gather-plan gathers-s walk-s]]
            [jobs.items.obtain.price :refer [gather-s price-of]]
            [jobs.items.obtain.seen :refer [seen-of]]
            [jobs.items.obtain.why :refer [gather-fix no-source-why tried-text]]
            [jobs.lib.look :as look]
            [jobs.lib.next-call :as next-call]
            [jobs.lib.pace :as pace]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.util :as u]
            [clojure.string]))

(defn cleanup
  "However the call ends: the bill's reserve entries are dropped."
  [c _]
  (reserve/release! c)
  (ctx/end c :done nil))

(def meta {:tags [:goal]})

(def doc
  "Carry at least :count of :item (or of any of :any-of, in that order of preference; carried names are added up), or every item of a :bill {item n}. A body that carries them is done at once. One call is the whole attempt: it yields (:continue) only while a child waits on the world.

  Ways (:ways, default all), tried in order, each only on what the body has seen (never x-ray); the README row has the detail. A :gather in :ways stands for the field ways below:
  - :loose: items of the wanted kind lying in sight, picked up by a jobs.items.collect-drops child.
  - :chest: seen containers that allow :take, known stock withdrawn, unknown stock looked into (nearest first, a few per call).
  - :craft and :smelt: a chain planned from the recipes over what is carried (a table is put down when needed); each step a craft or smelt child.
  - :gather, :hunt, :crops, :search: what a chain lacks, or a raw item itself, is felled, mined, fished, cut or scooped (:gather), hunted (:hunt: leather, raw meat, wool, shearing), cut ripe in a field of its own (:crops: wheat, carrot, potato, beetroot) by one child per round; with :search an unseen block or animal need is searched for (jobs.explore.search; :explore false drops it).
  A child that brings in nothing three times ends that way (:tried); a mine for an unseen ore that cut new ground looked, so it mines on while another run pays out of the search worth left (gather/strip-on!).
  A bill runs one obtain child per item still missing (not carried or worn), in order, after the furnace material (iron_ingot ..) the missing items need together, which is got in one go and reserved against deposits; each child on its own default :spend :job-time. The first child that fails ends the bill with its kind and cause, data {:item the one it stopped at :why}.

  A death since the call began starts it again from what is carried: :got counts from the respawn.

  Needs: valid :item or :any-of and :count, or a :bill (else impossible :bad-args); a source (else condition-failed :no-source with :why naming what each way lacks); a way priced within the call's worth (jobs.items.obtain.price/price-of: walk there and back, the death it risks; else :later :not-worth; no way known is :not-worth only for a child given a :worth; a :look instead, before any look round, while chests are allowed and none is seen or a raw need is unseen, and a look round is within the worth: the price is judged after it).
  Worth (ctx/worth): the caller's, at the root worth-of; each walk of a search for an unseen need is priced against it, within the job time left.
  Returns :done {:got n :item name} only with the count carried (the call's :least, default :all, may name fewer) (a bill: {:have [items]}); nothing-possible :no-source (with the craft child's cause as :cause when a gather also ended), :table-unreachable or the gather's walk cause (:zone-exit ..) when every allowed way was tried; the start refusal of a seen source's child (:exposed ..: nothing-possible for its :never, condition-failed for its :later), data {:got :tried :why :next} (:next: jobs.lib.next-call, the gather for the first unseen raw need, for an animal an explore.search for it); limit-reached :timeout after :spend :job-time (default-time: fetch-time/items-s, so a crafted item's budget counts its recipe tree; a bill its items' summed).
  Quote: the count as yield, time a guess per step, no most for a gather; a bill two gathers' time per missing item.")

(a/defargs args
  {:item {:doc "the item to get" :spec a/item? :default nil}
   :any-of {:doc "items, any one will do, the first preferred (instead of :item)" :spec (a/coll-of a/item?) :default nil}
   :bill {:doc "{item count}: carry at least count of each item, one obtain child after the other (instead of :item, :any-of and :count); the furnace material the items need together is got first" :spec (a/map-of a/item? number?) :default nil}
   :count {:doc "how many to carry at least (carried at the start count)" :spec (a/int-in 1 nil) :default 1}
   :for-plan {:doc "id of the plan whose work this is: a crafting table may stand in its own cells; passed to the craft steps" :spec a/name? :default nil}
   :avoid {:doc "cells [x y z] a crafting table is never put in or dug for and a climb out of the rock never starts through (a caller's dig line); passed to the craft steps" :spec (a/coll-of a/position?) :default nil}
   :center {:doc "a cell [x y z] the searches for an unseen block or animal centre on (a build's site): each starts there and stays within the ::site-radius setting of it; nil: where the body stands" :spec a/position? :default nil}
   :sky {:doc "the sky's darkening (0-15) the start check's price judges the light of its walks by (jobs.lib.walk-risk/estimate :sky); nil: as now" :spec (a/num-in 0 15) :default nil}
   :ignore-zones? :shared
   :explore {:doc "false drops the :search way (the search, priced by what the need is worth, for a raw need not seen): fails :no-source instead" :spec boolean? :default true}})

(defn default-time
  "The call's default :spend :job-time: fetch-time/items-s of its count of the item (any of :any-of); a bill the sum of its items' (each child runs on its own)."
  [{:keys [count bill] :as a}]
  (if (seq bill)
    (reduce + 0 (map (fn [[item n]] (default-time {:item item :count n})) bill))
    (fetch-time/items-s (names-of a) (or count 1))))

(def target [:item :any-of :bill])

(def safe? ready/threatened)
(def fit? ready/ok)

(def ways [:chest :craft :smelt :loose :gather :hunt :crops :search])

(def walk-causes
  "Gather endings where the body could not get to or from the source (docs/end-statuses.md no-path, and a zone it
  could not leave): the obtain ends with that cause, not :no-source, so a caller can try again later or from elsewhere."
  #{:unreachable :not-back :zone-exit :walk-failed :walk-in-failed :shut-in :exhausted :tree-unreachable
    :no-approach :out-of-reach :one-way :way-dives :detour :door-stuck :cut-again :replan-limit})

(def defaults {:least :all :ways (set ways) :spend {:job-time default-time}})

(defn source-seen?
  "True when a way would start from what is seen or carried: a usable seen chest that might hold one of the names, a chain
  made from what is carried, a loose item, or a gather of seen raw needs (not a search for unseen ones)."
  [c]
  (let [a (:args c)
        names (names-of a)
        n (shortfall c names)]
    (boolean (or (:bill a)
                 (zero? n)
                 (and (way? c :chest) (seq (candidates c names)))
                 (and (makes? c) (craft-plan c names n))
                 (and (way? c :loose) (seq (loose-way/seen c names)))
                 (and (gathers? c) (gather-viable? c names n))))))

(defn source-cause
  "nil when a way would start (source-seen?, or a search for unseen needs). Else the cause: the gather child's own
  decline, or {:reason :no-source :why .. :next} (:next: why/gather-fix, jobs.lib.next-call)."
  [c]
  (let [a (:args c)
        names (names-of a)
        n (shortfall c names)]
    (cond
      (source-seen? c) nil
      (and (gathers? c) (explorable? c names n)) nil
      :else (or (when (gathers? c) (gather-held c names n))
                (merge {:reason :no-source :why (no-source-why c names n)} (wanted-fields a)
                       (when-let [fix (gather-fix c names n)] {:next [fix]}))))))

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

(defn worth-of
  "The worth of a root call: what its count of the item (a bill all of it) is to the body (cost/find-worth)."
  [c]
  (let [a (:args c)]
    (if (:bill a) (cost/items-find-worth (:bill a)) (own-worth c (names-of a) (:count a 1)))))

(defn survey-first?
  "Before the first round, with chests allowed, none usable seen that might hold one of names and no look around yet:
  the round looks around first, so a chest behind the body is preferred to a craft or gather."
  [c names]
  (and (not (:start (ctx/mem c)))
       (not (look/surveyed? c))
       (way? c :chest)
       (empty? (candidates c names))))

(defn not-worth
  "{:reason :not-worth :price :worth} when the call (no bill) is worth less (ctx/worth) than price-of, the cheapest way
  known; with none (##Inf) only for a child its caller gave a :worth, to weigh that price: else (a root too) the source checks say what is lacking."
  [c]
  (let [{:keys [bill] given :worth} (:args c)
        worth (ctx/worth c)]
    (when-not (or bill (args-error (:args c)))
      (let [s (price-of c)]
        (when-not (or (cost/affords? s worth) (and (not (and (seq (:slots c)) given)) (not (js/isFinite s))))
          {:reason :not-worth :price s :worth worth})))))

(defn unseen-raw?
  "Before the first round with no look around yet, whether a raw need the price searches for is not seen."
  [c names]
  (and (not (:start (ctx/mem c)))
       (not (look/surveyed? c))
       (gathers? c)
       (boolean (some #(not (seen-of c %)) (explorable-needs c names (shortfall c names))))))

(defn look-first?
  "Whether a dear call looks around first for no more than its worth (a look round's looks): a chest behind the body
  (survey-first?) or a raw need beside it, unseen (unseen-raw?), may make it cheap."
  [c]
  (let [names (names-of (:args c))]
    (and (or (survey-first? c names) (unseen-raw? c names))
         (cost/affords? (cost/look-s (look/round-size)) (ctx/worth c)))))

(defn can-start? [c]
  (let [cause (no-source c)
        dear (not-worth c)]
    (contract/all [(checks/cause-status :never (bad-args c)) (checks/cause-status (:kind (cljs.core/meta cause) :later) cause)
                   (checks/cause-status (if (and dear (look-first? c)) :look :later) dear
                                        (when dear (str "obtain: costs " (js/Math.round (:price dear)) " s, worth " (js/Math.round (:worth dear)) " s")))])))

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
  "The plan's figures: the count as yield; time per craft and smelt step and the raw needs' gathers (plan/gathers-s),
  a gather's most open."
  [c]
  (let [a (:args c)
        names (names-of a)
        n (max 1 (shortfall c names))
        pl (or (craft-plan c names n) (gather-plan c names n))
        steps (:steps pl)
        smelts (reduce + 0 (map :count (filter #(= :smelt (:op %)) steps)))
        gathers (count (:gather pl))
        step-s (cost/craft-step-s)
        mean (+ (* step-s (count (remove #(= :smelt (:op %)) steps))) (smelt-s c smelts) (if (pos? gathers) (gathers-s c pl) 0))
        item (or (:item pl) (first names))]
    {:uses {:job-time [0 (max step-s mean) (if (or (nil? pl) (pos? gathers)) ##Inf (* 3 (max step-s mean)))]}
     :yields {:items {item [0 n n]}}}))

(defn quote-of
  "A single item: quote-item. A bill: the cost/fetch-value of the items still to get, from nothing (each child runs on its own)."
  [c]
  (if (:bill (:args c))
    {:uses {:job-time [0 (reduce + 0 (map (fn [[item n]] (:value (cost/fetch-value [{:name item :count n}]))) (bill/todo c))) ##Inf]}}
    (quote-item c)))

(defn chests-to-try
  "The candidates whose walk there and back (with its risk) costs no more than the cheapest other way that would start
  (a craft from what is carried, a gather of seen needs; a way already tried is out): a long walk to a chest, one that
  may not hold the item or one that does, is dearer than making it now. With no other way, all of them."
  [c m names n]
  (let [cs (candidates c names)
        other (when (seq cs)
                (apply min ##Inf (keep identity [(when-not (get-in m [:tried :craft]) (craft-s c names n))
                                                 (when-not (get-in m [:tried :gather]) (gather-s c names n))])))]
    (if (infinite? other) cs (filter #(<= (walk-s c (:pos %)) other) cs))))

(defn ^:async step! [c]
  (let [a (:args c)
        names (names-of a)
        inv (u/inventory (:primitives c))
        have (carried-of inv names)]
    (cond
      (survey-first? c names)
      (do (await (look/survey-until! c #(source-seen? c)))
          (look/note-surveyed! c)
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
                known (await (cheapest! c (filter :stock cs)))
                unknown (when-not known (await (cheapest! c (remove :stock cs))))]
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
                   (next-call/with-next (assoc data :tried (:tried m)
                                               :why (str "the seen crafting table cannot be reached and no other way makes it: "
                                                         (no-source-why c names (shortfall c names))))
                                        c 'jobs.items.obtain args nil))
          (get-in m [:gather :refusal])
          (let [{:keys [kind text job cause]} (get-in m [:gather :refusal])
                why (str "could not get to a source of " (clojure.string/join "/" names) ": " job " declines to start at worth "
                         (js/Math.round (ctx/worth c)) " s: " text)]
            (ctx/end c (if (= :later kind) :condition-failed :nothing-possible) (merge cause {:why why} (wanted-fields a))
                     (next-call/with-next (assoc data :tried (:tried m) :why why) c 'jobs.items.obtain args nil)))
          (walk-causes (get-in m [:tried :gather]))
          (let [k (get-in m [:tried :gather])
                why (str "could not get to a source of " (clojure.string/join "/" names) ": gathering ended " (pr-str k)
                         (some->> (get-in m [:gather :why]) (str ": ")))]
            (ctx/end c :nothing-possible (merge {:reason k :why why} (wanted-fields a))
                     (next-call/with-next (assoc data :tried (:tried m) :why why) c 'jobs.items.obtain args nil)))
          (and (get-in m [:tried :craft]) (get-in m [:craft :cause]))
          (let [k (get-in m [:craft :cause])
                gathered (get-in m [:tried :gather])
                why (str "could not make " (clojure.string/join "/" names) ": crafting ended " (pr-str k)
                         (some->> (get-in m [:craft :detail]) (str ": "))
                         (when gathered (str "; gathering ended " (pr-str gathered) (some->> (get-in m [:gather :why]) (str ": ")))))]
            (ctx/end c :nothing-possible (merge {:reason (if gathered :no-source k) :why why} (when gathered {:cause k}) (wanted-fields a))
                     (next-call/with-next (assoc data :tried (:tried m) :why why) c 'jobs.items.obtain args nil
                                          (gather-fix c names (shortfall c names)))))
          :else
          (let [tried (tried-text (:tried m) (get-in m [:gather :why]) (get-in m [:craft :detail]))
                why (str "no source for " (clojure.string/join "/" names) ": "
                         (some-> tried (subs 2) (str "; "))
                         (no-source-why c names (shortfall c names))
                         (blocked-text (:blocked-chests m)))]
            (ctx/end c :nothing-possible (merge {:reason :no-source :why why} (wanted-fields a))
                     (next-call/with-next (assoc data :tried (:tried m) :why why) c 'jobs.items.obtain args nil
                                          (gather-fix c names (shortfall c names))))))))))

(defn restarted
  "The job memory after a death: the children, the start count, the craft step, the ways tried and the bill's partial
  went with the body (a half-felled tree stays to fell); the call starts from now."
  [m now]
  (-> m (dissoc :children :start :craft :tried :partial) (update :gather select-keys [:tree]) (assoc :since now)))

(defn ^:async round
  "The whole attempt in one call: step! again until the count is carried or it ends; :continue only while a child waits on the world.
  A death since the call began (body memory :died) starts it again from what is carried (restarted)."
  [c]
  (let [m (ctx/mem c)
        died (ctx/latest c :died)]
    (cond
      (nil? (:since m)) (ctx/update-mem! c assoc :since (ctx/now c))
      (death-spot/died-after? died (:since m))
      (do (ctx/update-mem! c restarted (ctx/now c))
          (ctx/emit! c :obtain.resumed :info {:text "died since the obtain began: starting again from what is carried"}))))
  (await (if (:bill (:args c)) (bill/run! c) (pace/steps! c #(step! c)))))
