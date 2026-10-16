(ns jobs.items.obtain
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.checks :as checks]
            [engine.contract :as contract]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.items.obtain.base :refer [args-error carried-of holds makes? max-inspections names-of tried! wanted-fields way?]]
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
  "Get :count more of :item (or of any of :any-of, in that order of preference) than were carried at the first round.
  The target is booked once, so a cut or restart goes on toward the same count. One call is the whole attempt: it yields (:continue) only while a child waits on the world.

  Ways, in order (:ways, default all; the target met is done):
  - :chest: the containers the body has seen (perception's seenBlocks, never x-ray) within 32 blocks,
    still there, whose zone or claim allows :take (an open, unzoned chest does; jobs.lib.access). A chest known
    to hold a wanted name (body memory :fetch/stock) is withdrawn from (jobs.storage.withdraw child :take); one of
    unknown stock is walked to and looked into, nearest first, at most 4 per obtain (one that will not open under a
    seen solid block gets it dug, a jobs.blocks.dig child, and is looked into once more); one known not to hold any is
    skipped. Each chest is withdrawn from once. With none usable seen at the first round, one look around first.
  - :craft and :smelt: a chain of crafts planned from the recipes (jobs.items.recipes) over what is carried: logs to
    planks to sticks to the tool, and a crafting table when the chain needs one and none is seen (crafted, then put
    down on a free cell beside the body with a jobs.blocks.place child). Each step is a jobs.items.craft child; a
    furnace product (iron_ingot from raw_iron, charcoal from a log, cooked food; jobs.items.smelt.plan/made-from) is a step
    run by a jobs.items.smelt child, which fetches its own fuel and furnace; a chain with a smelt step needs :smelt, any
    other step :craft. The names are tried in
    order; the first with a plan is made, but one with no smelt step before one with. Three fruitless steps stop it (:tried :craft); a craft child that ran and made nothing stops it at once.
  - :gather: leather and feathers come from a cow or chicken within the sheep radius (a jobs.combat.hunt child, once; penned and kept animals are spared, and the end says which rule spared them); coal comes from a plain or deepslate ore seen; dirt is dug from seen dirt or grass; every raw item a chain lacks must be seen before it starts. A sapling (no recipe) comes from the leaves of its tree the body has seen, broken by
    jobs.gather.get-seeds (at most 60 leaves, one run; the drop is picked up). What a craft chain lacks that has no recipe (logs, coal, stone-tool material; recipes/gatherable?, stone-materials) is
    felled or mined as one child per round, only what the body has seen: logs by jobs.forestry.harvest-wood (of any species, but one only its species makes, oak_log for oak_planks, from that species' trees),
    raw fish (cod, salmon: also the input of a cooked fish's smelt step) by jobs.food.fish from water seen (its rod from a chest or a craft; string has no source, so no rod ends :no-source),
    sugar_cane and bamboo by jobs.gather.get-seeds from a seen stand of two or more,
    obsidian by gather.mine where seen (a diamond pickaxe), else jobs.gather.make-obsidian (water poured on a seen lava source from a stand beside it, mined, the bucket scooped back; no water bucket ends :no-source),
    cobblestone and ore drops (coal, raw_iron, raw_gold, diamond ..: from the plain or deepslate ore seen) by jobs.gather.mine (its pickaxe only from a chest or a craft, never gathered: no mine inside a mine's fetch, so no loop back into obtain). The crafts follow once the chain is whole (a chain that is craftable now
    is crafted first). A child that brings in nothing three times stops it (:tried :gather). A mined need (seen, or
    with :explore) whose block no carried tool harvests gets the cheapest tool that does first (an obtain child, slot
    :pickaxe: wooden_pickaxe for stone and coal, stone_pickaxe for iron); a mine that ends :no-tool (its pickaxe broke) does not count as a nothing run, that tool is got again the same way.
    With :explore, an unseen stone material or ore other than coal is left to the mine child (it tunnels for ore); any other unseen raw need is searched for (jobs.explore.search child, slot :explore, at most 3 per need;
    a notice :obtain.exploring says so); none found stops it (:tried {:gather {:searched ..}}).
  - Nothing else: nothing-possible :no-source; a gather whose last fruitless run could not walk to or from its source
    (walk-causes: :zone-exit, :not-home, :unreachable ..) ends nothing-possible with that cause instead.

  Bad args are impossible :bad-args {:why}.
  Conditions (before it starts): no source {:reason :no-source :why text :item|:any-of} (:why: what
  each way lacks; an unseen oak_log names other species of log seen) when no usable chest that might hold it is seen
  (with chests allowed, checked once the round has looked around), no craft chain makes it from what is carried, and
  nothing seen to gather what a chain lacks (with :explore, a chain whose raw needs are unseen passes: the search child
  solves it, and waits while the body is too hurt); when every raw need is seen and the gather child's
  own check declines (night in a shelter), that cause stands instead.
  Ends :done {:got n :item name} only with the count carried. Nothing-possible {:reason :no-source :why} or
  :table-unreachable when every allowed way was tried, data {:got n :tried {way why} :why}. Limit-reached {:line :job-time
  :reason :timeout} after :spend :job-time seconds (default ::job-time-s); the engine end takes the data of done? ({:got :tried}). A nested call
  for the same item is the engine's :duplicate-call.
  Quote: the plan's figures; yields the count, time a guess per step, no most for a gather.")

(a/defargs args
  {:item {:doc "the item to get" :spec a/item? :default nil}
   :any-of {:doc "items, any one will do, the first preferred (instead of :item)" :spec (a/coll-of a/item?) :default nil}
   :count {:doc "how many more than carried at the start (counts above 64 are taken as 64)" :spec (a/int-in 1 nil) :default 1}
   :fail-minutes {:doc "passed on to the smelt child's fetch; nil: the fetch limits" :spec (a/num-in 0 nil) :default nil}
   :for-plan {:doc "id of the plan whose work this is: a crafting table may stand in its own cells; passed to the craft steps" :spec a/name? :default nil}
   :avoid {:doc "cells [x y z] a crafting table is never put in or dug for (a caller's dig line); passed to the craft steps" :spec (a/coll-of a/position?) :default nil}
   :explore {:doc "walk and look for a raw need not seen: false, true (the ::explore setting) or jobs.explore.search options over it" :spec (a/or-of boolean? map?) :default true}})

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
        n (:count a)]
    (cond
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

(defn can-start? [c] (contract/all [(checks/cause-status :never (bad-args c)) (checks/cause-status :later (no-source c))]))

(defn held-name
  "The name of names carried most of in inventory inv, else the first."
  [inv names]
  (or (holds (into {} (map (juxt :name :count)) inv) names) (first names)))

(defn done?
  "The count carried, no tree a cut left half felled and no child under way (the round fells it whole first, a craft
  picks its table up)."
  [c]
  (let [{:keys [start] :as m} (ctx/mem c)
        inv (u/inventory (:primitives c))
        names (names-of (:args c))
        have (carried-of inv names)
        got (max 0 (- have (:have start 0)))
        left (if start (max 0 (- (:target start) have)) (:count (:args c)))
        data [:units :items :done (if start got 0) :left left]]
    (cond
      (or (nil? start) (pos? left))
      (apply contract/later :short (str "obtain: " left " " (clojure.string/join "/" names) " to get") (conj data :result
             (if start (merge {:got got :tried (:tried m)} (wanted-fields (:args c))) {:got 0})))

      (or (get-in m [:gather :tree]) (checks/child-under-way? c))
      (apply contract/later :wrapping-up "obtain: the count is carried, wrapping up" (conj data :result {:got got :item (held-name inv names)}))

      :else (apply contract/ok :carried (str "obtained " got " " (held-name inv names)) (conj data :result {:got got :item (held-name inv names)})))))

(def step-s "A guess of the seconds one craft step takes, walks included." 5)
(def smelt-s "A guess of the seconds one smelt step takes per item, the furnace's 10 s and its fetches." 15)
(def gather-s "A guess of the seconds one raw need takes to gather: a walk, a fell or a dig." 60)

(defn quote-of
  "The plan's figures: the count as yield; time guessed per craft and smelt step, a gather's most open."
  [c]
  (let [a (:args c)
        names (names-of a)
        n (min 64 (:count a 1))
        pl (or (craft-plan c names n) (gather-plan c names n))
        steps (:steps pl)
        smelts (reduce + 0 (map :count (filter #(= :smelt (:op %)) steps)))
        gathers (count (:gather pl))
        mean (+ (* step-s (count (remove #(= :smelt (:op %)) steps))) (* smelt-s smelts) (* gather-s gathers))
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
                (ctx/update-mem! c assoc :start {:have have :target (+ have (min 64 (:count a)))}))
            {:keys [target] :as start} (:start (ctx/mem c))
            got (max 0 (- have (:have start)))
            m (ctx/mem c)
            data (merge {:got got} (wanted-fields a))]
        (cond
          (get-in m [:gather :tree]) (await (finish-tree! c))
          (>= have target) (ctx/end c :done nil {:got got :item (held-name inv names)})
          (and (way? c :chest) (not (get-in m [:tried :chest])))
          (let [cs (candidates c names)
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
          (let [why (str "no source for " (clojure.string/join "/" names) ": " (no-source-why c names (:count a))
                         (blocked-text (:blocked-chests m))
                         (tried-text (:tried m) (get-in m [:gather :why]) (get-in m [:craft :detail])))]
            (ctx/end c :nothing-possible (merge {:reason :no-source :why why} (wanted-fields a))
                     (assoc data :tried (:tried m) :why why))))))))

(defn reflex-step
  "What a step gives the loop: a reflex job runs one round and may not :continue, so a child's yield is another step
  (a cut ends it)."
  [c r]
  (if (and (:reflex c) (= :continue r)) :again r))

(defn ^:async round
  "The whole attempt in one call: step! again until the count is carried or it ends; :continue only while a child waits on the world (a reflex's obtain steps on instead)."
  [c]
  (await (pace/steps! c (fn ^:async obtain-step []
                          (reflex-step c (await (step! c)))))))
