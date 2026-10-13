(ns jobs.items.obtain
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.items.obtain.base :refer [args-error carried-of holds limits max-gap-ms max-inspections names-of tried! wanted-fields]]
            [jobs.items.obtain.chest :refer [candidates inspect! withdraw!]]
            [jobs.items.obtain.craft :refer [craft-step!]]
            [jobs.items.obtain.gather :refer [finish-tree! gather-step!]]
            [jobs.items.obtain.need :refer [explorable? gather-held? gather-viable?]]
            [jobs.items.obtain.plan :refer [craft-plan]]
            [jobs.items.obtain.why :refer [no-source-why tried-text]]
            [jobs.lib.body :as body]
            [jobs.lib.declined :as declined]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [clojure.string]))

(def doc
  "Get :count more of :item (or of any of :any-of, in that order of preference) than were carried at the first round.
  The target is booked once, so a cut or restart goes on toward the same count. One call is the whole attempt: it yields (:continue) only while a child waits on the world.

  Sources, in order (carried, chests, crafting, gathering):
  - Carried: the target is met, done.
  - :chest (in :how): the containers the body has seen (perception's seenBlocks, never x-ray) within 32 blocks,
    still there, whose zone or claim allows :take (an open, unzoned chest does; jobs.lib.access). A chest known
    to hold a wanted name (body memory :fetch/stock) is withdrawn from (jobs.storage.withdraw child :take); one of
    unknown stock is walked to and looked into, nearest first, at most 4 per obtain; one known not to hold any is
    skipped. Each chest is withdrawn from once. With none usable seen at the first round, one look around first.
  - :craft (in :how): a chain of crafts planned from the recipes (jobs.items.recipes) over what is carried: logs to
    planks to sticks to the tool, and a crafting table when the chain needs one and none is seen (crafted, then put
    down on a free cell beside the body with a jobs.blocks.place child). Each step is a jobs.items.craft child; a
    furnace product (iron_ingot from raw_iron, charcoal from a log, cooked food; jobs.items.smelt.plan/made-from) is a step
    run by a jobs.items.smelt child, which fetches its own fuel and furnace one :depth deeper. The names are tried in
    order; the first with a plan is made, but one with no smelt step before one with. Three fruitless steps stop it (:tried :craft); a craft child that ran and made nothing stops it at once.
  - :gather (in :how): coal comes from a plain or deepslate ore seen; dirt is dug from seen dirt or grass; every raw item a chain lacks must be seen before it starts. A sapling (no recipe) comes from the leaves of its tree the body has seen, broken by
    jobs.gather.get-seeds (at most 60 leaves, one run; the drop is picked up). What a craft chain lacks that has no recipe (logs, coal, stone-tool material; recipes/gatherable?, stone-materials) is
    felled or mined as one child per round, only what the body has seen: logs by jobs.forestry.harvest-wood (of any species, but one only its species makes, oak_log for oak_planks, from that species' trees),
    raw fish (cod, salmon: also the input of a cooked fish's smelt step) by jobs.food.fish from water seen (its rod from a chest or a craft; string has no source, so no rod ends :no-source),
    obsidian by gather.mine where seen (a diamond pickaxe), else jobs.gather.make-obsidian (water poured on a seen lava source from a stand beside it, mined, the bucket scooped back; no water bucket ends :no-source),
    cobblestone and ore drops (coal, raw_iron, raw_gold, diamond ..: from the plain or deepslate ore seen) by jobs.gather.mine (its pickaxe only from a chest or a craft, never gathered: no mine inside a mine's fetch, so no loop back into obtain). The crafts follow once the chain is whole (a chain that is craftable now
    is crafted first). A child that brings in nothing three times stops it (:tried :gather). A mined need (seen, or
    with :explore) whose block no carried tool harvests gets the cheapest tool that does first (an obtain child, slot
    :pickaxe: wooden_pickaxe for stone and coal, stone_pickaxe for iron); a mine that ends :no-tool (its pickaxe broke) does not count as a nothing run, that tool is got again the same way.
    With :explore, an unseen stone material or ore other than coal is left to the mine child (it tunnels for ore); any other unseen raw need is searched for (jobs.explore.search child, slot :explore, at most 3 per need;
    a notice :obtain.exploring says so); none found stops it (:tried {:gather {:searched ..}}).
  - Nothing else: stopped :no-source with :tried.

  The check waits {:reason :no-source :why text :item|:any-of} (:why: what each source lacks; an unseen oak_log names other species of log seen) before the first round (once the round has looked around, when chests are allowed) when nothing is carried enough, no
  usable chest that might hold it is seen and no craft chain makes it from what is carried, and nothing seen to gather what a chain lacks (with :explore, a chain whose raw needs are unseen passes: the search child solves it; when every raw need is seen and the gather child's own check waits (night in a shelter), that wait stands instead). Ends {:status :done :got n :item name} or {:status :stopped
  :reason r :got n :tried {..}}, r :no-source, :cycle (a wanted name is on :chain), :timeout (:minutes without what is
  carried changing) or :bad-args. :depth is the nesting left for sources that need fetches of their own (not used yet: crafting
  plans its whole chain; a gather child fetches its own tools).")

(a/defargs args
  {:item {:doc "the item to get" :spec a/item? :default nil}
   :any-of {:doc "items, any one will do, the first preferred (instead of :item)" :spec (a/coll-of a/item?) :default nil}
   :count {:doc "how many more than carried at the start (counts above 64 are taken as 64)" :spec (a/int-in 1 nil) :default 1}
   :how {:doc "sources to use, a subset of #{:chest :craft :gather}; nil: all" :spec (a/coll-of #{:chest :craft :gather}) :default nil}
   :depth {:doc "nested fetches left; nil: the fetch limits (jobs.lib.fetch)" :spec (a/int-in 0 nil) :default nil}
   :minutes {:doc "minutes without a change in what is carried before it stops :timeout (a long chain keeps going while it makes progress); nil: the fetch limits" :spec (a/num-in 0 nil) :default nil}
   :fail-minutes {:doc "passed on to nested fetches; nil: the fetch limits" :spec (a/num-in 0 nil) :default nil}
   :chain {:doc "items being fetched above this one (a cycle stops)" :spec (a/coll-of a/item?) :default []}
   :avoid {:doc "cells [x y z] a crafting table is never put in or dug for (a caller's dig line); passed to the craft steps" :spec (a/coll-of a/position?) :default nil}
   :explore {:doc "walk and look for a raw need not seen: false, true (the ::explore setting) or jobs.explore.search options over it" :spec (a/or-of boolean? map?) :default true}})

(defn check [c]
  (let [a (:args c)
        names (names-of a)]
    (cond
      (args-error a) true
      (declined/key* (ctx/mem c)) (declined/check c)
      (:start (ctx/mem c)) true
      (and (contains? (:how (limits c)) :chest) (seq (candidates c names))) true
      (and (contains? (:how (limits c)) :craft) (craft-plan c names (:count a))) true
      (and (contains? (:how (limits c)) :gather) (gather-viable? c names (:count a))) true
      (and (contains? (:how (limits c)) :gather) (explorable? c names (:count a)))
      (if-let [hurt (body/too-hurt (:primitives c))] (ctx/wait c hurt) true)
      (and (contains? (:how (limits c)) :gather) (gather-held? c names (:count a))) false
      :else (let [reason (merge {:reason :no-source :why (no-source-why c names (:count a))} (wanted-fields a))]
              (if (contains? (:how (limits c)) :chest)
                (look/wait-unless-surveyed c reason)
                (ctx/wait c reason))))))

(defn survey-first?
  "Before the first round, with chests allowed, none usable seen that might hold one of names and no look around yet:
  the round looks around first, so a chest behind the body is preferred to a craft or gather."
  [c names]
  (and (not (:start (ctx/mem c)))
       (not (look/surveyed? c))
       (contains? (:how (limits c)) :chest)
       (empty? (candidates c names))))

(defn stop! [c reason extra]
  (let [a (:args c)
        res (merge {:status :stopped :reason reason :got (:got extra 0)} (wanted-fields a) extra)]
    (ctx/result! c res)
    :done))

(defn progress
  "The stall clock: prev ({:inv carried-counts :t ms :at ms of the last look}) kept while what is carried is the same, else
  restarted at now. A gap since the last look beyond max-gap (a preemption, a long wait) does not count."
  [prev inv now max-gap]
  (if (= inv (:inv prev))
    {:inv inv :at now :t (+ (:t prev) (max 0 (- now (:at prev) max-gap)))}
    {:inv inv :t now :at now}))

(defn ^:async step! [c]
  (let [a (:args c)
        names (names-of a)
        inv (u/inventory (:primitives c))
        have (carried-of inv names)
        now (ctx/now c)]
    (if-let [e (args-error a)]
      (do (ctx/emit! c :obtain.declined :warn {:reason :bad-args :text (str "items.obtain " e)})
          (stop! c :bad-args {:why e}))
      (if (survey-first? c names)
        (do (await (look/survey! c)) (if (check c) :again :continue))
        (let [_ (when-not (:start (ctx/mem c))
                  (ctx/update-mem! c update :craft dissoc :table-unreachable)
                  (ctx/update-mem! c assoc :start {:have have :target (+ have (min 64 (:count a)))}))
              {:keys [target] :as start} (:start (ctx/mem c))
              seen-inv (storage/carried-totals (u/inventory (:primitives c)))
              prog (progress (:progress (ctx/mem c)) seen-inv now (max-gap-ms))
              _ (when (not= prog (:progress (ctx/mem c))) (ctx/update-mem! c assoc :progress prog))
              t (:t prog)
              got (max 0 (- have (:have start)))
              o (limits c)
              m (ctx/mem c)]
          (cond
            (some (set (:chain a)) names) (stop! c :cycle {:chain (:chain a)})
            (get-in m [:gather :tree]) (await (finish-tree! c))
            (>= have target) (do (ctx/result! c {:status :done :got got :item (or (holds (into {} (map (juxt :name :count)) inv) names) (first names))})
                                 :done)
            (> (- now t) (* 60000 (:minutes o))) (stop! c :timeout {:got got :tried (:tried m)})
            (and (contains? (:how o) :chest) (not (get-in m [:tried :chest])))
            (let [cs (candidates c names)
                  known (first (filter :stock cs))
                  unknown (first (remove :stock cs))]
              (cond
                known (await (withdraw! c (:pos known) (:stock known) names have))
                (and unknown (< (:inspected m 0) (max-inspections))) (await (inspect! c (:pos unknown)))
                :else (tried! c :chest (if (or (seq (:done-chests m)) (pos? (:inspected m 0))) :lacking :none-seen))))
            (and (contains? (:how o) :craft) (not (get-in m [:tried :craft]))
                 (or (not (contains? (:how o) :gather)) (get-in m [:craft :step]) (get-in m [:craft :table-unreachable]) (craft-plan c names (- target have))))
            (await (craft-step! c names have target))
            (and (contains? (:how o) :gather) (not (get-in m [:tried :gather])))
            (await (gather-step! c names have target))
            :else (let [unreachable (= :table-unreachable (get-in m [:tried :craft]))]
                    (stop! c (if unreachable :table-unreachable :no-source)
                           (cond-> {:got got :tried (:tried m)}
                             (not unreachable) (assoc :why (str "no source for " (clojure.string/join "/" names) ": "
                                                                (no-source-why c names (:count a))
                                                                (tried-text (:tried m) (get-in m [:gather :why]) (get-in m [:craft :detail])))))))))))))

(defn ^:async round
  "The whole attempt in one call: step! again until the count is carried or it stops; :continue only while a child waits on the world, :declined while a smelt child cooks."
  [c]
  (declined/begin! c)
  (await (pace/steps! c (fn ^:async obtain-step []
                                      (let [r (await (step! c))]
                                        (when (:progress (ctx/mem c)) (ctx/update-mem! c assoc-in [:progress :at] (ctx/now c)))
                                        r)))))
