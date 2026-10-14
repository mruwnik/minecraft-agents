(ns jobs.items.get-tool
  (:require [engine.args :as a]
            ["minecraft-data" :as minecraft-data]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(def doc
  "Get a tool: one that harvests :block (minecraft-data harvestTools), the tool :item, or a tool of :kind (\"pickaxe\",
  \"axe\", \"shovel\", \"hoe\", \"sword\": any tier; any other kind such as \"shears\" is that item). Any one carried
  is enough: done at once. Else a jobs.items.obtain child (slot :obtain) gets one of the tools, cheapest material
  first (wooden, stone, copper, iron, diamond, netherite, golden), with :explore (default on: logs or stone not seen
  are walked to and looked for, bounded by the search options).
  Ways: :chest, :craft (the obtain's craft and smelt ways) and :gather; a fetch for a mine leaves :gather out, so a
  mine never starts another gather.

  Bad args (also an unknown :block) end condition-failed :bad-args {:why} (warn get-tool.declined).
  Conditions (before it starts): the obtain's failed condition (no source:
  {:reason :no-source :any-of [..]}, nothing seen to get one from, with :explore false).
  Ends :done {:tool name} only with the tool carried ({:needed false} for a block the hand harvests); else the
  obtain's ending kind and cause (its ending as :because), data {:any-of :tried :got}; an obtain done with no tool
  carried after is nothing-possible :lost.
  Quote: the obtain's.")

(a/defargs args
  {:block {:doc "a block name: get a tool that harvests it" :spec a/name? :default nil}
   :item {:doc "a tool item name" :spec a/item? :default nil}
   :kind {:doc "a tool kind, e.g. \"pickaxe\" or \"shears\"" :spec a/name? :default nil}
   :fail-minutes {:doc "passed on to the obtain; nil: the fetch limits" :spec (a/num-in 0 nil) :default nil}
   :explore {:doc "passed to the obtain: walk and look for raw material not seen (false: fail instead; a map: jobs.explore.search options)" :spec (a/or-of boolean? map?) :default true}})

(def tiered #{"pickaxe" "axe" "shovel" "hoe" "sword"})

(defn known-block?
  "Whether minecraft-data lists the block for the body's version."
  [p name]
  (.hasOwnProperty (.-blocksByName (minecraft-data (game/version-of p))) name))

(defn tools-for
  "The tool names that will do, cheapest first, or {:error text}. Empty for a block the hand harvests."
  [p {:keys [block item kind]}]
  (cond
    (and (string? block) (not (known-block? p block))) {:error (str "unknown block " block)}
    (string? block) (vec (sort-by weapon/cheap-rank (or (some-> (.harvestTools p block) js->clj) [])))
    (string? item) [item]
    (and (string? kind) (tiered kind)) (mapv #(str % "_" kind) tools/cheapness)
    (string? kind) [kind]
    :else {:error "needs :block, :item or :kind"}))

(defn carried-tool [p names]
  (let [held (set (map :name (u/inventory p)))]
    (first (filter held names))))

(def target [:block :item :kind])

(def ways [:chest :craft :gather])

(def defaults {:ways (set ways)})

(defn ways-of [c] (set (or (:ways (ctx/frozen-call c)) (:ways (:args c)) (:ways defaults))))

(defn obtain-args [c names]
  (let [ways (ways-of c)
        a (:args c)]
    (into {} (remove (comp nil? val))
          {:any-of names :count 1 :fail-minutes (:fail-minutes a) :explore (:explore a)
           :ways (cond-> (disj ways :craft) (ways :craft) (into [:craft :smelt]))})))

(defn bad-args [c]
  (let [names (tools-for (:primitives c) (:args c))]
    (when (map? names) {:reason :bad-args :why (:error names)})))

(defn no-source
  "Before it starts, with no tool that will do carried: the obtain's failed condition."
  [c]
  (let [p (:primitives c)
        names (tools-for p (:args c))]
    (when-not (or (ctx/frozen-call c) (map? names) (empty? names) (carried-tool p names))
      (let [w (atom nil)]
        (when-not (ctx/check-child (assoc c :wait w) :obtain 'jobs.items.obtain (obtain-args c names))
          (or (:cause @w) (:reason @w) :no-source))))))

(def conditions [[:fn no-source]])

(defn done?
  "Never done before its round: the round's ending names the tool, which a caller's repeat check reads."
  [_c]
  {:done? false :units :tools :done 0 :left 1})

(defn ^:async quote-of
  "Nothing for a tool carried or none needed, else the obtain's quote (too expensive when the obtain would be refused)."
  [c]
  (let [names (tools-for (:primitives c) (:args c))]
    (if (or (map? names) (empty? names) (carried-tool (:primitives c) names))
      {:uses {:time [0 0 0]}}
      (let [q (await (ctx/quote c 'jobs.items.obtain (obtain-args c names)))]
        (if (contract/ending? q) contract/too-expensive q)))))

(defn ^:async round [c]
  (let [p (:primitives c)
        names (tools-for p (:args c))]
    (cond
      (map? names) (do (ctx/emit! c :get-tool.declined :warn {:reason :bad-args :text (str "items.get-tool " (:error names))})
                       (ctx/end c :condition-failed (bad-args c) {:text (str "items.get-tool " (:error names))}))
      (empty? names) (ctx/end c :done nil {:needed false})
      (carried-tool p names) (ctx/end c :done nil {:tool (carried-tool p names)})
      :else
      (let [e (await (ctx/call c :obtain 'jobs.items.obtain (obtain-args c names)))
            tool (carried-tool p names)
            data (merge {:any-of names} (select-keys (:data e) [:tried :got :why]))]
        (cond
          (= :continue e) :continue
          tool (ctx/end c :done nil {:tool tool})
          (= :done (:kind e)) (ctx/end c :nothing-possible :lost data)
          :else (ctx/end c (:kind e) (:cause e) data))))))
