(ns jobs.items.get-tool
  (:require ["minecraft-data" :as minecraft-data]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [jobs.lib.blocks :as b]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(def doc
  "Get a tool: one that harvests :block (minecraft-data harvestTools), the tool :item, or a tool of :kind (\"pickaxe\",
  \"axe\", \"shovel\", \"hoe\", \"sword\": any tier; any other kind such as \"shears\" is that item). Any one carried
  is enough: done at once. Else a jobs.items.obtain child (slot :obtain) gets one of the tools, cheapest material
  first (wooden, stone, copper, iron, diamond, netherite, golden), with :depth one less.

  The check passes when a tool is carried, the obtain has begun, or the obtain's check passes; else it waits with the
  obtain's reason ({:reason :no-source :any-of [..]}: nothing seen to get one from).
  Ends {:status :done :tool name} ({:status :done :needed false} for a block the hand harvests) or {:status :stopped
  :reason r ...} with the obtain's reason and :tried, or :bad-args (also an unknown :block).")

(def args
  {:block {:doc "a block name: get a tool that harvests it" :default nil}
   :item {:doc "a tool item name" :default nil}
   :kind {:doc "a tool kind, e.g. \"pickaxe\" or \"shears\"" :default nil}
   :how {:doc "sources, a subset of #{:chest :craft :gather}; nil: all" :default nil}
   :depth {:doc "nested fetches left; nil: the fetch limits (jobs.lib.fetch)" :type :int :min 0 :default nil}
   :minutes {:doc "time budget; nil: the fetch limits" :type :number :min 0 :default nil}
   :fail-minutes {:doc "passed on to nested fetches; nil: the fetch limits" :type :number :min 0 :default nil}
   :chain {:doc "items being fetched above this one" :default []}})

(def tiered #{"pickaxe" "axe" "shovel" "hoe" "sword"})

(defn rank [n]
  (let [i (.indexOf tools/cheapness (first (str/split n #"_")))]
    (if (neg? i) (count tools/cheapness) i)))

(defn known-block?
  "Whether minecraft-data lists the block for the body's version."
  [p name]
  (.hasOwnProperty (.-blocksByName (minecraft-data (game/version-of p))) name))

(defn tools-for
  "The tool names that will do, cheapest first, or {:error text}. Empty for a block the hand harvests."
  [p {:keys [block item kind]}]
  (cond
    (and (string? block) (not (known-block? p block))) {:error (str "unknown block " block)}
    (string? block) (vec (sort-by rank (or (some-> (.harvestTools p block) js->clj) [])))
    (string? item) [item]
    (and (string? kind) (tiered kind)) (mapv #(str % "_" kind) tools/cheapness)
    (string? kind) [kind]
    :else {:error "needs :block, :item or :kind"}))

(defn carried-tool [p names]
  (let [held (set (map :name (u/inventory p)))]
    (first (filter held names))))

(defn obtain-args [c names]
  (let [a (:args c)
        o (fetch/merge-limits 'jobs.items.get-tool nil (fetch/body-limits (ctx/view c)) a)]
    {:any-of names :count 1 :depth (max 0 (dec (:depth o))) :how (:how o) :minutes (:minutes o)
     :fail-minutes (:fail-minutes o) :chain (vec (:chain a))}))

(defn check [c]
  (let [p (:primitives c)
        names (tools-for p (:args c))]
    (cond
      (map? names) true
      (empty? names) true
      (carried-tool p names) true
      (:begun (ctx/mem c)) true
      :else (boolean (ctx/check-child c :obtain 'jobs.items.obtain (obtain-args c names))))))

(defn ^:async round [c]
  (let [p (:primitives c)
        names (tools-for p (:args c))]
    (cond
      (map? names) (do (ctx/emit! c :get-tool.declined :warn {:reason :bad-args :text (str "items.get-tool " (:error names))})
                       (ctx/result! c {:status :stopped :reason :bad-args :why (:error names) :text (str "items.get-tool " (:error names))})
                       :done)
      (empty? names) (do (ctx/result! c {:status :done :needed false}) :done)
      (carried-tool p names) (do (ctx/result! c {:status :done :tool (carried-tool p names)}) :done)
      :else
      (let [_ (when-not (:begun (ctx/mem c)) (ctx/update-mem! c assoc :begun true))
            oargs (obtain-args c names)
            r (await (ctx/call-child c :obtain 'jobs.items.obtain oargs))]
        (case r
          :continue :continue
          :declined (let [w (b/child-wait c :obtain 'jobs.items.obtain oargs)]
                      (ctx/result! c (merge {:status :stopped :any-of names} (select-keys w [:reason])))
                      :done)
          (let [res (ctx/child-result c :obtain)
                tool (carried-tool p names)]
            (ctx/result! c (if tool
                             {:status :done :tool tool}
                             (merge {:status :stopped :reason (if (= :done (:status res)) :lost (:reason res)) :any-of names}
                                    (select-keys res [:tried :chain :got]))))
            :done))))))
