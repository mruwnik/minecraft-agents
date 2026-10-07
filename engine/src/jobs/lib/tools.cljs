(ns jobs.lib.tools
  "Which carried tool suits a block."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.cost.weapon :as weapon]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::low-fraction {:default 0.1 :doc "A tool at or under this fraction of its durability is low." :spec (a/num-in 0 nil)}
   ::wear-settle-ms {:default 250 :doc "Wait for the inventory to show a tool breaking, in ms." :spec (a/int-in 0 nil)}
   ::wear-settle-tries {:default 3 :doc "Times to wait for the break to show." :spec (a/int-in 0 nil)}
   ::quick-clear-ms {:default 1500 :doc "A block that carried tools or the bare hand break within this is cleared without a tool." :spec (a/int-in 0 nil)}})

(def shovel-blocks
  #{"dirt" "grass_block" "sand" "red_sand" "gravel" "clay" "soul_sand" "soul_soil" "mud" "snow_block"
    "coarse_dirt" "rooted_dirt" "podzol" "mycelium" "farmland" "dirt_path"})

(defn tool-kind
  "\"shovel\", \"axe\" or \"pickaxe\": the tool that digs block-name fastest."
  [block-name]
  (cond
    (shovel-blocks block-name) "shovel"
    (some #(str/ends-with? block-name %) ["_log" "_wood" "_planks"]) "axe"
    :else "pickaxe"))

(defn best-tool
  "The carried tool (of item-names) of the kind that suits block-name with the
  highest material, or nil."
  [item-names block-name]
  (let [suffix (str "_" (tool-kind block-name))
        rank weapon/weapon-rank]
    (->> item-names
         (filter #(str/ends-with? % suffix))
         (sort-by rank >)
         first)))

(def cheapness
  "Pickaxe materials, cheapest first, as a player would pick the tool to make: gold last (it harvests as wood does)."
  weapon/cheapness)

(defn cheapest-tool
  "The cheapest of item-names by material (cheapness order; unknown materials last), or nil for none."
  [item-names]
  (first (sort-by weapon/cheap-rank item-names)))

(defn harvest-need
  "nil when the carried items (names) can harvest a block with these harvest-tools (item names; empty when the hand
  does), else the cheapest tool that does."
  [item-names harvest-tools]
  (when (and (seq harvest-tools) (not-any? (set harvest-tools) item-names))
    (cheapest-tool harvest-tools)))

(defn suited-item
  "The carried item (map {:name :durability ...}) a careful player digs block-name with, or nil. It is of the right
  kind and, when harvest-tools is not empty, one of them. Cheapest material first, then the most worn."
  [items block-name harvest-tools]
  (let [suffix (str "_" (tool-kind block-name))
        ok (set harvest-tools)]
    (->> items
         (filter #(and (str/ends-with? (:name %) suffix) (or (empty? ok) (ok (:name %)))))
         (sort-by (juxt (comp weapon/cheap-rank :name) #(or (:durability %) js/Infinity)))
         first)))

(defn suited-tool
  "Name of suited-item, or nil."
  [items block-name harvest-tools]
  (:name (suited-item items block-name harvest-tools)))

(defn low-fraction [] (settings/get settings ::low-fraction))

(defn wear-event
  "What changed between prev (the tool last held) and now (the carried tools of that name; nil for none). Both are
  maps {:name :durability :max :n}, :n the count. :tool-broke when fewer are carried, :tool-low when the tool just
  is at the low fraction of its durability and prev has not told of it (:low-seen), else nil."
  [prev now]
  (when prev
    (let [low? (fn [t] (and (:durability t) (:max t) (<= (:durability t) (* (low-fraction) (:max t)))))]
      (cond
        (< (:n now 0) (:n prev 1)) (when (<= (or (:durability prev) 0) 20) :tool-broke)
        (and (low? now) (not (:low-seen prev))) :tool-low))))

(defn pick
  "The carried item to dig block-name with (suited-item against the block's harvest tools), or nil."
  [p block-name]
  (suited-item (u/inventory p) block-name (some-> (.harvestTools p block-name) js->clj)))

(defn wear-snapshot
  "{:name :durability :max :n} of the carried tool item: its most worn copy and how many of that name are carried."
  [p item]
  (let [same (filter #(= (:name item) (:name %)) (u/inventory p))
        worn (first (sort-by #(or (:durability %) js/Infinity) same))]
    (assoc (select-keys worn [:name :durability :max]) :n (count same))))

(defn wear-settle-ms [] (settings/get settings ::wear-settle-ms))
(defn wear-settle-tries [] (settings/get settings ::wear-settle-tries))

(defn ^:async note-wear!
  "Emit :tool.low or :tool.broke (agent-facing, :warn) when the tool picked last time wore out since; ends with a
  :tool.none warning when no tool of its kind is left. :tool-wear is then renewed (cleared after a break) so an event
  is told once. A tool with 2 or less durability left
  that still shows is re-read after short waits (the break reaches the inventory just after the dig). Called before each
  equip and after each dig (jobs.lib.tidy/dig!, the stair's dig!), since a tool can break inside the dig with no equip after it."
  [c]
  (let [p (:primitives c)
        prev (:tool-wear (ctx/mem c))
        about-to-break? (and prev (<= (or (:durability prev) js/Infinity) 2))
        [now ev] (loop [tries 0]
                   (let [now (when prev (wear-snapshot p prev))
                         ev (wear-event prev now)]
                     (if (and about-to-break? (not= ev :tool-broke) (< tries (wear-settle-tries)))
                       (do (await (ctx/act c :wait #js {:ms (wear-settle-ms)}))
                           (recur (inc tries)))
                       [now ev])))]
    (when ev
      (ctx/emit! c (if (= ev :tool-broke) :tool.broke :tool.low) :warn
                 {:tool (:name prev) :durability (:durability now) :left (:n now)})
      (when (and (= ev :tool-broke) (zero? (:n now))
                 (not-any? #(str/ends-with? (:name %) (str "_" (last (str/split (:name prev) #"_")))) (u/inventory p)))
        (ctx/emit! c :tool.none :warn {:tool (:name prev)})))
    (when prev
      (ctx/update-mem! c (fn [m]
                           (if (or (= ev :tool-broke) (zero? (:n now 0)))
                             (dissoc m :tool-wear)
                             (assoc m :tool-wear (cond-> now (or ev (:low-seen prev)) (assoc :low-seen true)))))))))

(defn ^:async equip-tool!
  "Hold the tool for digging block-name: the cheapest that harvests (pick), or with {:fast true} the best carried
  one (best-tool). Emits :tool.low / :tool.broke / :tool.none for the tool held last time (note-wear!). Does nothing
  when none is carried or it is already held. Resolves to the equip result, or nil."
  [c block-name opts]
  (let [p (:primitives c)
         _ (await (note-wear! c))
         item (when block-name
                (if (:fast opts)
                  (some->> (best-tool (map :name (u/inventory p)) block-name) (assoc {} :name))
                  (pick p block-name)))
         tool (:name item)]
     (when tool
       (ctx/update-mem! c assoc :tool-wear
                        (cond-> (wear-snapshot p item)
                          (and (= tool (:name (:tool-wear (ctx/mem c)))) (:low-seen (:tool-wear (ctx/mem c)))) (assoc :low-seen true))))
     (when (and tool (not= tool (.-held (.self p))))
       (await (ctx/act c :equip #js {:item tool :dest "hand"})))))

(defn equip-for!
  "equip-tool!, a promise."
  ([c block-name] (equip-tool! c block-name nil))
  ([c block-name opts] (equip-tool! c block-name opts)))

(defn need-kind
  "The tool kind (\"pickaxe\", \"sword\", \"shears\") of a tool item name; \"pickaxe\" for nil."
  [tool-name]
  (if tool-name (last (str/split tool-name #"_")) "pickaxe"))

(defn needed-kind
  "The kind of tool block-name needs and none carried is (the cheapest harvestTools entry), else \"pickaxe\"."
  [p block-name]
  (need-kind (harvest-need (map :name (u/inventory p)) (some-> (.harvestTools p block-name) js->clj))))

(defn can-harvest?
  "Whether the carried tools harvest block-name: its harvestTools are empty, or one is carried."
  [p block-name]
  (nil? (harvest-need (map :name (u/inventory p)) (some-> (.harvestTools p block-name) js->clj))))

(defn quick-clear-ms [] (settings/get settings ::quick-clear-ms))

(defn clear-ms
  "The least time in ms the bare hand or any carried item takes to break block-name (primitive clearTime; a block name the registry does not know counts as 0, free)."
  [p block-name]
  (->> (map :name (u/inventory p))
       distinct
       (cons nil)
       (map #(js/Number (.clearTime p block-name %)))
       (apply min)))

(defn needs-tool-to-clear?
  "Whether a job that only needs block-name gone (stair, tunnel, build) must have a tool first: no carried tool
  harvests it (can-harvest?) and nothing carried breaks it within quick-clear-ms. Leaves, snow, glass, dirt go by
  hand; stone, ore and a cobweb do not. A job that wants the drop (mine) asks can-harvest? alone."
  [p block-name]
  (and (not (can-harvest? p block-name))
       (> (clear-ms p block-name) (quick-clear-ms))))

(defn hand-better?
  "Whether the body should dig block bare-handed: it is not a pickaxe block, no tool of its kind is carried, and a
  pickaxe is held (soil by hand, never wearing the pickaxe out on it)."
  [p block]
  (let [held (.-held (.self p))]
    (boolean (and block held (str/ends-with? held "_pickaxe") (not= "pickaxe" (tool-kind block))
                  (nil? (pick p block))))))

(defn ^:async equip!
  "Hold the best carried tool for block (the mined block when not given)."
  ([c] (equip! c (:block (:args c))))
  ([c block]
   (await (equip-for! c block))
   (when (hand-better? (:primitives c) block)
     (await (ctx/act c :unequip #js {})))))
