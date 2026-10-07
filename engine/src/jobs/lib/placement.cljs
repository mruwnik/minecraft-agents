(ns jobs.lib.placement
  "From a wanted block state to the click that makes it. Minecraft decides a placed block's state from the face of
  the neighbour clicked, where on that face (the cursor), the player's look and sneaking, never from where the
  player stands (vanilla getStateForPlacement; the server checks reach, not that the look meets the face). So a
  facing is held as a yaw, a half is a face or a cursor height, an axis is a face on that axis:
  - stairs: facing = look, half by face (underside: top, top: bottom, a side: cursor above the middle is top)
  - slabs: :type by the same rule; :double is refused (a second slab merges)
  - logs and pillars: :axis = the axis of the face
  - fence gates, doors, beds: facing = look; a door's upper half and a bed's head are made by the other part
  - chests, furnaces, smokers, carved pumpkins ...: facing = opposite of the look; a chest is placed sneaking so it
    never joins a neighbour unless :type :left or :right asks for that
  - trapdoors: on a side face facing = that face and half by cursor; else facing = opposite of the look, half by face
  - torches: a floor torch looks straight down at the top of the block below (looking level can make a wall torch);
    wall torches and ladders hang on the block behind them, looking into it
  Anything else is placed plainly. A usable neighbour (chest, table, door ...) is clicked only when nothing else
  gives the state, and then sneaking. Directions are mineflayer's: yaw 0 looks north, pi/2 west; pitch -pi/2 down."
  (:require [engine.game :as game]
            [clojure.string :as str]
            [jobs.lib.util :as u]
            [shadow.resource :as rc]))

(def pi js/Math.PI)

(def steps {"north" [0 0 -1] "south" [0 0 1] "east" [1 0 0] "west" [-1 0 0] "up" [0 1 0] "down" [0 -1 0]})
(def opposite {"north" "south" "south" "north" "east" "west" "west" "east"})
(def yaws {"north" 0 "west" (/ pi 2) "south" pi "east" (* 1.5 pi)})

(def up [0 1 0])
(def down [0 -1 0])
(def sides [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1]])
(def axes {"x" [[-1 0 0] [1 0 0]] "y" [up down] "z" [[0 0 -1] [0 0 1]]})

(def loose
  "Cells a click cannot be made on: nothing there, a fluid, or what the game replaces."
  #{"air" "cave_air" "void_air" "water" "lava" "bubble_column" "fire" "soul_fire" "short_grass" "tall_grass" "grass"
    "fern" "large_fern" "snow" "vine" "dead_bush" "seagrass" "tall_seagrass" "light" "leaf_litter" "glow_lichen" "hanging_roots"})

(def usable
  "Blocks a right-click works (opens, toggles, uses). The table is interactable_blocks.txt, also read by engine/js/blocks.mjs."
  (re-pattern (str/trim (rc/inline "jobs/lib/interactable_blocks.txt"))))

(def thin
  "Blocks without a full face to hang a torch or ladder on, or to stand a door on."
  #"^(farmland|dirt_path|soul_sand|ladder|torch|wall_torch|lantern|soul_lantern|chain|scaffolding|cobweb|cake|lectern|hopper|anvil|chipped_anvil|damaged_anvil|bell|enchanting_table|brewing_stand|composter|grindstone|stonecutter|chest|trapped_chest|ender_chest)$|_(slab|stairs|fence|fence_gate|wall|pane|door|trapdoor|carpet|sign|button|pressure_plate|bed|torch|rail|leaves|sapling|flower)$|^glass_pane$")

(def floor-torch-base
  "Thin blocks a floor torch still stands on: their top has a middle."
  #"_(fence|wall)$")

(defn item-of
  "The item that places block (a wall torch or wall sign is placed with the standing one)."
  [block]
  (str/replace block #"(^|_)wall_(torch|sign|hanging_sign|banner)$" "$1$2"))

(defn family [block]
  (condp re-find block
    #"_stairs$" :stairs
    #"_slab$" :slab
    #"_trapdoor$" :trapdoor
    #"_door$" :door
    #"_fence_gate$" :gate
    #"_bed$" :bed
    #"^(torch|soul_torch|redstone_torch|copper_torch)$" :torch
    #"_wall_torch$|^wall_torch$" :wall-torch
    #"^ladder$" :ladder
    #"^(chest|trapped_chest|ender_chest|furnace|smoker|blast_furnace|carved_pumpkin|jack_o_lantern|lectern|beehive|bee_nest)$" :front
    #"(_log|_wood|_stem|_hyphae)$|^(basalt|polished_basalt|quartz_pillar|purpur_pillar|hay_block|bone_block|deepslate|muddy_mangrove_roots|bamboo_block|stripped_bamboo_block|ochre_froglight|verdant_froglight|pearlescent_froglight)$" :pillar
    :plain))

(defn state-value
  "The wanted value of state key k as a string, or nil (keys and values may be keywords or strings)."
  [want k]
  (let [v (if (contains? want k) (get want k) (get want (name k)))]
    (cond (keyword? v) (name v) (some? v) (str v))))

(defn add [a b] (mapv + a b))
(defn sub [a b] (mapv - a b))

(defn clickable? [block] (and block (not (loose (:name block)))))
(defn sturdy? [block] (and (clickable? block) (not (re-find thin (:name block)))))
(defn free? [block] (and block (contains? loose (:name block))))
(defn usable? [block] (boolean (re-find usable (:name block))))

(defn cursor
  "Where on the face of the against block that points along d (from it into the cell) to click; on a side face at
  height y."
  [d y]
  (let [[dx dy dz] (mapv #(+ 0.5 (* 0.5 %)) d)]
    [dx (if (zero? (second d)) y dy) dz]))

(defn option
  "A candidate click on the neighbour of cell against which the face along d points into the cell."
  [cell d y]
  {:against (sub cell d) :cursor (cursor d y) :d d})

(defn eye-dist [eye {:keys [against cursor]}]
  (if-not eye
    0
    (let [[x y z] (add against cursor)]
      (js/Math.hypot (- x (:x eye)) (- y (:y eye)) (- z (:z eye))))))

(defn choose
  "The first of options (vertical faces in their order, then sides nearest the eye) whose against block passes ok?,
  a plain block before a usable one; the usable one is clicked sneaking."
  [options eye block-at ok?]
  (let [vertical (remove #(zero? (second (:d %))) options)
        side (filter #(zero? (second (:d %))) options)
        ordered (concat vertical (sort-by #(eye-dist eye %) side))
        held (filter #(ok? (block-at (:against %))) ordered)
        pick (or (first (remove #(usable? (block-at (:against %))) held)) (first held))]
    (when pick
      (cond-> (select-keys pick [:against :cursor])
        (usable? (block-at (:against pick))) (assoc :sneak true)))))

(defn half-options
  "Clicks that give half (\"top\" or \"bottom\"): the underside of the block above or the top of the one below,
  then the sides at that height; never the face that gives the other half."
  [cell half]
  (if (= "top" half)
    (cons (option cell down 0.5) (map #(option cell % 0.75) sides))
    (cons (option cell up 0.5) (map #(option cell % 0.25) sides))))

(defn any-options [cell] (concat [(option cell up 0.5) (option cell down 0.5)] (map #(option cell % 0.5) sides)))

(defn look-toward [dir] {:yaw (yaws dir) :pitch 0})

(defn result
  "{:item :click} with the click and look, or {:item :refused :no-support} when there is no click."
  [item click look]
  (if click
    {:item item :click (merge click look)}
    {:item item :refused :no-support}))

(defn opened? [want] (= "true" (state-value want :open)))

(defmulti decide (fn [fam _want _cell _ctx] fam))

(defmethod decide :plain [_ want _ _] {:item (:block want)})

(defmethod decide :stairs [_ want cell {:keys [eye block-at]}]
  (let [facing (yaws (state-value want :facing))
        half (state-value want :half)]
    (if-not (or facing half)
      {:item (:block want)}
      (result (:block want) (choose (half-options cell (or half "bottom")) eye block-at clickable?)
              (when facing {:yaw facing :pitch 0})))))

(defmethod decide :slab [_ want cell {:keys [eye block-at]}]
  (let [half (state-value want :type)]
    (cond
      (= "double" half) {:item (:block want) :refused :double-slab}
      (nil? half) {:item (:block want)}
      :else (result (:block want) (choose (half-options cell half) eye block-at clickable?) nil))))

(defmethod decide :pillar [_ want cell {:keys [eye block-at]}]
  (let [axis (state-value want :axis)]
    (if-not (axes axis)
      {:item (:block want)}
      (result (:block want) (choose (map #(option cell % 0.5) (axes axis)) eye block-at clickable?) nil))))

(defn facing-click
  "A block whose facing is the look (gate, door, bed): any clickable neighbour, the look held that way."
  [want cell {:keys [eye block-at]} options]
  (let [facing (state-value want :facing)]
    (if-not (yaws facing)
      {:item (:block want)}
      (result (:block want) (choose options eye block-at clickable?) (look-toward facing)))))

(defmethod decide :gate [_ want cell ctx]
  (if (opened? want)
    {:item (:block want) :refused :opened}
    (facing-click want cell ctx (any-options cell))))

(defmethod decide :door [_ want cell {:keys [block-at] :as ctx}]
  (cond
    (= "upper" (state-value want :half)) {:item (:block want) :refused :other-half}
    (opened? want) {:item (:block want) :refused :opened}
    (not (sturdy? (block-at (add cell down)))) {:item (:block want) :refused :no-support}
    (not (free? (block-at (add cell up)))) {:item (:block want) :refused :no-room}
    :else (facing-click want cell ctx (any-options cell))))

(defmethod decide :bed [_ want cell {:keys [block-at] :as ctx}]
  (let [facing (state-value want :facing)]
    (cond
      (= "head" (state-value want :part)) {:item (:block want) :refused :other-half}
      (and (steps facing) (not (free? (block-at (add cell (steps facing)))))) {:item (:block want) :refused :no-room}
      :else (facing-click want cell ctx (any-options cell)))))

(defmethod decide :front [_ want cell {:keys [eye block-at]}]
  (let [facing (state-value want :facing)
        chest? (re-find #"chest$" (:block want))
        join? (#{"left" "right"} (state-value want :type))]
    (if-not (or (yaws facing) chest?)
      {:item (:block want)}
      (result (:block want)
              (cond-> (choose (any-options cell) eye block-at clickable?)
                (and chest? (not join?)) (assoc :sneak true))
              (when (yaws facing) (look-toward (opposite facing)))))))

(defmethod decide :trapdoor [_ want cell {:keys [eye block-at]}]
  (let [facing (state-value want :facing)
        half (or (state-value want :half) "bottom")
        on-side (when (steps facing) (choose [(option cell (steps facing) (if (= "top" half) 0.75 0.25))] eye block-at clickable?))]
    (cond
      (opened? want) {:item (:block want) :refused :opened}
      (not (opposite facing)) {:item (:block want)}
      on-side {:item (:block want) :click on-side}
      :else (result (:block want) (choose [(option cell (if (= "top" half) down up) 0.5)] eye block-at clickable?)
                    (look-toward (opposite facing))))))

(defmethod decide :torch [_ want cell {:keys [eye block-at]}]
  (let [base? #(or (sturdy? %) (and (clickable? %) (re-find floor-torch-base (:name %))))]
    (result (:block want) (choose [(option cell up 0.5)] eye block-at base?) {:pitch (- (/ pi 2))})))

(defn hung
  "A block that hangs on the block behind it (wall torch, ladder), looking into that block."
  [want cell {:keys [eye block-at]}]
  (let [facing (state-value want :facing)
        item (item-of (:block want))]
    (if-not (opposite facing)
      {:item item}
      (result item (choose [(option cell (steps facing) 0.5)] eye block-at sturdy?) (look-toward (opposite facing))))))

(defmethod decide :wall-torch [_ want cell ctx] (hung want cell ctx))
(defmethod decide :ladder [_ want cell ctx] (hung want cell ctx))

(defn click
  "How to place the block want (a name or {:block name & state}) at cell [x y z], for a body whose eye is at
  {:x :y :z} (nil: any) in a world block-at ([x y z] -> nil when unloaded | {:name n}):
  {:item i :click {:against [x y z] :cursor [cx cy cz] :yaw :pitch :sneak}} (cursor on the against block, 0..1;
  :yaw, :pitch, :sneak only when they matter), {:item i} to place plainly, or {:item i :refused reason}:
  :other-half, :opened, :double-slab, :no-support (no neighbour gives the state), :no-room."
  [want cell eye block-at]
  (let [want (if (string? want) {:block want} want)]
    (decide (family (:block want)) want cell {:eye eye :block-at block-at})))

(defn eye [body] {:x (:x body) :y (+ (:y body) game/eye-height) :z (:z body)})

(defn js-click [{:keys [against cursor] :as click}]
  (merge (select-keys click [:yaw :pitch :sneak])
         {:against (zipmap [:x :y :z] against) :cursor (zipmap [:x :y :z] cursor)}))
