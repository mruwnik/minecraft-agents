(ns engine.fake.placing
  "The game's rule from a click to the state of the block it places (vanilla getStateForPlacement), for the fake
  world: the face of the clicked neighbour, the cursor height on it, the look (yaw for a horizontal facing, yaw and
  pitch for torches and ladders) and nothing else, over world data {:blocks {[x y z] name} :states {[x y z] props}}.
  jobs.lib.placement runs the rule backwards to choose a click; this is the forward rule written on its own, so a job
  test through the fake checks the two against each other. Test-only; ported from the deleted js/placing.mjs.
  Directions are mineflayer's: yaw 0 looks north (-z), pi/2 west; pitch -pi/2 looks down.
  A result is {:blocks [{:pos :name :properties}]} (a door or bed is two blocks) or {:refused reason}."
  (:require [clojure.string :as str]))

(def dirs {:north [0 0 -1] :south [0 0 1] :east [1 0 0] :west [-1 0 0] :up [0 1 0] :down [0 -1 0]})
(def opposite {:north "south" :south "north" :east "west" :west "east" :up "down" :down "up"})
(def by-yaw [:north :west :south :east])
(def free-re #"^(air|cave_air|void_air|water|lava|fire|soul_fire|short_grass|tall_grass|grass|snow)$")
;; no full face to hang a torch or ladder on, or to stand a door on
(def thin-re #"^(farmland|dirt_path|soul_sand|ladder|torch|wall_torch|lantern|chain|cobweb|chest|trapped_chest|ender_chest)$|_(slab|stairs|fence|fence_gate|wall|pane|door|trapdoor|carpet|sign|button|pressure_plate|bed|torch|rail|leaves|sapling)$")
(def front-re #"^(chest|trapped_chest|ender_chest|furnace|smoker|blast_furnace|carved_pumpkin|jack_o_lantern|lectern|beehive|bee_nest)$")
(def pillar-re #"(_log|_wood|_stem|_hyphae)$|^(basalt|polished_basalt|quartz_pillar|purpur_pillar|hay_block|bone_block|deepslate|muddy_mangrove_roots|bamboo_block|stripped_bamboo_block)$")
(def torch-re #"^(torch|soul_torch|redstone_torch|copper_torch)$")

(defn dir-of [d] (some (fn [[k v]] (when (= v d) k)) dirs))
(defn plus [pos d] (mapv + pos d))
(defn horizontal [yaw] (by-yaw (mod (js/Math.round (/ yaw (/ js/Math.PI 2))) 4)))

(defn looking-directions
  "The six directions ordered by how much the look points along each (the game's getNearestLookingDirections)."
  [yaw pitch]
  (let [v [(* (- (js/Math.sin yaw)) (js/Math.cos pitch)) (js/Math.sin pitch) (* (- (js/Math.cos yaw)) (js/Math.cos pitch))]]
    (->> (keys dirs)
         (map (fn [d] [d (reduce + (map * (dirs d) v))]))
         (sort-by second >)
         (map first))))

(defn free? [name] (boolean (re-find free-re name)))
(defn sturdy? [name] (and (not (free? name)) (not (re-find thin-re name))))

(defn placed-blocks
  "The blocks a click places. args: {:item :pos :face [dx dy dz] (from the clicked block into pos) :cursor [x y z] on
  the clicked block :yaw :pitch}; the world gives the neighbours."
  [w {:keys [item pos face cursor yaw pitch] :or {yaw 0 pitch 0}}]
  (let [block-at #(get-in w [:blocks %] "air")
        clicked (dir-of face)
        look (horizontal yaw)
        wet (= "water" (block-at pos))
        half (cond (= clicked :down) "top" (= clicked :up) "bottom" (> (cursor 1) 0.5) "top" :else "bottom")
        one (fn [name properties] {:blocks [{:pos pos :name name :properties properties}]})
        item? #(str/ends-with? item %)]
    (cond
      (item? "_stairs") (one item {:facing (name look) :half half :shape "straight" :waterlogged wet})
      (item? "_slab") (one item {:type half :waterlogged wet})
      (item? "_trapdoor")
      (let [side (not (#{:up :down} clicked))]
        (one item {:facing (if side (name clicked) (opposite look))
                   :half (cond side half (= clicked :up) "bottom" :else "top")
                   :open false :powered false :waterlogged wet}))
      (item? "_door")
      (let [above (plus pos (dirs :up))
            door (fn [part] {:facing (name look) :half part :hinge "left" :open false :powered false})]
        (cond
          (not (free? (block-at above))) {:refused "no room for the upper half"}
          (not (sturdy? (block-at (plus pos (dirs :down))))) {:refused "no floor"}
          :else {:blocks [{:pos pos :name item :properties (door "lower")} {:pos above :name item :properties (door "upper")}]}))
      (item? "_fence_gate") (one item {:facing (name look) :open false :powered false :in_wall false})
      (item? "_bed")
      (let [head (plus pos (dirs look))
            bed (fn [part] {:facing (name look) :part part :occupied false})]
        (if (free? (block-at head))
          {:blocks [{:pos pos :name item :properties (bed "foot")} {:pos head :name item :properties (bed "head")}]}
          {:refused "no room for the head"}))
      (re-find front-re item)
      (one item (merge {:facing (opposite look)} (when (item? "chest") {:type "single" :waterlogged wet})))
      (re-find pillar-re item) (one item {:axis ({:east "x" :west "x" :up "y" :down "y" :north "z" :south "z"} clicked)})
      (re-find torch-re item)
      (or (some (fn [d]
                  (cond
                    (= d :up) nil
                    (and (= d :down) (not (free? (block-at (plus pos (dirs :down)))))) (one item {})
                    (and (not= d :down) (sturdy? (block-at (plus pos (dirs d))))) (one (str/replace item #"torch$" "wall_torch") {:facing (opposite d)})))
                (looking-directions yaw pitch))
          {:refused "nothing holds the torch"})
      (= item "ladder")
      (if-let [d (->> (looking-directions yaw pitch)
                      (remove #{:up :down})
                      (filter #(sturdy? (block-at (plus pos (dirs %)))))
                      first)]
        (one item {:facing (opposite d) :waterlogged wet})
        {:refused "nothing holds the ladder"})
      :else (one item {}))))

(defn place
  "[world' result]: the blocks a click places written into the world (their properties into :states when they have
  any); a refusal places nothing."
  [w args]
  (let [result (placed-blocks w args)]
    [(reduce (fn [w {:keys [pos name properties]}]
               (cond-> (assoc-in w [:blocks pos] name)
                 (seq properties) (assoc-in [:states pos] properties)))
             w
             (:blocks result))
     result]))
