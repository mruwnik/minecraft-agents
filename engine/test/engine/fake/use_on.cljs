(ns engine.fake.use-on
  "The fake's useOn: right-click a block with an item (or an empty hand, item nil), applying the few rules the jobs
  rely on, over world data {:blocks :states :ages {[x y z] n} :inventory [{:name :count}] :entities :next-entity-id
  :self {:pos [x y z] :held}}. (use-on world {:pos :item :face}) -> [world' result], the result
  {:status :before :after :consumed} with :reason and :distance on a refusal, and only {:status \"missing\"} for air.
  Test-only; ported from the deleted js/fake-use-on.mjs."
  (:require [engine.fake.doors :as doors]))

(def reach 4.5)
(def max-age {"wheat" 7 "carrots" 7 "potatoes" 7 "beetroots" 3})
(def tillable #{"dirt" "grass_block" "dirt_path"})
(def pathable #{"dirt" "grass_block" "coarse_dirt" "podzol" "mycelium" "rooted_dirt"})
(def compostable
  #{"wheat_seeds" "beetroot_seeds" "melon_seeds" "pumpkin_seeds" "wheat" "carrot" "potato" "beetroot" "apple"
    "melon_slice" "short_grass" "tall_grass" "oak_leaves" "birch_leaves" "spruce_leaves" "kelp" "sweet_berries"
    "oak_sapling" "birch_sapling" "spruce_sapling" "bread" "baked_potato" "cookie" "pumpkin" "melon"})
(def hives #{"beehive" "bee_nest"})
(def bed-re #"_bed$|^respawn_anchor$")
;; The generic window guard is a last resort: a window mineflayer misparses can desync the inventory, so refuse
;; window-opening blocks before the click.
(def container-re #"chest$|barrel$|shulker_box$|furnace$|smoker$|hopper$|dispenser$|dropper$|brewing_stand$|crafting_table$|anvil$|enchanting_table$|grindstone$|loom$|stonecutter$|cartography_table$|smithing_table$|lectern$|beacon$|^crafter$|command_block$|^structure_block$|^jigsaw$|^vault$")
(def hazards #{"flint_and_steel" "fire_charge" "lava_bucket"})
(def block-items #{"dirt" "cobblestone" "stone" "oak_planks" "oak_leaves" "pumpkin" "melon" "hay_block"})

(defn distance [a b] (js/Math.hypot (- (a 0) (b 0)) (- (a 1) (b 1)) (- (a 2) (b 2))))

(defn snapshot [w pos]
  {:name (get-in w [:blocks pos] "air")
   :properties (merge {} (get-in w [:states pos]) (when (contains? (:ages w) pos) {:age (get-in w [:ages pos])}))})

(defn spawn-item
  "World with a dropped item entity at pos."
  [w pos name count]
  (-> w
      (update :entities conj {:id (:next-entity-id w) :name "item" :kind "item" :pos pos :item {:name name :count count}})
      (update :next-entity-id inc)))

(defn landing
  "Where an item popped at pos comes to rest: it falls through air, at most 8 cells."
  [w pos]
  (loop [p pos n 0]
    (if (and (< n 8) (= "air" (get-in w [:blocks (update p 1 dec)] "air")))
      (recur (update p 1 dec) (inc n))
      p)))

(defn take-one
  "World with one of the named item taken out of the inventory (a stack that empties goes)."
  [w item]
  (update w :inventory
          (fn [inv] (->> inv (map #(if (= item (:name %)) (update % :count dec) %)) (remove #(zero? (:count %))) vec))))

(defn give-one [w item]
  (update w :inventory
          (fn [inv] (if (some #(= item (:name %)) inv)
                      (mapv #(if (= item (:name %)) (update % :count inc) %) inv)
                      (conj inv {:name item :count 1})))))

(defn tie-to-post
  "A click on a fence post ties every animal the body leads to a leash_knot entity there; [world' how-many]."
  [w pos]
  (let [led (filter :leashed-to-me (:entities w))]
    (if (empty? led)
      [w 0]
      (let [knot (first (filter #(and (= "leash_knot" (:name %)) (= pos (:pos %))) (:entities w)))
            w (if knot
                w
                (-> w
                    (update :entities conj {:id (:next-entity-id w) :name "leash_knot" :kind "other" :pos pos})
                    (update :next-entity-id inc)))
            knot-id (:id (first (filter #(and (= "leash_knot" (:name %)) (= pos (:pos %))) (:entities w))))]
        [(update w :entities (fn [es] (mapv #(if (:leashed-to-me %) (assoc % :leashed-to-me false :leash-holder knot-id) %) es)))
         (count led)]))))

(def face-delta {"up" [0 1 0] "down" [0 -1 0] "north" [0 0 -1] "south" [0 0 1] "east" [1 0 0] "west" [-1 0 0]})

(defn use-on [w {:keys [pos item face] :or {face "up"}}]
  (let [here (get-in w [:blocks pos] "air")
        before (snapshot w pos)
        done (fn [w' status & [consumed extra]] [w' (merge {:status status} extra {:before before :after (snapshot w' pos) :consumed (or consumed 0)})])
        self-pos (get-in w [:self :pos])
        near? (<= (distance self-pos pos) reach)
        props (get-in w [:states pos])
        level (get props :level 0)
        age (get-in w [:ages pos])
        hand? (nil? item)]
    (cond
      (= here "air") [w {:status "missing"}]
      (re-find bed-re here) (done w "cannot" 0 {:reason "bed"})
      (re-find container-re here) (done w "cannot" 0 {:reason "container"})
      (hazards item) (done w "cannot" 0 {:reason "hazard"})
      (and (block-items item) (not= here "composter")) (done w "cannot" 0 {:reason "use-place"})
      (and item (not-any? #(= item (:name %)) (:inventory w))) (done w "no-item")
      (not near?) (done w "unreachable" 0 {:reason "too-far"
                                           :distance (/ (js/Math.round (* 100 (distance self-pos (mapv + pos [0.5 0.5 0.5])))) 100)})
      :else
      (let [w (assoc-in w [:self :held] item)]
        (cond
          (and (re-find #"_fence$" here) (or (= item "lead") hand?))
          (let [[w' tied] (tie-to-post w pos)] (done w' (if (pos? tied) "used" "unchanged")))

          ;; A block with `locked: true` models a protected area: the click is lost.
          (and hand? (:locked props)) (done w "unchanged")

          ;; doors, trapdoors and gates flip `open` (iron ones ignore a hand); a lever flips `powered`; a button sets it
          (and hand? (doors/openable? here) (not (re-find #"^iron_" here))) (done (doors/flip-open w pos) "used")
          (and hand? (= here "lever")) (done (doors/set-wired w pos (not (:powered props))) "used")
          (and hand? (re-find #"_button$" here) (not (:powered props))) (done (doors/press w pos) "used")

          (and item (re-find #"_hoe$" item) (tillable here) (not= face "down")
               (not (contains? (:blocks w) (update pos 1 inc))))
          (done (assoc-in w [:blocks pos] "farmland") "used")

          (and item (re-find #"_shovel$" item) (pathable here) (not= face "down")
               (not (contains? (:blocks w) (update pos 1 inc))))
          (done (assoc-in w [:blocks pos] "dirt_path") "used")

          (and (= item "bone_meal") (max-age here))
          (if (>= (or age 0) (max-age here))
            (done w "unchanged")
            (done (-> w (assoc-in [:ages pos] (min (max-age here) (+ (or age 0) 2))) (take-one item)) "used" 1))

          (and (= item "bone_meal") (or (re-find #"_sapling$" here) (= here "grass_block")))
          (done (take-one w item) "used" 1)

          (and (hives here) (>= (get props :honey_level 0) 5) (#{"shears" "glass_bottle"} item))
          (let [w (assoc-in w [:states pos :honey_level] 0)]
            (if (= item "shears")
              (done (spawn-item w (landing w (mapv + pos (face-delta face))) "honeycomb" 3) "used")
              (done (-> w (take-one item) (give-one "honey_bottle")) "used" 1)))

          (and (= here "composter") (= level 8))
          (done (-> w (assoc-in [:states pos :level] 0) (spawn-item (update pos 1 inc) "bone_meal" 1)) "used")

          (and (= here "composter") (< level 7) (compostable item))
          (done (-> w (take-one item) (assoc-in [:states pos :level] (if (= (inc level) 7) 8 (inc level)))) "used" 1)

          :else (done w "unchanged"))))))
