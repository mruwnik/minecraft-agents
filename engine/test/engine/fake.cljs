(ns engine.fake
  "The fake world the cljs tests drive: the primitives object of README.md over one atom of cljs world data, never a
  server. (create spec) takes the spec as a cljs map (the keys createFake took: :self :time :players :raining :blocks
  {\"x,y,z\" name} :entities :inventory :equipment :containers :drops :recipes :unreachable :noPath :swimFails
  :mountFails :dismountFails :skipNight :settles :offlineScale :ages :states :unloaded :furnaces :enchantTables) and
  returns the JS object the engine's primitives are: methods taking (token, args) and returning promises of JS
  contract results, plus `world` (hold, override, calls, emit, setTime, advance, setRaining, settle, respawn, die,
  state).

  world.state is an atom of
    {:self {:username :pos [x y z] :health :food :foodSaturation :oxygen :onFire :inWater :inLava :onGround
            :isSleeping :effects :experience {:level :points :progress} :dimension :held :vehicle}
     :time :players [username] :raining :thundering :blocks {[x y z] name} :unloaded #{pos} :ages {pos n} :states {pos {prop v}}
     :entities [{:id :name :kind :pos [x y z] ...}] (flags kebab-case: :leashed-to-me :in-love :break-at ...)
     :inventory [{:name :count}] :equipment {part stack} :containers {pos [stack]} :drops :chat [{:message :to}]
     :recipes :unreachable :no-path #{pos} :furnaces :enchant-tables :controls :yaw :pitch :offline :settling
     :view-chunks n (spec :viewChunks; the pathWorld loads only the chunk columns within n chunks of the body's, as a
     server's view distance does: land the body walked away from unloads; nil loads every column) ...}
  The helpers state, set-block!, remove-block!, add-entity!, entities, self, swap-self! and add-item! are how tests
  read and change it. The mechanics (interact, leads, tempt, steer, furnace, enchant, trade, use-on, rails, placing,
  doors) are the engine.fake.* namespaces; this one holds the rest (walk, dig, place, craft, ...), the owner token,
  hold/override and the offline timing, and converts to and from the JS contract. Test-only."
  (:require ["minecraft-data" :as minecraft-data]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [engine.fake.animals :as animals]
            [engine.fake.enchant :as enchant]
            [engine.fake.furnace :as furnace]
            [engine.fake.rail :as rail]
            [engine.fake.placing :as placing]
            [engine.fake.steer :as steer]
            [engine.fake.trade :as trade]
            [engine.fake.unequip :as unequip]
            [engine.fake.use-on :as use-on]
            [engine.fake.node :as node]))

;; The JS modules the real primitives share with the fake (why they are not ported: they are the real ones).
(def sight (delay (node/require-here "./js/sight.mjs")))
(def blocks-mod (delay (node/require-here "./js/blocks.mjs")))
(def chat-mod (delay (node/require-here "./js/chat.mjs")))
(def vehicle-mod (delay (node/require-here "./js/vehicle.mjs")))

(def reach 4.5)
(def eye 1.62)
(def body-middle 0.9)
(def craft-reach 4.5)
(def entity-height 1.8)
(def hit-range 6)
(def fake-body-id -1)
(def offline-default-ms (* 5 60 1000))
(def offline-max-ms (* 10 60 1000))
(def ms-per-tick 50)
(def no-shape #{"air" "cave_air" "water" "lava" "fire" "short_grass" "tall_grass" "snow"})
(def see-through #{"air" "water" "lava" "fire" "short_grass" "tall_grass" "snow" "glass" "glass_pane" "torch" "wall_torch" "soul_torch"})
;; blocks whose collision shape does not fill the cell (or that have none); every other block is a full cube
(def not-full-cube
  #"^(air|cave_air|void_air|water|lava|bubble_column|fire|soul_fire|short_grass|tall_grass|fern|large_fern|snow|wheat|carrots|potatoes|beetroots|farmland|dirt_path|soul_sand|torch|wall_torch|vine|cobweb|ladder|sugar_cane|dead_bush|kelp|seagrass|tall_seagrass|rail|lever|dandelion|poppy|wither_rose|sunflower|lilac|rose_bush|peony)$|_(slab|stairs|carpet|sapling|sign|flower|button|pressure_plate|fence|pane|torch|rail)$")
(def fence-post [0.375 0 0.375 0.625 1.5 0.625])
(def pane-post [0.4375 0 0.4375 0.5625 1 0.5625])
(def crops {"wheat_seeds" "wheat" "carrot" "carrots" "potato" "potatoes" "beetroot_seeds" "beetroots"})
(def foods ["cooked_beef" "cooked_porkchop" "bread" "baked_potato" "cooked_chicken" "carrot" "apple" "sweet_berries"
            "beef" "porkchop" "mutton" "chicken" "rabbit"])
(def default-recipes
  {"bread" {:count 1 :needs {"wheat" 3} :table true}
   "oak_planks" {:count 4 :needs {"oak_log" 1}}
   "stick" {:count 4 :needs {"oak_planks" 2}}
   "crafting_table" {:count 1 :needs {"oak_planks" 4}}
   "torch" {:count 4 :needs {"coal" 1 "stick" 1}}
   "wooden_pickaxe" {:count 1 :needs {"oak_planks" 3 "stick" 2} :table true}
   "stone_pickaxe" {:count 1 :needs {"cobblestone" 3 "stick" 2} :table true}})
(def default-self
  {:username "Fake" :pos [0 64 0] :health 20 :food 20 :foodSaturation 5 :oxygen 20 :onFire false :inWater false
   :inLava false :onGround true :isSleeping false :effects [] :experience {:level 0 :points 0 :progress 0}
   :dimension "overworld"})
(def equipment-parts ["head" "torso" "legs" "feet" "offHand"])

(defn day-at? [t] (or (< t 12542) (> t 23460)))

;; ---- JS boundary

(defn kebab [s] (str/replace s #"[A-Z]" #(str "-" (str/lower-case %))))
(defn camel [s] (str/replace s #"-([a-z])" #(str/upper-case (second %))))
(defn key-name [k] (if (keyword? k) (name k) (str k)))
(defn rekey [f m] (into {} (map (fn [[k v]] [(keyword (f (key-name k))) v])) m))

(defn xyz? [m] (and (map? m) (= #{:x :y :z} (set (keys m)))))
(defn js-xyz? [o] (and (object? o) (number? (.-x o)) (number? (.-z o))))

(declare vec-pos)

(defn norm-pos
  "Every {:x :y :z} map and {x y z} JS object inside x as a vector."
  [x]
  (walk/postwalk #(if (or (xyz? %) (js-xyz? %)) (vec-pos %) %) x))

(defn vec-pos
  "[x y z] for a {:x :y :z} map or JS object; vectors pass through."
  [p]
  (cond (vector? p) p
        (map? p) [(:x p) (:y p) (:z p)]
        :else [(.-x p) (.-y p) (.-z p)]))

(defn pos-js [[x y z]] #js {:x x :y y :z z})

(defn args-in
  "A call's JS args as cljs data: keyword keys, every {:x :y :z} as a vector, functions left alone."
  [a]
  (norm-pos (js->clj a :keywordize-keys true)))

(def pos-keys #{:pos :table :dismountAt})

(defn to-js
  "A result as JS: keywords to strings, :pos-like vectors to {x y z} objects; raw JS values stay."
  [x]
  (cond
    (map? x) (let [o #js {}]
               (doseq [[k v] x]
                 (aset o (key-name k) (if (and (pos-keys k) (vector? v) (= 3 (count v))) (pos-js v) (to-js v))))
               o)
    (sequential? x) (to-array (map to-js x))
    (set? x) (to-array (map to-js x))
    (keyword? x) (name x)
    :else x))

(defn js-error [e]
  (let [{:keys [code]} (ex-data e)]
    (doto (js/Error. (ex-message e))
      (aset "code" code)
      (aset "badArgs" (= code "bad-args")))))

(defn cut-error [] (js-error (ex-info "cut: the ownership token changed" {:code "cut"})))

(defn entity-in
  "A spec entity: kebab-case keys, positions as vectors, the defaults of a player and a creeper."
  [e]
  (let [e (rekey kebab (norm-pos e))]
    (merge {:health 20}
           (when (= "player" (:kind e)) {:sleeping false :username (:name e)})
           (when (= "creeper" (:name e)) {:creeper true})
           e)))

(defn entity-js
  "An entity as the primitives report it: camelCase keys, {x y z} positions."
  [e]
  (to-js (rekey camel (cond-> e (:dismount-at e) (update :dismount-at pos-js)))))

;; ---- the initial world

(defn parse-cell [k] (mapv js/Number (str/split (key-name k) #",")))
(defn cells [m f] (into {} (map (fn [[k v]] [(parse-cell k) (f v)])) m))
(defn cell-set [coll] (into #{} (map parse-cell) coll))
(defn string-keys [m] (into {} (map (fn [[k v]] [(key-name k) v])) m))

(defn centre-xz
  "pos with a whole x or z moved to the middle of its cell (a body stands at the middle of a cell; fractions stay)."
  [[x y z]]
  (let [mid #(if (= % (js/Math.floor %)) (+ % 0.5) %)]
    [(mid x) y (mid z)]))

(defn initial-state [spec]
  (let [spec (if (object? spec) (js->clj spec :keywordize-keys true) spec)
        self (cond-> (norm-pos (:self spec))
               (and (:bodyHitbox spec) (:pos (:self spec))) (update :pos centre-xz))]
    (-> {:self (merge default-self self {:held (:held self)})
         :time (:time spec 1000)
         :players (vec (:players spec))
         :raining (:raining spec false)
         :thundering (:thundering spec false)
         :blocks (cells (:blocks spec) identity)
         :rank (into {} (map-indexed (fn [i [k _]] [(parse-cell k) i])) (:blocks spec))
         :rank-next (count (:blocks spec))
         :unloaded (cell-set (:unloaded spec))
         :ages (cells (:ages spec) identity)
         :states (cells (:states spec) identity)
         :entities (mapv entity-in (:entities spec))
         :inventory (vec (:inventory spec))
         :equipment (string-keys (:equipment spec))
         :containers (cells (:containers spec) vec)
         :drops (string-keys (:drops spec))
         :chat []
         :recipes (merge default-recipes
                         (into {} (map (fn [[k r]] [(key-name k) (update r :needs string-keys)])) (:recipes spec)))
         :unreachable (cell-set (:unreachable spec))
         :no-path (cell-set (:noPath spec))
         :swim-fails (:swimFails spec false)
         :mount-fails (:mountFails spec false)
         :dismount-fails (:dismountFails spec false)
         :next-entity-id 1000
         :offline false
         :controls {}
         :yaw (:yaw spec 0)
         :pitch (:pitch spec 0)
         :skip-night (:skipNight spec true)
         :settles (:settles spec false)
         :body-hitbox (:bodyHitbox spec false)
         :view-chunks (:viewChunks spec)
         :settling false
         :offline-scale (:offlineScale spec 0.001)
         :furnaces (cells (:furnaces spec) #(rekey kebab %))
         :enchant-tables (cells (:enchantTables spec) identity)}
        furnace/start)))

;; ---- blocks keep the order they were put in (as the old Map did): the nearest-first lists break ties by it

(defn put-block
  "World with block at pos; a new cell goes last in the order."
  [w pos block]
  (cond-> (assoc-in w [:blocks pos] block)
    (not (contains? (:blocks w) pos)) (-> (assoc-in [:rank pos] (:rank-next w)) (update :rank-next inc))))

(defn drop-block [w pos] (-> w (update :blocks dissoc pos) (update :rank dissoc pos)))

;; ---- item lists

(defn add-to
  "Items with count of name added to the first stack of it (or a new one)."
  [items item n]
  (let [i (first (keep-indexed #(when (= item (:name %2)) %1) items))]
    (if i (update-in items [i :count] + n) (conj items {:name item :count n}))))

(defn take-from
  "[items' taken]: up to n of item out of the first stack of it; an emptied stack goes."
  [items item n]
  (let [i (first (keep-indexed #(when (= item (:name %2)) %1) items))]
    (if-not i
      [items 0]
      (let [taken (min n (:count (items i)))
            left (- (:count (items i)) taken)]
        [(if (zero? left) (into (subvec items 0 i) (subvec items (inc i))) (assoc-in items [i :count] left)) taken]))))

(defn carried [items item] (transduce (comp (filter #(= item (:name %))) (map :count)) + 0 items))
(defn with-slots [items] (vec (map-indexed (fn [slot i] (cond-> {:name (:name i) :count (:count i) :slot slot} (:durability i) (assoc :durability (:durability i) :maxDurability (:max-durability i)))) items)))

(defn give [w item n] (update w :inventory add-to item n))
(defn take-one [w item] (update w :inventory #(first (take-from % item 1))))

;; ---- world queries

(defn dist [[ax ay az] [bx by bz]] (js/Math.hypot (- ax bx) (- ay by) (- az bz)))
(defn block-name [w pos] (get-in w [:blocks pos] "air"))
(defn body-pos [w] (get-in w [:self :pos]))
(defn near? [w pos] (<= (dist (body-pos w) pos) reach))
(defn find-entity [w id] (first (filter #(= id (:id %)) (:entities w))))
(defn replaceable? [item] (.isReplaceable ^js @blocks-mod item))

(defn spawn [w pos item n] (use-on/spawn-item w pos item n))

(defn settle
  "Where the body stands decides lava and water: lava is left by stepping out of it, water puts out fire."
  [w]
  (let [here (block-name w (body-pos w))]
    (-> w
        (assoc-in [:self :inLava] (= here "lava"))
        (assoc-in [:self :inWater] (= here "water"))
        (update :self #(cond-> % (= here "water") (assoc :onFire false))))))

(defn step-toward [from to maxd]
  (let [f (/ maxd (dist from to))]
    (mapv (fn [a b] (js/Math.round (+ a (* (- b a) f)))) from to)))

(declare can-see?)

(defn chase-body
  "Hostiles with :chase {:speed s :follow r} move s blocks per block the body walked, straight toward it, while it is
  within r (default 35) and in sight (:visible when given); else they stay."
  [w from]
  (let [to (body-pos w)
        walked (dist from to)
        chase (fn [e]
                (let [{:keys [speed follow] :or {follow 35}} (:chase e)
                      d (dist (:pos e) to)]
                  (if (and (:chase e) (pos? d) (<= d follow) (if (some? (:visible e)) (:visible e) (can-see? w e)))
                    (assoc e :pos (mapv (fn [a b] (+ a (* (- b a) (min 1 (/ (* speed walked) d))))) (:pos e) to))
                    e)))]
    (update w :entities #(mapv chase %))))

(defn after-walk [w from] (-> w (animals/drag-leashed from) animals/tempt-follow (chase-body from)))

(defn properties-of
  "A cell's block properties: the rail's, its state and its crop age."
  [w pos]
  (merge (rail/rail-properties w pos) (get-in w [:states pos]) (when (contains? (:ages w) pos) {:age (get-in w [:ages pos])})))

;; ---- the acts: (act w args) -> [world' result], args as cljs data

(defn body-cells
  "The cells the body's hitbox (0.6 wide, 1.8 tall) intersects."
  [w]
  (let [[x y z] (body-pos w)
        span (fn [v _] (range (js/Math.floor (- v 0.3)) (inc (js/Math.floor (+ v 0.3 -1e-9)))))]
    (set (for [i (span x 0.6) j (range (js/Math.floor y) (inc (js/Math.floor (+ y 1.8 -1e-9)))) k (span z 0.6)] [i j k]))))

(defn move-to [w {:keys [pos range maxDistance] :or {range 1 maxDistance 64}}]
  (let [here (body-pos w)
        d (dist here pos)
        blocked (fn [extra] [w (merge {:status "blocked" :pos here :distance d} extra)])]
    (cond
      ((:unreachable w) pos) (blocked nil)
      ((:no-path w) pos) (blocked {:reason "noPath"})
      (<= d range) [w {:status "arrived" :pos here :distance d}]
      (> d maxDistance)
      (let [w' (-> w (assoc-in [:self :pos] (step-toward here pos maxDistance)) (after-walk here))]
        [w' {:status "partial" :pos (body-pos w') :distance (dist (body-pos w') pos)}])
      :else
      (let [stand (if (:body-hitbox w) (centre-xz pos) pos)
            w' (-> w (assoc-in [:self :pos] stand) settle (after-walk here))]
        [w' {:status "arrived" :pos stand :distance 0}]))))

(defn dig [w {:keys [pos]}]
  (let [block (block-name w pos)
        dropped (if (contains? (:drops w) block) (get (:drops w) block) block)
        names (filter seq (if (sequential? dropped) dropped [dropped]))]
    (cond
      (= block "air") [w {:status "missing"}]
      (not (near? w pos)) [w {:status "unreachable"}]
      (= block "bedrock") [w {:status "cannot"}]
      :else
      (let [w (-> w (drop-block pos) (update :ages dissoc pos) (update :states dissoc pos))
            ids (map #(+ (:next-entity-id w) %) (range (count names)))
            w' (reduce #(spawn %1 pos %2 1) w names)]
        [w' {:status "dug" :block block :drops (mapv (fn [id item] {:id id :name item :count 1 :pos pos}) ids names)}]))))

(defn look-at
  "The body turned (Minecraft degrees on :yaw/:pitch, as drive sets them): toward pos from the eye (the feet cell's
  centre, 1.62 up), else from a mineflayer yaw and pitch in radians, as the real look takes them."
  [w {:keys [pos yaw pitch]}]
  (let [deg (/ 180 js/Math.PI)]
    (if pos
      (let [[x y z] (body-pos w)
            [px py pz] pos
            dx (- px (+ (js/Math.floor x) 0.5)) dy (- py (+ y 1.62)) dz (- pz (+ (js/Math.floor z) 0.5))]
        (assoc w :yaw (mod (* deg (js/Math.atan2 (- dx) dz)) 360) :pitch (- (* deg (js/Math.atan2 dy (js/Math.hypot dx dz))))))
      (assoc w :yaw (mod (- 180 (* deg yaw)) 360) :pitch (- (* deg pitch))))))

(defn plain-click
  "The click a place without one makes: the first solid neighbour's face (below first), looked at from the eye."
  [w pos]
  (when-some [against (->> [[0 -1 0] [1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0]]
                           (map #(mapv + pos %))
                           (remove #(no-shape (block-name w %)))
                           first)]
    (let [cursor (mapv #(+ 0.5 (/ (- %1 %2) 2)) pos against)
          [dx dy dz] (map - (mapv + against cursor) (body-pos w) [0 eye 0])]
      {:against against :cursor cursor :yaw (js/Math.atan2 (- dx) (- dz)) :pitch (js/Math.atan2 dy (js/Math.hypot dx dz))})))

(defn place-block
  "A block item takes the state the game gives it from the click (engine.fake.placing); without a click the face is
  the first solid neighbour's. The game refusing it (no room for a door's upper half) places nothing, keeps the item."
  [w pos item click]
  (let [c (or click (plain-click w pos))
        r (if c
            (placing/placed-blocks w {:item item :pos pos :face (mapv - pos (:against c)) :cursor (:cursor c)
                                      :yaw (or (:yaw c) (:yaw w)) :pitch (or (:pitch c) (:pitch w))})
            {:blocks [{:pos pos :name item :properties {}}]})]
    (if (:refused r)
      [w {:status "failed" :reason (str "Server refused to place " item " at (" (str/join ", " pos) "): " (:refused r))}]
      (let [w (reduce (fn [w {:keys [pos name properties]}]
                        (-> w (put-block pos name)
                            (update :states #(if (seq properties) (assoc % pos properties) (dissoc % pos)))))
                      (take-one w item) (:blocks r))
            w (rail/rails-placed w (:blocks r))]
        [w {:status "placed" :block item
            :placed {:name (:name (first (:blocks r)))
                     :properties (merge (rail/rail-properties w pos) (:properties (first (:blocks r))))}}]))))

(defn place [w {:keys [pos item click]}]
  (let [here (block-name w pos)
        failed #(vector w {:status "failed" :reason (str "Server refused to place " item " at (" (str/join ", " pos) "): the block is still air")})]
    (cond
      (not (near? w pos)) [w {:status "unreachable"}]
      (and (:body-hitbox w) (not (#{"bucket" "water_bucket"} item)) (contains? (body-cells w) pos)) (failed)

      (= item "bucket")                 ; scoops the water cell it is aimed at
      (cond
        (not= here "water") [w {:status "missing"}]
        (zero? (carried (:inventory w) "bucket")) [w {:status "no-item"}]
        :else [(-> w (take-one "bucket") (give "water_bucket" 1) (drop-block pos)) {:status "placed" :block "bucket"}])

      (and (not (#{"air" "water" "lava"} here)) (not (replaceable? here))) [w {:status "occupied"}]
      (and (crops item) (not= "farmland" (block-name w (update pos 1 dec)))) (failed)
      (zero? (carried (:inventory w) item)) [w {:status "no-item"}]
      (and click (no-shape (block-name w (:against click)))) [w {:status "no-support"}]

      (crops item)                      ; a seed or tuber becomes the young crop
      [(-> w (take-one item) (put-block pos (crops item)) (assoc-in [:ages pos] 0)) {:status "placed" :block item}]

      (= item "water_bucket")           ; pours water and leaves the empty bucket
      (let [w (-> w (take-one item) (give "bucket" 1) (put-block pos "water"))]
        [(cond-> w (= pos (body-pos w)) settle) {:status "placed" :block "water"}])

      (str/ends-with? item "campfire")
      [(-> w (take-one item) (assoc-in [:states pos] {:lit true}) (put-block pos item))
       {:status "placed" :block item :placed {:name item :properties {:lit true}}}]

      :else (place-block w pos item click))))

(defn jump-place
  "Raises the body one block per placement: needs the item, something solid under the feet and the two cells above the
  feet free (the cell the head moves into is the one two above the start)."
  [w {:keys [item count] :or {count 1}}]
  (let [total (min (or count 1) 8)
        outcome (fn [w placed reason]
                  [w (cond-> {:status (cond (= placed total) "done" (pos? placed) "partial" :else "failed") :placed placed}
                       reason (assoc :reason reason))])]
    (loop [w w placed 0]
      (let [[x y z] (body-pos w)
            at [(js/Math.floor x) y (js/Math.floor z)]     ; a body-hitbox position is fractional: the cell it is over
            [cx _ cz] at
            under (block-name w [cx (dec y) cz])]
        (cond
          (= placed total) (outcome w placed nil)
          (zero? (carried (:inventory w) item)) (outcome w placed "no-item")
          (#{"air" "water" "lava"} under) (outcome w placed "no-support")
          (not= "air" (block-name w [cx (+ y 2) cz])) (outcome w placed "no-headroom")
          :else (recur (-> w (take-one item) (put-block at item) (assoc-in [:self :pos] [x (inc y) z])) (inc placed)))))))

(defn collect [w {:keys [id]}]
  (let [e (find-entity w id)]
    (cond
      (or (nil? e) (not= "item" (:kind e))) [w {:status "gone" :gained []}]
      ((:unreachable w) (:pos e)) [w {:status "unreachable" :gained []}]
      :else [(-> w (assoc-in [:self :pos] (:pos e)) (update :entities #(filterv (fn [x] (not= id (:id x))) %))
                 (give (get-in e [:item :name]) (get-in e [:item :count])))
             {:status "collected" :gained [(select-keys (:item e) [:name :count])]}])))

(defn toss
  "Throws the item three blocks along +x as one item entity (the real body throws where it looks)."
  [w {:keys [item count slot]}]
  (let [[x y z] (body-pos w)
        thrown [(+ x 3) y z]
        total (carried (:inventory w) item)
        n (min (or count total) total)]
    (cond
      (and (number? slot) (let [stack (get (:inventory w) slot)] (or (nil? stack) (not= item (:name stack)))))
      [w {:status "no-item" :count 0}]

      (number? slot)                    ; exactly that slot's whole stack
      (let [stack (get (:inventory w) slot)]
        [(-> w (update :inventory #(into (subvec % 0 slot) (subvec % (inc slot)))) (spawn thrown item (:count stack)))
         {:status "tossed" :count (:count stack)}])

      (<= n 0) [w {:status "no-item" :count 0}]

      :else [(-> (loop [w w left n]
                   (if (pos? left)
                     (let [[inv taken] (take-from (:inventory w) item left)] (recur (assoc w :inventory inv) (- left taken)))
                     w))
                 (spawn thrown item n))
             {:status "tossed" :count n}])))

(defn craft [w {:keys [item count table] :or {count 1}}]
  (let [recipe (get (:recipes w) item)
        inv #(:inventory %)
        crafting-table-near? (fn [pos] (and (= "crafting_table" (block-name w pos)) (<= (dist (body-pos w) pos) craft-reach)))
        short-of (fn [w] (into {} (keep (fn [[n c]] (let [m (- c (carried (inv w) n))] (when (pos? m) [n m])))) (:needs recipe)))
        shortage (fn [w] {:recipes [(:needs recipe)] :have (into {} (map (fn [i] [(:name i) (carried (inv w) (:name i))])) (inv w))})]
    (cond
      (nil? recipe) [w {:status "cannot" :reason "no-recipe"}]
      (and (:table recipe) table (not= "crafting_table" (block-name w table))) [w {:status "unreachable" :reason "not-a-table"}]
      (and (:table recipe) table (not (crafting-table-near? table))) [w {:status "out-of-reach" :reason "too-far" :table table}]
      (and (:table recipe) (not table)) [w {:status "unreachable" :reason "no-table"}]
      (and (empty? (short-of w)) (>= (clojure.core/count (inv w)) 36) (zero? (carried (inv w) item))) [w {:status "full" :made 0 :used {}}]
      :else
      (let [[w' made used] (loop [w w made 0 used {} batches (js/Math.ceil (/ count (:count recipe)))]
                             (if (and (pos? batches) (empty? (short-of w)))
                               (let [w (reduce (fn [w [n c]] (update w :inventory #(first (take-from % n c)))) w (:needs recipe))]
                                 (recur (give w item (:count recipe)) (+ made (:count recipe))
                                        (merge-with + used (:needs recipe)) (dec batches)))
                               [w made used]))]
        (cond
          (>= made count) [w' {:status "crafted" :item item :made made :used used}]
          (pos? made) [w' (merge {:status "partial" :item item :made made :used used :reason "no-item"} (shortage w'))]
          :else [w' (merge {:status "no-item"} (shortage w'))])))))

(defn chat [w {:keys [message to]}]
  (.assertSendable ^js @chat-mod message)
  (cond
    (and to (not-any? #(and (= "player" (:kind %)) (or (= to (:username %)) (= to (:name %)))) (:entities w)))
    [w {:status "gone" :to to}]
    :else [(update w :chat conj {:message message :to to})
           (cond-> {:status "sent" :parts 1} to (assoc :to to))]))

(defn inspect-container [w {:keys [pos]}]
  (let [items (get-in w [:containers pos])]
    (cond
      (nil? items) [w {:status "missing"}]
      (not (near? w pos)) [w {:status "unreachable"}]
      :else [w {:status "ok" :items (with-slots items)}])))

(defn transfer [w {:keys [pos direction item count]}]
  (let [items (get-in w [:containers pos])]
    (cond
      (nil? items) [w {:status "missing" :moved 0}]
      (not (near? w pos)) [w {:status "unreachable" :moved 0}]
      :else
      (let [deposit? (= direction "deposit")
            [from to] (if deposit? [(:inventory w) items] [items (:inventory w)])
            [from' moved] (take-from from item (or count (carried from item)))
            to' (add-to to item moved)]
        (if (zero? moved)
          [w {:status "no-item" :moved 0}]
          [(if deposit? (-> w (assoc :inventory from') (assoc-in [:containers pos] to')) (-> w (assoc :inventory to') (assoc-in [:containers pos] from')))
           {:status "ok" :moved moved}])))))

(defn equip [w {:keys [item dest]}]
  (let [part (when (contains? #{"head" "torso" "legs" "feet"} dest) dest)
        old (when part (get-in w [:equipment part]))]
    (cond
      (zero? (carried (:inventory w) item)) [w {:status "no-item"}]
      (not part) [(assoc-in w [:self :held] item) {:status "equipped"}]
      :else [(cond-> (-> w (update :inventory #(first (take-from % item 1))) (assoc-in [:equipment part] {:name item :count 1}))
               old (update :inventory add-to (:name old) 1))
             {:status "equipped"}])))

(defn eat [w {:keys [item]}]
  (let [food (or item (first (filter #(pos? (carried (:inventory w) %)) foods)))
        hunger (get-in w [:self :food])]
    (cond
      (>= hunger 20) [w {:status "full" :food hunger}]
      (or (nil? food) (zero? (carried (:inventory w) food))) [w {:status "no-food"}]
      :else (let [now (min 20 (+ hunger 5))]
              [(-> w (take-one food) (assoc-in [:self :food] now)) {:status "ate" :item food :food now}]))))

(defn attack [w {:keys [id]}]
  (let [e (find-entity w id)]
    (cond
      (nil? e) [w {:status "gone"}]
      (not (near? w (:pos e))) [w {:status "out-of-reach"}]
      (:invulnerable e) [w {:status "hit" :health (:health e) :hurt false}]
      (pos? (- (:health e) 5))
      [(animals/update-entity w id #(update % :health - 5)) {:status "hit" :health (- (:health e) 5) :hurt true}]
      (:lingers e) [(animals/update-entity w id #(assoc % :health 0)) {:status "killed" :health 0 :hurt true}]
      :else [(reduce (fn [w d] (spawn w (:pos e) (:name d) (:count d)))
                     (update w :entities #(filterv (fn [x] (not= id (:id x))) %)) (:drops e))
             {:status "killed" :health 0 :hurt true}])))

(defn sleep [w {:keys [pos]}]
  (cond
    (day-at? (:time w)) [w {:status "not-night"}]
    (not (str/ends-with? (block-name w pos) "_bed")) [w {:status "missing"}]
    (not (near? w pos)) [w {:status "unreachable"}]
    ;; the night is skipped at once and the server wakes the body at morning; with others awake it lies in bed
    (:skip-night w) [(assoc w :time 0) {:status "sleeping"}]
    :else [(assoc-in w [:self :isSleeping] true) {:status "sleeping"}]))

(defn swim
  "Rises to the top water cell above the body and refills oxygen; swimFails makes it time out unmoved. With :toward it
  swims there by the steer's kinematics (steer/swim-lands?): landed on the target, else timed out on a jump crest
  where it was, out of the water and off the ground."
  [w {:keys [toward]}]
  (let [before (get-in w [:self :oxygen])
        head #(block-name % (update (body-pos %) 1 + 1))
        risen (loop [w w] (if (= "water" (head w)) (recur (update-in w [:self :pos 1] inc)) w))
        w' (assoc-in risen [:self :oxygen] 20)
        oxygen {:before before :after 20}
        crest #(-> % (assoc-in [:self :inWater] false) (assoc-in [:self :onGround] false))]
    (cond
      (:swim-fails w) [w {:status "timeout" :oxygen {:before before :after before}}]
      (not toward) [(assoc-in w' [:self :inWater] true) {:status "surfaced" :oxygen oxygen}]
      (or (> (dist (body-pos w') toward) 6) (not (steer/swim-lands? w' toward))) [(crest w') {:status "timeout" :oxygen oxygen}]
      :else [(-> w' (assoc-in [:self :pos] toward) settle (assoc-in [:self :onGround] true)) {:status "landed" :oxygen oxygen}])))

(declare vehicle-of)

(defn mount [w {:keys [id]}]
  (let [e (find-entity w id)
        riders (count (:passengers e))
        refused (when e (.mountRefusal ^js @vehicle-mod (:name e) riders))
        vehicle (get-in w [:self :vehicle])]
    (cond
      (some? vehicle) [w {:status "already-mounted" :vehicle (vehicle-of w)}]
      (nil? e) [w {:status "gone"}]
      refused [w {:status refused}]
      (> (dist (body-pos w) (:pos e)) (.-MOUNT_REACH ^js @vehicle-mod)) [w {:status "out-of-reach"}]
      (:mount-fails w) [w {:status "timeout"}]
      :else (let [w' (-> w (assoc-in [:self :held] nil) (assoc-in [:self :vehicle] id) (assoc-in [:self :pos] (:pos e))
                         (animals/update-entity id #(update % :passengers (fnil conj []) fake-body-id)))]
              [w' {:status "mounted" :vehicle (vehicle-of w')}]))))

(defn vehicle-of
  "The vehicle the body rides as {:id :uuid :name}, or nil."
  [w]
  (when-some [id (get-in w [:self :vehicle])]
    (let [e (find-entity w id)]
      (if e {:id (:id e) :uuid (:uuid e) :name (:name e)} {:id id :uuid nil :name nil}))))

(defn dismount [w {:keys [yaw]}]
  (let [id (get-in w [:self :vehicle])
        e (find-entity w id)
        from (or (:pos e) (body-pos w))
        pos (or (:dismount-at e) (update from 0 inc))
        here (block-name w (mapv #(js/Math.floor %) pos))
        left (fn [passengers] (not-empty (filterv #(not= fake-body-id %) passengers)))]
    (cond
      (nil? id) [w {:status "not-mounted"}]
      (:dismount-fails w) [w {:status "timeout" :mounted true}]
      :else [(-> (cond-> w e (animals/update-entity id #(let [p (left (:passengers %))] (if p (assoc % :passengers p) (dissoc % :passengers)))))
                 (assoc-in [:self :vehicle] nil) (assoc :dismount-yaw yaw) (assoc-in [:self :pos] pos)
                 (assoc-in [:self :inWater] (= here "water")) (assoc-in [:self :inLava] (= here "lava")))
             {:status "dismounted" :pos pos}])))

;; ---- reading the world

(defn equipment-view [w]
  (let [gear (fn [i] (when i (cond-> {:name (:name i) :count (:count i 1)} (some? (:durability i)) (assoc :durability (:durability i)))))
        held (get-in w [:self :held])
        carried-stack (first (filter #(= held (:name %)) (:inventory w)))]
    (merge (into {} (map (fn [part] [part (gear (get-in w [:equipment part]))])) equipment-parts)
           {"mainHand" (when held (gear {:name held :count (:count carried-stack 1)}))})))

(defn eye-point [w [x y z]] #js {:x (+ x 0.5) :y y :z (+ z 0.5)})

(defn cell-of [c] [(.-x c) (.-y c) (.-z c)])

(defn can-see?
  "A cell with a block that is not see-through stops the eye; unknown cells and unloaded ones do not."
  [w e]
  (let [[sx sy sz] (body-pos w) [ex ey ez] (:pos e)
        blocks-sight (fn [c] (let [cell (cell-of c)] (and (not (see-through (block-name w cell))) (not ((:unloaded w) cell)))))]
    (.lineClear ^js @sight #js {:x (+ sx 0.5) :y (+ sy eye) :z (+ sz 0.5)} #js {:x (+ ex 0.5) :y (+ ey body-middle) :z (+ ez 0.5)} blocks-sight)))

(defn can-hit?
  "A melee swing needs a clear line from the eye to some point of the target's hitbox; blocks by name."
  [w e]
  (let [[sx sy sz] (body-pos w) [ex ey ez] (:pos e)
        shapes-at (fn [c]
                    (let [cell (cell-of c) block (block-name w cell)]
                      (cond
                        (or (no-shape block) ((:unloaded w) cell)) #js []
                        (str/ends-with? block "_fence") #js [(to-array fence-post)]
                        (= block "glass_pane") #js [(to-array pane-post)]
                        :else #js [#js [0 0 0 1 1 1]])))]
    (boolean (some #(.rayClear ^js @sight #js {:x (+ sx 0.5) :y (+ sy eye) :z (+ sz 0.5)} #js {:x (+ ex 0.5) :y (+ ey %) :z (+ ez 0.5)} shapes-at)
                   [0.2 (/ entity-height 2) (- entity-height 0.1)]))))

(defn entity-view [w e]
  (let [distance (dist (body-pos w) (:pos e))
        o (entity-js (dissoc e :offers :busy))]
    (aset o "distance" distance)
    (when (#{"hostile" "item" "player"} (:kind e)) (aset o "visible" (if (some? (:visible e)) (:visible e) (can-see? w e))))
    (when (= "player" (:kind e)) (aset o "sleeping" (boolean (and (:sleeping e) (can-see? w e)))))
    (when-not (or (= "item" (:kind e)) (> distance hit-range))
      (aset o "hittable" (if (some? (:hittable e)) (:hittable e) (can-hit? w e))))
    o))

(defn self-view [w]
  (let [s (:self w)]
    (to-js (merge (dissoc s :vehicle :held :pos :experience)
                  {:username (:username s) :pos (:pos s) :settling (:settling w)
                   :vehicle (some-> (vehicle-of w) (update :uuid identity))
                   :experience (:experience s)
                   :timeOfDay (:time w) :isDay (day-at? (:time w)) :players (:players w) :raining (:raining w) :thundering (:thundering w)
                   :held (:held s) :equipment (equipment-view w) :inventory (with-slots (:inventory w))}))))

;; ---- the world object's helpers for tests

(defn state "The world atom of a fake." [p] (.-state (.-world p)))
(defn entities "The fake's entities, as cljs maps." [p] (:entities @(state p)))
(defn self "The fake body's :self map." [p] (:self @(state p)))
(defn swap-self! [p f & args] (apply swap! (state p) update :self f args))
(defn set-block! [p pos block] (swap! (state p) put-block (vec-pos pos) block))
(defn remove-block! [p pos] (swap! (state p) drop-block (vec-pos pos)))
(defn add-entity!
  "Adds an entity (a spec entity: camelCase keys, {:x :y :z} or vector pos) to the fake."
  [p e]
  (swap! (state p) update :entities conj (entity-in e)))
(defn add-item! [p item n] (swap! (state p) give item n))

;; ---- the primitives object

(defn call-record [name token args]
  (let [copy (try (js/structuredClone args) (catch :default _ (js/Object.assign #js {} args)))]
    #js {:name name :token token :args copy}))

(defn create
  "The fake primitives object over the world spec (see the ns docstring)."
  ([] (create {}))
  ([spec]
   (let [state (atom (initial-state spec))
         spec (if (object? spec) (js->clj spec :keywordize-keys true) spec)
         listeners (atom #{})
         calls #js []
         holds (atom {})                ; name -> [hold atom]
         pending (atom #{})             ; {:token :reject}
         overrides (atom {})
         owner (atom nil)
         sleeper (atom nil)             ; the offline call in its wait: {:token :wake}
         away (atom nil)                ; promise of the body being back, while offline
         emit! (fn [event] (doseq [l @listeners] (l event)))
         act! (fn [f] (fn [_token a]
                        (let [[w r] (f @state (args-in a))]
                          (reset! state w)
                          (to-js r))))
         guarded (fn [f] (fn [token a] (try (f token a) (catch ExceptionInfo e (throw (js-error e))))))
         check-owner (fn [token] (when-not (= token @owner) (throw (cut-error))))
         acts {"moveTo" (act! move-to)
               "dig" (act! dig)
               "place" (act! place)
               "jumpPlace" (act! jump-place)
               "collect" (fn [_ a]
                           (let [{:keys [id]} (args-in a)
                                 [w r] (collect @state {:id id})]
                             (reset! state w)
                             (when (= "collected" (:status r))
                               (emit! #js {:kind "picked-up" :item (:name (first (:gained r))) :count (:count (first (:gained r)))}))
                             (to-js r)))
               "toss" (act! toss)
               "craft" (act! craft)
               "chat" (act! chat)
               "inspectContainer" (act! inspect-container)
               "transfer" (act! transfer)
               "equip" (act! equip)
               "eat" (act! #(eat %1 (or %2 {})))
               "attack" (act! attack)
               "sleep" (act! sleep)
               "swim" (act! swim)
               "useOn" (act! use-on/use-on)
               "look" (fn [_ a] (swap! state look-at (args-in a)) #js {:status "ok"})
               "wait" (fn [_ _] (swap! state animals/tempt-follow) #js {:status "ok"})
               "interact" (act! (fn [w a] (animals/interact w (merge {:item nil} a))))
               "trade" (guarded (act! trade/trade))
               "unequip" (act! (fn [w _] (unequip/unequip w)))
               "mount" (act! mount)
               "dismount" (act! dismount)
               "furnace" (guarded (act! furnace/furnace))
               "enchant" (guarded (act! enchant/enchant))
               "steer" (fn [token a]
                         (let [{:keys [timeoutS] :or {timeoutS 60}} (js->clj a :keywordize-keys true)
                               decide (.-decide a)
                               decide' (when (fn? decide)
                                         (fn [pose]
                                           (let [out (decide (to-js (rekey camel pose)))]
                                             (when out
                                               {:controls (some-> (.-controls out) (js->clj :keywordize-keys true))
                                                :yaw (.-yaw out) :done (.-done out) :failed (.-failed out)}))))]
                           (-> (try (steer/steer! {:state state :owner-of #(deref owner) :after-walk after-walk} token
                                                  {:decide decide' :timeout-s timeoutS})
                                    (catch ExceptionInfo e (throw (js-error e))))
                               (.then #(to-js (cond-> % (:pose %) (update :pose (fn [p] (rekey camel p))))) #(throw (if (instance? ExceptionInfo %) (js-error %) %))))))}
         ;; Leaves for a shortened wait, then comes back. A cut ends the wait early; the body is still back (online
         ;; event, isOffline false) before the call resolves 'cut'.
         acts (assoc acts "offline"
                     (fn ^:async offline-act [token a]
                               (let [ms (.-ms a)
                                     wanted (js/Math.floor (min (if (nil? ms) offline-default-ms ms) offline-max-ms))
                                     released (atom nil)]
                                 (reset! away (js/Promise. (fn [resolve] (reset! released resolve))))
                                 (swap! state assoc :offline true)
                                 (emit! #js {:kind "offline" :ms wanted})
                                 (let [full (await (js/Promise.
                                                    (fn [resolve]
                                                      (let [timer (js/setTimeout #(resolve true) (* wanted (:offline-scale @state)))]
                                                        (reset! sleeper {:token token :wake (fn [] (js/clearTimeout timer) (resolve false))})))))]
                                   (reset! sleeper nil)
                                   (swap! state #(cond-> (assoc % :offline false :settling (:settles %))
                                                   full (update :time (fn [t] (mod (+ t (js/Math.floor (/ wanted ms-per-tick))) 24000))))))
                                 (reset! away nil)
                                 (emit! #js {:kind "online" :pos (pos-js (body-pos @state))})
                                 (@released nil)
                                 (if (= token @owner) #js {:status "ok" :ms wanted} #js {:status "cut"}))))
         ;; Wait on a hold if one is armed; resolves to a forced result or undefined.
         wait-hold (fn [name token]
                     (let [armed (first (filter #(not (:taken @%)) (get @holds name)))]
                       (if-not armed
                         (js/Promise.resolve js/undefined)
                         (do (swap! armed assoc :taken true)
                             (js/Promise.
                              (fn [resolve reject]
                                (let [entry {:token token :reject reject}]
                                  (swap! pending conj entry)
                                  (swap! armed assoc :release
                                         (fn [result]
                                           (swap! pending disj entry)
                                           (swap! holds update name (fn [q] (filterv #(not (identical? armed %)) q)))
                                           (resolve result)))
                                  (when (:released @armed) ((:release @armed) (:result @armed))))))))))
         call-async (fn ^:async call-async [name token a]
                         (let [a a]
                           (.push calls (call-record name token a))
                           (check-owner token)
                           (when-some [p @away] (await p))
                           (check-owner token)
                           (let [forced (await (wait-hold name token))]
                             (check-owner token)
                             (cond
                               (not (undefined? forced)) forced
                               (and (#{"moveTo" "steer"} name) (some? (get-in @state [:self :vehicle]))) #js {:status "mounted"}
                               :else
                               (let [_ (when-not (#{"sleep" "wait"} name) (swap! state assoc-in [:self :isSleeping] false))
                                     impl (acts name)
                                     override (@overrides name)]
                                 (let [impl' (fn [t x] (js/Promise. (fn [resolve] (resolve (impl t x)))))]
                                   (await (if override (override token a impl') (impl' token a)))))))))
         call! (fn [name token a] (call-async name token (if (some? a) a #js {})))
         world #js {:state state
                    :calls calls
                    ;; The next call to `name` waits until release(result?) is called or the owner changes.
                    :hold (fn [name]
                            (let [h (atom {:taken false :released false :result nil})]
                              (swap! holds update name (fnil conj []) h)
                              (fn release
                                ([] (release js/undefined))
                                ([result]
                                 (if-let [r (:release @h)]
                                   (r result)
                                   (swap! h assoc :released true :result result))))))
                    :override (fn [name f] (swap! overrides assoc name f))
                    :emit emit!
                    :setTime (fn [t] (swap! state assoc :time t))
                    :advance (fn [ticks]
                               (swap! state #(-> % (assoc :time (mod (+ (:time %) ticks) 24000)) (furnace/advance ticks))))
                    :setRaining (fn [on & [thunder]] (swap! state assoc :raining on :thundering (boolean thunder)))
                    :settle (fn [on] (swap! state assoc :settling on))
                    ;; Respawns where the body stands: emits respawned like the real body and opens a settling window if the spec settles.
                    :respawn (fn []
                               (swap! state #(assoc % :settling (:settles %)))
                               (emit! #js {:kind "respawned" :pos (pos-js (body-pos @state)) :dimension (get-in @state [:self :dimension])}))
                    ;; Dies where the body stands: emits died like the real body, then drops the inventory there as item
                    ;; entities and resets the experience.
                    :die (fn []
                           (let [w @state
                                 {:keys [level points]} (get-in w [:self :experience])]
                             (emit! #js {:kind "died" :pos (pos-js (body-pos w)) :inventory (to-js (with-slots (:inventory w)))
                                         :experience #js {:level level :points points}})
                             (reset! state (-> (reduce #(spawn %1 (body-pos w) (:name %2) (:count %2)) w (:inventory w))
                                               (assoc :inventory [])
                                               (assoc-in [:self :experience] {:level 0 :points 0 :progress 0})))))}
         p #js {}]
     (aset world "state" state)
     (doseq [name (keys acts)] (aset p name (fn [token a] (call! name token a))))
     (doseq [[k f]
             {"setOwner" (fn [token]
                           (when-not (= token @owner) (swap! state assoc :controls {}))
                           (reset! owner token)
                           (when (and @sleeper (not= token (:token @sleeper))) ((:wake @sleeper)))
                           (doseq [entry @pending :when (not= token (:token entry))]
                             (swap! pending disj entry)
                             ((:reject entry) (cut-error))))
              "isOwner" (fn [token] (= token @owner))
              ;; Manual takeover: records controls (booleans) and the look in Minecraft degrees on the state.
              "drive" (fn [token a]
                        (check-owner token)
                        (let [{:keys [controls look]} (args-in (or a #js {}))
                              {:keys [yaw dyaw pitch dpitch]} look]
                          (swap! state (fn [w]
                                         (cond-> (update w :controls merge (or controls {}))
                                           look (assoc :yaw (mod (+ (mod (or yaw (+ (:yaw w) (or dyaw 0))) 360) 360) 360)
                                                       :pitch (min 90 (max -90 (or pitch (+ (:pitch w) (or dpitch 0)))))))))
                          #js {:pos (pos-js (body-pos @state)) :yaw (:yaw @state) :pitch (:pitch @state)}))
              "stopDriving" (fn [] (swap! state assoc :controls {}))
              "isOffline" (fn [] (:offline @state))
              "isSettling" (fn [] (and (not (:offline @state)) (:settling @state)))
              "self" (fn [] (if (:offline @state) #js {:status "offline"} (self-view @state)))
              "entities"
              (fn [a]
                (let [{:keys [radius kind names max] :or {radius 16 max 32}} (js->clj (or a #js {}) :keywordize-keys true)
                      w @state]
                  (if (:offline w)
                    #js []
                    (->> (:entities w)
                         (map #(entity-view w %))
                         (filter #(and (<= (.-distance %) radius) (or (nil? kind) (= kind (.-kind %)))
                                       (or (nil? names) (some #{(.-name %)} names))))
                         (sort-by #(.-distance %))
                         (take max)
                         to-array))))
              "blocks"
              (fn [a]
                (let [w @state
                      {:keys [radius names max properties] :or {radius 16 max 64}} (js->clj (or a #js {}) :keywordize-keys true)
                      match (when a (.-match a))
                      ok? (cond names (set names) match #(match %) :else (constantly true))]
                  (if (:offline w)
                    #js []
                    (->> (:blocks w)
                         (map (fn [[pos block]] {:name block :pos pos :distance (dist (body-pos w) pos)}))
                         (filter #(and (<= (:distance %) radius) (ok? (:name %))))
                         (sort-by (juxt :distance #(get (:rank w) (:pos %) js/Infinity) :pos))
                         (take max)
                         (map (fn [b]
                                (let [pos (:pos b)
                                      props (properties-of w pos)]
                                  (to-js (merge b (when (contains? (:ages w) pos) {:age (get-in w [:ages pos])})
                                                (when (and properties (seq props)) {:properties props}))))))
                         to-array))))
              "digTime" (fn [_pos _item] (or (:dig-ms @state) 0))
              "harvestTools"
              (fn [block]
                (let [data (minecraft-data "26.1")
                      ids (some-> (aget (.-blocksByName data) block) .-harvestTools js/Object.keys)]
                  (when ids (to-array (map #(.-name (aget (.-items data) %)) ids)))))
              "blockAt"
              (fn [a]
                (let [w @state pos (vec-pos (args-in a))
                      block (block-name w pos)
                      props (properties-of w pos)]
                  (when-not (or (:offline w) ((:unloaded w) pos))
                    (to-js (merge {:name block :pos pos}
                                  (when (contains? (:ages w) pos) {:age (get-in w [:ages pos])})
                                  (when (seq props) {:properties props})
                                  (when-not (re-find not-full-cube block) {:fullCube true}))))))
              "pathWorld" (fn [] (let [{:keys [snapshot table space]} (steer/path-world @state)]
                                   #js {:snapshot snapshot :table table :space space}))
              "onBodyEvent" (fn [listener] (swap! listeners conj listener) (fn [] (swap! listeners disj listener)))
              "close" (fn [])}]
       (aset p k f))
     (aset p "world" world)
     p)))
