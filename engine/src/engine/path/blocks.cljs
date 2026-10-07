(ns engine.path.blocks
  "What the planner needs to know about a block state, as flat typed arrays indexed by state id (a JS object whose fields the
  search reads in its inner loop with no allocation): built once from prismarine-registry by interop. Heights are in 1/16 block
  above the cell's floor."
  (:require ["prismarine-block" :as prismarine-block]
            ["prismarine-registry" :as prismarine-registry]
            [engine.path.offsets :as offsets]))

(def ^:const OPEN 0) ;; no collision, not fluid, not hazard
(def ^:const SOLID 1) ;; has collision
(def ^:const WATER 2)
(def ^:const LAVA 3)
(def ^:const CLIMB 4) ;; ladder, vines, scaffolding: the body climbs while its feet are inside
(def ^:const OPENABLE 5) ;; doors, gates, trapdoors: walked as their current collision
(def ^:const NARROW 6) ;; collision that does not cover the cell (posts, panes, bamboo): a walk may not pass through

(def ^:const HAZARD-NONE 0)
(def ^:const HAZARD-AVOID 1) ;; forbidden by default
(def ^:const DAMAGE-STAND 2) ;; hurts when stood on
(def ^:const DAMAGE-TOUCH 3) ;; hurts when walked into
(def ^:const SLOW 4) ;; slows walking
(def ^:const PORTAL 5) ;; nether portal, end portal, end gateway: stepping in sends the body elsewhere

;; the planner's block table is built for this version; a body on another version would plan on the wrong states
(def MC-VERSION "26.1")

;; `climb` per state: 1 the body climbs inside it, 2 an open trapdoor (vanilla counts it as ladder when it sits directly above a
;; ladder of the same facing), 3 a closed trapdoor a hand can open (iron ones are 0). `climbName` says which climbable it is.
(def ^:const CLIMB-NONE 0)
(def ^:const CLIMB-INSIDE 1)
(def ^:const CLIMB-TRAP-OPEN 2)
(def ^:const CLIMB-TRAP-SHUT 3)
;; `openable` per state: 1 a closed door, gate or trapdoor a hand opens (wood, copper), 2 one only redstone opens (iron), 0 anything
;; else (an open one is plain geometry); `openState` is the same block with open=true; `openKind` says which of the three it is.
;; `activator`: what a body can use to open an iron door.
(def ^:const OPEN-HAND 1)
(def ^:const OPEN-REDSTONE 2)
(def ^:const KIND-DOOR 1)
(def ^:const KIND-GATE 2)
(def ^:const KIND-TRAPDOOR 3)
(def ^:const ACT-BUTTON 1)
(def ^:const ACT-LEVER 2)
(def ^:const ACT-PLATE 3)
(def ^:const LADDER 1)
(def ^:const VINES 2)
(def ^:const SCAFFOLDING 3)

(def ^:private WATER-NAMES #{"water" "bubble_column" "kelp" "kelp_plant" "seagrass" "tall_seagrass"})
(def ^:private CLIMB-NAMES #"^(ladder|vine|scaffolding|(weeping|twisting)_vines(_plant)?|cave_vines(_plant)?)$")
;; iron doors and trapdoors do not open by hand; copper ones do, so they count
(def ^:private OPENABLE-NAMES #"^(?!iron_)\w+_(door|fence_gate|trapdoor)$")
(def ^:private TRAPDOOR #"_trapdoor$")
(def ^:private DOOR #"_door$")
(def ^:private GATE #"_fence_gate$")
(def ^:private IRON-OPENABLE #"^iron_(door|trapdoor)$")
(def ^:private BUTTON #"(^|_)button$")
(def ^:private PLATE #"_pressure_plate$")
(def ^:private NARROW-NAMES #"^bamboo$|_pane$|_bars$|(^|_)fence$|_wall$|(^|_)chain$|^end_rod$|lightning_rod$")
(def ^:private AVOID-NAMES #{"lava" "fire" "soul_fire" "powder_snow" "cobweb"})
(def ^:private TOUCH-NAMES #{"sweet_berry_bush" "wither_rose" "cactus"})
(def ^:private SLOW-NAMES #{"soul_sand" "honey_block"})
(def ^:private PORTAL-NAMES #{"nether_portal" "end_portal" "end_gateway"})
(def ^:private LIT-CAMPFIRES #{"campfire" "soul_campfire"})
(def ^:private STAIRS #"_stairs$")

;; the direction code a body walks to climb a bottom straight stairs block, as the planner numbers its cardinal moves:
;; the high back is on the facing side (facing=north has its tall half at z 0..0.5), so you enter from the opposite side.
;; Corner shapes stay 0: their tall part blocks a centred entry.
(def ^:private STAIR-UP {"east" 1 "west" 2 "south" 3 "north" 4})
(def ^:private FACING {"east" 1 "west" 2 "south" 3 "north" 4})
;; a wall button on a block's east face has facing=east and sits on the block to its west
(def ^:private ATTACH-OPPOSITE {"east" 2 "west" 1 "south" 4 "north" 3})

;; the data's bamboo box is wrong (0.156..0.344); vanilla is Block.box(6.5, 0, 6.5, 9.5, 16, 9.5), offset per position at lookup
(def ^:private BAMBOO-BOX #js [0.40625 0 0.40625 0.59375 1 0.59375])
(def ^:private WHOLE 16)
(def ^:private FOOTPRINT 16) ;; sample cells per axis for the coverage test; every vanilla shape edge is a multiple of 1/16

(defn- test? [re name] (.test ^js re name))

(defn- prop [props k] (aget props k))

(defn- climb-of [name props]
  (cond
    (test? CLIMB-NAMES name) CLIMB-INSIDE
    (not (test? TRAPDOOR name)) CLIMB-NONE
    (true? (prop props "open")) CLIMB-TRAP-OPEN
    (test? OPENABLE-NAMES name) CLIMB-TRAP-SHUT
    :else CLIMB-NONE))

(defn- climb-name-of [name]
  (cond
    (= name "ladder") LADDER
    (= name "scaffolding") SCAFFOLDING
    (test? CLIMB-NAMES name) VINES
    :else 0))

(defn- leaves-gaps?
  "true when the boxes' xz projections leave part of the 1x1 footprint uncovered (cell centres sampled at 1/16)"
  [shapes]
  (boolean
   (some (fn [k]
           (let [x (/ (+ (quot k FOOTPRINT) 0.5) FOOTPRINT)
                 z (/ (+ (rem k FOOTPRINT) 0.5) FOOTPRINT)]
             (not-any? (fn [b] (and (> x (aget b 0)) (< x (aget b 3)) (> z (aget b 2)) (< z (aget b 5)))) shapes)))
         (range (* FOOTPRINT FOOTPRINT)))))

(defn- sixteenths [v] (js/Math.round (* v 16)))

(defn- kind-of [name props top]
  (cond
    (or (contains? WATER-NAMES name) (and (true? (prop props "waterlogged")) (zero? top))) WATER
    (= name "lava") LAVA
    (test? CLIMB-NAMES name) CLIMB
    (test? OPENABLE-NAMES name) OPENABLE
    (and (pos? top) (test? NARROW-NAMES name)) NARROW
    (pos? top) SOLID
    :else OPEN))

(defn- hazard-of [name props]
  (cond
    (contains? AVOID-NAMES name) HAZARD-AVOID
    (or (= name "magma_block") (and (contains? LIT-CAMPFIRES name) (true? (prop props "lit")))) DAMAGE-STAND
    (contains? TOUCH-NAMES name) DAMAGE-TOUCH
    (contains? SLOW-NAMES name) SLOW
    (contains? PORTAL-NAMES name) PORTAL
    :else HAZARD-NONE))

(defn- attach-of [props]
  (case (prop props "face")
    "floor" 6
    "ceiling" 5
    (get ATTACH-OPPOSITE (prop props "facing") 0)))

(defn build-state-table
  "The planner's block table of a prismarine registry: typed arrays indexed by state id (top, base, kind, hazard, stairUp, climb,
  climbName, facing, openable, openState, openKind, doorHalf, activator, attach, floor, special, flowing, bubble, magma,
  dripleaf, farmland, boxStart, boxCount, boxes, offsetMax, partial, nameIds), as a JS object. nameIds is a Map of block name to
  its [first last] state id (see state-ids)."
  [registry]
  (let [Block (prismarine-block registry)
        offset-max (.-OFFSET_MAX ^js (offsets/offsets))
        blocks (.-blocksArray ^js registry)
        size (inc (reduce (fn [m b] (max m (.-maxStateId ^js b))) 0 blocks))
        top (js/Uint8Array. size)
        base (.fill (js/Uint8Array. size) 16)
        kind (js/Uint8Array. size)
        hazard (js/Uint8Array. size)
        stair-up (js/Uint8Array. size)
        box-start (js/Uint32Array. size)
        box-count (js/Uint8Array. size)
        offset-maxes (js/Float32Array. size)
        partial (js/Uint8Array. size)
        climb (js/Uint8Array. size)
        climb-name (js/Uint8Array. size)
        facing (js/Uint8Array. size) ;; ladder and trapdoor facing: 1 east, 2 west, 3 south, 4 north
        special (js/Uint8Array. size) ;; partial collision or climbable: the states a search must look closer at near a cell
        floor (js/Uint8Array. size) ;; height a body can stand on, 1/16 of the cell: the top, but none for a ladder
        flowing (js/Uint8Array. size) ;; water that is not a source: it pushes the body
        bubble (js/Uint8Array. size) ;; bubble column: 1 lifts (drag=false, over soul sand), 2 drags down (drag=true, over magma)
        magma (js/Uint8Array. size) ;; a magma block: a body must not end its route on it
        dripleaf (js/Uint8Array. size) ;; a big dripleaf leaf with collision: a floor that tilts under a body
        farmland (js/Uint8Array. size) ;; farmland: a body landing on it from a jump or a fall tramples it to dirt
        openable (js/Uint8Array. size)
        open-state (js/Uint32Array. size)
        open-kind (js/Uint8Array. size)
        door-half (js/Uint8Array. size) ;; 1 lower half of a door, 2 upper half
        activator (js/Uint8Array. size)
        attach (js/Uint8Array. size) ;; where a button's or lever's supporting block lies from it: 1 +x, 2 -x, 3 +z, 4 -z, 5 +y, 6 -y
        name-ids (js/Map.)
        floats #js []]
    (doseq [^js block blocks]
      (.set name-ids (.-name block) #js [(.-minStateId block) (.-maxStateId block)]))
    (doseq [block blocks
            id (range (.-minStateId ^js block) (inc (.-maxStateId ^js block)))]
      (let [state (.fromStateId ^js Block id 0)
            name (.-name ^js block)
            ;; a body inside scaffolding meets nothing: its collision is only for a body standing on top, so it is a floor, not a wall
            shapes (cond (= name "bamboo") #js [BAMBOO-BOX]
                         (= name "scaffolding") #js []
                         :else (or (.-shapes ^js state) #js []))
            props (.getProperties ^js state)
            n-shapes (.-length shapes)
            door-name? (or (test? OPENABLE-NAMES name) (test? IRON-OPENABLE name))]
        ;; the data's shape for snow layers matches the server: (layers - 1) * 2 / 16, so no correction is needed
        (aset top id (reduce (fn [m s] (max m (sixteenths (aget s 4)))) 0 shapes))
        (aset base id (reduce (fn [m s] (min m (sixteenths (aget s 1)))) 16 shapes))
        (aset kind id (kind-of name props (aget top id)))
        (aset hazard id (hazard-of name props))
        (aset box-start id (/ (.-length floats) 6))
        (aset box-count id n-shapes)
        (run! (fn [s] (dotimes [k 6] (.push floats (aget s k)))) shapes)
        (aset offset-maxes id (or (aget offset-max name) 0))
        (aset partial id (if (and (pos? n-shapes) (leaves-gaps? shapes)) 1 0))
        (aset climb id (climb-of name props))
        (aset climb-name id (climb-name-of name))
        (aset facing id (if (or (= (aget climb-name id) LADDER) (test? TRAPDOOR name) (test? DOOR name) (test? GATE name))
                          (get FACING (prop props "facing") 0)
                          0))
        (when door-name?
          (aset open-kind id (cond (test? TRAPDOOR name) KIND-TRAPDOOR (test? GATE name) KIND-GATE :else KIND-DOOR))
          (aset door-half id (cond (not= (aget open-kind id) KIND-DOOR) 0 (= (prop props "half") "upper") 2 :else 1))
          (when (false? (prop props "open"))
            (aset openable id (if (test? IRON-OPENABLE name) OPEN-REDSTONE OPEN-HAND))
            (aset open-state id (.-stateId ^js (.fromProperties ^js Block name (js/Object.assign #js {} props #js {:open true}) 0)))))
        (aset activator id (cond (test? BUTTON name) ACT-BUTTON (= name "lever") ACT-LEVER (test? PLATE name) ACT-PLATE :else 0))
        (when (or (= (aget activator id) ACT-BUTTON) (= (aget activator id) ACT-LEVER))
          (aset attach id (attach-of props)))
        (aset floor id (cond (= name "scaffolding") WHOLE (zero? (aget climb-name id)) (aget top id) :else 0))
        (aset flowing id (if (and (= name "water") (not= (js/Number (prop props "level")) 0)) 1 0))
        (aset bubble id (if (= name "bubble_column") (if (true? (prop props "drag")) 2 1) 0))
        (aset magma id (if (= name "magma_block") 1 0))
        (aset farmland id (if (= name "farmland") 1 0))
        ;; (a magma bubble column is special too: the cells beside it cost risk, which a search only looks for where one is near)
        (aset special id (bit-or (aget partial id)
                                 (if (= (aget climb id) CLIMB-NONE) 0 1)
                                 (if (= (aget bubble id) 2) 1 0)
                                 (if (pos? (aget openable id)) 1 0)))
        (when (and (= name "big_dripleaf") (pos? (aget top id)))
          ;; the leaf's box is 11..15/16 (or lower tilted): a body stands on its top, so for the planner it is a low block like a carpet
          (aset dripleaf id 1)
          (aset base id 0))
        (when (and (test? STAIRS name) (= (prop props "half") "bottom") (= (prop props "shape") "straight"))
          (aset stair-up id (get STAIR-UP (prop props "facing") 0)))))
    #js {:top top :base base :kind kind :hazard hazard :stairUp stair-up :climb climb :climbName climb-name :facing facing
         :openable openable :openState open-state :openKind open-kind :doorHalf door-half :activator activator :attach attach
         :floor floor :special special :flowing flowing :bubble bubble :magma magma :dripleaf dripleaf :farmland farmland
         :boxStart box-start :boxCount box-count :boxes (js/Float32Array.from floats) :offsetMax offset-maxes :partial partial :nameIds name-ids}))

(defn state-ids
  "Every state id of the block called name in table (build-state-table), nil when there is no such block."
  [^js table name]
  (when-some [^js r (.get (.-nameIds table) name)]
    (range (aget r 0) (inc (aget r 1)))))

(defonce ^:private shared (volatile! nil))

(defn default-state-table
  "One table per process: building walks every state in the registry."
  []
  (or @shared (vreset! shared (build-state-table (prismarine-registry MC-VERSION)))))
