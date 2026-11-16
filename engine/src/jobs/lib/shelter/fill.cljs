(ns jobs.lib.shelter.fill
  "What a shelter reads of the cells round the feet without placing anything: which stop a mob (sealed?), which a mob stands
  on, and the cells still to fill (fill-cells, open-cells). Placing them is jobs.survival.dig-in-cells."
  (:require [jobs.lib.block-sets :as bs]
            [jobs.lib.click :as click]
            [jobs.lib.danger :as danger]
            [jobs.lib.found :as found]
            [jobs.lib.land :as land]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(def sides [[1 0] [-1 0] [0 1] [0 -1]])

(def corners "The diagonal neighbours: a mob standing there still reaches the body." [[1 1] [1 -1] [-1 1] [-1 -1]])

(def mob-proof-shapes
  "Blocks that stop a mob although their collision shape does not fill the cell: fences (1.5 high), walls, panes,
  iron bars, gates, doors and trapdoors."
  #"(_fence|_fence_gate|_wall|_pane|_door|_trapdoor)$|^iron_bars$")

(def loose
  "Things a mob or bolt passes that a placement does not overwrite: dug out of a cell before it is filled. Crops have no
  collision, so a field cell holding one is open, not a fixture."
  #"(^|_)(rail|torch|fence|fence_gate|ladder|lantern|button|lever|carpet|sign|banner|pressure_plate|sapling)$|^cobweb$|^(wheat|carrots|potatoes|beetroots|nether_wart|(attached_)?(melon|pumpkin)_stem)$")

(defn kind
  "What a block name means for a fill: :open (air, water or replaceable), :container (never touched), :loose (dug first with
  :clear-loose) or :solid (done)."
  [n]
  (cond
    (or (bs/air n) (= "water" n) (bs/replaceable n)) :open
    (found/containers n) :container
    (or (bs/clearable n) (and n (re-find loose n))) :loose
    :else :solid))

(defn open-openable
  "The block at cell when it is a door, gate or trapdoor (wooden, copper or iron) standing open, else nil."
  [p cell]
  (let [b (u/seen-block p cell)]
    (when (and b (#{:openable :iron} (click/kind-of (.-name b))) (click/reached? :open (click/props-of b)))
      b)))

(defn sealed?
  "Whether a cell already stops a mob: its block fills the cell (blockAt's :fullCube: stone, leaves, glass) or is a
  fence, wall, pane, bar, or a gate, door or trapdoor that is shut (one standing open is a way in). Signs, rails,
  plates, buttons, levers, carpets, torches, plants, slabs and stairs let a mob walk or step through (or leave a gap
  it fits through), so they are not sealed; nor is an unloaded cell."
  [p cell]
  (let [b (u/seen-block p cell)]
    (boolean (and b
                  (not (open-openable p cell))
                  (or (.-fullCube b) (re-find mob-proof-shapes (.-name b)))))))

(defn stand-on?
  "Whether a mob can stand on the top of the block at cell: a full cube or any other floor (slab, stair, farmland, path,
  soul sand, chest, carpet; jobs.lib.land/floor?), not a fence, wall, pane or bars (too tall to stand on), a block with no
  collision (torch, sign, button: a mob falls through it) or a cell not loaded."
  [p cell]
  (let [b (u/seen-block p cell)]
    (boolean (and b
                  (not (re-find mob-proof-shapes (.-name b)))
                  (or (re-find #"_carpet$" (.-name b)) (not (bs/passable? (.-name b))))
                  (or (.-fullCube b) (land/floor? (.-name b)))))))

(defn fill-cells
  "The cells to fill around the feet cell, in placement order: sides at feet
  height, sides at head height, a support beside the roof cell (a block needs a
  solid face neighbour to be placed against, and the roof cell has none until
  the support exists; none when one of the four is placed already or the roof cell is sealed; else the one farthest from
  threats ({:x :y :z}), the +x one with none), then the roof cell above the head; only those the
  predicate sealed? (cell -> bool) does not hold for. A corner cell (diagonal, feet or head height) is among them only
  when a mob could stand in it: an open one on a floor it stands on (stand-on?, cell -> bool), or above a corner
  that is one; the corners follow the sides."
  [sealed? {:keys [x y z]} threats stand-on?]
   (let [roof {:x x :y (+ y 2) :z z}
         supports (for [[dx dz] sides] {:x (+ x dx) :y (+ y 2) :z (+ z dz)})
         gap (fn [{cx :x cy :y cz :z}]
               (apply min (map #(u/dist {:x (+ cx 0.5) :y (+ cy 0.5) :z (+ cz 0.5)} %) threats)))
         support (if (empty? threats) (first supports) (apply max-key gap supports))
         at (fn [dy [dx dz]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})
         corner-cells (fn [corner]
                        (let [feet-c (at 0 corner)
                              feet-stand? (and (not (sealed? feet-c)) (stand-on? (at -1 corner)))]
                          (cond-> []
                            feet-stand? (conj feet-c)
                            (or feet-stand? (stand-on? feet-c)) (conj (at 1 corner)))))]
     (filterv #(not (sealed? %))
              (concat (for [dy [0 1] d sides] (at dy d))
                      (mapcat corner-cells corners)
                      (when-not (or (sealed? roof) (some sealed? supports)) [support])
                      [roof]))))

(defn fixture?
  "Whether a block no fill places into or clears stands in the cell: a bed, container or workstation (kind :container,
  or :solid not sealed?, not an open door, gate or trapdoor: the fill shuts it). The cell over it is filled instead."
  [p cell]
  (let [n (u/seen-name p cell)]
    (boolean (and n (#{:container :solid} (kind n)) (not (sealed? p cell)) (not (open-openable p cell))))))

(defn shut-rooms
  "The rooms (reach-room/room*'s answers) room-columns takes its columns from, but for the hostiles in them; kept per body
  while the world and its memory stay the same (reach-room/kept): a shelter's quote and its checks ask it several times.
  None, and no flood, when neither the feet cell nor a column beside it is roofed: every room taken holds one of those cells
  (its flood's start) and is roofed over every cell."
  [p {:keys [x y z] :as feet}]
  (reach-room/kept
   p (str "shut-rooms|" x "|" y "|" z)
   (fn [_]
     (if (and (not (sh-sense/room-roofed? p feet))
              (not-any? (fn [[dx dz]] (sh-sense/room-roofed? p {:x (+ x dx) :y y :z (+ z dz)})) (concat sides corners)))
       []
       (let [base (reach-room/shut-kind-at p)
             kind-at (reach-room/room-kind-at p (fn [cx cy cz] (if (and (== cx x) (== cz z) (<= y cy (inc y))) :solid (base cx cy cz))))
             roofed? (fn [{:keys [cells]}]
                       (every? (fn [[cx cy cz]] (let [cell {:x cx :y cy :z cz}]
                                                  (and (sh-sense/room-roofed? p cell) (<= (u/dist feet cell) sh-sense/spawn-free-radius))))
                               cells))]
         (if (reach-room/kept-flood p x y z false)
           (let [room (reach-room/room p feet)
                 beside? (fn [{cx :x cy :y cz :z}] (and (== y cy) (== 1 (+ (js/Math.abs (- cx x)) (js/Math.abs (- cz z))))))]
             (if (and room (seq (:doors room)) (roofed? room) (every? #(or (nil? (open-openable p (:cell %))) (beside? (:cell %))) (:doors room)))
               [room]
               []))
           (first
            (reduce (fn [[rooms found :as acc] [dx dz]]
                      (let [col [(+ x dx) (+ z dz)]]
                        (if (or (contains? found col) (not (sh-sense/room-roofed? p {:x (first col) :y y :z (second col)})))
                          acc
                          (let [room (reach-room/room* p {:x (first col) :y y :z (second col)} kind-at)]
                            (if (and room (contains? (:cells room) [(first col) y (second col)]) (roofed? room))
                              [(conj rooms room) (into found (map (fn [[cx _ cz]] [cx cz])) (:cells room))]
                              acc)))))
                    [[] #{}]
                    (concat sides corners)))))))))

(defn room-columns
  "The [x z] columns beside the feet whose feet-height cell lies in a closed room (reach-room/room*, the
  body's own feet and head cells counted solid) roofed over every cell (sh-sense/room-roofed?), within sh-sense/spawn-free-radius of
  the feet and with no hostile the body knows of in it (danger/seen-hostiles), while the body's own cells open it (no
  closed room round the feet): the body stands in its wall or doorway, and that side is the room's, a way no mob comes.
  A body inside such a room with a door (closed round the feet; a pit has none) whose every open door is beside its feet: all the room's columns;
  the shelter shuts the door (an open door is a cell to fill, sealed?) rather than fill the room. The rooms: shut-rooms."
  [p feet]
  (let [mobs (delay (set (map #(let [{:keys [x y z]} (danger/mob-pos p %)] [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)])
                              (danger/seen-hostiles p))))]
    (into #{} (comp (remove (fn [{:keys [cells]}] (some @mobs cells))) (mapcat :cells) (map (fn [[cx _ cz]] [cx cz])))
          (shut-rooms p feet))))

(defn open-cells
  "The fill-cells around the feet cell that are not sealed? in the world; the roof support away from threats. A
  fixture? cell, and the cells of a room-columns column, are no fill target."
  ([p feet] (open-cells p feet nil))
  ([p feet threats]
   (let [rooms (delay (room-columns p feet))]
     (filterv #(not (or (fixture? p %) (contains? @rooms [(:x %) (:z %)])))
              (fill-cells #(sealed? p %) feet threats #(stand-on? p %))))))
