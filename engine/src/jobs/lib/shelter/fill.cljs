(ns jobs.lib.shelter.fill
  "What a shelter reads of the cells round the feet without placing anything: which stop a mob (sealed?), which a mob stands
  on, and the cells still to fill (fill-cells, open-cells). Placing them is jobs.survival.dig-in-cells."
  (:require [jobs.lib.click :as click]
            [jobs.lib.land :as land]
            [jobs.lib.util :as u]))

(def sides [[1 0] [-1 0] [0 1] [0 -1]])

(def corners "The diagonal neighbours: a mob standing there still reaches the body." [[1 1] [1 -1] [-1 1] [-1 -1]])

(def mob-proof-shapes
  "Blocks that stop a mob although their collision shape does not fill the cell: fences (1.5 high), walls, panes,
  iron bars, gates, doors and trapdoors."
  #"(_fence|_fence_gate|_wall|_pane|_door|_trapdoor)$|^iron_bars$")

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
  soul sand, chest, carpet; jobs.lib.land/floor?), not a fence, wall, pane or bars (too tall to stand on) or a cell not loaded."
  [p cell]
  (let [b (u/seen-block p cell)]
    (boolean (and b
                  (not (re-find mob-proof-shapes (.-name b)))
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

(defn open-cells
  "The fill-cells around the feet cell that are not sealed? in the world; the roof support away from threats."
  ([p feet] (open-cells p feet nil))
  ([p feet threats] (fill-cells #(sealed? p %) feet threats #(stand-on? p %))))
