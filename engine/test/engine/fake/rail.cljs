(ns engine.fake.rail
  "Rails in the fake world, after the game's connection rule (BaseRailBlock / RailState), as pure functions over world
  data {:blocks {[x y z] name} :states {[x y z] props} :self {:pos [x y z]}}.

  A rail takes its shape when it is placed (rails-placed): from the rails beside it, at its own height or one up or
  down, that still have a free end (a rail with two live connections is full), and the rails it joins take theirs
  again; a rail whose neighbour is broken keeps its shape. The shape is kept in the cell's state, so out-of-order
  placing shows as the wrong shapes the game gives. A rail that was never placed (written into a test's world) reads
  as if it had been placed in line order. The first shape of a lone rail follows where the body stands (measured:
  along x when it stands out along x).

  A powered rail is lit when a source touches it (a redstone block, a redstone torch, a lever switched on; below
  counts) or touches one of the powered rails up to 8 further along its own unbroken run of powered rails, slopes
  included (measured live). Not modelled: detector and activator rails, the redstone signal a corner rail may take.
  Test-only; ported from the deleted js/fake-rail.mjs.")

(def reach 8)
(def sides [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0] [0 -1 0]])
(def north [0 0 -1])
(def south [0 0 1])
(def west [-1 0 0])
(def east [1 0 0])
(def up [0 1 0])

(def connections
  {"north_south" [north south]
   "east_west" [east west]
   "ascending_east" [west [1 1 0]]
   "ascending_west" [[-1 1 0] east]
   "ascending_north" [[0 1 -1] south]
   "ascending_south" [north [0 1 1]]
   "south_east" [east south]
   "south_west" [west south]
   "north_west" [west north]
   "north_east" [north east]})

(defn rail? [name] (boolean (re-find #"(^|_)rail$" name)))

(defn last-match
  "The value of the last [test value] pair whose test holds (the game assigns the shape in sequence), else nil."
  [& pairs]
  (reduce (fn [found [test value]] (if test value found)) nil pairs))

(defn plus [pos d] (mapv + pos d))
(defn same-column? [[ax _ az] [bx _ bz]] (and (= ax bx) (= az bz)))
(defn name-at [w pos] (get-in w [:blocks pos] "air"))
(defn rail-at? [w pos] (rail? (name-at w pos)))
(defn straight? [w pos] (not= "rail" (name-at w pos)))

(defn rail-at
  "getRail: the rail at pos, else the one on top of it, else the one under it."
  [w pos]
  (->> [[0 0 0] up [0 -1 0]]
       (map #(plus pos %))
       (filter #(rail-at? w %))
       first))

(defn climbing
  "A flat straight shape turns to climb when the rail one up lies ahead."
  [w pos shape]
  (let [rail-up? #(rail-at? w (plus (plus pos %) up))]
    (case shape
      "north_south" (cond (rail-up? south) "ascending_south" (rail-up? north) "ascending_north" :else shape)
      "east_west" (cond (rail-up? west) "ascending_west" (rail-up? east) "ascending_east" :else shape)
      shape)))

(defn choose-shape
  "The shape a rail placed now at pos takes among the rails beside it. can-connect? says whether a neighbouring rail
  has a free end for it. Straight rails (powered ones) take no corner."
  [w pos initial straight can-connect?]
  (let [joins? (fn [d] (let [r (rail-at w (plus pos d))] (and (some? r) (can-connect? r))))
        [n so we e] (map joins? [north south west east])
        ns (or n so)
        ew (or we e)
        [se sw ne nw] [(and so e) (and so we) (and n e) (and n we)]
        shape (last-match
                [(and ns (not ew)) "north_south"]
                [(and ew (not ns)) "east_west"]
                [(and (not straight) se (not n) (not we)) "south_east"]
                [(and (not straight) sw (not n) (not e)) "south_west"]
                [(and (not straight) nw (not so) (not e)) "north_west"]
                [(and (not straight) ne (not so) (not we)) "north_east"])
        shape (if (some? shape)
                shape
                (last-match
                  [(and ns ew) initial]
                  [(and (not straight) nw) "north_west"]
                  [(and (not straight) ne) "north_east"]
                  [(and (not straight) sw) "south_west"]
                  [(and (not straight) se) "south_east"]))]
    (or (climbing w pos shape) initial)))

(defn shape-of [w pos]
  (or (get-in w [:states pos :shape])
      (choose-shape w pos "north_south" (straight? w pos) (constantly true))))

(defn connections-of [w pos]
  (mapv #(plus pos %) (connections (shape-of w pos))))

(defn live-connections
  "The live connections of the rail at pos: those whose rail connects back."
  [w pos]
  (filterv (fn [c]
             (let [other (rail-at w c)]
               (and (some? other) (some #(same-column? % pos) (connections-of w other)))))
           (connections-of w pos)))

(defn can-connect-to?
  "A rail can take one more connection from `from` when it already holds one to it or has a free end."
  [w rail from]
  (let [live (live-connections w rail)]
    (or (some #(same-column? % from) live) (not= 2 (count live)))))

(defn connect-to
  "The rail at pos takes the connection to `from` besides its live ones."
  [w rail from]
  (let [held (conj (live-connections w rail) from)
        has? (fn [d] (boolean (some #(same-column? % (plus rail d)) held)))
        [n so we e] (map has? [north south west east])
        corner? (not (straight? w rail))
        shape (last-match
                [(or n so) "north_south"]
                [(or we e) "east_west"]
                [(and corner? so e (not n) (not we)) "south_east"]
                [(and corner? so we (not n) (not e)) "south_west"]
                [(and corner? n we (not so) (not e)) "north_west"]
                [(and corner? n e (not so) (not we)) "north_east"])]
    (assoc-in w [:states rail :shape] (or (climbing w rail shape) "north_south"))))

(defn first-shape
  "Where the body stands decides the shape of a rail with no neighbour."
  [w [px _ pz]]
  (let [[sx _ sz] (get-in w [:self :pos])]
    (if (> (abs (- px sx)) (abs (- pz sz))) "east_west" "north_south")))

(defn rail-placed [w pos]
  (let [initial (first-shape w pos)
        w (assoc-in w [:states pos :shape] initial)
        shape (choose-shape w pos initial (straight? w pos) #(can-connect-to? w % pos))
        w (assoc-in w [:states pos :shape] shape)]
    (reduce (fn [w c]
              (let [rail (rail-at w c)]
                (if (and (some? rail) (can-connect-to? w rail pos)) (connect-to w rail pos) w)))
            w
            (connections-of w pos))))

(defn rails-placed
  "World after the blocks a place just set ({:name :pos [x y z]}): every rail among them settles and joins its
  neighbours."
  [w blocks]
  (reduce (fn [w b] (if (rail? (:name b)) (rail-placed w (:pos b)) w)) w blocks))

(defn power-source? [w pos]
  (let [name (name-at w pos)]
    (or (= "redstone_block" name)
        (boolean (re-find #"^redstone(_wall)?_torch$" name))
        (and (= "lever" name) (true? (get-in w [:states pos :powered]))))))

(defn rail-lit? [w pos]
  (let [touched? (fn [p] (some #(power-source? w (plus p %)) sides))
        ;; walks the run from pos through its connection c, up to reach rails
        along? (fn [c]
                 (loop [prev pos, rail (rail-at w c), i 0]
                   (cond
                     (or (>= i reach) (nil? rail) (not= "powered_rail" (name-at w rail))) false
                     (touched? rail) true
                     :else (let [nxt (first (remove #(same-column? % prev) (connections-of w rail)))]
                             (recur rail (when nxt (rail-at w nxt)) (inc i))))))]
    (boolean (or (touched? pos) (some along? (connections-of w pos))))))

(defn rail-properties
  "The worked-out properties of the block at pos in world w ({} for anything but a rail)."
  [w pos]
  (let [name (name-at w pos)]
    (cond
      (not (rail? name)) {}
      (= "powered_rail" name) {:shape (shape-of w pos) :powered (rail-lit? w pos)}
      :else {:shape (shape-of w pos)})))
