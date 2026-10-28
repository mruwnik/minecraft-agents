(ns jobs.lib.cost.find
  "The chance a look round finds what a search is for, and a search's seconds per find. Animals: a mixture prior
  (base-rates/animal-ground-share of the land spawns them, at base-rates/animal-pack-density, the rest none) lowered by
  the ground within ::region that looks for them found none on (live :searched notes, engine.notes), so a search over
  empty ground soon stops paying. Ore: the game's vein placement (base-rates) over the faces a look sees of the rock the
  veins replace, at each face's height, and over the rock a tunnel cuts. Pure."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.cost.base-rates :as base-rates]))

(a/defargs settings
  {::region {:default 128 :spec (a/num-in 1 nil)
             :doc "Blocks round a spot whose searched ground tells the chance of a find there: the game sets animals by biome, and a biome spans a few hundred blocks."}
   ::leg-spacing {:default 16 :doc "Blocks between a search's legs (jobs.explore.search)." :spec (a/int-in 4 64)}
   ::vein-seen {:default 4.16 :doc "Ore blocks a look sees of each vein it sees (the game places a vein as one clump): 104 blocks in 25 groups seen together in the look tool's lists of the trial logs (all 7 lists cut short at their nearest blocks, so far groups may be undercounted), coal and iron pooled. Recount: node tools/event-counts.mjs vein-seen." :spec (a/num-in 1 nil)}})

(defn leg-spacing [] (settings/get settings ::leg-spacing))

(def cell "Blocks on a side of the squares searched ground is counted in." 8)

(defn density
  "Packs per block of the kinds of names where they spawn, nil when one has no base rate (a block: no chance is priced)."
  [names]
  (when (and (seq names) (every? base-rates/animal-pack-density names))
    (reduce + (map base-rates/animal-pack-density names))))

(defn searched-for? [names {:keys [kind what]}]
  (and (= :searched kind) (every? (set what) names)))

(defn in-vain?
  "Whether note is a :searched note for every one of names whose look saw none of them (its :saw: kept, barred or
  baby ones seen still show the land has them)."
  [names n]
  (and (searched-for? names n) (not-any? (set (:saw n)) names)))

(defn looked-area
  "Blocks of ground within ::region (XZ) of [x z] that the :searched notes of ns for every one of names looked over in vain (in-vain?),
  each square of `cell` counted once."
  [ns names [x z]]
  (let [region (settings/get settings ::region)
        squares (fn [{[nx _ nz] :pos r :r}]
                  (for [i (range (js/Math.floor (/ (- nx r) cell)) (inc (js/Math.floor (/ (+ nx r) cell))))
                        j (range (js/Math.floor (/ (- nz r) cell)) (inc (js/Math.floor (/ (+ nz r) cell))))
                        :let [cx (* (+ i 0.5) cell) cz (* (+ j 0.5) cell)]
                        :when (and (<= (js/Math.hypot (- cx nx) (- cz nz)) r)
                                   (<= (js/Math.hypot (- cx x) (- cz z)) region))]
                    (+ (* i 1000003) j)))]
    (->> ns
         (filter #(in-vain? names %))
         (filter (fn [{[nx _ nz] :pos r :r}] (<= (js/Math.hypot (- nx x) (- nz z)) (+ region r))))
         (into #{} (mapcat squares))
         count
         (* cell cell))))

(defn find-chance
  "Chance a look over `a` blocks finds one of names after `looked` blocks round it were looked over for them in vain: the
  chance this land spawns them (animal-ground-share, lowered by e^(-density x looked) for the ground that showed none)
  times the chance a look that wide meets a pack there (1 - e^(-density x a)). 1 for names with no base rate."
  [names looked a]
  (if-let [d (density names)]
    (let [q base-rates/animal-ground-share
          w (* q (js/Math.exp (- (* d looked))))]
      (* (/ w (+ w (- 1 q))) (- 1 (js/Math.exp (- (* d a))))))
    1))

(defn look-chance
  "find-chance of a look round of radius r at [x z], over the ground the notes ns searched for names within ::region."
  [ns names at r]
  (find-chance names (looked-area ns names at) (* js/Math.PI r r)))

(defn ore-of
  "The base-rates resource whose ore blocks names all are (\"coal\" for coal_ore and deepslate_coal_ore), else nil."
  [names]
  (let [rs (set (map #(-> % (str/replace #"^deepslate_" "") (str/replace #"_ore$" "")) names))]
    (when (and (seq names) (every? #(str/ends-with? % "_ore") names) (= 1 (count rs)) (seq (base-rates/host-blocks (first rs))))
      (first rs))))

(defn vein-seen [] (settings/get settings ::vein-seen))

(defn ore-chance
  "Chance a look or a cut lays bare a vein when `ore` of its blocks are expected among the faces it shows: veins at
  ::vein-seen blocks each, 1 - e^(-ore / vein-seen)."
  [ore]
  (- 1 (js/Math.exp (- (/ ore (vein-seen))))))

(defn seen-ore
  "Blocks of resource expected among seen ({:name :pos}, faces the body has seen): each of the rock its veins replace
  (base-rates/host?) at the density of a face open to air at its height; other ground holds none."
  [resource seen]
  (->> seen
       (filter #(base-rates/host? resource (:name %)))
       (map #(js/Math.floor (:y (:pos %))))
       frequencies
       (reduce (fn [acc [y n]] (+ acc (* n (base-rates/expected-density resource {:y y} nil :exposed? true)))) 0)))

(def tunnel-faces "Rock faces a 1x2 tunnel lays bare per block it cuts: two walls two high, the roof and the floor." 6)

(defn tunnel-ore
  "Blocks of resource expected among the faces n blocks of tunnel cut at pos's height lay bare (buried rock: no air
  discard), the ground taken as all rock its veins replace."
  [resource pos n]
  (* n tunnel-faces (base-rates/expected-density resource pos nil)))

(defn tunnel-rate
  "Blocks of kinds a strip tunnel is expected to lay bare per second, cutting cells ({:name :y}, feet and head of each
  block in order, :name nil where unseen): a seen block of kinds counts one; host rock of resource (and an unseen cell,
  taken as host rock) half a block's tunnel-ore; air and other ground none. Each block costs walk-s and each cell
  (dig-s name), an unseen one that of the dearest host rock. The cut ends at a seen cell never dug (dig-s nil). nil
  when it meets a seen fluid first: the tunnel stops there and a flood or lava is no ground to mine."
  [resource kinds cells dig-s walk-s]
  (let [unseen-s (apply max (or (seq (keep dig-s (base-rates/host-blocks resource))) [(dig-s "stone")]))
        rock (fn [y] (if resource (* 0.5 (tunnel-ore resource {:y y} 1)) 0))]
    (loop [[{n :name y :y} & more :as cells] cells ore 0 s 0]
      (cond
        (empty? cells) (if (pos? s) (/ ore s) 0)
        (and n (bs/fluids n)) nil
        (nil? n) (recur more (+ ore (rock y)) (+ s unseen-s (* 0.5 walk-s)))
        (bs/air n) (recur more ore (+ s (* 0.5 walk-s)))
        (nil? (dig-s n)) (if (pos? s) (/ ore s) 0)
        :else (recur more (+ ore (cond (kinds n) 1 (base-rates/host? resource n) (rock y) :else 0))
                     (+ s (dig-s n) (* 0.5 walk-s)))))))

(defn ore-level
  "The feet height a strip tunnel for an ore block (\"iron_ore\") runs at, jobs.gather.mine's and the height its tunnel is
  priced at: base-rates/best-level of its resource; nil (where the body stands) for one with none."
  [block]
  (some-> (ore-of [block]) base-rates/best-level))

(defn tunnel-chance
  "leg-chance of a strip tunnel for resource at height y, n blocks a leg: ore-chance of tunnel-ore over each leg's cut,
  every leg alike (a number)."
  [resource y n]
  (ore-chance (tunnel-ore resource {:y y} n)))

(defn leg-chance
  "The chance a leg's look finds one of names: a number when every leg's is alike, else a fn of the leg's index (0 the
  first); nil when none is priced. Animals: a fn, find-chance of `a` blocks of new ground per leg after `looked`
  searched in vain. An ore's blocks: ore-chance of seen-ore over `seen`, the faces a look round here shows, each leg's
  look taken to show rock alike. Others (logs, other blocks): nil."
  [names {:keys [looked a seen]}]
  (cond
    (density names) (fn [i] (find-chance names (+ looked (* i a)) a))
    (ore-of names) (ore-chance (seen-ore (ore-of names) seen))))

(defn chance-of "Leg i's chance of a leg-chance." [chance i] (if (number? chance) chance (chance i)))

(defn leg-stops?
  "The :price stop of a search: a leg of price leg stops it when it costs more than the worth left times the chance its
  look finds one (a chance of 0 always stops)."
  [leg chance left]
  (not (<= leg (* chance left))))

(defn search-s
  "Seconds a search spends per find: legs of leg-s seconds, leg i finding with (chance i) (leg-chance's), walked until
  leg-stops? on the worth left, the seconds expected until a find or that stop over the chance of a find by then: a
  search tried before a sure way of cost C (taken when it finds none) is cheaper exactly when this is under C.
  Infinity when no leg pays, chance is nil (none priced) or the worth has no bound (it sets no stop). A chance alike
  for every leg (a number) is leg-s / chance once the first leg pays, however many legs the worth pays for. Each leg
  spends leg-s (positive) of a finite worth, so it ends."
  [chance leg-s worth]
  {:pre [(pos? leg-s)]}
  (cond
    (not (and chance (js/isFinite worth))) js/Infinity
    (number? chance) (if (leg-stops? leg-s chance worth) js/Infinity (/ leg-s chance))
    :else
    (loop [i 0 spent 0 miss 1 expect 0]
      (let [p (chance i)]
        (if (leg-stops? leg-s p (- worth spent))
          (if (< miss 1) (/ expect (- 1 miss)) js/Infinity)
          (recur (inc i) (+ spent leg-s) (* miss (- 1 p)) (+ expect (* miss leg-s))))))))
