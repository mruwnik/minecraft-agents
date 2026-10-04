(ns engine.fake.animals
  "The animals of the fake world over cljs world data {:blocks {[x y z] name} :states {[x y z] props} :inventory
  [{:name :count}] :entities [{:id :name :kind :pos [x y z] ...}] :next-entity-id :self {:pos [x y z] :held}}:
  interact (feed, shear, lead on and off, knots), the lead's drag and its path regime, the food's tempt, and the
  line walk they share. Pure functions, test-only; the same rules as js/fake-interact.mjs, fake-leash.mjs,
  fake-lead-path.mjs, fake-tempt.mjs and fake-walkline.mjs.
  Entity flags are kebab-case keywords (:in-love :leashed-to-me :leash-holder :breaks-at ...). The interact refusal
  list is copied from js/interact.mjs (REFUSED), not read by interop: the cljs tests do not load the Mineflayer
  adapter."
  (:require [engine.fake.pockets :as pockets]
            [engine.fake.use-on :as use-on]))

;; ---- walking a line (fake-walkline)

(def step 0.1)
(def open-air-re #"^(air|cave_air|void_air|short_grass|tall_grass|fern)$")

(defn cell [[x y z]] [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)])
(defn flat [[ax _ az] [bx _ bz]] (js/Math.hypot (- ax bx) (- az bz)))
(defn dist [[ax ay az] [bx by bz]] (js/Math.hypot (- ax bx) (- ay by) (- az bz)))

(defn open-air?
  "Nothing stands in the cell at pos: open air only (a dragged animal jams in a gate, open or not)."
  [w pos]
  (boolean (re-find open-air-re (get-in w [:blocks (cell pos)] "air"))))

(defn walkable?
  "Open air or an open fence gate: what an animal walking by its own path can enter."
  [w pos]
  (let [k (cell pos)]
    (or (open-air? w pos)
        (and (re-find #"_fence_gate$" (get-in w [:blocks k] "")) (true? (get-in w [:states k :open]))))))

(defn candidates
  "The positions a walk of the points visits, each at the first point's height, in steps of 0.1 block."
  [points]
  (let [y (second (first points))]
    (for [[[fx _ fz] [tx _ tz]] (partition 2 1 points)
          :let [steps (max 1 (js/Math.ceil (/ (flat [fx 0 fz] [tx 0 tz]) step)))]
          n (range 1 (inc steps))]
      [(+ fx (/ (* (- tx fx) n) steps)) y (+ fz (/ (* (- tz fz) n) steps))])))

(defn walk-line
  "Walk points[0] -> ... -> the last point. {:pos :clear}: where the walk ended, and whether it got within stop of
  the last point without meeting a cell enters? refuses (the start cell is never judged)."
  ([points enters?] (walk-line points enters? 0))
  ([points enters? stop]
   (let [target (last points)
         start (cell (first points))
         begin (vec (first points))]
     (if (<= (flat begin target) stop)
       {:pos begin :clear true}
       (let [r (reduce
                (fn [at next]
                  (cond
                    (and (not= (cell next) start) (not (enters? next))) (reduced {:pos at :clear false})
                    (<= (flat next target) stop)
                    (let [d (flat at target)
                          k (/ stop d)
                          [tx _ tz] target
                          [ax ay az] at]
                      (reduced {:pos (if (> d stop) [(+ tx (* (- ax tx) k)) ay (+ tz (* (- az tz) k))] next) :clear true}))
                    :else next))
                begin (candidates points))]
         (if (map? r) r {:pos r :clear true}))))))

;; ---- the food's lure (fake-tempt)

(def breeding-food
  {"cow" ["wheat"] "mooshroom" ["wheat"] "sheep" ["wheat"] "goat" ["wheat"]
   "pig" ["carrot" "potato" "beetroot"]
   "chicken" ["wheat_seeds" "melon_seeds" "pumpkin_seeds" "beetroot_seeds" "torchflower_seeds"]
   "rabbit" ["carrot" "golden_carrot" "dandelion"]
   "bee" ["dandelion" "poppy" "blue_orchid" "allium" "azure_bluet" "red_tulip" "orange_tulip" "white_tulip"
          "pink_tulip" "oxeye_daisy" "cornflower" "lily_of_the_valley" "sunflower" "lilac" "rose_bush" "peony"
          "torchflower" "pink_petals" "wildflowers"]})

(def tempt-range 10)
(def tempt-stop 2.5)
(def gate-search 16)
(def axes [[1 0] [0 1]])

(defn centre [[x y z] & [dx dz]] [(+ x (or dx 0) 0.5) y (+ z (or dz 0) 0.5)])

(defn open-gates
  "Cells of the open fence gates within gate-search of the body."
  [w]
  (->> (:blocks w)
       (filter (fn [[k name]] (and (re-find #"_fence_gate$" name) (true? (get-in w [:states k :open])))))
       (map first)
       sort
       (filter #(<= (dist (centre %) (get-in w [:self :pos])) gate-search))))

(defn walk-to
  "Where animal e ends walking to the body by its own path: straight first, then each way through an open gate, both
  directions; the first that gets there, else the straight walk as far as it went (pressed against the fence)."
  ([w e] (walk-to w e tempt-stop))
  ([w e stop]
   (let [enters? #(walkable? w %)
         body (get-in w [:self :pos])
         y (second (:pos e))
         straight (walk-line [(:pos e) body] enters? stop)
         ways (for [g (open-gates w)
                    [dx dz] axes
                    way [[(centre g (- dx) (- dz)) (centre g) (centre g dx dz)]
                         [(centre g dx dz) (centre g) (centre g (- dx) (- dz))]]]
                way)
         through (->> ways
                      (map (fn [way] (walk-line (concat [(:pos e)] (map (fn [[x _ z]] [x y z]) way) [body]) enters? stop)))
                      (filter :clear)
                      first)]
     (:pos (or through straight)))))

(defn set-pos [w id pos]
  (update w :entities (fn [es] (mapv #(if (= id (:id %)) (assoc % :pos pos) %) es))))

(defn tempt-follow
  "Animals off the lead that eat what the body holds and stand within tempt-range walk to it."
  [w]
  (let [held (get-in w [:self :held])
        body (get-in w [:self :pos])]
    (if-not held
      w
      (reduce (fn [w e] (set-pos w (:id e) (walk-to w e)))
              w
              (filter #(and (not (:leashed %))
                            (some #{held} (get breeding-food (:name %)))
                            (<= (dist (:pos %) body) tempt-range))
                      (:entities w))))))

;; ---- the lead's path regime (fake-lead-path)

(def path-range 6)
(def rest-gap 3.3)

(defn path-follow
  "The new position of a led animal near the body, or nil when the path regime does not apply (the drag runs)."
  [w e]
  (when-not (or (:snaps e) (some? (:trail e)))
    (let [d (flat (:pos e) (get-in w [:self :pos]))]
      (cond
        (> d path-range) nil
        (or (:pin e) (<= d rest-gap)) (:pos e)
        :else (walk-to w e rest-gap)))))

;; ---- the lead (fake-leash)

(def follow-gap 2)

(defn update-entity [w id f]
  (update w :entities (fn [es] (mapv #(if (= id (:id %)) (f %) %) es))))

(defn find-entity [w id] (first (filter #(= id (:id %)) (:entities w))))

(defn stretches-on-walk?
  "breaks-at (a number on the spec): the lead breaks when, at any point of the walk, the body is farther than that
  from the animal. The body goes from `from` to where it is now in one-block steps; the animal, once the body is
  beyond its trail, closes in :pace (default 1: keeps up) blocks per block the body walks."
  [w e from]
  (when-some [breaks-at (:breaks-at e)]
    (let [to (get-in w [:self :pos])
          total (flat from to)
          gap (or (:trail e) follow-gap)
          [fx _ fz] from
          [tx _ tz] to]
      (loop [walked 1 [ax az] [(first (:pos e)) (last (:pos e))]]
        (when (<= walked (js/Math.ceil total))
          (let [k (/ (min walked total) total)
                body [(+ fx (* (- tx fx) k)) 0 (+ fz (* (- tz fz) k))]
                d (flat [ax 0 az] body)]
            (cond
              (> d breaks-at) true
              (<= d gap) (recur (inc walked) [ax az])
              :else (let [s (min (or (:pace e) 1) (- d gap))]
                      (recur (inc walked) [(+ ax (/ (* (- (first body) ax) s) d)) (+ az (/ (* (- (last body) az) s) d))])))))))))

(defn drag-one [w from id]
  (let [e (find-entity w id)
        body (get-in w [:self :pos])
        gap (or (:trail e) follow-gap)]
    (cond
      (or (:snaps e) (stretches-on-walk? w e from))
      (-> w
          (update-entity id #(assoc % :leashed false :leashed-to-me false))
          (use-on/spawn-item (:pos e) "lead" 1))

      (path-follow w e) (set-pos w id (path-follow w e))

      ;; trail (a number on the spec): the animal stays where it is within that many blocks of the body, else it is
      ;; dragged to that far behind it, as a slow animal on a long lead trails a fast body.
      (and (some? (:trail e)) (<= (flat (:pos e) body) gap)) w

      :else (set-pos w id (:pos (walk-line [(:pos e) (update body 0 - gap)] #(open-air? w %)))))))

(defn drag-leashed
  "Animals on the body's lead follow it after a walk that began at `from`; a spec with :snaps, or a lead stretched past
  :breaks-at, breaks the lead and it drops as an item. A led animal is dragged in a straight line: anything at its
  feet but open air stops it on its side."
  ([w] (drag-leashed w (get-in w [:self :pos])))
  ([w from] (reduce #(drag-one %1 from %2) w (map :id (filter :leashed-to-me (:entities w))))))

(defn untie-knot
  "An empty hand on the knot removes it and hands what was tied to it back to the body's own lead. [world' how-many]."
  [w knot]
  (let [tied (filter #(= (:id knot) (:leash-holder %)) (:entities w))
        w (reduce #(update-entity %1 (:id %2) (fn [e] (assoc e :leashed-to-me true :leash-holder nil))) w tied)]
    [(update w :entities (fn [es] (filterv #(not= (:id knot) (:id %)) es))) (count tied)]))

;; ---- interact (fake-interact)

(def reach 3.5)
(def opens-window ["villager" "wandering_trader" "chest_minecart" "hopper_minecart"])
(def mounts ["horse" "donkey" "mule" "skeleton_horse" "zombie_horse" "camel" "camel_husk" "llama" "trader_llama" "minecart" "happy_ghast"])

(defn refusal
  "Why a use on an entity of this name is always refused (copied from js/interact.mjs), or nil."
  [name]
  (cond
    (some #{name} opens-window) "opens-window"
    (or (some #{name} mounts) (re-find #"_(boat|chest_boat|raft)$" name)) "mounts"))

(def none {:consumed 0 :worn 0 :love false :leash nil :changed {}})

(defn finish [[w r]]
  (let [used (or (pos? (:consumed r)) (pos? (:worn r)) (:love r) (some? (:leash r)) (seq (:changed r)))]
    [w (merge {:status (if used "used" "no-effect")} r)]))

(defn feed [w e item]
  (cond
    (:baby e) [(use-on/take-one w item) (assoc none :consumed 1)]
    (or (:in-love e) (:cooldown e)) [w none]
    :else [(-> w (use-on/take-one item) (update-entity (:id e) #(assoc % :in-love true)))
           (assoc none :consumed 1 :love true)]))

(defn shear [w e]
  (if (or (not= "sheep" (:name e)) (:sheared e) (:baby e))
    [w none]
    [(-> w (update-entity (:id e) #(assoc % :sheared true)) (use-on/spawn-item (:pos e) "white_wool" 1))
     (assoc none :worn 1 :changed {:sheared [false true]})]))

(defn leash-on [w e]
  (if (:leashed e)
    [w none]
    [(-> w (use-on/take-one "lead") (update-entity (:id e) #(assoc % :leashed true :leashed-to-me true)))
     (assoc none :consumed 1 :leash "attached")]))

(defn unleash
  "`:pickup true` on the entity: the body stands close enough to pick the dropped lead up at once."
  [w e]
  (let [w (update-entity w (:id e) #(assoc % :leashed false :leashed-to-me false))]
    [(if (:pickup e) (use-on/give-one w "lead") (use-on/spawn-item w (:pos e) "lead" 1))
     (assoc none :leash "detached")]))

(defn effect [w e item]
  (cond
    (false? (:accepts e)) [w none]
    (and item (some #{item} (get breeding-food (:name e)))) (feed w e item)
    (= "shears" item) (shear w e)
    (= "lead" item) (leash-on w e)
    (and (nil? item) (:leashed-to-me e)) (unleash w e)
    (and (nil? item) (= "leash_knot" (:name e)))
    (let [[w' n] (untie-knot w e)] [w' (if (pos? n) (assoc none :changed {:tied [true false]}) none)])
    :else [w none]))

(defn interact
  "Use an item (item nil: the empty hand) on an entity. [world' result], the result {:status :reason :consumed :worn
  :love :leash :changed}."
  [w {:keys [id item]}]
  (let [e (find-entity w id)
        refused (some-> e :name refusal)
        status (fn [s & [reason]] [w (cond-> (assoc none :status s) reason (assoc :reason reason))])]
    (cond
      (nil? e) (status "gone")
      refused (status "cannot" refused)
      (and item (zero? (pockets/carried w item))) (status "no-item")
      (> (dist (get-in w [:self :pos]) (:pos e)) reach) (status "out-of-reach")
      (and (nil? item) (get-in w [:self :held]) (>= (count (:inventory w)) pockets/slots)) (status "full")
      :else
      (let [w (assoc-in w [:self :held] item)]
        (cond
          (:mounts e) [w (assoc none :status "failed" :reason "mounted")]
          (:opens e) [w (assoc none :status "failed" :reason "opened-window")]
          :else (finish (effect w e item)))))))
