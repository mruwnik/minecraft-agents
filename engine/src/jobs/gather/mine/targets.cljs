(ns jobs.gather.mine.targets
  "Seen-block targets of jobs.gather.dig-for: how a block would be dug (classify), the scan for targets and the choice of the next, the ground snapshot and the guards that skip a target, own-zone tests, drops lying near."
  (:require [jobs.gather.mine.way-set :as ways]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.game :as game]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.block-sets :as bs]
            [clojure.string :as str]
            [jobs.lib.death-spot :as death-spot]
            [jobs.lib.hide-spot :as hide-spot]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.near :as near]
            [jobs.lib.result :as result]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.access.zones :as zones]
            [jobs.lib.util :as u]
            [jobs.lib.targets :as targets]
            [jobs.lib.reach :as reach-lib]
            [jobs.lib.look :as look :refer [cell-of headings]]
            [jobs.lib.world :as known]
            [jobs.gather.mine-stay :as stay]))

(a/defargs settings
  {::reach {:default 3 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}
   ::collect-radius {:default 6 :doc "How far around the last dug cell to collect drops after a dig, in blocks." :spec (a/num-in 0 nil)}
   ::stall-rounds {:default 5 :doc "Rounds in a row a walk to one target waits without the body getting nearer before the target is skipped as a failure." :spec (a/int-in 1 nil)}
   ::zone-margin {:default 8 :doc "Before it digs, the body walks to a start cell this many blocks outside any zone it owns; its stair and tunnel stop at such a zone." :spec (a/int-in 0 nil)}})

(defn reach [] (settings/get settings ::reach))

(defn base-ore
  "The stone kind of an ore block (deepslate_iron_ore: iron_ore); any other block as it is."
  [block]
  (let [plain (some-> block (str/replace #"^deepslate_" ""))]
    (if (contains? blocks/ores plain) plain block)))

(defn items
  "The items the call mines for: :item (one or several), else what :block drops."
  [{:keys [item block]}]
  (cond (sequential? item) (vec item)
        item [item]
        block [(blocks/item-name {:block block})]
        :else []))

(defn kinds
  "The block names that count as mined: :block (an ore with its deepslate kind) when given, else every block that drops one of the items."
  [{:keys [block] :as args}]
  (if block
    (let [base (base-ore block)]
      (if (contains? blocks/ores base) #{base (str "deepslate_" base)} #{block}))
    (into #{} (mapcat blocks/blocks-dropping) (items args))))

(defn main-block
  "The block the call stands for where one is needed (the tool, the level, the soil rule): :block, else the first kind by name."
  [args]
  (or (:block args) (first (sort (kinds args)))))

(def not-solid (into bs/air #{"water" "lava" "short_grass" "tall_grass"}))

(defn carried
  "Total of the item carried over all stacks."
  [c]
  (transduce (map #(u/carried-count (:primitives c) %)) + 0 (items (:args c))))

(defn had
  "The count carried when the run began (now, before it began)."
  [c]
  (:had (ctx/mem c) (carried c)))

(defn got
  "How many more are carried than when the run began."
  [c]
  (max 0 (- (carried c) (had c))))

(defn owed
  "How many more than were carried at the start the call's :count asks for."
  [c]
  (max 0 (- (:count (:args c)) (had c))))

(defn around [{:keys [x y z]} [dx dy dz]] {:x (+ x dx) :y (+ y dy) :z (+ z dz)})

(defn face-names
  "The names of the 6 faces of cell pos, as the body knows them: the last seen block, nil for a cell never seen. The cells
  it stands in are felt, and lava directly on top shows as drips, so those read the world. The cell open (a cell a cut is about to
  dig), when given, reads as air."
  [c pos open]
  (let [p (:primitives c)
        feet (cell-of (u/self-pos c))
        own? #(or (= % feet) (= % (update feet :y inc)))]
    (map (fn [f]
           (let [cell (around pos f)
                 seen (look/seen-block p cell)]
             (cond
               (= cell open) "air"
               (own? cell) (u/seen-name p cell)
               (and (= [0 1 0] f) (= "lava" (u/seen-name p cell))) "lava"
               (not (:unknown seen)) (:name seen))))
         rules/neighbour-deltas)))

(defn classify
  "How the cell of a block would be dug: :ok, :wet (only water stops it), :buried (no seen air face) or :no (seen
  lava beside it, or lava on top). A face never seen is neither air nor a hazard. The cell open, when given, is a face the dig will
  have opened (the end of a side cut)."
  ([c wet pos] (classify c wet pos nil))
  ([c wet pos open]
   (let [names (face-names c pos open)]
     (cond
       (not-any? bs/air names) :buried
       (some #{"lava"} names) :no
       (and (some #{"water"} names) (not wet)) :wet
       :else :ok))))

(defn rules-in
  "The rules input: another's plans always refuse, this body's own too unless the spare-own-builds setting is false; a base
  block (chest, bed, furnace ...) is never dug. The call's :through plan is the permission to dig its cells."
  [c]
  (assoc (access/rules-input c {:own-plans-ok? (false? (access/spare-own-builds)) :except (:through (:args c))}) :keep-base? true))

(def soil-blocks "Mined blocks gathered from the top layer of the ground only." #{"dirt" "grass_block"})

(defn too-deep?
  "Whether a soil dig at pos would go deeper than the ground the run started on: that makes a pit the body is left in."
  [c pos]
  (let [{:keys [start]} (ctx/mem c)]
    (boolean (and start (contains? soil-blocks (main-block (:args c))) (< (:y pos) (dec (:y start)))))))

(defn floor-hole?
  "Whether digging pos would open a hole below the body's feet whose drop is lost: not for soil or ores, which are dug where they lie."
  [c pos]
  (let [block (main-block (:args c))]
    (and (< (:y pos) (js/Math.floor (:y (u/self-pos c))))
         (not (contains? soil-blocks block))
         (not (contains? blocks/ores (base-ore block))))))

(defn scan
  "{:targets [pos] nearest first (the higher of two as near), those within a recent death's radius last of all, those below the feet after all level or higher ones (a hole in the floor loses its drop; not soil or ores) and those over the ground snapshot after all others, :refused [verdict] of blocks a
  zone or plan refuses, :wet? true when an unskipped block was rejected only for water, :wet-n how many}. Only seen blocks that are
  still there count."
  [c]
  (let [{:keys [radius wet]} (:args c)
        accept (ways/accept (:args c))
        p (:primitives c)
        skipped (set (:skipped (ctx/mem c)))
        ground (into #{} (map :pos) (:ground (ctx/mem c)))
        here (u/self-pos c)
        stay? (ways/stay-level? (:args c))
        seen (->> (if (ways/seen? (:args c)) (look/seen-blocks p {:names (vec (kinds (:args c))) :radius radius :max 128 :live? true}) [])
                  (map :pos)
                  (remove skipped)
                  (remove #(too-deep? c %)))
        cells (filter #(or (not stay?) (stay/beside-body? (cell-of here) %)) seen)
        heading (headings (:heading (ctx/mem c)))
        side (when (and stay? heading)
               (->> seen
                    (remove #(stay/beside-body? (cell-of here) %))
                    (keep (fn [pos] (when-let [cut (stay/side-cut (cell-of here) heading (settings/get stay/settings ::stay/ore-reach) pos)] (assoc cut :pos pos))))
                    (filter #(not-any? (comp bs/air (fn [cell] (u/seen-name p cell))) (stay/side-cells (cell-of here) %)))
                    (filter #(and (= :ok (classify c wet (:pos %) (stay/side-face (cell-of here) %))) (= :ok (access/judge (access/may-dig? (rules-in c) (:pos %)) accept))))
                    (sort-by #(u/dist here (:pos %)))
                    vec))
        deaths (death-spot/recent c)
        hides (hide-spot/recent c)
        graded (map (juxt identity #(classify c wet %)) cells)
        in (rules-in c)
        judged (->> graded
                    (filter #(= :ok (second %)))
                    (map (fn [[pos]] (let [v (access/may-dig? in pos)] [pos v (access/judge v accept)]))))]
    {:side side
     :far (when stay? (into [] (comp (remove #(stay/beside-body? (cell-of here) %)) (remove (set (map :pos side))) (map access/cell)) seen))
     :targets (->> judged (filter #(= :ok (nth % 2))) (map first) (sort-by (juxt #(if (or (seq (death-spot/near deaths %)) (seq (hide-spot/near hides %))) 1 0) #(if (ground %) 1 0) #(if (floor-hole? c %) 1 0) #(u/dist here %) #(- (:y %)))) vec)
     :refused (into [] (comp (filter #(= :refused (nth % 2))) (map second)) judged)
     :wet? (boolean (some #(= :wet (second %)) graded))
     :wet-n (count (filter #(= :wet (second %)) graded))}))

(defn off-ground?
  "Whether a target is not one of the ground snapshot's cells."
  [c pos]
  (not-any? #(= pos (:pos %)) (:ground (ctx/mem c))))

(defn ground-pos
  "The cells of the 5x5 under start at y-1 and y-2."
  [start]
  (for [dy [-1 -2] dx (range -2 3) dz (range -2 3)] (around start [dx dy dz])))

(defn snapshot
  "The solid cells of the 5x5 under the start at y-1 and y-2 that the body sees (the cells under them it learns as it digs
  them: book-ground!), as {:pos :name}."
  [c start]
  (vec (for [pos (ground-pos start)
             :let [name (u/seen-name (:primitives c) pos)]
             :when (and name (not (not-solid name)))]
         {:pos pos :name name})))

(defn book-ground!
  "A cell of the ground footprint that the snapshot did not see, about to be dug (or dug, its block name given): the block
  is known now, so record it for the mend."
  ([c pos] (book-ground! c pos (u/seen-name (:primitives c) (select-keys pos [:x :y :z]))))
  ([c pos name]
  (let [{:keys [start ground]} (ctx/mem c)
        cell (select-keys pos [:x :y :z])]
    (when (and (:track-ground (:args c)) start name (not (not-solid name))
               (some #{cell} (ground-pos start))
               (not-any? #(= cell (:pos %)) ground))
      (ctx/update-mem! c update :ground (fnil conj []) {:pos cell :name name})))))

(defn way-floor
  "The first of cells (maps {:x :y :z}) that is a still-solid floor of a stair this body cut (memory :stair-way): digging it
  cuts the walk back up."
  [c cells]
  (let [ways (ledger/ways-of c)
        name-at #(u/seen-name (:primitives c) %)]
    (first (filter #(and (contains? ways (access/cell %)) (rules/solid-floor? name-at %)) cells))))

(defn last-exit?
  "Whether digging cell pos would leave the body, which has a side cell to step to now, with none (the floor or the
  step-up of its last way out): Rowan's pit ended on a one-cell column."
  [c pos]
  (let [p (:primitives c)
        feet (let [f (cell-of (u/self-pos c))] [(:x f) (:y f) (:z f)])]
    (and (pos? (reach-lib/exits p :raw feet))
         (zero? (reach-lib/exits p :raw feet [(:x pos) (:y pos) (:z pos)])))))

(defn skip! [c pos] (ctx/update-mem! c update :skipped (fnil conj []) pos))

(defn drop-radius
  "The entity search radius that covers the collect-radius setting around the last dug cell, seen from the body: the body
  stands up to its reach from the cell and the drop may have fallen further."
  [c]
  (let [r (settings/get settings ::collect-radius)
        dug (:dug-at (ctx/mem c))
        here (u/pos-of (.-pos (.self (:primitives c))))]
    (if dug
      (+ r (u/dist here (zipmap [:x :y :z] (map #(+ 0.5 %) dug))))
      r)))

(defn left-behind
  "Drops of the item lying within radius (default: the collect-radius setting around the last dug cell) of the body, as
  {[x y z] count}."
  ([c] (left-behind c (drop-radius c)))
  ([c radius]
  (let [wanted (set (items (:args c)))]
    (into {}
          (comp (filter #(contains? wanted (some-> (.-item %) .-name)))
                (map (fn [e] [(access/cell (cell-of (u/pos-of (.-pos e)))) (or (some-> (.-item e) .-count) 1)])))
          (look/seen-items (:primitives c) {:radius radius :max 32})))))

(defn skip-failed!
  "Skip the target and count a failure."
  [c pos]
  (skip! c pos)
  (ctx/update-mem! c update :failures (fnil inc 0)))

(defn cluster-of
  "pos and the cells of targets joined to it by touching sides, edges or corners."
  [pos targets]
  (let [touching? (fn [a b] (and (not= a b) (every? #(<= (js/Math.abs (- (% a) (% b))) 1) [:x :y :z])))]
    (loop [found #{pos} todo [pos]]
      (if-let [cur (peek todo)]
        (let [more (remove found (filter #(touching? cur %) targets))]
          (recur (into found more) (into (pop todo) more)))
        found))))

(defn skip-unreachable!
  "Skip a target the walk cannot reach (result/unreachable-walk?) and the targets touching it: their stands are the same cells, so
  the verdict holds. No failure counted, so a vein of such cells never ends the run."
  [c pos targets]
  (let [cluster (cluster-of pos targets)]
    (ctx/emit! c :mine.unreachable :info {:pos (access/cell pos) :why (result/why (near/last-walk c)) :cluster (count cluster)
                                          :text (str "mine skips " (str/join "," (access/cell pos)) (when (> (count cluster) 1) (str " and " (dec (count cluster)) " touching") ) ": no way to stand beside it")})
    (doseq [cell cluster] (skip! c cell))
    (ctx/update-mem! c update :unreached #(into (vec %) (remove (set %)) (map access/cell cluster)))))

(defn stall-rounds [] (settings/get settings ::stall-rounds))

(defn stalled!
  "A walk to pos is waiting (:partial): :continue, but the ::stall-rounds-th round in a row with the body no nearer warns
  mine.stalled and skips the target as a failure (:again)."
  [c pos]
  (let [d (u/dist (u/self-pos c) (cell-of pos))
        prev (:stall (ctx/mem c))
        n (if (and (= pos (:pos prev)) (<= (:d prev) d)) (inc (:n prev)) 1)]
    (if (< n (stall-rounds))
      (do (ctx/update-mem! c assoc :stall {:pos pos :d d :n n}) :continue)
      (do (ctx/emit! c :mine.stalled :warn {:pos (access/cell pos) :rounds n :text (str "mine's walk to " (str/join "," (access/cell pos)) " waited " n " rounds without getting nearer; it skips the target")})
          (ctx/update-mem! c dissoc :stall)
          (skip-failed! c pos)
          :again))))

(defn step-cell
  "The feet cell k blocks along the tunnel from its origin."
  [{:keys [origin heading]} k]
  (let [[dx dz] (headings heading)]
    (-> origin (update :x + (* k dx)) (update :z + (* k dz)))))

(defn zone-margin [] (settings/get settings ::zone-margin))

(defn own-zones
  "The zones this body owns (none with :ignore-zones?)."
  [c]
  (when-not (:ignore-zones? (:args c))
    (filter #(zones/same-owner? (:owner %) (ctx/self-name c)) (known/zones c))))

(defn grown
  "Zone z's box grown by m blocks on x and z."
  [z m]
  (-> z (update :min #(-> % (update 0 - m) (update 2 - m))) (update :max #(-> % (update 0 + m) (update 2 + m)))))

(defn within-own-zone
  "The own zone whose box, grown by m, holds the cell [x y z], else nil."
  [c m cell]
  (first (filter #(zones/in-box? cell (grown % m)) (own-zones c))))

(defn drop-in-line?
  "Whether the line from the body's eye to where the drop of the block at pos will lie crosses no solid block (one behind
  another cannot be aimed at, nor its item seen)."
  [c pos]
  (let [p (:primitives c)
        {:keys [x y z]} (u/self-pos c)
        kind-at (fn [x y z] (reach-lib/arrow-kind-of (u/seen-block p {:x x :y y :z z})))]
    (reach-lib/ray-clear? kind-at
                          [(+ (js/Math.floor x) 0.5) (+ y game/eye-height) (+ (js/Math.floor z) 0.5)]
                          [(+ (:x pos) 0.5) (+ (:y pos) 0.25) (+ (:z pos) 0.5)])))

(defn in-line
  "The targets whose drop lies in a clear line from the body's eye, else all of them."
  [c targets]
  (let [clear (filterv #(drop-in-line? c %) targets)]
    (if (seq clear) clear targets)))

(def building-window
  "Blocks round the body in which a plan's footprint cells count as a building to keep clear of (x and z; y within 8)."
  24)

(defn footprint-boxes
  "One {:name plan-id :min :max} box per plan with footprint cells near [x y z] (mineshaft plans left out: a stair is meant to join them)."
  [c [x y z]]
  (let [shafts (known/shafts c)]
    (->> (known/footprints c)
         (filter (fn [[[cx cy cz] id]] (and (not (shafts id)) (<= (js/Math.abs (- cx x)) building-window) (<= (js/Math.abs (- cz z)) building-window)
                                            (<= (js/Math.abs (- cy y)) 8))))
         (group-by val)
         (map (fn [[id cells]]
                (let [ks (map key cells)]
                  {:name id :min (apply mapv min ks) :max (apply mapv max ks)}))))))

(defn buildings
  "Boxes {:name :min :max} of what a stair is not started beside: plan footprints near cell, zones of other owners that do not allow :dig (the body inside one is walked out of it first), live claims the
  body is not inside, and the :home place (3 blocks round it). None with :ignore-zones?."
  [c cell]
  (when-not (:ignore-zones? (:args c))
    (let [self (ctx/self-name c)
          home (mem/place (ctx/view c) :home)]
      (concat (footprint-boxes c cell)
              (filter #(and (zones/foreign-owner? (:owner %) self) (not (contains? (:allow %) :dig))) (known/zones c))
              (for [cl (known/claims c) :when (not (or (zones/same-owner? (:owner cl) self) (zones/in-box? cell cl)))] (assoc cl :name (:id cl)))
              (when home [{:name "home" :min [(- (:x home) 3) (- (:y home) 3) (- (:z home) 3)] :max [(+ (:x home) 3) (+ (:y home) 3) (+ (:z home) 3)]}])))))

(defn within-keep-clear
  "The own zone, or with stair? the building, whose box grown by m holds the cell [x y z], else nil."
  [c m cell stair?]
  (first (filter #(zones/in-box? cell (grown % m)) (concat (own-zones c) (when stair? (buildings c cell))))))

(defn ^:async next-target!
  "The target to walk to next: of the targets off the ground snapshot (else those over it) that the body sees in a
  clear line when any does (the block under the feet only when there is no other: it is dug from beside, where a drop
  deeper down is out of sight), the one the body walks to
  soonest (targets/nearest!), :searching while that search goes on, the nearest in a line when none is found reachable
  (its walk decides); nil with no targets."
  [c targets]
  (let [{:keys [ground start]} (ctx/mem c)
        ground (into (set (map :pos ground)) (when (and start (:track-ground (:args c))) (ground-pos start)))
        under (update (cell-of (u/self-pos c)) :y dec)
        pool (or (not-empty (filterv #(not= under %) targets)) targets)
        off (vec (remove ground pool))
        off (or (not-empty (filterv #(not (floor-hole? c %)) off)) off)
        group (in-line c (if (seq off) off pool))]
    (when (seq group)
      (let [a (await (targets/nearest! c group (reach) {:tag :mine}))]
        (case (:status a)
          :found (:target a)
          :searching :searching
          (first group))))))
