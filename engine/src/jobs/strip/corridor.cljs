(ns jobs.strip.corridor
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.danger :as danger]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.strip :as strip]
            [jobs.lib.tools :as tools]
            [jobs.lib.torch :as torch]
            [jobs.lib.trip :as trip]
            [jobs.lib.util :as u]))

(def max-steps "Pieces of work of one round before it gives the round back with :continue (danger is weighed again in between)." 6)
(def lit "Light at the feet from which no torch is hung." 8)
(def torch-gap "Cells within which a torch already hung counts as lighting the feet." 3)

(def doc
  "Strip one mineshaft corridor segment (jobs.lib.strip) on one trip. The segment is read again at every step (what is left
  is what the seen blocks show, jobs.lib.strip/targets), so a restart needs nothing but the args. Each step, in this order:
  the trip ends if the body is short of food (jobs.lib.trip/food-short?) or has fewer than :want-free slots free (bag-full);
  a torch is hung on the floor beside the rail when the feet light is under 8 and none hangs within 3 cells (a
  jobs.blocks.place child; torches are the caller's to stock); a chest minecart in the segment is looted (jobs.strip.loot-cart
  child); else the next target is dug (jobs.blocks.dig child, no fetch, drops collected): cobwebs on the walk line first, then
  the rest outward from the :entry end, each with a carried tool of its kind (shears, axe, pickaxe). With no target left it
  walks to the far end and looks round there, so crossings and new corridors come into view, then takes back the torches
  standing in the segment (the caller seals behind it).
  The check declines (wait :danger) while a real danger is near (jobs.lib.danger), unless :danger-check is false.
  A spawner follows :spawner: :light (default) hangs torches on the dark standable centre-line cells of the segment inside its
  spawn box (4 either side, 1 up or down; light 8), leaves the spawner and those torches, and lists it in :kept {:why :spawner :lit true};
  side passages and pockets the body cannot stand beside stay as they are. :break lights it so, then digs it with a pickaxe
  (no drop expected); :seal stops the trip :spawner at once with it left; :skip leaves it, listed in :kept {:why :spawner}.
  Every child's failure ends the trip stopped with the child's reason in :child.
  Ends {:status :done :taken {class n} :kept [{:pos :why}] :new-segments [segment ..]}, :new-segments being the corridors
  seen from the far end that are not this segment (the caller folds them into its memory with merge-segments); or stopped
  {:status :stopped :reason r :taken ..} with r one of :bag-full :food-short :no-torches :no-tool (:tool kind) :spawner (:cell) :refused
  :unreachable :dig-failed :cart-failed :torch-failed :bad-args. Events: strip.corridor.done (info), strip.corridor.stopped (warn).")

(a/defargs args
  {:segment {:doc "the segment map {:y :axis :line :from :to :open-ends} of jobs.lib.strip, or its id string" :spec (a/pred "a segment map or its id string" #(or (map? %) (string? %))) :default nil}
   :entry {:doc "[x y z] of the base side: targets are taken outward from the end nearest it" :spec ::a/pos :default nil}
   :take {:doc "classes taken: :wood :cobweb :rail :ore" :spec (a/coll-of #{:wood :cobweb :rail :ore}) :default [:wood :cobweb :rail :ore]}
   :spawner {:doc "a spawner in the segment: :light lights its spawn box and leaves it, :break lights it and digs it, :seal stops the trip :spawner, :skip leaves it unlit" :spec #{:light :break :seal :skip} :default :light}
   :want-free {:doc "free slots under which the trip turns back; nil: jobs.lib.trip/want-free" :spec (a/int-in 0 36) :default nil}
   :danger-check {:doc "decline while a real danger is near" :spec boolean? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims" :spec boolean? :default false}})

(defn seg-of
  "The segment of the :segment arg: a map as given, an id string \"x-27-290:-180..-160\" read back (its ends taken as closed)."
  [s]
  (if (map? s)
    s
    (when-let [[_ axis y line from to] (some->> s (re-matches #"([xz])(-?\d+)-(-?\d+):(-?\d+)\.\.(-?\d+)"))]
      {:id s :axis (keyword axis) :y (js/parseInt y) :line (js/parseInt line) :from (js/parseInt from) :to (js/parseInt to) :open-ends #{}})))

(defn check
  "Declines while a real danger is near."
  [c]
  (or (not (:danger-check (:args c)))
      (not (danger/danger-near? (:primitives c) (danger/hostile-radius) (danger/ranged-radius)))
      (ctx/wait c {:reason :danger})))

(defn finish!
  [c status extra]
  (let [m (ctx/mem c)
        res (merge {:status status :taken (:taken m {})} extra)]
    (ctx/result! c res)
    (ctx/emit! c (if (= :done status) :strip.corridor.done :strip.corridor.stopped) (if (= :done status) :info :warn)
               (assoc res :text (str "corridor " (name (or (:reason res) :done)) ": took " (pr-str (:taken res)))))
    :done))

(defn stop! [c reason extra] (finish! c :stopped (assoc extra :reason reason)))

(defn light-at
  "Block or sky light of the cell, nil when no light data is loaded there."
  [p [x y z]]
  (when-let [raw (some-> (aget p "perception") :raw)]
    (when (<= 0 (.stateAt ^js raw x y z))
      (let [l (.lightAt ^js raw x y z)] (max (bit-and l 15) (bit-shift-right l 4))))))

(defn torch-spot
  "A cell beside the centre line at the feet's height or a step along it, free, whose floor is not seen open: where a torch stands. nil when none."
  [block-at axis [x y z]]
  (let [cells (for [d [0 1 -1] side [1 -1]] (if (= axis :x) [(+ x d) y (+ z side)] [(+ x side) y (+ z d)]))]
    (first (filter (fn [[cx cy cz]]
                     (let [below (block-at [cx (dec cy) cz])]
                       (and (strip/air? (block-at [cx cy cz])) (not (strip/passable? below)))))
                   cells))))

(defn torch-due?
  "The feet are dark and no torch of this trip hangs within torch-gap of them."
  [c feet]
  (let [dark (some-> (light-at (:primitives c) feet) (< lit))]
    (boolean (and dark (not-any? #(<= (u/dist (zipmap [:x :y :z] feet) (zipmap [:x :y :z] %)) torch-gap) (:torches (ctx/mem c)))))))

(defn ^:async hang-torch! [c seg feet]
  (let [p (:primitives c)
        spot (torch-spot (access/seen-at p) (:axis seg) feet)
        o (cond (zero? (torch/torches-held p)) :none
                (nil? spot) :no-spot
                :else (await (b/place-cell! c (zipmap [:x :y :z] spot) "torch" (select-keys (:args c) [:ignore-zones?]))))]
    (case o
      :continue :continue
      :none (stop! c :no-torches {})
      :no-spot (do (ctx/update-mem! c update :torches (fnil conj []) feet) :again)
      :refused (stop! c :refused {:cell spot})
      (:placed :already) (do (ctx/update-mem! c update :torches (fnil conj []) spot) :again)
      (stop! c :torch-failed {:child o :cell spot}))))

(defn ^:async loot! [c id]
  (let [args {:id id}
        r (await (ctx/call-child c :cart 'jobs.strip.loot-cart args))]
    (case r
      :continue :continue
      :declined (stop! c :cart-failed {:child (:reason (b/child-wait c :cart 'jobs.strip.loot-cart args)) :id id})
      (let [res (ctx/child-result c :cart)]
        (if (= :stopped (:status res))
          (stop! c :cart-failed {:child (:reason res) :id id})
          :again)))))

(defn carts
  "The ids of chest minecarts in sight within 2 cells of the segment."
  [c seg]
  (->> (look/seen-entities (:primitives c) {:names ["chest_minecart"] :radius 32 :max 16})
       (filter #(<= (strip/gap seg (u/cell-vec (:pos %))) 2))
       (mapv :id)))

(defn spawners
  "The spawners seen in the segment: [{:pos :name :class :spawner}], nearest the entry first."
  [c seg]
  (:targets (strip/targets [] seg {:entry (some-> (:entry (:args c)) a/cell ((juxt :x :y :z))) :take #{:spawner} :block-at (access/seen-at (:primitives c))})))

(defn in-spawn-box?
  "Whether cell is inside the spawner's spawn box: 4 either side along x and z, 1 up or down."
  [{:keys [x y z]} [cx cy cz]]
  (and (<= (js/Math.abs (- cx x)) 4) (<= (js/Math.abs (- cy y)) 1) (<= (js/Math.abs (- cz z)) 4)))

(defn dark-near
  "A standable cell of the corridor's centre line inside the spawn box of pos whose light is under 8 and which no torch of
  this trip hangs near, the nearest to pos first; nil when none."
  [c seg {:keys [x y z]}]
  (let [block-at (access/seen-at (:primitives c))
        {:keys [axis line from to]} seg
        along (if (= axis :x) x z)
        hung (:torches (ctx/mem c))
        d2 (fn [[cx cy cz]] (+ (* (- cx x) (- cx x)) (* (- cy y) (- cy y)) (* (- cz z) (- cz z))))]
    (->> (range (max from (- along 4)) (inc (min to (+ along 4))))
         (map #(strip/at axis (:y seg) line %))
         (filter #(and (in-spawn-box? {:x x :y y :z z} %) (let [n (block-at %)] (or (strip/air? n) (strip/rail? n)))
                       (some-> (light-at (:primitives c) %) (< lit))
                       (not-any? (fn [h] (<= (u/dist (zipmap [:x :y :z] %) (zipmap [:x :y :z] h)) torch-gap)) hung)))
         (sort-by d2)
         first)))

(defn ^:async dig! [c {:keys [pos name class]}]
  (let [cell [(:x pos) (:y pos) (:z pos)]
        kind (tools/tool-kind name)]
    (cond
      (and (not= :torch class) (not-any? #(tools/of-kind? (:name %) kind) (u/inventory (:primitives c)))) (stop! c :no-tool {:tool kind :cell cell})
      :else
      (let [o (await (b/dig-cell! c pos (merge {:collect true :need-drop (not= :spawner class)} (select-keys (:args c) [:ignore-zones?]))))]
        (case o
          :continue :continue
          (:dug :missing) (do (ctx/update-mem! c #(-> % (update :dug (fnil conj []) cell) (update-in [:taken class] (fnil inc 0)))) :again)
          :refused (stop! c :refused {:cell cell})
          :unreachable (stop! c :unreachable {:cell cell})
          (stop! c :dig-failed {:child o :cell cell}))))))

(defn torch-cells
  "The torches standing in the segment's corridor, nearest the body first: [{:pos :name :class :torch}], not those in the
  spawn box of a spawner under :light. The cells this trip hung count while the body has not seen them change."
  [c seg]
  (let [block-at (access/seen-at (:primitives c))
        {:keys [axis y line from to]} seg
        keep-lit (when (= :light (:spawner (:args c))) (spawners c seg))
        here (zipmap [:x :y :z] (b/feet-cell c))
        torch? (fn [n] (and n (re-find #"torch$" n)))
        seen (for [a (range from (inc to)) w [-1 0 1] h [0 1 2]
                   :let [cell (strip/at axis (+ y h) (+ line w) a)]
                   :when (torch? (block-at cell))]
               cell)
        hung (filter #(let [n (block-at %)] (or (nil? n) (torch? n))) (:torches (ctx/mem c)))]
    (->> (distinct (concat seen hung))
         (remove (fn [cell] (some #(in-spawn-box? (:pos %) cell) keep-lit)))
         (map (fn [[x yy z :as cell]] {:pos {:x x :y yy :z z} :name (or (block-at cell) "torch") :class :torch}))
         (sort-by #(u/dist here (:pos %))))))

(defn ^:async look-end!
  "Walk to the segment's far end from the entry and look round there once: :again, :continue or a stop."
  [c seg]
  (let [{:keys [axis y line from to]} seg
        entry (some-> (:entry (:args c)) a/cell)
        ea (when entry (if (= axis :x) (:x entry) (:z entry)))
        far (if (and ea (< (js/Math.abs (- ea from)) (js/Math.abs (- ea to)))) to from)
        cell (strip/at axis y line far)
        w (await (near/go-near! c cell 1))]
    (case w
      :partial :continue
      :blocked (stop! c :unreachable {:cell cell})
      (do (await (look/look-around! c))
          :again))))

(defn done! [c seg]
  (let [p (:primitives c)
        blocks (look/seen-blocks p {:match #(or (strip/fence? %) (strip/planks? %) (strip/rail? %)) :radius 24 :max 512})
        news (remove #(strip/overlaps? seg %) (strip/segments blocks {:block-at (access/seen-at p)}))]
    (finish! c :done {:kept (into (:kept (strip/targets [] seg {:block-at (access/seen-at p)}))
                                  (map (fn [t] (cond-> {:pos (:pos t) :why :spawner} (= :light (:spawner (:args c))) (assoc :lit true))))
                                  (when (not= :break (:spawner (:args c))) (spawners c seg)))
                      :new-segments (vec news)})))

(defn ^:async step! [c seg]
  (let [p (:primitives c)
        {:keys [entry take want-free spawner]} (:args c)
        found (strip/targets [] seg {:entry (some-> entry a/cell ((juxt :x :y :z))) :take (cond-> (set take) (= :break spawner) (conj :spawner)) :block-at (access/seen-at p)})
        sealed-spawner (when (= :seal spawner) (first (spawners c seg)))
        dug (set (:dug (ctx/mem c)))
        fresh (fn [xs] (first (remove #(dug [(:x (:pos %)) (:y (:pos %)) (:z (:pos %))]) xs)))
        target (fresh (:targets found))
        cart (first (carts c seg))
        feet (assoc (b/feet-cell c) 1 (:y seg))
        torch (fresh (torch-cells c seg))
        dark (if (= :light spawner)
               (some #(dark-near c seg (:pos %)) (spawners c seg))
               (when (= :spawner (:class target)) (dark-near c seg (:pos target))))
        work? (or target cart)]
    (when (and target (:looked (ctx/mem c))) (ctx/update-mem! c dissoc :looked))
    (cond
      (trip/food-short? c) (stop! c :food-short {})
      sealed-spawner (stop! c :spawner {:cell (u/cell-vec (:pos sealed-spawner))})
      (and work? (< (u/free-slots p) (or want-free (trip/want-free)))) (stop! c :bag-full {})
      (and work? (torch-due? c feet)) (await (hang-torch! c seg feet))
      cart (await (loot! c cart))
      dark (await (hang-torch! c seg dark))
      target (await (dig! c target))
      (not (:looked (ctx/mem c))) (await (look-end! c seg))
      torch (await (dig! c torch))
      :else (done! c seg))))

(defn ^:async round [c]
  (if-let [seg (seg-of (:segment (:args c)))]
    (let [n (atom 0)]
      (await (pace/steps! c #(if (< (swap! n inc) max-steps) (step! c seg) :continue))))
    (stop! c :bad-args {:why "needs :segment (a map or an id)"})))
