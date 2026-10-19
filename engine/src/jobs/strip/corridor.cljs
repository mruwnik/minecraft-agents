(ns jobs.strip.corridor
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.strip :as strip]
            [jobs.lib.tool-kind :as kind]
            [jobs.lib.tools :as tools]
            [jobs.lib.torch :as torch]
            [jobs.lib.trip :as trip]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one corridor trip may take: the digs, torches and walks." :spec (a/num-in 0 nil)}
   ::lit {:default 8 :doc "Light at the feet from which no torch is hung." :spec (a/int-in 0 16)}
   ::torch-gap {:default 3 :doc "Cells within which a torch already hung counts as lighting the feet." :spec (a/int-in 0 16)}
   ::hop {:default 8 :doc "Cells walked between two steps on the way to the far end: a dark corridor is seen about this far." :spec (a/int-in 1 33)}})

(defn lit [] (settings/get settings ::lit))
(defn torch-gap [] (settings/get settings ::torch-gap))
(defn hop [] (settings/get settings ::hop))

(def doc
  "Strip one mineshaft corridor segment (jobs.lib.strip) on one trip. The segment is read again at every step (what is left
  is what the seen blocks show, jobs.lib.strip/targets), so a restart needs nothing but the args. Each step, in this order:
  a torch is hung on the floor beside the rail when the feet light is under 8 and none hangs within 3 cells (a
  jobs.blocks.place child; torches are the caller's to stock); a chest minecart in the segment is looted (jobs.strip.loot-cart
  child); else the next target is dug (jobs.blocks.dig child, no fetch, drops collected): cobwebs on the walk line first, then
  the rest outward from the :entry end, each with a carried tool of its kind (shears, axe, pickaxe). With no target left it
  walks to the far end (8 cells a step, so a spawner on the way is seen and lit) and looks round there, so crossings and new corridors come into view, then takes back the torches
  standing in the segment (the caller seals behind it).
  It does not start (a skipped job) while the body is short of food (:food-short). Its can-run? is later, with what was taken in
  the cause, when the next step cannot go on: :food-short, :bag-full (under :want-free slots free with
  work left), :no-torches (one is due, none carried), :no-tool (:tool, :cell); a called child ends condition-failed with it. Its safe? is ready/safe-at-by-night the end of the segment nearest :entry. A :segment that
  is neither a map nor a readable id is never possible (:bad-args).
  A spawner seen at any step stays in the result (the far end may be out of sight of it). A spawner follows :spawner: :light (default) hangs torches on the dark standable centre-line cells of the segment inside its
  spawn box (4 either side, 1 up or down; light 8), leaves the spawner and those torches, and lists it in :kept {:why :spawner :lit true};
  side passages and pockets the body cannot stand beside stay as they are. :break lights it so, then digs it with a pickaxe
  (no drop expected); :seal stops the trip :spawner at once with it left; :skip leaves it, listed in :kept {:why :spawner}.
  Every child's failure ends the trip with the child's reason in :child.
  Ends :done with data {:taken {class n} :kept [{:pos :why}] :new-segments [segment ..]}, :new-segments being the corridors
  seen from the far end, this segment's longer reading included (the caller folds them into its memory with merge-segments); or
  condition-failed :bag-full :food-short :no-torches :no-tool (cause keys :taken, as can-run?) or :bag-full (a cart it cannot empty) :refused, or
  nothing-possible with :spawner :unreachable :dig-failed :cart-failed :torch-failed (data :cell, :child), the data carrying :taken.
  Settings: ::lit (8), ::torch-gap (3), ::hop (8).")

(a/defargs args
  {:segment {:doc "the segment map {:y :axis :line :from :to :open-ends} of jobs.lib.strip, or its id string" :spec (a/pred "a segment map or its id string" #(or (map? %) (string? %))) :default nil}
   :entry {:doc "[x y z] of the base side: targets are taken outward from the end nearest it" :spec ::a/pos :default nil}
   :take {:doc "classes taken: :wood :cobweb :rail :ore" :spec (a/coll-of #{:wood :cobweb :rail :ore}) :default [:wood :cobweb :rail :ore]}
   :spawner {:doc "a spawner in the segment: :light lights its spawn box and leaves it, :break lights it and digs it, :seal stops the trip :spawner, :skip leaves it unlit" :spec #{:light :break :seal :skip} :default :light}
   :want-free {:doc "free slots under which the trip turns back; nil: jobs.lib.trip/want-free" :spec (a/int-in 0 36) :default nil}
   :ignore-zones? :shared})

(defn seg-of
  "The segment of the :segment arg: a map as given, an id string \"x-27-290:-180..-160\" read back (its ends taken as closed)."
  [s]
  (if (map? s)
    s
    (when-let [[_ axis y line from to] (some->> s (re-matches #"([xz])(-?\d+)-(-?\d+):(-?\d+)\.\.(-?\d+)"))]
      {:id s :axis (keyword axis) :y (js/parseInt y) :line (js/parseInt line) :from (js/parseInt from) :to (js/parseInt to) :open-ends #{}})))

(defn bad-segment
  "Never :bad-args: the :segment arg is neither a map nor a readable id."
  [c]
  (if (seg-of (:segment (:args c)))
    (contract/ok :args "a segment given")
    (contract/never :bad-args "the :segment is neither a map nor a readable id")))

(def not-now
  "Later :food-short: no food and none to eat."
  (checks/refusing :trip "food to eat" (fn [c] (when (trip/food-short? c) :food-short))))

(def failed-conditions "Stop reasons that end condition-failed. The rest are nothing-possible." #{:bag-full :refused})

(defn stop! [c reason extra]
  (let [res (merge {:taken (:taken (ctx/mem c) {})} extra)]
    (ctx/end c (if (failed-conditions reason) :condition-failed :nothing-possible) reason res)))

(def unlit-ok
  "Call keys for the corridor's own walks and digs: it hangs a torch before it works in the dark (torch-due?), so darkness
  is no reason to wait and only a threat holds them."
  {:checks {:safe? '(not (threatened))}})

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
  (let [dark (some-> (sh-sense/light-at (:primitives c) (zipmap [:x :y :z] feet)) (< (lit)))]
    (boolean (and dark (not-any? #(<= (u/dist (zipmap [:x :y :z] feet) (zipmap [:x :y :z] %)) (torch-gap)) (:torches (ctx/mem c)))))))

(defn ^:async hang-torch! [c seg feet]
  (let [p (:primitives c)
        spot (torch-spot (access/seen-at p) (:axis seg) feet)
        o (cond (nil? spot) :no-spot
                :else (await (b/place-cell! c (zipmap [:x :y :z] spot) "torch" (merge unlit-ok (select-keys (:args c) [:ignore-zones?])))))]
    (case o
      :continue :continue
      :no-spot (do (ctx/update-mem! c update :torches (fnil conj []) feet) :again)
      :refused (stop! c :refused {:cell spot})
      (:placed :already) (do (ctx/update-mem! c update :torches (fnil conj []) spot) :again)
      (stop! c :torch-failed {:child o :cell spot}))))

(defn ^:async loot! [c id]
  (let [e (await (ctx/call c :cart 'jobs.strip.loot-cart (assoc unlit-ok :id id)))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) :again
      (= :bag-full (contract/cause-key e)) (stop! c :bag-full {:id id})
      :else (stop! c :cart-failed {:child (contract/cause-key e) :id id}))))

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
                       (some-> (sh-sense/light-at (:primitives c) (zipmap [:x :y :z] %)) (< (lit)))
                       (not-any? (fn [h] (<= (u/dist (zipmap [:x :y :z] %) (zipmap [:x :y :z] h)) (torch-gap))) hung)))
         (sort-by d2)
         first)))

(defn ^:async dig! [c {:keys [pos class]}]
  (let [cell [(:x pos) (:y pos) (:z pos)]
        o (await (b/dig-cell! c pos (merge {:collect true :need-drop (not= :spawner class)} unlit-ok (select-keys (:args c) [:ignore-zones?]))))]
    (case o
      :continue :continue
      (:dug :missing) (do (ctx/update-mem! c #(-> % (update :dug (fnil conj []) cell) (update-in [:taken class] (fnil inc 0)))) :again)
      :refused (stop! c :refused {:cell cell})
      :unreachable (stop! c :unreachable {:cell cell})
      (stop! c :dig-failed {:child o :cell cell}))))

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

(defn side-dirs
  "The headings a stop looks along to see the walls: both sides and the four diagonals. A spawner sits in a side wall, out of
  the cone of a look along the corridor."
  [axis]
  (if (= axis :x)
    [[0 1] [0 -1] [1 1] [1 -1] [-1 1] [-1 -1]]
    [[1 0] [-1 0] [1 1] [-1 1] [1 -1] [-1 -1]]))

(defn walk-stop
  "The coordinate to walk to from here toward far: the farthest stand? cell within hop, else the cell hop away. go-to refuses a
  goal whose floor was never seen, and the rail cells of a dark corridor show before their floor does."
  [stand? here far]
  (let [dir (if (> far here) 1 -1)
        reach-to (if (> (js/Math.abs (- far here)) (hop)) (+ here (* (hop) dir)) far)]
    (or (first (filter stand? (range reach-to here (- dir)))) reach-to)))

(defn walk-range
  "How near the walk to stop-at must get: exactly there at a hop stop, else a seen cell next to the body counts as arrived and
  the glances there repeat for ever."
  [stop-at far]
  (if (= stop-at far) 1 0))

(defn ^:async look-end!
  "Walk to the segment's far end from the entry, hop cells at a time (each stop is a step: it sees and lights what stands
  there), and look round there once: :again, :continue or a stop."
  [c seg]
  (let [{:keys [axis y line from to]} seg
        entry (some-> (:entry (:args c)) a/cell)
        ea (when entry (if (= axis :x) (:x entry) (:z entry)))
        far (if (and ea (< (js/Math.abs (- ea from)) (js/Math.abs (- ea to)))) to from)
        [fx _ fz] (b/feet-cell c)
        here (if (= axis :x) fx fz)
        seen? #(let [[x cy z] (strip/at axis y line %)] (reach/seen-standable? (:primitives c) {:x x :y cy :z z}))
        stop-at (walk-stop seen? here far)
        cell (strip/at axis y line stop-at)
        w (await (near/go-near! c cell (walk-range stop-at far) {:checks (:checks unlit-ok)}))]
    (case w
      :partial :continue
      ;; no floor beyond was ever seen and go-to will not walk there: what can be seen from here is all there is
      :blocked (if (seen? stop-at)
                 (stop! c :unreachable {:cell cell})
                 (do (await (look/look-around! c))
                     :again))
      (if (= stop-at far)
        (do (await (look/look-around! c))
            :again)
        (do (await (look/glance! c (side-dirs (:axis seg))))
            :again)))))

(defn result-of
  "What the segment leaves behind: what was taken, the cells kept (bridge planks, spawners), the segments seen beyond."
  [c seg]
  (let [p (:primitives c)
        blocks (look/seen-blocks p {:match #(or (strip/fence? %) (strip/planks? %) (strip/rail? %)) :radius 24 :max 512})
        news (strip/segments blocks {:block-at (access/seen-at p)})]
    {:taken (:taken (ctx/mem c) {})
     :kept (into (:kept (strip/targets [] seg {:block-at (access/seen-at p)}))
                 (map (fn [t] (cond-> {:pos (:pos t) :why :spawner} (= :light (:spawner (:args c))) (assoc :lit true))))
                 (when (not= :break (:spawner (:args c))) (distinct (concat (spawners c seg) (:spawners (ctx/mem c))))))
     :new-segments (vec news)}))

(defn owed
  "What the segment still asks for: {:target :cart :torch :dark :sealed-spawner}, each nil when nothing is."
  [c seg]
  (let [p (:primitives c)
        {:keys [entry take spawner]} (:args c)
        found (strip/targets [] seg {:entry (some-> entry a/cell ((juxt :x :y :z))) :take (cond-> (set take) (= :break spawner) (conj :spawner)) :block-at (access/seen-at p)})
        dug (set (:dug (ctx/mem c)))
        fresh (fn [xs] (first (remove #(dug [(:x (:pos %)) (:y (:pos %)) (:z (:pos %))]) xs)))
        target (fresh (:targets found))]
    {:target target
     :sealed-spawner (when (= :seal spawner) (first (spawners c seg)))
     :cart (first (carts c seg))
     :torch (fresh (torch-cells c seg))
     :dark (if (= :light spawner)
             (some #(dark-near c seg (:pos %)) (spawners c seg))
             (when (= :spawner (:class target)) (dark-near c seg (:pos target))))}))

(defn blocker
  "The cause {:reason .. :taken ..} the next step cannot go on for until the world changes, in the order step! takes its
  branches, or nil: food short, a full bag with work left, no torch for the one to hang, no tool for the next dig."
  [c seg]
  (let [p (:primitives c)
        {:keys [want-free spawner]} (:args c)
        {:keys [target cart dark]} (owed c seg)
        feet (assoc (b/feet-cell c) 1 (:y seg))
        work? (or target cart)
        no-torch? (zero? (torch/torches-held p))
        kind (some-> target :name kind/tool-kind)
        cause (cond
                (trip/food-short? c) {:reason :food-short}
                (and work? (< (u/free-slots p) (or want-free (trip/want-free)))) {:reason :bag-full}
                (and work? no-torch? (torch-due? c feet)) {:reason :no-torches}
                cart nil
                (and dark no-torch?) {:reason :no-torches}
                (and target (not= :torch (:class target))
                     (not-any? #(kind/of-kind? (:name %) kind) (u/inventory p))) {:reason :no-tool :tool kind :cell (u/cell-vec (:pos target))})]
    (some-> cause (assoc :taken (:taken (ctx/mem c) {})))))

(defn ^:async step! [c seg]
  (let [p (:primitives c)
        {:keys [target cart torch dark sealed-spawner]} (owed c seg)
        feet (assoc (b/feet-cell c) 1 (:y seg))
        work? (or target cart)]
    (when-let [seen (seq (spawners c seg))]
      (ctx/update-mem! c update :spawners #(vec (distinct (concat % seen)))))
    (when (and target (:looked (ctx/mem c))) (ctx/update-mem! c dissoc :looked))
    (cond
      sealed-spawner (stop! c :spawner {:cell (u/cell-vec (:pos sealed-spawner))})
      (and work? (torch-due? c feet)) (await (hang-torch! c seg feet))
      cart (await (loot! c cart))
      dark (await (hang-torch! c seg dark))
      target (await (dig! c target))
      (not (:looked (ctx/mem c))) (await (look-end! c seg))
      torch (await (dig! c torch))
      :else (or (ctx/done c) :continue))))

(defn ^:async round [c]
  (let [seg (seg-of (:segment (:args c)))]
    (await (pace/steps! c #(step! c seg)))))

;; ------------------------------------------------------------------ the contract

(def target [:segment])

(def defaults {:spend {:job-time ::job-time-s}})

(defn place
  "The end of the segment nearest :entry (its :from end without one)."
  [c]
  (when-let [{:keys [axis y line from to]} (seg-of (:segment (:args c)))]
    (let [cell (fn [v] (if (= axis :x) {:x v :y y :z line} {:x line :y y :z v}))
          entry (a/cell (:entry (:args c)))
          d (fn [v] (if entry (u/dist entry (cell v)) 0))]
      (cell (if (< (d to) (d from)) to from)))))

(def safe? (ready/safe-at-by-night place))

(def fit? ready/fit?)

(def can-start? (cond/and bad-segment not-now))

(defn can-run?
  "Later the cause (blocker): facts that change. A called child ends condition-failed with it, what was taken in its data."
  [c]
  (if-let [seg (seg-of (:segment (:args c)))]
    (checks/cause-status :later (blocker c seg) nil)
    (contract/ok :args "no segment")))

(defn done?
  "The segment is stripped: nothing left to take or light, the far end looked at. The result is what it leaves behind."
  [c]
  (let [seg (seg-of (:segment (:args c)))
        dug (count (:dug (ctx/mem c)))
        left (when seg (owed c seg))
        stripped? (and seg (:looked (ctx/mem c)) (not-any? left [:target :cart :torch :dark :sealed-spawner]))]
    (if stripped?
      (contract/ok :stripped (str "corridor done: took " (pr-str (:taken (ctx/mem c) {}))) :units :cells :done dug :result (result-of c seg))
      (contract/later :digging (str "corridor: " dug " cells dug") :units :cells :done dug
                      :result {:taken (:taken (ctx/mem c) {})}))))

(defn quote-of [c]
  (let [{:keys [from to]} (seg-of (:segment (:args c)))]
    {:uses {:job-time [1 (max 1 (- (or to 0) (or from 0))) (settings/get settings ::job-time-s)]}}))

(def meta {:tags [:mine :light]})
