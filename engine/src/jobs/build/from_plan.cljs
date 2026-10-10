(ns jobs.build.from-plan
  (:require [jobs.lib.child :as child]
            [engine.args :as a]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.escape :as escape]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.watch :as watch]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach :as reach]
            [jobs.lib.shelter :as sh]
            [jobs.lib.spent :as spent]
            [plan.shape :as shape]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.check-cache :as check-cache]
            [jobs.build.from-plan.plan :as plan :refer [retry-distance max-retries facing-of facing-ok? body-cells stands-for plan-trouble owed need-of shortage hazards decide rules-input refuse permitted world-block carried-counts judged no-zones planned how placeable misplaced missing digging unseen buildable problem]]
            [jobs.build.from-plan.text :refer [summary announce! stopped-text assoc-fetch-failed]]))

(def doc
  "Build what a plan wants (:plan, optionally only its :part) from what the body carries.
  A planned cell that stands empty gets its block placed. So does a cell holding a replaceable block (leaf_litter,
  fern, vine, water ...), which a place takes over.
  A cell holding any other wrong block (dirt, leaves, stone ...) is dug first and then placed, when its item is
  carried and the rules allow the dig. The best carried tool is used, or the hand for blocks that need none. A
  pickaxe block without a pickaxe is fetched a tool for, else given up as :no-tool. A block that could not be dug, or may not be, is listed
  under :wrong. So is a block that came out of a place in the wrong state (:placed true); it is never dug or placed
  again.
  A door's upper half, a bed's head and a tall plant's upper half are never targets: the lower or foot cell places
  the whole item. A :clear cell holding a block that is not replaceable and no container is dug (nothing carried
  needed). Crops and trees are not this job's.
  Its walks never dig through the build, except from a hole the body stands in (2 or more levels walled): they may climb out.
  One call works until the plan is built or nothing more applies (:continue only while a walk or fetch waits). Every
  step re-reads the plan and the world, then takes the first step that applies:
  0. A place that would shut the body in (it has a way out and would have none) is held back; the body first walks
     out of the plan's footprint (go-to child), then places from outside. Held back :give-up times, a cell is given
     up as :unreachable.
  1. Place every buildable cell within :reach of the eye, lowest first. A cell is buildable when its block is
     carried, the cell below it is not itself still owed, and it is not the body's own feet or head cell.
     jobs.lib.placement picks the click (neighbour, face, cursor, look, sneak) that gives the wanted state. A
     :facing want that is placed plainly waits until the body looks the right way, from the far side.
  2. Dig the wrong blocks within reach.
  3. Walk to a stand cell one to three blocks beside the nearest buildable (or diggable) cell, on the ground there
     (the body's feet height, or one up or down on a slope).
  4. Walk toward the nearest unloaded cell. Unseen cells are never taken as built.
  5. By day, with a bed built and no respawn bed known (jobs.lib.shelter/spawn-bed-live), use the bed to set the
     respawn point (jobs.survival.set-spawn child, once).
  6. Finish.
  A cell whose state no neighbour gives, or with no block to click beside it yet, waits for one. If still missing at
  the end it is given up with jobs.lib.placement's reason (:no-support, :no-room, :opened, :double-slab). A cell whose place is refused, or whose stand cell
  cannot be reached, :give-up times is given up (:refused, :occupied, :no-item, :unreachable or :unloaded).
  Zones: every place is checked against zones and the footprints of the other active plans, when the cell is
  chosen and again before the place. A cell in a zone that does not allow :place, or in another plan's footprint,
  is refused for good (not counted as given up). It is listed in :refused [{:pos :reason :zone|:claim|:footprint|:hazard}], with one
  build.refused warn per reason.
  Water beside a cell is no obstacle by default. :accept names the fluid hazards taken (:fluid-adjacent,
  :lava-adjacent). A cell with an untaken one is refused as :hazard.
  :sturdy-ground true accepts a sturdy block on the ground of a rail line (plan.rail/ground) where the plan wants
  fill; it is not listed :wrong.
  Result: {:placed n :missing [[x y z] ...] :short {item n} :given-up {[x y z] reason} :wrong [{:pos :found
  :want}] :refused [...] :fetch-failed {item {:failed :why}}}. A build that leaves cells missing ends :stopped (:reason :incomplete, :text naming the
  missing count and what it left); only a complete one has no :status. For a block placed in the wrong state, :found names the state it came out in.
  Events: build.done (info), build.short, build.gave-up, build.refused and build.wrong (warns). Event texts name
  the count and the first few cells. The whole list is in :cells of the event and in the result.
  The job declines (one build.declined warn naming the plan and the reason):
  - while no zone list has been read (it waits :plan-trouble).
  A plan that is missing, unreadable or has no cells to build (in :part) ends the job at once: stopped, :reason :plan-trouble,
  with the cause in :text (after the one warn), so a held job never blocks the queue.
  Wood: a generic wood want (jobs.build.wood: planks, logs, wooden_stairs, wooden_slabs, wooden_doors, wooden_fences,
  fence_gates, wooden_trapdoors ...) is built in one species chosen for the plan (carried most, else the nearest log seen,
  else oak), booked, fetched by that species, and named in the result (:wood \"birch\"); a species name (oak_planks) stays exact.
  When cells are missing but none of their blocks is carried and none can be fetched, the job waits (:no-items with :needs).
  Material: while cells wait for an item not carried (and not given up or refused), each round fetches it, once no cell can be placed from what is carried (:fetch,
  jobs.lib.fetch with :explore on: a jobs.items.obtain child for the missing count, walking off to look for what is unseen; an item whose fetch failed is skipped for the next); a pickaxe block to dig without a pickaxe fetches the tool the same way
  (jobs.items.get-tool). With :fetch false, or once a fetch failed, the build goes on with what is carried and ends
  :short as before (the stop text and :fetch-failed name each failed fetch and why), a pickaxe block given up :no-tool.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world" :spec a/name? :default nil}
   :part {:doc "only the cells of this part" :spec a/name? :default nil}
   :reach {:doc "cells whose centre is this close to the eye are placed without walking, in blocks" :spec (a/num-in 0 nil) :default u/eye-reach}
   :give-up {:doc "refused places or failed walks after which a cell is given up" :spec (a/int-in 1 nil) :default 3}
   :accept {:doc "fluid hazards of a cell taken: :fluid-adjacent (water beside; placing beside or into water seals and bridges), :lava-adjacent (lava beside; not taken by default: the body stands beside the cell)" :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default [:fluid-adjacent]}
   :fetch {:doc "get the blocks and tools the plan lacks (jobs.lib.fetch): true, a set of kinds or a map of limits; false builds with what is carried" :spec fetch/option? :default {:explore true}}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}
   :surface {:doc "put each column of the plan on the ground the body has seen there: the column's lowest cell stands on the ground's top, the rest keep their height above it (an unseen column stays as planned); for a pen on a slope" :spec boolean? :default false}
   :sturdy-ground {:doc "a sturdy block on the ground of a rail line (plan.rail/ground) is no wrong block, whatever fill the plan wants there" :spec boolean? :default false}})

(defn check
  "Passes while there is something to build, dig, fetch or look at, and for a plan that can never be built (the round
  ends it :stopped :plan-trouble). Otherwise it waits (:plan-trouble while no zone list is read, or :no-items
  with the {item n} still needed). The plan's cells are judged once per check-cache window."
  [c]
  (let [{:keys [cells trouble]} (check-cache/cached c :planned #(planned c))
        p (:primitives c)]
    (cond
      (and trouble (= no-zones trouble)) (ctx/wait c {:reason :plan-trouble :why trouble})
      trouble true
      (or (:begun (ctx/mem c))
          (empty? (owed cells))
          (seq (buildable cells (carried-counts p) {}))
          (seq (digging cells))
          (some #(or (= :clear (:want %)) (pos? (get (carried-counts p) (:item %) 0))) (unseen cells {}))
          (when-let [w (need-of c cells)] (some? (fetch/due c 'jobs.build.from-plan w)))) true
      :else (ctx/wait c {:reason :no-items :plan (:plan (:args c)) :part (:part (:args c))
                         :needs (into {} (shortage (owed cells) (carried-counts p)))}))))

;; ------------------------------------------------------------------ steps

(defn count-fail
  "m with one more failure on pos; given up as reason at the give-up-th."
  [m pos reason give-up]
  (let [n (inc (get-in m [:fails pos] 0))]
    (if (>= n give-up)
      (-> m (update :fails dissoc pos) (assoc-in [:given-up pos] reason))
      (assoc-in m [:fails pos] n))))

(defn walk-fail
  "count-fail for a failed walk or an out-of-reach arrival, which the job's own stand choice caused: a cell it gives up
  is noted with where the body stood (:walk-given), for retry-walks."
  [m pos reason give-up at]
  (let [m2 (count-fail m pos reason give-up)]
    (cond-> m2
      (and (contains? (:given-up m2) pos) (not (contains? (:given-up m) pos))) (assoc-in [:walk-given pos] at))))

(defn retry-walks
  "m with the cells given up for a walk opened again (given-up and :walk-given dropped, :retries counted): those the body
  stands retry-distance from where it gave up, all of them when finishing?; a cell opened max-retries times stays."
  [m body finishing?]
  (let [open (filter (fn [[pos at]]
                       (and (< (get-in m [:retries pos] 0) (max-retries))
                            (or finishing? (>= (u/dist body at) (retry-distance)))))
                     (:walk-given m))]
    (reduce (fn [m [pos _]]
              (-> m (update :given-up dissoc pos) (update :walk-given dissoc pos) (update-in [:retries pos] (fnil inc 0))))
            m open)))

(defn ^:async place-one!
  "Place the cell's item after asking the access rules and jobs.lib.placement once more; a refusal is booked, nothing
  is placed."
  [c {:keys [pos item want] :as cell}]
  (let [d (decide (rules-input c) (:accept (:args c)) pos)
        h (how c cell)]
    (cond
      (vector? d) (ctx/update-mem! c refuse pos (second d))
      (not= :place d) nil
      (:refused h) (ctx/update-mem! c assoc-in [:unplaceable pos] (:refused h))
      :else
      (let [_ (ctx/update-mem! c assoc :placing pos)
            outcome (await (blocks/place-cell! c (zipmap [:x :y :z] pos) item
                                               {:for-plan (:plan (:args c)) :ignore-zones? (boolean (:ignore-zones? (:args c)))
                                                :click (:click h) :clear false}))
            give-up (:give-up (:args c))]
        (case outcome
          :continue :continue
          :placed (let [wrong (misplaced want item (:block (ctx/child-result c :place)))
                        _ (spent/note! c item pos)]
                    (ctx/update-mem! c #(cond-> (update (dissoc % :placing) :placed (fnil inc 0))
                                          wrong (assoc-in [:misplaced pos] wrong))))
          :already (ctx/update-mem! c dissoc :placing)
          (:occupied :need :no-item :no-support :unreachable) (ctx/update-mem! c #(count-fail (dissoc % :placing) pos (if (= :need outcome) :no-item outcome) give-up))
          (ctx/update-mem! c #(count-fail (dissoc % :placing) pos :refused give-up)))))))

(defn settle-placing!
  "A place that a cut left unbooked (:placing, set before the act): the world is the answer, so a cell the plan now
  judges :match was placed and counts. Another body placing the same block there in the gap would count too."
  [c cells]
  (when-let [pos (:placing (ctx/mem c))]
    (let [placed? (boolean (some #(and (= pos (:pos %)) (= :match (:answer %))) cells))]
      (ctx/update-mem! c #(cond-> (dissoc % :placing) placed? (update :placed (fnil inc 0)))))))

(defn pos-map [pos] (zipmap [:x :y :z] pos))

(defn ^:async dig-one!
  "Dig the wrong block of the cell (a blocks.dig child) after asking the access rules once more; a refusal is
  booked, a failed dig counts a failure, a dug cell counts a dig (given up as :refilled when it keeps coming back)."
  [c {:keys [pos]}]
  (let [v (access/may-dig? (rules-input c) (pos-map pos))
        accept (set (:accept (:args c)))
        judged (access/judge v accept)]
    (case judged
      :ok (let [outcome (await (blocks/dig-cell! c (pos-map pos)
                                                 {:accept accept
                                                  :ignore-zones? (boolean (:ignore-zones? (:args c)))
                                                  :for-plan (:plan (:args c))}))]
            (case outcome
              :continue :continue
              :dug (ctx/update-mem! c #(let [m (update-in % [:digs pos] (fnil inc 0))]
                                         (if (>= (get-in m [:digs pos]) (:give-up (:args c)))
                                           (assoc-in m [:given-up pos] :refilled)
                                           m)))
              :missing (ctx/update-mem! c count-fail pos :missing (:give-up (:args c)))
              (ctx/update-mem! c count-fail pos :refused (:give-up (:args c)))))
      :refused (ctx/update-mem! c refuse pos (select-keys v [:reason :zone :plan :claim]))
      :hazard (ctx/update-mem! c refuse pos {:reason :hazard
                                             :hazards (vec (distinct (remove accept (map access/hazard-key (:hazards v)))))})
      nil)))

(defn diggable
  "The cells of digs not closed (given up or refused); a block no carried tool harvests is given up here as :no-tool."
  [c digs closed]
  (let [open (remove #(contains? closed (:pos %)) digs)
        {no-tool true ok false} (group-by #(tools/needs-tool-to-clear? (:primitives c) (:found %)) open)]
    (when (seq no-tool)
      (ctx/update-mem! c update :given-up merge (into {} (map (fn [cell] [(:pos cell) :no-tool])) no-tool)))
    (vec ok)))

(defn in-reach
  "The buildable cells the body can place from where it stands, lowest first, then nearest."
  [c todo]
  (let [body (u/self-pos c)
        mine (body-cells body)]
    (->> todo
         (filter #(and (<= (u/eye-dist body (:pos %)) (:reach (:args c)))
                       (not (mine (:pos %)))
                       (or (:click %) (facing-ok? (facing-of (:want %)) (:pos %) body))))
         (sort-by (juxt #(get (:pos %) 1) #(u/eye-dist body (:pos %)))))))

(defn in-dig-reach
  "The cells to dig the body can reach from where it stands, lowest first, then nearest."
  [c cells]
  (let [body (u/self-pos c)
        mine (body-cells body)]
    (->> cells
         (filter #(and (<= (u/eye-dist body (:pos %)) (:reach (:args c))) (not (mine (:pos %)))))
         (sort-by (juxt #(get (:pos %) 1) #(u/eye-dist body (:pos %)))))))

;; ------------------------------------------------------------------ keeping a way out

(defn seals?
  "Whether placing at cell would shut the body in: it has a way out now (jobs.lib.reach/enclosed?) and would have none
  with the cell solid. Only a cell at the body's own height band (feet to a block above the head) can; the flood is
  bounded (reach/room-cells)."
  [c cell]
  (let [p (:primitives c)
        by (js/Math.floor (:y (u/self-pos c)))
        cy (second cell)]
    (boolean (and (<= by cy (+ by 2))
                  (not (reach/enclosed? p))
                  (reach/enclosed? p #{(vec cell)})))))

(defn exit-point
  "A cell two blocks outside the box of the planned cells, on the side nearest the body, at its feet height."
  [cells body]
  (let [xs (map #(nth (:pos %) 0) cells)
        zs (map #(nth (:pos %) 2) cells)
        y (js/Math.floor (:y body))
        [x0 x1 z0 z1] [(apply min xs) (apply max xs) (apply min zs) (apply max zs)]
        bx (js/Math.floor (:x body))
        bz (js/Math.floor (:z body))
        clamp (fn [v lo hi] (max lo (min hi v)))]
    (->> [[(- x0 2) y (clamp bz z0 z1)] [(+ x1 2) y (clamp bz z0 z1)]
          [(clamp bx x0 x1) y (- z0 2)] [(clamp bx x0 x1) y (+ z1 2)]]
         (sort-by (fn [[x _ z]] (+ (js/Math.abs (- x bx)) (js/Math.abs (- z bz)))))
         first)))

(defn ^:async leave!
  "One go-to call (child :leave) out of the plan's footprint to the :leave cell; dropped once the walk ends (arrived
  or not: the next step judges where the body stands). :continue while the walk waits, else :again."
  [c]
  (let [[x y z] (:leave (ctx/mem c))
        r (await (child/call-walk c :leave {:pos {:x x :y y :z z} :range 1 :escalate false :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (when-not (= :continue r)
      (ctx/update-mem! c dissoc :leave))
    (if (= :continue r) :continue :again)))

(defn buried?
  "Whether every neighbour of cell [x y z] is a seen solid block: the cell is inside the ground, so it is never seen
  (as opposed to unloaded)."
  [p [x y z]]
  (every? (fn [[dx dy dz]]
            (let [n (u/seen-name p {:x (+ x dx) :y (+ y dy) :z (+ z dz)})]
              (and n (not (shape/air? n)) (not (#{"water" "lava"} n)))))
          [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))

(def hole-depth
  "Levels of walls round the body from its feet up that make it a body in a hole (its own dig): its walks may then climb out."
  2)

(defn in-hole?
  "Whether the body stands in a hole at least hole-depth deep (escape/pit-depth)."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    (<= hole-depth (escape/pit-depth (escape/block-at-of (:primitives c)) [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))))

(def hole-walks
  "Walks in a row that fail from a hole (even climbing out) after which the build stops :shut-in."
  2)

(defn hole-blocked!
  "Count a failed walk from a hole; at hole-walks of them in a row stop the build :shut-in (:done), else :again."
  [c]
  (ctx/update-mem! c update :hole-blocked (fnil inc 0))
  (if (< (:hole-blocked (ctx/mem c)) hole-walks)
    :again
    (do (ctx/result! c {:status :stopped :reason :shut-in :placed (:placed (ctx/mem c) 0)
                        :text (str "build of " (:plan (:args c)) " stopped: the body is shut in a hole and cannot climb out")})
        :done)))

(defn ^:async walk-to!
  "Walk to a stand cell beside cell (:continue while the walk waits, else :again); a cell that cannot be walked to, or is still out of reach (or unseen, as
  :unloaded) on arrival, counts a failure."
  [c cells cell]
  (let [body (u/self-pos c)
        planned (set (map :pos cells))
        bad (set (:bad-stands (ctx/mem c)))
        facing (when-not (or (:click cell) (:dig? cell)) (facing-of (:want cell)))
        stands (stands-for (:primitives c) (:pos cell) (js/Math.floor (:y body)) facing planned bad)
        stand (first (sort-by #(u/dist body (zipmap [:x :y :z] %)) stands))
        give-up (:give-up (:args c))]
    (if-not stand
      (do (ctx/update-mem! c walk-fail (:pos cell) :unreachable give-up (u/self-pos c))
          :again)
      (let [w (await (near/go-near! c (zipmap [:x :y :z] stand) 0 {:zone-tolls true :escalate (in-hole? c)}))]
        (when (= :blocked w)
          (ctx/update-mem! c #(-> (walk-fail % (:pos cell) :unreachable give-up (u/self-pos c))
                                  (update :bad-stands (fnil conj []) stand))))
        (when (and (= :there w) (nil? (:found cell)))
          (let [pos (:pos cell)]
            ;; a cell outside the view cone is seen only by turning to it
            (when (dig-look/unknown? (:primitives c) pos) (await (dig-look/look-at! c pos)))
            (when (nil? (u/seen-block (:primitives c) (zipmap [:x :y :z] pos)))
              (ctx/update-mem! c walk-fail pos (if (buried? (:primitives c) pos) :buried :unloaded) give-up (u/self-pos c)))))
        (when (and (= :there w) (:found cell) (empty? (if (:dig? cell) (in-dig-reach c [cell]) (in-reach c [cell]))))
          (ctx/update-mem! c walk-fail (:pos cell) :unreachable give-up (u/self-pos c)))
        (if (and (= :blocked w) (in-hole? c))
          (hole-blocked! c)
          (do (ctx/update-mem! c dissoc :hole-blocked)
              (if (= :partial w) :continue :again)))))))


(defn finish! [c cells]
  (let [result (merge (assoc-fetch-failed c (summary c cells (ctx/mem c) (:sturdy-ground (:args c))))
                      (some-> (spent/log c) spent/summary))]
    (announce! c result)
    (ctx/result! c (if (seq (:missing result))
                     (assoc result :status :stopped :reason :incomplete :text (stopped-text (:plan (:args c)) result))
                     result))
    :done))

(defn spawn-cell
  "By day, the foot cell of a bed the plan wants that stands built, when no respawn bed is known and the spawn was not
  yet tried by this job; else nil."
  [c cells]
  (let [p (:primitives c)]
    (when (and (not (:spawn-tried (ctx/mem c))) (not (sh/night? p)) (nil? (sh/spawn-bed-live (ctx/view c))))
      (some (fn [{:keys [pos want]}]
              (when (and (map? want) (= "foot" (some-> (:part want) name)) (str/ends-with? (str (:block want)) "_bed")
                         (str/ends-with? (str (:name (world-block p pos))) "_bed"))
                pos))
            cells))))

(defn ^:async set-spawn!
  "Use the built bed at pos to set the respawn point (jobs.survival.set-spawn child); one try."
  [c pos]
  (let [w (await (ctx/call-child c :spawn 'jobs.survival.set-spawn {:bed pos}))]
    (if (= :continue w)
      :continue
      (do (ctx/update-mem! c assoc :spawn-tried true)
          :again))))

(defn ^:async build-step!
  "One step of the build (:again, or :continue while a walk waits): place or dig what is in reach, else walk, else finish."
  [c cells]
  (let [_ (ctx/update-mem! c retry-walks (u/self-pos c) false)
        closed #(merge (:given-up (ctx/mem c)) (:refused (ctx/mem c)))
        todo (placeable c (permitted c (buildable cells (carried-counts (:primitives c)) (closed))))
        digs (diggable c (digging cells) (closed))
        given-up (closed)
        near (in-reach c todo)
        dig-near (in-dig-reach c digs)
        nearest #(first (sort-by (fn [cell] (u/dist (u/self-pos c) (zipmap [:x :y :z] (:pos cell)))) %))]
    (cond
      (:leave (ctx/mem c)) (await (leave! c))
      (seq near) (do (await (watch/watch! c {}))
                     (loop [left near]
                       (when (seq left)
                         (if (seals? c (:pos (first left)))
                           (ctx/update-mem! c #(-> (count-fail % (:pos (first left)) :unreachable (:give-up (:args c)))
                                                   (assoc :leave (exit-point cells (u/self-pos c)))))
                           (do (await (place-one! c (first left)))
                               (recur (rest left))))))
                     :again)
      (seq dig-near) (loop [left dig-near]
                       (if (seq left)
                         (if (= :continue (await (dig-one! c (first left))))
                           :continue
                           (recur (rest left)))
                         :again))
      (seq todo) (await (walk-to! c cells (nearest todo)))
      (seq digs) (await (walk-to! c cells (nearest digs)))
      (seq (unseen cells given-up)) (await (walk-to! c cells (nearest (unseen cells given-up))))
      (spawn-cell c cells) (await (set-spawn! c (spawn-cell c cells)))
      :else (let [before (ctx/mem c)]
              (ctx/update-mem! c retry-walks (u/self-pos c) true)
              (if (= before (ctx/mem c)) (finish! c cells) :again)))))

(defn ^:async step [c]
  (let [{:keys [trouble wood]} (planned c)]
    (cond
      (= no-zones trouble) :declined
      trouble (do (ctx/result! c {:status :stopped :reason :plan-trouble :text (str "plan " (:plan (:args c)) " cannot be built: " trouble)})
                  :done)
      :else
      (do (when-not (:begun (ctx/mem c)) (ctx/update-mem! c assoc :begun true))
          (when (and wood (nil? (:wood (ctx/mem c)))) (ctx/update-mem! c assoc :wood wood))
          (or (await (fetch/fetch! c 'jobs.build.from-plan problem))
              (let [cells (:cells (planned c))]
                (settle-placing! c cells)
                (await (build-step! c cells))))))))

(defn ^:async round
  "The whole attempt: loop the steps until the plan is built or nothing more applies (finish); :continue only while a
  walk or a fetch waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
