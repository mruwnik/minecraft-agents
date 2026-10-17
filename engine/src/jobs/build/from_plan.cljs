(ns jobs.build.from-plan
  (:require [jobs.lib.child :as child]
            [engine.args :as a]
            [clojure.string :as str]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.job-common :as common]
            [jobs.lib.need :as need]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.access :as access]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.escape :as escape]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.tools :as tools]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.util :as u]
            [jobs.lib.storage :as storage]
            [jobs.lib.watch :as watch]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach :as reach]
            [plan.shape :as shape]
            [jobs.lib.check-cache :as check-cache]
            [jobs.build.from-plan.plan :as plan :refer [retry-distance max-retries facing-of facing-ok? body-cells stands-for plan-trouble owed need-of shortage hazards decide rules-input refuse permitted judged no-zones planned how placeable misplaced missing digging unseen buildable problem]]
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
  One call works until the plan is built or nothing more applies (it yields only while a walk or fetch waits). Every
  step re-reads the plan and the world, then takes the first step that applies:
  0. A buildable cell under the body's feet or head makes it walk out of the footprint first (once per cell) when it
     stands on ground the plan did not build, so that cell is placed from outside before the walls close round the body.
     A place that would shut the body in (it has a way out and would have none) is held back; the body first walks
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
  5. Finish.
  A cell whose state no neighbour gives, or with no block to click beside it yet, waits for one. If still missing at
  the end it is given up with jobs.lib.placement's reason (:no-support, :no-room, :opened, :double-slab). A cell whose place is refused, or whose stand cell
  cannot be reached, :give-up times is given up (:refused, :occupied, :no-item, :unreachable or :unloaded).
  Zones: every place is checked against zones and the footprints of the other active plans, when the cell is
  chosen and again before the place. A cell in a zone that does not allow :place, or in another plan's footprint,
  is refused for good (not counted as given up). It is listed in :refused [{:pos :reason :zone|:claim|:footprint|:hazard}], with one
  build.refused warn per reason.
  Water beside a cell is no obstacle by default. :accept names the fluid hazards taken (:fluid-adjacent,
  :lava-adjacent). A cell with an untaken one is refused as :hazard.
  Plan properties: :surface true puts each column of the plan on the ground the body has seen there (the column's lowest
  cell stands on the ground's top, the rest keep their height above it; an unseen column stays as planned), for a pen on a
  slope; :sturdy-ground true accepts a sturdy block on the ground of a rail line (plan.rail/ground) where the plan wants
  fill, not listed :wrong.
  done? holds when no cell is owed, none came out wrong; the engine then ends it :done.
  Ends with data {:placed n :missing [[x y z] ...] :short {item n} :given-up {[x y z] reason} :wrong [{:pos :found
  :want}] :refused [...] :fetch-failed {item {:failed :why}}}: done when nothing is missing (and no block came out wrong: nothing-possible :wrong-blocks) or the cells placed meet
  the call's :least (default :all); else with :text naming the missing count and what it left: nothing-possible
  :incomplete when cells were given up, condition-failed {:reason :no-items :needs} when items are short, condition-failed
  :refused when the rules refuse the rest, nothing-possible :shut-in. For a block placed in the wrong state, :found names the state it came out in.
  Events: build.short, build.gave-up, build.refused, build.wrong and build.gap (a fence or wall placed over air; warns). Event texts name
  the count and the first few cells. The whole list is in :cells of the event and in the result.
  Start facts (one build.declined warn naming the plan and the reason): no zone list read (unless :ignore-zones?) is a
  failed condition {:reason :no-zones}; a plan that is missing, unreadable or has no cells to
  build (in :part) is impossible {:reason :plan-trouble :why text} (a plan missing or unreadable waits), at the start and later in the call; at the start, with nothing to place, dig, fetch or
  look at, a failed condition {:reason :no-items :needs {item n}}.
  Wood: a generic wood want (jobs.build.wood: planks, logs, wooden_stairs, wooden_slabs, wooden_doors, wooden_fences,
  fence_gates, wooden_trapdoors ...) is built in one species chosen for the plan (carried most, else the nearest log seen,
  else oak), booked, fetched by that species, and named in the result (:wood \"birch\"); a species name (oak_planks) stays exact.
  When cells are missing but none of their blocks is carried and none can be fetched, the call ends condition-failed :no-items with :needs.
  Material: while cells wait for an item not carried (and not given up or refused), each round fetches it, once no cell can be placed from what is carried (:fetch,
  jobs.lib.need with :explore on: a jobs.items.obtain child for the missing count, walking off to look for what is unseen; an item whose fetch failed is not asked again in the call); a pickaxe block to dig without a pickaxe fetches the tool the same way
  (jobs.items.get-tool). With :fetch false, or once a fetch failed, the build goes on with what is carried and ends
  short (the text and :fetch-failed name each failed fetch and why), a pickaxe block given up :no-tool.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world" :spec a/name? :default nil}
   :part {:doc "only the cells of this part" :spec a/name? :default nil}
   :reach {:doc "cells whose centre is this close to the eye are placed without walking, in blocks" :spec (a/num-in 0 nil) :default u/eye-reach}
   :give-up {:doc "refused places or failed walks after which a cell is given up" :spec (a/int-in 1 nil) :default 3}
   :accept {:doc "fluid hazards of a cell taken: :fluid-adjacent (water beside; placing beside or into water seals and bridges), :lava-adjacent (lava beside; not taken by default: the body stands beside the cell)" :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default [:fluid-adjacent]}
   :fetch {:doc "get the blocks and tools the plan lacks (jobs.lib.need): true, a set of kinds or a map of limits; false builds with what is carried" :spec need/option? :default {:explore true}}
   :ignore-zones? :shared})

(def target [:plan :part])
(def defaults {:least :all :spend {:job-time ::job-time-s}})
(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one build may take." :spec (a/int-in 1 nil)}
   ::cell-s {:default 3 :doc "Seconds one cell of the plan is guessed to take, for the quote." :spec (a/num-in 0 nil)}})

(defn judged-plan
  "planned, judged once per check-cache window."
  [c]
  (check-cache/cached c :planned #(planned c)))

(defn plan-now
  "The plan as the check or the round reads it: a check (no token) judged once per check-cache window, a round afresh."
  [c]
  (if (:token c) (planned c) (judged-plan c)))

(defn plan-problem
  "Later :no-zones while no zone list is read, later :plan-trouble for a plan that is missing or unreadable (it may be
  saved or fixed later). A readable plan with no cells to build (in :part) is never: the cause is
  {:reason :plan-trouble :why text}."
  [c]
  (let [{:keys [trouble later?]} (plan-now c)]
    (cond
      (nil? trouble) (contract/ok :plan "the plan can be built")
      (= no-zones trouble) (contract/later :no-zones "no zone list has been read")
      later? (contract/later :plan-trouble trouble :why trouble)
      :else (contract/never :plan-trouble trouble :why trouble))))

(def no-items
  "Later :no-items {:plan :part :needs} when there is nothing to build, dig, fetch or look at: the cells' items are
  not carried and no fetch is due."
  (checks/refusing
   :items "there is something to build"
   (fn [c]
     (let [{:keys [cells trouble]} (judged-plan c)
           carried (storage/carried-totals (u/inventory (:primitives c)))]
       (when-not (or trouble
                     (empty? (owed cells))
                     (seq (buildable cells carried {}))
                     (seq (digging cells))
                     (seq (unseen cells (merge (:given-up (ctx/mem c)) (:refused (ctx/mem c)))))
                     (some? (need-of c cells)))
         {:reason :no-items :plan (:plan (:args c)) :part (:part (:args c))
          :needs (into {} (shortage (owed cells) carried))})))))

(def can-start? (cond/and plan-problem no-items))
(def can-run? plan-problem)

(declare result-data)

(defn done?
  "Done when the plan has nothing owed, no block came out wrong (the round ends it with its build.wrong warn); the cells placed this call, the cells still owed and the result so far. A check (no token)
  reads the plan judged once per check-cache window, a round afresh."
  [c]
  (let [{:keys [cells trouble]} (plan-now c)
        placed (:placed (ctx/mem c) 0)
        left (count (owed cells))
        plan (:plan (:args c))
        result (when-not trouble (result-data c cells))
        data [:units :cells :done placed :left left :result result]]
    (cond
      trouble (apply contract/later :plan-trouble (str "plan " plan ": " trouble) data)
      (pos? left) (apply contract/later :owed (str "build of " plan ": " left " cells owed") data)
      (seq (:wrong result)) (apply contract/later :wrong (str "build of " plan ": " (count (:wrong result)) " blocks wrong") data)
      :else (apply contract/ok :built (str "build of " plan " done: placed " placed) data))))

(defn quote-of [c]
  (let [{:keys [cells trouble]} (judged-plan c)
        open (if trouble [] (owed cells))
        n (count open)
        s (* n (settings/get settings ::cell-s))
        items (frequencies (keep :item (vals (into {} (map (juxt :pos identity)) open))))]
    {:uses (cond-> {:job-time [(* 0.5 s) s (* 4 s)]}
             (seq items) (assoc :items (update-vals items (fn [k] [0 k k]))))
     :done [0 n n]}))

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

(defn warn-gap!
  "A build.gap warn when item, a fence or wall just placed at pos, stands over a seen cell of air: an animal walks out under it."
  [c pos item]
  (let [below (u/seen-name (:primitives c) (zipmap [:x :y :z] (update pos 1 dec)))]
    (when (and (re-find #"_(fence|wall)$" item) below (shape/air? below))
      (ctx/emit! c :build.gap :warn {:plan (:plan (:args c)) :cells [pos]
                                     :text (str "build of " (:plan (:args c)) ": " item " at " (pr-str pos) " stands over air, a gap under it")}))))

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
            e (await (blocks/place-call! c (zipmap [:x :y :z] pos) item
                                         {:for-plan (:plan (:args c)) :ignore-zones? (boolean (:ignore-zones? (:args c)))
                                          :click (:click h) :clear false}))
            outcome (blocks/place-outcome e)
            give-up (:give-up (:args c))]
        (case outcome
          :continue :continue
          :placed (let [wrong (misplaced want item (:block (:data e)))]
                    (warn-gap! c pos item)
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
    ;; the flood with the cell solid first: most cells do not shut the body in, and then the second flood is skipped
    (boolean (and (<= by cy (+ by 2))
                  (reach/enclosed? p #{(vec cell)})
                  (not (reach/enclosed? p))))))

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
        r (await (child/call-walk* c :leave {:pos {:x x :y y :z z} :range 1 :ways #{:walk :clear-seal} :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
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
  "Count a failed walk from a hole; at hole-walks of them in a row end the build :shut-in, else :again."
  [c]
  (ctx/update-mem! c update :hole-blocked (fnil inc 0))
  (if (< (:hole-blocked (ctx/mem c)) hole-walks)
    :again
    (ctx/end c :nothing-possible :shut-in
             {:placed (:placed (ctx/mem c) 0)
              :text (str "build of " (:plan (:args c)) " stopped: the body is shut in a hole and cannot climb out")})))

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
      (let [w (await (near/go-near! c (zipmap [:x :y :z] stand) 0 (cond-> {:zone-tolls true} (not (in-hole? c)) (assoc :ways #{:walk :clear-seal}))))]
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


(defn result-data
  "What the build left (text/summary), with the failed fetches of the call."
  [c cells]
  (assoc-fetch-failed c (summary c cells (ctx/mem c) (plan/sturdy-ground? c))))

(defn ending-of
  "The ending of a build with nothing more to do: done when nothing is missing (and no block came out wrong: nothing-possible :wrong-blocks) or the placed cells meet the call's
  :least; else by what holds the rest back: given-up cells (nothing-possible :incomplete), items not carried and not
  fetched (condition-failed :no-items), cells refused by the rules (condition-failed :refused)."
  [c {:keys [placed missing short given-up refused wrong] :as result}]
  (let [data (assoc result :text (stopped-text (:plan (:args c)) result))]
    (cond
      (and (empty? missing) (seq wrong)) (ctx/end c :nothing-possible :wrong-blocks data)
      (or (empty? missing) (common/enough? c placed (+ placed (count missing)))) (ctx/end c :done nil result)
      (seq given-up) (ctx/end c :nothing-possible :incomplete data)
      (seq short) (ctx/end c :condition-failed {:reason :no-items :needs short} data)
      (seq refused) (ctx/end c :condition-failed :refused data)
      :else (ctx/end c :nothing-possible :incomplete data))))

(defn finish! [c cells]
  (let [result (result-data c cells)]
    (announce! c result)
    (ending-of c result)))

(defn ^:async build-step!
  "One step of the build (:again, or :continue while a walk waits): place or dig what is in reach, else walk, else finish."
  [c cells]
  (let [_ (ctx/update-mem! c retry-walks (u/self-pos c) false)
        closed #(merge (:given-up (ctx/mem c)) (:refused (ctx/mem c)))
        wanted (buildable cells (storage/carried-totals (u/inventory (:primitives c))) (closed))
        todo (placeable c (permitted c wanted))
        digs (diggable c (digging cells) (closed))
        given-up (closed)
        near (in-reach c todo)
        mine (body-cells (u/self-pos c))
        feet (first (sort mine))
        on-ground (not-any? #(= (update (:pos %) 1 inc) feet) cells)
        underfoot (when on-ground (first (filter #(mine (:pos %)) wanted)))
        dig-near (in-dig-reach c digs)
        nearest #(first (sort-by (fn [cell] (u/dist (u/self-pos c) (zipmap [:x :y :z] (:pos cell)))) %))]
    (cond
      (:leave (ctx/mem c)) (await (leave! c))
      (and underfoot (not= (:pos underfoot) (:stepped-off (ctx/mem c))))
      (do (ctx/update-mem! c assoc :leave (exit-point cells (u/self-pos c)) :stepped-off (:pos underfoot))
          :again)
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
      :else (let [before (ctx/mem c)]
              (ctx/update-mem! c retry-walks (u/self-pos c) true)
              (if (= before (ctx/mem c)) (finish! c cells) :again)))))

(defn book-failed!
  "Book the failed fetch for wait w (not asked again in this call) and warn fetch.failed."
  [c w e]
  (let [cause (:cause e)
        failed (if (map? cause) (or (:why cause) (:reason cause)) (contract/cause-key e))]
    (ctx/update-mem! c #(-> (dissoc % :fetching) (assoc-in [:fetch-failed (plan/fetch-key w)] {:failed failed})))
    (ctx/emit! c :fetch.failed :warn {:for (:reason w) :failed failed
                                      :text (str "could not fetch " (or (:item w) (str "a tool for " (:block w))) ": " (name failed))})))

(defn ^:async fetch-step!
  "Fetch what the plan lacks (jobs.lib.need) when a fetch is due: :continue while the child waits on the world, :again
  once it ended (a failed one is booked, a limit ends the build), nil when nothing is due."
  [c]
  (when-let [w (plan/problem c)]
    (let [o (assoc (select-keys (need/opts c plan/fetch-job) [:how :explore]) :for-plan (:plan (:args c)))
          key (plan/fetch-key w)
          what (or (:item w) (str "a tool for " (:block w)))
          _ (when-not (= key (:fetching (ctx/mem c)))
              (ctx/update-mem! c assoc :fetching key)
              (ctx/emit! c :fetch.started :info {:for (:reason w) :args (select-keys w [:item :block])
                                                 :text (str "fetching " what " for " (name (:reason w)))}))
          e (await (if (= :need (:reason w))
                     (need/item! c (:item w) (:count w) o)
                     (need/tool! c {:block (:block w)} o)))]
      (cond
        (= :continue e) :continue
        (= :done (:kind e)) (do (ctx/update-mem! c dissoc :fetching)
                                (ctx/emit! c :fetch.done :info {:for (:reason w) :text (str "fetched " what)})
                                :again)
        (#{:limit-reached :cut-off} (:kind e)) (ctx/end c (:kind e) (:cause e) (result-data c (:cells (planned c))))
        :else (do (book-failed! c w e) :again)))))

(defn ^:async step [c]
  (let [{:keys [trouble wood]} (planned c)]
    (if trouble
      :continue
      (do (when (and wood (nil? (:wood (ctx/mem c)))) (ctx/update-mem! c assoc :wood wood))
          (or (await (fetch-step! c))
              (let [cells (:cells (planned c))]
                (settle-placing! c cells)
                (await (build-step! c cells))))))))

(defn ^:async round
  "The whole attempt: loop the steps until the plan is built or nothing more applies (finish); :continue only while a
  walk or a fetch waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
