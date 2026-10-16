(ns jobs.strip.site
  (:require [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.shafts :as shafts]
            [jobs.lib.shelter :as sh]
            [jobs.lib.strip :as strip]
            [jobs.lib.torch :as torch]
            [jobs.lib.trip :as trip]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]))

(def doc
  "Strip one abandoned mineshaft (a site) of its wood, cobwebs, rails, ore and chest-minecart loot, from a field base, and
  leave it sealed behind. Submit it listed with :survive-death? true: the site's state lives in body memory
  :strip/sites {site {:at :base :shaft :was :segments :stops :phase :trip ..}}, so a restart, a stop or a death resumes
  where it was. Every step is a registered job run as a child, re-reading the world first; a child that fails ends
  this job stopped. Phases:
  :base   build the plan :base (build.from-plan), move the respawn to its bed (survival.set-spawn) and :home to the same
          bed (memory.set-place); the old :home and :spawn-bed are kept in :was. :shears  items.get-tool shears.
  :strip  go down (the :shaft plan with access.stair :through, nil = the nearest usable mineshaft plan, false = walk), read the
          site's corridors from the seen blocks (jobs.lib.strip), then repeat: strip.corridor on the next open segment
          (a branch only after the segment it hangs on; one with an unseen end first), strip.seal at the mouth of a
          segment once it and every segment beyond it are stripped, sealed or skipped. The segment holding :at is never
          sealed. A segment that stops :stop-limit times for one reason is :skipped with the reason. A stop for
          :bag-full :food-short :no-torches :no-tool, or low torches or free slots, is a trip: deposit into the base chests
          (storage.deposit), obtain torches, get the tool, get food (survival.get-food, once per trip; still hungry with none: :food-short; :fetch false skips the try), then back down; at most :max-trips.
  Each time at the base, before the deposit, the raw ore carried (raw_iron raw_gold raw_copper) is loaded into the base
          furnaces (items.smelt, :fetch only from chests; one kind split over both furnaces, several one kind each) and
          the body goes back down; the furnaces and their args are kept in :cooking. The next visit replays those
          children to collect the output, which is deposited with the rest. A smelt that gives up is listed in
          :smelt-failed and the ore stays raw (not with :smelt false).
  :haul   first collects the output still in the furnaces (waits while it cooks) and banks it in the first base chest, then
          storage.haul from the base chests to the main storage (not with :haul false).
  The cleanup export puts :home and the respawn bed back to :was however the call ends (a cut included). The torches the
  corridor took back are carried.
  Needs :site and :base (a new site also :at), else impossible :bad-args. Its safe? is ready/safe-at :at.
  Ends :done with data {:segments {state n} :taken {class n} :hauled {item n} :smelted {raw-item n} :smelt-failed [{:furnace :reason}]},
  or condition-failed (:bad-plan :no-site :no-shears :no-torches :food-short :base-full, or :child-failed), nothing-possible
  (:unreachable when the walk down found no way, :trips-exhausted, :all-skipped with :skipped {reason n} when no segment was
  stripped or sealed, :child-failed), or :child-failed ending with the kind of a
  child that ended condition-failed, limit-reached or impossible, with the same data and
  :phase, :child and :because (the child's reason) for a child. Event: strip.site.stopped (warn).")

(a/defargs args
  {:site {:doc "the site's name, the key of its memory" :spec a/name? :default nil}
   :at {:doc "[x y z] of a rail, plank or fence of the site the body has seen; kept once given" :spec ::a/pos :default nil}
   :base {:doc "id of the field-base plan (blueprint field-base)" :spec a/name? :default nil}
   :shaft {:doc "id of a mineshaft plan down from the base; nil: the nearest usable one; false: walk" :spec (a/or-of a/name? boolean?) :default nil}
   :radius {:doc "how far from :at a corridor belongs to the site" :spec (a/int-in 8 65) :default 64}
   :take {:doc "classes stripped" :spec (a/coll-of #{:wood :cobweb :rail :ore}) :default [:wood :cobweb :rail :ore]}
   :spawner {:doc "a spawner in a segment: :light lights its spawn box and leaves it, :break lights and digs it, :seal seals the segment's mouth at once (its subtree skipped), :skip leaves it unlit" :spec #{:light :break :seal :skip} :default :light}
   :fetch {:doc "get what a trip lacks: food (jobs.survival.get-food, tried once per trip) and, passed to strip.seal, the blocks to seal with; true, a set of kinds or a map of limits; false stops :food-short and :no-blocks" :spec need/option? :default true}
   :seal-with {:doc "blocks to seal with, the first carried; nil: strip.seal's own" :spec (a/coll-of a/item?) :default nil}
   :smelt {:doc "false leaves the raw ore carried to the base raw" :spec boolean? :default true}
   :haul {:doc "false leaves the loot in the base chests" :spec boolean? :default true}
   :want-free {:doc "free slots under which a trip turns back; nil: jobs.lib.trip/want-free" :spec (a/int-in 0 36) :default nil}
   :max-trips {:doc "trips to the base before it stops :trips-exhausted" :spec (a/int-in 0 100) :default 12}
   :stop-limit {:doc "stops of one segment for one reason before it is skipped" :spec (a/int-in 1 10) :default 3}
   :ignore-zones? {:doc "act regardless of zones and claims" :spec boolean? :default false}})

(a/defargs settings
  {::job-time-s {:default 7200 :doc "Seconds a strip of one site may take: base, descent, every segment, trips and the haul." :spec (a/num-in 0 nil)}
   ::max-sites {:default 4 :doc "Sites kept in memory; finished ones go first once there are more." :spec (a/int-in 1 nil)}})

(def kind :strip/sites)
(def policy {:cap 1 :ttl :forever})
(def trip-reasons #{:bag-full :food-short :no-torches :no-tool})
(def settled #{:stripped :sealed :skipped})
(def condition-reasons "Stop reasons that end condition-failed: a fact of the base, the bag or the site. The rest are nothing-possible."
  #{:bad-plan :no-site :no-shears :no-torches :food-short :base-full})

(defn sites [c] (or (:data (ctx/latest c kind)) {}))
(defn site [c] (get (sites c) (:site (:args c))))

(defn save!
  "Write the site's state as (f state & xs); sites finished earlier go once there are more than ::max-sites."
  [c f & xs]
  (let [id (:site (:args c))
        all (update (sites c) id #(apply f % xs))
        old (take (- (count all) (settings/get settings ::max-sites)) (filter #(= :done (:phase (all %))) (remove #{id} (keys all))))]
    (ctx/remember! c kind (apply dissoc all old) policy)))

(defn vec3 [v] (let [{:keys [x y z]} (a/cell v)] [x y z]))

(defn counts [s] (frequencies (map :state (vals (:segments s)))))

(defn ^:async call!
  "Run a child one round: :continue while it waits on the world, else its result as an old parent reads it,
  {:status :stopped :reason its cause :ending e} when it did not end :done."
  [c slot job args]
  (let [e (await (ctx/call c slot job args))]
    (cond (= :continue e) :continue
          (= :done (:kind e)) (assoc (:data e) :status :done)
          :else (assoc (:data e) :status :stopped :reason (if (map? (:cause e)) (contract/cause-key e) (or (:cause e) (:kind e))) :ending e))))

(defn failed? [res] (boolean (or (= :stopped (:status res)) (:gave-up res) (false? (:ok res)))))

(defn ^:async run-all!
  "The steps [[slot job args] ..] in order: nil when all ended well, :continue while one waits, else the failure with :child."
  [c steps]
  (loop [[[slot job args] & more] steps]
    (if-not slot
      nil
      (let [res (await (call! c slot job args))]
        (cond (= :continue res) :continue
              (failed? res) (assoc res :child (str job))
              :else (recur more))))))

(defn ^:async smelt-run!
  "Each [slot args] smelt child once: {:cooking {slot args} :smelted {item n} :failed [{:furnace :reason}]}. One that waits on its
  cooking (or on a danger) stays in :cooking; one that gave up is dropped."
  [c entries]
  (loop [[[slot args] & more] entries acc {:cooking {} :smelted {} :failed []}]
    (if-not slot
      acc
      (let [res (await (call! c slot 'jobs.items.smelt args))
            cooking? (or (= :continue res) (= :cooking (:reason res)))]
        (recur more (cond cooking? (assoc-in acc [:cooking slot] args)
                          (failed? res) (update acc :failed conj {:furnace (:furnace args) :reason (:reason res)})
                          :else (update acc :smelted #(merge-with + % {(:item args) (:smelted res 0)}))))))))

(defn smelt-saved
  "The site state after smelt-run! result r: cooking replaced by what still cooks plus what was loaded, the output counted."
  [s r kept]
  (cond-> (-> s
              (assoc :cooking (merge kept (:cooking r)))
              (update :smelted #(merge-with + (or % {}) (:smelted r)))
              (update :smelt-failed (fnil into []) (:failed r)))
    (some pos? (vals (:smelted r))) (assoc :bank true)))

(defn stop!
  "Stop with reason and fields."
  [c reason fields]
  (let [s (site c)]
    (ctx/update-mem! c assoc :stopping (merge {:reason reason :phase (:phase s)} fields))
    :again))

(defn child-stop! [c failure]
  (stop! c :child-failed {:child (:child failure) :because (:reason failure) :child-kind (get-in failure [:ending :kind])}))

(defn spot [plan name]
  (some (fn [[k v]] (when (str/ends-with? k (str "/" name)) v)) (:spots plan)))

(defn plan-of [c s] (known/plan c (:base s)))
(defn chests [c s] (let [plan (plan-of c s)] (vec (keep #(spot plan %) ["chest-1" "chest-3"]))))
(def ores ["raw_iron" "raw_gold" "raw_copper"])
(defn furnaces
  "[[slot furnace-cell] ..] of the plan's furnaces."
  [c s] (let [plan (plan-of c s)] (vec (keep-indexed (fn [i k] (when-let [f (spot plan k)] [(keyword (str "smelt-" i)) f])) ["furnace-1" "furnace-2"]))))

(defn loads
  "[[slot args] ..] smelting the carried ore {item n} in the free furnaces fs [[slot cell] ..]: one kind split over them, several one kind each, most first."
  [fs carried ignore-zones?]
  (let [kinds (sort-by (comp - val) (filter (comp pos? val) (select-keys carried ores)))
        k (count fs)
        one (fn [[slot f] item n] [slot {:furnace f :item item :count (min 64 n) :fetch {:what #{:item} :how #{:chest}} :ignore-zones? ignore-zones?}])]
    (cond (or (zero? k) (empty? kinds)) []
          (= 1 (count kinds)) (let [[item n] (first kinds)]
                                (vec (keep-indexed (fn [i fs1] (let [share (+ (quot n k) (if (< i (rem n k)) 1 0))] (when (pos? share) (one fs1 item share)))) fs)))
          :else (mapv (fn [fs1 [item n]] (one fs1 item n)) fs kinds))))

(defn shears? [c] (boolean (some #(= "shears" (:name %)) (u/inventory (:primitives c)))))

(defn ^:async base!
  [c s]
  (let [plan (plan-of c s)
        bed (spot plan "bed")
        zones (select-keys (:args c) [:ignore-zones?])]
    (cond
      (or (nil? plan) (:broken plan) (nil? bed)) (stop! c :bad-plan {:base (:base s)})
      (nil? (:was s)) (do (save! c assoc :was {:home (mem/place (ctx/view c) :home)
                                               :spawn-bed (:pos (:data (sh/spawn-bed-live (ctx/view c))))})
                          :again)
      :else (let [f (await (run-all! c [[:build 'jobs.build.from-plan (merge {:plan (:base s)} zones)]
                                        [:spawn 'jobs.survival.set-spawn {:bed bed}]
                                        [:home 'jobs.memory.set-place {:name "home" :pos bed}]]))]
              (cond (= :continue f) :continue
                    f (child-stop! c f)
                    :else (do (save! c assoc :phase :shears) :again))))))

(defn ^:async shears! [c s]
  (let [f (when-not (shears? c) (await (run-all! c [[:shears 'jobs.items.get-tool {:kind "shears"}]])))]
    (cond (= :continue f) :continue
          f (child-stop! c f)
          (not (shears? c)) (stop! c :no-shears {})
          :else (do (save! c assoc :phase :strip) :again))))

(defn tree [s] (strip/order (vals (:segments s)) (vec3 (:at s))))

(defn beyond [t id] (mapcat (fn [k] (cons k (beyond t k))) (get-in t [:children id])))

(defn seal-ready
  "The segment to seal next, deepest first: stripped, not the root, everything beyond it settled."
  [s t]
  (let [state #(get-in s [:segments % :state])]
    (->> (reverse (:order t))
         (filter #(and (= :stripped (state %)) (not= % (:root t)) (every? settled (map state (beyond t %)))))
         first)))

(defn next-open
  "The next segment to strip: open, its parent settled, one with an unseen end first."
  [s t]
  (let [open (filter #(and (= :open (get-in s [:segments % :state]))
                           (settled (get-in s [:segments (get-in t [:parent %]) :state] :stripped)))
                     (:order t))]
    (get (:segments s) (or (first (filter #(seq (get-in s [:segments % :open-ends])) open)) (first open)))))

(defn mouth
  "{:at :side :entry} of seg on the side of its parent: the cell one step inside the segment from where the parent meets it."
  [s t seg]
  (let [parent (get (:segments s) (get-in t [:parent (:id seg)]))
        {:keys [axis y line from to]} seg
        j (cond (nil? parent) from
                (= axis (:axis parent)) (if (<= (:to parent) from) from to)
                :else (:line parent))
        side (if (<= j from) :from :to)]
    {:side side :at (strip/at axis y line (if (= side :from) (inc j) (dec j))) :entry (if parent (strip/at axis y line j) (vec3 (:at s)))}))

(defn in-site? [c s]
  (let [{:keys [x y z]} (u/self-pos c)
        [ax ay az] (vec3 (:at s))]
    (and (<= (js/Math.abs (- x ax)) (:radius (:args c))) (<= (js/Math.abs (- z az)) (:radius (:args c))) (<= (js/Math.abs (- y ay)) 4))))

(defn shaft-for [c s]
  (let [id (:shaft s)]
    (when-not (false? id)
      (let [usable (:usable (shafts/lookup c (u/self-pos c) {:foreign-zone? (:ignore-zones? (:args c))}))]
        (first (if (string? id) (filter #(= id (:id %)) usable) usable))))))

(defn ^:async enter!
  "Down to the site: the shaft's stair when one applies, then a walk to :at."
  [c s]
  (let [{:keys [top to-y heading id]} (shaft-for c s)
        above? (and top (> (:y (u/self-pos c)) (inc to-y)))
        cell (if above? top (vec3 (:at s)))
        w (await (child/call-walk* c :enter (cond-> {:pos cell :range (if above? 1 2)} above? (assoc :ways #{:walk :clear-seal}))))]
    (cond
      (= :continue w) :continue
      (not= :done (:kind w)) (stop! c (if (= :nothing-possible (:kind w)) :unreachable :child-failed)
                                    {:cell cell :child "jobs.movement.go-to" :because (contract/cause-key w) :child-kind (:kind w)})
      above? (let [f (await (run-all! c [[:stair 'jobs.access.stair {:dir :down :heading heading :y to-y :through id :stock-torches false
                                                                       :ignore-zones? (:ignore-zones? (:args c))}]]))]
               (cond (= :continue f) :continue f (child-stop! c f) :else :again))
      :else :again)))

(defn ^:async read-site! [c s]
  (await (look/look-around! c))
  (let [p (:primitives c)
        blocks (look/seen-blocks p {:match #(or (strip/fence? %) (strip/planks? %) (strip/rail? %)) :radius (:radius (:args c)) :max 1024})
        segs (filterv #(<= (strip/gap % (vec3 (:at s))) (:radius (:args c))) (strip/segments blocks {:block-at (access/seen-at p)}))]
    (if (empty? segs)
      (stop! c :no-site {:at (:at s)})
      (do (save! c assoc :segments (strip/merge-segments {} segs)) :again))))

(defn trip-due? [c s]
  (let [p (:primitives c)]
    (or (:trip s) (< (torch/torches-held p) (trip/torch-low)) (< (u/free-slots p) (or (:want-free (:args c)) (trip/want-free))))))

(defn ^:async deposit! [c s i]
  (await (run-all! c [[:deposit 'jobs.storage.deposit (merge {:chest (nth (chests c s) i) :keep {"torch" 64} :make-chests? false}
                                                             (select-keys (:args c) [:ignore-zones?]))]])))

(defn load-plan [c s]
  (let [cooking (:cooking s)
        free (remove #(contains? cooking (first %)) (furnaces c s))]
    (loads free (into {} (map (juxt :name :count)) (u/inventory (:primitives c))) (:ignore-zones? (:args c)))))

(defn ^:async trip!
  "At the base: deposit into the next base chest while the bag is short of room, obtain torches, get the lost tool; then back down."
  [c s]
  (let [p (:primitives c)
        {:keys [reason tool deposits] :or {deposits 0}} (:trip s)
        cs (chests c s)
        short? (< (u/free-slots p) (or (:want-free (:args c)) (trip/want-free)))
        step (cond
               (and (seq (:cooking s)) (not (:collected (:trip s)))) :collect
               (and (:smelt (:args c)) (not (:loaded (:trip s))) (seq (load-plan c s))) :load
               (or (:bank s)
                   short? (and (= :bag-full reason) (zero? deposits))) :deposit
               (< (torch/torches-held p) (trip/torch-low)) :torches
               (and (= :no-tool reason) (not (:tool-done (:trip s)))) :tool
               (trip/food-short? c) :food)]
    (case step
      :collect (let [r (await (smelt-run! c (:cooking s)))]
                 (save! c #(-> (smelt-saved % r {}) (assoc-in [:trip :collected] true)))
                 :again)
      :load (let [r (await (smelt-run! c (load-plan c s)))]
              (save! c #(-> (smelt-saved % r (:cooking %)) (assoc-in [:trip :loaded] true)))
              :again)
      :deposit (if (>= deposits (count cs))
                 (stop! c :base-full {})
                 (let [f (await (deposit! c s deposits))]
                   (cond (= :continue f) :continue
                         f (child-stop! c f)
                         :else (do (save! c #(-> % (update-in [:trip :deposits] (fnil inc 0)) (dissoc :bank))) :again))))
      :torches (let [f (await (run-all! c [[:torches 'jobs.items.obtain {:item "torch" :count (- (trip/torch-stock) (torch/torches-held p)) :explore false}]]))]
                 (cond (= :continue f) :continue f (child-stop! c f)
                       (< (torch/torches-held p) (trip/torch-low)) (stop! c :no-torches {})
                       :else :again))
      :tool (let [f (await (run-all! c [[:tool 'jobs.items.get-tool {:kind tool}]]))]
              (cond (= :continue f) :continue f (child-stop! c f) :else (do (save! c assoc-in [:trip :tool-done] true) :again)))
      :food (if (or (:food-done (:trip s)) (not (need/opts c 'jobs.strip.site)))
              (stop! c :food-short {})
              (let [f (await (run-all! c [[:food 'jobs.survival.get-food {}]]))]
                (if (= :continue f)
                  :continue
                  (do (save! c assoc-in [:trip :food-done] true) :again))))
      (if (> (:trips s 0) (:max-trips (:args c)))
        (stop! c :trips-exhausted {:trips (:trips s)})
        (do (save! c #(-> % (dissoc :trip) (update :trips (fnil inc 0)))) :again)))))

(defn ^:async seal! [c s t id]
  (let [seg (get (:segments s) id)
        {:keys [at side]} (mouth s t seg)
        f (await (run-all! c [[:seal 'jobs.strip.seal (cond-> {:at at :axis (:axis seg) :side side :ignore-zones? (:ignore-zones? (:args c)) :fetch (:fetch (:args c))}
                                                        (:seal-with (:args c)) (assoc :use {:blocks (:seal-with (:args c))}))]]))]
    (cond (= :continue f) :continue
          f (child-stop! c f)
          :else (do (save! c assoc-in [:segments id :state] :sealed) :again))))

(defn ^:async corridor! [c s t seg]
  (let [{:keys [entry]} (mouth s t seg)
        {:keys [take spawner want-free stop-limit ignore-zones?]} (:args c)
        res (await (call! c :corridor 'jobs.strip.corridor
                          (cond-> {:segment seg :entry entry :take take :spawner spawner
                                   :ignore-zones? ignore-zones?}
                            want-free (assoc :want-free want-free))))
        id (:id seg)
        why (:reason res)
        n (inc (get-in s [:stops id why] 0))
        taken #(update % :taken (partial merge-with +) (:taken res))
        radius (:radius (:args c))]
    (cond
      (= :continue res) :continue
      (= :spawner why) (do (save! c #(-> % taken
                                         (assoc-in [:segments id] (assoc seg :state :stripped :why :spawner))
                                         (update :segments (fn [m] (reduce (fn [m k] (assoc m k (assoc (get m k) :state :skipped :why :spawner))) m (beyond t id))))))
                           :again)
      (= :done (:status res)) (do (save! c #(-> % taken (assoc-in [:segments id :state] :stripped)
                                                (update :segments strip/merge-segments (filter (fn [g] (<= (strip/gap g (vec3 (:at s))) radius)) (:new-segments res)))))
                                  :again)
      (>= n stop-limit) (do (save! c #(-> % taken (assoc-in [:segments id] (assoc seg :state :skipped :why why)))) :again)
      :else (do (save! c #(cond-> (-> % taken (assoc-in [:stops id why] n))
                            (trip-reasons why) (assoc :trip {:reason why :tool (:tool res)})))
                :again))))

(defn all-skipped
  "{why n} when the segments are all skipped for a failing stop (none stripped or sealed, none left on purpose by a
  spawner policy), else nil."
  [s]
  (let [segs (vals (:segments s))]
    (when (and (seq segs) (every? #(and (= :skipped (:state %)) (not= :spawner (:why %))) segs))
      (frequencies (map :why segs)))))

(defn ^:async strip-step! [c s]
  (let [t (tree s)
        ready (seal-ready s t)
        seg (next-open s t)]
    (cond
      (not (in-site? c s)) (if (trip-due? c s) (await (trip! c s)) (await (enter! c s)))
      (empty? (:segments s)) (await (read-site! c s))
      (trip-due? c s) (await (trip! c s))
      ready (await (seal! c s t ready))
      seg (await (corridor! c s t seg))
      (all-skipped s) (stop! c :all-skipped {:skipped (all-skipped s)})
      :else (do (save! c assoc :phase (if (:haul (:args c)) :haul :done)) :again))))

(defn ^:async haul-chests! [c s]
  (let [res (await (call! c :haul 'jobs.storage.haul (merge {:from (chests c s)} (select-keys (:args c) [:ignore-zones?]))))]
    (cond (= :continue res) :continue
          (failed? res) (child-stop! c (assoc res :child "jobs.storage.haul"))
          :else (do (save! c assoc :phase :done :hauled (:moved res {})) :again))))

(defn ^:async haul! [c s]
  (cond
    (seq (:cooking s)) (let [r (await (smelt-run! c (:cooking s)))]
                         (save! c smelt-saved r {})
                         (if (seq (:cooking r)) :continue :again))
    (:bank s) (let [f (if (empty? (chests c s)) {:reason :base-full} (await (deposit! c s 0)))]
                (cond (= :continue f) :continue
                      f (child-stop! c f)
                      :else (do (save! c dissoc :bank) :again)))
    :else (await (haul-chests! c s))))

(defn restore-steps [{:keys [home spawn-bed]}]
  (cond-> [(if home
             [:home 'jobs.memory.set-place {:name "home" :pos (vec3 home)}]
             [:home 'jobs.memory.forget-place {:name "home"}])]
    spawn-bed (conj [:spawn 'jobs.survival.set-spawn {:bed (vec3 spawn-bed)}])))

(def passed-kinds "Child ending kinds a :child-failed stop ends with as its own." #{:condition-failed :limit-reached :impossible})

(defn result-of
  "What the call reports: the segments by state, what was taken, hauled and smelted."
  [s]
  {:segments (counts s) :taken (:taken s {}) :hauled (:hauled s {}) :smelted (:smelted s {}) :smelt-failed (:smelt-failed s [])})

(defn stop-with!
  "End the call stopped, with fields whose :reason is the cause."
  [c fields]
  (let [{:keys [reason child-kind] :as fields} fields
        data (merge (result-of (site c)) (dissoc fields :reason :child-kind))]
    (ctx/emit! c :strip.site.stopped :warn (assoc (merge data (select-keys fields [:reason]))
                                                  :text (str "strip site " (:site (:args c)) " stopped " (pr-str (:segments data)))))
    (ctx/end c (cond (and (= :child-failed reason) (passed-kinds child-kind)) child-kind
                     (condition-reasons reason) :condition-failed
                     :else :nothing-possible)
          reason data)))

(defn ^:async cleanup
  "However the call ends: :home and the respawn bed go back to :was. Done, or nothing-possible with the failed child's
  reason."
  [c _]
  (let [s (site c)
        f (when (:was s) (await (run-all! c (restore-steps (:was s)))))]
    (cond (= :continue f) :continue
          f (ctx/end c :nothing-possible (:reason f) (select-keys f [:child]))
          :else (ctx/end c :done nil))))

(defn ^:async step! [c]
  (let [s (site c)]
    (if-let [why (:stopping (ctx/mem c))]
      (stop-with! c why)
      (case (:phase s)
        :base (await (base! c s))
        :shears (await (shears! c s))
        :strip (await (strip-step! c s))
        :haul (await (haul! c s))
        :done (or (ctx/done c) :continue)))))

(defn ^:async round [c]
  (when-not (site c)
    (let [{:keys [at base shaft]} (:args c)]
      (save! c (constantly {:at at :base base :shaft shaft :phase :base :segments {} :trips 0 :smelt-failed []}))))
  (await (pace/steps! c #(step! c))))

;; ------------------------------------------------------------------ the contract

(def target [:site])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? (ready/safe-at #(a/cell (:at (:args %)))))

(def fit? ready/fit?)

(defn bad-args
  "Never :bad-args: no :site or :base, and a new site without :at (the site is saved after the start)."
  [c]
  (let [{:keys [at base] id :site} (:args c)]
    (cond (not (and id base)) (contract/never :bad-args "needs :site and :base")
          (and (nil? at) (nil? (site c))) (contract/never :bad-args "a new site needs :at")
          :else (contract/ok :args "site, base and at given"))))

(def can-start? bad-args)

(defn done?
  "The strip is over (phase :done); the segments settled so far are the progress."
  [c]
  (let [s (site c)
        states (map :state (vals (:segments s)))
        data [:units :segments :done (count (filter settled states)) :left (count (remove settled states)) :result (result-of s)]]
    (if (= :done (:phase s))
      (apply contract/ok :stripped (str "strip site " (:site (:args c)) " done " (pr-str (:segments (result-of s)))) data)
      (apply contract/later :stripping "strip site: segments left to strip" data))))

(defn quote-of [c]
  {:uses {:job-time [1 (* 120 (max 1 (count (:segments (site c))))) (settings/get settings ::job-time-s)]}})
