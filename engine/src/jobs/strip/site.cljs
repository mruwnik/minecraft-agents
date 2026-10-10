(ns jobs.strip.site
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.child :as child]
            [jobs.lib.danger :as danger]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
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
          (storage.deposit), obtain torches, get the tool, then back down; at most :max-trips.
  :haul   storage.haul from the base chests to the main storage (not with :haul false).
  :restore  :home and the respawn bed back to :was (also on any stop). The torches the corridor took back are
          carried.
  Ends {:status :done :segments {state n} :taken {class n} :hauled {item n}}, or {:status :stopped :reason r :phase p}:
  :child-failed (:child the job, :because its reason), :bad-plan, :no-site, :no-shears, :unreachable, :base-full,
  :food-short, :trips-exhausted. The check waits :danger in the strip phase. Events: strip.site.done (info), strip.site.stopped (warn).")

(a/defargs args
  {:site {:doc "the site's name, the key of its memory" :spec a/name? :default nil}
   :at {:doc "[x y z] of a rail, plank or fence of the site the body has seen; kept once given" :spec ::a/pos :default nil}
   :base {:doc "id of the field-base plan (blueprint field-base)" :spec a/name? :default nil}
   :shaft {:doc "id of a mineshaft plan down from the base; nil: the nearest usable one; false: walk" :spec (a/or-of a/name? boolean?) :default nil}
   :radius {:doc "how far from :at a corridor belongs to the site" :spec (a/int-in 8 65) :default 64}
   :take {:doc "classes stripped" :spec (a/coll-of #{:wood :cobweb :rail :ore}) :default [:wood :cobweb :rail :ore]}
   :spawner {:doc "a spawner in a segment: :break lights and digs it, :seal seals the segment's mouth at once (its subtree skipped), :skip leaves it" :spec #{:break :seal :skip} :default :skip}
   :seal-with {:doc "blocks to seal with, the first carried; nil: strip.seal's own" :spec (a/coll-of a/item?) :default nil}
   :haul {:doc "false leaves the loot in the base chests" :spec boolean? :default true}
   :want-free {:doc "free slots under which a trip turns back; nil: jobs.lib.trip/want-free" :spec (a/int-in 0 36) :default nil}
   :max-trips {:doc "trips to the base before it stops :trips-exhausted" :spec (a/int-in 0 100) :default 12}
   :stop-limit {:doc "stops of one segment for one reason before it is skipped" :spec (a/int-in 1 10) :default 3}
   :danger-check {:doc "wait while a real danger is near in the strip phase" :spec boolean? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims" :spec boolean? :default false}})

(def kind :strip/sites)
(def policy {:cap 1 :ttl :forever})
(def max-sites 4)
(def trip-reasons #{:bag-full :food-short :no-torches :no-tool})
(def settled #{:stripped :sealed :skipped})

(defn sites [c] (or (:data (ctx/latest c kind)) {}))
(defn site [c] (get (sites c) (:site (:args c))))

(defn save!
  "Write the site's state as (f state & xs); sites finished earlier go once there are more than max-sites."
  [c f & xs]
  (let [id (:site (:args c))
        all (update (sites c) id #(apply f % xs))
        old (take (- (count all) max-sites) (filter #(= :done (:phase (all %))) (remove #{id} (keys all))))]
    (ctx/remember! c kind (apply dissoc all old) policy)))

(defn vec3 [v] (let [{:keys [x y z]} (a/cell v)] [x y z]))

(defn check [c]
  (or (not (:danger-check (:args c)))
      (not= :strip (:phase (site c)))
      (not (danger/danger-near? (:primitives c) (danger/hostile-radius) (danger/ranged-radius)))
      (ctx/wait c {:reason :danger})))

(defn counts [s] (frequencies (map :state (vals (:segments s)))))

(defn ^:async call!
  "Run a child to its end: :continue while it waits on the world, else its result (a declined child is {:status :stopped}
  with its wait reason; a danger decline is :continue, the check waits for it)."
  [c slot job args]
  (let [r (await (child/run! c slot job args))]
    (cond (= :continue r) :continue
          (and (= :declined r) (= :danger (:reason (b/child-wait c slot job args)))) :continue
          (= :declined r) {:status :stopped :reason (or (:reason (b/child-wait c slot job args)) :declined)}
          :else (or (ctx/child-result c slot) {}))))

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

(defn stop!
  "Stop with reason and fields. The home and respawn are put back first (a restore pass of its own, as the restore phase does)."
  [c reason fields]
  (let [s (site c)]
    (ctx/update-mem! c assoc :stopping (merge {:reason reason :phase (:phase s)} fields))
    :again))

(defn child-stop! [c failure] (stop! c :child-failed {:child (:child failure) :because (:reason failure)}))

(defn spot [plan name]
  (some (fn [[k v]] (when (str/ends-with? k (str "/" name)) v)) (:spots plan)))

(defn plan-of [c s] (known/plan c (:base s)))
(defn chests [c s] (let [plan (plan-of c s)] (vec (keep #(spot plan %) ["chest-1" "chest-3"]))))
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
        w (await (near/go-near! c (if above? top (vec3 (:at s))) (if above? 1 2) {:escalate (not above?)}))]
    (cond
      (= :partial w) :continue
      (= :blocked w) (stop! c :unreachable {:cell (if above? top (vec3 (:at s)))})
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

(defn ^:async trip!
  "At the base: deposit into the next base chest while the bag is short of room, obtain torches, get the lost tool; then back down."
  [c s]
  (let [p (:primitives c)
        {:keys [reason tool deposits] :or {deposits 0}} (:trip s)
        cs (chests c s)
        short? (< (u/free-slots p) (or (:want-free (:args c)) (trip/want-free)))
        step (cond
               (or short? (and (= :bag-full reason) (zero? deposits))) :deposit
               (< (torch/torches-held p) (trip/torch-low)) :torches
               (and (= :no-tool reason) (not (:tool-done (:trip s)))) :tool
               (trip/food-short? c) :food)]
    (case step
      :deposit (if (>= deposits (count cs))
                 (stop! c :base-full {})
                 (let [f (await (run-all! c [[:deposit 'jobs.storage.deposit (merge {:chest (nth cs deposits) :keep {"torch" 64} :make-chests? false}
                                                                                    (select-keys (:args c) [:ignore-zones?]))]]))]
                   (cond (= :continue f) :continue
                         f (child-stop! c f)
                         :else (do (save! c update-in [:trip :deposits] (fnil inc 0)) :again))))
      :torches (let [f (await (run-all! c [[:torches 'jobs.items.obtain {:item "torch" :count (- (trip/torch-stock) (torch/torches-held p))}]]))]
                 (cond (= :continue f) :continue f (child-stop! c f)
                       (< (torch/torches-held p) (trip/torch-low)) (stop! c :no-torches {})
                       :else :again))
      :tool (let [f (await (run-all! c [[:tool 'jobs.items.get-tool {:kind tool}]]))]
              (cond (= :continue f) :continue f (child-stop! c f) :else (do (save! c assoc-in [:trip :tool-done] true) :again)))
      :food (stop! c :food-short {})
      (if (> (:trips s 0) (:max-trips (:args c)))
        (stop! c :trips-exhausted {:trips (:trips s)})
        (do (save! c #(-> % (dissoc :trip) (update :trips (fnil inc 0)))) :again)))))

(defn ^:async seal! [c s t id]
  (let [seg (get (:segments s) id)
        {:keys [at side]} (mouth s t seg)
        f (await (run-all! c [[:seal 'jobs.strip.seal (cond-> {:at at :axis (:axis seg) :side side :ignore-zones? (:ignore-zones? (:args c))}
                                                        (:seal-with (:args c)) (assoc :with (:seal-with (:args c))))]]))]
    (cond (= :continue f) :continue
          f (child-stop! c f)
          :else (do (save! c assoc-in [:segments id :state] :sealed) :again))))

(defn ^:async corridor! [c s t seg]
  (let [{:keys [entry]} (mouth s t seg)
        {:keys [take spawner want-free stop-limit ignore-zones? danger-check]} (:args c)
        res (await (call! c :corridor 'jobs.strip.corridor
                          (cond-> {:segment seg :entry entry :take take :spawner spawner
                                   :danger-check danger-check :ignore-zones? ignore-zones?}
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
      :else (do (save! c assoc :phase (if (:haul (:args c)) :haul :restore)) :again))))

(defn ^:async haul! [c s]
  (let [res (await (call! c :haul 'jobs.storage.haul (merge {:from (chests c s)} (select-keys (:args c) [:ignore-zones?]))))]
    (cond (= :continue res) :continue
          (failed? res) (child-stop! c (assoc res :child "jobs.storage.haul"))
          :else (do (save! c assoc :phase :restore :hauled (:moved res {})) :again))))

(defn restore-steps [{:keys [home spawn-bed]}]
  (cond-> [(if home
             [:home 'jobs.memory.set-place {:name "home" :pos (vec3 home)}]
             [:home 'jobs.memory.forget-place {:name "home"}])]
    spawn-bed (conj [:spawn 'jobs.survival.set-spawn {:bed (vec3 spawn-bed)}])))

(defn finish! [c status fields]
  (let [s (site c)
        res (merge {:status status :segments (counts s) :taken (:taken s {}) :hauled (:hauled s {})} fields)]
    (ctx/result! c res)
    (ctx/emit! c (if (= :done status) :strip.site.done :strip.site.stopped) (if (= :done status) :info :warn)
               (assoc res :text (str "strip site " (:site (:args c)) " " (name status) " " (pr-str (counts s)))))
    :done))

(defn ^:async restore! [c s]
  (let [f (when (:was s) (await (run-all! c (restore-steps (:was s)))))]
    (cond (= :continue f) :continue
          f (child-stop! c f)
          :else (do (save! c assoc :phase :done) :again))))

(defn ^:async stopping! [c {:keys [reason] :as why}]
  (let [s (site c)
        f (when (and (:was s) (not= :restore (:phase why))) (await (run-all! c (restore-steps (:was s)))))]
    (if (= :continue f)
      :continue
      (finish! c :stopped (assoc why :restore-failed (some-> f :reason))))))

(defn ^:async step! [c]
  (let [s (site c)]
    (if-let [why (:stopping (ctx/mem c))]
      (await (stopping! c why))
      (case (:phase s)
        :base (await (base! c s))
        :shears (await (shears! c s))
        :strip (await (strip-step! c s))
        :haul (await (haul! c s))
        :restore (await (restore! c s))
        :done (finish! c :done {})))))

(defn ^:async round [c]
  (let [{:keys [site at base shaft]} (:args c)]
    (cond
      (not (and site base)) (finish! c :stopped {:reason :bad-args :why "needs :site and :base"})
      (and (not (jobs.strip.site/site c)) (nil? at)) (finish! c :stopped {:reason :bad-args :why "a new site needs :at"})
      :else (do (when-not (jobs.strip.site/site c)
                  (save! c (constantly {:at at :base base :shaft shaft :phase :base :segments {} :trips 0})))
                (await (pace/steps! c #(step! c)))))))
