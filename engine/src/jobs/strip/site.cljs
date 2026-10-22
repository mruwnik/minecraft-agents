(ns jobs.strip.site
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.waits :as waits]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.shafts :as shafts]
            [jobs.lib.strip :as strip]
            [jobs.lib.torch :as torch]
            [jobs.lib.trip :as trip]
            [jobs.lib.util :as u]
            [jobs.strip.site-plan :refer [chests]]
            [jobs.strip.site-tree :refer [beyond counts mouth next-open seal-ready settled tree unlinked vec3]]
            [jobs.strip.steps :as steps :refer [reason-of run-all!]]))

(def doc
  "Strip one abandoned mineshaft (a site) of its wood, cobwebs, rails, ore and chest-minecart loot and leave it sealed behind.
  The corridor part of jobs.strip.all, usable alone with any base: it does not build the base, hold shears, smelt or haul.
  Submit it listed with :survive-death? true: the site's state lives in body memory
  :strip/sites {site {:at :shaft :segments :stops :phase :trip ..}}, so a restart, a stop or a death resumes
  where it was. Every step is a registered job run as a child, re-reading the world first; a child that fails ends
  this job stopped. Phases:
  :strip  go down (the :shaft plan with access.stair :through, nil = the nearest usable mineshaft plan, false = walk), read the
          site's corridors from the seen blocks (jobs.lib.strip), then repeat: strip.corridor on the next open segment
          (a branch only after the segment it hangs on; one with an unseen end first), strip.seal at the mouth of a
          segment once it and every segment beyond it are stripped, sealed or skipped. A segment no stripped one reaches is
          :skipped :unlinked. A segment the far end shows longer than it was read is stripped again. The segment holding :at is never
          sealed. A segment that stops ::stop-limit (3) times for one reason is :skipped with the reason. A stop for
          :bag-full :food-short :no-torches :no-tool, or low torches or free slots, is a trip: deposit into the :base plan's chests
          (storage.deposit), obtain torches, get the tool, get food (survival.get-food, once per trip; still hungry with none: :food-short; :ways #{:carried} skips the try), then back down; at most ::max-trips (12).
  The loot still carried at the end is left to jobs.strip.haul. A child that waits on the night or the light is waited out with :continue, not a child failure.
  Needs :site (a new site also :at), else impossible :bad-args. Its safe? is ready/safe-at-by-night :at.
  Ends :done with data {:segments {state n} :taken {class n}},
  or condition-failed (:no-site :no-torches :food-short :base-full, or :child-failed), nothing-possible
  (:unreachable when the walk down found no way, :trips-exhausted, :all-skipped with :skipped {reason n} when no segment was
  stripped or sealed, :child-failed), or :child-failed ending with the kind of a
  child that ended condition-failed, limit-reached or impossible, with the same data and
  :phase, :child and :because (the child's reason) for a child. Event: strip.site.stopped (warn).")

(a/defargs args
  {:site {:doc "the site's name, the key of its memory" :spec a/name? :default nil}
   :at {:doc "[x y z] of a rail, plank or fence of the site the body has seen; kept once given" :spec ::a/pos :default nil}
   :base {:doc "id of the field-base plan whose chests take a trip's deposits; nil: a trip that must deposit stops :base-full" :spec a/name? :default nil}
   :shaft {:doc "id of a mineshaft plan down from the base; nil: the nearest usable one; false: walk" :spec (a/or-of a/name? boolean?) :default nil}
   :take {:doc "classes stripped" :spec (a/coll-of #{:wood :cobweb :rail :ore}) :default [:wood :cobweb :rail :ore]}
   :spawner {:doc "a spawner in a segment: :light lights its spawn box and leaves it, :break lights and digs it, :seal seals the segment's mouth at once (its subtree skipped), :skip leaves it unlit" :spec #{:light :break :seal :skip} :default :light}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 7200 :doc "Seconds a strip of one site may take: base, descent, every segment, trips and the haul." :spec (a/num-in 0 nil)}
   ::radius {:default 64 :doc "How far from :at a corridor belongs to the site." :spec (a/int-in 8 65)}
   ::max-trips {:default 12 :doc "Trips to the base before the strip stops :trips-exhausted." :spec (a/int-in 1 nil)}
   ::stop-limit {:default 3 :doc "Stops of one segment for one reason before it is skipped." :spec (a/int-in 1 nil)}
   ::max-sites {:default 4 :doc "Sites kept in memory; finished ones go first once there are more." :spec (a/int-in 1 nil)}})

(def kind :strip/sites)
(def policy {:cap 1 :ttl :forever})
(def trip-reasons #{:bag-full :food-short :no-torches :no-tool})
(def condition-reasons "Stop reasons that end condition-failed: a fact of the base, the bag or the site. The rest are nothing-possible."
  #{:no-site :no-torches :food-short :base-full})
(defn sites [c] (or (:data (ctx/latest c kind)) {}))
(defn site [c] (get (sites c) (:site (:args c))))

(defn save!
  "Write the site's state as (f state & xs); sites finished earlier go once there are more than ::max-sites."
  [c f & xs]
  (let [id (:site (:args c))
        all (update (sites c) id #(apply f % xs))
        old (take (- (count all) (settings/get settings ::max-sites)) (filter #(= :done (:phase (all %))) (remove #{id} (keys all))))]
    (ctx/remember! c kind (apply dissoc all old) policy)))

(defn stop!
  "Stop with reason and fields."
  [c reason fields]
  (let [s (site c)]
    (ctx/update-mem! c assoc :stopping (merge {:reason reason :phase (:phase s)} fields))
    :again))

(defn child-stop! [c failure]
  (let [e (:ending failure)]
    (stop! c :child-failed {:child (:child failure) :because (reason-of e) :child-kind (:kind e)})))

(defn in-site? [c s]
  (let [{:keys [x y z]} (u/self-pos c)
        [ax ay az] (vec3 (:at s))]
    (and (<= (js/Math.abs (- x ax)) (settings/get settings ::radius)) (<= (js/Math.abs (- z az)) (settings/get settings ::radius)) (<= (js/Math.abs (- y ay)) 4))))

(defn at-site?
  "Within sight of :at: a body anywhere in the radius (the base it just built) cannot read the corridors."
  [c s]
  (let [{:keys [x y z]} (u/self-pos c)
        [ax ay az] (vec3 (:at s))]
    (<= (+ (* (- x ax) (- x ax)) (* (- y ay) (- y ay)) (* (- z az) (- z az))) 36)))

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
      above? (let [f (await (run-all! c [[:stair 'jobs.access.stair {:dir :down :heading heading :y to-y :through id :need-drop true
                                                                       :ignore-zones? (:ignore-zones? (:args c))}]]))]
               (cond (= :continue f) :continue f (child-stop! c f) :else :again))
      :else :again)))

(defn ^:async read-site! [c s]
  (await (look/look-around! c))
  (let [p (:primitives c)
        blocks (look/seen-blocks p {:match #(or (strip/fence? %) (strip/planks? %) (strip/rail? %)) :radius (settings/get settings ::radius) :max 1024})
        segs (filterv #(<= (strip/gap % (vec3 (:at s))) (settings/get settings ::radius)) (strip/segments blocks {:block-at (access/seen-at p)}))]
    (if (empty? segs)
      (stop! c :no-site {:at (:at s)})
      (do (save! c assoc :segments (strip/merge-segments {} segs)) :again))))

(defn trip-due? [c s]
  (let [p (:primitives c)]
    (or (:trip s) (< (torch/torches-held p) (trip/torch-low)) (< (u/free-slots p) (trip/want-free)))))

(defn base-chests [c] (chests c (:base (:args c))))

(defn ^:async deposit! [c i]
  (await (run-all! c [[:deposit 'jobs.storage.deposit (merge {:chest (nth (base-chests c) i) :keep {"torch" 64} :make-chests? false}
                                                             (select-keys (:args c) [:ignore-zones?]))]])))

(defn ^:async trip!
  "At the base: deposit into the next base chest while the bag is short of room, obtain torches, get the lost tool, food; then back down."
  [c s]
  (let [p (:primitives c)
        {:keys [reason tool deposits] :or {deposits 0}} (:trip s)
        cs (base-chests c)
        short? (< (u/free-slots p) (trip/want-free))
        step (cond
               (or short? (and (= :bag-full reason) (zero? deposits))) :deposit
               (< (torch/torches-held p) (trip/torch-low)) :torches
               (and (= :no-tool reason) (not (:tool-done (:trip s)))) :tool
               (trip/food-short? c) :food)]
    (case step
      :deposit (if (>= deposits (count cs))
                 (stop! c :base-full {})
                 (let [f (await (deposit! c deposits))]
                   (cond (= :continue f) :continue
                         f (child-stop! c f)
                         :else (do (save! c update-in [:trip :deposits] (fnil inc 0)) :again))))
      :torches (let [f (await (run-all! c [[:torches 'jobs.items.obtain {:item "torch" :count (need/carry-count c :torches ["torch"] (- (trip/torch-stock) (torch/torches-held p))) :explore false}]]))]
                 (cond (= :continue f) :continue f (child-stop! c f)
                       (< (torch/torches-held p) (trip/torch-low)) (stop! c :no-torches {})
                       :else :again))
      :tool (let [f (await (run-all! c [[:tool 'jobs.items.get-tool {:kind tool}]]))]
              (cond (= :continue f) :continue f (child-stop! c f) :else (do (save! c assoc-in [:trip :tool-done] true) :again)))
      :food (if (or (:food-done (:trip s)) (empty? (need/fetch-how c)))
              (stop! c :food-short {})
              (let [f (await (run-all! c [[:food 'jobs.survival.get-food {}]]))]
                (if (= :continue f)
                  :continue
                  (do (save! c assoc-in [:trip :food-done] true) :again))))
      (if (>= (:trips s 0) (settings/get settings ::max-trips))
        (stop! c :trips-exhausted {:trips (:trips s)})
        (do (save! c #(-> % (dissoc :trip) (update :trips (fnil inc 0)))) :again)))))

(defn ^:async seal! [c s t id]
  (let [seg (get (:segments s) id)
        {:keys [at side]} (mouth s t seg)
        f (await (run-all! c [[:seal 'jobs.strip.seal (cond-> {:at at :axis (:axis seg) :side side :ignore-zones? (:ignore-zones? (:args c))}
                                                (some? (:ways (:args c))) (assoc :ways (:ways (:args c))))]]))]
    (cond (= :continue f) :continue
          f (child-stop! c f)
          :else (do (save! c assoc-in [:segments id :state] :sealed) :again))))

(defn ^:async corridor! [c s t seg]
  (let [{:keys [entry]} (mouth s t seg)
        {:keys [take spawner ignore-zones?]} (:args c)
        e (waits/waiting-out c (await (ctx/call c :corridor 'jobs.strip.corridor
                                                {:segment seg :entry entry :take take :spawner spawner :ignore-zones? ignore-zones?})))
        res (merge (:data e) (when (map? (:cause e)) (:cause e)))
        id (:id seg)
        why (when-not (= :done (:kind e)) (reason-of e))
        n (inc (get-in s [:stops id why] 0))
        taken #(update % :taken (partial merge-with +) (:taken res))
        radius (settings/get settings ::radius)]
    (cond
      (= :continue e) :continue
      (= :spawner why) (do (save! c #(-> % taken
                                         (assoc-in [:segments id] (assoc seg :state :stripped :why :spawner))
                                         (update :segments (fn [m] (reduce (fn [m k] (assoc m k (assoc (get m k) :state :skipped :why :spawner))) m (beyond t id))))))
                           :again)
      (= :done (:kind e)) (do (save! c #(-> % taken
                                            (update :segments strip/merge-segments (filter (fn [g] (<= (strip/gap g (vec3 (:at s))) radius)) (:new-segments res)))
                                            (update-in [:segments id] (fn [m] (assoc m :state (if (= (select-keys seg [:from :to]) (select-keys m [:from :to])) :stripped :open))))))
                                  :again)
      (>= n (settings/get settings ::stop-limit)) (do (save! c #(-> % taken (assoc-in [:segments id] (assoc seg :state :skipped :why why)))) :again)
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
      (and (empty? (:segments s)) (not (at-site? c s))) (await (enter! c s))
      (empty? (:segments s)) (await (read-site! c s))
      (trip-due? c s) (await (trip! c s))
      ready (await (seal! c s t ready))
      seg (await (corridor! c s t seg))
      (seq (unlinked s t)) (do (save! c update :segments #(reduce (fn [m k] (assoc m k (assoc (get m k) :state :skipped :why :unlinked))) % (unlinked s t))) :again)
      (all-skipped s) (stop! c :all-skipped {:skipped (all-skipped s)})
      :else (do (save! c assoc :phase :done) :again))))

(defn result-of
  "What the call reports: the segments by state and what was taken."
  [s]
  {:segments (counts s) :taken (:taken s {})})

(defn stop-with!
  "End the call stopped, with fields whose :reason is the cause."
  [c fields]
  (let [{:keys [reason child-kind] :as fields} fields
        data (merge (result-of (site c)) (dissoc fields :reason :child-kind))]
    (ctx/emit! c :strip.site.stopped :warn (assoc (merge data (select-keys fields [:reason]))
                                                  :text (str "strip site " (:site (:args c)) " stopped " (pr-str (:segments data)))))
    (ctx/end c (cond (and (= :child-failed reason) (steps/passed-kinds child-kind)) child-kind
                     (condition-reasons reason) :condition-failed
                     :else :nothing-possible)
          reason data)))

(defn ^:async step! [c]
  (let [s (site c)]
    (if-let [why (:stopping (ctx/mem c))]
      (stop-with! c why)
      (case (:phase s)
        :strip (await (strip-step! c s))
        :done (or (ctx/done c) :continue)))))

(defn ^:async round [c]
  (when-not (site c)
    (let [{:keys [at shaft]} (:args c)]
      (save! c (constantly {:at at :shaft shaft :phase :strip :segments {} :trips 0}))))
  (await (pace/steps! c #(step! c))))

;; ------------------------------------------------------------------ the contract

(def target [:site])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? (ready/safe-at-by-night #(a/cell (or (:at (:args %)) (:at (site %))))))

(def fit? ready/fit?)

(def ways need/fetch-ways)

(defn bad-args
  "Never :bad-args: no :site, and a new site without :at (the site is saved after the start)."
  [c]
  (let [{:keys [at] id :site} (:args c)]
    (cond (not id) (contract/never :bad-args "needs :site")
          (and (nil? at) (nil? (site c))) (contract/never :bad-args "a new site needs :at")
          :else (contract/ok :args "site and at given"))))

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

(def meta {:tags [:mine]})
