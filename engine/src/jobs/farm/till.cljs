(ns jobs.farm.till
  (:require [engine.contract :as contract]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.waits :as waits]
            [jobs.lib.child :as child]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.blocks :as b]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.job-common :as common]
            [jobs.lib.gate :as gate]
            [jobs.lib.need :as need]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.field-area :as fa]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.places :as places]
            [jobs.lib.toll-cells :as tc]))

(def doc
  "Hoe dirt, grass_block, dirt_path, coarse_dirt or rooted_dirt (two uses: dirt first) into farmland over a set of ground cells, given as the :area: a box {:min :max} or the
  square of a circle {:pos :radius} at its y (at most 256 cells). Water nearby is not its concern.
  Needs a hoe: the call's :use {:tools [..]} names the ones it may use (default every carried hoe); with none carried it
  fetches one (jobs.lib.need, from the call's :ways; a failed fetch ends the call with its ending as the :because, tilled and skipped
  in the data), with :ways #{:carried} the call is refused {:reason :no-tool :kind \"hoe\"}. Ground cover over a cell (grass,
  ferns, flowers, snow layer) is dug first with a jobs.blocks.dig child; a cover dig that does not clear it counts a try.
  Cells are skipped with a reason: :not-tillable, :covered (something else above), :not-permitted (zone rules),
  :unreachable, :gone, :refused (the hoe failed twice), :no-hoe (the hoe is gone) or :cover-stuck (two failed cover
  digs).
  With :for-plan (the id of the plan the cells belong to) that plan's own footprint does not refuse a cell. Zones
  and the footprints of other plans are checked when a cell is chosen and again before the hoe or cover dig.
  Refused before it starts: impossible {:reason :bad-args :why}; failed conditions
  {:reason :no-zones} (no zone list read, unless :ignore-zones?), {:reason :refused :zones ..} (every tillable cell is
  refused) and the hoe above.
  Ends with data {:tilled n :skipped {pos reason}}: done when it tilled some (the least accepted is :least, cells) or had
  nothing to till; with none tilled and cells skipped, condition-failed for :not-tillable, :covered, :not-permitted, :gone
  , :no-hoe and :refused, else nothing-possible (:unreachable, :cover-stuck), the cause the one skip reason or
  :nothing-tilled when mixed. done? is :ok when every cell is farmland and no child is under way; it counts the cells
  tilled this call and the cells left.")

(a/defargs args
  {:area {:doc "the ground cells: a box {:min :max} (corners inclusive, any order, each [x y z] or {:x :y :z}) or a circle {:pos :radius}, the square |dx|,|dz| <= radius at pos's y; at most 256 cells" :spec fa/spec :default nil}
   :for-plan {:doc "id of the plan whose work this is: its own footprint does not refuse; nil: every plan's footprint does" :spec a/name? :default nil}
   :ignore-zones? :shared})

(def ways need/fetch-ways)

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds the call may take (a guess until measured)." :spec (a/num-in 0 nil)}
   ::max-cells {:default 256 :doc "Cells one call looks at before it gives the round back." :spec (a/int-in 0 nil)}
   ::max-tries {:default 2 :doc "Attempts before the job gives up." :spec (a/int-in 0 nil)}})
(defn max-cells [] (settings/get settings ::max-cells))
(def loosened
  "Grounds a hoe first turns into dirt (a second use makes farmland)."
  #{"coarse_dirt" "rooted_dirt"})
(def tillable (into #{"dirt" "grass_block" "dirt_path"} loosened))
(def ground-cover
  "What lies on a cell and is dug first: the plants, small flowers and snow layer a placement clears (b/clearable) and the tall flowers."
  (into b/clearable #{"short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "snow" "sunflower" "lilac" "rose_bush" "peony"}))
(defn max-tries [] (settings/get settings ::max-tries))

(defn span
  "[lo..hi] inclusive of two numbers in either order."
  [a b]
  (range (min a b) (inc (max a b))))

(defn cells
  "The ground cells [{:x :y :z} ...] of the :area. Throws ex-info for no box or circle, or over max-cells cells."
  [{:keys [area]}]
  (let [{lo :min hi :max :as b} (fa/box area)]
    (when-not b (throw (ex-info "till needs :area, a box or a circle" {})))
    (let [n (fa/cell-count b)]
      (when (> n (max-cells))
        (throw (ex-info (str "till covers " n " cells, at most " (max-cells)) {:cells n})))
      (vec (for [x (span (:x lo) (:x hi)) y (span (:y lo) (:y hi)) z (span (:z lo) (:z hi))] {:x x :y y :z z})))))

(def target [:area])
(def defaults {:use {:tools :till} :spend {:job-time ::job-time-s}})
(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)

(defn listed
  "The hoes the call names (:use {:tools [..]}) as a set, or nil when it names none (the engine's default list is not given)."
  [c]
  (let [l (get-in (:args c) [:use :tools])]
    (when (vector? l) (set l))))

(defn hoe
  "The name of a carried hoe the call may use, or nil."
  [c]
  (let [only (listed c)]
    (some #(when (and (.endsWith (:name %) "_hoe") (or (nil? only) (only (:name %)))) (:name %)) (u/inventory (:primitives c)))))

(defn fetch-opts
  "The limits {:how} for fetching a hoe, nil when the call's ways leave only :carried."
  [c]
  (when-let [how (seq (need/fetch-how c))]
    {:how (set how)}))

(defn hoe-wanted
  "What the fetch gets: a listed hoe by name, else any hoe."
  [c]
  (if-let [l (listed c)] {:item (first (sort l))} {:kind "hoe"}))

(defn pending
  "[[pos block-name-or-nil] ...] of the cells not skipped and not farmland."
  [c]
  (let [skipped (:skipped (ctx/mem c) {})]
    (->> (cells (:args c))
         (remove #(contains? skipped %))
         (map (fn [pos] [pos (u/seen-name (:primitives c) pos)]))
         (remove (fn [[_ n]] (= "farmland" n)))
         vec)))

(defn permitted?
  "Whether action at pos passes the zone rules, for the plan the job works when it has :for-plan."
  [c action pos]
  (gate/allowed? c :till.declined "till" action pos {:except (:for-plan (:args c))}))

(defn survey
  "Walks cs in order, reading each cell once, and stops at the first cell the job may work. {:todo? any cell not
  farmland and not skipped, :work? a tillable cell no zone, claim or plan refuses, :verdicts [refusals]}. Without
  refuse (a function from pos to a refusal or nil) no cell counts as refused."
  [c cs refuse]
  (let [skipped (:skipped (ctx/mem c) {})
        p (:primitives c)]
    (reduce (fn [acc pos]
              (let [n (when-not (contains? skipped pos) (u/seen-name p pos))
                    v (when (and (not (contains? skipped pos)) (not= "farmland" n) (or (nil? n) (tillable n))) (when refuse (refuse pos)))]
                (cond
                  (contains? skipped pos) acc
                  (= "farmland" n) acc
                  v (-> acc (assoc :todo? true) (update :verdicts conj v))
                  (or (nil? n) (tillable n)) (reduced (assoc acc :todo? true :work? true))
                  :else (assoc acc :todo? true))))
            {:verdicts []} cs)))

(def no-hoe {:reason :no-tool :kind "hoe"})

(defn bad-args
  "Never :bad-args unless the cells can be read from the args (a box or circle within the limit)."
  [c]
  (let [cs (try (cells (:args c))
                (catch :default e (ex-message e)))]
    (if (string? cs)
      (contract/never :bad-args cs :why cs)
      (contract/ok :args "cells given"))))

(defn problem
  "Why the call cannot start now, a cause map, or nil: no zone list read (unless :ignore-zones?), every tillable cell
  refused by a zone, claim or plan, and cells to till with no hoe that may be fetched.
  Walks the cells lazily and stops at the first one that needs work, reading each cell at most once."
  [c]
  (when-let [cs (try (cells (:args c)) (catch :default _ nil))]
    (let [in (access/zone-input c {:except (:for-plan (:args c))})
          refuse (when-not (:ignore-zones? in)
                   (fn [pos] (let [v (rules/social-verdict :dig (assoc in :cell (access/cell pos)))]
                               (when (gate/refused? v) v))))]
      (or (common/no-zones c)
          (let [{:keys [todo? work? verdicts]} (survey c cs refuse)]
            (cond
              (and (seq verdicts) (not work?)) (assoc (access/refusal-fields verdicts) :reason :refused)
              (and todo? (nil? (hoe c)) (nil? (fetch-opts c))) no-hoe))))))

(def can-start? (cond/and bad-args (checks/refusing :tillable "a cell to till, a hoe and zones allow" problem)))

(declare result-of)

(defn done?
  "Every cell is farmland (a skipped cell is not) and no child is under way; the cells tilled this call and the cells
  left to till."
  [c]
  (let [cs (try (cells (:args c)) (catch :default _ nil))
        open (count (remove #(= "farmland" (u/seen-name (:primitives c) %)) cs))
        {:keys [tilled] :as result} (result-of c)
        data [:units :cells :done tilled :left (if cs (count (pending c)) 0) :result result]]
    (cond
      (or (empty? cs) (pos? open)) (apply contract/later :to-till (str "till: " open " cells to till") data)
      (checks/child-under-way? c) (apply contract/later :wrapping-up "till: every cell is farmland, wrapping up" data)
      :else (apply contract/ok :tilled (str "tilled " tilled ", every cell is farmland") data))))

(defn quote-of [c]
  (let [cs (try (cells (:args c)) (catch :default _ nil))
        p (:primitives c)
        names (remove #{"farmland"} (map #(u/seen-name p %) cs))
        n (count names)
        uses (+ n (count (filter loosened names)))
        h (hoe c)]
    {:uses (cond-> {:job-time [(* 0.5 n) (* 2 n) (* 10 n)]} (and h (pos? n)) (assoc :wear {h [uses uses uses]}))
     :done [0 n n]}))

(defn skip!
  "Record the cells as skipped with reason and emit one :till.skipped each (debug: the ending lists them all)."
  [c poss reason]
  (ctx/update-mem! c update :skipped #(into (or % {}) (map (fn [p] [p reason])) poss))
  (doseq [pos poss]
    (ctx/emit! c :till.skipped :debug {:pos pos :reason reason
                                      :text (str "skipped " (pr-str pos) ": " (name reason))})))

(defn bump!
  "Count a failed try on pos; at max-tries skip it with reason."
  [c pos reason]
  (let [n (inc (get-in (ctx/mem c) [:cell-tries pos] 0))]
    (if (>= n (max-tries))
      (skip! c [pos] reason)
      (ctx/update-mem! c assoc-in [:cell-tries pos] n))))

(defn cover-args
  "The jobs.blocks.dig args for the ground cover at pos: no tool needed, the drop left, every dig hazard taken (a
  plant over the ground)."
  [c pos]
  (merge (select-keys (:args c) [:for-plan :ignore-zones?])
         {:pos pos :collect false :need-drop false :accept #{:fluid-adjacent :falling-block}}))

(defn nearest
  "The [pos name] of todo nearest the body."
  [c todo]
  (let [me (u/self-pos c)]
    (apply min-key #(u/dist me (first %)) todo)))

(def condition-skips
  "Skip reasons that name a cell or a hoe that is not there to use; any other skip is nothing-possible."
  #{:not-tillable :covered :not-permitted :gone :no-hoe :refused})

(defn result-of
  "The data of the call's ending: what is tilled and skipped so far."
  [c]
  (let [mem (ctx/mem c)]
    {:tilled (count (:tilled mem)) :skipped (:skipped mem {})}))

(defn conclude!
  "The ending of a run with no cell left to work (job-common/conclude!)."
  [c]
  (let [{:keys [tilled skipped] :as result} (result-of c)]
    (common/conclude! c {:verb "tilled" :mixed :nothing-tilled :condition-skips condition-skips
                         :texts {:no-hoe "no hoe, nothing tilled"}}
                      tilled skipped result)))

(defn ^:async till-step!
  "One step: skip what cannot be tilled, else walk to the nearest cell, clear ground cover above it or use the hoe on it."
  [c]
  (let [p (:primitives c)
        todo (pending c)
        bad (into [] (comp (filter (fn [[_ n]] (and (some? n) (not (tillable n))))) (map first)) todo)]
    (when (seq bad) (skip! c bad :not-tillable))
    (let [unpermitted (into [] (comp (map first) (remove (set bad)) (remove #(permitted? c :dig %))) todo)
          _ (when (seq unpermitted) (skip! c unpermitted :not-permitted))
          bad? (into (set bad) unpermitted)
          cands (remove (fn [[pos _]] (bad? pos)) todo)
          tool (hoe c)]
      (cond
        (empty? cands) (conclude! c)

        (nil? tool)
        (do (skip! c (map first cands) :no-hoe) :again)

        :else
        (let [[target _] (nearest c cands)
              w (await (common/walk! c target 3 {:tolls (tc/walk-tolls c (:pos (places/parse-pos target)))}))]
          (case w
            :continue :continue
            :blocked (do (bump! c target :unreachable) :again)
            (let [above-pos (update target :y inc)
                  above (u/seen-name p above-pos)]
              (cond
                (not (permitted? c :dig target))
                (do (skip! c [target] :not-permitted) :again)

                (and (ground-cover above) (not (permitted? c :dig above-pos)))
                (do (skip! c [target] :not-permitted) :again)

                (ground-cover above)
                (let [e (waits/waiting-out c (await (ctx/call c :cover 'jobs.blocks.dig (cover-args c above-pos))))]
                  (cond
                    (= :continue e) :continue
                    (not (and (= :done (:kind e)) (#{:dug :already-clear} (:reason (:data e)))))
                    (do (bump! c target :cover-stuck) :again)
                    :else :again))

                (not (or (nil? above) (bs/air above)))
                (do (skip! c [target] :covered) :again)

                :else
                (let [res (await (child/use-on! c target tool "up"))
                      {:keys [status raw]} res]
                  (if (= :continue res)
                    :continue
                    (do (cond
                          (and (= :used status) (= "farmland" (some-> (.-after raw) .-name)))
                          (ctx/update-mem! c #(-> % (update :tilled (fnil conj #{}) target) (update :cell-tries dissoc target)))

                          (and (= :used status) (loosened (some-> (.-before raw) .-name)) (= "dirt" (some-> (.-after raw) .-name)))
                          nil

                          (= :missing status) (skip! c [target] :gone)
                          (= :no-item status) (bump! c target :no-hoe)
                          :else (bump! c target :refused))
                        :again)))))))))))

(defn ^:async hoe!
  "Get a hoe when none may be used, cells are left and the ways allow a fetch: nil when one is carried (or came), :continue while the fetch waits
  on the world, else the ending of the call: done when enough is tilled already, else the fetch's kind and cause."
  [c]
  (when (and (nil? (hoe c)) (seq (pending c)) (fetch-opts c))
    (let [e (await (need/tool! c (hoe-wanted c) (fetch-opts c)))]
      (cond
        (= :continue e) :continue
        (= :done (:kind e)) nil
        (common/enough? c (count (:tilled (ctx/mem c))) (count (cells (:args c)))) (conclude! c)
        :else (ctx/end c (:kind e) (:cause e) (result-of c))))))

(defn ^:async round
  "The whole attempt: fetch a hoe when none is carried, then loop the steps (skip what cannot be tilled, walk to the
  nearest cell, clear ground cover above it or use the hoe on it) until the cells are done; :continue only while a walk
  or fetch waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s []
                          (or (await (hoe! c))
                              (await (till-step! c)))))))
