(ns jobs.build.path
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.access :as access]
            [jobs.lib.job-common :as common]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.gate :as gate]
            [jobs.lib.need :as need]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.util :as u]
            [jobs.lib.pace :as pace]
            [jobs.lib.places :as places]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.toll-cells :as tc]))

(def doc
  "Shovel grass_block, dirt, coarse_dirt, podzol, mycelium or rooted_dirt into dirt_path over a set of ground cells,
  given as the box :from/:to or the square :center/:radius (at most 256 cells).
  Needs a shovel: the call's :use {:tools [..]} names the ones it may use (default every carried shovel); with none
  carried it fetches one (jobs.lib.need, :fetch); a failed fetch ends condition-failed {:reason :no-tool :kind \"shovel\"}
  with the fetch's ending as the :because, and with :fetch false the call is refused the same.
  Ground cover over a cell (grass, ferns, snow layer) is dug first with a jobs.blocks.dig child. If that child
  declines or gives up, it counts a try.
  Cells are skipped with a reason: :not-pathable, :covered (something else above), :not-permitted (zone rules),
  :unreachable, :gone, :refused (the shovel failed twice), :no-shovel or :cover-stuck (two failed cover digs). A cell
  that already is a path is left alone and not counted.
  Zones and the footprints of plans are checked when a cell is chosen and again before the shovel or cover dig.
  Refused before it starts: {:reason :bad-args :why} (impossible: the cells args are wrong), as can-start? refusals
  {:reason :no-zones} (no zone list read, unless :ignore-zones?), {:reason :refused :zones ..} (every pathable cell is
  refused) and the shovel above.
  Ends with data {:pathed n :skipped {pos reason}}: done when it pathed some (the least accepted is :least, cells) or had
  nothing to path; with none pathed and cells skipped, condition-failed for :not-pathable, :covered, :not-permitted, :gone
  and :no-shovel, else nothing-possible (:unreachable, :refused, :cover-stuck), the cause the one skip reason or
  :nothing-pathed when mixed. Already done when every cell is a path; done? counts the cells pathed this call and the
  cells left.")

(a/defargs args
  {:from {:doc "box corner (inclusive); with :to, any order" :spec ::a/pos :default nil}
   :to {:doc "opposite box corner (inclusive)" :spec ::a/pos :default nil}
   :center {:doc "centre of a square of cells at its y; with :radius" :spec ::a/pos :default nil}
   :radius {:doc "the square covers |dx|,|dz| <= radius, 0 to 7" :spec (a/int-in 0 7) :default nil}
   :fetch {:doc "get a shovel when none is carried (jobs.lib.need): true, a set of kinds or a map of limits; false refuses the call :no-tool" :spec need/option? :default true}
   :ignore-zones? :shared})

(a/defargs settings
  {::max-cells {:default 256 :doc "Cells one call looks at before it gives the round back." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 600 :doc "Seconds one call may take." :spec (a/int-in 1 nil)}
   ::max-tries {:default 2 :doc "Attempts before the job gives up." :spec (a/int-in 0 nil)}})
(defn max-cells [] (settings/get settings ::max-cells))
(def pathable #{"grass_block" "dirt" "coarse_dirt" "podzol" "mycelium" "rooted_dirt"})
(def ground-cover #{"short_grass" "tall_grass" "fern" "large_fern" "dead_bush" "snow"})
(defn max-tries [] (settings/get settings ::max-tries))
(def no-shovel {:reason :no-tool :kind "shovel"})
(def target [:from :to :center :radius])
(def defaults {:use {:tools :shovel} :spend {:job-time ::job-time-s}})
(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)

(defn span [a b] (range (min a b) (inc (max a b))))
(defn span-size [a b] (inc (js/Math.abs (- a b))))

(defn cells
  "The ground cells [{:x :y :z} ...] of args: the box :from/:to, or the square :center/:radius at center's y. Throws
  ex-info for neither form or over max-cells cells."
  [{:keys [from to center radius]}]
  (let [[xa xb ya yb za zb] (cond
                              (and from to) [(:x from) (:x to) (:y from) (:y to) (:z from) (:z to)]
                              (and center radius) [(- (:x center) radius) (+ (:x center) radius) (:y center) (:y center)
                                                   (- (:z center) radius) (+ (:z center) radius)]
                              :else (throw (ex-info "path needs :from and :to, or :center and :radius" {})))
        n (* (span-size xa xb) (span-size ya yb) (span-size za zb))]
    (when (> n (max-cells))
      (throw (ex-info (str "path covers " n " cells, at most " (max-cells)) {:cells n})))
    (vec (for [x (span xa xb) y (span ya yb) z (span za zb)] {:x x :y y :z z}))))

(defn listed
  "The shovels the call names (:use {:tools [..]}) as a set, or nil when it names none (the engine's default list is not given)."
  [c]
  (let [l (get-in (:args c) [:use :tools])]
    (when (vector? l) (set l))))

(defn shovel
  "The name of a carried shovel the call may use, or nil."
  [c]
  (let [only (listed c)]
    (some #(when (and (.endsWith (:name %) "_shovel") (or (nil? only) (only (:name %)))) (:name %)) (u/inventory (:primitives c)))))

(defn fetch-opts
  "The limits {:how :explore} for fetching a shovel, nil when :fetch does not allow tools."
  [c]
  (when-let [o (need/opts c 'jobs.build.path)]
    (when (contains? (:what o) :tool) (select-keys o [:how :explore]))))

(defn shovel-wanted
  "What the fetch gets: a listed shovel by name, else any shovel."
  [c]
  (if-let [item (first (sort (listed c)))] {:item item} {:kind "shovel"}))

(defn pending
  "[[pos block-name-or-nil] ...] of the cells not skipped and not already a path."
  [c]
  (let [skipped (:skipped (ctx/mem c) {})]
    (->> (cells (:args c))
         (remove #(contains? skipped %))
         (map (fn [pos] [pos (u/seen-name (:primitives c) pos)]))
         (remove (fn [[_ n]] (= "dirt_path" n)))
         vec)))

(defn permitted?
  "Whether action at pos passes the zone rules and the plans' footprints."
  [c action pos]
  (gate/allowed? c :path.declined "path" action pos {}))

(defn refused-all
  "The refusals when every pathable pending cell is refused by a zone, claim or plan, else nil."
  [c]
  (let [in (access/zone-input c {})
        verdicts (into [] (keep (fn [[pos n]]
                                  (when (or (nil? n) (pathable n))
                                    (let [v (rules/social-verdict :dig (assoc in :cell (access/cell pos)))]
                                      (if (gate/refused? v) v ::open)))))
                       (pending c))]
    (when (and (seq verdicts) (every? #(not= ::open %) verdicts)) verdicts)))

(defn valid-cells
  "The cells of the call, nil when its cells args are wrong."
  [c]
  (try (cells (:args c)) (catch :default _ nil)))

(defn bad-args
  "Never :bad-args when the cells args are wrong."
  [c]
  (let [cs (try (cells (:args c)) (catch :default e (ex-message e)))]
    (if (string? cs)
      (contract/never :bad-args cs :why cs)
      (contract/ok :args "the cells fit"))))

(def no-zones
  "Later :no-zones when no zone list could be read (unless :ignore-zones?)."
  (checks/refusing :zones-read "a zone list is read"
                      #(when (valid-cells %) (common/no-zones %))))

(def all-refused
  "Later :refused when every pathable cell is refused by a zone, claim or plan (unless :ignore-zones?)."
  (checks/refusing
   :not-refused "a cell is not refused"
   (fn [c]
     (when-let [vs (and (valid-cells c) (not (:ignore-zones? (:args c))) (refused-all c))]
       (assoc (access/refusal-fields vs) :reason :refused)))))

(def shovel-lacking
  "Later :no-tool when cells are left to path, no shovel is carried and :fetch will not get one."
  (checks/refusing
   :shovel "a shovel is carried or fetched"
   (fn [c]
     (when (and (valid-cells c) (seq (pending c)) (nil? (shovel c)) (nil? (fetch-opts c)))
       no-shovel))))

(def can-start? (cond/and bad-args no-zones all-refused shovel-lacking))
(def can-run? no-zones)

(declare result-of)

(defn done?
  "Ok when every cell is a path; else later, with the cells pathed this call, the cells left to path and the result so far."
  [c]
  (let [cs (valid-cells c)]
    (checks/goal (boolean (and (seq cs) (every? #(= "dirt_path" (u/seen-name (:primitives c) %)) cs)))
                 [:pathed "every cell is a path"] [:owed "cells are left to path"]
                 {:units :cells :done (count (:pathed (ctx/mem c))) :left (if cs (count (pending c)) 0) :result (result-of c)})))

(defn quote-of [c]
  (let [cs (try (cells (:args c)) (catch :default _ nil))
        p (:primitives c)
        n (count (remove #(= "dirt_path" (u/seen-name p %)) cs))
        s (shovel c)]
    {:uses (cond-> {:job-time [(* 0.5 n) (* 2 n) (* 10 n)]} (and s (pos? n)) (assoc :wear {s [n n n]}))
     :done [0 n n]}))

(defn skip! [c poss reason]
  (ctx/update-mem! c update :skipped #(into (or % {}) (map (fn [p] [p reason])) poss))
  (doseq [pos poss]
    (ctx/emit! c :path.skipped :info {:pos pos :reason reason
                                      :text (str "skipped " (pr-str pos) ": " (name reason))})))

(defn bump!
  "Count a failed try on pos; at max-tries skip it with reason."
  [c pos reason]
  (let [n (inc (get-in (ctx/mem c) [:cell-tries pos] 0))]
    (if (>= n (max-tries))
      (skip! c [pos] reason)
      (ctx/update-mem! c assoc-in [:cell-tries pos] n))))

(defn cover-args
  "The jobs.blocks.dig args for the ground cover at pos: no tool needed, the drop left."
  [c pos]
  (merge (select-keys (:args c) [:ignore-zones?])
         {:pos pos :collect false :need-drop false :accept #{:fluid-adjacent :falling-block}}))

(def condition-skips
  "Skip reasons that name a cell or a shovel that is not there to use; any other skip is nothing-possible."
  #{:not-pathable :covered :not-permitted :gone :no-shovel})

(defn result-of
  "The data of the call's ending: what is pathed and skipped so far."
  [c]
  (let [mem (ctx/mem c)]
    {:pathed (count (:pathed mem)) :skipped (:skipped mem {})}))

(defn conclude!
  "The ending of a run with no cell left to work (job-common/conclude!)."
  [c]
  (let [{:keys [pathed skipped] :as result} (result-of c)]
    (common/conclude! c {:verb "pathed" :mixed :nothing-pathed :condition-skips condition-skips
                         :texts {:no-shovel "no shovel, nothing pathed"}}
                      pathed skipped result)))

(defn ^:async use-shovel!
  "Use the shovel on target and book the outcome."
  [c target tool]
  (let [res (await (child/use-on! c target tool "up"))
        {:keys [status raw]} res]
    (if (= :continue res)
      :continue
      (do (cond
            (and (= :used status) (= "dirt_path" (some-> (.-after raw) .-name)))
            (ctx/update-mem! c #(-> % (update :pathed (fnil conj #{}) target) (update :cell-tries dissoc target)))

            (= :missing status) (skip! c [target] :gone)
            (= :no-item status) (bump! c target :no-shovel)
            :else (bump! c target :refused))
          :again))))

(defn ^:async work!
  "Walk to target and clear the cover above it, or shovel it."
  [c target tool]
  (let [p (:primitives c)
        w (await (common/walk! c target 3 {:tolls (tc/walk-tolls c (:pos (places/parse-pos target)))}))]
    (if (= :continue w)
      :continue
      (let [above-pos (update target :y inc)
            above-xyz ((juxt :x :y :z) above-pos)
            _ (when (dig-look/unknown? p above-xyz) (await (dig-look/look-at! c above-xyz)))
            above (u/seen-name p above-pos)]
        (cond
          (= :blocked w) (do (bump! c target :unreachable) :again)
          (not (permitted? c :dig target)) (do (skip! c [target] :not-permitted) :again)
          (and (ground-cover above) (not (permitted? c :dig above-pos))) (do (skip! c [target] :not-permitted) :again)

          (ground-cover above)
          (let [e (await (ctx/call c :cover 'jobs.blocks.dig (cover-args c above-pos)))]
            (cond
              (= :continue e) :continue
              (not (and (= :done (:kind e)) (#{:dug :already-clear} (:reason (:data e)))))
              (do (bump! c target :cover-stuck) :again)
              :else :again))

          (nil? above) (do (bump! c target :unloaded) :again)
          (not (bs/air above)) (do (skip! c [target] :covered) :again)
          :else (await (use-shovel! c target tool)))))))

(defn ^:async path-step!
  "One step: skip what cannot be pathed, else work the cell nearest the body."
  [c]
  (let [todo (pending c)
        bad (into [] (comp (filter (fn [[_ n]] (and (some? n) (not (pathable n))))) (map first)) todo)]
    (when (seq bad) (skip! c bad :not-pathable))
    (let [unpermitted (into [] (comp (map first) (remove (set bad)) (remove #(permitted? c :dig %))) todo)
          _ (when (seq unpermitted) (skip! c unpermitted :not-permitted))
          bad? (into (set bad) unpermitted)
          cands (remove (fn [[pos _]] (bad? pos)) todo)
          tool (shovel c)
          me (u/self-pos c)]
      (cond
        (empty? cands) (conclude! c)
        (nil? tool) (do (skip! c (map first cands) :no-shovel) :again)
        :else (await (work! c (first (apply min-key #(u/dist me (first %)) cands)) tool))))))

(defn ^:async fetch-shovel!
  "Get a shovel when none may be used and cells are left: nil when one is carried (or came), :continue while the fetch
  waits on the world, else the ending of the call: done when enough is pathed already, else condition-failed :no-tool
  (the fetch's ending as its :because)."
  [c]
  (when (and (nil? (shovel c)) (seq (pending c)))
    (let [e (if-let [o (fetch-opts c)]
              (await (need/tool! c (shovel-wanted c) o))
              {:kind :condition-failed :cause no-shovel})]
      (cond
        (= :continue e) :continue
        (= :done (:kind e)) nil
        (common/enough? c (count (:pathed (ctx/mem c))) (count (cells (:args c)))) (conclude! c)
        :else (contract/with-because (ctx/end c :condition-failed no-shovel (result-of c)) e)))))

(defn ^:async round
  "The whole attempt: fetch a shovel when none is carried, then loop the steps until the cells are done; :continue only
  while a walk or fetch waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s []
                          (or (await (fetch-shovel! c))
                              (await (path-step! c)))))))
