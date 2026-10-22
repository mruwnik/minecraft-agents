(ns jobs.apiary.harvest
  (:require [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [engine.contract :as contract]
            [engine.settings :as settings]
            [jobs.lib.waits :as waits]
            [jobs.lib.game-facts :as facts]
            [engine.args :as a]
            [jobs.lib.pen-read :as pen-read]
            [engine.ctx :as ctx]
            [jobs.lib.apiary :as apiary :refer [classify found-hives hives tool-for tool-item]]
            [jobs.lib.child :as child]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]))

(def doc
  "Take the honey of the ripe hives (honey_level 5) near a centre, nearest first, at most :max hives, all in one call
  (:continue only while a walk or pick-up waits on the world). It looks around once first. Shears give 3
  honeycomb, a glass bottle gives a honey bottle. After each shears harvest the honeycomb on the ground is
  collected; when none shows yet the job pauses (jobs.time.noop, holding :comb) for the comb-wait setting and looks once more.
  A hive is worked only when it is smoked by vanilla's rule: a lit campfire at most 5 blocks under it with only
  air-like blocks between, or a lit campfire directly under the first block in the way.
  - An unsmoked hive angers the bees, so it is declined without a click (:not-smoked).
  - A smoked hive over an open lit fire (nothing with a collision box on it; a non-moss carpet counts) burns
    the bees that land, so it is declined too (:open-fire).
  - A hive the body cannot walk to, or that refuses the item, is skipped for the rest of the run.
  A tool of :with that is not carried is fetched (jobs.lib.need) unless the ways leave only :carried; a failed fetch ends
  nothing-possible or condition-failed with :reason :need.
  Start facts, found after a quiet look around, make it wait (a skipped job): :no-hive, :not-ripe, :not-smoked,
  :open-fire (no hive can be worked), :no-tool (none of :with is carried, ways #{:carried}), :refused or :no-zones (the
  zone rules refuse every hive).
  Result: {:harvested n :reason r :with item :declined {pos reason} :skipped {pos reason} :collected n}. :reason is
  :harvested (some taken, nothing more to do), :limit (:max reached), :no-tool, :no-hive, :not-ripe,
  :not-smoked, :open-fire, :need, :unreachable or :gave-up (3 fruitless hives in a row).
  The ending: :done once the call's :least (default 1, at most :max; :all = every hive worked) hives were taken,
  whatever stopped the run; else, with none taken, condition-failed for :no-hive, :not-ripe, :not-smoked,
  :open-fire, :no-tool, :refused and :no-zones, nothing-possible for the rest; some taken but fewer than :least is
  nothing-possible with the reason that stopped the last one (:harvested then names what was left: :not-ripe,
  :no-hive ..), the result as its data.
  Zones: a hive in another owner's zone or claim, or in a plan's footprint, is left out of the survey (all refused
  ends :no-hive) and checked again before the click. The job warns apiary.declined once, with :reason :refused (or
  :no-zones when no zone list was read). :ignore-zones? true skips the check.")

(a/defargs args
  {:with {:doc ":shears, :bottle or :either (shears first when both are carried)" :spec #{:shears :bottle :either} :default :either}
   :box {:doc "{:min pos :max pos}, hives inside it only; overrides :center and :radius" :spec pen-read/box-arg :default nil}
   :center {:doc "centre of the search; the body's position when the job first runs when nil" :spec ::a/pos :default nil}
   :radius {:doc "hives within this many blocks of the centre count, when :box is nil" :spec (a/num-in 0 nil) :default 12}
   :max {:doc "hives to harvest in one run, at most" :spec (a/int-in 1 nil) :default 8}
   :ignore-zones? :shared})

(a/defargs settings
  {::reach {:default 3 :doc "How near the body stands to act on its target, in blocks." :spec (a/int-in 0 nil)}
   ::max-strikes {:default 3 :doc "Strikes at one target before the job gives up on it." :spec (a/int-in 0 nil)}
   ::comb-wait-s {:default 5 :doc "Seconds a shears harvest pauses for its comb to show, once, before it goes on without." :spec (a/num-in 0 nil)}
   ::collect-radius {:default 8 :doc "Blocks round the body in which honeycomb is picked up." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 120 :doc "Seconds a harvest run may take." :spec (a/num-in 0 nil)}})
(defn reach [] (settings/get settings ::reach))
(defn max-strikes [] (settings/get settings ::max-strikes))

;; ------------------------------------------------------------------ the world

(defn hive-allowed?
  "Whether the job may take honey from the hive at pos (one warn per job when refused)."
  [c pos]
  (gate/allowed? c :apiary.declined "apiary harvest" :harvest pos {:own-plans-ok? true}))

(defn wanted-tool
  "The item name to fetch for :with: shears unless only :bottle."
  [with]
  (tool-item (if (= :bottle (keyword with)) :bottle :shears)))

(defn idle-reason
  "Why there is nothing to do: first what was skipped, then what was declined, then what was not there."
  [m {:keys [declined hives ripe]}]
  (cond
    (pos? (:harvested m 0)) :harvested
    (seq (:skipped m)) (first (vals (:skipped m)))
    (seq declined) (first (vals declined))
    (zero? hives) :no-hive
    (zero? ripe) :not-ripe
    :else :no-hive))

(def failed-conditions "Reasons that end condition-failed: the world or the zones bar the work. The rest are nothing-possible."
  #{:no-hive :not-ripe :not-smoked :open-fire :no-tool :refused :no-zones})

(defn enough?
  "Whether taken hives meet the call's :least (default 1, at most :max); :all: the run stopped for :harvested or :limit."
  [c reason taken]
  (let [least (:least (ctx/frozen-call c) 1)]
    (if (= :all least)
      (and (pos? taken) (boolean (#{:harvested :limit} reason)))
      (>= taken (min least (:max (:args c)))))))

(defn ending-of
  "The ending of a run that stopped for reason: :done when enough? hives were taken, else with none taken
  condition-failed for failed-conditions, else nothing-possible; result is its data."
  [c reason result]
  (let [taken (:harvested result)]
    (cond
      (enough? c reason taken)
      (ctx/end c :done nil (assoc result :text (str "apiary harvest done: " (name reason) ", " taken " hives")))
      (and (zero? taken) (failed-conditions reason)) (ctx/end c :condition-failed reason result)
      :else (ctx/end c :nothing-possible reason result))))

(defn finish!
  "Emit the outcome and end the job (ending-of)."
  [c reason0 seen]
  (let [m (ctx/mem c)
        reason (if (and (= :harvested reason0) (not (enough? c reason0 (:harvested m 0))))
                 (idle-reason (dissoc m :harvested) seen)
                 reason0)
        result {:harvested (:harvested m 0)
                :reason reason
                :with (:tool m)
                :declined (:declined seen {})
                :skipped (:skipped m {})
                :collected (:collected m 0)}]
    (ending-of c reason result)))

(defn skip!
  "Leave the hive alone for the rest of the run and count a fruitless hive."
  [c pos reason]
  (ctx/update-mem! c #(-> % (assoc-in [:skipped (apiary/pos-key pos)] reason) (update :strikes (fnil inc 0)))))

;; ------------------------------------------------------------------ rounds

(defn comb-wait-s [] (settings/get settings ::comb-wait-s))

(defn ^:async wait-for-comb!
  "The one comb-wait-s pause; :continue while it is cut, then the second look (:again)."
  [c]
  (if (= :continue (await (child/pause! c (comb-wait-s) :comb)))
    :continue
    (do (ctx/update-mem! c assoc :comb-wait :waited) :again)))

(defn ^:async collect!
  "Pick up the honeycomb lying about, then go back to the hives. :continue while the pick-up waits; when no comb shows
  yet (the spawn may not have reached the body) one comb-wait-s pause, then a second look; any other end of it closes
  the phase."
  [c]
  (if (= :pausing (:comb-wait (ctx/mem c)))
    (await (wait-for-comb! c))
    (let [e (waits/waiting-out c (await (ctx/call c :collect 'jobs.items.collect-drops {:radius (settings/get settings ::collect-radius) :items ["honeycomb"]})))
          done? (= :done (:kind e))
          got (if done? (get-in e [:data :collected] 0) 0)]
      (when done?
        (ctx/update-mem! c update :collected (fnil + 0) got))
      (cond
        (= :continue e) :continue
        (and done? (zero? got) (not (:comb-wait (ctx/mem c))))
        (do (ctx/update-mem! c assoc :comb-wait :pausing)
            (await (wait-for-comb! c)))
        :else (do (ctx/update-mem! c #(-> % (dissoc :phase) (dissoc :comb-wait))) :again)))))

(defn book-harvest!
  "Count the hive at pos as taken when it is no longer ripe; the comb is then collected after shears."
  [c pos tool]
  (let [level (some-> (u/seen-block (:primitives c) pos) .-properties .-honey_level)]
    (if (< (or level facts/ripe-level) facts/ripe-level)
      (ctx/update-mem! c #(cond-> (-> % (update :harvested (fnil inc 0)) (assoc :strikes 0))
                            (= "shears" tool) (assoc :phase :collect)))
      (skip! c pos :no-effect))))

(defn settle-click!
  "A cut left a click unbooked: book it from the hive's level now."
  [c]
  (when-let [{:keys [pos tool]} (:clicking (ctx/mem c))]
    (ctx/update-mem! c dissoc :clicking)
    (when (< (or (some-> (u/seen-block (:primitives c) pos) .-properties .-honey_level) facts/ripe-level) facts/ripe-level)
      (book-harvest! c pos tool))))

;; the comb pops out of the face clicked: the side the body stands on, so it lies in view (on top, the hive hides it from below)
(defn face-toward
  "The face of the hive at pos the body at `from` looks at: \"up\" from above, else the horizontal side it is on."
  [pos from]
  (let [dx (- (:x from) (:x pos)) dz (- (:z from) (:z pos))]
    (cond
      (> (:y from) (:y pos)) "up"
      (>= (js/Math.abs dx) (js/Math.abs dz)) (if (neg? dx) "west" "east")
      :else (if (neg? dz) "north" "south"))))

(defn ^:async harvest!
  "Walk to the hive and use the tool once; book the outcome."
  [c pos tool]
  (let [w (await (near/go-near! c pos (reach) {:ways #{:walk :clear-seal}}))]
    (case w
      :partial :continue
      :blocked (do (skip! c pos :unreachable) :again)
      (let [_ (when (hive-allowed? c pos) (ctx/update-mem! c assoc :clicking {:pos pos :tool tool}))
            res (if (hive-allowed? c pos)
                  (await (child/use-on! c pos tool (face-toward pos (u/self-pos c))))
                  {:status :refused})
            level (some-> (:raw res) .-after .-properties .-honey_level)]
        (if (= :continue res)
          :continue
          (do (ctx/update-mem! c dissoc :clicking)
              (if (and (= :used (:status res)) (< (or level facts/ripe-level) facts/ripe-level))
                (book-harvest! c pos tool)
                (skip! c pos (if (= :used (:status res)) :no-effect (:status res))))
              :again))))))

(def ways need/fetch-ways)

(def target [:box :center])

(def safe? (price/staying :work (ready/safe-at-by-night work-place/spot)))
(def fit? ready/fit?)


(def defaults {:least 1 :spend {:job-time ::job-time-s}})

(defn can-start?
  "Later with the cause that bars the first step: no hive can be worked (idle-reason), none of :with is carried or the
  zone rules refuse every hive. A look first while the body has not looked round near here."
  [c]
  (let [center (apiary/center-of c)
        found (found-hives c center)
        hs (hives c center true)
        seen (classify c hs)
        refusal (when (and (seq found) (empty? hs)) (:refusal (gate/look c :harvest (map :pos found) {:own-plans-ok? true})))
        reason (cond
                 refusal refusal
                 (empty? (:todo seen)) (idle-reason {} seen)
                 (and (nil? (tool-for (:with (:args c)) (u/inventory (:primitives c))))
                      (nil? (need/fetch-opts c))) :no-tool)]
    (cond
      (nil? reason) (contract/ok :start "a hive can be worked")
      (look/surveyed? c :near) (contract/later reason (str "harvest waits: " (name reason)))
      :else (contract/look reason (str "harvest waits: " (name reason))))))

(defn done?
  "Later with the hives harvested and the ripe hives still to work: the run ends itself with the reason it stopped for."
  [c]
  (let [seen (classify c (hives c (apiary/center-of c) true))]
    (contract/later :harvesting (str (:harvested (ctx/mem c) 0) " hives harvested") :units :hives
                    :done (:harvested (ctx/mem c) 0) :left (count (:todo seen)))))

(defn quote-of [c]
  {:uses {:job-time [0 (* 15 (or (:max (:args c)) 8)) ##Inf]}})

(defn ^:async supply!
  "Fetch the tool when the ways allow: :again once it is carried, :continue while the fetch waits, else the ending of
  the run (reason :need, the fetch's ending as its cause); nil when the ways leave only :carried."
  [c seen]
  (when-let [opts (need/fetch-opts c)]
    (let [e (await (need/item! c (wanted-tool (:with (:args c))) 1 opts))]
      (cond
        (= :continue e) :continue
        (= :done (:kind e)) :again
        :else (ctx/end c (:kind e) (:cause e) {:harvested (:harvested (ctx/mem c) 0) :reason :need
                                               :with (:tool (ctx/mem c)) :declined (:declined seen {})
                                               :skipped (:skipped (ctx/mem c) {}) :collected (:collected (ctx/mem c) 0)})))))

(defn ^:async step
  "One piece of the harvest: collect after a shears harvest; else classify the hives and finish when none can be
  worked, the budget is spent or three hives in a row failed; else harvest the nearest workable hive. :again,
  :continue while a walk or collect waits on the world, or :done."
  [c]
  (let [center (apiary/center-of c)
        _ (when-not (:center (ctx/mem c)) (ctx/update-mem! c assoc :center center))
        _ (settle-click! c)
        m (ctx/mem c)
        _ (when (not= :collect (:phase m))
            (await (look/survey! c)))]
    (if (= :collect (:phase m))
      (await (collect! c))
      (let [seen (classify c (hives c center))
            tool (tool-for (:with (:args c)) (u/inventory (:primitives c)))
            pos (first (:todo seen))]
        (cond
          (>= (:harvested m 0) (:max (:args c))) (finish! c :limit seen)
          (>= (:strikes m 0) (max-strikes)) (finish! c :gave-up seen)
          (nil? pos) (finish! c (idle-reason m seen) seen)
          (nil? tool) (or (await (supply! c seen)) (finish! c :no-tool seen))
          :else (do (ctx/update-mem! c assoc :tool tool)
                    (await (harvest! c pos tool))))))))

(defn ^:async round [c]
  (await (pace/steps! c #(step c))))
