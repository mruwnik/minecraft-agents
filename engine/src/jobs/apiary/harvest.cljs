(ns jobs.apiary.harvest
  (:require [engine.ctx :as ctx]
            [jobs.lib.apiary :as apiary]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :as look]
            [jobs.lib.util :as u]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]))

(def doc
  "Take the honey of the ripe hives (honey_level 5) near a centre, nearest first, at most :max hives, all in one call
  (:continue only while a walk or the comb pick-up waits on the world). Shears give 3
  honeycomb, a glass bottle gives a honey bottle. After each shears harvest the honeycomb on the ground is
  collected.
  A hive is worked only when it is smoked by vanilla's rule: a lit campfire at most 5 blocks under it with only
  air-like blocks between, or a lit campfire directly under the first block in the way.
  - An unsmoked hive angers the bees, so it is declined without a click (:not-smoked).
  - A smoked hive over an open lit fire (nothing with a collision box on it; a non-moss carpet counts) burns
    the bees that land, so it is declined too (:open-fire).
  - A hive the body cannot walk to, or that refuses the item, is skipped for the rest of the run.
  Result: {:harvested n :reason r :with item :declined {pos reason} :skipped {pos reason} :collected n}. :reason is
  :harvested (some taken, nothing more to do), :limit (:max reached), :no-tool, :no-hive, :not-ripe,
  :not-smoked, :open-fire, :unreachable or :gave-up (3 fruitless hives in a row). Declines and give-ups also warn
  apiary.gave-up.
  Zones: a hive in another owner's zone or claim, or in a plan's footprint, is left out of the survey (all refused
  ends :no-hive) and checked again before the click. The job warns apiary.declined once, with :reason :refused (or
  :no-zones when no zone list was read). :ignore-zones? true skips the check.")

(def args
  {:with {:doc ":shears, :bottle or :either (shears first when both are carried)" :default :either}
   :box {:doc "{:from pos :to pos}, hives inside it only; overrides :center and :radius" :default nil}
   :center {:doc "centre of the search; the body's position when the job first runs when nil" :type :pos :default nil}
   :radius {:doc "hives within this many blocks of the centre count, when :box is nil" :default 12}
   :max {:doc "hives to harvest in one run, at most" :default 8}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def ripe-level 5)
(def reach 3)
(def max-strikes 3)
(def tool-item {:shears "shears" :bottle "glass_bottle"})
(def hive-names #js ["beehive" "bee_nest"])

;; ------------------------------------------------------------------ the world

(defn ripe? [b] (>= (or (get-in b [:properties :honey_level]) 0) ripe-level))

(defn hive-allowed?
  "Whether the job may take honey from the hive at pos (one warn per job when refused)."
  [c pos]
  (gate/allowed? c :apiary.declined "apiary harvest" :harvest pos))

(defn hives
  "Every hive the body has seen in the area that the zone rules let the job use, as {:pos :ripe}, nearest to the
  body first."
  [c center]
  (let [p (:primitives c)
        me (u/self-pos c)
        {:keys [radius box]} (:args c)
        search (if box 64 (+ radius (u/dist me center)))
        found (->> (look/seen-blocks p {:radius search :names hive-names :properties? true :live? true :max 64})
                   (map (fn [b] {:pos (:pos b) :ripe (ripe? b)}))
                   (filter #(apiary/in-area? {:box box :center center :radius radius} (:pos %))))
        ok (set (gate/allowed c :apiary.declined "apiary harvest" :harvest (map :pos found)))]
    (->> found
         (filter #(ok (:pos %)))
         (sort-by #(u/dist me (:pos %))))))

(defn tool-for
  "The item name to harvest with under :with, from the inventory, or nil."
  [with inventory]
  (let [carried? (fn [n] (some #(= n (:name %)) inventory))
        wanted (case (keyword with)
                 :shears [:shears]
                 :bottle [:bottle]
                 [:shears :bottle])]
    (some #(when (carried? (tool-item %)) (tool-item %)) wanted)))

(defn classify
  "The hives as {:todo [pos] :declined {key reason} :ripe n :hives n}: ripe hives not skipped, sorted by the verdict."
  [c hs]
  (let [block-at (apiary/seen-block-at-fn (:primitives c))
        skipped (:skipped (ctx/mem c) {})
        ripe (->> hs (filter :ripe) (remove #(contains? skipped (apiary/pos-key (:pos %)))))
        verdicts (map (fn [h] [(:pos h) (apiary/hive-verdict block-at (:pos h))]) ripe)]
    {:hives (count hs)
     :ripe (count (filter :ripe hs))
     :todo (vec (keep (fn [[pos v]] (when (= :ok v) pos)) verdicts))
     :declined (into {} (keep (fn [[pos v]] (when-not (= :ok v) [(apiary/pos-key pos) v])) verdicts))}))

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

(defn finish!
  "Emit the outcome, hand it to the parent and end the job."
  [c reason seen]
  (let [m (ctx/mem c)
        result {:harvested (:harvested m 0)
                :reason reason
                :with (:tool m)
                :declined (:declined seen {})
                :skipped (:skipped m {})
                :collected (:collected m 0)}]
    (ctx/emit! c :apiary.done :info (assoc result :text (str "apiary harvest done: " (name reason) ", " (:harvested result) " hives")))
    (when-not (#{:harvested :limit} reason)
      (ctx/emit! c :apiary.gave-up :warn {:reason reason :declined (:declined result) :skipped (:skipped result)
                                         :text (str "apiary harvest stopped: " (name reason))}))
    (ctx/result! c result)
    :done))

(defn skip!
  "Leave the hive alone for the rest of the run and count a fruitless hive."
  [c pos reason]
  (ctx/update-mem! c #(-> % (assoc-in [:skipped (apiary/pos-key pos)] reason) (update :strikes (fnil inc 0)))))

;; ------------------------------------------------------------------ rounds

(defn ^:async collect!
  "Pick up the honeycomb lying about, then go back to the hives. :continue while the pick-up waits; any other end
  of it (done, declined) closes the phase."
  [c]
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius 8 :filter ["honeycomb"]}))]
    (when (= :done r)
      (ctx/update-mem! c update :collected (fnil + 0) (:collected (ctx/child-result c :collect) 0)))
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c dissoc :phase) :again))))

(defn book-harvest!
  "Count the hive at pos as taken when it is no longer ripe; the comb is then collected after shears."
  [c pos tool]
  (let [level (some-> (u/seen-block (:primitives c) pos) .-properties .-honey_level)]
    (if (< (or level ripe-level) ripe-level)
      (ctx/update-mem! c #(cond-> (-> % (update :harvested (fnil inc 0)) (assoc :strikes 0))
                            (= "shears" tool) (assoc :phase :collect)))
      (skip! c pos :no-effect))))

(defn settle-click!
  "A cut left a click unbooked: book it from the hive's level now."
  [c]
  (when-let [{:keys [pos tool]} (:clicking (ctx/mem c))]
    (ctx/update-mem! c dissoc :clicking)
    (when (< (or (some-> (u/seen-block (:primitives c) pos) .-properties .-honey_level) ripe-level) ripe-level)
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
  (let [w (await (near/go-near! c pos reach {:zone-tolls true :escalate false}))]
    (case w
      :partial :continue
      :blocked (do (skip! c pos :unreachable) :again)
      (let [_ (when (hive-allowed? c pos) (ctx/update-mem! c assoc :clicking {:pos pos :tool tool}))
            r (if (hive-allowed? c pos)
                (await (ctx/act c :useOn (clj->js {:pos pos :item tool :face (face-toward pos (u/self-pos c))})))
                #js {:status "refused"})
            level (some-> r .-after .-properties .-honey_level)]
        (ctx/update-mem! c dissoc :clicking)
        (if (and (= "used" (.-status r)) (< (or level ripe-level) ripe-level))
          (book-harvest! c pos tool)
          (skip! c pos (keyword (if (= "used" (.-status r)) "no-effect" (.-status r)))))
        :again))))

(defn check [_c] true)

(defn ^:async step
  "One piece of the harvest: collect after a shears harvest; else classify the hives and finish when none can be
  worked, the budget is spent or three hives in a row failed; else harvest the nearest workable hive. :again,
  :continue while a walk or collect waits on the world, or :done."
  [c]
  (let [center (apiary/center-of c)
        _ (when-not (:center (ctx/mem c)) (ctx/update-mem! c assoc :center center))
        _ (settle-click! c)
        m (ctx/mem c)
        _ (when (and (not= :collect (:phase m)) (empty? (hives c center)) (not (look/looked-here? c)))
            (await (look/look-around! c)))]
    (if (= :collect (:phase m))
      (await (collect! c))
      (let [seen (classify c (hives c center))
            tool (tool-for (:with (:args c)) (u/inventory (:primitives c)))
            pos (first (:todo seen))]
        (cond
          (>= (:harvested m 0) (:max (:args c))) (finish! c :limit seen)
          (>= (:strikes m 0) max-strikes) (finish! c :gave-up seen)
          (nil? pos) (finish! c (idle-reason m seen) seen)
          (nil? tool) (finish! c :no-tool seen)
          :else (do (ctx/update-mem! c assoc :tool tool)
                    (await (harvest! c pos tool))))))))

(def max-steps "Steps of one call before it gives the round back with :continue." 400)

(defn ^:async round [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) max-steps) (step c) :continue)))))
