(ns jobs.apiary.harvest
  (:require [engine.ctx :as ctx]
            [engine.jobs.apiary :as apiary]
            [engine.jobs.gate :as gate]
            [engine.jobs.util :as u]
            [engine.path.near :as near]))

(def doc
  "Take the honey of the ripe hives (honey_level 5) near a centre with shears
  (3 honeycomb) or a glass bottle (a honey bottle), nearest first, at most :max
  hives. A hive is worked only when it is smoked by vanilla's own rule: a lit
  campfire at most 5 blocks under it, only air-like blocks between, or a lit
  campfire directly under the first block in the way. An unsmoked hive angers
  the bees, so it is declined without a click (:not-smoked); a smoked hive over
  an open lit fire (nothing with a collision box on it, a non-moss carpet counts)
  burns the bees that land, so it is declined too (:open-fire). A hive the body
  cannot walk to, or that refuses the item, is skipped for the rest of the run.
  After each shears harvest the honeycomb on the ground is collected. Ends with
  a result {:harvested n :reason r :with item :declined {pos reason} :skipped
  {pos reason} :collected n}; :reason is :harvested (some taken, nothing more to
  do), :limit (:max reached), :no-tool, :no-hive, :not-ripe, :not-smoked,
  :open-fire, :unreachable, or :gave-up after 3 fruitless hives in a row. Declines and
  give-ups also emit a warn apiary.gave-up.

  Zones and claims are a rule the job consults: a hive in a zone or claim of another owner, or in a plan's footprint,
  is not one to take honey from (:harvest of the zone rules), left out of the survey (all refused ends :no-hive) and
  asked again right before the click. One apiary.declined warn per job names the zones, claims and plans ({:reason
  :refused ...}); without a zone list it declines with {:reason :no-zones}. :ignore-zones? acts regardless.")

(def args
  {:with {:doc ":shears, :bottle or :either (shears first when both are carried)" :default :either}
   :box {:doc "{:from pos :to pos}, hives inside it only; overrides :center and :radius" :default nil}
   :center {:doc "centre of the search; the body's position when the job first runs when nil" :default nil}
   :radius {:doc "hives within this many blocks of the centre count, when :box is nil" :default 12}
   :max {:doc "hives to harvest in one run, at most" :default 8}
   :walk-timeout-s {:doc "bound of one walk towards a hive" :default 8}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def ripe-level 5)
(def reach 3)
(def max-strikes 3)
(def tool-item {:shears "shears" :bottle "glass_bottle"})
(def hive-names #js ["beehive" "bee_nest"])

;; ------------------------------------------------------------------ the world

(defn pos-key [{:keys [x y z]}] (str x "," y "," z))

(defn ripe? [b] (>= (or (some-> b .-properties .-honey_level) 0) ripe-level))

(defn in-area? [{:keys [box]} center radius pos]
  (if box
    (let [{:keys [from to]} box
          within (fn [k] (<= (min (k from) (k to)) (k pos) (max (k from) (k to))))]
      (and (within :x) (within :y) (within :z)))
    (<= (u/dist center pos) radius)))

(defn hive-allowed?
  "Whether the job may take honey from the hive at pos (one warn per job when refused)."
  [c pos]
  (gate/allowed? c :apiary.declined "apiary harvest" :harvest pos))

(defn hives
  "Every hive in the area that the zone rules let the job use, as {:pos :ripe}, nearest to the body first."
  [c center]
  (let [p (:primitives c)
        me (u/self-pos c)
        {:keys [radius box] :as a} (:args c)
        search (if box 64 (+ radius (u/dist me center)))
        found (->> (array-seq (.blocks p #js {:radius search :names hive-names :properties true :max 64}))
                   (map (fn [b] {:pos (u/pos-of (.-pos b)) :ripe (ripe? b)}))
                   (filter #(in-area? a center radius (:pos %))))
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
  (let [block-at (apiary/block-at-fn (:primitives c))
        skipped (:skipped (ctx/mem c) {})
        ripe (->> hs (filter :ripe) (remove #(contains? skipped (pos-key (:pos %)))))
        verdicts (map (fn [h] [(:pos h) (apiary/hive-verdict block-at (:pos h))]) ripe)]
    {:hives (count hs)
     :ripe (count (filter :ripe hs))
     :todo (vec (keep (fn [[pos v]] (when (= :ok v) pos)) verdicts))
     :declined (into {} (keep (fn [[pos v]] (when-not (= :ok v) [(pos-key pos) v])) verdicts))}))

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
  (ctx/update-mem! c #(-> % (assoc-in [:skipped (pos-key pos)] reason) (update :strikes (fnil inc 0)))))

(defn center-of [c]
  (or (:center (:args c)) (:center (ctx/mem c)) (u/self-pos c)))

;; ------------------------------------------------------------------ rounds

(defn ^:async collect!
  "Pick up the honeycomb lying about, then go back to the hives."
  [c]
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius 8 :filter ["honeycomb"]}))]
    (when (= :done r)
      (ctx/update-mem! c #(-> % (update :collected (fnil + 0) (:collected (ctx/child-result c :collect) 0)) (dissoc :phase))))
    :continue))

(defn ^:async harvest!
  "Walk to the hive and use the tool once; book the outcome."
  [c pos tool]
  (let [w (await (near/walk-near! c pos reach))]
    (case w
      :partial :continue
      :blocked (do (skip! c pos :unreachable) :continue)
      (let [r (if (hive-allowed? c pos)
                (await (ctx/act c :useOn (clj->js {:pos pos :item tool :face "up"})))
                #js {:status "refused"})
            level (some-> r .-after .-properties .-honey_level)]
        (if (and (= "used" (.-status r)) (< (or level ripe-level) ripe-level))
          (ctx/update-mem! c #(cond-> (-> % (update :harvested (fnil inc 0)) (assoc :strikes 0))
                                (= "shears" tool) (assoc :phase :collect)))
          (skip! c pos (keyword (if (= "used" (.-status r)) "no-effect" (.-status r)))))
        :continue))))

(defn check [_c] true)

(defn ^:async round
  "One bounded step: collect after a shears harvest; else classify the hives and
  finish when none can be worked, the budget is spent or three hives in a row
  failed; else harvest the nearest workable hive."
  [c]
  (let [center (center-of c)
        _ (when-not (:center (ctx/mem c)) (ctx/update-mem! c assoc :center center))
        m (ctx/mem c)]
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
