(ns jobs.forestry.maintain
  (:require [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.access :as access]
            [jobs.lib.trees :as forestry]
            [jobs.lib.util :as u]
            [jobs.lib.look :as look]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.step-off :as step-off]
            [jobs.lib.world :as known]))

(def doc
  "Keep the trees a forest plan wants (cells wanting {:tree species}; a large tree is four cells).
  One call does the first step that applies, again and again until none is left (:continue only while a child
  waits on the world):
  1. Collect the drops of a tree just felled.
  2. Go on felling the tree begun.
  3. Plant the planned sapling on a planned cell standing bare (air over soil) or owing one.
  4. Fell the nearest grown tree of the wanted species on a planned cell (jobs.forestry.fell-tree as a child). The
     cell is noted as owing a sapling before the base log is dug, so a restart or plan edit never loses the replant.
  5. Finish. Result: {:felled :planted :left :bare}.
  With a planned cell never seen, the first round looks around once (and yields) before it decides.
  Cells are read from the plan and the world at every step. A sapling of the wanted species is left to grow.
  Anything else on a planned cell (another species, a block) is left and reported once (forest.foreign). Trees off
  the planned cells are never touched.
  A bare cell whose sapling is not carried is skipped, with one forest.no-sapling note. A later run plants it.
  A tree is left standing (forest.left, once, and skipped for 30 minutes via body memory :forestry/left) when:
  - its column holds more than :max-logs logs and :pillar? is false (else it is felled from a pillar),
  - fell-tree gave up on it,
  - a log of it may not be dug (another plan's footprint or a zone; checked for the whole column before the first
    dig and again before every felling round), or
  - planting it was refused or failed three times.
  Waits (check) unless a grown tree stands on a planned cell that is not left, or a bare planned cell has its
  sapling carried. A started job always runs on to finish.
  The job declines (one forest.declined warn naming the plan and the reason) while the plan is missing,
  unreadable or has no tree cells, and while no zone list is loaded.")

(def args
  {:plan {:doc "id of a plan of the body's world; its tree cells are the forest" :default nil}
   :part {:doc "only the cells of this part" :default nil}
   :max-logs {:doc "with :pillar? false, a tree whose column holds more logs than this is too tall to fell from the ground and is left" :default 6}
   :pillar? {:doc "fell a tree too tall for the ground from a pillar (jobs.forestry.fell-tree); false: leave one over :max-logs" :default true}
   :accept {:doc "dig hazards (jobs.lib.access.rules) taken: a set of :fluid-adjacent :falling-block :under-feet"
            :default #{:fluid-adjacent :falling-block}}
   :collect-radius {:doc "how far from where the body stands the drops of a felled tree are collected, in blocks" :default 8}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

;; ------------------------------------------------------------------ access

(defn cell-vec [{:keys [x y z]}] [x y z])

(defn feet-cell [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn access-input
  "The rules' input for a cell as the body is now."
  [c pos]
  (let [p (:primitives c)]
    (merge {:block-at (fn [[x y z]] (u/seen-name p {:x x :y y :z z}))
            :cell (cell-vec pos)
            :feet (feet-cell c)
            :ledger #{}}
           (access/zone-input c {:except (:plan (:args c)) :ignore-zones? (:ignore-zones? (:args c))}))))

(defn refusal
  "The reason the body may not dig the log at pos (a hazard it does not accept, or a refusal), nil when it may."
  [c pos]
  (let [v (rules/may-dig? (access-input c pos))]
    (when-not (rules/accepts? v (:accept (:args c)))
      (or (:reason v) (:reason (first (remove #((:accept (:args c)) (:reason %)) (:hazards v))))))))

(defn place-refusal [c pos]
  (let [v (rules/may-place? (access-input c pos))]
    (when-not (:ok v) (:reason v))))

;; ------------------------------------------------------------------ pure helpers

(defn cell-pos [[x y z]] {:x x :y y :z z})

(defn tree-cells
  "{pos species} of the cells of a plan answer (of part, when given) that want a tree."
  [answer part]
  (into {} (keep (fn [{:keys [pos want] :as cell}]
                   (when (and (map? want) (string? (:tree want)) (or (nil? part) (= part (:part cell))))
                     [(cell-pos pos) (:tree want)])))
        (:cells answer)))

(defn plan-trouble
  "Why a plan answer with these tree cells cannot be worked, or nil."
  [answer cells]
  (cond
    (nil? answer) "no such plan"
    (:broken answer) (str "the plan cannot be read: " (:broken answer))
    (empty? cells) "no tree cells"))

(def soil #{"dirt" "grass_block" "coarse_dirt" "podzol" "rooted_dirt" "moss_block"})

(defn soil-for [species]
  (if (= "mangrove" species) (into soil ["mud" "clay" "muddy_mangrove_roots"]) soil))

(defn up [pos] (update pos :y inc))
(defn down [pos] (update pos :y dec))

(defn classify
  "What stands on the planned cell pos for species: :ripe (its log), :growing (its sapling), :bare (air over its
  soil, nothing growing below a log), :no-ground (air, but no soil below), :cramped (air over soil with a log
  above), :unloaded, or :foreign (anything else)."
  [p pos species]
  (let [here (u/seen-name p pos)
        below (u/seen-name p (down pos))]
    (cond
      (nil? here) :unloaded
      (= here (str species "_log")) :ripe
      (= here (forestry/sapling-of species)) :growing
      (not (rules/air here)) :foreign
      (nil? below) :unloaded
      (not ((soil-for species) below)) :no-ground
      (some-> (u/seen-name p (up pos)) forestry/log-name?) :cramped
      :else :bare)))

(defn carried-names [p] (set (map :name (u/inventory p))))

;; ------------------------------------------------------------------ left cells

(def left-kind :forestry/left)

(def left-policy {:cap 50 :ttl (* 30 60 1000)})

(defn left-cells
  "The cells to skip: those noted left in body memory and not yet expired."
  [c]
  (set (map (comp :pos :data) (ctx/entries c left-kind))))

(defn leave!
  "Skip the cell pos for a while and say why, once."
  [c pos reason & {:as more}]
  (ctx/remember! c left-kind {:pos pos :reason reason} left-policy)
  (ctx/warn-once! c [:left pos reason] :forest.left
                  (merge {:pos pos :reason reason}
                         more
                         {:text (str "forest leaves the tree at " (pr-str (cell-vec pos)) " standing: " (name reason)
                                     (when-let [why (:why more)] (str " (" (name why) ")")))})))

;; ------------------------------------------------------------------ the plan

(defn planned
  "{:trees {pos species}} of the plan, or {:trouble text} (warned once per reason) when it cannot be worked."
  [c]
  (let [{:keys [plan part]} (:args c)
        answer (known/plan c plan)
        trees (tree-cells answer part)
        trouble (or (plan-trouble answer trees)
                    (when (and (nil? (known/zones c)) (not (:ignore-zones? (:args c)))) "no zone list"))]
    (if-not trouble
      {:trees trees}
      (do (ctx/warn-once! c [plan trouble] :forest.declined
                          {:plan plan :part part :reason trouble
                           :text (str "forest declines plan " plan (when part (str " part " part)) ": " trouble)})
          {:trouble trouble}))))

(defn note-cells!
  "Say, once each, which planned cells hold something foreign, cannot take a sapling, or want a sapling not carried."
  [c classes]
  (let [p (:primitives c)
        have (carried-names p)]
    (doseq [[pos [kind species]] classes]
      (case kind
        :foreign (let [found (u/seen-name p pos)]
                   (ctx/warn-once! c [:foreign pos found] :forest.foreign
                                   {:pos pos :found found :wanted species
                                    :text (str "forest leaves " found " at " (pr-str (cell-vec pos)) " (the plan wants a " species " tree)")}))
        (:no-ground :cramped) (ctx/warn-once! c [:unplantable pos kind] :forest.unplantable
                                              {:pos pos :why kind :wanted species
                                               :text (str "forest cannot plant " species " at " (pr-str (cell-vec pos)) ": " (name kind))})
        nil))
    (let [missing (->> classes
                       (filter (fn [[_ [kind species]]] (and (= :bare kind) (not (have (forestry/sapling-of species))))))
                       (group-by (comp second val)))]
      (doseq [[species cells] missing]
        (ctx/warn-once! c [:no-sapling species] :forest.no-sapling
                        {:species species :cells (mapv key cells)
                         :text (str "forest has no " (forestry/sapling-of species) " for " (count cells) " bare planned cells")})))))

(defn classes-of
  "{pos [kind species]} of every planned cell."
  [c trees]
  (into {} (map (fn [[pos species]] [pos [(classify (:primitives c) pos species) species]])) trees))

(defn ripe-cells
  "The planned cells with a grown tree not left, nearest to the body first."
  [c classes]
  (let [left (left-cells c)
        here (u/self-pos c)]
    (->> classes
         (filter (fn [[pos [kind _]]] (and (= :ripe kind) (not (left pos)))))
         (map key)
         (sort-by #(u/dist here %)))))

(defn owed-cells
  "[{:pos :species}] cells to plant: the planned cells standing bare and those this job cut (its :replant debts)
  that are bare now, not left, nearest to the body first."
  [c classes]
  (let [p (:primitives c)
        left (left-cells c)
        here (u/self-pos c)
        planned (keep (fn [[pos [kind species]]] (when (= :bare kind) {:pos pos :species species})) classes)
        cut (keep (fn [{:keys [pos species]}]
                    (when (and (not (contains? classes pos)) (= :bare (classify p pos species)))
                      {:pos pos :species species}))
                  (:replant (ctx/mem c)))]
    (->> (concat planned cut)
         (remove #(left (:pos %)))
         (sort-by #(u/dist here (:pos %))))))

(defn plantable
  "The owed cells whose sapling is carried."
  [c owed]
  (let [have (carried-names (:primitives c))]
    (filterv #(have (forestry/sapling-of (:species %))) owed)))

;; ------------------------------------------------------------------ check

(defn field-cells
  "The planned tree cells of field and the ground under them, as {:x :y :z}."
  [field]
  (mapcat (fn [pos] [pos (update pos :y dec)]) (keys (:trees field))))

(defn ^:async survey-field!
  "Before the first step, look around once when a cell of field was never seen: :continue when it looked, else nil."
  [c field]
  (when (and (not (:begun (ctx/mem c))) (look/unseen? (:primitives c) (field-cells field)))
    (await (look/survey! c))))

(defn check
  "A started job always passes (its finishing round must run); else a grown tree is ripe on a planned cell, or a
  bare one has its sapling carried. With the plan unworkable, never."
  [c]
  (let [field (planned c)]
    (if (:trouble field)
      (ctx/wait c {:reason :plan-trouble :why (:trouble field)})
      (let [classes (classes-of c (:trees field))]
        (note-cells! c classes)
        (or (boolean (or (:begun (ctx/mem c))
                         (seq (ripe-cells c classes))
                         (seq (plantable c (owed-cells c classes)))))
            (look/wait-unless-surveyed c {:reason :nothing-to-do} (field-cells field)))))))

;; ------------------------------------------------------------------ steps

(defn bump [m k] (update m k (fnil inc 0)))

(defn ^:async collect!
  "Step 1: one collect-drops call while a sweep is owed. :again, :continue while it waits."
  [c]
  (when-let [species (:collect (ctx/mem c))]
    (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops
                                   {:radius (:collect-radius (:args c))
                                    :filter (vec (distinct (mapcat forestry/drop-filter species)))}))]
      (when (= :done r) (ctx/update-mem! c dissoc :collect))
      (if (= :continue r) :continue :again))))

(defn fell-slot [pos] (keyword (str "fell-" (:x pos) "-" (:y pos) "-" (:z pos))))

(defn remaining-logs
  "The logs still standing in the column of the tree begun at pos."
  [c pos species]
  (forestry/logs-at (:primitives c) pos species))

(defn column-refusal
  "[log-pos reason] of the first log of the column that may not be dug, nil when all may."
  [c logs]
  (some #(when-let [r (refusal c (:pos %))] [(:pos %) r]) logs))

(defn too-tall? [c logs] (and (not (:pillar? (:args c))) (> (count logs) (:max-logs (:args c)))))

(defn finished-felling!
  "The fell child ended: count the tree felled, or leave it when logs of it still stand; collect either way."
  [c pos species]
  (let [logs (remaining-logs c pos species)]
    (ctx/update-mem! c (fn [m] (-> m (dissoc :cut) (update :collect (fnil conj []) species))))
    (if (seq logs)
      (if-let [bad (column-refusal c logs)]
        (leave! c pos :refused :why (second bad) :log (first bad))
        (leave! c pos :unreachable))
      (ctx/update-mem! c bump :felled))))

(defn fell-args
  "The args of the fell-tree child for the tree of species at pos."
  [c pos species]
  {:at pos :species species :radius 16 :for-plan (:plan (:args c))
   :accept (:accept (:args c)) :pillar? (:pillar? (:args c)) :ignore-zones? (:ignore-zones? (:args c))})

(defn ^:async fell!
  "Steps 2 and 4: go on with the tree begun, else begin the nearest ripe one. :again, :continue while the fell child
  waits, or nil with no tree."
  [c trees classes]
  (let [m (ctx/mem c)
        {:keys [pos species] :as cut} (:cut m)]
    (cond
      cut
      (let [logs (remaining-logs c pos species)
            bad (column-refusal c logs)]
        (cond
          (not= species (get trees pos)) (do (ctx/update-mem! c dissoc :cut) :again)
          bad (do (ctx/update-mem! c dissoc :cut)
                  (leave! c pos :refused :why (second bad) :log (first bad))
                  :again)
          :else
          (let [r (await (ctx/call-child c (fell-slot pos) 'jobs.forestry.fell-tree (fell-args c pos species)))]
            (when (not= :continue r) (finished-felling! c pos species))
            (if (= :continue r) :continue :again))))

      :else
      (when-let [pos (first (ripe-cells c classes))]
        (let [species (get trees pos)
              logs (remaining-logs c pos species)
              bad (column-refusal c logs)]
          (cond
            (too-tall? c logs) (leave! c pos :too-tall :logs (count logs))
            bad (leave! c pos :refused :why (second bad) :log (first bad))
            :else (ctx/update-mem! c (fn [m] (-> m
                                                (assoc :cut {:pos pos :species species})
                                                (update :replant (fnil conj []) {:pos pos :species species}))))))
        :again))))

(defn fail-plant!
  "Count a failed attempt to plant at pos; leave the cell at the third."
  [c pos why]
  (let [n (inc (get-in (ctx/mem c) [:fails pos] 0))]
    (ctx/update-mem! c assoc-in [:fails pos] n)
    (when (>= n (u/max-failures))
      (leave! c pos why))))

(defn settle-debts!
  "Drop the replant debts whose cell no longer wants a sapling: one now holding a sapling or something foreign, or
  with no soil. A cell still holding the tree's log (being felled, or left) keeps its debt."
  [c]
  (let [p (:primitives c)
        debts (:replant (ctx/mem c))
        open (filterv #(not (#{:growing :foreign :no-ground} (classify p (:pos %) (:species %)))) debts)]
    (when (not= (count debts) (count open))
      (ctx/update-mem! c assoc :replant open))))

(defn body-cells
  "The cells the body occupies: feet and head."
  [c]
  (let [[x y z] (feet-cell c)]
    #{[x y z] [x (inc y) z]}))

(defn ^:async step-off!
  "Walk off pos, which the body stands on, so a sapling can go there (jobs.lib.step-off). :again."
  [c pos]
  (let [r (await (step-off/step-off-zoned! c pos {}))]
    (when (:unreachable r)
      (fail-plant! c pos :unreachable))
    :again))

(defn ^:async plant!
  "Step 3: plant at the nearest owed cell whose sapling is carried, the cells under the body last (and walking off
  one when it is the only one). :again, :continue while the walk waits, or nil."
  [c classes]
  (let [under (body-cells c)
        owed (sort-by #(contains? under (cell-vec (:pos %))) (plantable c (owed-cells c classes)))]
    (when-let [{:keys [pos species]} (first owed)]
      (let [item (forestry/sapling-of species)
            w (await (near/go-near! c pos 3 {:zone-tolls true}))]
        (cond
          (= :partial w) :continue
          (= :blocked w) (do (fail-plant! c pos :unreachable) :again)
          (contains? under (cell-vec pos)) (await (step-off! c pos))
          :else
          (if-let [why (place-refusal c pos)]
            (do (leave! c pos :refused :why why)
                :again)
            (let [outcome (await (blocks/place-cell! c pos item {:for-plan (:plan (:args c))
                                                                 :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
              (case outcome
                :continue :continue
                (:placed :already) (do (ctx/update-mem! c bump :planted)
                                       (ctx/forget-where! c forestry/replant-kind #(= pos (:pos %)))
                                       :again)
                (:no-item :occupied) (do (fail-plant! c pos outcome) :again)
                :need (do (fail-plant! c pos :no-item) :again)
                (do (fail-plant! c pos :failed) :again)))))))))

(defn finish!
  "Step 5: say what was done and be done."
  [c classes]
  (let [m (ctx/mem c)
        bare (vec (keep (fn [[pos [kind _]]] (when (= :bare kind) pos)) classes))
        left (vec (sort-by (juxt :x :y :z) (left-cells c)))
        result {:felled (:felled m 0) :planted (:planted m 0) :left left :bare bare}]
    (ctx/emit! c :forest.done :info
               (assoc result :text (str "forest done: felled " (:felled result) ", planted " (:planted result)
                                        ", left " (count left) ", bare " (count bare))))
    (ctx/result! c result)
    :done))

(defn ^:async step [c]
  (let [field (planned c)]
    (if (:trouble field)
      :declined
      (let [trees (:trees field)]
        (or (await (survey-field! c field))
            (do (when-not (:begun (ctx/mem c))
                  (ctx/update-mem! c assoc :begun true))
                (settle-debts! c)
                (let [classes (classes-of c trees)]
                  (note-cells! c classes)
                  (or (await (collect! c))
                      (when (:cut (ctx/mem c)) (await (fell! c trees classes)))
                      (await (plant! c classes))
                      (await (fell! c trees classes))
                      (finish! c classes)))))))))

(def max-steps "Steps of one call before it gives the round back with :continue." 400)

(defn ^:async round [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) max-steps) (step c) :continue)))))
