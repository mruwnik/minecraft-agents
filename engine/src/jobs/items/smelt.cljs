(ns jobs.items.smelt
  (:require [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.look :as look]
            [engine.jobs.util :as u]
            [engine.path.near :as near]))

(def doc
  "Smelt count of an item in a furnace, blast furnace or smoker without
  standing by it. The first round walks to the furnace, reads it, takes any
  output already there, and loads the input and the fuel (worked out from the
  count at one fuel item per 8 coal-items, 1.5 for wood, 0.5 for a stick;
  taken from what is carried when :fuel is nil, coal and charcoal first). Then
  the job ends its round and its check declines until the cook should be done
  (job.waiting reason cooking, with :furnace and :ready-at), so the body does other jobs meanwhile; the next round takes the output. The
  wait is decided by the clock, not by the furnace: ready-at is now plus the
  cook time of the items loaded (200 ticks each in a furnace, 100 in the other
  two) plus a margin; the check also wakes the job early once :unlit-from has
  passed and the furnace block is no longer lit (the fuel ran out or the cook
  ended) or no longer a furnace (broken). The check reads only memory, the clock and one block, no
  window. A woken round that finds the cook unfinished (the chunk was unloaded,
  say) waits again from the furnace's own bars. Memory keeps what is owed
  (:owed {:item :count}, :got, :ready-at, :unlit-from) and :target, the input
  the furnace must hold after the load, so a restart or a cut before the load
  was noted neither loses the output nor loads twice. Ends with a result
  {:smelted n :wanted n}, plus :reason after a warn (smelt.gave-up) when it
  gives up: no-furnace-seen (no :furnace given and none seen), no-furnace, not-a-furnace, furnace-gone (broken or replaced meanwhile), output-gone (input and output both gone: someone emptied it),
  unreachable, no-item, nothing-smeltable, not-smeltable (the kind of furnace
  cannot cook it, also found when it never lit), no-fuel, unknown-fuel,
  out-of-fuel (leftover input is taken back), furnace-busy (the input slot
  holds another item), furnace-full, fuel-busy (the fuel slot holds another
  fuel), inventory-full (the output stays in the furnace), rejected-fuel.
  Output found in the furnace at the start is taken and does not count. A furnace in another's zone or claim that
  does not allow :take is not touched: the round ends with reason \"refused\" (and :zones, :claims) after one
  smelt.gave-up warn, before anything is loaded or taken (:ignore-zones? lifts it).")

(def args
  {:furnace {:doc "furnace, blast furnace or smoker position {:x :y :z}; when nil the first round takes the nearest one the body has seen (perception's memory, never x-ray) within 32 blocks that is still there and cooks :item (any kind when :item is nil), emits smelt.furnace naming it and keeps it; with none seen the job ends with reason no-furnace-seen" :default nil}
   :item {:doc "what to smelt; the first smeltable thing carried when nil" :default nil}
   :count {:doc "how many; all carried (at most one stack) when nil" :default nil}
   :fuel {:doc "fuel item to load; the best carried when nil" :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def tick-ms 50)
(def slack-ms 2000)
(def grace-ms 6000)
(def recheck-ms 6000)
(def slot-max 64)
(def seen-radius 32)

;; ------------------------------------------------------------------ what burns and what cooks

(def cook-ticks {"furnace" 200 "blast_furnace" 100 "smoker" 100})

(def foods #{"beef" "porkchop" "chicken" "mutton" "rabbit" "cod" "salmon" "potato" "kelp"})
(def furnace-only #{"cobblestone" "cobbled_deepslate" "stone" "sand" "red_sand" "clay_ball" "clay" "netherrack"
                    "sandstone" "red_sandstone" "stone_bricks" "quartz_block" "sea_pickle" "cactus" "chorus_fruit"})

(defn ore? [name]
  (boolean (or (re-find #"_ore$" name) (re-find #"^raw_(iron|gold|copper)$" name) (= "ancient_debris" name))))

(defn smelts?
  "Whether a kind of furnace (the block name) cooks the item."
  [kind name]
  (boolean
   (case kind
     "smoker" (contains? foods name)
     "blast_furnace" (ore? name)
     "furnace" (or (ore? name) (contains? foods name) (contains? furnace-only name)
                   (boolean (re-find #"_(log|wood)$" name)))
     false)))

(defn fuel-per-unit
  "How many items one fuel item smelts, in any of the three kinds, or nil
  when it is no fuel this job knows."
  [name]
  (cond
    (#{"coal" "charcoal"} name) 8
    (= "coal_block" name) 80
    (= "blaze_rod" name) 12
    (= "dried_kelp_block" name) 20
    (re-find #"_(planks|log|wood)$" name) 1.5
    (= "stick" name) 0.5))

(defn fuel-rank
  "Lower burns first: coal and charcoal, then wood, then sticks, then the rest."
  [name]
  (cond (#{"coal" "charcoal"} name) 0
        (re-find #"_(planks|log|wood)$" name) 1
        (= "stick" name) 2
        :else 3))

;; ------------------------------------------------------------------ the plan

(defn carried-count [carried name]
  (transduce (comp (filter #(= name (:name %))) (map :count)) + 0 carried))

(defn existing-capacity
  "How many items the fuel already in the furnace will cook: the burning fuel's bar and the fuel slot."
  [kind state]
  (let [slot (:fuel state)]
    (+ (js/Math.floor (/ (get-in state [:burn :left] 0) (cook-ticks kind)))
       (if slot (js/Math.floor (* (or (fuel-per-unit (:name slot)) 0) (:count slot))) 0))))

(defn pick-fuel
  "The fuel item to load: the given one, else the one already in the slot when carried, else the best carried.
  Never the item being smelted. {:give-up reason} when there is none."
  [{:keys [carried item fuel state]}]
  (let [usable (fn [name] (and (not= name item) (fuel-per-unit name) (pos? (carried-count carried name))))
        slot-name (:name (:fuel state))]
    (cond
      fuel (cond (nil? (fuel-per-unit fuel)) {:give-up "unknown-fuel"}
                 (usable fuel) {:fuel fuel}
                 :else {:give-up "no-fuel"})
      (and slot-name (usable slot-name)) {:fuel slot-name}
      :else (if-let [best (->> carried (map :name) distinct (filter usable) (sort-by fuel-rank) first)]
              {:fuel best}
              {:give-up "no-fuel"}))))

(defn fuel-plan
  "Fuel for n more items on top of e already in the slot. {:count n :fuel {:item :count}|nil}, with n cut to what
  the fuel covers (n 0 is a load already made that only needs its fire fed), or {:give-up reason}."
  [{:keys [kind state carried] :as m} n e]
  (let [existing (existing-capacity kind state)
        deficit (- (+ e n) existing)]
    (if (<= deficit 0)
      {:count n :fuel nil}
      (let [picked (pick-fuel m)
            name (:fuel picked)
            slot (:fuel state)]
        (cond
          (and (:give-up picked) (> existing e)) {:count (min n (- existing e)) :fuel nil}
          (:give-up picked) picked
          (and slot (not= (:name slot) name)) {:give-up "fuel-busy"}
          :else (let [per (fuel-per-unit name)
                      units (min (js/Math.ceil (/ deficit per)) (carried-count carried name))
                      covered (- (js/Math.floor (+ existing (* units per))) e)
                      n' (max 0 (min n covered))]
                  (if (and (pos? n) (zero? n'))
                    {:give-up "no-fuel"}
                    {:count n' :fuel {:item name :count (min units (js/Math.ceil (/ (- (+ e n') existing) per)))}})))))))

(defn plan-load
  "What to put in the furnace: {:item :count :fuel {:item :count}|nil}, or {:give-up reason}.
  m: :kind and :state (a read of the furnace), :carried ({:name :count} each), :item :count :fuel from the args,
  :target (the input the furnace must hold, set once a load was planned, so a resumed load adds only the rest)."
  [{:keys [state carried item count target] :as m}]
  (let [kind (:kind state)
        item (or item (some #(when (smelts? kind (:name %)) (:name %)) carried))
        have (carried-count carried item)
        input (:input state)
        e (if (= item (:name input)) (:count input) 0)
        room (- slot-max e)
        wanted (cond target (max 0 (- target e)) :else (min (or count have) have room))
        n (if target (min wanted have) wanted)]
    (cond
      (nil? item) {:give-up "nothing-smeltable"}
      (and (not target) (zero? have)) {:give-up "no-item"}
      (not (smelts? kind item)) {:give-up "not-smeltable"}
      (and input (not= item (:name input))) {:give-up "furnace-busy"}
      (and (not target) (<= room 0)) {:give-up "furnace-full"}
      :else (let [r (fuel-plan (assoc m :item item :kind kind) (if target (max n 0) n) e)]
              (if (:give-up r) r (assoc r :item item))))))

(defn judge
  "What a visit found: :take (output to collect), :wait (input cooking), :not-smeltable (input, fuel, no fire),
  :out-of-fuel (input, nothing to burn) or :done."
  [state]
  (cond
    (:output state) :take
    (and (:input state) (:lit state)) :wait
    (and (:input state) (:fuel state)) :not-smeltable
    (:input state) :out-of-fuel
    :else :done))

(defn wait-ms
  "How long until the items in the input are cooked: what the cook bar still has to count, from the bar when it is
  known, plus a margin."
  [kind state]
  (let [per (cook-ticks kind)
        left (- (* (get-in state [:input :count] 0) per) (get-in state [:cook :done] 0))]
    (+ slack-ms (* tick-ms (max 0 left)))))

;; ------------------------------------------------------------------ the check

(def furnace-block? #{"furnace" "blast_furnace" "smoker"})

(defn furnace-of
  "The furnace this job uses: the :furnace arg, else the one its first round chose (job memory), else nil."
  [c]
  (or (:furnace (:args c)) (:furnace (ctx/mem c))))

(defn nearest-furnace
  "The nearest furnace, blast furnace or smoker the body has seen (perception's seenBlocks: memory of what it saw,
  never x-ray) within seen-radius, still that block now, that cooks item (any kind when item is nil); nil when none."
  [c item]
  (let [p (:primitives c)]
    (when-let [f (aget p "seenBlocks")]
      (some (fn [b]
              (let [pos (u/pos-of (.-pos b))
                    kind (.-name b)]
                (when (and (or (nil? item) (smelts? kind item))
                           (= kind (u/block-name p pos)))
                  pos)))
            (array-seq (.call f p #js {:radius seen-radius :names (clj->js (vec furnace-block?)) :max 16}))))))

(defn needs-attention?
  "Whether the block at pos is loaded and is no longer a lit furnace: it is not lit (the fuel ran out, or the cook
  is over) or it is not a furnace any more (broken, replaced)."
  [p pos]
  (let [block (.blockAt p (clj->js pos))]
    (and (some? block)
         (or (not (furnace-block? (.-name block)))
             (false? (some-> (.-properties block) .-lit))))))

(defn check
  "The cook is due: no furnace chosen yet (the round chooses one), nothing loaded yet, or the clock passed ready-at,
  or since :unlit-from the furnace block is unlit or gone. Else it waits with reason :cooking. Memory, the clock and
  one block: no window is opened."
  [c]
  (let [furnace (furnace-of c)
        m (ctx/mem c)
        now (ctx/now c)]
    (or (nil? furnace)
        (nil? (:ready-at m))
        (>= now (:ready-at m))
        (and (>= now (:unlit-from m)) (needs-attention? (:primitives c) furnace))
        (ctx/wait c {:reason :cooking :furnace furnace :ready-at (:ready-at m)}))))

;; ------------------------------------------------------------------ the round

(defn ^:async visit!
  "One furnace visit through act, as a cljs map."
  [c op extra]
  (let [r (await (ctx/act c :furnace (clj->js (merge {:pos (furnace-of c) :op op} extra))))]
    (js->clj r :keywordize-keys true)))

(defn wait-until!
  "Remember when to look again and end the round."
  [c ms]
  (let [now (ctx/now c)]
    (ctx/update-mem! c assoc :ready-at (+ now ms) :unlit-from (+ now grace-ms))
    :continue))

(defn finish!
  "Hand the parent what was smelted and return :done."
  [c extra]
  (let [m (ctx/mem c)]
    (ctx/result! c (merge {:smelted (:got m 0) :wanted (:count (:owed m) 0)} extra))
    :done))

(defn stop!
  "Warn and finish with a reason."
  [c reason]
  (ctx/emit! c :smelt.gave-up :warn {:reason reason :text (str "smelt gave up: " reason)})
  (finish! c {:reason reason}))

(defn ^:async stop-back!
  "Give up after a load: leftover input goes back to the pockets first."
  [c reason]
  (await (visit! c "take" {:output false :input true}))
  (stop! c reason))

(defn refuse!
  "End with reason \"refused\": the furnace is in another's zone or claim; one smelt.gave-up warn names them."
  [c verdict]
  (let [fields (access/refusal-fields [verdict])]
    (ctx/emit! c :smelt.gave-up :warn (assoc fields :reason "refused" :text (str "smelt gave up: refused by " (access/refusal-text fields))))
    (finish! c (select-keys fields [:zones :claims :reason]) )))

(defn give-up!
  "Count a failed round in job memory: :continue until u/max-failures, then stop with the reason."
  [c reason]
  (let [tries (inc (:failures (ctx/mem c) 0))]
    (ctx/update-mem! c assoc :failures tries)
    (if (< tries u/max-failures)
      :continue
      (stop! c reason))))

(defn carried [c] (u/inventory (:primitives c)))

(defn ^:async load!
  "Put the planned input and fuel in. :continue when they are in, else the reason it gives up."
  [c state plan]
  (let [{:keys [item count fuel]} plan
        m (ctx/mem c)
        e (if (= item (get-in state [:input :name])) (get-in state [:input :count]) 0)
        target (or (:target m) (+ e count))
        _ (ctx/update-mem! c assoc :target target :owed {:item item :count target})
        load (cond-> {}
               (pos? count) (assoc :input {:item item :count count})
               fuel (assoc :fuel {:item (:item fuel) :count (:count fuel)}))
        r (if (empty? load) {:status "ok"} (await (visit! c "load" load)))]
    (case (:status r)
      "ok" (do (ctx/emit! c :smelt.loaded :info {:text (str "smelting " target " " item) :item item :count target :fuel (:item fuel)})
               (wait-until! c (+ slack-ms (* tick-ms target (cook-ticks (:kind state))))))
      "busy" (stop! c (if (= "input" (:slot r)) "furnace-busy" "fuel-busy"))
      "no-item" (stop! c "no-item")
      "rejected" (stop! c (if (= "fuel" (:slot r)) "rejected-fuel" "furnace-full"))
      (stop! c (str "load " (:status r))))))

(defn ^:async start!
  "The first rounds: plan, take what the furnace holds already, load."
  [c state]
  (let [m (ctx/mem c)
        {:keys [item count fuel]} (:args c)
        plan (plan-load {:state state :carried (carried c) :item item :count count :fuel fuel :target (:target m)})]
    (if (:give-up plan)
      (stop! c (:give-up plan))
      (let [freed (when (:output state) (await (visit! c "take" {})))]
        (if (= "full" (:status freed))
          (stop! c "inventory-full")
          (await (load! c state plan)))))))

(defn ^:async take!
  "Take the output with one furnace visit and add what was taken to :got. Returns the visit result: status \"full\" when the inventory has no room, :input while the furnace still holds input."
  [c]
  (let [r (await (visit! c "take" {}))
        got (reduce + 0 (map :count (:taken r)))]
    (ctx/update-mem! c update :got (fnil + 0) got)
    r))

(defn ^:async feed!
  "The fire is out with input left: put fuel in for what remains, or give the input back."
  [c state]
  (let [m (ctx/mem c)
        item (:item (:owed m))
        remaining (get-in state [:input :count])
        plan (plan-load {:state state :carried (carried c) :item item :fuel (:fuel (:args c)) :target remaining})]
    (if (or (:give-up plan) (nil? (:fuel plan)))
      (await (stop-back! c "out-of-fuel"))
      (let [r (await (visit! c "load" {:fuel {:item (:item (:fuel plan)) :count (:count (:fuel plan))}}))]
        (if (= "ok" (:status r))
          (wait-until! c (wait-ms (:kind state) state))
          (await (stop-back! c "out-of-fuel")))))))

(defn ^:async collect!
  "The cook is due: take the output, then wait again, feed the fire, give up, or finish."
  [c state]
  (case (judge state)
    :take (let [r (await (take! c))]
            (cond
              (= "full" (:status r)) (stop! c "inventory-full")
              (:input r) (wait-until! c recheck-ms)
              :else (do (ctx/emit! c :smelt.done :info {:text (str "smelted " (:got (ctx/mem c))) :smelted (:got (ctx/mem c))})
                        (finish! c {}))))
    :wait (wait-until! c (wait-ms (:kind state) state))
    :out-of-fuel (await (feed! c state))
    :not-smeltable (await (stop-back! c "not-smeltable"))
    :done (let [m (ctx/mem c)]
            (if (< (:got m 0) (:count (:owed m) 0))
              (stop! c "output-gone")
              (do (ctx/emit! c :smelt.done :info {:text (str "smelted " (:got m)) :smelted (:got m)})
                  (finish! c {}))))))

(defn ^:async round-at!
  "Reach the furnace, read it, then load (first) or collect."
  [c furnace owed?]
  (case (await (near/walk-near! c furnace 3))
    :partial :continue
    :blocked (give-up! c "unreachable")
    (let [state (await (visit! c "read" {}))]
      (case (:status state)
        "ok" (if (and owed? (:ready-at (ctx/mem c)))
               (await (collect! c state))
               (await (start! c state)))
        "missing" (stop! c (if owed? "furnace-gone" "no-furnace"))
        "cannot" (stop! c (if owed? "furnace-gone" "not-a-furnace"))
        "unreachable" (give-up! c "unreachable")
        (stop! c (str "furnace " (:status state)))))))

(defn ^:async round-with
  "The round once the furnace is known."
  [c furnace]
  (let [owed? (some? (:owed (ctx/mem c)))]
    (if-let [v (access/container-refusal c :take furnace)]
      (refuse! c v)
      (await (round-at! c furnace owed?)))))

(defn ^:async round
  "One bounded step: reach the furnace, read it, then load (first) or collect. Without a :furnace the first round
  chooses the nearest seen one (kept in memory); with none in view it looks around once from where it stands (the
  four headings, level and down, a sight pass after each) and chooses again; none seen after that ends no-furnace-seen."
  [c]
  (if-let [furnace (furnace-of c)]
    (await (round-with c furnace))
    (if-let [pos (nearest-furnace c (:item (:args c)))]
      (do (ctx/update-mem! c assoc :furnace pos)
          (ctx/emit! c :smelt.furnace :info {:furnace pos :text (str "smelting at the furnace seen at " (:x pos) " " (:y pos) " " (:z pos))})
          (await (round-with c pos)))
      (if (look/looked-here? c)
        (stop! c "no-furnace-seen")
        (await (look/look-around! c))))))
