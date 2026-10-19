(ns jobs.gather.mine.drops
  "The drops of jobs.gather.mine: digging one cell, then collecting what it dropped (a look down the column, a
  collect-drops child, a walk or a short cut beside the dug cell) and telling what was left or lost."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.gather.mine.targets :refer [carried book-ground! drop-radius left-behind]]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.near :as near]
            [jobs.lib.result :as result]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]))

(a/defargs settings
  {::pickup-grace-ms {:default 400 :doc "How long the body waits for a drop to be picked up, in ms." :spec (a/int-in 0 nil)}
   ::drop-cut-max {:default 4 :doc "The most cells dug to open a way beside a dug cell whose drop did not come in and that no walk gets beside; 0: no such cut." :spec (a/int-in 0 nil)}})

(defn pickup-grace-ms [] (settings/get settings ::pickup-grace-ms))

(defn ^:async note-left!
  "Report drops of the item collect-drops left lying (out of reach), once per cell. A drop still listed right after
  the collect may be one whose pickup packet has not arrived, so a drop counts only when it is still lying after a
  short wait. why maps a cell to the reason collect-drops left it, shown when known."
  [c why]
  (let [known (:left (ctx/mem c))
        fresh? (fn [[pos _]] (not (contains? known pos)))
        first-look (into {} (filter fresh?) (left-behind c))
        fresh (if (empty? first-look)
                first-look
                (do (await (ctx/act c :wait #js {:ms (pickup-grace-ms)}))
                    (into {} (filter (fn [[pos _]] (contains? first-look pos))) (left-behind c))))]
    (when (seq fresh)
      (ctx/emit! c :mine.left-behind :info {:items (mapv (fn [[pos n]] (cond-> {:pos pos :count n} (why pos) (assoc :reason (why pos)))) fresh)
                                            :text (str "mine left drops it could not pick up at "
                                                       (str/join " " (map (fn [[pos _]] (str/join "," pos)) fresh)))})
      (ctx/update-mem! c update :left merge fresh))
    fresh))

(defn drop-lost!
  "A dig after which the carried count did not rise and no drop of the item lies in sight: said once per dug cell, and
  kept for the result's :lost."
  [c]
  (let [{:keys [dug-at lost]} (ctx/mem c)]
    (when (and dug-at (not (some #{dug-at} lost)))
      (ctx/emit! c :mine.drop-lost :info {:cell dug-at
                                          :text (str "mine dug at " (str/join "," dug-at) ": nothing came in and no drop is in sight")})
      (ctx/update-mem! c update :lost (fnil conj []) dug-at))))

(defn ^:async look-for-drop!
  "Once per dug cell, glance at the dug cell and the cell under it, then down the column at each unsensed cell (jobs.lib.dig-look),
  where a drop of the dig that fell may lie out of sight."
  [c]
  (let [dug (:dug-at (ctx/mem c))]
    (when (and dug (not= dug (:drop-looked (ctx/mem c))))
      (ctx/update-mem! c assoc :drop-looked dug)
      (await (dig-look/look-at! c dug))
      (await (dig-look/look-at! c (update dug 1 dec)))
      (await (dig-look/look-unknown! c (map #(update dug 1 - %) [2 3 4]))))))

(defn ^:async dig-cell!
  "Dig pos with a jobs.blocks.dig child: this job's rules judged the cell, the hazards it accepts are the child's."
  [c pos]
  (book-ground! c pos)
  (await (blocks/dig-cell! c pos {:accept (set (:accept (:args c))) :ignore-zones? (boolean (:ignore-zones? (:args c)))})))

(defn drop-cut-max [] (settings/get settings ::drop-cut-max))

(defn drop-cut
  "The cells to dig, in walking order, for the cheapest way (jobs.lib.dig-route, falls of 1 at most, a cell never seen
  taken for stone) from the body's cell to a stand within 1 of the dug cell, at most max-digs of them; nil with no such way."
  [c dug max-digs]
  (let [p (:primitives c)
        at (fn [[x y z]] (or (u/seen-name p {:x x :y y :z z}) "stone"))
        from (u/cell-vec (u/self-pos c))]
    (->> [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 -1 0] [0 1 0]]
         (keep #(:dig (dig-route/route at from (mapv + dug %) {:max-digs max-digs :max-fall 1})))
         (filter #(<= (count %) max-digs))
         (sort-by count)
         first)))

(defn ^:async cut-to-drop!
  "Open a way beside the dug cell (drop-cut), one dig child per cell, at most ::drop-cut-max tries per dug cell; the
  way is planned again after each dig and a look at what it laid open. :cut (open: walk again), :partial (a dig waits on
  the world) or nil (no way, or a dig failed)."
  [c dug]
  (loop []
    (let [n (get-in (ctx/mem c) [:drop-cut dug] 0)
          cells (drop-cut c dug (- (drop-cut-max) n))]
      (cond
        (nil? cells) nil
        (empty? cells) :cut
        :else (let [cell (first cells)
                    o (await (dig-cell! c (zipmap [:x :y :z] cell)))]
                (case o
                  :continue :partial
                  (:dug :missing) (do (ctx/update-mem! c assoc :drop-cut {dug (inc n)})
                                      (await (dig-look/look-at! c cell))
                                      (await (dig-look/look-at! c (update cell 1 dec)))
                                      (recur))
                  nil))))))

(defn lip-point
  "Where the body leans out over the cell it stands beside: its own cell's edge toward dug (a cell [x y z] at its level),
  nil when dug is not such a cell or has a seen floor. From the middle of a cell the rim hides what lies against the wall under the dug cell."
  [c dug]
  (let [[x y z] (u/cell-vec (u/self-pos c))
        [dx dy dz] (map - dug [x y z])]
    (when (and (zero? dy) (= 1 (+ (js/Math.abs dx) (js/Math.abs dz)))
               (let [under (u/seen-name (:primitives c) (zipmap [:x :y :z] (update dug 1 dec)))]
                 (or (nil? under) (rules/air under))))
      {:x (+ x 0.5 (* 0.4 dx)) :y y :z (+ z 0.5 (* 0.4 dz))})))

(defn ^:async peek-over-lip!
  "A dug cell the body stands beside whose drop is still not in sight: once per dug cell, a step to the edge of its own cell
  toward it (walk/centre!: go-to works in cells and would not leave this one), so the drop at the foot of a pit wall shows.
  :peeked after the step, nil when there is none to take."
  [c dug]
  (when-let [pt (and (not= dug (:lip-peeked (ctx/mem c))) (lip-point c dug))]
    (ctx/update-mem! c assoc :lip-peeked dug)
    (await (walk/centre! c (:x pt) (:z pt) 0.12))
    :peeked))

(defn ^:async look-floor-toward!
  "The floor cells under the line from the body to dug (a cell short of it) that the body has not sensed, looked at once per dug
  cell: a walk plans only over floors it has seen. True when it looked at any."
  [c dug]
  (when (not= dug (:floor-looked (ctx/mem c)))
    (ctx/update-mem! c assoc :floor-looked dug)
    (let [[x y z] (u/cell-vec (u/self-pos c))
          [dx _ dz] (map - dug [x y z])
          n (max (js/Math.abs dx) (js/Math.abs dz))
          cells (for [i (range 1 n)] [(+ x (js/Math.round (* dx (/ i n)))) (dec y) (+ z (js/Math.round (* dz (/ i n))))])
          unseen (filter #(dig-look/unknown? (:primitives c) %) cells)]
      (await (dig-look/look-unknown! c unseen))
      (boolean (seq unseen)))))

(defn ^:async walk-to-drop!
  "A dig whose drop did not come in: once per dug cell, walk within 1 of it (near/go-near!, no escalation), where a drop
  out of sight or out of reach from the dig spot is seen and picked up; a dug cell no walk gets beside (dug from a
  distance over a wall) is cut to first (cut-to-drop!; not with :stay-level). :walked (look and collect again), :partial
  (go-to or a dig waits) or nil (no walk: it came in, the cell was walked to already, or the walk was blocked)."
  [c]
  (let [{:keys [dug-at drop-walked last-carried]} (ctx/mem c)]
    (when (and dug-at (not= dug-at drop-walked) (<= (carried c) (or last-carried 0)))
      (let [pos (zipmap [:x :y :z] dug-at)
            go! #(near/go-near! c pos 1 {:ways #{:walk :clear-seal}})
            w (await (go!))
            w (if (and (= :blocked w) (await (look-floor-toward! c dug-at))) (await (go!)) w)
            cut (when (and (= :blocked w) (not (:stay-level (:args c))) (let [res (near/last-walk c)] (or (result/unreachable-walk? res) (= :detour (result/why res)))))
                  (await (cut-to-drop! c dug-at)))
            w (case cut :cut (await (go!)) :partial :partial w)]
        (when (= :there w) (await (peek-over-lip! c dug-at)))
        (when-not (= :partial w) (ctx/update-mem! c assoc :drop-walked dug-at))
        (when (= :there w) (ctx/update-mem! c dissoc :drop-looked))
        (case w :partial :partial :there :walked nil)))))

(defn ^:async collect! [c]
  (await (look-for-drop! c))
  (let [r (await (ctx/call c :collect 'jobs.items.collect-drops
                           (cond-> {:radius (drop-radius c) :items [(blocks/item-name (:args c))] :dig true}
                             (:dug-at (ctx/mem c)) (assoc :holes [(:dug-at (ctx/mem c))]))))
        why (into {} (map (fn [{:keys [pos reason]}] [[(:x pos) (:y pos) (:z pos)] reason])) (when (map? r) (:left (:data r))))
        w (when (not= :continue r) (await (walk-to-drop! c)))]
    (when (and (not= :continue r) (nil? w))
      (when (and (empty? (await (note-left! c why))) (<= (carried c) (:last-carried (ctx/mem c) 0)))
        (drop-lost! c))
      (let [now (carried c)]
        (ctx/update-mem! c (fn [m]
                             (-> m
                                 (dissoc :collecting)
                                 (assoc :dry (if (> now (:last-carried m 0)) 0 (inc (:dry m 0)))
                                        :last-carried now))))))
    (if (or (= :continue r) (= :partial w)) :continue :again)))
