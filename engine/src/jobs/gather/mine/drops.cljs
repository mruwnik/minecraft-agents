(ns jobs.gather.mine.drops
  "The drops of jobs.gather.dig-for: digging one cell, then collecting what it dropped (a look down the column, a
  collect-drops child, a walk or a short cut beside the dug cell) and telling what was left or lost."
  (:require [jobs.gather.mine.way-set :as ways]
            [jobs.lib.block-sets :as bs]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.gather.mine.targets :refer [carried book-ground! drop-radius items left-behind]]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.waits :as waits]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.dig-route :as dig-route]
            [jobs.lib.near :as near]
            [jobs.lib.result :as result]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]))

(a/defargs settings
  {::pickup-grace-ms {:default 400 :doc "How long the body waits for a drop to be picked up, in ms." :spec (a/int-in 0 nil)}
   ::drop-cut-max {:default 4 :doc "The most cells dug to open a way beside a dug cell whose drop did not come in and that no walk gets beside; 0: no such cut." :spec (a/int-in 0 nil)}
   ::target-cut-max {:default 12 :doc "The most cells dug to open a way beside a seen ore that no walk gets beside; 0: no such cut." :spec (a/int-in 0 nil)}})

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

(def lost-range
  "Blocks from the dug cell within which a dig that brought nothing and shows no drop is declared lost: a look from farther says nothing."
  8)

(defn drop-verdict
  "What is said of a dig that brought nothing and shows no drop, the body being dist blocks from the dug cell: :lost within
  lost-range, :unreached beyond it, nil when it was said already (dug-at is in said-lost or said-unreached)."
  [dug-at dist said-lost said-unreached]
  (cond
    (nil? dug-at) nil
    (some #{dug-at} said-lost) nil
    (<= dist lost-range) :lost
    (some #{dug-at} said-unreached) nil
    :else :unreached))

(defn drop-lost!
  "A dig after which the carried count did not rise and no drop of the item lies in sight, judged from within lost-range of
  the dug cell: said once per dug cell, and kept for the result's :lost. From farther away the drop is only unreached (said once)."
  [c]
  (let [{:keys [dug-at lost drops-unreached]} (ctx/mem c)
        cell (str/join "," dug-at)]
    (case (drop-verdict dug-at (u/dist (u/self-pos c) (zipmap [:x :y :z] dug-at)) lost drops-unreached)
      :lost (do (ctx/emit! c :mine.drop-lost :info {:cell dug-at :text (str "mine dug at " cell ": nothing came in and no drop is in sight")})
                (ctx/update-mem! c update :lost (fnil conj []) dug-at))
      :unreached (do (ctx/emit! c :mine.drop-unreached :info {:cell dug-at :text (str "mine dug at " cell ": no way back to look for the drop")})
                     (ctx/update-mem! c update :drops-unreached (fnil conj []) dug-at))
      nil)))

(defn ^:async dig-cell!
  "Dig pos with a jobs.blocks.dig child: this job's rules judged the cell, the hazards it accepts are the child's."
  [c pos]
  (book-ground! c pos)
  (await (blocks/dig-cell! c pos {:accept (ways/accept (:args c)) :ignore-zones? (boolean (:ignore-zones? (:args c)))})))

(defn drop-cut-max [] (settings/get settings ::drop-cut-max))
(defn target-cut-max [] (settings/get settings ::target-cut-max))

(defn drop-cut
  "The cells to dig, in walking order, for the cheapest way (jobs.lib.dig-route priced as go-to's, falls of 1 at most, a cell
  never seen taken for stone) from the body's cell to a stand within 1 of the dug cell, at most max-digs of them; nil with
  no such way, :searching while a search that spent its nodes goes on (kept: ask again)."
  [c dug max-digs]
  (let [p (:primitives c)
        at (fn [[x y z]] (or (u/seen-name p {:x x :y y :z z}) "stone"))
        prices (assoc (dig-route/body-prices p (fn [[x y z]] (nil? (u/seen-name p {:x x :y y :z z})))) :max-digs max-digs :max-fall 1)
        from (u/cell-vec (u/self-pos c))
        routes (mapv #(let [goal (mapv + dug %)]
                        (dig-route/route-resumed [(:id c) :drop-cut from goal max-digs] at from goal prices))
                     [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 -1 0] [0 1 0]])]
    (or (->> routes (filter :dig) (sort-by :seconds) (map :dig) first)
        (when (some #{:searching} (map :problem routes)) :searching))))

(defn ^:async cut-to-drop!
  "Open a way beside the cell dug (drop-cut), one dig child per cell, at most cap tries per cell (a dug cell's drop: ::drop-cut-max; a seen ore: ::target-cut-max); the
  way is planned again after each dig and a look at what it laid open. :cut (open: walk again), :partial (a dig waits on
  the world) or nil (no way, or a dig failed)."
  [c dug cap]
  (loop []
    (let [n (get-in (ctx/mem c) [:drop-cut dug] 0)
          cells (drop-cut c dug (- cap n))]
      (cond
        (nil? cells) nil
        (= :searching cells) :partial
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
                 (or (nil? under) (bs/air under))))
      {:x (+ x 0.5 (* 0.6 dx)) :y y :z (+ z 0.5 (* 0.6 dz))})))

(defn roof-cell
  "The cell over dug when it is solid: the 1.8-tall body cannot lean into a pocket with a roof, so the lean stops at the edge."
  [c dug]
  (let [roof (update dug 1 inc)
        n (u/seen-name (:primitives c) (zipmap [:x :y :z] roof))]
    (when (and n (not (bs/air n))) roof)))

(defn ^:async peek-over-lip!
  "A dug cell the body stands beside whose drop is still not in sight: once per dug cell, a step to the edge of its own cell
  toward it (walk/centre!: go-to works in cells and would not leave this one), so the drop at the foot of a pit wall shows.
  A solid cell over dug is dug first (roof-cell). :peeked after the step, nil when there is none to take."
  [c dug]
  (when-let [pt (and (not= dug (:lip-peeked (ctx/mem c))) (lip-point c dug))]
    (ctx/update-mem! c assoc :lip-peeked dug)
    (when-let [roof (roof-cell c dug)]
      (await (dig-cell! c (zipmap [:x :y :z] roof))))
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

(def search-ended-whys
  "go-to's :why of a walk whose search ended without a verdict on the cell (a big open cave ate the budget): the dug cell may still be walled in."
  #{:budget :search-spent :moved-while-searching :no-progress})

(defn cut-after?
  "Whether the blocked walk res (go-to's ending) leaves a cut beside the dug cell worth trying: a verdict on the cell, a detour,
  or a search that ran out before it had one."
  [res]
  (boolean (or (result/unreachable-walk? res)
               (contains? (conj search-ended-whys :detour) (result/why res)))))

(def drop-walk-opts "No dive: a drop sunk into deep water is not swum after (the card-9d1e1c07 rule of the walk to dig)." {:ways #{:walk :clear-seal} :dive false})

(defn ^:async walk-to-drop!
  "A dig whose drop did not come in: once per dug cell, walk within 1 of it (near/go-near!, no escalation), where a drop
  out of sight or out of reach from the dig spot is seen and picked up; a dug cell no walk gets beside (dug from a
  distance over a wall) is cut to first (cut-to-drop!; not on a level-keeping tunnel). :walked (look and collect again), :partial
  (go-to or a dig waits) or nil (no walk: it came in, the cell was walked to already, or the walk was blocked)."
  [c]
  (let [{:keys [dug-at drop-walked last-carried]} (ctx/mem c)]
    (when (and dug-at (not= dug-at drop-walked) (<= (carried c) (or last-carried 0)))
      (let [pos (zipmap [:x :y :z] dug-at)
            go! #(near/go-near! c pos 1 drop-walk-opts)
            w (await (go!))
            w (if (and (= :blocked w) (await (look-floor-toward! c dug-at))) (await (go!)) w)
            cut (when (and (= :blocked w) (not (ways/stay-level? (:args c))) (cut-after? (near/last-walk c)))
                  (await (cut-to-drop! c dug-at (drop-cut-max))))
            w (case cut :cut (await (go!)) :partial :partial w)]
        (when (= :there w) (await (peek-over-lip! c dug-at)))
        (when-not (= :partial w) (ctx/update-mem! c assoc :drop-walked dug-at))
        (when (= :there w) (ctx/update-mem! c dissoc :drop-looked))
        (case w :partial :partial :there :walked nil)))))

(defn ^:async collect! [c]
  (let [dug (:dug-at (ctx/mem c))
        look? (and dug (not= dug (:drop-looked (ctx/mem c))))
        _ (when look? (ctx/update-mem! c assoc :drop-looked dug))
        r (waits/waiting-out c (await (ctx/call c :collect 'jobs.items.collect-drops
                           (cond-> {:radius (drop-radius c) :items (items (:args c)) :dig true}
                             dug (assoc :holes [dug] :look-holes? look?)))))
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
