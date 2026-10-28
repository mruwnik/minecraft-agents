(ns jobs.lib.surface-up
  "Up out of a cave for the gather source: a body under rock whose search found nothing first walks up a stair of its own
  that ends under the open sky (go-to weighs it against a new one), else walks to the nearest wall and cuts a stair up (by hand if it must) until sky shows."
  (:require [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.breath :as breath]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.ready :as ready]
            [jobs.lib.shore :as shore]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.places :as places]
            [jobs.lib.util :as u]
            [jobs.lib.column-kinds :as kinds]
            [jobs.lib.walk.world :as wworld]))

(def wall-reach "Blocks out from the feet the nearest wall is looked for." 8)

(defn wall-of
  "The wall block (a feet-height cell) next to the cell, or nil: a solid block on one of its four sides, none of the cells
  [x y z] in avoid (a caller's dig line: the stair's first floor would lie in it)."
  [p {:keys [x y z]} & [avoid]]
  (first (filter #(and (sh-sense/solid-at? p %) (not (contains? avoid [(:x %) (:y %) (:z %)])))
                 [{:x (inc x) :y y :z z} {:x (dec x) :y y :z z} {:x x :y y :z (inc z)} {:x x :y y :z (dec z)}])))

(defn stand-by-wall
  "[stand wall] of the nearest cell at the feet's height with a floor, room for the body and a wall (wall-of, avoid) on a side, else nil."
  [p & [avoid]]
  (let [{:keys [x y z] :as feet} (sh-sense/feet p)
        r wall-reach]
    (->> (for [dx (range (- r) (inc r)) dz (range (- r) (inc r))] {:x (+ x dx) :y y :z (+ z dz)})
         (sort-by #(u/dist feet %))
         (some (fn [cell]
                 (when (and (not (sh-sense/solid-at? p cell))
                            (not (sh-sense/solid-at? p (update cell :y inc)))
                            (sh-sense/solid-at? p (update cell :y dec)))
                   (when-let [wall (wall-of p cell avoid)] [cell wall])))))))

(def extra-steps "Stair steps cut after the sky shows, to climb out of the pit the stair ends in." 2)

(defn ring-free?
  "Whether no solid block stands beside the feet cell (all eight sides at feet height): the body is out on level ground."
  [p]
  (let [{:keys [x y z]} (sh-sense/feet p)]
    (not-any? #(sh-sense/solid-at? p {:x (+ x (% 0)) :y y :z (+ z (% 1))})
              (for [dx [-1 0 1] dz [-1 0 1] :when (not= 0 dx dz)] [dx dz]))))

(defn climbed?
  "Whether the body came out on open ground and has not gone down since: it looks round from there (a search), it is not climbed toward again."
  [c]
  (let [done-y (:done-y (:surface (ctx/mem c)))]
    (boolean (and done-y (> (+ (:y (sh-sense/feet (:primitives c))) 3) done-y)))))

(defn surfacing?
  "Whether the way up is on: a search for the need already ran and found nothing while the body is under rock (no sky to
  see a tree or a cow from), and from then until it stands out on open ground."
  [c need]
  (and (not (climbed? c))
       (or (:on (:surface (ctx/mem c)))
           (and (pos? (get-in (ctx/mem c) [:explored (:key need)] 0))
                (sh-sense/buried? (:primitives c))))))

(defn open-tops
  "The tops of the stairs of the body's own that end under the open sky above its feet, as {:x :y :z} (the stand on the last step)."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        stairs (ledger/stair-stands c)
        kind-at (kinds/seen-kind-at p)
        surface-at (kinds/surface-kind-at p)
        top? (fn [[x y z]] (and (not-any? (fn [[dx dz]] (contains? stairs [(+ x dx) (inc y) (+ z dz)])) [[1 0] [-1 0] [0 1] [0 -1]])
                                (keyword-identical? :open (kind-at x (+ y 2) z))
                                (= y (kinds/top surface-at x z))))]
    (->> stairs
         (filter (fn [[_ y _ :as cell]] (and (> y (inc (:y feet))) (top? cell))))
         (map (fn [[x y z]] {:x x :y y :z z})))))

(defn own-top
  "The nearest top of a stair of the body's own that ends under the open sky above its feet (open-tops), else nil."
  [c]
  (let [feet (sh-sense/feet (:primitives c))]
    (first (sort-by #(u/dist feet %) (open-tops c)))))

(def sky-scan "Blocks out from the feet, and between columns, of the look for a surface the body remembers." {:reach 8 :step 4 :height 80})

(defn known-surface
  "The y of the open ground the body knows above its feet, else nil: the nearest (by the flat distance from the feet) of the
  tops of its own stairs that end under the open sky and the lowest sky-lit stands (open, seen solid under) it remembers in
  the columns round it. Only a ground a stair straight toward it reaches (flat distance within its rise) counts: a stair
  rises a block for each block it goes along."
  [c]
  (let [p (:primitives c)
        {:keys [x y z] :as feet} (sh-sense/feet p)
        {:keys [reach step height]} sky-scan
        stand-in (fn [cx cz]
                   (some (fn [cy] (let [cell {:x cx :y cy :z cz}]
                                    (when (and (sh-sense/seen-cell? p cell) (not (sh-sense/solid-at? p cell))
                                               (pos? (or (sh-sense/sky-light-at p cell) 0))
                                               (sh-sense/seen-cell? p (update cell :y dec)) (sh-sense/solid-at? p (update cell :y dec)))
                                      cell)))
                         (range (inc y) (+ y height))))
        stands (keep (fn [[dx dz]] (stand-in (+ x dx) (+ z dz)))
                     (for [dx (range (- reach) (inc reach) step) dz (range (- reach) (inc reach) step) :when (not= 0 dx dz)] [dx dz]))
        flat #(u/flat-dist feet %)]
    (->> (concat (open-tops c) stands)
         (filter #(<= (flat %) (- (:y %) y)))
         (sort-by (juxt flat :y))
         first
         :y)))

(defn rise
  "Blocks from the feet up to the sky of the feet column: to the first seen open cell the sky reaches. Seen rock and cave
  air above count; past the last seen rock and a gap of cells never seen or of unread light (more than dig-look/rock-depth: further than that
  the body has no reason to take for rock) the sky is where the body knows it (known-surface) or the surface prior puts it
  (cost/expected-rise), never nearer than that rock."
  [c]
  (let [p (:primitives c)
        {:keys [y] :as feet} (sh-sense/feet p)]
    (loop [dy 1 last-rock 0 gap 0]
      (let [cell (assoc feet :y (+ y dy))
            seen? (sh-sense/seen-cell? p cell)
            solid? (and seen? (sh-sense/solid-at? p cell))
            light (when (and seen? (not solid?)) (sh-sense/sky-light-at p cell))]
        (cond
          (>= (+ y dy) places/max-y) dy
          (pos? (or light 0)) dy
          solid? (recur (inc dy) dy 0)
          (and (nil? light) (> (inc gap) (dig-look/rock-depth)))
          (js/Math.ceil (cost/expected-rise y (+ last-rock (dig-look/rock-depth) 1) (known-surface c)))
          :else (recur (inc dy) last-rock (if (nil? light) (inc gap) gap)))))))

(defn climb-s
  "Seconds the stair up from a buried body's feet to the sky takes: rise blocks, each a walk step with the cuts of its
  three cells (a seen open cell costs none, the rest stone), a step that cuts at least cw/stair-cost of one step; 0 for a body that is not
  buried, has climbed out already (climbed?) or has a stair of its own to walk up (own-top)."
  [c]
  (let [p (:primitives c)]
    (if (or (not (sh-sense/buried? p)) (climbed? c) (own-top c))
      0
      (let [leg-s (cost/stair-leg-pricer (wworld/unseen-share p))
            feet (sh-sense/feet p)
            cut-s (fn [n] (when-not (and (sh-sense/seen-cell? p n) (not (sh-sense/solid-at? p n)))
                            (or (cost/dig-cost (or (u/seen-name p n) "stone")) ##Inf)))
            leg (fn [dy] (let [cells (map #(assoc feet :y (+ (:y feet) dy %)) (range 3))]
                           (leg-s (map cut-s cells) nil (some #(not (sh-sense/seen-cell? p %)) (cons (assoc feet :y (+ (:y feet) dy -1)) cells)))))]
        (transduce (map leg) + 0 (range 1 (inc (rise c))))))))

(defn ^:async climb!
  "One stair step up from a wall (jobs.lib.dig-in-leave/climb!): :continue while it goes on, :stopped when no stair goes up."
  [c wall]
  (if (= :continue (await (dig-leave/climb! c {} wall))) :continue :stopped))

(defn ^:async step!
  "One round of the way up: :continue while it walks to a wall or climbs a step, :out once the body stands on open ground,
  :stopped when there is no wall in reach or no stair goes up. avoid: cells [x y z] no climb starts through (wall-of)."
  [c & [avoid]]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        buried (sh-sense/buried? p)
        extra (:extra (:surface (ctx/mem c)) 0)
        own-tried (:own-tried (:surface (ctx/mem c)))
        _ (ctx/update-mem! c assoc :surface {:on true :extra (if buried 0 extra) :own-tried own-tried})
        own (when (and buried (not own-tried)) (own-top c))
        avoid (set avoid)
        wall (wall-of p feet avoid)
        [stand wall-cell] (when-not (or own wall) (stand-by-wall p avoid))]
    (cond
      own (let [r (await (child/call-walk* c :own-way-walk (ready/forage-walk c {:pos own :range 1 :ways #{:walk} :spend {:tries 1}})))]
            (when-not (= :continue r) (ctx/update-mem! c assoc-in [:surface :own-tried] true))
            :continue)
      (and (or (not buried) (and (nil? wall) (nil? stand))) (or (ring-free? p) (>= extra extra-steps)))
      (do (ctx/update-mem! c assoc :surface {:done-y (:y feet)}) :out)
      wall (do (when-not buried (ctx/update-mem! c assoc-in [:surface :extra] (inc extra)))
               (await (climb! c wall)))
      (nil? stand) :stopped
      :else (let [r (await (child/call-walk* c :surface-walk (ready/forage-walk c {:pos stand :range 0 :ways #{:walk} :spend {:tries 1}})))]
              (cond
                (= :continue r) :continue
                (:arrived (:data r)) :continue
                :else :stopped)))))

(def max-failed-stands "Dry stands a walk out of water may fail to reach before the body stays." 3)

(def water-radius "Blocks out from the feet a body in water looks for a dry stand." 12)

(defn ^:async leave-water!
  "A body in water first walks to a dry stand joined to it by seen water and air (the shore.reachable-land within
  water-radius), as a player climbs out before mining; the stand is chosen once and kept in mem until the walk ends.
  :continue while it walks, :again after a walk that ended short (the next round picks another, up to max-failed-stands
  in a row), :out when it is dry, arrived or sees no stand."
  [c]
  (let [p (:primitives c)
        m (:leave-water (ctx/mem c))
        failed (:failed m #{})
        stand (when (and (breath/in-water? p) (< (count failed) max-failed-stands))
                (or (:stand m) (shore/reachable-land p (u/self-pos c) water-radius failed)))]
    (cond
      (nil? stand) :out
      :else (let [_ (when-not (:stand m)
                      (ctx/update-mem! c assoc-in [:leave-water :stand] stand)
                      (ctx/emit! c :obtain.leave-water :info {:message (str "in water: walking to dry land at " (:x stand) " " (:y stand) " " (:z stand)) :pos stand}))
                  r (await (child/call-walk* c :leave-water-walk (ready/forage-walk c {:pos stand :range 0 :ways #{:walk} :spend {:tries 1}})))]
              (cond
                (= :continue r) :continue
                (:arrived (:data r)) (do (ctx/update-mem! c dissoc :leave-water) :out)
                :else (do (ctx/update-mem! c update :leave-water #(-> % (dissoc :stand) (update :failed (fnil conj #{}) (select-keys stand [:x :y :z]))))
                          :again))))))
