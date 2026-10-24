(ns jobs.lib.surface-up
  "Up out of a cave for the gather source: a body under rock whose search found nothing first walks up a stair of its own
  that ends under the open sky (go-to weighs it against a new one), else walks to the nearest wall and cuts a stair up (by hand if it must) until sky shows."
  (:require [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.breath :as breath]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.ready :as ready]
            [jobs.lib.shore :as shore]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.column-kinds :as kinds]))

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

(defn own-top
  "The nearest top of a stair of the body's own that ends under the open sky above its feet, as {:x :y :z}, else nil."
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
         (map (fn [[x y z]] {:x x :y y :z z}))
         (sort-by #(u/dist feet %))
         first)))

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
