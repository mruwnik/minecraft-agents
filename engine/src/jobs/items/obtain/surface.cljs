(ns jobs.items.obtain.surface
  "Up out of a cave for the gather source: a body under rock whose search found nothing walks to the nearest wall and cuts a stair up (by hand if it must) until sky shows."
  (:require [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(def wall-reach "Blocks out from the feet the nearest wall is looked for." 8)

(defn wall-of
  "The wall block (a feet-height cell) next to the cell, or nil: a solid block on one of its four sides."
  [p {:keys [x y z]}]
  (first (filter #(sh-sense/solid-at? p %)
                 [{:x (inc x) :y y :z z} {:x (dec x) :y y :z z} {:x x :y y :z (inc z)} {:x x :y y :z (dec z)}])))

(defn stand-by-wall
  "[stand wall] of the nearest cell at the feet's height with a floor, room for the body and a wall on a side, else nil."
  [p]
  (let [{:keys [x y z] :as feet} (sh-sense/feet p)
        r wall-reach]
    (->> (for [dx (range (- r) (inc r)) dz (range (- r) (inc r))] {:x (+ x dx) :y y :z (+ z dz)})
         (sort-by #(u/dist feet %))
         (some (fn [cell]
                 (when (and (not (sh-sense/solid-at? p cell))
                            (not (sh-sense/solid-at? p (update cell :y inc)))
                            (sh-sense/solid-at? p (update cell :y dec)))
                   (when-let [wall (wall-of p cell)] [cell wall])))))))

(def extra-steps "Stair steps cut after the sky shows, to climb out of the pit the stair ends in." 2)

(defn ring-free?
  "Whether no solid block stands beside the feet cell (all eight sides at feet height): the body is out on level ground."
  [p]
  (let [{:keys [x y z]} (sh-sense/feet p)]
    (not-any? #(sh-sense/solid-at? p {:x (+ x (% 0)) :y y :z (+ z (% 1))})
              (for [dx [-1 0 1] dz [-1 0 1] :when (not= 0 dx dz)] [dx dz]))))

(defn surfacing?
  "Whether the way up is on: a search for the need already ran and found nothing while the body is under rock (no sky to
  see a tree or a cow from), and from then until it stands out on open ground."
  [c need]
  (or (:on (:surface (ctx/mem c)))
      (and (pos? (get-in (ctx/mem c) [:explored (:key need)] 0))
           (sh-sense/buried? (:primitives c)))))

(defn ^:async climb!
  "One stair step up from a wall (jobs.lib.dig-in-leave/climb!): :continue while it goes on, :stopped when no stair goes up."
  [c wall]
  (if (= :continue (await (dig-leave/climb! c {} wall))) :continue :stopped))

(defn ^:async step!
  "One round of the way up: :continue while it walks to a wall or climbs a step, :out once the body stands on open ground,
  :stopped when there is no wall in reach or no stair goes up."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        buried (sh-sense/buried? p)
        extra (:extra (:surface (ctx/mem c)) 0)
        _ (ctx/update-mem! c assoc :surface {:on true :extra (if buried 0 extra)})
        wall (wall-of p feet)
        [stand wall-cell] (when-not wall (stand-by-wall p))]
    (cond
      (and (not buried) (or (ring-free? p) (>= extra extra-steps)))
      (do (ctx/update-mem! c dissoc :surface) :out)
      wall (do (when-not buried (ctx/update-mem! c assoc-in [:surface :extra] (inc extra)))
               (await (climb! c wall)))
      (nil? stand) :stopped
      :else (let [r (await (child/call-walk* c :surface-walk {:pos stand :range 0 :ways #{:walk} :spend {:tries 1}}))]
              (cond
                (= :continue r) :continue
                (:arrived (:data r)) :continue
                :else :stopped)))))
