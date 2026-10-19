(ns jobs.survival.retreat-doors
  "Shutting a door, gate or trapdoor on the threat in a flight (retreat, night)."
  (:require [jobs.lib.click :as click]
            [engine.ctx :as ctx]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-cornered :as cornered]
            [jobs.lib.rooms :as room-lib]))

(def door-reach
  "Farthest (blocks, feet to the cell's middle) an open door may be for the flight to shut it: within a hand's reach."
  4)

(defn door-key [{:keys [x y z]}] [x y z])

(defn lower-half
  "The cell of a door's lower half (a gate's or trapdoor's own cell) for an openable block b at cell."
  [b cell]
  (if (= "upper" (:half (click/props-of b))) (update cell :y dec) cell))

(defn door-to-shut
  "The nearest door, gate or trapdoor a hand shuts that stands open within door-reach of the body, is nearer the
  threat at threat-pos than the body is (it lies between them, or beyond the body towards the threat), is not the
  one the body stands in, and has not been clicked this flight (:doors-clicked): {:cell :name}, or nil."
  [c threat-pos]
  (let [p (:primitives c)
        self (u/self-pos c)
        {fx :x fy :y fz :z} (sh-sense/feet p)
        r (js/Math.ceil door-reach)
        clicked (:doors-clicked (ctx/mem c) #{})
        mid room-lib/mid]
    (->> (for [x (range (- fx r) (+ fx r 1)) y (range (dec fy) (+ fy 3)) z (range (- fz r) (+ fz r 1))]
           {:x x :y y :z z})
         (keep (fn [cell]
                 (when-let [b (dig-cells/open-openable p cell)]
                   (when (= :openable (click/kind-of (.-name b)))
                     (let [low (lower-half b cell)]
                       {:cell low :name (.-name b) :half (:half (click/props-of b))})))))
         distinct
         (remove #(clicked (door-key (:cell %))))
         (remove #(click/standing-in? self (:cell %) (:half %)))
         (filter #(<= (u/dist self (mid (:cell %))) door-reach))
         (filter #(< (u/dist threat-pos (mid (:cell %))) (u/dist threat-pos self)))
         (sort-by #(u/dist self (mid (:cell %))))
         first)))

(defn ^:async shut-door!
  "Shut door (door-to-shut) with one click; every door is clicked at most once a flight, so a door that will not stay
  shut does not hold the flight. :again: the next step sees whether the danger is still there."
  [c {:keys [cell name]}]
  (ctx/update-mem! c update :doors-clicked (fnil conj #{}) (door-key cell))
  (let [r (await (dig-cells/shut-open! c cell))]
    (when (= :shut r)
      (ctx/emit! c :retreat.door-shut :info {:cell (door-key cell) :text (str "shut the " name " at " (door-key cell)
                                                                             " on the hostile")})))
  :again)


(defn home-door
  "The door of the home room (jobs.lib.reach/room) that stands open and has not been clicked this flight, or nil."
  [c room]
  (let [clicked (:doors-clicked (ctx/mem c) #{})]
    (->> (:doors room)
         (filter #(dig-cells/open-openable (:primitives c) (:cell %)))
         (remove #(clicked (door-key (:cell %))))
         (sort-by #(u/dist (u/self-pos c) (room-lib/mid (:cell %))))
         first)))

(defn ^:async shut-home-door!
  "Shut an open door of the home room the body is in: from where it stands within door-reach, else a step to the room
  cell inside it first, as also when it stands in the door's own cell (a step that gets no nearer gives the door up for
  the flight). :again."
  [c {:keys [cell inside]}]
  (let [b (dig-cells/open-openable (:primitives c) cell)
        in-door? (click/standing-in? (u/self-pos c) cell (:half (click/props-of b)))]
    (if (and (not in-door?) (<= (u/dist (u/self-pos c) (room-lib/mid cell)) door-reach))
      (await (shut-door! c {:cell cell :name (.-name b)}))
      (do (when-not (await (cornered/step! c :home inside 0))
            (ctx/update-mem! c update :doors-clicked (fnil conj #{}) (door-key cell)))
          :again))))
