(ns jobs.survival.respond.doors
  "Shutting a door, gate or trapdoor on the threat in a flight (retreat, night)."
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.click :as click]
            [jobs.lib.look :as look]
            [engine.ctx :as ctx]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.lib.seal :as seal]
            [jobs.survival.respond.options :as cornered]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.rooms :as room-lib]))

(a/defargs settings
  {::door-reach {:default 4 :doc "Farthest (blocks, feet to the cell's middle) an open door may be for the flight to shut it: within a hand's reach." :spec (a/num-in 0 nil)}})
(defn door-reach [] (settings/get settings ::door-reach))

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
        r (js/Math.ceil (door-reach))
        clicked (:doors-clicked (ctx/mem c) #{})
        mid room-lib/mid]
    (->> (for [x (range (- fx r) (+ fx r 1)) y (range (dec fy) (+ fy 3)) z (range (- fz r) (+ fz r 1))]
           {:x x :y y :z z})
         (keep (fn [cell]
                 (when-let [b (seal/open-openable p cell)]
                   (when (= :openable (click/kind-of (.-name b)))
                     (let [low (lower-half b cell)]
                       {:cell low :name (.-name b) :half (:half (click/props-of b))})))))
         distinct
         (remove #(clicked (door-key (:cell %))))
         (remove #(click/standing-in? self (:cell %) (:half %)))
         (filter #(<= (u/dist self (mid (:cell %))) (door-reach)))
         (filter #(< (u/dist threat-pos (mid (:cell %))) (u/dist threat-pos self)))
         (sort-by #(u/dist self (mid (:cell %))))
         first)))

(defn door-for
  "door-to-shut against the nearest of threats (JS entities): the one door both the flight's step and respond's cover price."
  [c threats]
  (let [p (:primitives c)]
    (when-let [t (first (sort-by #(danger-q/mob-distance p %) threats))]
      (door-to-shut c (danger-q/mob-pos p t)))))

(defn sealing-doors
  "{:n :unseen?}: how many open doors the closed room (room-lib/room-doors) the body stands in has, when the door of
  door-for threats is one of them: shutting them all closes the room; :unseen? when a cell of the room is not seen yet
  (the worse case: a side may be open). nil when the body is in no closed room (another opening) or that door is none
  of its room's."
  [c threats]
  (let [p (:primitives c)]
    (when-let [door (door-for c threats)]
      (when-let [{:keys [doors unseen?]} (room-lib/room-doors c (sh-sense/feet p))]
        (let [open (filter #(seal/open-openable p (:cell %)) doors)]
          (when (some #(= (door-key (:cell door)) (door-key (:cell %))) open)
            {:n (count open) :unseen? unseen?}))))))

(defn heading-to
  "The heading [dx dz] from cell {:x :z} nearest the way to cell [x y z]."
  [{fx :x fz :z} [x _ z]]
  (let [dx (- x fx) dz (- z fz)]
    (if (> (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0] [0 (js/Math.sign dz)])))

(defn room-look
  "{:seen n :unseen n :sides [[x y z]] :headings [[dx dz]]}: what a look into the room the body stands in has to show
  when sealing (sealing-doors) says a cell of it is unseen: how many of its boundary cells are seen and unseen
  (reach-room/boundary), the unseen ones not looked at this run (:looked-sides) and their headings, a level and a down
  look each. nil when none is left to look at."
  [c sealing]
  (when (:unseen? sealing)
    (let [feet (sh-sense/feet (:primitives c))
          {:keys [seen unseen]} (reach-room/boundary (:primitives c) feet)
          left (remove (:looked-sides (ctx/mem c) #{}) unseen)]
      (when (seq left)
        {:seen seen :unseen (count unseen) :sides (vec left) :headings (vec (distinct (map #(heading-to feet %) left)))}))))

(defn ^:async look-into-room!
  "The look of room-look: a glance along each of its headings; its sides count as looked at for the run. :continue,
  so the next call decides on what it showed."
  [c {:keys [sides headings]}]
  (ctx/update-mem! c update :looked-sides (fnil into #{}) sides)
  (await (look/glance! c headings))
  :continue)

(defn ^:async shut-door!
  "Shut door (door-to-shut) with one click; every door is clicked at most once a flight, so a door that will not stay
  shut does not hold the flight. :again: the next step sees whether the danger is still there."
  [c {:keys [cell name]}]
  (ctx/update-mem! c update :doors-clicked (fnil conj #{}) (door-key cell))
  (let [r (await (seal/shut-open! c cell))]
    (when (= :shut r)
      (ctx/emit! c :retreat.door-shut :info {:cell (door-key cell) :text (str "shut the " name " at " (door-key cell)
                                                                             " on the hostile")})))
  :again)


(defn home-door
  "The door of the home room (jobs.lib.reach.room/room) that stands open and has not been clicked this flight, or nil."
  [c room]
  (let [clicked (:doors-clicked (ctx/mem c) #{})]
    (->> (:doors room)
         (filter #(seal/open-openable (:primitives c) (:cell %)))
         (remove #(clicked (door-key (:cell %))))
         (sort-by #(u/dist (u/self-pos c) (room-lib/mid (:cell %))))
         first)))

(defn ^:async shut-home-door!
  "Shut an open door of the home room the body is in: from where it stands within door-reach, else a step to the room
  cell inside it first, as also when it stands in the door's own cell (a step that gets no nearer gives the door up for
  the flight). :again."
  [c {:keys [cell inside]}]
  (let [b (seal/open-openable (:primitives c) cell)
        in-door? (click/standing-in? (u/self-pos c) cell (:half (click/props-of b)))]
    (if (and (not in-door?) (<= (u/dist (u/self-pos c) (room-lib/mid cell)) (door-reach)))
      (await (shut-door! c {:cell cell :name (.-name b)}))
      (do (when-not (await (cornered/step! c :home inside 0))
            (ctx/update-mem! c update :doors-clicked (fnil conj #{}) (door-key cell)))
          :again))))
