(ns jobs.lib.goal
  "Where a walk that was cut is going: body memory :walk-goal {:job root :slots :goal}, written by an open go-to of a
  job no trigger fired (a reflex's walk is no destination the body was bound for) and dropped when the walk ends or its
  call is cleaned up."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]))

(def pos-spec
  "The spec of a :goal arg: a position map or vector."
  (a/pred "{:x :y :z} or [x y z]" #(or (map? %) (vector? %))))

(def kind :walk-goal)

(def ttl-ms "A walk-goal no cleanup dropped is forgotten after this." (* 60 60 1000))

(defn- mine? [c] #(and (= (:job %) (:root c)) (= (:slots %) (:slots c))))

(defn- own-entries [c] (filter (comp (mine? c) :data) (ctx/entries c kind)))

(defn note!
  "This call's walk is open toward goal."
  [c goal]
  (when (nil? (:reflex c))
    (let [own (own-entries c)]
      (when-not (some #(= goal (:goal (:data %))) own)
        (when (seq own) (ctx/forget-where! c kind (mine? c)))
        (ctx/remember! c kind {:job (:root c) :slots (:slots c) :goal goal} {:cap 8 :ttl ttl-ms})))))

(defn clear!
  "This call's walk has ended. Writes only when it left a goal."
  [c]
  (when (seq (own-entries c)) (ctx/forget-where! c kind (mine? c))))

(defn interrupted-walk
  "{:goal :route} of the most recently noted open walk of a job other than root-id's; nil when none."
  [view root-id]
  (->> (mem/entries view kind)
       (remove #(= root-id (:job (:data %))))
       last
       :data))

(defn interrupted-goal
  "The goal of interrupted-walk."
  [view root-id]
  (:goal (interrupted-walk view root-id)))

(defn corners
  "The cells [x y z] of route ({:x :y :z} cells) where the way turns, the first and last included: the same polyline in few points."
  [route]
  (let [cells (mapv (fn [{:keys [x y z]}] [x y z]) route)
        dir (fn [a b] (mapv compare b a))]
    (if (< (count cells) 3)
      cells
      (into [(first cells)]
            (concat (for [[a b n] (map vector cells (rest cells) (drop 2 cells)) :when (not= (dir a b) (dir b n))] b)
                    [(peek cells)])))))

(defn note-route!
  "This call's open walk goes by route (the cells go-to's plan walks, then straight on to the goal): corners of it are kept
  with the goal for the ones who price a return to it."
  [c route]
  (when (nil? (:reflex c))
    (when-let [e (last (own-entries c))]
      (let [corners (corners route)]
        (when-not (= corners (:route (:data e)))
          (ctx/forget-where! c kind (mine? c))
          (ctx/remember! c kind (assoc (:data e) :route corners) {:cap 8 :ttl ttl-ms}))))))

(defn route-distance
  "The least distance in 3D from the point {:x :y :z} to the polyline through pts ([x y z])."
  [pts {:keys [x y z]}]
  (let [seg (fn [[ax ay az] [bx by bz]]
              (let [dx (- bx ax) dy (- by ay) dz (- bz az)
                    len2 (+ (* dx dx) (* dy dy) (* dz dz))
                    t (if (zero? len2) 0 (max 0 (min 1 (/ (+ (* (- x ax) dx) (* (- y ay) dy) (* (- z az) dz)) len2))))]
                (js/Math.hypot (- x (+ ax (* t dx))) (- y (+ ay (* t dy))) (- z (+ az (* t dz))))))]
    (if (= 1 (count pts))
      (apply js/Math.hypot (map - [x y z] (first pts)))
      (reduce min (map seg pts (rest pts))))))
