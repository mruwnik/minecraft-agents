(ns jobs.gather.mine-stay
  "The :stay-level option of jobs.gather.mine: ore only beside the body, and a cut that opens into a cave sealed again."
  (:require [jobs.lib.access.rules :as rules]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.bridge :as bridge]))

(defn beside-body?
  "Whether the target is one column or less from the body's feet cell and at its feet or head height."
  [body target]
  (and (<= (abs (- (:x target) (:x body))) 1)
       (<= (abs (- (:z target) (:z body))) 1)
       (<= 0 (- (:y target) (:y body)) 1)))

(def around [[1 0 0] [-1 0 0] [0 0 1] [0 0 -1] [0 1 0]])

(defn breaches
  "The cells of cut (feet cell first) that touch seen air other than the cells of the cut, the cell the body stands in (from) and the one over it."
  [name-at from cut]
  (let [open (into #{from (update from :y inc)} cut)]
    (filterv (fn [cell] (some #(let [n (mapv + [(:x cell) (:y cell) (:z cell)] %)
                                     q {:x (n 0) :y (n 1) :z (n 2)}]
                                 (and (not (open q)) (rules/air (name-at q))))
                              around))
             cut)))

(defn ^:async seal!
  "Place a carried building block in each of cells: true when they all stand, false with none carried or a failed place. in: the rules input."
  [c in cells]
  (let [item (blocks/pick c blocks/building-blocks)
        block-at (:block-at in)]
    (loop [[cell & more] cells]
      (cond
        (nil? item) false
        (nil? cell) true
        :else (let [pos [(:x cell) (:y cell) (:z cell)]
                    plans (bridge/bridge-plans block-at pos (bridge/eye-y c))]
                (if (and (seq plans) (nil? (await (bridge/place-plans! c in item plans false))))
                  (recur more)
                  false))))))
