(ns jobs.movement.go-to.headroom
  "Why a walled-in goal is walled in when the wall is headroom: a low block (bed, bottom slab) in the way with a solid cell
  two above it, so a body stepping onto it would hit its head."
  (:require [clojure.string :as str]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.util :as u]))

(def low-block #"_bed$|_slab$")

(defn low-block? [b]
  (and (some->> b .-name (re-find low-block))
       (not (contains? #{"top" "double"} (some-> b .-properties .-type)))))

(defn low-passage
  "The low block a body walled in round goal {:x :y :z} would have to step over to leave, whose cell two above is solid:
  {:block name :at [x y z] :under [x y z]} (the first found), else nil. Walks the cells the goal reaches over the blocks seen."
  [p {:keys [x y z]}]
  (let [kind-at (reach/lookup p reach-room/body-kind-of)]
    (when-let [cells (reach-room/flood-cells (partial reach/forward kind-at) [x y z] reach/node-budget)]
      (first
        (for [c cells
              :let [cx (aget c 0) cy (aget c 1) cz (aget c 2)]
              d (array-seq reach/dirs)
              :let [nx (+ cx (aget d 0)) nz (+ cz (aget d 1))
                    b (u/seen-block p {:x nx :y cy :z nz})]
              :when (and (low-block? b)
                         (reach/passable? kind-at nx (inc cy) nz)
                         (not (reach/passable? kind-at nx (+ cy 2) nz)))]
          {:block (.-name b) :at [nx cy nz] :under [nx (+ cy 2) nz]})))))

(defn words
  "The give-up text for a :low-passage (low-passage's answer)."
  [{:keys [block at under]}]
  (str "the way in is over the " (str/replace block "_" " ") " at " at ", which leaves no headroom under the block at " under))
