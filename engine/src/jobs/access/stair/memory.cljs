(ns jobs.access.stair.memory
  "What a stair remembers on the body: the floors of the ways it walked and the cells it plugged against water."
  (:require [engine.ctx :as ctx]
            [jobs.access.stair.cells :as cells]
            [plan.shape :as shape]))

(def way-policy {:cap 200 :ttl :forever})

(def plug-policy {:cap 60 :ttl :forever})

(defn plugs-of
  "The set of cells this body plugged against water in its current dimension (memory :stair-plug), whatever job did."
  [c]
  (let [dim (.-dimension (.self (:primitives c)))]
    (set (keep #(when (= dim (:dim (:data %))) (:cell (:data %))) (ctx/entries c :stair-plug)))))

(defn plug!
  "Remember cell as filled against water, so a later stair keeps it shut."
  [c cell]
  (ctx/remember! c :stair-plug {:dim (.-dimension (.self (:primitives c))) :cell cell} plug-policy))

(defn record-way!
  "Remember the steps walked (memory :stair-way, one entry a connected way: joined-way), so a climb cut in many runs
  does not push the body's other stairs out of the memory."
  [c origin dir heading steps]
  (let [dim (.-dimension (.self (:primitives c)))
        olds (keep #(when (= dim (:dim (:data %))) (:floors (:data %))) (ctx/entries c :stair-way))
        {:keys [floors gone]} (cells/joined-way olds (shape/add origin [0 -1 0]) (cells/way-floors origin dir heading steps))]
    (when (seq gone) (ctx/forget-where! c :stair-way #(and (= dim (:dim %)) (contains? gone (:floors %)))))
    (ctx/remember! c :stair-way {:dim dim :floors floors} way-policy)))

(def tool-failed-policy {:cap 16 :ttl (* 60 60 1000)})

(defn tool-failed?
  "Whether a fetch of a tool for block failed and is not to be tried again yet (memory :stair-tool-failed)."
  [c block]
  (boolean (some #(and (= block (:block (:data %))) (< (ctx/now c) (:until (:data %)))) (ctx/entries c :stair-tool-failed))))

(defn tool-failed!
  "Remember the failed fetch of a tool for block for minutes."
  [c block minutes]
  (ctx/remember! c :stair-tool-failed {:block block :until (+ (ctx/now c) (* 60000 minutes))} tool-failed-policy))
