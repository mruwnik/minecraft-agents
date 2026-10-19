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

(defn remember-way!
  "Remember floors as one connected way (memory :stair-way) with every earlier way that holds ground or one of the floors:
  those are taken in, so a climb cut in many runs does not push the body's other stairs out of the memory. Returns the
  joined floors."
  [c ground floors]
  (let [dim (.-dimension (.self (:primitives c)))
        olds (keep #(when (= dim (:dim (:data %))) (:floors (:data %))) (ctx/entries c :stair-way))
        {:keys [floors gone]} (cells/joined-way olds ground floors)]
    (when (seq gone) (ctx/forget-where! c :stair-way #(and (= dim (:dim %)) (contains? gone (:floors %)))))
    (ctx/remember! c :stair-way {:dim dim :floors floors} way-policy)
    floors))

(defn record-way!
  "Remember the steps walked (memory :stair-way, one entry a connected way: remember-way!)."
  [c origin dir heading steps]
  (remember-way! c (shape/add origin [0 -1 0]) (cells/way-floors origin dir heading steps)))

(defn tool-failed?
  "Whether a fetch of a tool for block failed in this call (job memory), so it is dug bare-handed and not asked for again."
  [c block]
  (contains? (:tool-failed (ctx/mem c)) block))

(defn tool-failed!
  "Remember the failed fetch of a tool for block for the rest of the call."
  [c block]
  (ctx/update-mem! c update :tool-failed (fnil conj #{}) block))
