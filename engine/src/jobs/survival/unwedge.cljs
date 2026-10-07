(ns jobs.survival.unwedge
  (:require [engine.ctx :as ctx]
            [jobs.lib.pace :as pace]
            [jobs.lib.result :as result]
            [jobs.lib.access :as access]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.breath :as breath]
            [jobs.survival.breathe :as breathe]))

(def doc
  "Get out of a block that fills the feet cell (jobs.lib.breath), e.g. sand that fell on the body.
  One run, paced between tries, until the feet cell is no longer a full block (then :done):
  - Once, step to a free side cell (feet and head cells passable, something to stand on).
  - Else dig the feet block with the best carried tool (a last resort, tidied up like breathe's digs).
    A block the dig reports :cannot (bedrock) is final; other failed digs (or a dig refused because lava touches
    the cell) count, three in a row are final.
  A final failure warns :unwedge.blocked once, writes an :unwedge-blocked entry (cell, 10 min; the trigger holds
  its fire for that cell meanwhile) and ends stopped {:reason :blocked :why}: the body holds still, nothing refires.
  Still wedged after max-passes tries ends stopped :still-wedged. Never :continue.")

(def args
  {:ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(defn check [c]
  (or (some? (breath/wedged-cell (:primitives c)))
      (ctx/wait c {:reason :not-wedged})))

(defn ^:async block!
  "Give up on cell: warn once, remember it so the trigger stays quiet there. Resolves :done, stopped."
  [c cell why]
  (ctx/emit! c :unwedge.blocked :warn {:cell cell :why why :text (str "wedged in a block I cannot dig out of: " (name why))})
  (ctx/remember! c :unwedge-blocked {:cell cell :why why} breath/blocked-policy)
  (result/stop! c :blocked (str "wedged in a block I cannot dig out of: " (name why)) :why why :cell cell))

(defn lava-near?
  "Whether lava touches the cell (beside or above it): digging would let it flow in. Water is not refused: a
  flooded cell is breathe's to swim out of, while a body left wedged is stuck for good."
  [p cell]
  (some #(= "lava" (u/block-name p (merge-with + cell %)))
        [{:x 1 :y 0 :z 0} {:x -1 :y 0 :z 0} {:x 0 :y 0 :z 1} {:x 0 :y 0 :z -1} {:x 0 :y 1 :z 0}]))

(defn ^:async fail!
  "One failed dig on cell: final after u/max-failures in a row. Resolves :again or :done (stopped)."
  [c cell why]
  (if (u/count-fail! c) (await (block! c cell (keyword why))) :again))

(defn ^:async dig-feet!
  [c cell]
  (let [p (:primitives c)]
    (if (lava-near? p cell)
      (await (fail! c cell "lava"))
      (let [block (u/feel-name p cell)
            _ (access/trespass! c "unwedge" (:trespass (access/choose c :dig [[cell]] identity)))
            _ (await (tools/equip-tool! c block {:fast true}))
            status (.-status (await (tidy/dig! c cell true)))]
        (cond
          (or (= "dug" status) (= "missing" status)) (do (u/progress! c) :again)
          (= "cannot" status) (await (block! c cell :cannot))
          :else (await (fail! c cell status)))))))

(def max-passes "Tries (a step or a dig) in one run before it stops." 8)

(defn ^:async pass!
  "One try at the wedged cell: the side step once, else a dig. Resolves :again or :done (stopped)."
  [c cell]
  (let [p (:primitives c)
        side (when-not (:side-tried (ctx/mem c)) (breathe/side-cell p (.self p)))]
    (if-not side
      (await (dig-feet! c cell))
      (do (ctx/update-mem! c assoc :side-tried true)
          ;; raw moveTo kept: an emergency step out of a block (range 0), where the planner may have no standable cell.
          (await (ctx/act c :moveTo (clj->js {:pos side :range 0})))
          :again))))

(defn ^:async round [c]
  (let [p (:primitives c)]
    (loop [i 0]
      (let [cell (breath/wedged-cell p)]
        (cond
          (nil? cell) :done
          (not (ctx/alive? c)) :done
          (<= max-passes i) (result/stop! c :still-wedged "still wedged after several tries")
          :else (let [r (await (pass! c cell))]
                  (if (= :again r)
                    (do (await (pace/pace!)) (recur (inc i)))
                    r)))))))
