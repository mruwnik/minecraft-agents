(ns jobs.survival.unwedge
  (:require [jobs.lib.checks :as checks]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.pace :as pace]
            [jobs.lib.access :as access]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.lib.breath :as breath]))

(def doc
  "Get out of a block that fills the feet cell (jobs.lib.breath), e.g. sand that fell on the body.
  One run, paced between tries, until the feet cell is no longer a full block (then :done):
  - Once, step to a free side cell (feet and head cells passable, something to stand on).
  - Else dig the feet block with the best carried tool (a last resort, tidied up like breathe's digs).
    A block the dig reports :cannot (bedrock) is final; other failed digs (or a dig refused because lava touches
    the cell) count, three in a row are final.
  A final failure writes an :unwedge-blocked entry (cell, 10 min; the trigger holds
  its fire for that cell meanwhile) and ends nothing-possible {:reason :blocked :why} with data {:cell}: the body
  holds still, nothing refires.
  A body not wedged is done at once. Ways :step and :dig (both by default);
  a way left out is not tried, and a run with no way left ends nothing-possible :no-way.
  Still wedged after max-passes tries ends nothing-possible :still-wedged. Ends :done once the feet cell is free.
  Never yields :continue (only a cut round returns it, unseen).
  Quote: a few passes of ::pass-s.")

(a/defargs args
  {:ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(def target [])
(def ways [:step :dig])
(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s} :ways #{:step :dig}})


(defn done? [c]
  (let [wedged (some? (breath/wedged-cell (:primitives c)))]
    (checks/goal (not wedged) [:free "the feet cell is free"] [:wedged "wedged in a block"] {:units :cells :left (if wedged 1 0)})))

(defn ^:async block!
  "Give up on cell: warn once, remember it so the trigger stays quiet there. Resolves the nothing-possible ending."
  [c cell why]
  (ctx/remember! c :unwedge-blocked {:cell cell :why why} breath/blocked-policy)
  (ctx/end c :nothing-possible {:reason :blocked :why why} {:cell cell}))

(defn lava-near?
  "Whether lava touches the cell (beside or above it): digging would let it flow in. Water is not refused: a
  flooded cell is breathe's to swim out of, while a body left wedged is stuck for good."
  [p cell]
  (some #(= "lava" (u/block-name p (merge-with + cell %)))
        [{:x 1 :y 0 :z 0} {:x -1 :y 0 :z 0} {:x 0 :y 0 :z 1} {:x 0 :y 0 :z -1} {:x 0 :y 1 :z 0}]))

(defn ^:async fail!
  "One failed dig on cell: final after u/max-failures in a row. Resolves :again or the blocked ending."
  [c cell why]
  (if (u/count-fail! c) (await (block! c cell (keyword why))) :again))

(defn ^:async dig-feet!
  [c cell]
  (let [p (:primitives c)]
    (cond
      (not (ctx/way? c :dig)) (ctx/end c :nothing-possible {:reason :no-way :ways (:ways (ctx/frozen-call c))})
      (lava-near? p cell) (await (fail! c cell "lava"))
      :else
      (let [block (u/feel-name p cell)
            _ (access/trespass! c "unwedge" (:trespass (access/choose c :dig [[cell]] identity)))
            _ (await (tools/equip-tool! c block {:fast true}))
            status (.-status (await (tidy/dig! c cell true)))]
        (cond
          (or (= "dug" status) (= "missing" status)) (do (u/progress! c) :again)
          (= "cannot" status) (await (block! c cell :cannot))
          :else (await (fail! c cell status)))))))

(a/defargs settings
  {::job-time-s {:default 30 :doc "Seconds getting out of a block may take." :spec (a/num-in 0 nil)}
   ::max-passes {:default 8 :doc "Tries (a step or a dig) in one run before it stops." :spec (a/int-in 0 nil)}
   ::pass-s {:default 1.5 :doc "Seconds one try (a step or a dig) takes, the quote's unit." :spec (a/num-in 0 nil)}})
(defn max-passes [] (settings/get settings ::max-passes))

(defn ^:async quote-of [_]
  (let [t (settings/get settings ::pass-s)]
    {:uses {:job-time [t (* 2 t) (* t (max 2 (max-passes)))]}}))

(defn ^:async pass!
  "One try at the wedged cell: the side step once, else a dig. Resolves :again or an ending."
  [c cell]
  (let [p (:primitives c)
        side (when (and (ctx/way? c :step) (not (:side-tried (ctx/mem c)))) (breath/side-cell p (.self p)))]
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
          (nil? cell) (ctx/end c :done nil)
          (not (ctx/alive? c)) :continue
          (<= (max-passes) i) (ctx/end c :nothing-possible :still-wedged {:tries i})
          :else (let [r (await (pass! c cell))]
                  (if (= :again r)
                    (do (await (pace/pace!)) (recur (inc i)))
                    r)))))))
