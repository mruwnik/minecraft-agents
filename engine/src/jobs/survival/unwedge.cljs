(ns jobs.survival.unwedge
  (:require [engine.ctx :as ctx]
            [engine.jobs.access :as access]
            [engine.jobs.tidy :as tidy]
            [engine.jobs.tools :as tools]
            [engine.jobs.util :as u]
            [engine.triggers.wedged :as w]
            [jobs.survival.breathe :as breathe]))

(def doc
  "Get out of a block that fills the feet cell (engine.triggers.wedged), e.g. sand that fell on the body.
  One action per round:
  - Once per job, step to a free side cell (feet and head cells passable, something to stand on).
  - Else dig the feet block with the best carried tool (a last resort, tidied up like breathe's digs) and
    wait for the next round to see whether the body is free. A block the dig reports :cannot (bedrock) is final;
    other failed digs count, three in a row are final.
  A final failure warns :unwedge.blocked once, writes an :unwedge-blocked entry (cell, 10 min; the trigger holds
  its fire for that cell meanwhile) and ends: the body holds still, nothing refires.
  Ends when the feet cell is no longer a full block.")

(def args {})

(defn check [c] (some? (w/wedged-cell (:primitives c))))

(defn after
  "The round's outcome once it acted: :done when the feet cell is free (a listed job whose check turned false would
  only wait), else :continue."
  [c]
  (if (check c) :continue :done))

(defn ^:async block!
  "Give up on cell: warn once, remember it so the trigger stays quiet there. Resolves :done."
  [c cell why]
  (ctx/emit! c :unwedge.blocked :warn {:cell cell :why why :text (str "wedged in a block I cannot dig out of: " (name why))})
  (ctx/remember! c :unwedge-blocked {:cell cell :why why} w/blocked-policy)
  :done)

(defn ^:async dig-feet!
  [c cell]
  (let [p (:primitives c)
        block (u/block-name p cell)
        _ (access/trespass! c "unwedge" (:trespass (access/choose c :dig [[cell]] identity)))
        _ (await (tools/equip-tool! c block {:fast true}))
        status (.-status (await (tidy/dig! c cell true)))]
    (cond
      (contains? #{"dug" "missing"} status) (after c)
      (= "cannot" status) (await (block! c cell :cannot))
      :else (let [tries (inc (:failures (ctx/mem c) 0))]
              (ctx/update-mem! c assoc :failures tries)
              (if (< tries u/max-failures) :continue (await (block! c cell (keyword status))))))))

(defn ^:async round [c]
  (let [p (:primitives c)
        cell (w/wedged-cell p)]
    (cond
      (nil? cell) :done
      :else
      (let [side (when-not (:side-tried (ctx/mem c)) (breathe/side-cell p (.self p)))]
        (if side
          (do (ctx/update-mem! c assoc :side-tried true)
              ;; raw moveTo kept: an emergency step out of a block (range 0), where the planner may have no standable cell.
              (await (ctx/act c :moveTo (clj->js {:pos side :range 0})))
              (after c))
          (await (dig-feet! c cell)))))))
