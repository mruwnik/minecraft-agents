(ns jobs.survival.dig-in.pit
  "dig-in's pit mode: the descent (straight down, or a zigzag when the floor is not known) and the roof."
  (:require [engine.ctx :as ctx]
            [jobs.lib.blocks :as lb]
            [jobs.lib.dig-look :as look]
            [jobs.lib.shelter :as sh]
            [jobs.lib.solid :as solid]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-in.mode :as mode]
            [jobs.survival.dig-in.site :as site]))

(defn ^:async look-below!
  "Look at the cell to dig when it or one of its sides is unknown, so the checks read what is there."
  [c {:keys [x y z]}]
  (let [cells (cons [x y z] (for [[dx dz] dig-cells/sides] [(+ x dx) y (+ z dz)]))]
    (when (some #(look/unknown? (:primitives c) %) cells)
      (await (look/look-at! c [x y z])))))

(defn ^:async hole-round
  "The cell below the feet is dug and the body is about to walk into it (for the drops or to descend): looks at the cell
  under it. :done (a :no-floor or :fluid-adjacent stop) when that is seen not solid or fluid shows in or beside the
  hole, else nil once the flow delay has passed and the
  cut was looked at again (look/wait-settled!)."
  [c {:keys [x y z]}]
  (let [p (:primitives c)
        under-cell [x (- y 2) z]
        hole {:x x :y (dec y) :z z}
        _ (when (look/unknown? p under-cell) (await (look/look-at! c under-cell)))
        lavas (when (= :seal (:on-lava (:args c))) (seq (dig-cells/lava-around p hole dig-cells/beside-deltas)))
        _ (when lavas
            (await (dig-cells/seal-lava! c (mode/blocks c) lavas))
            (await (look/look-at! c [x (dec y) z])))
        under (u/block-name-or p {:x x :y (- y 2) :z z} "stone")]
    (cond
      (not (solid/solid? under))
      (do (site/remember-failed-site! c :no-floor)
          (ctx/emit! c :dig_in_failed :warn {:text (str under " under the hole; not stepping into it")})
          :done)
      (or (dig-cells/wet? p hole) (dig-cells/lateral-fluid p hole))
      (do (site/remember-failed-site! c :fluid-adjacent)
          (ctx/emit! c :dig_in_failed :warn {:text (str (or (u/seen-name p hole) "fluid") " in or beside the hole; not stepping into it")})
          :done)
      :else (await (look/wait-settled! c [[x (dec y) z]])))))

(defn ^:async descend-round
  "One step down toward the pit: dig the block below the feet, look at what the dig laid open, collect what it dropped,
  and step into the hole (hole-round). Gives up (dig_in_failed warn, done) rather than dig when the block below is a
  hazard or the cell under it is seen not to be solid (a thin floor over water, lava or air)."
  [c]
  (let [blocks (mode/blocks c)
        p (:primitives c)
        {:keys [x y z]} (sh/feet p)
        below {:x x :y (dec y) :z z}
        _ (await (look-below! c below))
        name (u/seen-name p below)
        under (u/seen-name p {:x x :y (- y 2) :z z})
        fluid (dig-cells/lateral-fluid p below)
        here (first (filter #(dig-cells/wet? p %) [{:x x :y y :z z} {:x x :y (inc y) :z z}]))
        roof (:roof (ctx/mem c))
        over (when roof (first (filter #(dig-cells/wet? p %) [roof (update roof :y inc)])))]
    (cond
      here (do (site/remember-failed-site! c :fluid-here)
               (ctx/emit! c :dig_in_failed :warn {:text (str (u/seen-name p here) " where the body stands; not digging down")})
               :done)
      fluid (do (site/remember-failed-site! c :fluid-adjacent)
                (ctx/emit! c :dig_in_failed :warn {:text (str fluid " beside the descent cell; not opening the pit")})
                :done)
      (dig-cells/wet? p below) (do (site/remember-failed-site! c :hazard-below)
                         (ctx/emit! c :dig_in_failed :warn {:text (str name " below the body; not digging down")})
                         :done)
      over (do (site/remember-failed-site! c :fluid-above)
               (ctx/emit! c :dig_in_failed :warn {:text (str (u/seen-name p over) " at or above the roof cell; not digging further")})
               :done)
      (and (sh/solid-at? p below) under (not (solid/solid? under)))
      (do (site/remember-failed-site! c :no-floor)
          (ctx/emit! c :dig_in_failed :warn {:text (str under " under the floor; not digging through it")})
          :done)
      (and (sh/solid-at? p below) (nil? (lb/pick c blocks)) (not (tools/can-harvest? p name)))
      (do (site/remember-material! c {:pos (:roof (ctx/mem c)) :needs name})
          (ctx/emit! c :dig_in_failed :warn
                     {:text (str "cannot harvest " name " without a "
                                 (tools/harvest-need (map :name (u/inventory p)) (js->clj (.harvestTools p name)))
                                 "; nothing to roof the pit with, so not digging")})
          :done)
      (not (sh/solid-at? p below))
      (or (await (hole-round c (sh/feet p)))
          (let [before (:y (sh/feet p))
                ;; raw moveTo kept: a step into the cell the job is digging, range 0.5, inside its own pit; the planner has no standable goal there.
                r (await (ctx/act c :moveTo (clj->js {:pos below :range 0.5})))]
            (if (< (:y (sh/feet p)) before)
              (do (u/progress! c) :continue)
              (site/fail-site! c :descent-stalled (str "cannot descend into the pit: " (.-status r))))))
      :else (let [_ (await (tools/equip-for! c name {:fast true}))
                  r (await (tidy/dig! c below true))]
              (if (= "dug" (.-status r))
                (let [placeable (some #(some #{(.-name %)} blocks) (array-seq (.-drops r)))]
                  (u/progress! c)
                  (ctx/update-mem! c assoc :dug-at (ctx/now c))
                  (await (look/see-round! c [x (dec y) z]))
                  (or (await (hole-round c {:x x :y y :z z}))
                      (do (await (dig-cells/collect-drops! c blocks (.-drops r)))
                          (if (or placeable (some? (lb/pick c blocks)))
                            :continue
                            (do (site/remember-material! c {:pos (:roof (ctx/mem c))})
                                (ctx/emit! c :dig_in_failed :warn {:text "nothing to roof the pit with"})
                                :done)))))
                (site/fail-site! c :dig-failed (str "cannot dig down: " (.-status r))))))))

(defn ^:async dig-pit-cell!
  "Dig one cell of the zigzag pit (its placeable drops noted for dig-cells/collect-pit-drops!), look at what it laid open,
  seal lava it shows (:on-lava :seal) while the body stands above, and let it settle: nil, or :done (a stop) when the dig
  fails or fluid shows in or beside the cell."
  [c cell]
  (let [p (:primitives c)
        blocks (mode/blocks c)
        {:keys [on-lava]} (:args c)
        v [(:x cell) (:y cell) (:z cell)]
        name (dig-cells/rock-name p cell)]
    (if (and (nil? (lb/pick c blocks)) (not (tools/can-harvest? p name)))
      (do (site/remember-material! c {:pos (:roof (ctx/mem c)) :needs name})
          (ctx/emit! c :dig_in_failed :warn {:text (str "cannot harvest " name "; nothing to roof the pit with, so not digging")})
          :done)
      (let [_ (await (tools/equip-for! c name {:fast true}))
            r (await (tidy/dig! c cell true))]
        (if (not= "dug" (.-status r))
          (site/fail-site! c :dig-failed (str "cannot dig down: " (.-status r)))
          (do (u/progress! c)
              (ctx/update-mem! c assoc :dug-at (ctx/now c))
              (ctx/update-mem! c update :pit-drops (fnil into [])
                               (for [d (array-seq (.-drops r)) :when (some #{(.-name d)} blocks)] {:id (.-id d) :cell cell}))
              (await (look/see-round! c v))
              (await (look/wait-settled! c [v]))
              (when-let [lavas (when (= :seal on-lava) (seq (dig-cells/lava-around p cell dig-cells/around-deltas)))]
                (await (dig-cells/seal-lava! c blocks lavas))
                (await (look/look-at! c v)))
              (when-let [fluid (first (filter #(dig-cells/wet? p %) [cell (update cell :y inc) (update cell :y dec)]))]
                (site/remember-failed-site! c :fluid-adjacent)
                (ctx/emit! c :dig_in_failed :warn {:text (str (u/seen-name p fluid) " in or beside the dug cell; not going on")})
                :done)))))))

(defn ^:async zigzag-round
  "One step down the pit when its floor is not known (the shape's :plugs are set): dig the next drop's cells, all beside
  or below-beside the body (dig-cells/pit-steps), and step into it only once the floor under it is seen solid (the eye
  sees the other column's cells down to 2 under the feet). Stops (done) :no-floor when that floor is seen not solid."
  [c]
  (let [p (:primitives c)
        {:keys [start shape]} (ctx/mem c)
        feet (sh/feet p)
        steps (:steps shape)
        here (first (filter #(dig-cells/wet? p %) [feet (update feet :y inc)]))
        at (first (keep-indexed #(when (= feet %2) %1) (cons start (map :to steps))))]
    (when at (await (dig-cells/collect-pit-drops! c feet)))
    (if (nil? at)
      (site/fail-site! c :left-pit "the body left the pit's cells")
      (let [{:keys [dig to floor]} (nth steps at)
            cell (first (remove #(dig-cells/open-cell? p %) dig))
            _ (when (and cell (look/unknown? p [(:x cell) (:y cell) (:z cell)])) (await (look/look-at! c [(:x cell) (:y cell) (:z cell)])))]
        (cond
          here
          (do (site/remember-failed-site! c :fluid-here)
              (ctx/emit! c :dig_in_failed :warn {:text (str (u/seen-name p here) " where the body stands; not digging down")})
              :done)
          (and cell (dig-cells/lateral-fluid p cell))
          (do (site/remember-failed-site! c :fluid-adjacent)
              (ctx/emit! c :dig_in_failed :warn {:text (str (dig-cells/lateral-fluid p cell) " beside the cell to dig; not opening the pit")})
              :done)
          (and cell (dig-cells/wet? p cell))
          (do (site/remember-failed-site! c :hazard-below)
              (ctx/emit! c :dig_in_failed :warn {:text (str (u/seen-name p cell) " in the cell to dig; not digging")})
              :done)
          (and cell (not (dig-cells/rock-solid? p cell)))
          (do (site/remember-failed-site! c :no-floor)
              (ctx/emit! c :dig_in_failed :warn {:text "open air where the pit would go; not digging"})
              :done)
          cell (or (await (dig-pit-cell! c cell)) :continue)
          :else
          (let [fv [(:x floor) (:y floor) (:z floor)]
                _ (when (look/unknown? p fv) (await (look/look-at! c fv)))]
            (if-not (dig-cells/known-solid? p floor)
              (do (site/remember-failed-site! c :no-floor)
                  (ctx/emit! c :dig_in_failed :warn {:text (str (or (u/seen-name p floor) "an unseen cell") " under the hole; not stepping into it")})
                  :done)
              (let [before (:y feet)
                    ;; raw moveTo kept: a step into the cell the job dug, range 0.5, inside its own pit; the planner has no standable goal there.
                    r (await (ctx/act c :moveTo (clj->js {:pos to :range 0.5})))]
                (if (< (:y (sh/feet p)) before)
                  (do (u/progress! c) :continue)
                  (site/fail-site! c :descent-stalled (str "cannot descend into the pit: " (.-status r))))))))))))

(defn ^:async roof-round
  "In the pit: place one block at each plug cell (a zigzag pit's side cell beside the head), then at the roof cell."
  [c]
  (let [blocks (mode/blocks c)
        p (:primitives c)
        {:keys [roof shape]} (ctx/mem c)
        cell (or (first (remove #(dig-cells/sealed? p %) (:plugs shape))) roof)
        item (lb/pick c blocks)]
    (if (nil? item)
      (do (site/remember-material! c {:pos roof})
          (ctx/emit! c :dig_in_failed :warn {:text "nothing to roof the pit with"})
          :done)
      (let [r (await (tidy/place! c cell item true))]
        (cond
          (and (= "occupied" (.-status r)) (not= cell roof) (not (dig-cells/sealed? p cell)))
          (let [_ (ctx/update-mem! c update :plug-occupied (fnil inc 0))
                n (:plug-occupied (ctx/mem c))]
            (if (< n site/max-refusals)
              :continue
              (site/fail-site! c :roof-failed (str "cannot seal the pit: the plug cell is occupied by " (or (u/seen-name p cell) "a block") " that does not seal"))))
          (#{"placed" "occupied"} (.-status r))
          (do (when (= "placed" (.-status r)) (dig-cells/note-placed! c cell item))
              (if (= cell roof) :done :continue))
          (dig-cells/keep-waiting! c :mob-since r) :yield
          :else (site/fail-site! c :roof-failed (str "cannot roof the pit: " (.-status r) (dig-cells/mob-text r))))))))
