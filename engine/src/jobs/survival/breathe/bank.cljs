(ns jobs.survival.breathe.bank
  "The bank step of jobs.survival.breathe for a body with no block to place: a bank two above the water cannot be climbed, but one
  natural block dug from its edge at the height above the water leaves a notch a swimmer climbs onto and steps up from."
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.breath :as breath]
            [jobs.lib.escape :as escape]
            [jobs.lib.shore :as shore]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.breathe-pocket :as pocket]
            [jobs.survival.breathe.air :as air]))

(defn solid-natural?
  "A seen block that may be dug from the bank: natural, not protected, not one that falls."
  [name]
  (boolean (and name (not (bs/air name)) (not= "water" name) (rules/natural? name)
                (not (escape/protected? name)) (not (contains? pocket/falling name)))))

(defn bank-cell
  "{:cell :water} for the nearest bank cell within radius of pos (the feet cell at the water surface) that is natural, over a
  solid block, under two open cells, and beside surface water; cells in failed (a seq) are skipped. nil if none."
  [p pos radius failed]
  (let [skip (set failed)
        fx (js/Math.floor (:x pos)) fy (js/Math.floor (:y pos)) fz (js/Math.floor (:z pos))]
    (some (fn [[dx dz]]
            (let [cell {:x (+ fx dx) :y (inc fy) :z (+ fz dz)}
                  name-at (fn [k] (u/seen-name p (update cell :y + k)))
                  water (some (fn [[sx sz]]
                                (let [w {:x (+ (:x cell) sx) :y fy :z (+ (:z cell) sz)}]
                                  (when (and (= "water" (u/seen-name p w)) (bs/air (u/seen-name p (update w :y inc)))) w)))
                              shore/sides)]
              (when (and water (not (skip cell)) (solid-natural? (name-at 0))
                         (let [below (name-at -1)] (and below (not (bs/air below)) (not= "water" below)))
                         (bs/air (name-at 1)) (bs/air (name-at 2)))
                {:cell cell :water water})))
          (breath/columns radius))))

(defn ^:async dig!
  "Dig the bank cell of bank-cell (the best tool in hand), when it fits share of the air left. :again either way; a cell that
  was not dug is not tried again."
  [c {:keys [cell water]} share]
  (let [p (:primitives c)
        self (.self p)
        air-ms (breath/air-left-ms self)]
    (await (tools/equip! c (u/seen-name p cell)))
    (let [allowed-ms (* share air-ms)]
      (if (< allowed-ms (.digTime p (clj->js cell)))
        (ctx/update-mem! c update :failed-banks (fnil conj []) cell)
        (do
          (when (< 4 (u/dist (u/pos-of (.-pos self)) cell))
            ;; raw moveTo kept: a few strokes to the water beside the bank, no plan wanted
            (await (ctx/act c :moveTo (clj->js {:pos water :range 1 :timeoutS 5}))))
          (access/trespass! c "breathe" (:trespass (access/choose c :dig [[cell]] identity)))
          (let [r (await (tidy/dig! c cell true {:timeoutS (/ allowed-ms 1000)}))]
            (when-not (contains? #{"dug" "missing"} (air/status r))
              (ctx/update-mem! c update :failed-banks (fnil conj []) cell)))))
      :again)))
