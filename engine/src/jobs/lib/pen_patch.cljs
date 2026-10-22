(ns jobs.lib.pen-patch
  "Patching a pen's leaks, shared by jobs.animals.pen-check and jobs.build.pen: a :gap is filled with a fence, a :climb
  or :perch-in fence gets one on top, each by a jobs.blocks.place child. Only the choice of the fence item differs between the jobs."
  (:require [engine.ctx :as ctx]
            [clojure.string :as str]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]))

(def patchable #{:gap :climb :perch-in})

(defn patch-cell
  "The cell [x y z] a patch fills for a leak ({:pos :why}), else nil: the :gap cell, the cell above a :climb or :perch-in fence."
  [{:keys [pos why]}]
  (when (patchable why)
    [(:x pos) (if (#{:climb :perch-in} why) (inc (:y pos)) (:y pos)) (:z pos)]))

(defn patch-cells
  "The distinct cells the patchable leaks need filled."
  [leaks]
  (vec (distinct (keep patch-cell leaks))))

(defn another-pass?
  "Whether a patch pass may follow: under max-passes done, and the last pass (if any) put something right."
  [pass pass-ok max-passes]
  (and (< (or pass 0) max-passes) (or (zero? (or pass 0)) (pos? (or pass-ok 0)))))

(defn over-gate?
  "Whether pos is the cell right above a fence gate: a block there leaves an open gate a doorway one block high."
  [c [x y z]]
  (some-> (u/seen-name (:primitives c) {:x x :y (dec y) :z z}) (str/ends-with? "_fence_gate")))

(defn leak-text
  "\": why at [x y z]\" listing the first leaks of result, with how many more; a :perch-in names the block stood on."
  [{:keys [leaks leaks-total]}]
  (let [shown (take 3 leaks)
        more (- (or leaks-total (count leaks)) (count shown))]
    (when (seq shown)
      (str ": " (str/join ", " (map (fn [{:keys [pos why stand stand-block]}]
                                      (str (name why) " at [" (:x pos) " " (:y pos) " " (:z pos) "]"
                                           (when stand (str " (an animal steps onto the top from the " (or stand-block "block") " at [" (:x stand) " " (:y stand) " " (:z stand) "]; fence higher there or take that block away)"))))
                                    shown))
           (when (pos? more) (str " and " more " more"))))))

(defn gate-drop-text
  [{:keys [gate-drops]}]
  (when-let [{:keys [gate drop]} (first gate-drops)]
    (str "; gate at [" (:x gate) " " (:y gate) " " (:z gate) "] has a drop of " drop " inside, animals will not step down")))

(defn ^:async place-fence!
  "One fence at pos by a blocks.place child (phase :patch); its result, or :continue while it runs. Args pass :ignore-zones?; extra is merged into the child's args.
  Never above a gate: that result is a :nothing-possible :over-gate and the leak stays reported."
  [c pos item extra]
  (if (over-gate? c pos)
    {:kind :nothing-possible :cause {:reason :over-gate :why "a fence above a gate would shut the doorway"}}
    (await (ctx/call c :patch 'jobs.blocks.place
                     (merge (select-keys (:args c) [:ignore-zones?]) extra {:pos pos :item item})))))
