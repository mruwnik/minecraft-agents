(ns jobs.lib.pen-patch
  "Patching a pen's leaks, shared by jobs.animals.pen-check and jobs.build.pen: a :gap is filled with a fence, a :climb
  or :perch-in fence gets one on top, each by a jobs.blocks.place child. Only the choice of the fence item differs between the jobs."
  (:require [engine.ctx :as ctx]
            [jobs.lib.need :as need]))

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

(defn ^:async place-fence!
  "One fence at pos by a blocks.place child (phase :patch); its result, or :continue while it runs. Args pass :ignore-zones?; extra is merged into the child's args."
  [c pos item extra]
  (await (ctx/call c :patch 'jobs.blocks.place
                   (merge (select-keys (:args c) [:ignore-zones?]) extra {:pos pos :item item}))))
