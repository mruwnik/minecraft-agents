(ns jobs.lib.area
  "The area a search job works: the :center arg with the call's :radius. The job's target
  names :center, so calls over different areas are not duplicates. A job over an area works where the body stands;
  a caller that names a :center walks there first."
  (:require [engine.args :as a]
            [jobs.lib.checks :as checks]
            [jobs.lib.util :as u]))

(defn center
  "The call's :center as a cell {:x :y :z}, nil when it names none."
  [c]
  (some-> (:center (:args c)) a/cell))

(defn outside
  "{:reason :outside-area :center :distance :radius :text} while the call has a :center and the body stands farther
  than :radius from it, else nil."
  [c]
  (let [radius (:radius (:args c))
        at (center c)
        d (when at (u/dist (u/self-pos c) at))]
    (when (and at (> d radius))
      {:reason :outside-area :center at :radius radius :distance (js/Math.round d)
       :text (str "the body stands " (js/Math.round d) " blocks from the area centre " (pr-str at) " (radius " radius ")")})))

(def inside
  "The check that the body stands within :radius of the call's :center (nil: any place)."
  (checks/refusing :inside-area "in the call's area" outside))
