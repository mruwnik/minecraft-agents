(ns engine.bench-courses
  "The live tester's courses as a library for the bench harness (bench-lang/courses.mjs): names and {snapshot from goal}
  built by engine.path.courses. Exported by the :planner-bench build; run the harness from engine/ (the fixture loads
  js/path/*.mjs relative to the working directory)."
  (:require [engine.path.courses :as courses]))

(defn course-names [] (clj->js (courses/course-names)))

(defn course-snapshot
  "{snapshot, from, goal} of one course as JS"
  [name]
  (let [{:keys [snapshot from goal]} (courses/course-snapshot name)]
    #js {:snapshot snapshot :from (clj->js from) :goal (clj->js goal)}))
