(ns engine.child-ends
  "The job.child_ended events of the blocks jobs (and hunt) in a test's collected events.")

(defn ends
  "The job.child_ended events whose ending data holds the boolean key (:dug, :placed, :used; a count under the same name is a parent's), in order; seen is the collecting atom
  or a map holding it as :seen. Reads both the legacy-projected and the canonical event shape."
  [seen key]
  (let [seen (:seen seen seen)]
    (filterv (fn [e]
               (and (= :child_ended (:kind e))
                    (boolean? (get (:data (or (:ending e) (get-in e [:data :ending]))) key))))
             @seen)))

(defn data
  "The ending data of the child_ended event e."
  [e]
  (:data (or (:ending e) (get-in e [:data :ending]))))
