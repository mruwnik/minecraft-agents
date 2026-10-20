(ns engine.pause-bounds-test
  "A pause counts as job time and wall time: a job that loops on pauses until the world changes needs a :wall-time default."
  (:require [cljs.test :refer [deftest is]]
            [jobs.animals.herd :as herd]
            [jobs.movement.stay-near :as stay-near]))

(deftest jobs-that-loop-on-pauses-bound-their-wall-time
  (doseq [[nm defaults] {"stay-near" stay-near/defaults "herd" herd/defaults}]
    (is (some? (get-in defaults [:spend :wall-time])) (str nm " has a default :wall-time"))))
