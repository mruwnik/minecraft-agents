(ns engine.triggers.tidy-pending
  "The tidy-pending trigger: a job that trespassed (engine.jobs.tidy) tidies up after itself without being asked. It
  holds while the body is safe (health at least :min-health, no hostile within :danger-radius) and a :tidy entry whose
  cell is loaded is either restorable now (the block is as the job left it, and a dug block's item is carried; fewer
  than engine.jobs.tidy/max-tries tries) or one that the last run of jobs.survival.restore-broken has not reported
  (its cell is not in the latest :tidy-reported entry): the run then restores, or warns once and forgets what it
  cannot. Persistence :stop: a run that leaves entries holds them, the condition turns false and fires again only
  when something has changed (items carried, the hostile gone, new entries). Entries whose cell is not loaded wait
  until the body is near."
  (:require [engine.jobs.tidy :as tidy]
            [engine.memory :as mem]))

(def defaults {:min-health 14 :danger-radius 8})

(defn loaded-entries
  "The :tidy data maps in view whose cell is loaded."
  [p view]
  (filterv #(some? (tidy/block-now p (:cell %))) (map :data (mem/entries view :tidy))))

(defn restorable? [p {:keys [tries] :as e}]
  (and (< tries tidy/max-tries) (nil? (tidy/why-not p e))))

(defn holds? [p view args]
  (let [args (merge defaults args)
        entries (loaded-entries p view)
        reported (set (:cells (:data (mem/latest view :tidy-reported))))]
    (boolean (and (seq entries)
                  (not (tidy/unsafe? p args))
                  (some #(or (restorable? p %) (not (contains? reported (:cell %)))) entries)))))

(def trigger
  {:name :tidy-pending
   :when (fn [p view args _kn] (holds? p view args))
   :job '(jobs.survival.restore-broken)
   :args defaults
   :persistence :stop})
