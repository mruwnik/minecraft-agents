(ns engine.triggers.tidy-pending
  "The tidy-pending trigger: a job that trespassed (engine.jobs.tidy) tidies up after itself without being asked. It
  holds while the body is safe (health at least :min-health, no hostile within :danger-radius) and a :tidy entry whose
  cell is loaded is either restorable now (the block is as the job left it, and a dug block's item is carried; fewer
  than engine.jobs.tidy/max-tries tries) or one that the last run of jobs.survival.restore-broken has not reported
  (its cell is not in the latest :tidy-reported entry): the run then restores, or warns once and forgets what it
  cannot. A run that reports leaves only entries it cannot restore now, so the condition turns false and holds again
  only when something has changed (item carried, body out of the cell, hostile gone, new entries). Persistence
  :cooldown 10 s, not :stop: a run cut short by a backoff leaves entries unreported or still restorable (under
  max-tries), the condition stays true, and a :stop latch, cleared only by a false condition, would then hold for
  good, later entries included. The retry after the cooldown is bounded: each try counts toward max-tries, and a
  run that ends reports. Entries whose cell is not loaded wait
  until the body is near. Entries recorded by a job that is still live (running, queued, paused) are ignored: the
  job may still be digging there; the trigger fires once it has ended."
  (:require [engine.jobs.tidy :as tidy]
            [engine.memory :as mem]))

(def defaults {:min-health 14 :danger-radius 8})

(defn loaded-entries
  "The :tidy data maps in view whose cell is loaded and whose recording job is not live (not in the set live of
  job ids: running, queued or paused); an entry without a job id counts as ended."
  [p view live]
  (->> (map :data (mem/entries view :tidy))
       (remove #(contains? live (:job %)))
       (filterv #(some? (tidy/block-now p (:cell %))))))

(defn restorable? [p {:keys [tries] :as e}]
  (and (< tries tidy/max-tries) (nil? (tidy/why-not p e))))

(defn holds? [p view args live]
  (let [args (merge defaults args)
        entries (loaded-entries p view live)
        reported (set (:cells (:data (mem/latest view :tidy-reported))))]
    (boolean (and (seq entries)
                  (not (tidy/unsafe? p args))
                  (some #(or (restorable? p %) (not (contains? reported (:cell %)))) entries)))))

(def trigger
  {:name :tidy-pending
   :when (fn [p view args _kn live] (holds? p view args live))
   :job '(jobs.survival.restore-broken)
   :args defaults
   :persistence :cooldown
   :cooldown-s 10})
