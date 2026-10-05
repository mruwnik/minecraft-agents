(ns engine.triggers.tidy-pending
  "The tidy-pending trigger: a job that trespassed (engine.jobs.tidy) tidies up after itself without being asked.
  It holds while the body is safe (health at least :min-health, no hostile within :danger-radius) and a :tidy entry
  is pending. An entry is pending when its cell is loaded and either:
    it is restorable now: the block is as the job left it, a dug block's item is carried, and it has had fewer than
      engine.jobs.tidy/max-tries tries; or
    the last run of jobs.survival.restore-broken has not reported it (its cell is not in the latest :tidy-reported).
  The run restores what it can, and warns once about and forgets the rest.
  After a run that reports, the condition turns false. It holds again only when something changed (item carried, body
  out of the cell, hostile gone, new entries).
  Entries of a job that is still live (running, queued, paused) are ignored: that job may still be digging there.
  Entries in an unloaded cell wait until the body is near.
  Persistence is :cooldown (10 s), not :stop. A run cut short by a backoff leaves entries pending, so the condition
  stays true and a :stop latch would never clear. The retries are bounded: each try counts toward max-tries."
  (:require [engine.jobs.tidy :as tidy]
            [engine.memory :as mem]))

(def defaults {:min-health 14 :danger-radius 8 :reach 3})

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
                  (some #(or (not (contains? reported (:cell %)))
                             (and (restorable? p %) (not (tidy/unreachable? p % (:reach args)))))
                        entries)))))

(def trigger
  {:name :tidy-pending
   :when (fn [p view args _kn live] (holds? p view args live))
   :job '(jobs.survival.restore-broken)
   :args defaults
   :persistence :cooldown
   :cooldown-s 10})
