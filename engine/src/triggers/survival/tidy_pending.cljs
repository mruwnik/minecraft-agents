(ns triggers.survival.tidy-pending
  "The tidy-pending trigger: a job that trespassed (jobs.lib.tidy) tidies up after itself without being asked.
  It holds while the body is safe (health at least :min-health, no hostile within :danger-radius) and a :tidy entry
  is pending. An entry is pending when its cell is loaded and either:
    it is restorable now: the block is as the job left it, a dug block's item is carried, and it has had fewer than
      jobs.lib.tidy/max-tries tries; or
    the last run of jobs.survival.restore-broken has not reported it (its cell is not in the latest :tidy-reported).
  The run restores what it can, and warns once about and forgets the rest.
  After a run that reports, the condition turns false. It holds again only when something changed (item carried, body
  out of the cell, hostile gone, new entries).
  It never holds while the body is airborne (a go-to step may be mid-jump). A cell a run tried is not held for again
  until the body is :move-far blocks from where it was or retry-s passed since the run counted its try (the entry's
  :tried stamp), so a cell no run can reach is tried once per window.
  Entries of a job that is still live (running, queued, paused) are ignored: that job may still be digging there.
  Entries in an unloaded cell wait until the body is near.
  Persistence is :cooldown (10 s), not :stop. A run cut short by a backoff leaves entries pending, so the condition
  stays true and a :stop latch would never clear. The retries are bounded: each try counts toward max-tries."
  (:require [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [engine.memory :as mem]))

(def defaults {:min-health 14 :danger-radius 8 :reach 3 :move-far 8})

(def retry-s 120)

(defn airborne? [p] (false? (.-onGround (.self p))))

(defn tried-lately?
  "Whether entry e, with its :tried stamp (written by restore-broken's counted try: :t :x :y :z :can), was tried less
  than retry-s ago with the body since within :move-far blocks of there, and whether it is restorable now (can) as
  then. An entry without a stamp was never tried."
  [{:keys [move-far]} now here can {:keys [tried]}]
  (boolean
   (and (:t tried)
        (= can (:can tried))
        (<= 0 (- now (:t tried)) (dec (* 1000 retry-s)))
        (< (js/Math.hypot (- (:x here) (:x tried)) (- (:z here) (:z tried))) move-far)
        (< (js/Math.abs (- (:y here) (:y tried))) move-far))))

(defn loaded-entries
  "The :tidy data maps in view whose cell is loaded and whose recording job is not live (not in the set live of
  job ids: running, queued or paused); an entry without a job id counts as ended."
  [p view live]
  (->> (map :data (mem/entries view :tidy))
       (remove #(contains? live (:job %)))
       (filterv #(some? (tidy/block-now p (:cell %))))))

(defn holds?
  "Whether some pending entry is worth a run now (see the ns doc); "
  [p view args live]
  (let [reported (set (:cells (:data (mem/latest view :tidy-reported))))]
    (filterv #(or (not (contains? reported (:cell %)))
                  (and (tidy/restorable? p %) (not (tidy/unreachable? p % (:reach args)))))
             (loaded-entries p view live))))

(defn tidy-pending
  [p view args _kn live]
  (let [args (merge defaults args)
        now (:now view)
        h (u/self-pos {:primitives p})
        here {:x (:x h) :y (:y h) :z (:z h)}
        pending (when-not (or (airborne? p) (tidy/unsafe? p args))
                  (holds? p view args live))]
    (boolean (some #(not (tried-lately? args now here (tidy/restorable? p %) %)) pending))))
