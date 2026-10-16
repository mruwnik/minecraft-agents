(ns triggers.items.loose-items
  "The loose-items trigger: a seen dropped item nearby is worth picking up (jobs.lib.loose)."
  (:require [jobs.lib.body :as body]
            [jobs.lib.loose :as loose]
            [jobs.lib.util :as u]
            [triggers.storage.inventory-nearly-full :as nearly-full]))

(def defaults {})

(defn loose-items
  "Holds when some seen item within :radius is one jobs.lib.loose takes (worth the detour, not thrown, not owed to a
  live job, not in another's zone or claim) and the body is on the ground, has more than the nearly-full free slots.
  A danger near is the job's check (safe?), not the trigger's. It reads the jobs.lib.loose settings only, as the job it fires does
  (register :args are ignored), and counts live jobs as that job does (loose/live-ids), so both judge alike.
  Low in the register: a survival reflex that holds fires first. Cooldown 30 s: what a run could not reach is
  remembered (:loose-left) and skipped."
  [p view _args kn _live]
  (let [o (loose/options {})]
    (boolean
     (and (not (body/recovery-pending? view :loose-items))
          (not (false? (.-onGround (.self p))))
          (> (u/free-slots p) (nearly-full/nearly-full-free))
          (seq (loose/candidates p view kn {} nil))))))
