(ns triggers.items.loose-items
  "The loose-items trigger: a seen dropped item nearby is worth picking up (jobs.lib.loose)."
  (:require [jobs.lib.danger :as danger-q]
            [jobs.lib.loose :as loose]
            [jobs.lib.util :as u]
            [triggers.storage.inventory-nearly-full :as nearly-full]))

(def defaults {})

(defn loose-items
  "Holds when some seen item within :radius is one jobs.lib.loose takes (worth the detour, not thrown, not owed to a
  live job, not in another's zone or claim) and the body is on the ground, has more than the nearly-full free slots
  and no real danger within :danger-radius. Every arg is a jobs.lib.loose option; the settings fill the rest.
  Low in the register: a survival reflex that holds fires first. Cooldown 30 s: what a run could not reach is
  remembered (:loose-left) and skipped."
  [p view args kn live]
  (let [o (loose/options args)]
    (boolean
     (and (not (false? (.-onGround (.self p))))
          (> (u/free-slots p) (nearly-full/nearly-full-free))
          (nil? (danger-q/nearest-danger p (:danger-radius o) {} {}))
          (seq (loose/candidates p view kn args live))))))
