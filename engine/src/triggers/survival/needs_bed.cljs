(ns triggers.survival.needs-bed
  "The needs-bed trigger: by day, a body whose respawn bed is lost (reset, found gone, or seen not a bed) or overdue for sleep (sh/max-days-awake in-game days since it last rested:
  sh/rest-since, its sleep or respawn, else the first thing it remembers) with
  no bed carried and none remembered within the urgent radius gets one (jobs.items.obtain: wool from sheep, planks), so
  the night job can put it down and sleep before phantoms come. Not in the first light or with a hostile known near
  (jobs.lib.dawn)."
  (:require [jobs.lib.body :as body]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.shelter :as sh]))

(defn why
  "Why a body by day needs a bed: {:why :spawn-lost :pos p} (sh/spawn-lost-pos), {:why :unslept :days d} (sh/max-days-awake
  passed and sh/standing-bed finds none within sh/urgent-bed-radius), else nil."
  [p view]
  (if-let [pos (sh/spawn-lost-pos p view)]
    {:why :spawn-lost :pos pos}
    (let [days (or (sh/days-awake-in view) 0)]
      (when (and (>= days sh/max-days-awake)
                 (nil? (sh/standing-bed p view sh/urgent-bed-radius)))
        {:why :unslept :days days}))))

(defn needs-bed
  "Holds by day, awake, with no bed item carried, when why names a reason."
  [p view _args]
  (boolean
   (and (not (body/recovery-pending? view :needs-bed))
        (not (sh/night? p))
        (not (sh/sleeping? p))
        (dawn/calm? p)
        (nil? (sh/carried-bed p))
        (some? (why p view)))))
