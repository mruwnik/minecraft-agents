(ns triggers.survival.needs-bed
  "The needs-bed trigger: by day, a body whose respawn bed is lost (reset, found gone, or seen not a bed) or overdue for sleep (sh-beds/max-days-awake in-game days since it last rested:
  sh-beds/rest-since, its sleep or respawn, else the first thing it remembers) with
  no bed carried and none remembered within the urgent radius gets one (jobs.items.obtain: wool from sheep, planks), so
  the night job can put it down and sleep before phantoms come. A when-convenient errand: it holds only in the last
  :lead-ticks of the day (early on, the body's own work goes first), and its search stays within a short walk
  (its default job is a :priced obtain: as far as the bed is worth). Not in the first light or with a hostile known near (jobs.lib.dawn),
  nor while a listed job will bring a bed (jobs.lib.makers: a from-plan whose plan has one, an obtain of one)."
  (:require [jobs.lib.body :as body]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.makers :as makers]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.spawn :as sh-spawn]))

(def defaults {:lead-ticks 6000})

(defn bed? [name] (or (= "bed" name) (.endsWith name "_bed")))

(defn why
  "Why a body by day needs a bed: {:why :spawn-lost :pos p} (sh-spawn/spawn-lost-pos), {:why :unslept :days d} (sh-beds/max-days-awake
  passed and sh-spawn/standing-bed finds none within (sh-beds/urgent-bed-radius)), else nil."
  [p view]
  (if-let [pos (sh-spawn/spawn-lost-pos p view)]
    {:why :spawn-lost :pos pos}
    (let [days (or (sh-beds/days-awake-in view) 0)]
      (when (and (>= days (sh-beds/max-days-awake))
                 (nil? (sh-spawn/standing-bed p view (sh-beds/urgent-bed-radius))))
        {:why :unslept :days days}))))

(defn needs-bed
  "Holds by day, in the last :lead-ticks before dusk, awake, on the surface (wool and planks are not underground), with no bed item carried and none a listed job brings, when why names a reason."
  [p view args kn live]
  (boolean
   (and (< 0 (- sh-clock/dusk-tick (.-timeOfDay (.self p))) (:lead-ticks (merge defaults args)))
        (not (body/recovery-pending? view :needs-bed))
        (not (sh-sense/night? p))
        (not (sh-sense/sleeping? p))
        (not (sh-sense/buried? p))
        (dawn/calm? p)
        (nil? (sh-beds/carried-bed p))
        (nil? (makers/listed-maker view kn live bed?))
        (some? (why p view)))))
