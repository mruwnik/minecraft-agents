(ns triggers.survival.needs-bed
  "The needs-bed trigger: by day, a body whose respawn bed is lost (reset, found gone, or seen not a bed) or overdue for sleep (sh/max-days-awake in-game days since it last rested:
  sh/rest-since, its sleep or respawn, else the first thing it remembers) with
  no bed carried and none remembered within the urgent radius gets one (jobs.items.obtain: wool from sheep, planks), so
  the night job can put it down and sleep before phantoms come. Not in the first light or with a hostile known near
  (jobs.lib.dawn)."
  (:require [jobs.lib.body :as body]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.shelter :as sh]))

(def beds
  (mapv #(str % "_bed") ["white" "orange" "magenta" "light_blue" "yellow" "lime" "pink" "gray"
                         "light_gray" "cyan" "purple" "blue" "brown" "green" "red" "black"]))

(def defaults {:any-of beds :count 1 :explore true})

(defn needs-bed
  "Holds by day, awake, with no bed item carried, when the respawn bed is lost (sh/spawn-lost-pos), or when overdue
  (sh/max-days-awake) and no bed is remembered within sh/urgent-bed-radius."
  [p view _args]
  (boolean
   (and (not (body/recovery-pending? view :needs-bed))
        (not (sh/night? p))
        (not (sh/sleeping? p))
        (dawn/calm? p)
        (nil? (sh/carried-bed p))
        (or (some? (sh/spawn-lost-pos p view))
            (and (>= (or (sh/days-awake-in view) 0) sh/max-days-awake)
                 (nil? (sh/bed-in-view p view sh/urgent-bed-radius)))))))
