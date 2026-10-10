(ns triggers.survival.needs-bed
  "The needs-bed trigger: by day, a body overdue for sleep (sh/max-days-awake in-game days since it last rested:
  sh/rest-since, its sleep or respawn, else the first thing it remembers) with
  no bed carried and none remembered within the urgent radius gets one (jobs.items.obtain: wool from sheep, planks), so
  the night job can put it down and sleep before phantoms come."
  (:require [jobs.lib.shelter :as sh]))

(def beds
  (mapv #(str % "_bed") ["white" "orange" "magenta" "light_blue" "yellow" "lime" "pink" "gray"
                         "light_gray" "cyan" "purple" "blue" "brown" "green" "red" "black"]))

(def defaults {:any-of beds :count 1 :explore true})

(defn needs-bed
  "Holds by day, awake, when overdue (sh/max-days-awake), no bed item is carried and no bed is remembered within
  sh/urgent-bed-radius."
  [p view _args]
  (boolean
   (and (not (sh/night? p))
        (not (sh/sleeping? p))
        (>= (or (sh/days-awake-in view) 0) sh/max-days-awake)
        (nil? (sh/carried-bed p))
        (nil? (sh/bed-in-view p view sh/urgent-bed-radius)))))
