(ns jobs.survival.night.sites
  "The night's settings for where it shelters: the home radius, the flee's candidates."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.shelter.clock :as sh-clock]
            [clojure.string :as str]))

(a/defargs settings
  {:jobs.survival.night.sites/home-radius {:default 12 :doc "A remembered roofed place (:roofed-places) this close is walked into, whatever the light, before a bed is used or put down; a carried bed is never put down in the open while one is." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/home-walk-margin-s {:default 60 :doc "Seconds of walking past the start of the dark that a remembered roofed place (:roofed-places) is still worth: its walk (all lit) may take past the dark's start before it is walked to, bed or not." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/home-walk-speed {:default 4.3 :doc "Blocks a second used to bound the roofed places looked at for a walk." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/home-walk-slack {:default 1.4 :doc "Factor on the straight line to a roofed place, for the real route, when bounding the places looked at." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/walk-radius {:default 32 :doc "A roofed place (:roofed-places) farther than this is not walked to." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/flee-radius {:default 64 :doc "When no shelter can be dug here, how far the night looks for somewhere safer (a known bed or roofed place, a seen cave or overhang to wall in, or seen ground the carried tools dig); 0 never flees." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/wall-value {:default 3 :doc "A block worth more than this each (jobs.lib.cost/item-value) is not counted as a wall block when a cave is walled in, and shut-in digs it again at dawn." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/max-sites {:default 4 :doc "Shut-in calls a night makes (each a failed site) before it cuts a niche or holds exposed." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/max-flees {:default 3 :doc "Walks to somewhere safer a night tries." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/ground-candidates {:default 16 :doc "Seen ground cells costed per flee: the nearest ones." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/cave-candidates {:default 16 :doc "Seen cave cells costed per flee: the nearest ones." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/niche-reach {:default 16 :doc "Blocks within which the last-resort niche's wall is looked for." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/hold-ms {:default 5000 :doc "How long one hold waits before the night looks again (the wait primitive does not wake a sleeping body)." :spec (a/int-in 1 nil)}
   :jobs.survival.night.sites/scan-radius {:default 64 :doc "The farthest the flee reads the world for ground and cave cells." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/cave-scan {:default 2048 :doc "Solid blocks one cave search reads: nearest first, so it reaches about 16 blocks around the body." :spec (a/int-in 0 nil)}})
(defn home-radius [] (settings/get settings :jobs.survival.night.sites/home-radius))
(defn home-walk-budget-s
  "Seconds a walk to a roofed place may take now: what is left before the dark plus the margin."
  [p]
  (+ (max 0 (/ (- sh-clock/dark-tick (.-timeOfDay (.self p))) 20))
     (settings/get settings :jobs.survival.night.sites/home-walk-margin-s)))

(defn home-walk-radius
  "Straight-line blocks within which a roofed place may be worth the walk: the budget at home-walk-speed, over the slack."
  [p]
  (/ (* (home-walk-budget-s p) (settings/get settings :jobs.survival.night.sites/home-walk-speed))
     (settings/get settings :jobs.survival.night.sites/home-walk-slack)))
(defn walk-radius [] (settings/get settings :jobs.survival.night.sites/walk-radius))
(defn flee-radius [] (settings/get settings :jobs.survival.night.sites/flee-radius))
(defn wall-value [] (settings/get settings :jobs.survival.night.sites/wall-value))
(defn niche-reach [] (settings/get settings :jobs.survival.night.sites/niche-reach))
(defn hold-ms [] (settings/get settings :jobs.survival.night.sites/hold-ms))
(defn scan-radius [] (settings/get settings :jobs.survival.night.sites/scan-radius))
(defn max-sites [] (settings/get settings :jobs.survival.night.sites/max-sites))

(defn forget-failed!
  "The night is over: forget shut-in's failed sites (:shut-in-failed) and the doorways it gave up, so the next night tries them again."
  [c]
  (ctx/forget-where! c :doorway-tried (constantly true))
  (ctx/forget-where! c :shut-in-failed (constantly true)))

(defn exposed-why
  "A short why for a night with no shelter, from the failed sites' reasons: counts per reason, and a hint to move when
  the server refused the walls and every other way failed for want of blocks that stay (spawn protection). With no site tried,
  why is shut-in's own text of what each way lacked (no blocks, no tool), when it gave one."
  [sites & [why]]
  (let [reasons (map :reason sites)
        counts (frequencies reasons)]
    (cond
      (empty? sites) (or why "no site tried")
      :else (str (str/join ", " (map (fn [[r n]] (str n " " (name r))) (sort-by (comp name key) counts)))
                 (when (and (some #{:walls-server-refused} reasons) (every? #{:walls-server-refused :roof-failed :lays-open} reasons))
                   "; the server refused every block placed (spawn protection?): move well away from here and try again")))))

(defn max-flees [] (settings/get settings :jobs.survival.night.sites/max-flees))

(defn ground-candidates [] (settings/get settings :jobs.survival.night.sites/ground-candidates))

(defn cave-candidates [] (settings/get settings :jobs.survival.night.sites/cave-candidates))

(defn cave-scan [] (settings/get settings :jobs.survival.night.sites/cave-scan))

