(ns jobs.survival.night.sites
  "The night's settings for where it shelters: the time a walk home may take, the flee's candidates."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.shelter.clock :as sh-clock]
            [clojure.string :as str]))

(a/defargs settings
  {:jobs.survival.night.sites/home-walk-margin-s {:default 60 :doc "Seconds of walking past the start of the dark that a remembered roofed place (:roofed-places) is still worth: its walk (all lit) may take past the dark's start before it is walked to, bed or not." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/flee-radius {:default 64 :doc "When no shelter can be dug here, how far the night looks for somewhere safer (a known bed or roofed place, a seen cave or overhang to wall in, or seen ground the carried tools dig); 0 never flees." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/wall-value {:default 3 :doc "A block worth more than this each (jobs.lib.cost/item-value) is not counted as a wall block when a cave is walled in, and shut-in digs it again at dawn." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/max-sites {:default 4 :doc "Shut-in calls a night makes (each a failed site) before it cuts a niche or holds exposed." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/max-flees {:default 3 :doc "Walks to somewhere safer a night tries." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/niche-reach {:default 16 :doc "Blocks within which the last-resort niche's wall is looked for." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/hold-ms {:default 5000 :doc "How long one hold waits before the night looks again (the wait primitive does not wake a sleeping body)." :spec (a/int-in 1 nil)}})
(defn home-walk-budget-s
  "Seconds a walk to a roofed place may take now: what is left before the dark plus the margin."
  [p]
  (+ (max 0 (/ (- sh-clock/dark-tick (.-timeOfDay (.self p))) 20))
     (settings/get settings :jobs.survival.night.sites/home-walk-margin-s)))

(defn flee-radius [] (settings/get settings :jobs.survival.night.sites/flee-radius))
(defn wall-value [] (settings/get settings :jobs.survival.night.sites/wall-value))
(defn niche-reach [] (settings/get settings :jobs.survival.night.sites/niche-reach))
(defn hold-ms [] (settings/get settings :jobs.survival.night.sites/hold-ms))
(defn max-sites [] (settings/get settings :jobs.survival.night.sites/max-sites))

(defn forget-failed!
  "The night is over: forget shut-in's failed sites (:shut-in-failed), where the night ended exposed and the doorways it gave up, so the next night tries them again."
  [c]
  (ctx/forget-where! c :doorway-tried (constantly true))
  (ctx/forget-where! c :shut-in-failed (constantly true))
  (ctx/forget-where! c :night-exposed (constantly true)))

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

