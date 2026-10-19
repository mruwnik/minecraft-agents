(ns jobs.survival.night.sites
  "The night's settings for where it shelters: the home radius, the retry rules of an exposed hold and the flee's candidates."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.survival.shut-in-night :as shut-night]
            [clojure.string :as str]))

(a/defargs settings
  {:jobs.survival.night.sites/home-radius {:default 12 :doc "A remembered roofed place (:roofed-places) this close is walked into, whatever the light, before a bed is used or put down; a carried bed is never put down in the open while one is." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/walk-radius {:default 32 :doc "A roofed place (:roofed-places) farther than this is not walked to." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/flee-radius {:default 64 :doc "When no shelter can be dug here, how far the night looks for somewhere safer (a known bed or roofed place, a seen cave or overhang to wall in, or seen ground the carried tools dig); 0 never flees." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/wall-value {:default 3 :doc "A block worth more than this each (jobs.lib.cost/item-value) is not counted as a wall block when a cave is walled in, and shut-in digs it again at dawn." :spec (a/num-in 0 nil)}
   :jobs.survival.night.sites/max-sites {:default 4 :doc "Shut-in calls a night makes (each a failed site) before it cuts a niche or holds exposed." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/retry-after-ms {:default 660000 :doc "How long a body holds exposed before it tries to dig in again (past shut-in's own 10-minute give-up)." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/max-retries {:default 2 :doc "Dig-in retries after a night went exposed." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/max-flees {:default 3 :doc "Walks to somewhere safer a night tries." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/ground-candidates {:default 16 :doc "Seen ground cells costed per flee: the nearest ones." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/cave-candidates {:default 16 :doc "Seen cave cells costed per flee: the nearest ones." :spec (a/int-in 0 nil)}
   :jobs.survival.night.sites/cave-scan {:default 2048 :doc "Solid blocks one cave search reads: nearest first, so it reaches about 16 blocks around the body." :spec (a/int-in 0 nil)}})
(defn home-radius [] (settings/get settings :jobs.survival.night.sites/home-radius))
(defn walk-radius [] (settings/get settings :jobs.survival.night.sites/walk-radius))
(defn flee-radius [] (settings/get settings :jobs.survival.night.sites/flee-radius))
(defn wall-value [] (settings/get settings :jobs.survival.night.sites/wall-value))
(defn max-sites [] (settings/get settings :jobs.survival.night.sites/max-sites))

(defn forget-failed!
  "The night is over: forget shut-in's failed sites (:shut-in-failed) and the doorways it gave up, so the next night tries them again."
  [c]
  (ctx/forget-where! c :doorway-tried (constantly true))
  (ctx/forget-where! c :shut-in-failed (constantly true)))

(defn exposed-why
  "A short why for a night with no shelter, from the failed sites' reasons: counts per reason, and a hint to move when
  the server refused the walls and every other way failed for want of blocks that stay (spawn protection)."
  [sites]
  (let [reasons (map :reason sites)
        counts (frequencies reasons)]
    (cond
      (empty? sites) "no site tried"
      :else (str (str/join ", " (map (fn [[r n]] (str n " " (name r))) (sort-by (comp name key) counts)))
                 (when (and (some #{:walls-server-refused} reasons) (every? #{:walls-server-refused :roof-failed :lays-open} reasons))
                   "; the server refused every block placed (spawn protection?): move well away from here and try again")))))

(defn retry-after-ms [] (settings/get settings :jobs.survival.night.sites/retry-after-ms))

(defn max-retries [] (settings/get settings :jobs.survival.night.sites/max-retries))

(defn retry-due?
  "Whether the exposed hold digs in again: under max-retries, and the body has moved off the spot it went exposed on
  or retry-after-ms passed."
  [c]
  (let [{:keys [pos at retries]} (:exposed (ctx/mem c))]
    (and pos (< (or retries 0) (max-retries))
         (or (>= (u/dist (sh-sense/feet (:primitives c)) pos) (inc (shut-night/futile-radius)))
             (>= (- (ctx/now c) at) (retry-after-ms))))))

(defn max-flees [] (settings/get settings :jobs.survival.night.sites/max-flees))

(defn ground-candidates [] (settings/get settings :jobs.survival.night.sites/ground-candidates))

(defn cave-candidates [] (settings/get settings :jobs.survival.night.sites/cave-candidates))

(defn cave-scan [] (settings/get settings :jobs.survival.night.sites/cave-scan))

