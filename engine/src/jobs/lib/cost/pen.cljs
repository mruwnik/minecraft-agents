(ns jobs.lib.cost.pen
  "What gaps in a pen's fence cost: each animal walks out through a gap at a rate a second, Gamma(a, b) a priori (a
  escapes over b animal-seconds by one gap), so over t seconds with g gaps an animal is gone with chance
  1 - (1 + g t / b)^-a (the Gamma mean of 1 - exp(-rate g t)), and a herd of n loses n times that."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost.survival :as survival]
            [jobs.lib.cost.value :as value]))

(a/defargs settings
  {::escape-prior {:default {:escapes 26 :animal-s 765}
                   :doc "Gamma prior of the rate an animal walks out through one fence gap: :escapes over :animal-s animal-seconds, the :setting of node tools/event-counts.mjs pen-escapes over the :pen-escapes rows of worlds/test/priors-counts.edn: the educated guess of 1 per 300 animal-seconds plus the fixture pen-gap-escapes (cows in a 5x5 ring with one gap, 30 s watches: 25 of 28 cows out, 465 animal-s; 7 of 8 alone and 18 of 20 in fours, so the chance does not grow with the herd and its escapes do). An escaped animal counts half the watch."
                   :spec (a/map-with {:escapes a/pos-num? :animal-s a/pos-num?})}
   ::pass-gap-s {:default 1200 :spec (a/num-in 0 nil)
                 :doc "Seconds a gap left now stays open until the next pass mends it: a Minecraft day, a proxy (no pen keeps a pass schedule or a record of its last pass)."}})

(defn escape-chance
  "The chance an animal in a pen with gaps fence gaps has walked out within secs seconds."
  [gaps secs]
  (let [{:keys [escapes animal-s]} (settings/get settings ::escape-prior)]
    (- 1 (survival/gamma-survival escapes animal-s (* gaps (max 0 secs))))))

(defn gap-loss
  "Seconds of animal worth (value/animal-worth of mob) a herd of n loses through gaps fence gaps left open ::pass-gap-s."
  [mob n gaps]
  (if (zero? gaps)
    0
    (* n (escape-chance gaps (settings/get settings ::pass-gap-s)) (value/animal-worth mob))))

(defn herd-loss
  "Seconds of animal worth a herd (its mob kinds) loses through one gap left open secs seconds."
  [mobs secs]
  (* (escape-chance 1 secs) (reduce + 0 (map value/animal-worth mobs))))
