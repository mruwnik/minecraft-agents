(ns jobs.build.from-plan.light
  "A dark build site: the body lights it with torches (jobs.build.light-area) once per call, then builds; a player does not wait for dawn."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.build.plan :as plan]
            [jobs.lib.shelter.sense :as sense]
            [jobs.lib.torch :as torch]))

(defn can-light?
  "Whether torches are carried, or coal and sticks (or planks) to craft them, and the site is not lit yet this call."
  [c]
  (let [p (:primitives c)]
    (and (not (:light-tried (ctx/mem c)))
         (or (pos? (torch/torches-held p))
             (and (pos? (torch/fuel-held p)) (pos? (torch/stick-supply p)))))))

(defn lights-itself
  "safe, but ok where it only waits for light (:exposed, lowered by :light) and the body can hang torches itself."
  [safe]
  (let [dark-wait? (fn [[kind _ data]] (and (= :later kind) (= :exposed (:reason data)) (contains? (:lowered-by data) :light)))
        lit (fn [f] (fn [c] (let [s (f c)] (if (and (dark-wait? s) (can-light? c)) (contract/ok :torches "dark: the body hangs torches to build by") s))))]
    (with-meta (lit safe) (update (meta safe) :jobs.lib.ready/exposure #(some-> % lit)))))

(defn dark-site?
  "Whether the cell the body stands in is dark as it knows it (a solid plan cell has no light of its own to read)."
  [c]
  (let [p (:primitives c)]
    (not (sense/lit-here? p (sense/feet p)))))

(defn ^:async light!
  "jobs.build.light-area over the box round the owed cells while the site is dark, called again while it goes on; once it ends, :light-tried
  whatever came of it (the build goes on in the dark rather than ask again). :continue while it waits, :again once ended, nil when not due."
  [c cells]
  (when (and (can-light? c) (dark-site? c) (seq (plan/owed cells)))
    (let [ps (map :pos (plan/owed cells))
          lo (apply mapv min ps)
          hi (apply mapv max ps)
          d (await (ctx/call c :light 'jobs.build.light-area {:boxes [[lo hi]]}))]
      (when-not (= :continue d)
        (ctx/update-mem! c assoc :light-tried true))
      (if (= :continue d) :continue :again))))
