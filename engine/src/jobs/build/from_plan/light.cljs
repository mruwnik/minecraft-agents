(ns jobs.build.from-plan.light
  "A dark build site: the body lights it with torches (jobs.build.light-area) once per call, then builds; a player does not wait for dawn."
  (:require [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.build.plan :as plan]
            [jobs.lib.cost :as cost]
            [jobs.lib.need :as need]
            [jobs.lib.shelter.sense :as sense]
            [jobs.lib.torch :as torch]))

(defn can-light?
  "Whether torches are carried, or coal and sticks (or planks) to craft them, and the site is not lit yet this call."
  [c]
  (and (not (:light-tried (ctx/mem c)))
       (torch/lightable? (:primitives c))))

(def torches-wanted "How many torches a build that has none fetches to light its site." 8)

(defn fetch-due?
  "Whether torches are worth fetching to light the site instead of waiting wait-s seconds for the sky's light (nil: it never comes, a cave: left dark): none
  usable, no fetch tried yet this call, the call's ways allow one, and the wait costs more than a torch trip (jobs.lib.cost/light-cost)."
  [c wait-s]
  (and (not (:torches-tried (ctx/mem c)))
       (not (torch/lightable? (:primitives c)))
       (some? (need/fetch-opts c))
       (some? wait-s)
       (> wait-s (:cost (cost/light-cost {:carried 0})))))

(defn lights-itself
  "safe, but ok where it only waits for light (:exposed, lowered by :light) and the body can hang torches itself, or fetch them."
  [safe]
  (let [dark-wait? (fn [[kind _ data]] (and (= :later kind) (= :exposed (:reason data)) (contains? (:lowered-by data) :light)))
        wait-s (fn [[_ _ data]] (some-> (:recheck-after-ms data) (/ 1000)))
        lit (fn [f] (fn [c] (let [s (f c)]
                              (if (and (dark-wait? s) (or (can-light? c) (fetch-due? c (wait-s s))))
                                (contract/ok :torches "dark: the body hangs torches to build by")
                                s))))]
    (with-meta (lit safe) (update (meta safe) :jobs.lib.ready/exposure #(some-> % lit)))))

(defn dark-site?
  "Whether the body's cell or an open cell at or over an owed cell is dark as the body knows it (a solid cell has no light of
  its own to read)."
  [c cells]
  (let [p (:primitives c)
        open (for [{[x y z] :pos} cells dy [0 1]
                   :let [cell {:x x :y (+ y dy) :z z}]
                   :when (not (sense/solid-at? p cell))]
               cell)]
    (boolean (some #(not (sense/lit-here? p %)) (cons (sense/feet p) open)))))

(defn ^:async fetch-torches!
  "Fetch torches (jobs.lib.need) for a dark site with none to hang: :continue while the child waits on the world, :again once it ended
  (:torches-tried whatever came of it, so a failed fetch is asked once and the wait for light names its reason)."
  [c]
  (let [e (await (need/item! c "torch" torches-wanted (need/fetch-opts c)))]
    (when-not (= :continue e)
      (ctx/update-mem! c assoc :torches-tried true))
    (if (= :continue e) :continue :again)))

(defn site-wait
  "Seconds until the open cells at or over the owed cells are lit by the sky (nil: never)."
  [c cells]
  (let [p (:primitives c)]
    (sense/light-in p (for [{[x y z] :pos} cells dy [0 1]
                            :let [cell {:x x :y (+ y dy) :z z}]
                            :when (not (sense/solid-at? p cell))]
                        cell))))

(defn ^:async light!
  "jobs.build.light-area over the box round the owed cells while the site is dark, called again while it goes on; once it ends, :light-tried
  whatever came of it (the build goes on in the dark rather than ask again). With no torch to hang and a wait for light that costs more than
  a trip, torches are fetched first. :continue while it waits, :again once ended, nil when not due."
  [c cells]
  (let [owed (plan/owed cells)]
    (cond
      (or (empty? owed) (not (dark-site? c owed))) nil
      (can-light? c) (let [ps (map :pos owed)
                           lo (apply mapv min ps)
                           hi (apply mapv max ps)
                           d (await (ctx/call c :light 'jobs.build.light-area {:boxes [[lo hi]]}))]
                       (when-not (= :continue d)
                         (ctx/update-mem! c assoc :light-tried true))
                       (if (= :continue d) :continue :again))
      (fetch-due? c (site-wait c owed)) (await (fetch-torches! c)))))
