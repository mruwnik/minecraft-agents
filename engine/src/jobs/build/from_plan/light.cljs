(ns jobs.build.from-plan.light
  "A dark build site: the body lights it with torches (jobs.build.light-area) once per call, then builds; a player does not wait for dawn."
  (:require [engine.composite :as composite]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.build.light-area :as light-area]
            [jobs.items.obtain.price :as obtain-price]
            [jobs.lib.build.plan :as plan]
            [jobs.lib.cost :as cost]
            [jobs.lib.need :as need]
            [jobs.lib.shelter.sense :as sense]
            [jobs.lib.torch :as torch]))

(defn can-light?
  "Whether torches are carried and the site is not lit yet this call."
  [c]
  (and (not (:light-tried (ctx/mem c)))
       (pos? (torch/torches-held (:primitives c)))))

(defn light-args
  "The jobs.build.light-area args for the box round the owed cells."
  [owed]
  (let [ps (map :pos owed)]
    {:boxes [[(apply mapv min ps) (apply mapv max ps)]]}))

(defn torches-owed
  "Torches jobs.build.light-area would hang over the owed cells now, as its lighting plans them in the call it would get, at least one."
  [c owed]
  (if (empty? owed)
    1
    (let [[_ args] (ctx/child-job c 'jobs.build.light-area (light-args owed))]
      (max 1 (count (:spots (light-area/lighting (assoc c :slots (conj (:slots c) :light) :args args))))))))

(defn torches-short
  "Torches owed over the owed cells beyond those carried."
  [c owed]
  (max 0 (- (torches-owed c owed) (torch/torches-held (:primitives c)))))

(defn fetch-due?
  "Whether the torches short over the owed cells (torches-short) are worth getting (crafted from what is carried, or fetched) to light the site instead of waiting wait-s seconds for the
  sky's light (nil: it never comes, a cell under a canopy or in a cave, and no night is left to fetch through: any price). A never-lit cell at night
  (dawn?) needs them after dawn too: worth the night left plus their price by day (obtain.price/item-price at :sky 0), so it stays :later until dawn when the night's
  walk risk costs more than waiting: some short, none got yet this call, and the call's ways allow a torch
  obtain whose start check takes its price (the cheapest way, the death its walk risks) for no more than the wait saves (jobs.lib.cost/light-wait-worth).
  The torches are counted last: that plans the lighting."
  [c wait-s owed & [dawn?]]
  (and (not (:torches-tried (ctx/mem c)))
       (some? (need/fetch-opts c))
       (let [n (torches-short c owed)
             o (need/fetch-opts c)]
         (and (pos? n)
              (composite/ok? (need/item-status c "torch" n o (when wait-s (if dawn? (+ wait-s (obtain-price/item-price c "torch" n o 0)) (cost/light-wait-worth wait-s n)))))))))

(defn lights-itself
  "safe, but ok where it only waits for light (:exposed, lowered by :light) or no light comes (:exposed, a torch lowers it) and the body can hang torches
  itself, or fetch them for the owed cells of (cells-of c). A wait for light names :item \"torch\": torches carried end it."
  [safe cells-of]
  (let [exposed? (fn [[_ _ data]] (= :exposed (:reason data)))
        wait-for-light? (fn [[kind :as s]] (and (= :later kind) (exposed? s) (contains? (:lowered-by (nth s 2)) :light)))
        no-light? (fn [[kind :as s]] (and (= :never kind) (exposed? s) (contains? (:lowered-by (nth s 2)) :torch)))
        wait-s (fn [[_ _ data]] (some-> (:wait-ms data) (/ 1000)))
        dawn-s (fn [c] (let [s (sense/sky-in (:primitives c))] (when (pos? (or s 0)) s)))
        lit (fn [f] (fn [c] (let [s (f c)]
                              (if (and (or (wait-for-light? s) (no-light? s))
                                       (or (can-light? c) (if (wait-for-light? s) (fetch-due? c (wait-s s) (plan/owed (cells-of c))) (fetch-due? c (dawn-s c) (plan/owed (cells-of c)) true))))
                                (contract/ok :torches "dark: the body hangs torches to build by")
                                (cond
                                  (wait-for-light? s) (assoc-in s [2 :item] "torch")
                                  (and (no-light? s) (dawn-s c)) (let [ms (* 1000 (dawn-s c))]
                                                                   (-> s (assoc 0 :later) (update 2 assoc :wait-ms ms :recheck-after-ms ms)))
                                  :else s)))))]
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
  "Get n torches (jobs.lib.need) for a dark site short of them: :continue while the child waits on the world, :again once it ended
  (:torches-tried whatever came of it, so a failed fetch is asked once and the wait for light names its reason)."
  [c n]
  (let [e (await (need/item! c "torch" n (need/fetch-opts c)))]
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
  "While the site is dark: the torches short of what jobs.build.light-area would hang over the owed cells are got first when fetch-due?;
  then light-area over their box with the carried ones only (:ways #{:carried}), called again while it goes on; once it ends, :light-tried
  whatever came of it (the build goes on in the dark rather than ask again). :continue while it waits, :again once ended, nil when not due."
  [c cells]
  (let [owed (plan/owed cells)]
    (cond
      (or (empty? owed) (not (dark-site? c owed))) nil
      (and (not (:light-tried (ctx/mem c))) (fetch-due? c (site-wait c owed) owed)) (await (fetch-torches! c (torches-short c owed)))
      (can-light? c) (let [d (await (ctx/call c :light 'jobs.build.light-area (assoc (light-args owed) :ways #{:carried})))]
                       (when-not (= :continue d)
                         (ctx/update-mem! c assoc :light-tried true))
                       (if (= :continue d) :continue :again)))))
