(ns jobs.food.fish
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.result :as result]
            [jobs.lib.shore :as shore]
            [jobs.lib.util :as u]))

(def doc
  "Catch :count fish with a fishing rod from a bank beside seen water.
  The bank is a land cell with seen water (two deep) beside it at the level below, within :radius of the body (:pos names the water cell
  to cast at instead). The body walks there (go-to), holds the rod and casts: one cast waits for a bite up to :cast-s, then reels.
  A body with no rod gets one first (a jobs.items.obtain child, unless :fetch is false).
  Ends {:status :done :got n :caught {item n}} once :count fish (cod, salmon, tropical fish, pufferfish) came in, else {:status :stopped :got n :reason r}:
  :no-rod (none carried and none could be had), :no-water (no seen water with a bank within :radius), :unreachable (the bank could not be walked to),
  :no-bite (:max-misses casts in a row caught nothing) or :timeout (:timeout-s from the first round). Info fish.caught per fish.
  Resumes from the carried fish: the goal is the count carried at the first round plus :count.")

(a/defargs args
  {:count {:doc "fish to catch" :spec (a/int-in 1 nil) :default 3}
   :pos {:doc "the water cell to cast at, [x y z] or {:x :y :z}; nil: the nearest bank in sight" :spec ::a/pos :default nil}
   :radius {:doc "how far from the body to look for a bank" :spec (a/num-in 1 nil) :default 24}
   :cast-s {:doc "seconds one cast waits for a bite" :spec (a/num-in 1 nil) :default 45}
   :max-misses {:doc "casts in a row without a catch before the job gives up :no-bite" :spec (a/int-in 1 nil) :default 8}
   :timeout-s {:doc "seconds from the first round before the job gives up" :spec (a/num-in 1 nil) :default 600}
   :fetch {:doc "get a rod when none is carried (jobs.lib.fetch): true, a set of kinds or a map of limits; false ends :no-rod" :spec fetch/option? :default true}})

(defn check
  "Passes: the round decides, and ends :stopped with the reason when it cannot fish."
  [_c] true)

(def fish-items #{"cod" "salmon" "tropical_fish" "pufferfish"})

(defn fish-carried [c]
  (reduce + 0 (keep #(when (fish-items (:name %)) (:count %)) (u/inventory (:primitives c)))))

(defn rod-carried? [c] (boolean (some #(= "fishing_rod" (:name %)) (u/inventory (:primitives c)))))

(defn rod-problem [c]
  (when-not (rod-carried? c) {:reason :need :item "fishing_rod" :count 1}))

(defn spots
  "The banks {:land :water} to cast from: the one beside :pos when given, else the seen ones within :radius, nearest first;
  only water with water below it (two deep)."
  [c]
  (let [p (:primitives c)
        deep? #(= "water" (u/seen-name p (update (:water %) :y dec)))
        found (if-let [pos (:pos (:args c))]
                (let [w (near/cell-of pos)]
                  (filter #(= w (:water %)) (shore/spots p (u/self-pos c) {:radius (:radius (:args c))})))
                (shore/spots p (u/self-pos c) {:radius (:radius (:args c))}))]
    (filter deep? found)))

(defn stop! [c reason text]
  (result/stop! c reason text :got (- (fish-carried c) (:base (ctx/mem c) 0))))

(defn done! [c]
  (let [got (- (fish-carried c) (:base (ctx/mem c)))]
    (result/finish! c {:got got :text (str "caught " got " fish")})))

(defn ^:async cast! [c {:keys [water]}]
  (let [r (await (ctx/act c :fish (clj->js {:pos water :timeoutS (:cast-s (:args c))})))
        status (.-status r)]
    (case status
      "caught" (do (ctx/update-mem! c assoc :misses 0)
                   (ctx/emit! c :fish.caught :info {:item (.-item r) :text (str "caught a " (.-item r))})
                   :again)
      (do (ctx/update-mem! c update :misses (fnil inc 0)) :again))))

(defn ^:async step [c]
  (let [m (ctx/mem c)
        {:keys [count max-misses timeout-s]} (:args c)]
    (cond
      (>= (fish-carried c) (+ (:base m) count)) (done! c)
      (> (- (ctx/now c) (:started m)) (* 1000 timeout-s)) (stop! c :timeout "fishing took too long")
      (>= (:misses m 0) max-misses) (stop! c :no-bite (str max-misses " casts in a row caught nothing"))
      :else
      (or (await (fetch/fetch! c 'jobs.food.fish rod-problem))
          (if-not (rod-carried? c)
            (stop! c :no-rod "no fishing rod is carried and none could be had")
            (if-let [spot (first (spots c))]
              (let [w (await (near/go-near! c (:land spot) 1))]
                (case w
                  :there (await (cast! c spot))
                  :partial :continue
                  (stop! c :unreachable (str "could not walk to the bank at " (pr-str (:land spot))))))
              (stop! c :no-water "no seen water with a bank in reach")))))))

(defn ^:async round [c]
  (ctx/update-mem! c update :base #(or % (fish-carried c)))
  (ctx/update-mem! c update :started #(or % (ctx/now c)))
  (await (pace/steps! c #(step c))))
