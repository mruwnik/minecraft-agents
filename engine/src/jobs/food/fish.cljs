(ns jobs.food.fish
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.foods :as foods]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.near :as near]
            [jobs.lib.pace :as pace]
            [jobs.lib.shore :as shore]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.util :as u]))

(def doc
  "Carry :count fish, caught with a fishing rod from a bank beside seen water.
  The bank is a land cell with seen water (two deep) beside it at the level below, within :radius of the body (:pos names the water cell
  to cast at instead). The body walks there (go-to), holds the rod and casts: one cast waits for a bite up to :cast-s, then reels.
  A body with no rod gets one first (jobs.lib.need, unless :fetch is false).
  Ends with the data {:got n :text}, info fish.caught per fish, and the ending:
  - :done: :count fish (cod, salmon, tropical fish, pufferfish) are carried, or the call's :least (default 1, :all = :count)
    before any stop below, or when the time ends it (the engine's rule: report :limit).
  Before the first cast it waits (can-start?) on :no-rod (none carried and :fetch false) and :no-water (no seen water with a bank
  within :radius).
  - :condition-failed :no-water (the water gone mid-run), or a failed rod fetch (the cause names the rod, :because the fetch's ending).
  - :nothing-possible :unreachable (the bank could not be walked to) or :no-bite (:max-misses casts in a row caught nothing).
  - :limit-reached: the call's :spend :job-time (default ::job-time-s) is used up with fewer than :least caught; the engine ends it, with {:got}.
  The goal is to carry :count fish: the carried fish are counted at every round, so a body already carrying them is done,
  and fish eaten or stored mid-job are caught again.")

(a/defargs args
  {:count {:doc "fish to catch" :spec (a/int-in 1 nil) :default 3}
   :pos {:doc "the water cell to cast at, [x y z] or {:x :y :z}; nil: the nearest bank in sight" :spec ::a/pos :default nil}
   :radius {:doc "how far from the body to look for a bank" :spec (a/num-in 1 nil) :default 24}
   :cast-s {:doc "seconds one cast waits for a bite" :spec (a/num-in 1 nil) :default 45}
   :max-misses {:doc "casts in a row without a catch before the job gives up :no-bite" :spec (a/int-in 1 nil) :default 8}
   :fetch {:doc "get a rod when none is carried (jobs.lib.need): true, a set of kinds or a map of limits; false ends :no-rod" :spec need/option? :default true}})

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds from the first round before fishing gives up." :spec (a/num-in 1 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(def target [:pos])

(def defaults {:spend {:job-time ::job-time-s}})
(def safe? (ready/safe-at-or-if-lit #(when (:pos (:args %)) (work-place/spot %))))
(def fit? ready/fit?)

(defn rod-carried? [c] (boolean (some #(= "fishing_rod" (:name %)) (u/inventory (:primitives c)))))

(defn got "The fish carried: the goal is to carry :count, whatever was eaten or stored on the way." [c] (foods/fish-carried (:primitives c)))

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


(defn done? [c]
  (let [n (got c) want (:count (:args c))]
    (if (>= n want)
      (contract/ok :caught (str "caught " n " fish") :units :fish :done n :left 0 :result {:got n})
      (contract/later :fishing (str "caught " n " of " want " fish") :units :fish :done n :left (- want n) :result {:got n}))))

(defn time-limit "Seconds the call may fish." [c]
  (or (get-in (ctx/frozen-call c) [:spend :job-time]) (time-s)))

(defn quote-of [c]
  (let [most (time-limit c)]
    {:uses {:job-time [0 (min most (* 30 (:count (:args c) 1))) most]}}))

(defn stop!
  "End the call with the fish got so far and text as the data: done when the call's :least (default 1, :all = :count)
  were caught, else kind and cause."
  [c kind cause text]
  (let [least (:least (ctx/frozen-call c) 1)
        n (got c)]
    (if (>= n (if (= :all least) (:count (:args c)) least))
      (ctx/end c :done nil {:got n :text (str "caught " n " fish; " text)})
      (ctx/end c kind cause {:got n :text text}))))

(defn start-problem
  "At the call's start, why it cannot fish: [cause text], or nil."
  [c]
  (cond
    (and (not (rod-carried? c)) (nil? (need/parse-arg (:fetch (:args c))))) [:no-rod "no fishing rod is carried and :fetch is off"]
    (empty? (spots c)) [:no-water "no seen water with a bank in reach"]))

(defn can-start?
  "Later with the cause that bars the first cast."
  [c]
  (if-let [[cause text] (start-problem c)]
    (if (= :no-water cause) (contract/look cause text) (contract/later cause text))
    (contract/ok :ready "a rod and water to cast at")))

(defn ^:async cast! [c {:keys [water]}]
  (let [r (await (ctx/act c :fish (clj->js {:pos water :timeoutS (:cast-s (:args c))})))
        status (.-status r)]
    (case status
      "caught" (do (ctx/update-mem! c assoc :misses 0)
                   (ctx/emit! c :fish.caught :info {:item (.-item r) :text (str "caught a " (.-item r))})
                   :again)
      (do (ctx/update-mem! c update :misses (fnil inc 0)) :again))))

(defn ^:async get-rod!
  "A rod through jobs.lib.need (its time is not fishing time): :again once it is done, :continue while it waits, else the ending."
  [c]
  (let [limits (select-keys (need/parse-arg (:fetch (:args c))) [:how :explore])
        e (await (u/untimed! c #(need/item! c "fishing_rod" 1 limits)))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) :again
      :else (assoc-in e [:data :got] (got c)))))

(defn ^:async walk! [c {:keys [land]}]
  (let [r (await (child/call-walk* c :bank {:pos (select-keys land [:x :y :z]) :range 1 :ways #{:walk :clear-seal} :zone-tolls true}))]
    (cond
      (= :continue r) :continue
      (= :done (:kind r)) :arrived
      :else (stop! c :nothing-possible :unreachable (str "could not walk to the bank at " (pr-str land))))))

(defn ^:async step [c]
  (let [m (ctx/mem c)
        {:keys [count max-misses]} (:args c)]
    (cond
      (>= (got c) count) (let [n (got c)] (ctx/end c :done nil {:got n :text (str "caught " n " fish")}))
      (>= (:misses m 0) max-misses) (stop! c :nothing-possible :no-bite (str max-misses " casts in a row caught nothing"))
      (not (rod-carried? c)) (await (get-rod! c))
      :else
      (if-let [spot (first (spots c))]
        (let [w (await (walk! c spot))]
          (if (= :arrived w) (await (cast! c spot)) w))
        (or (await (look/survey! c))
            (stop! c :condition-failed :no-water "no seen water with a bank in reach"))))))

(defn ^:async round [c]
  (await (pace/steps! c #(step c))))

(def meta {:tags [:food]})
