(ns jobs.forestry.harvest-wood
  (:require [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.waits :as waits]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.cond :as cond]
            [engine.settings :as settings]
            [clojure.string :as str]
            [jobs.forestry.fell-tree :as fell-tree]
            [jobs.lib.area :as area]
            [jobs.lib.checks :as checks]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.trip :as trip]
            [jobs.lib.util :as u]
            [jobs.lib.trees :refer [default-radius]]))

(def doc
  "Fell trees (jobs.forestry.fell-tree, which picks up its own logs; the :replant-owed trigger plants the saplings) until
  :count logs are carried, or one tree without :count.
  :center (default: where the body stands at the first round) with :radius is the area: a call first walks to the centre while the body stands farther than :radius from it (jobs.lib.area).
  Before the first tree it deposits at the known chest when the bag is fullish (jobs.lib.trip/ready!).
  With :count, the felling repeats on the next tree until that many logs are carried; done? holds at once when they are carried already (:done :already).
  Needs: a tree in range (a start wait :no-tree until one shows); an axe is fetched (from the call's :ways, which may leave out :pillar).
  Ends :done (the wood is the goal). Any other ending is the felling's, passed up with :got (the logs carried) in the data: condition-failed :no-tree when no tree is left, a need, a limit, nothing-possible :tree-unreachable / :tree-blocked (data :why, :columns).")

(a/defargs args
  {:species {:doc "log species; any when nil" :spec a/name? :default nil}
   :radius {:doc "search radius in blocks" :spec (a/num-in 0 nil) :default default-radius}
   :center {:doc "the centre of the area the job works, a cell [x y z] or {:x :y :z}; nil: where the body stands when the job first runs (jobs.lib.area)" :spec a/position? :default nil}
   :ignore-zones? :shared
   :avoid {:doc "columns [x z] of trees to leave alone (passed to fell-tree)" :spec (a/coll-of (a/pred "an [x z] column" vector?)) :default nil}
   :leafless? {:doc "when no tree with leaves is in range, fell leafless log columns (passed to fell-tree)" :spec boolean? :default false}
   :count {:doc "logs to carry: another tree is felled while fewer of the species' logs (any log when no species) are carried; nil: one tree" :spec (a/int-in 1 nil) :default nil}})

(def ways (conj need/fetch-ways :pillar))

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds a harvest may take: every tree's felling." :spec (a/num-in 0 nil)}})

(defn fell-args [{:keys [species radius ignore-zones? ways avoid leafless?]}]
  (cond-> {:species species :radius radius :ignore-zones? ignore-zones?}
    leafless? (assoc :leafless? true)
    (some? ways) (assoc :ways ways)
    (seq avoid) (assoc :avoid avoid)))

(defn logs-carried
  "How many logs of the species (any log when nil) the body carries."
  [c]
  (let [log? (if-let [sp (:species (:args c))] #{(str sp "_log")} #(str/ends-with? % "_log"))]
    (->> (u/inventory (:primitives c)) (filter #(log? (:name %))) (map :count) (reduce + 0))))

(defn more-trees?
  "Whether :count asks for more logs than are carried."
  [c]
  (let [n (:count (:args c))]
    (and n (< (logs-carried c) n))))

(defn end-with
  "The felling's ending e as this job's: its kind, cause and data, the logs carried added."
  [c e]
  (ctx/end c (:kind e) (:cause e) (assoc (:data e) :got (logs-carried c))))

(declare fell!)

(defn ^:async step
  "Calls the fell child once; a tree felled starts the next while :count asks for more logs, else the job is done. A
  child that waits on the world is :continue."
  [c]
  (let [w (when-not (:logs-before (ctx/mem c)) (await (area/walk-in! c)))]
    (if w
      (case w :there :again :continue :continue w)
      (await (fell! c)))))

(defn ^:async fell!
  [c]
  (when-not (:logs-before (ctx/mem c)) (ctx/update-mem! c assoc :logs-before (logs-carried c)))
  (let [e (waits/waiting-out c (await (ctx/call c :fell 'jobs.forestry.fell-tree (fell-args (:args c)))))]
    (cond
      (= :continue e) :continue
      (not= :done (:kind e)) (end-with c e)
      (more-trees? c) (do (ctx/update-mem! c #(-> % (assoc :logs-before nil) (update :felled (fnil inc 0)) (update :children dissoc :fell)))
                          :again)
      :else (do (ctx/update-mem! c assoc :phase :done)
                (ctx/end c :done nil nil)))))

(defn ^:async round
  [c]
  (if-let [t (await (trip/ready! c))]
    t
    (await (pace/steps! c #(step c)))))

;; ------------------------------------------------------------------ the contract

(def target [:species :center])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? (price/staying :work (ready/safe-at-or-roaming #(fell-tree/place (update % :args fell-args)))))

(def fit? "ready/fit-to-forage: a body only food heals felling wood (a rod, a hoe) is not held for health." ready/fit-to-forage)

(defn with-fell-args [check] (fn [c] (check (assoc c :args (fell-args (:args c))))))

(def no-tree-to-fell
  "fell-tree's :no-tree while nothing is felled yet and the next tree is still to be felled; the other phases are the
  children's."
  (let [no-tree (with-fell-args fell-tree/no-tree)]
    (fn [c]
      (if-not (pos? (:felled (ctx/mem c) 0))
        (no-tree c)
        (contract/ok :tree-found "a tree is felled or not wanted now")))))

(def can-start? (area/unless-outside no-tree-to-fell))

(defn done?
  "Ok once the last tree is felled, or (with :count) the logs are carried and no tree is under way; else later with the
  logs got this call."
  [c]
  (let [m (ctx/mem c)
        got (max 0 (- (logs-carried c) (:logs-before m (logs-carried c))))
        want (:count (:args c))
        idle? (not (checks/child-under-way? c))
        data (concat [:units :logs :done got :result {:logs got}] (when want [:left (max 0 (- want (logs-carried c)))]))]
    (if (or (= :done (:phase m)) (and want idle? (not (more-trees? c))))
      (apply contract/ok :harvested (str "harvest-wood: " got " logs got") data)
      (apply contract/later :harvesting (str "harvest-wood: " got " logs, a tree is being felled") data))))

(defn ^:async quote-of [c]
  (let [e (await (fell-tree/quote-of (assoc c :args (fell-args (:args c)))))
        trees (max 1 (min 8 (quot (or (:count (:args c)) 3) 3)))
        t (:job-time (:uses e))
        scale #(* trees (+ % 6))]
    (cond-> {:uses {:job-time [(scale (nth t 0)) (scale (nth t 1)) (max (settings/get settings ::job-time-s) (scale (nth t 2)))]}
             :done [0 1 1]}
      (:yields e) (assoc :yields (:yields e)))))
