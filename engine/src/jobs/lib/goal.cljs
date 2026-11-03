(ns jobs.lib.goal
  "Where a walk that was cut is going: body memory :walk-goal {:job root :slots :goal}, written by an open go-to of a
  job no trigger fired (a reflex's walk is no destination the body was bound for) and dropped when the walk ends or its
  call is cleaned up."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]))

(def pos-spec
  "The spec of a :goal arg: a position map or vector."
  (a/pred "{:x :y :z} or [x y z]" #(or (map? %) (vector? %))))

(def kind :walk-goal)

(def ttl-ms "A walk-goal no cleanup dropped is forgotten after this." (* 60 60 1000))

(defn- mine? [c] #(and (= (:job %) (:root c)) (= (:slots %) (:slots c))))

(defn- own-entries [c] (filter (comp (mine? c) :data) (ctx/entries c kind)))

(defn note!
  "This call's walk is open toward goal."
  [c goal]
  (when (nil? (:reflex c))
    (let [own (own-entries c)]
      (when-not (some #(= goal (:goal (:data %))) own)
        (when (seq own) (ctx/forget-where! c kind (mine? c)))
        (ctx/remember! c kind {:job (:root c) :slots (:slots c) :goal goal} {:cap 8 :ttl ttl-ms})))))

(defn clear!
  "This call's walk has ended. Writes only when it left a goal."
  [c]
  (when (seq (own-entries c)) (ctx/forget-where! c kind (mine? c))))

(defn interrupted-goal
  "The goal of the most recently noted open walk of a job other than root-id's; nil when none."
  [view root-id]
  (->> (mem/entries view kind)
       (remove #(= root-id (:job (:data %))))
       last
       :data
       :goal))
