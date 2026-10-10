(ns jobs.survival.go-home
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.result :as result]
            [jobs.lib.util :as u]
            [triggers.survival.dusk-home :as dusk]))

(def doc
  "Walk home before night: to the remembered :home, else the remembered :bed (a go-to child, :range 2, walking on
  through the night). Waits (check) at night, with neither place known (:no-home) or when the place is farther than
  :limit blocks (:too-far); the night job then digs in or sleeps. Done {:home pos} on arrival; stopped :walk-failed
  with the walk's reason otherwise. The :dusk-home trigger starts it when the day left no longer covers the walk;
  a work job it cuts is resumed by the scheduler afterwards.")

(a/defargs args
  {:limit {:doc "farthest place, in blocks, worth the walk" :spec (a/num-in 0 nil) :default 160}})

(defn place [c] (dusk/target (ctx/view c)))

(defn check [c]
  (let [p (:primitives c)
        pos (place c)]
    (cond
      (not (.-isDay (.self p))) (ctx/wait c {:reason :night :text "night has come; the night job has the body"})
      (nil? pos) (ctx/wait c {:reason :no-home :text "no :home or :bed remembered to walk to"})
      (> (u/dist (u/self-pos c) pos) (:limit (:args c)))
      (ctx/wait c {:reason :too-far :pos pos :text (str "the bed or home at " (pr-str pos) " is farther than " (:limit (:args c)) " blocks")})
      :else true)))

(defn ^:async round [c]
  (let [pos (place c)
        w (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos pos :range 2 :at-night :go}))
        r (ctx/child-result c :walk)]
    (cond
      (= :continue w) :continue
      (:arrived r) (result/finish! c {:home pos})
      :else (result/stop! c :walk-failed (or (:text r) "could not walk home") :walk r))))
