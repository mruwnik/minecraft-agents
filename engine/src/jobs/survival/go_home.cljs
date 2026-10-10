(ns jobs.survival.go-home
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.places :as places]
            [jobs.lib.result :as result]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

(def doc
  "Walk home before night: to the remembered :home, else the remembered :bed (a go-to child, :range 2, walking on
  through the night). Waits (check) at night or with neither place known (:no-home). A place farther than :limit blocks ends stopped
  :too-far with one warn go-home.too-far and a :too-far memory (the night job then digs in or sleeps; :dusk-home skips that place). Done {:home pos} on arrival, which
  also records :home there unless a live one exists; stopped :walk-failed
  with the walk's reason otherwise. The :dusk-home trigger starts it when the day left no longer covers the walk;
  a work job it cuts is resumed by the scheduler afterwards.")

(a/defargs args
  {:limit {:doc "farthest place, in blocks, worth the walk" :spec (a/num-in 0 nil) :default 160}})

(def too-far-policy
  "One :too-far entry, one in-game day: dusk-home does not fire again for the home it names."
  {:cap 1 :ttl 1200000})

(defn place [c] (sh/home-target (ctx/view c)))

(defn check [c]
  (let [p (:primitives c)
        pos (place c)]
    (cond
      (not (.-isDay (.self p))) (ctx/wait c {:reason :night :text "night has come; the night job has the body"})
      (nil? pos) (ctx/wait c {:reason :no-home :text "no :home or :bed remembered to walk to"})
      :else true)))

(defn too-far!
  "Home is beyond :limit at dusk: one warn so the agent can dig in or turn back, and stop."
  [c pos]
  (let [d (js/Math.round (u/dist (u/self-pos c) pos))
        text (str "home at " (pr-str pos) " is " d " blocks away, beyond " (:limit (:args c)) ": night finds the body here; dig in or head back now")]
    (ctx/emit! c :go-home.too-far :warn {:pos pos :distance d :text text})
    (ctx/remember! c :too-far {:home pos} too-far-policy)
    (result/stop! c :too-far text :distance d)))

(defn ^:async walk! [c pos]
  (let [w (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos pos :range 2 :at-night :go}))
        r (ctx/child-result c :walk)]
    (cond
      (= :continue w) :continue
      (:arrived r) (do (places/offer! c :home pos)
                       (result/finish! c {:home pos}))
      :else (result/stop! c :walk-failed (or (:text r) "could not walk home") :walk r))))

(defn ^:async round [c]
  (let [pos (place c)]
    (if (> (u/dist (u/self-pos c) pos) (:limit (:args c)))
      (too-far! c pos)
      (await (walk! c pos)))))
