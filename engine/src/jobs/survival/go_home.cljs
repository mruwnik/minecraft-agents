(ns jobs.survival.go-home
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [engine.contract :as contract]
            [jobs.lib.child :as child]
            [jobs.lib.places :as places]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]))

(def doc
  "Walk home before night: to the remembered :home, else the remembered :bed (a go-to child, :range 2, walking on
  through the night).
  Conditions (at the start only): not night, else condition-failed :night; a :home or :bed remembered, else :no-home.
  Done {:home pos} on arrival (also records :home there unless a live one exists), or at once when already within 2 of it.
  A place farther than :limit blocks is a fact: condition-failed :too-far {:pos :distance :text} with one warn go-home.too-far and a
  :too-far memory (the night job then digs in or sleeps; :dusk-home skips that place). A walk that did not arrive is
  nothing-possible {:reason :walk-failed :walk go-to's cause} with {:text}. The :dusk-home trigger starts it when the day left
  no longer covers the walk; a work job it cuts is resumed by the scheduler afterwards.
  Quote: time by the straight distance at :speed blocks a second; done 1 visit.")

(a/defargs args
  {:limit {:doc "farthest place, in blocks, worth the walk" :spec (a/num-in 0 nil) :default 160}})

(def too-far-policy
  "One :too-far entry, one in-game day: dusk-home does not fire again for the home it names."
  {:cap 1 :ttl 1200000})

(defn place [c] (sh/home-target (ctx/view c)))

(a/defargs settings
  {::time-s {:default 300 :doc "Seconds the walk home may take." :spec (a/num-in 0 nil)}
   ::speed {:default 4.3 :doc "Blocks a second the walk home covers, for the quote." :spec (a/num-in 0 nil)}})

(def arrive-range "How near the place counts as home." 2)

(defn start-fact
  "The fact that bars starting, or nil."
  [c]
  (when-not (ctx/frozen-call c)
    (cond
      (not (.-isDay (.self (:primitives c)))) :night
      (nil? (place c)) :no-home)))

(def conditions [[:fn start-fact]])

(def target [])

(def defaults {:spend {:time ::time-s}})

(defn at-home? [c] (let [pos (place c)] (boolean (and pos (<= (u/dist (u/self-pos c) pos) arrive-range)))))

(defn done? [c] (let [d (at-home? c)] {:done? d :units :visits :done (if d 1 0) :left (if d 0 1)}))

(defn quote-of [c]
  (let [pos (place c)
        s (if pos (/ (u/dist (u/self-pos c) pos) (settings/get settings ::speed)) 0)]
    {:uses {:time [0 s (max (* 3 s) 10)]} :done [0 1 1]}))

(defn too-far!
  "Home is beyond :limit at dusk: one warn so the agent can dig in or turn back, and the fact."
  [c pos]
  (let [d (js/Math.round (u/dist (u/self-pos c) pos))
        text (str "home at " (pr-str pos) " is " d " blocks away, beyond " (:limit (:args c)) ": night finds the body here; dig in or head back now")]
    (ctx/emit! c :go-home.too-far :warn {:pos pos :distance d :text text})
    (ctx/remember! c :too-far {:home pos} too-far-policy)
    (ctx/end c :condition-failed :too-far {:pos pos :distance d :text text})))

(defn ^:async walk! [c pos]
  (let [e (await (child/call-walk* c :walk {:pos pos :range arrive-range :at-night :go}))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (places/offer! c :home pos)
                              (ctx/end c :done nil {:home pos}))
      :else (ctx/end c :nothing-possible {:reason :walk-failed :walk (contract/cause-key e)}
                      {:text (or (get-in e [:data :text]) "could not walk home")}))))

(defn ^:async round [c]
  (let [pos (place c)]
    (cond
      (nil? pos) (ctx/end c :condition-failed :no-home)
      (> (u/dist (u/self-pos c) pos) (:limit (:args c))) (too-far! c pos)
      :else (await (walk! c pos)))))
