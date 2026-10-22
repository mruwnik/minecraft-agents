(ns jobs.survival.go-home
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [engine.contract :as contract]
            [jobs.lib.checks :as checks]
            [jobs.lib.cost :as cost]
            [jobs.lib.child :as child]
            [jobs.lib.next-call :as next-call]
            [jobs.lib.places :as places]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.util :as u]))

(def doc
  "Walk home before night: to the remembered :home, else the remembered :bed (a go-to child, :range 2, walking on
  through the night).
  Conditions (at the start only): a :home or :bed remembered, else :no-home.
  Done {:home pos} on arrival (also records :home there unless a live one exists), or at once when already within 2 of it.
  A place farther than :limit blocks is a condition: the call waits (:too-far {:pos :distance :text}) while the night job digs in or sleeps. A walk that did not arrive is
  nothing-possible {:reason :walk-failed :walk go-to's cause} with {:text}. The :dusk-home trigger starts it when the day left
  no longer covers the walk; a work job it cuts is resumed by the scheduler afterwards.
  With :commute? (the :dusk-home trigger's) a body in the ground (jobs.lib.shelter/in-ground?: its work goes on through the
  night) whose walk there and back, by go-to's quote, costs more than sheltering where it stands (jobs.lib.cost/commute-s)
  stays: nothing-possible {:reason :shelter-cheaper}, :next the night job (the :night trigger's). Other endings carry :next too (jobs.lib.next-call): what a failed walk names, then the call again.
  Quote: time by the straight distance at :speed blocks a second; done 1 visit.")

(a/defargs args
  {:commute? {:doc "price the walk there and back against sheltering where the body stands, when it is in the ground" :spec boolean? :default false}
   :limit {:doc "farthest place, in blocks, worth the walk" :spec (a/num-in 0 nil) :default sh-clock/home-limit}})

(defn place [c] (sh/home-target (ctx/view c)))

(a/defargs settings
  {::job-time-s {:default 300 :doc "Seconds the walk home may take." :spec (a/num-in 0 nil)}
   ::speed {:default 4.3 :doc "Blocks a second the walk home covers, for the quote." :spec (a/num-in 0 nil)}})

(def arrive-range "How near the place counts as home." 2)

(defn home-status
  "A home within :limit blocks: else :no-home, or :too-far {:pos :distance} with a text telling the agent to dig in or
  turn back."
  [c]
  (if-let [pos (place c)]
    (let [d (js/Math.round (u/dist (u/self-pos c) pos))]
      (if (> d (:limit (:args c)))
        (contract/later :too-far (str "home at " (pr-str pos) " is " d " blocks away, beyond " (:limit (:args c)) ": night finds the body here; dig in or head back now")
                        :pos pos :distance d)
        (contract/ok :home "home within reach" :pos pos)))
    (contract/later :no-home "no home or bed remembered")))

(def can-start? home-status)

(def target [])

(def defaults {:checks {:safe? '(not (threatened)) :fit? '(always)} :spend {:job-time ::job-time-s}})

(def meta {:category :reflex})

(def safe? ready/threatened)

(def fit? ready/ok)

(defn at-home? [c] (let [pos (place c)] (boolean (and pos (<= (u/dist (u/self-pos c) pos) arrive-range)))))

(defn done? [c]
  (let [d (and (not (checks/running? c)) (at-home? c))]
    (checks/goal d [:home "already home"] [:away "not home"] {:units :visits :done (if d 1 0) :left (if d 0 1)})))

(defn quote-of [c]
  (let [pos (place c)
        s (if pos (/ (u/dist (u/self-pos c) pos) (settings/get settings ::speed)) 0)]
    {:uses {:job-time [0 s (max (* 3 s) 10)]} :done [0 1 1]}))

(defn ^:async walk! [c pos]
  (let [e (await (child/call-walk* c :walk {:pos pos :range arrive-range}))]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (do (places/offer! c :home pos)
                              (ctx/end c :done nil {:home pos}))
      :else (ctx/end c :nothing-possible {:reason :walk-failed :walk (contract/cause-key e)}
                      {:text (or (get-in e [:data :text]) "could not walk home")}))))

(defn ^:async commute-dearer?
  "Whether the call is priced (:commute?), the body in the ground and go-to's quote of the way to pos dearer than sheltering here."
  [c pos]
  (let [p (:primitives c)]
    (and (:commute? (:args c)) (sh/in-ground? p (ctx/view c))
         (let [r (:route (await (ctx/quote c 'jobs.movement.go-to {:pos pos :range arrive-range})))]
           (and (:found r) (pos? (cost/commute-s true (or (:seconds r) 0) (sh/shelter-here-s))))))))

(defn ^:async round [c]
  (let [pos (place c)]
    (cond
      (nil? pos) (ctx/end c :condition-failed :no-home)
      (await (commute-dearer? c pos))
      (ctx/end c :nothing-possible {:reason :shelter-cheaper}
               {:text "in the ground, the walk home and back costs more than sheltering here; its work goes on"
                :next [(next-call/call 'jobs.survival.night {})]})
      :else (next-call/on-ending c (await (walk! c pos)) 'jobs.survival.go-home args))))
