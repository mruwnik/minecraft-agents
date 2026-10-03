(ns jobs.movement.go-to
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(def doc
  "Walk to :pos. A walk that ends more than 1 closer than any before (:best)
  is progress and resets the count; any other walk that does not arrive
  (partial without a new best, blocked) counts, and three in a row give up
  with an unreachable warn (last status and reason). A far hop that fell back
  to the XZ point is told as a :hop-fallback info event. Hands over {:arrived true}, or {:arrived false :reason
  :unreachable} when it gave up (ctx/result!), and emits it as a :result info
  event.")

(def args
  {:pos {:doc "target position {:x :y :z}" :default nil}
   :range {:doc "how close counts as there" :default 1}})

(def max-blocked 3)

(defn check [_c] true)

(defn finish!
  "Hand result over (ctx/result!), emit it as a :result info event, end the job."
  [c result]
  (ctx/result! c result)
  (ctx/emit! c :result :info result)
  :done)

(defn arrived! [c] (finish! c {:arrived true}))

(defn give-up! [c pos tries r]
  (ctx/emit! c :unreachable :warn {:target pos :tries tries :status (.-status r) :reason (.-reason r)
                                    :text (str "gave up walking to " pos)})
  (finish! c {:arrived false :reason :unreachable}))

(defn note-hop! [c pos r]
  (when (= "xz" (.-hop r))
    (ctx/emit! c :hop-fallback :info {:target pos :text "far hop: no surface reachable from underground, walking to the XZ point"})))

(defn ^:async round [c]
  (let [{:keys [pos range]} (:args c)
        d (u/dist (u/self-pos c) pos)]
    (if (<= d range)
      (arrived! c)
      (let [r (await (ctx/act c :moveTo (clj->js {:pos pos :range range})))
            best (:best (ctx/mem c) d)]
        (note-hop! c pos r)
        (if (= "arrived" (.-status r))
          (arrived! c)
          (let [progress? (and (number? (.-distance r)) (< (.-distance r) (dec best)))
                tries (if progress? 0 (inc (:blocked (ctx/mem c) 0)))]
            (ctx/update-mem! c assoc :blocked tries :best (if progress? (.-distance r) best))
            (if (< tries max-blocked)
              :continue
              (give-up! c pos tries r))))))))
