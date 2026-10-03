(ns jobs.movement.go-to
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(def doc
  "Walk to :pos. A partial walk continues next round and resets the count;
  three blocked walks in a row give up with an unreachable warn (last status
  and reason). Hands over {:arrived true}, or {:arrived false :reason
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

(defn ^:async round [c]
  (let [{:keys [pos range]} (:args c)]
    (if (<= (u/dist (u/self-pos c) pos) range)
      (arrived! c)
      (let [r (await (ctx/act c :moveTo (clj->js {:pos pos :range range})))]
        (case (.-status r)
          "arrived" (arrived! c)
          "partial" (do (ctx/update-mem! c assoc :blocked 0)
                        :continue)
          (let [tries (inc (:blocked (ctx/mem c) 0))]
            (ctx/update-mem! c assoc :blocked tries)
            (if (< tries max-blocked)
              :continue
              (give-up! c pos tries r))))))))
