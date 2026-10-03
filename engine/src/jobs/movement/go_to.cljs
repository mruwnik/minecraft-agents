(ns jobs.movement.go-to
  (:require [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(def doc
  "Walk to :pos. A partial walk continues next round; three blocked walks
  give up with an unreachable warn.")

(def args
  {:pos {:doc "target position {:x :y :z}" :default nil}
   :range {:doc "how close counts as there" :default 1}})

(def max-blocked 3)

(defn check [_c] true)

(defn ^:async round [c]
  (let [{:keys [pos range]} (:args c)]
    (if (<= (u/dist (u/self-pos c) pos) range)
      :done
      (let [r (await (ctx/act c :moveTo (clj->js {:pos pos :range range})))]
        (case (.-status r)
          "arrived" :done
          "partial" :continue
          (let [tries (inc (:blocked (ctx/mem c) 0))]
            (ctx/update-mem! c assoc :blocked tries)
            (if (< tries max-blocked)
              :continue
              (do (ctx/emit! c :unreachable :warn {:target pos :tries tries
                                                    :text (str "gave up walking to " pos)})
                  :done))))))))
