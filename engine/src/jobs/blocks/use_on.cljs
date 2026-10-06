(ns jobs.blocks.use-on
  (:require [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]))

(def doc
  "Right-click the block at :pos ([x y z] or {:x :y :z}) once, with :item in hand (the empty hand when nil), on :face.
  One call is the whole attempt: walk into reach (one jobs.movement.go-to call, range 3, no escalation), click. The
  check always passes. It yields :continue only when the walk waits on the world. Zones are not consulted: a click
  changes no block by itself.

  Ends with info blocks.use-on.done and {:used true|false :pos :item :face :reason}. :reason is the primitive's
  status (:used, :unchanged, :no-item, :no-room, :missing, :cannot, :unreachable, :failed); only :used is done.
  Anything else, or a walk that failed twice (:unreachable), is {:status :stopped}. A bad :pos or :face
  is :bad-args with a blocks.use-on.declined warn.")

(def args
  {:pos {:doc "the block to click, [x y z] or {:x :y :z}" :default nil}
   :item {:doc "the item to hold for the click; nil: the empty hand" :default nil}
   :face {:doc "the face clicked: up down north south east west" :default "up"}})

(def faces #{"up" "down" "north" "south" "east" "west"})
(def max-walks "Failed walks of one call before it stops :unreachable." 2)

(defn check [_c] true)

(defn finish! [c {:keys [used] :as result}]
  (ctx/emit! c :blocks.use-on.done :info
             (assoc result :text (str (if used "clicked " "did not click ") (pr-str (b/cell (:pos result)))
                                      (when-not used (str ": " (name (:reason result)))))))
  (ctx/result! c result)
  :done)

(defn ^:async round [c]
  (let [{:keys [item face] :or {face "up"}} (:args c)
        {:keys [pos error]} (b/parse (:args c))
        error (or error (when-not (faces face) (str "face is one of " (pr-str (sort faces)))))]
    (if error
      (do (ctx/emit! c :blocks.use-on.declined :warn {:reason :bad-args :text (str "blocks.use-on " error)})
          (ctx/result! c {:status :stopped :used false :reason :bad-args :text error})
          :done)
      (loop [fails 0]
        (let [w (when-not (b/in-reach? c pos) (await (b/walk! c pos)))]
          (cond
            (= :continue w) :continue
            (map? w) (if (>= (inc fails) max-walks)
                       (finish! c {:status :stopped :used false :pos pos :item item :face face :reason :unreachable :why (:unreachable w)})
                       (recur (inc fails)))
            :else
            (let [r (await (ctx/act c :useOn (clj->js (cond-> {:pos pos :face face} item (assoc :item item)))))
                  status (keyword (.-status r))]
              (finish! c (cond-> {:used (= :used status) :pos pos :item item :face face :reason status}
                           (not= :used status) (assoc :status :stopped))))))))))
