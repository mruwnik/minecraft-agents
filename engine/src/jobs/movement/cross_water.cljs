(ns jobs.movement.cross-water
  (:require [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.look :as look]
            [jobs.lib.result :as result]
            [jobs.lib.shore :as shore]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]))

(def doc
  "Cross a lake or river by boat, from where the body stands (or floats) to a far shore it has seen. A choice for a caller or a travel job to make: go-to never boats.
  Children: jobs.movement.boat-launch (put a boat on the water and board it; a boat not carried is fetched), then jobs.movement.boat-land (drive to the water beside
  the far shore, step off facing the land, and with :recover take the boat back). A body already aboard skips the launch.
  The far shore is :pos (a land cell: it must be a seen shore), else a seen shore within :radius of the body, at least :min-cross blocks away, with water between it and the body
  (not the start bank): the one nearest :goal when given, else the farthest.
  The check waits (:unseen) while a given :pos is not sensed. Ends {:status :done :land {:x :y :z} :boat id :recovered bool}, info boat.landed (from boat-land), or
  {:status :stopped :reason r}: :bad-args, :not-water (no water in sight to launch on), :no-far-shore (:pos is no shore, or none far enough is in sight),
  :no-boat (none carried and none could be fetched; :cause from the launch), :boat-not-seen (the placed boat is not in sight), the launch's other stops (:refused, :no-support,
  :unreachable, :occupied, :board-failed, :failed; :cause names the child),
  the landing's stops (:blocked, :drive-failed, :not-landed, :recover-failed, :no-shore; :cause names the child).
  Resumes safely: aboard it goes on to the landing; on foot after the launch it lets boat-land finish or recover, and ends :done only when the body stands on the far shore
  (else :not-landed).

  Never :continue except when a child waits on the world.")

(def args
  {:pos {:doc "the far-shore land cell to step onto, [x y z] or {:x :y :z}; nil: choose one (see :goal)" :type :pos :default nil}
   :goal {:doc "with no :pos: pick the seen far shore nearest this cell; nil: the farthest" :type :pos :default nil}
   :radius {:doc "how far from the body to look for the far shore" :type :number :min 1 :default 24}
   :min-cross {:doc "an auto-chosen far shore is at least this many blocks from the start" :type :number :min 1 :default 6}
   :item {:doc "the boat item to launch; nil: the first carried boat or raft, else one is fetched" :default nil}
   :recover {:doc "take the boat back after landing" :default true}})

(def crossing-kind :cross-water)
(def crossing-policy {:cap 4 :ttl (* 10 60 1000)})

(defn given-pos [c] (when (:pos (:args c)) (:pos (b/parse (:args c)))))

(defn check
  "Waits while a given far-shore cell is not sensed."
  [c]
  (let [cell (given-pos c)]
    (or (nil? cell)
        (some? (u/seen-name (:primitives c) cell))
        (ctx/wait c {:reason :unseen :why "the far-shore cell is not sensed yet"}))))

(defn water-between?
  "Whether a seen water cell lies on the straight line from `from` to the spot's land cell, at the spot's water level."
  [p from {:keys [land water]}]
  (let [to (vehicle/centre land)
        n (js/Math.ceil (* 2 (u/dist from to)))]
    (some (fn [i]
            (let [t (/ i (max n 1))
                  cell {:x (js/Math.floor (+ (:x from) (* t (- (:x to) (:x from)))))
                        :y (:y water)
                        :z (js/Math.floor (+ (:z from) (* t (- (:z to) (:z from)))))}]
              (= "water" (u/seen-name p cell))))
          (range 1 n))))

(defn far-spot
  "The shore spot {:land :water} within :radius of `from`, at least :min-cross away and across water from it (not the start bank), or nil:
  the one nearest :goal when given, else the farthest."
  [p from {:keys [radius min-cross goal]}]
  (->> (shore/spots p from {:radius radius})
       (map #(assoc % :d (u/dist from (vehicle/centre (:land %)))))
       (filter #(and (>= (:d %) min-cross) (water-between? p from %)))
       (sort-by (if goal #(u/dist goal (vehicle/centre (:land %))) #(- (:d %))))
       first))

(defn target-spot [c]
  (let [p (:primitives c)
        {:keys [radius min-cross goal]} (:args c)]
    (if-let [cell (given-pos c)]
      (shore/spot-at p cell)
      (far-spot p (u/self-pos c) {:radius radius :min-cross min-cross
                                                  :goal (when goal (:pos (b/parse {:pos goal})))}))))

(defn water-in-sight? [c]
  (boolean (seq (look/seen-blocks (:primitives c) {:names ["water"] :radius (:radius (:args c)) :max 1 :live? true}))))

(defn crossing [c]
  (some #(when (= (:root c) (:job (:data %))) (:data %)) (ctx/entries c crossing-kind)))

(def launch-stops {:need :no-boat :no-boat :boat-not-seen})

(defn on-shore?
  "Whether the body stands on or next to the landing cell."
  [c land]
  (<= (u/dist (u/self-pos c) (vehicle/centre land)) 1.5))

(defn ^:async land! [c land]
  (let [{:keys [recover]} (:args c)
        r (await (ctx/call-child c :land 'jobs.movement.boat-land {:pos land :recover recover}))
        res (ctx/child-result c :land)]
    (cond
      (= :continue r) :continue
      (and (= :done r) (not= :stopped (:status res)))
      (do (ctx/forget-where! c crossing-kind #(= (:root c) (:job %)))
          (result/finish! c (select-keys res [:land :boat :recovered])))
      ;; on foot after the launch with the landing already done: done only if the body is on the far shore
      (and (crossing c) (= :not-aboard (:reason res)) (on-shore? c land))
      (do (ctx/forget-where! c crossing-kind #(= (:root c) (:job %)))
          (result/finish! c {:land land :recovered false}))
      (and (crossing c) (= :not-aboard (:reason res)))
      (do (ctx/forget-where! c crossing-kind #(= (:root c) (:job %)))
          (result/stop! c :not-landed "on foot, but not on the far shore"
                        :cause (result/cause-of :land res)))
      :else
      (do (ctx/forget-where! c crossing-kind #(= (:root c) (:job %)))
          (result/stop! c (or (:reason res) :failed) (or (:text res) "could not land on the far shore")
                        :cause (when res (result/cause-of :land res)))))))

(defn ^:async launch! [c spot]
  (let [{:keys [item]} (:args c)
        r (await (ctx/call-child c :launch 'jobs.movement.boat-launch {:action :launch :item item}))
        res (ctx/child-result c :launch)]
    (cond
      (= :continue r) :continue
      (and (= :done r) (not= :stopped (:status res)))
      (do (ctx/remember! c crossing-kind {:job (:root c) :land (:land spot)} crossing-policy)
          (await (land! c (:land spot))))
      :else
      (result/stop! c (get launch-stops (:reason res) (or (:reason res) :failed))
                    (or (:text res) "could not launch a boat")
                    :cause (when res (result/cause-of :launch res))))))

(defn ^:async round [c]
  (let [p (:primitives c)
        mounted (vehicle/mounted? p)
        saved (crossing c)]
    (cond
      (and (:pos (:args c)) (nil? (given-pos c))) (result/stop! c :bad-args (:error (b/parse (:args c))))
      saved (await (land! c (:land saved)))
      :else
      (let [spot (target-spot c)]
        (cond
          (nil? spot) (result/stop! c :no-far-shore (if (given-pos c) "the given cell is no seen shore" "no far shore in sight"))
          mounted (await (land! c (:land spot)))
          (not (water-in-sight? c)) (result/stop! c :not-water "no water in sight to launch a boat on")
          :else (await (launch! c spot)))))))
