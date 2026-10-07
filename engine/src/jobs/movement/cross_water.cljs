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
  The far shore is :pos (a land cell: it must be a seen shore), else the farthest seen shore within :radius of the body that is at least :min-cross blocks away.
  The check waits (:unseen) while a given :pos is not sensed. Ends {:status :done :land {:x :y :z} :boat id :recovered bool}, info boat.landed (from boat-land), or
  {:status :stopped :reason r}: :bad-args, :not-water (no water in sight to launch on), :no-far-shore (:pos is no shore, or none far enough is in sight),
  :no-boat (none carried and none to fetch), the launch's other stops (:refused, :no-support, :unreachable, :occupied, :board-failed, :failed; :cause names the child),
  the landing's stops (:blocked, :drive-failed, :not-landed, :recover-failed, :no-shore; :cause names the child).
  Resumes safely: aboard it goes on to the landing; on foot after the launch it lets boat-land finish or recover.

  Never :continue except when a child waits on the world.")

(def args
  {:pos {:doc "the far-shore land cell to step onto, [x y z] or {:x :y :z}; nil: the farthest seen shore at least :min-cross away" :type :pos :default nil}
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

(defn far-spot
  "The shore spot {:land :water} farthest from `from` within :radius that is at least :min-cross away, or nil."
  [p from {:keys [radius min-cross]}]
  (->> (shore/spots p from {:radius radius})
       (map #(assoc % :d (u/dist from (vehicle/centre (:land %)))))
       (filter #(>= (:d %) min-cross))
       (sort-by :d >)
       first))

(defn target-spot [c]
  (let [p (:primitives c)
        {:keys [radius min-cross]} (:args c)]
    (if-let [cell (given-pos c)]
      (shore/spot-at p cell)
      (far-spot p (u/self-pos c) {:radius radius :min-cross min-cross}))))

(defn water-in-sight? [c]
  (boolean (seq (look/seen-blocks (:primitives c) {:names ["water"] :radius (:radius (:args c)) :max 1 :live? true}))))

(defn crossing [c]
  (some #(when (= (:root c) (:job (:data %))) (:data %)) (ctx/entries c crossing-kind)))

(def launch-stops {:need :no-boat})

(defn ^:async land! [c land]
  (let [{:keys [recover]} (:args c)
        r (await (ctx/call-child c :land 'jobs.movement.boat-land {:pos land :recover recover}))
        res (ctx/child-result c :land)]
    (cond
      (= :continue r) :continue
      (and (= :done r) (not= :stopped (:status res)))
      (do (ctx/forget-where! c crossing-kind #(= (:root c) (:job %)))
          (result/finish! c (select-keys res [:land :boat :recovered])))
      ;; on foot after the launch with the landing already done: nothing left to do
      (and (crossing c) (= :not-aboard (:reason res)))
      (do (ctx/forget-where! c crossing-kind #(= (:root c) (:job %)))
          (result/finish! c {:land (:land (crossing c)) :recovered false}))
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
