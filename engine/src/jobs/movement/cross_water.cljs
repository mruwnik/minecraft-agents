(ns jobs.movement.cross-water
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.movement.boat-launch :as launch]
            [jobs.lib.blocks :as b]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.look :as look]
            [jobs.lib.sense :as sense]
            [jobs.lib.shore :as shore]
            [jobs.lib.util :as u]
            [jobs.lib.vehicle :as vehicle]))

(def doc
  "Cross a lake or river by boat, from where the body stands (or floats) to a far shore it has seen. A choice for a caller or a travel job to make: go-to never boats.
  Children: jobs.movement.boat-launch (put a boat on the water and board it; a boat not carried is fetched), then jobs.movement.boat-land (drive to the water beside
  the far shore, step off facing the land, and with :recover take the boat back). A body already aboard skips the launch.
  The far shore is :pos (a land cell: it must be a seen shore), else a seen shore within :radius of the body, at least :min-cross blocks away, with water between it and the body
  (not the start bank): the one nearest :goal when given, else the farthest.
  A given :pos is looked at first; one the body still cannot make out is crossed toward all the same (boat-land drives nearer to it).
  Impossible: :bad-args (:pos names no cell, :item is no boat). Conditions, asked before every round (a listed call waits on them; the engine looks round once first): :no-far-shore (:pos is no shore,
  or none far enough is in sight), :not-water (no water in sight to launch on, unless already aboard). A crossing under way skips them.
  Ends done (data {:land {:x :y :z} :boat id :recovered bool}), info boat.landed (from boat-land), or the failed child's kind with its ending as the :because:
  condition-failed :no-boat (none carried and none could be fetched), :boat-not-seen (the placed boat is not in sight), the launch's other causes (:refused, :no-support,
  :unreachable, :occupied, :board-failed, :failed) and the landing's (:blocked, :drive-failed, :not-landed, :recover-failed, :no-shore); or nothing-possible :not-landed (on foot after the launch, not on the far shore).
  Done (:already before the start) when the body stands on the far shore with no recovery owed.
  Resumes safely: aboard it goes on to the landing; on foot after the launch it lets boat-land finish or recover, and ends done only when the body stands on the far shore
  (else :not-landed). Quote: time by the straight distance to the far shore.

  Never :continue except when a child waits on the world.")

(a/defargs args
  {:pos {:doc "the far-shore land cell to step onto, [x y z] or {:x :y :z}; nil: choose one (see :goal)" :spec ::a/pos :default nil}
   :goal {:doc "with no :pos: pick the seen far shore nearest this cell; nil: the farthest" :spec ::a/pos :default nil}
   :radius {:doc "how far from the body to look for the far shore" :spec (a/num-in 1 nil) :default 24}
   :min-cross {:doc "an auto-chosen far shore is at least this many blocks from the start" :spec (a/num-in 1 nil) :default 6}
   :item {:doc "the boat item to launch; nil: the first carried boat or raft, else one is fetched" :spec a/item? :default nil}
   :recover {:doc "take the boat back after landing" :spec boolean? :default true}})

(def crossing-kind :cross-water)
(def crossing-policy {:cap 4 :ttl :forever})

(defn given-pos [c] (when (:pos (:args c)) (:pos (b/parse (:args c)))))

(def target [:pos :goal])

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::boat-speed {:default 5 :doc "Blocks a second the boat covers, for the quote." :spec (a/num-in 0 nil)}
   ::launch-s {:default 24 :doc "Seconds the launch and the landing take besides the drive, for the quote." :spec (a/num-in 0 nil)}})

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

(defn failure
  "This call's ending for a child's ending r that is no :done: its kind (an impossible one is nothing-possible here) and
  its cause, renamed by `renames`; a limit keeps the child's cause; the child's data is kept."
  [c r renames]
  (let [kind (if (#{:condition-failed :limit-reached} (:kind r)) (:kind r) :nothing-possible)
        key (contract/cause-key r)]
    (ctx/end c kind (if (= :limit-reached kind) (:cause r) (get renames key key)) (:data r))))

(defn ^:async land! [c land]
  (let [{:keys [recover]} (:args c)
        r (await (ctx/call c :land 'jobs.movement.boat-land {:pos land :recover recover}))]
    (cond
      (= :continue r) :continue
      (= :done (:kind r))
      (do (ctx/forget-where! c crossing-kind #(= (:root c) (:job %)))
          (ctx/end c :done nil (select-keys (:data r) [:land :boat :recovered])))
      ;; on foot after the launch with the landing already done: done only if the body is on the far shore
      (and (crossing c) (= :not-aboard (contract/cause-key r)) (on-shore? c land))
      (do (ctx/forget-where! c crossing-kind #(= (:root c) (:job %)))
          (ctx/end c :done nil {:land land :recovered false}))
      (and (crossing c) (= :not-aboard (contract/cause-key r)))
      (do (ctx/forget-where! c crossing-kind #(= (:root c) (:job %)))
          (ctx/end c :nothing-possible :not-landed {:land land}))
      :else
      (do (ctx/forget-where! c crossing-kind #(= (:root c) (:job %)))
          (failure c r {})))))

(defn ^:async launch! [c spot]
  (let [{:keys [item]} (:args c)
        r (await (ctx/call c :launch 'jobs.movement.boat-launch {:item item}))]
    (cond
      (= :continue r) :continue
      (= :done (:kind r))
      (do (ctx/remember! c crossing-kind {:job (:root c) :land (:land spot)} crossing-policy)
          (await (land! c (:land spot))))
      :else (failure c (if (= :place-failed (contract/cause-key r)) (:because r) r) launch-stops))))

(defn spot-of
  "The far-shore spot the body would cross to now: {:land :water}, a given cell still not made out ({:land cell}), or nil."
  [c]
  (let [[spot reads] (sense/decide #(target-spot c))]
    (or spot (when (and (given-pos c) (or (seq (:cells reads)) (nil? (u/seen-name (:primitives c) (given-pos c)))))
               {:land (given-pos c)}))))

(def bad-args
  "Never :bad-args when :pos names no cell or :item is no boat."
  (checks/args-fit "needs a cell :pos and a boat :item"
                   (fn [c] (let [{:keys [pos item]} (:args c)]
                             (not (or (and pos (nil? (given-pos c)))
                                      (and item (not (launch/boat-name? item)))))))))

(defn start-fact
  "While no crossing is under way: later :no-far-shore with no spot to cross to, :not-water with no water in sight to
  launch on from the shore (a look first while the body has not looked round)."
  [c]
  (cond
    (crossing c) (contract/ok :under-way "a crossing under way")
    (nil? (spot-of c)) (contract/look :no-far-shore (if (given-pos c) "the given cell is no seen shore" "no far shore in sight"))
    (and (not (vehicle/mounted? (:primitives c))) (not (water-in-sight? c)))
    (contract/look :not-water "no water in sight to launch a boat on")
    :else (contract/ok :start "a shore to cross to")))

(def safe? (ready/safe-at-by-night (fn [c] (a/cell (:pos (:args c))))))
(def fit? ready/ok)

(def defaults {:spend {:job-time ::job-time-s}})

(def can-start? (cond/and bad-args start-fact))

(def can-run? start-fact)

(defn done?
  "Ok when the body stands on the far shore (the crossing's, else the given :pos) with no recovery owed: no crossing
  under way, or :recover false. With one under way the landing still wraps up (the boat is taken back)."
  [c]
  (let [saved (crossing c)
        land (or (:land saved) (given-pos c))
        ashore (and land (not (vehicle/mounted? (:primitives c))) (on-shore? c land))]
    (if (and ashore (or (nil? saved) (false? (:recover (:args c)))))
      (contract/ok :on-shore "stands on the far shore" :units :crossings :done 1 :left 0 :result {:land land :recovered false})
      (contract/later :crossing (if ashore "on the far shore, the landing wraps up" "crossing") :units :crossings :done 0 :left 1))))

(defn quote-of [c]
  (let [land (or (given-pos c) (:land (spot-of c)))
        d (if land (u/dist (u/self-pos c) (vehicle/centre land)) (:radius (:args c)))
        s (+ (settings/get settings ::launch-s) (/ d (settings/get settings ::boat-speed)))]
    {:uses {:job-time [s s (* 3 s)]}}))

(defn ^:async round [c]
  (let [p (:primitives c)
        mounted (vehicle/mounted? p)
        saved (crossing c)]
    (if saved
      (await (land! c (:land saved)))
      (let [spot (await (sense/decide! c :cross-water.shore #(target-spot c)))
            ;; a given far shore still not made out after the look: cross toward it, the landing drives nearer
            spot (or spot (spot-of c))]
        (cond
          (and (or (nil? spot) (not (water-in-sight? c))) (not (look/surveyed? c)))
          (await (look/survey! c)) ; a shore or water behind the body is not seen until it looks

          (nil? spot) :continue ; can-run? asks again
          mounted (await (land! c (:land spot)))
          (not (water-in-sight? c)) :continue
          :else (await (launch! c spot)))))))
