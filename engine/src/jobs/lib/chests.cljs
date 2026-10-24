(ns jobs.lib.chests
  "Working with storage chests: the pool a job tries, walking into reach, and looking in (look-in!), shared by the
  storage and item jobs (deposit, withdraw, haul, obtain, get-food)."
  (:require [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.access :as access]
            [jobs.lib.child :as child]
            [jobs.lib.places :as places]
            [jobs.lib.solid :as solid]
            [jobs.lib.stock :as stock]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]
            [jobs.lib.waits :as waits]))

(defn retracted?
  "retract-if-missing!, forget a missing chest as storage, and true when status is \"missing\" at the recorded chest place (so the job ends at once)."
  [c chest status]
  (let [recorded (mem/place (ctx/view c) :chest)]
    (when (= "missing" status) (storage/drop-from-group! c chest))
    (places/retract-if-missing! c :chest chest status)
    (and (= "missing" status) (some? recorded) (= chest recorded))))

(defn cell-pos [cell] (zipmap [:x :y :z] cell))

(defn own-plan-chests
  "The cells of chests in the plans this body made (plan cells that want a chest) that stand there now: a player uses
  every chest of their own base."
  [c]
  (let [cells (known/memo c [:own-chest-cells (ctx/self-name c)]
                          #(vec (for [id (access/own-plans c)
                                      {:keys [pos want]} (:cells (known/plan c id))
                                      :when (= "chest" (if (map? want) (:block want) want))]
                                  (vec pos))))]
    (filterv #(= "chest" (u/seen-name (:primitives c) (cell-pos %))) cells)))

(defn chests
  "{:pool cells :open cells :group? bool}: the chests to try. group? when the body's storage group is in play (no :chest
  argument, or one in the group): the :chest argument or place first, then the group nearest first. Else only the
  argument. open is the pool without the ones found full this call."
  ([c] (chests c :put))
  ([c action]
  (let [view (ctx/view c)
        args (:args c)
        main (some-> (storage/chest-of view args) storage/cell-of)
        members (vec (distinct (concat (storage/group view) (own-plan-chests c))))
        group? (or (nil? (:chest args)) (some? (some #{main} members)))
        here (u/self-pos c)
        near (sort-by (fn [[x y z]] (+ (js/Math.abs (- x (:x here))) (js/Math.abs (- y (:y here))) (js/Math.abs (- z (:z here))))) members)
        pool (vec (if group? (distinct (concat (when main [main]) near)) (when main [main])))
        full (:full (ctx/mem c) #{})
        refused? (fn [cell] (access/container-refusal c action (cell-pos cell)))]
    {:pool pool :open (vec (remove #(or (full %) (refused? %)) pool)) :group? group?})))

(defn pool-positions [action] (fn [c] (map cell-pos (:pool (chests c action)))))

(defn mark-full!
  "The chest took nothing more: skip it for the rest of the call. :again."
  [c chest]
  (ctx/update-mem! c update :full (fnil conj #{}) (storage/cell-of chest))
  :again)

(defn walk-args "The go-to args of the walk into reach of the chest at pos." [c pos]
  {:pos pos :range 3 :ways #{:walk :clear-seal} :ignore-zones? (boolean (:ignore-zones? (:args c)))})

(defn ^:async walk!
  "Within 3 of the chest: nil. Else one go-to call (walk-args): :continue while it waits on the world, :again once there,
  :unreachable when it gave up."
  [c chest]
  (if (u/within? (u/self-pos c) chest 3)
    nil
    (let [e (await (child/call-walk* c :walk (walk-args c chest)))]
      (cond
        (= :continue e) :continue
        (:arrived (:data e)) :again
        :else :unreachable))))

(defn ^:async look-in!
  "Walk into reach of the chest at pos (walk!) and look into it. :continue while a child waits, else
  {:status s :items js-array :retracted? bool}: \"ok\" books the stock (jobs.lib.stock); \"unreachable\" when go-to gave up.
  A chest that will not open under a seen solid block gets the block dug once (a jobs.blocks.dig child, which keeps to
  zones and claims) and is looked into again; a timeout is looked into once more. Any other status is returned;
  \"missing\" at the recorded :chest place retracts it (:retracted? true)."
  [c pos]
  (let [cell (stock/cell-of pos)
        above (update pos :y inc)
        w (await (walk! c pos))]
    (cond
      (= :continue w) :continue
      (= :unreachable w) {:status "unreachable"}
      (= :clearing (get-in (ctx/mem c) [:lid cell]))
      (let [e (waits/waiting-out c (await (ctx/call c :clear 'jobs.blocks.dig {:pos above :ignore-zones? (boolean (:ignore-zones? (:args c)))})))]
        (if (= :continue e)
          :continue
          (do (ctx/update-mem! c assoc-in [:lid cell] :cleared)
              (await (look-in! c pos)))))
      :else
      (let [seen (await (ctx/act c :inspectContainer (clj->js {:pos pos})))
            status (.-status seen)]
        (cond
          (= "ok" status)
          (do (stock/note-stock! c pos (.-items seen))
              {:status status :items (.-items seen)})
          (and (nil? (get-in (ctx/mem c) [:lid cell])) (solid/solid? (u/seen-name (:primitives c) above)))
          (do (ctx/update-mem! c assoc-in [:lid cell] :clearing)
              (await (look-in! c pos)))
          (and (= "timeout" status) (not (contains? (:timed-out (ctx/mem c)) cell)))
          (do (ctx/update-mem! c update :timed-out (fnil conj #{}) cell)
              (await (look-in! c pos)))
          :else {:status status :items (.-items seen) :retracted? (retracted? c pos status)})))))
