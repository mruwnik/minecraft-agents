(ns jobs.survival.recover-drops.danger
  "The dangers near a recover-drops trip and the declines they cause."
  (:require [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.flee-search :as fs]
            [jobs.lib.ready :as ready]
            [jobs.lib.rooms :as room-lib]
            [jobs.lib.util :as u]
            [jobs.survival.recover-drops.deaths :as dth]))

(defn decline!
  "End this run with the death still open, the ending carrying the reason and a text: condition-failed
  for a danger or the night, nothing-possible for a walk that did not arrive."
  [c reason text fields]
  (let [text (str "recover-drops declined: " text)]
    (ctx/end c (if (= :unreachable reason) :nothing-possible :condition-failed) reason (assoc fields :text text))))

(defn threatened?
  "The nearest real danger the hostile-near trigger sees (its radii and options: the same test as the (threatened) check)
  that the trip has not been weighed with near (:weighed), or nil."
  [c]
  (let [args (ready/trigger-args c)]
    (danger-q/nearest-danger (:primitives c) (danger-q/radius-in args) {:ranged-radius (:ranged-radius args :fire-range)}
                             {:skip (:weighed (ctx/mem c)) :sight? (:visible-only args true)
                              :ignore-heard? (body/heard-muted? (ctx/view c))})))

(defn striker
  "The attacker {:id :name} of the latest blow that hurt the body within jobs.lib.danger/::hit-s, or nil."
  [c]
  (let [view (ctx/view c)
        since (- (:now view) (* 1000 (settings/get danger-q/settings ::danger-q/hit-s)))]
    (some #(get-in % [:data :attacker]) (reverse (mem/since view :hurt since)))))

(defn flee?
  "Whether the trip runs from a blow: a mob struck the body within hit-s (striker) that this trip has not run from before
  (:fled: a flight from it ended, or a leg gained nothing on it; it came back or keeps up, so a flight does not shake it and the price alone decides)."
  [c]
  (let [s (striker c)]
    (boolean (and s (not (contains? (:fled (ctx/mem c)) (:id s)))))))

(defn flight-over!
  "Once no blow has landed for hit-s, the mobs fled this flight (:fleeing) are :fled."
  [c]
  (when-not (striker c)
    (ctx/update-mem! c #(-> % (update :fled (fnil into #{}) (:fleeing %)) (dissoc :fleeing)))))

(defn chasers
  "The real dangers to the body (the hostile radius, ranged ones their fire range, a mob that struck it within hit-s
  among them) as flee-search chasers {:name :mob :held :pos}."
  [c]
  (let [p (:primitives c)]
    (mapv (fn [e] {:name (cost/mob-kind e) :mob (.-name e) :held (.-held e) :pos (danger-q/mob-pos p e)})
          (danger-q/dangers p (danger-q/hostile-radius) {:ranged-radius :fire-range} {:sight? false :hit-ids (danger-q/hit-ids (ctx/view c))}))))

(defn ^:async flee-step!
  "One leg out of the reach of the mobs at hand after a blow: the cell the one priced flee search picks (jobs.lib.flee-search,
  its ring where they give the body up, within one step's search meter), walked as a jobs.movement.run-away leg of a
  flee step's seconds. Emits recover-drops.fled {:mob :gap}, gap the blocks to the nearest chaser after the leg (nil: none known). :next,
  with :recheck set (the pile is weighed again where the flight leaves the body), the striker :fleeing; a leg with no end
  to take or that gained nothing makes it :fled at once."
  [c]
  (let [{who :name id :id} (striker c)
        chs (chasers c)
        gap (fn [chs] (apply min ##Inf (map #(u/dist (u/self-pos c) (:pos %)) chs)))
        gap0 (gap chs)
        step (when (seq chs) (fs/pick (await (fs/search! c chs [] {:meter (room-lib/search-meter 0)})) {}))
        route (:route (meta step))
        _ (when step
            (await (ctx/call c :flee 'jobs.movement.run-away
                             (cond-> {:pos step :range 1 :leg-s (settings/get fs/settings ::fs/step-s)} route (assoc :route route)))))
        gap1 (gap (or (seq (chasers c)) chs))]
    (ctx/emit! c :recover-drops.fled :info {:mob who :gap (when (js/isFinite gap1) (/ (js/Math.round (* 10 gap1)) 10))
                                            :text (str "struck by " who ": " (if step "ran from it" "no way away from it"))})
    (ctx/update-mem! c #(-> (assoc % :recheck true) (update (if (> gap1 gap0) :fleeing :fled) (fnil conj #{}) id)))
    (if (ctx/alive? c) :next :continue)))

(defn hit-since?
  "Whether the body was hurt since the decision was taken."
  [c decided]
  (boolean (seq (ctx/since c :hurt (inc (:at decided 0))))))

(defn danger-fields
  "{:mob :mob-pos} of the danger m: its place when seen, else the rough spot its band gives (jobs.lib.danger/mob-pos)."
  [p m]
  {:mob (.-name m) :mob-pos (danger-q/mob-pos p m)})

(defn decline-danger!
  "End condition-failed :danger for the danger m: its name and place are in the event."
  [c m]
  (let [{:keys [mob mob-pos] :as fields} (danger-fields (:primitives c) m)]
    (ctx/update-mem! c assoc :hostile-cut true)
    (dth/save-trip! c)
    (decline! c :danger (str mob " at " (pr-str mob-pos)) fields)))

(defn meet-danger!
  "A danger m the trip has not been weighed with near: :next with the trip to be weighed again with it, once per mob;
  else (no decision yet, or a mob with no id) the decline."
  [c m]
  (if (and (:decided (ctx/mem c)) (some? (.-id m)))
    (do (ctx/update-mem! c #(-> % (assoc :recheck true) (update :weighed (fnil conj #{}) (.-id m)))) :next)
    (decline-danger! c m)))
