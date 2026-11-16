(ns jobs.survival.recover-drops.danger
  "The dangers near a recover-drops trip and the declines they cause."
  (:require [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.ready :as ready]
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

(defn blow
  "The :hurt entry of the latest blow by a mob within jobs.lib.danger/::hit-s, or nil."
  [c]
  (let [view (ctx/view c)
        since (- (:now view) (* 1000 (settings/get danger-q/settings ::danger-q/hit-s)))]
    (last (filter #(get-in % [:data :attacker]) (mem/since view :hurt since)))))

(defn unanswered?
  "Whether a mob's blow (blow) landed since the trip last answered one (:answered)."
  [c]
  (boolean (some-> (blow c) :t (> (:answered (ctx/mem c) 0)))))

(defn gap
  "Blocks from the body to the nearest real danger in sight within the hostile radius (ranged ones their fire range), or nil."
  [c]
  (let [p (:primitives c)
        ds (map #(u/dist (u/self-pos c) (danger-q/mob-pos p %))
                (danger-q/dangers p (danger-q/hostile-radius) {:ranged-radius :fire-range} {:sight? true}))]
    (when (seq ds) (/ (js/Math.round (* 10 (apply min ds))) 10))))

(defn ^:async answer!
  "A mob's blow is a readiness failure of the trip: jobs.survival.respond-to-hostile (the priced fight, flight or cover)
  answers it as a child, then the pile is weighed again where that leaves the body (:recheck). Every blow up to its
  end is answered (:answered); a later one runs it again. Emits recover-drops.answered {:mob :gap :ending}, gap as gap
  gives it. :next, or :continue for a cut."
  [c]
  (let [who (get-in (blow c) [:data :attacker :name])
        e (await (ctx/call c :respond 'jobs.survival.respond-to-hostile {}))]
    (if (or (= :continue e) (not (ctx/alive? c)))
      :continue
      (let [g (gap c)]
        (ctx/update-mem! c assoc :answered (ctx/now c) :recheck true)
        (ctx/emit! c :recover-drops.answered :info {:mob who :gap g :ending (:kind e)
                                                     :text (str "struck by " who ": answered (" (name (:kind e)) "), "
                                                                (if g (str "the nearest danger " g " blocks off") "no danger in sight"))})
        :next))))

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
