(ns jobs.survival.recover-drops.danger
  "The dangers near a recover-drops trip and the declines they cause."
  (:require [engine.ctx :as ctx]
            [jobs.lib.danger :as danger-q]
            [jobs.survival.recover-drops.config :as cfg]
            [jobs.survival.recover-drops.deaths :as dth]))

(defn decline!
  "End this run with the death still open, the ending carrying the reason and a text: condition-failed
  for a danger or the night, nothing-possible for a walk that did not arrive."
  [c reason text fields]
  (let [text (str "recover-drops declined: " text)]
    (ctx/end c (if (= :unreachable reason) :nothing-possible :condition-failed) reason (assoc fields :text text))))

(defn threatened?
  "The nearest real danger within :danger-radius the trip has not been weighed with near (:weighed), or nil."
  [c]
  (danger-q/nearest-danger (:primitives c) (cfg/danger-radius) {} {:skip (:weighed (ctx/mem c))}))

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
    (decline! c :danger (str mob " at " (pr-str mob-pos) " within " (cfg/danger-radius) " blocks") fields)))

(defn meet-danger!
  "A danger m the trip has not been weighed with near: :next with the trip to be weighed again with it, once per mob;
  else (no decision yet, or a mob with no id) the decline."
  [c m]
  (if (and (:decided (ctx/mem c)) (some? (.-id m)))
    (do (ctx/update-mem! c #(-> % (assoc :recheck true) (update :weighed (fnil conj #{}) (.-id m)))) :next)
    (decline-danger! c m)))
