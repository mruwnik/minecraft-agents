(ns engine.jobs.world
  "What a job reads of its body's world (engine.world): plans, zones, claims, plan authors and footprints."
  (:require [engine.ctx :as ctx]
            [engine.world :as world]))

(defn plan
  "The body's world's plan id, from memory (engine.world/answer): nil when there is no such plan,
  {:id :broken text} when it was never readable, else {:id :plan :cells :errors}, with :error
  when the file is bad now and this is its last good copy."
  [ctx id]
  (world/plan (:world (:engine ctx)) id))

(defn zones
  "The body's world's zone list [{:name :min [x y z] :max [x y z] :owner :allow #{..}} ...] (see engine.zones).
  nil when the zone file is missing or was never readable: a dig or place job declines on nil, with one warn.
  \"No zones\" is [], never nil."
  [ctx]
  (world/zones (:world (:engine ctx))))

(defn claims
  "The active, unexpired area claims of the body's world (claims.edn) at the clock: [{:id :owner :min :max :until ..}].
  Zones and claims are a rule jobs consult (engine.access.zones); the engine enforces neither."
  [ctx]
  (world/live-claims (world/area-claims (:world (:engine ctx))) (ctx/now ctx)))

(defn plan-authors
  "{plan-id by}: the body each plan names as its maker (:metadata :by); a plan without it is not listed."
  [ctx]
  (world/plan-authors (:world (:engine ctx))))

(defn footprints
  "{[x y z] plan-id}: the cells of every plan, as engine.access.rules takes :footprints (a refusal then names
  the :plan). A job working plan P passes {:except P} to leave P's own cells out."
  ([ctx] (footprints ctx {}))
  ([ctx {:keys [except]}]
   (world/footprints (:world (:engine ctx)) except)))
