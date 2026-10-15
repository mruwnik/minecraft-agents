(ns jobs.lib.submitted
  "For a job whose meaning depends on where the body stands and that takes no position of its own (stair, tunnel, mine,
  bridge, pillar). The agent tool jobs.mjs submit adds :from (the body's position at submit) to its args; the job's
  check waits (away-wait, reason :moved-since-submit with :from :at :distance :limit) until its first round when the
  body then stands farther than ::limit blocks from it (a night walk home, a standing job's trip). :here true skips the
  check; a job with no :from (a child call, a spec written by hand) is not checked. A job declares the args :from and :here (specs a/position? and boolean?, both default nil/false) and its round calls begin! first."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::limit {:default 6 :doc "How far from where it was submitted a job with a :from still starts, in blocks." :spec (a/num-in 0 nil)}})
(defn limit [] (settings/get settings ::limit))

(defn begin!
  "Mark that the job's first round has started: from now on its position is its own business."
  [c]
  (ctx/update-mem! c assoc ::begun true))

(defn away-cause
  "nil, or the cause {:reason :moved-since-submit ..} when the job has not begun, has a :from and the body stands more than
  ::limit from it."
  [c]
  (let [{:keys [from here]} (:args c)
        from (some-> from a/cell)
        at (when from (u/self-pos c))
        distance (when from (u/dist at from))]
    (when (and from (not here) (not (::begun (ctx/mem c))) (> distance (limit)))
      {:reason :moved-since-submit :from from :at (into {} (map (fn [[k v]] [k (js/Math.floor v)])) (select-keys at [:x :y :z])) :distance (js/Math.round distance) :limit (limit)})))
