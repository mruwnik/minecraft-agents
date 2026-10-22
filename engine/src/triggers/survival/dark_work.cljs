(ns triggers.survival.dark-work
  "The dark-work trigger: a body with a job live stands on dark spawn ground (sh-sense/spawnable?, not lit-here?) under a
  roof, carries a torch, and is not shut in a room no mob can spawn near (sh-inside/safe-room-at?: a small dark room is
  harmless, a cave or a hall past the spawn-free radius is not). Runs jobs.build.light-area over the cells within :near of the body,
  so a dig in an unlit cave hangs a torch. Placing a torch near the body is never held."
  (:require [jobs.lib.body :as body]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.torch :as torch]))

(def defaults {})

(defn dark-work
  [p view _args _kn live]
  (boolean
   (and (seq live)
        (not (body/recovery-pending? view :dark-work))
        (not (false? (.-onGround (.self p))))
        (pos? (torch/torches-held p))
        (let [feet (sh-sense/feet p)]
          (and (sh-sense/room-roofed? p feet)
               (sh-sense/spawnable? p feet)
               (not (sh-sense/lit-here? p feet))
               (not (sh-inside/safe-room-at? p feet)))))))
