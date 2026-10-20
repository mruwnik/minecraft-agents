(ns triggers.survival.stuck-underground
  "The stuck-underground trigger: a body with no listed job, deep underground (its feet :below blocks or more under the
  remembered :home, else :bed, and buried: no sky light at its cell) walks home. mine, tunnel and the digs end where they
  stop and never walk back, so this is what gets an idle body out of a hole. It holds only after the body has been in that state
  :idle-s seconds (a clock kept per body here, reset whenever the state ends), so the gap between two agent calls walks nowhere. It needs a remembered home or bed, and holds nothing while a job is listed, the body sleeps or a death is unrecovered."
  (:require [jobs.lib.body :as body]
            [jobs.lib.loose :as loose]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(def defaults {:below 6 :idle-s 120})

(def since
  "Per primitives object: the wall time its body first showed the stuck state, cleared when the state ends."
  (js/WeakMap.))

(defn stuck-underground
  [p view args _kn _live]
  (let [{:keys [below idle-s]} (merge defaults args)
        pos (sh/home-target view)
        feet (u/self-pos {:primitives p})
        stuck? (and pos
                    (not (body/recovery-pending? view :stuck-underground))
                    (not (sh-sense/sleeping? p))
                    (empty? (loose/live-ids view nil))
                    (>= (- (:y pos) (:y feet)) below)
                    (sh-sense/buried? p))]
    (when-not stuck? (.delete since p))
    (boolean
     (when stuck?
       (when-not (.has since p) (.set since p (:now view)))
       (>= (- (:now view) (.get since p)) (* 1000 idle-s))))))
