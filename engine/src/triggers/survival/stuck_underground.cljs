(ns triggers.survival.stuck-underground
  "The stuck-underground trigger: a body with no listed job, deep underground (its feet :below blocks or more under the
  remembered :home, else :bed, and buried: no sky light at its cell) walks home. mine, tunnel and the digs end where they
  stop and never walk back, so this is what gets an idle body out of a hole. It needs a remembered home or bed, and holds nothing while a job is listed, the body sleeps or a death is unrecovered."
  (:require [jobs.lib.body :as body]
            [jobs.lib.loose :as loose]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]))

(def defaults {:below 6})

(defn stuck-underground
  [p view args _kn _live]
  (let [{:keys [below]} (merge defaults args)
        pos (sh/home-target view)
        feet (u/self-pos {:primitives p})]
    (boolean
     (and pos
          (not (body/recovery-pending? view :stuck-underground))
          (not (sh-sense/sleeping? p))
          (empty? (loose/live-ids view nil))
          (>= (- (:y pos) (:y feet)) below)
          (sh-sense/buried? p)))))
