(ns jobs.lib.ready
  "The readiness checkers a job binds as its safe? and fit? exports: fns of the ctx giving a status (engine.contract).
  safe-at, safe-here and safe-if-lit are :later while the place is exposed (the estimate of jobs.lib.cost/exposure is
  above ::risk-limit; :look while the light there is unseen, :never when nothing lowers it) or a danger is near
  (threatened); fit? is :later below the risky health; ok never refuses."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.cond :as lcond]
            [jobs.lib.checks :as checks]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger]
            [jobs.lib.shelter :as sh]))

(a/defargs settings
  {::risk-limit {:default 0.001 :spec (a/num-in 0 nil)
                 :doc "Spawn chance per second (jobs.lib.cost/exposure) above which a place or the walk to it is exposed: a single dark cell is 0.005, a lit or sunlit one 0."}})

(defn place-text [{:keys [x y z]}] (str x " " y " " z))

(defn at-status
  "The status of the exposure of the walk of c's body to `to`: ok while sheltered or the chance is within ::risk-limit,
  else by what lowers it."
  [c to]
  (let [p (:primitives c)
        shelter (:data (ctx/latest c :shelter))
        {:keys [chance lowered-by look-at]} (cost/exposure p to shelter)
        where (place-text to)]
    (cond
      (and (sh/night? p) (sh/safe-from-night? p) (not (sh/walk-leaves-shelter? p shelter to 1)))
      (contract/ok :sheltered (str "sheltered at " where))
      (<= chance (settings/get settings ::risk-limit)) (contract/ok :unexposed (str "no spawn risk at " where))
      (contains? lowered-by :look) (contract/look :exposed (str "light at " where " not seen") :look-at look-at)
      (contains? lowered-by :dawn) (contract/later :exposed (str "dark at " where " until dawn") :lowered-by lowered-by :chance chance)
      :else (contract/never :exposed (str "exposed at " where ", nothing lowers it") :chance chance))))

(defn exposed
  "The status of the exposure of c's body at `to` ({:x :y :z}, nil: where it stands), cached."
  [c to]
  (let [p (:primitives c)
        to (update-vals (or to (sh/feet p)) js/Math.floor)]
    (check-cache/cached c [::exposed to] #(at-status c to))))

(defn threatened
  "Later :hostile-near while the hostile-near trigger would hold (the same radii and options)."
  [c]
  (if (check-cache/cached c ::threatened #(danger/hostile-near? (:primitives c) (ctx/view c) {}))
    (contract/later :hostile-near "a hostile is near")
    (contract/ok :no-hostile-near "no hostile near")))

(defn safe-at
  "The checker of a job that goes to the place (f c) ({:x :y :z}, nil: here): not threatened and not exposed there.
  Its meta ::exposure is the exposed half alone, which (exposed) of a :checks form asks."
  [f]
  (with-meta (fn [c] (contract/all [(threatened c) (exposed c (f c))]))
    {::exposure (fn [c] (exposed c (f c)))}))

(def safe-here "The checker of a job that stays put." (safe-at (constantly nil)))

(defn dark-sky
  "Later :dark at night above ground."
  [c]
  (let [p (:primitives c)]
    (if (and (sh/night? p) (not (sh/buried? p)))
      (contract/later :dark "night above ground, waits for dawn")
      (contract/ok :lit "lit or underground"))))

(def safe-if-lit
  "The checker of a job that roams: not threatened, and no dark sky over it."
  (with-meta (fn [c] (contract/all [(threatened c) (dark-sky c)])) {::exposure dark-sky}))

(def fit? "Later :low-health below the risky health." checks/low-health)

(defn ok "The checker of a job with nothing to check." [_] (contract/ok :ready "nothing to check"))

;; ------------------------------------------------------------------ the :checks vocabulary

(defn own-export
  "The checker of the frame's own export k (:safe? or :fit?, on the def the engine puts on the ctx), ok when it has none."
  [k]
  (fn [c]
    (if-let [f (get-in c [:def k])] (contract/asked c f) (contract/ok :ready (str "no " (name k))))))

(defn exposure-of [c]
  (if-let [f (::exposure (meta (get-in c [:def :safe?])))] (f c) (contract/ok :ready "no exposure check")))

(def vocabulary
  "The condition entries only a job's :checks forms use (the :ready/vocabulary hook); each holds when what its name says holds."
  {'job-safe {:args [] :type :status :doc "the frame's own safe? holds" :fn #(own-export :safe?)}
   'job-fit {:args [] :type :status :doc "the frame's own fit? holds" :fn #(own-export :fit?)}
   'exposed {:args [] :type :status :doc "the place of the frame's own safe? is exposed (dark)"
             :fn #(lcond/not exposure-of)}
   'threatened {:args [] :type :status :doc "a hostile is near (ready/threatened)" :fn #(lcond/not threatened)}
   'starving {:args [] :type :status :doc "food is 0" :fn #(lcond/= (lcond/food) 0)}
   'always {:args [] :type :status :doc "always holds" :fn (fn [] (fn [_] (contract/ok :always "always")))}})
