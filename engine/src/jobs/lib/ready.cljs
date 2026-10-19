(ns jobs.lib.ready
  "The readiness checkers a job binds as its safe? and fit? exports: fns of the ctx giving a status (engine.contract).
  safe-at, safe-at-by-night and safe-if-lit are :later while the place is exposed (the estimate of jobs.lib.cost/exposure is
  above ::risk-limit; :look while the light there is unseen and the body has not looked round, then :later; :never when light is further off than the job waits; darkness no sky light reaches is no exposure) or a danger is near
  (threatened); fit? is :later below the risky health or while poisoned (fit-armed also while unarmed); ok never refuses."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.combat :as combat]
            [jobs.lib.cond :as lcond]
            [jobs.lib.checks :as checks]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.storage :as storage]))

(a/defargs settings
  {::risk-limit {:default 0.001 :spec (a/num-in 0 nil)
                 :doc "Spawn chance per second (jobs.lib.cost/exposure) above which a place or the walk to it is exposed: a single dark cell is 0.005, a lit or sunlit one 0."}
   ::light-patience-s {:default 1200 :spec (a/num-in 0 nil)
                       :doc "Seconds readiness waits for light (jobs.lib.shelter.sense/light-in) before it calls the darkness unfixable; 1200 is a whole day, so a full night is waited for."}})

(defn unforced
  "The :checks forms a child would be forced with, minus the keys c's own frame runs under a form for: a call key beats
  the inherited form, so a forced one would replace the caller's."
  [c forms]
  (apply dissoc forms (keys (:checks (ctx/frozen-call c)))))

(defn place-text [{:keys [x y z]}] (str x " " y " " z))

(defn at-status
  "The status of the exposure of the walk of c's body to `to`: ok while sheltered or the chance is within ::risk-limit,
  else by what lowers it; darkness no sky light reaches (a cave: dawn never lifts it) is no exposure, only threats count; light further off than ::light-patience-s is not waited for."
  [c to]
  (let [p (:primitives c)
        shelter (:data (ctx/latest c :shelter))
        {:keys [chance lowered-by look-at light-in]} (cost/exposure p to shelter)
        patience (settings/get settings ::light-patience-s)
        where (place-text to)]
    (cond
      (and (sh-sense/dark-outside? p) (sh-inside/safe-from-night? p) (not (sh-inside/walk-leaves-shelter? p shelter to 1)))
      (contract/ok :sheltered (str "sheltered at " where))
      (<= chance (settings/get settings ::risk-limit)) (contract/ok :unexposed (str "no spawn risk at " where))
      (contains? lowered-by :look) (contract/look :exposed (str "light at " where " not seen") :look-at look-at)
      (and (contains? lowered-by :light) (<= light-in patience))
      (contract/later :exposed (str "dark at " where " until light") :lowered-by lowered-by :chance chance)
      (contains? lowered-by :light)
      (contract/never :exposed (str "dark at " where ", light is " (js/Math.round light-in) " s away, over the " (js/Math.round patience) " s it waits") :chance chance)
      :else (contract/ok :sealed (str "dark at " where ", no sky light reaches it: waiting never lights it, only threats count")))))

(defn exposed
  "The status of the exposure of c's body at `to` ({:x :y :z}, nil: where it stands), cached."
  [c to]
  (let [p (:primitives c)
        to (update-vals (or to (sh-sense/feet p)) js/Math.floor)]
    (check-cache/cached c [::exposed to] #(at-status c to))))

(defn trigger-args
  "The args of the register's hostile-near entry (muted or not), {} when it has none."
  [c]
  (or (some #(when (= :hostile-near (:trigger %)) (:args %)) (:register (some-> (:engine c) :state deref))) {}))

(defn threatened
  "Later :hostile-near while the hostile-near trigger would hold (the radii and options of its register entry)."
  [c]
  (if (check-cache/cached c ::threatened #(danger/hostile-near? (:primitives c) (ctx/view c) (trigger-args c)))
    (contract/later :hostile-near "a hostile is near")
    (contract/ok :no-hostile-near "no hostile near")))

(defn safe-at
  "The checker of a job that goes to the place (f c) ({:x :y :z}, nil: here): not threatened and not exposed there.
  Its meta ::exposure is the exposed half alone, which (exposed) of a :checks form asks."
  [f]
  (with-meta (fn [c] (contract/all [(threatened c) (exposed c (f c))]))
    {::exposure (fn [c] (exposed c (f c))) ::plain true}))

(defn safe-at-by-night
  "The checker of a job that goes to the place (f c): while the place is dark safe-at it, by day or when body and place are both
  underground only not threatened (an unseen place's light is no reason to wait, and darkness underground does not end
  at dawn; a walk from a tunnel to open ground is exposed). A buried body's unseen place no higher than its feet is
  underground (Q279: caves count only reachable hostiles); one above them may be open ground."
  [f]
  (let [at (safe-at f)
        by-night? (fn [c]
                    (let [p (:primitives c)
                          feet (sh-sense/feet p)
                          at (f c)
                          to (cond (nil? at) feet
                                   (:y at) (update-vals at js/Math.floor))]
                      (and (sh-sense/dark-at? p (if (and to (sh-sense/solid-at? p to)) (update to :y inc) (or to feet)))
                           (not (and to (sh-sense/buried? p)
                                     (or (sh-sense/buried-at? p to)
                                         (and (not (sh-sense/seen-cell? p to)) (<= (:y to) (:y feet)))))))))]
    (with-meta (fn [c] (if (by-night? c) (at c) (threatened c)))
      {::exposure (fn [c] (if (by-night? c) ((::exposure (meta at)) c) (contract/ok :day "an unseen place's light is no reason to wait")))})))

(def safe-at-chest
  "The checker of a job that goes to its chest (the :chest arg or the known :chest place)."
  (safe-at-by-night #(storage/chest-of (ctx/view %) (:args %))))

(def safe-here-by-night "safe-at-by-night where the job stands." (safe-at-by-night (constantly nil)))

(def safe-at-from
  "safe-at-by-night for a job that works where the body stood at submit (:submitted-at), here with none or with :here."
  (safe-at-by-night (fn [c] (let [args (:args c)] (when-not (:here args) (a/cell (:submitted-at args)))))))

(defn dark-sky
  "Later :dark while the light at the feet is dark above ground (sh-sense/dark-at?)."
  [c]
  (let [p (:primitives c)]
    (if (and (sh-sense/dark-at? p (sh-sense/feet p)) (not (sh-sense/buried? p)))
      (contract/later :dark "dark sky above ground, waits for light")
      (contract/ok :lit "lit or underground"))))

(def safe-if-lit
  "The checker of a job that roams: not threatened, and no dark sky over it."
  (with-meta (fn [c] (contract/all [(threatened c) (dark-sky c)])) {::exposure dark-sky}))

(defn safe-at-or-if-lit
  "The checker of a job that goes to the place (f c) when it has one, else roams: safe-at-by-night, or safe-if-lit when (f c) is nil."
  [f]
  (let [at (safe-at-by-night f)]
    (with-meta (fn [c] (if (f c) (at c) (safe-if-lit c)))
      {::exposure (fn [c] (if (f c) ((::exposure (meta at)) c) (dark-sky c)))})))

(defn or-starving
  "The status of `safe` for c, or at food 0 (a starving body walks by night) the status of threatened when `safe` is not ok."
  [safe c]
  (let [s (safe c)]
    (if (and (not= :ok (first s)) (zero? (.-food (.self (:primitives c))))) (threatened c) s)))

(def fit? "Later :low-health below the risky health, :poisoned while poisoned." checks/low-health)

(defn armed
  "Later :unarmed while the body carries no weapon (jobs.lib.combat/default-weapons)."
  [c]
  (if (combat/best-weapon (:primitives c) combat/default-weapons)
    (contract/ok :armed "a weapon is carried")
    (contract/later :unarmed "no weapon carried")))

(defn fit-armed
  "The fit? of a job that goes where mobs may be: fit? and armed."
  [c]
  (contract/all [(fit? c) (armed c)]))

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
   'job-safe-or-starving {:args [] :type :status :doc "the frame's own safe? holds, or at food 0 no hostile is near"
                          :fn (fn [] #(or-starving (own-export :safe?) %))}
   'healthy {:args [] :type :status :doc "health is enough and the body is not poisoned (fit?)" :fn (fn [] fit?)}
   'armed {:args [] :type :status :doc "a weapon is carried" :fn (fn [] armed)}
   'starving {:args [] :type :status :doc "food is 0" :fn #(lcond/= (lcond/food) 0)}
   'always {:args [] :type :status :doc "always holds" :fn (fn [] (fn [_] (contract/ok :always "always")))}})
