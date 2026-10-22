(ns jobs.lib.ready
  "The readiness checkers a job binds as its safe? and fit? exports: fns of the ctx giving a status (engine.contract).
  safe-at, safe-at-by-night and safe-if-lit are :later while the place is exposed and the stay there is not affordable (jobs.lib.price/priced; the estimate of jobs.lib.cost/exposure is
  above ::risk-limit; :look while the light there is unseen and the body has not looked round, then :later; :never when light is further off than the job waits; a dark hall no sky light reaches is exposed by its spawn chance, :never lowered by a torch; a small dark place is no exposure) or a danger is near
  (threatened); fit? is :later below the risky health or while poisoned (fit-armed also while unarmed); ok never refuses."
  (:require [jobs.lib.cost.health :as cost-health]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.body :as body]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.combat :as combat]
            [jobs.lib.cond :as lcond]
            [jobs.lib.checks :as checks]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger]
            [jobs.lib.price :as price]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.foods :as foods]
            [jobs.lib.look-edge :as look-edge]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::risk-limit {:default 0.001 :spec (a/num-in 0 nil)
                 :doc "Spawn chance per second (jobs.lib.cost/exposure) above which a place or the walk to it is exposed: a single dark cell is 0.005, a lit or sunlit one 0."}
   ::light-patience-s {:default 1200 :spec (a/num-in 0 nil)
                       :doc "Seconds readiness waits for light (jobs.lib.shelter.sense/light-in) before it calls the darkness unfixable; 1200 is a whole day, so a full night is waited for."}
   ::hidden-below-depth {:default 2 :spec (a/num-in 0 nil)
                         :doc "Blocks under the feet from which an unseen place with a seen roof over it counts as underground: no look asks for it and waiting for dawn does not light it."}
   ::unseen-judged-here-within {:default 6 :spec (a/num-in 0 nil)
                                :doc "Blocks: an unseen place this near the feet is judged where the body stands (no look shows a cell behind a wall)."}
   ::crowd-radius {:default 16 :spec (a/num-in 0 nil)
                   :doc "Blocks within which the real dangers the body knows of all count in the fight a long stay meets (outmatched): beyond the hostile-near radius they still come."}})

(defn unforced
  "The :checks forms a child would be forced with, minus the keys c's own frame runs under a form for: a call key beats
  the inherited form, so a forced one would replace the caller's."
  [c forms]
  (apply dissoc forms (keys (:checks (ctx/frozen-call c)))))

(defn place-text [{:keys [x y z]}] (str/join " " (remove nil? [x y z])))

(defn roof-over?
  "Whether a solid block the body has seen lies over the cell `to` in its column (within 32): terrain or a roof covers it."
  [p to]
  (boolean (some #(sh-sense/solid-at? p (update to :y + %)) (range 1 33))))

(defn hidden-below?
  "Whether `to` is an unseen cell at least ::hidden-below-depth under the feet with a seen solid block over it in its column (within 32): terrain hides it (a pit or cliff foot seen from its rim is seen), so it is underground (Q279). A valley behind a ridge has no seen roof and stays open surface."
  [p to]
  (and (:y to)
       (>= (- (:y (sh-sense/feet p)) (:y to)) (settings/get settings ::hidden-below-depth))
       (not (sh-sense/seen-cell? p to))
       (roof-over? p to)))

(defn sense-looked?
  "Whether a look round already looked at every one of cells: what it could not show (a dark cell beyond the body's sight) stays unseen."
  [c cells]
  (every? (sense/round-looked c) cells))

(defn hall-cell
  "The open cell the body would stand in at `to`: the feet when `to` is solid rock or unseen. A torch there ends a hall-at?."
  [p to]
  (let [to (update-vals to js/Math.floor)]
    (if (or (nil? (:y to)) (sh-sense/solid-at? p to) (not (sh-sense/seen-cell? p to))) (sh-sense/feet p) to)))

(defn hall-at?
  "Whether the dark place `to` is in a dark hall (sh-inside/dark-hall-at?), judged from its hall-cell."
  [p to]
  (sh-inside/dark-hall-at? p (hall-cell p to)))

(defn at-status
  "The status of the exposure of the walk of c's body to `to`: ok while sheltered or the chance is within ::risk-limit,
  else by what lowers it; darkness no sky light reaches (a cave: dawn never lifts it) is exposed only in a dark hall (hall-at?), :never, lowered by a torch, else no exposure; light further off than ::light-patience-s is not waited for."
  [c to]
  (let [p (:primitives c)
        shelter (:data (ctx/latest c :shelter))
        {:keys [chance lowered-by look-at light-in]} (cost/exposure p to shelter)
        patience (settings/get settings ::light-patience-s)
        where (place-text to)
        unseen-sides (delay (when (and (sh-sense/dark-outside? p) (not (u/within? (sh-sense/feet p) to 1))) (look-edge/pending c (sense/round-looked c) 0)))]
    (cond
      (and (sh-sense/dark-outside? p) (sh-inside/safe-from-night? p) (not (sh-inside/walk-leaves-shelter? p shelter to 1)))
      (contract/ok :sheltered (str "sheltered at " where))
      (<= chance (settings/get settings ::risk-limit)) (contract/ok :unexposed (str "no spawn risk at " where))
      (seq @unseen-sides)
      (contract/look :exposed (str "walls of the room round the body not all seen; at " where) :look-at @unseen-sides :room-sides true)
      (and (contains? lowered-by :look) (hidden-below? p to))
      (contract/ok :sealed (str "unseen below at " where ": underground, no look shows it and dawn does not light it; only threats count"))
      (and (contains? lowered-by :look) (sense-looked? c look-at) (sh-sense/buried? p) (not (u/within? (sh-sense/feet p) to 0)))
      (at-status c (sh-sense/feet p))
      (contains? lowered-by :look) (contract/look :exposed (str "light at " where " not seen") :look-at look-at)
      (and (contains? lowered-by :light) (<= light-in patience))
      (contract/later :exposed (str "dark at " where " until light") :lowered-by lowered-by :chance chance :recheck-after-ms (* 1000 light-in))
      (contains? lowered-by :light)
      (contract/never :exposed (str "dark at " where ", light is " (js/Math.round light-in) " s away, over the " (js/Math.round patience) " s it waits") :chance chance)
      (hall-at? p to) (contract/never :exposed (str "dark at " where ", a hall of dark floor beyond " sh-inside/spawn-free-radius " blocks: mobs spawn in it, waiting never lights it, a torch does") :lowered-by #{:torch} :chance chance :at (hall-cell p to))
      :else (contract/ok :sealed (str "dark at " where ", no dark floor reaches beyond " sh-inside/spawn-free-radius " blocks: nothing spawns near the body")))))

(defn hall-status
  "The exposure of `to` (nil: here) for a place no check waits for light at (underground): ok unless the body is buried and the place lies in a dark hall (hall-at?), then priced like any exposure."
  [c to]
  (let [p (:primitives c)
        to (update-vals (or to (sh-sense/feet p)) js/Math.floor)]
    (price/priced c (if (and (sh-sense/buried? p) (hall-at? p to))
                      (contract/never :exposed (str "dark at " (place-text to) ", a hall of dark floor beyond " sh-inside/spawn-free-radius " blocks: mobs spawn in it, a torch lowers it")
                                      :lowered-by #{:torch} :at (hall-cell p to) :chance (:chance (cost/exposure p to (:data (ctx/latest c :shelter)))))
                      (contract/ok :day "an unseen place's light is no reason to wait")))))

(defn exposed
  "The status of the exposure of c's body at `to` ({:x :y :z}, nil: where it stands), cached."
  [c to]
  (let [p (:primitives c)
        to (update-vals (or to (sh-sense/feet p)) js/Math.floor)]
    (price/priced c (check-cache/cached c [::exposed to (sense/round-looked c)] #(at-status c to)))))

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

(defn unarmed-threatened
  "Later :hostile-near while no weapon is carried and a hostile with a way to the body is known within jobs.lib.dawn's
  hostile radius (seen, heard or remembered; jobs.lib.dawn/::with-hostile lifts it): a player with bare hands does not
  set out with one about, but one behind rock cannot reach the bench where the weapon is made."
  [c]
  (if (and (not (combat/best-weapon (:primitives c) combat/default-weapons))
           (check-cache/cached c ::unarmed-threatened #(dawn/hostile-known? (:primitives c) {:way? true})))
    (contract/later :hostile-near "no weapon carried and a hostile is known near")
    (contract/ok :armed-or-clear "a weapon is carried or no hostile is known near")))

(defn outmatched
  "Later :outmatched while the fight with every real danger known within ::crowd-radius (jobs.lib.danger/dangers with a way to
  the body or a line of fire; heard mobs while muted and watched ones left out, as the hostile-near trigger does) would cost the
  kit carried more than price/allowed (jobs.lib.cost/fight-damage, the hostile reflex's formula): a player with a wooden sword
  does not dig on in a cave five mobs walk, one in iron does."
  [c]
  (let [p (:primitives c)
        view (ctx/view c)
        r (settings/get settings ::crowd-radius)
        near (check-cache/cached c ::outmatched
                                 #(danger/dangers p r {:ranged-radius r} {:ignore-heard? (body/heard-muted? view) :watched (body/watched view)}))
        hp (if (seq near)
             (cost/fight-damage {:weapon (combat/best-weapon p combat/default-weapons)
                                 :equipment (cost/equipment-of (.-equipment (.self p)))
                                 :mobs (mapv (fn [e] {:name (.-name e) :distance (danger/mob-distance p e) :hits 0}) near)})
             0)
        ok-hp (price/allowed c)]
    (if (<= hp ok-hp)
      (contract/ok :matched "no fight known near that the kit cannot afford")
      (contract/later :outmatched (str "a fight with the hostiles known near costs " (price/round1 hp) " hp, over the " (price/round1 ok-hp)
                                       " the body can afford: " (dawn/hostile-text (map #(dawn/describe p %) near)))
                      :hp hp :allowed ok-hp))))

(defn unless-outmatched
  "The checker safe of a long stay, :later too while outmatched; its ::exposure half is safe's."
  [safe]
  (with-meta (fn [c] (contract/all [(safe c) (outmatched c)])) (meta safe)))

(defn leads-away?
  "Whether the straight walk from the body to `goal` ({:x :y :z}) never takes it nearer to any hostile it knows within the
  hostile-near radii, and there is one: a player walks away from a danger."
  [c goal]
  (let [p (:primitives c)
        args (trigger-args c)
        body (u/pos-of (.-pos (.self p)))
        goal (update goal :y #(or % (:y body)))
        near (danger/known-hostiles p (danger/radius-in args) {:ranged-radius (danger/ranged-radius-in args)})]
    (and (seq near)
         (every? (fn [e] (>= (danger/way-distance body goal (danger/mob-pos p e)) (- (danger/mob-distance p e) 0.5)))
                 near))))

(defn threatened-walking
  "threatened, but ok while the walk to `goal` (nil: no walk) leads away from every hostile near (leads-away?)."
  [c goal]
  (let [t (threatened c)]
    (if (and goal (= :later (first t)) (leads-away? c goal))
      (contract/ok :walking-away "a hostile is near, the walk leads away from it")
      t)))

(defn safe-at
  "The checker of a job that goes to the place (f c) ({:x :y :z}, nil: here): not threatened and not exposed there.
  With walk? a hostile near is no threat to a walk that leads away from it (leads-away?).
  Its meta ::exposure is the exposed half alone, which (exposed) of a :checks form asks."
  ([f] (safe-at f false))
  ([f walk?]
   (with-meta (fn [c] (contract/all [(if walk? (threatened-walking c (f c)) (threatened c)) (exposed c (f c))]))
     {::exposure (fn [c] (exposed c (f c))) ::plain true})))

(defn near-unseen-here
  "f, but an unseen place within ::unseen-judged-here-within blocks of the feet is judged where the body stands: a look cannot show a cell
  behind a wall (the job walks round to it), so asking for one would wait for ever."
  [f]
  (fn [c]
    (let [p (:primitives c)
          at (f c)]
      (when-not (and at (:y at) (not (sh-sense/seen-cell? p (update-vals at js/Math.floor)))
                     (<= (u/dist (sh-sense/feet p) at) (settings/get settings ::unseen-judged-here-within)))
        at))))

(defn safe-at-by-night
  "The checker of a job that goes to the place (f c): while the place is dark safe-at it, by day or when body and place are both
  underground not threatened and not in a dark hall (hall-status; an unseen place's light is no reason to wait, and darkness underground does not end
  at dawn; a walk from a tunnel to open ground is exposed). A buried body's unseen place no higher than its feet with a seen solid block over it is
  underground (a place within ::unseen-judged-here-within of the feet that the walk would leave the body's night shelter for (walk-leaves-shelter?; a farm beside the wall) is never: the dark outside is waited out; Q345: a small dark cave counts only reachable hostiles, a hall its spawns); one above them, or with open sky over it (outside a hut), may be open ground. dug?: the job digs to the place, so no seen roof is needed (rock is what a player digs through). hall? false: a dark hall underground is no exposure (a job that leads out of it)."
  ([f] (safe-at-by-night f false))
  ([f walk?] (safe-at-by-night f walk? false))
  ([f walk? dug?] (safe-at-by-night f walk? dug? true))
  ([f walk? dug? hall?]
  (let [at (safe-at f walk?)
        by-night? (fn [c]
                    (let [p (:primitives c)
                          feet (sh-sense/feet p)
                          at (f c)
                          to (cond (nil? at) feet
                                   (:y at) (update-vals at js/Math.floor))]
                      (or (and to (<= (u/dist feet (update to :y #(or % (:y feet)))) (settings/get settings ::unseen-judged-here-within)) (sh-inside/walk-leaves-shelter? p (:data (ctx/latest c :shelter)) to 1))
                          (and (not (sh-sense/lit-here? p (if (and to (sh-sense/solid-at? p to)) (update to :y inc) (or to feet))))
                               (not (and to (sh-sense/buried? p)
                                         (or (sh-sense/buried-at? p to)
                                             (and (not (sh-sense/seen-cell? p to)) (<= (:y to) (:y feet)) (or dug? (roof-over? p to))))))))))]
    (with-meta (fn [c] (if (by-night? c) (at c) (contract/all (cond-> [(if walk? (threatened-walking c (f c)) (threatened c))] hall? (conj (hall-status c (f c)))))))
      {::exposure (fn [c] (if (by-night? c) ((::exposure (meta at)) c) (if hall? (hall-status c (f c)) (contract/ok :day "a way out of the dark is no exposure"))))}))))

(defn ok-when-near
  "safe, but ok while (near? c) holds: the job makes no walk there, so it meets no exposure the body is not already in (a wall closed beside a weak body at night is the safe act)."
  [near? safe]
  (with-meta (fn [c] (if (near? c) (contract/ok :in-reach "no walk to make, nothing to wait for") (safe c)))
    {::exposure (fn [c] (if (near? c) (contract/ok :in-reach "no walk to make") ((::exposure (meta safe)) c)))}))

(defn dark-near-ok
  "safe, but a wait for light (jobs.lib.cost/exposure lowered by :light, or light too far off) is lifted to the threat check alone while the place (f c) lies
  within the radius no mob spawns in round the body (cost/near-body?): a build or light job works there in the dark, which only a large dark area or a hidden pit makes a danger."
  [f safe]
  (let [dark-wait? (fn [[kind _ data]] (and (#{:later :never} kind) (= :exposed (:reason data))
                                           (or (= :never kind) (contains? (:lowered-by data) :light))))
        near? (fn [c] (let [at (f c)] (or (nil? at) (cost/near-body? (:primitives c) at))))
        lift (fn [g lifted] (fn [c] (let [s (g c)] (if (and (dark-wait? s) (near? c)) (lifted c) s))))]
    (with-meta (lift safe threatened)
      (update (meta safe) ::exposure #(some-> % (lift (fn [_] (contract/ok :dark-near "dark, but within the radius no mob spawns in"))))))))

(defn safe-at-build
  "safe-at-by-night for a job that builds at the place (f c): dark-near-ok, a long stay."
  [f]
  (price/staying :long (dark-near-ok f (safe-at-by-night f))))

(defn chest-place "The cell of the chest of c's job (the :chest arg or the known :chest place), nil: none." [c] (storage/chest-of (ctx/view c) (:args c)))

(def safe-at-chest
  "The checker of a job that goes to its chest (the :chest arg or the known :chest place)."
  (safe-at-by-night chest-place))

(def safe-here-by-night "safe-at-by-night where the job stands." (safe-at-by-night (constantly nil)))

(def safe-at-from
  "safe-at-by-night for a job that works where the body stood at submit (:submitted-at), here with none or with :here."
  (safe-at-by-night (fn [c] (let [args (:args c)] (when-not (:here args) (a/cell (:submitted-at args)))))))

(def safe-at-from-out
  "safe-at-from for a job that leads out of the dark (a stair up): a dark hall underground is no exposure, only threats count."
  (safe-at-by-night (fn [c] (let [args (:args c)] (when-not (:here args) (a/cell (:submitted-at args))))) false false false))

(defn day-would-light?
  "Whether the day (weather aside) brings the feet cell enough light: false in a dim shaft or pit whose sky light never reaches
  the spawn limit, where waiting for light waits for ever."
  [p]
  (let [sky (sh-sense/sky-state p)
        lvl (sh-sense/sky-light-at p (sh-sense/feet p))]
    (or (nil? sky) (nil? lvl) (some? (sh-sense/light-in-s lvl (:t sky) 0 0)))))

(defn open-to-sky?
  "Whether the feet cell is above ground: sky light reaches it straight down (a stair step lit round a corner is not), else, with no light data, not buried."
  [p]
  (let [feet (sh-sense/feet p)]
    (if (nil? (sh-sense/sky-light-at p feet)) (not (sh-sense/buried? p)) (sh-sense/sky-reaches-from-above? p feet))))

(defn dark-sky
  "Later :dark while the light at the feet is dark above ground (not sh-sense/lit-here?, open-to-sky?) and the day would lift it."
  [c]
  (let [p (:primitives c)]
    (if (and (not (sh-sense/lit-here? p (sh-sense/feet p))) (open-to-sky? p) (day-would-light? p))
      (contract/later :dark "dark sky above ground, waits for light")
      (contract/ok :lit "lit or underground"))))

(def safe-if-lit
  "The checker of a job that roams: not threatened, and no dark sky over it."
  (with-meta (fn [c] (contract/all [(threatened c) (dark-sky c)])) {::exposure dark-sky}))

(defn sheltered-dark
  "Later :dark while the body is shut in a hut (a wide closed room, not a dug tunnel) or in its own night shelter and the open sky is dark: a walk that has no
  place (a search, a roam) leaves the room into the dark."
  [c]
  (let [p (:primitives c)]
    (if (and (sh-sense/dark-outside? p) (not (sh-sense/sleeping? p)) (or (and (sh-sense/shut-in-hut? p) (sh-inside/safe-from-night? p)) (sh-inside/sheltered-in c)))
      (contract/later :dark "sheltered for the night, a walk with no place leaves the room under a dark sky"
                      :recheck-after-ms (* 1000 (sh-sense/sky-in p)))
      (contract/ok :lit "no room to leave under a dark sky"))))

(def safe-roaming
  "The checker of a job that roams and may leave a room: safe-if-lit, and not sheltered under a dark sky (sheltered-dark)."
  (with-meta (fn [c] (contract/all [(threatened c) (dark-sky c) (sheltered-dark c)]))
    {::exposure (fn [c] (contract/all [(dark-sky c) (sheltered-dark c)]))}))

(defn safe-at-or
  "The checker of a job that goes to the place (f c) when it has one, else (fallback c): safe-at-by-night there (dug?: the job digs to it)."
  ([f fallback] (safe-at-or f fallback false))
  ([f fallback dug?]
   (let [at (safe-at-by-night f false dug?)]
     (with-meta (fn [c] (if (f c) (at c) (fallback c)))
       {::exposure (fn [c] (if (f c) ((::exposure (meta at)) c) ((::exposure (meta fallback)) c)))}))))

(defn safe-at-or-if-lit
  "The checker of a job that goes to the place (f c) when it has one, else roams: safe-at-by-night, or safe-if-lit when (f c) is nil."
  [f]
  (safe-at-or f safe-if-lit))

(defn safe-at-or-roaming
  "safe-at-or-if-lit for a job that leaves the room on its way: with no place it is safe-roaming."
  ([f] (safe-at-or-roaming f false))
  ([f dug?] (safe-at-or f safe-roaming dug?)))

(defn fit? "Later :low-health below the risky health, :poisoned while poisoned." [c] (checks/low-health c))

(defn fit-min
  "fit? with the health floor a job's own setting gives, for jobs whose acts need less than risky-min-health."
  [floor c]
  (checks/low-health-min floor c))

(def cannot-heal? price/cannot-heal?)

(defn hurt-cannot-heal?
  "Health under foods/::low-health with only food able to heal it (cannot-heal?): work that risks the body should wait for food."
  [c]
  (and (< (.-health (.self (:primitives c))) (foods/low-health)) (cannot-heal? c)))

(defn fit-to-forage
  "The fit? of a job that looks for food: fit?, but a :low-health wait is lifted while the body can not heal (cannot-heal?), else it would never end."
  [c]
  (let [s (fit? c)]
    (if (and (= :low-health (:reason (nth s 2 nil))) (cannot-heal? c))
      (contract/ok :forage "hurt, but only food heals it")
      s)))

(defn starving?
  "Whether the body must get food whatever the dark: food 0, or health under the risky health with food under foods/top-up-food and nothing a meal would eat carried (cannot-heal?: only food heals it)."
  [c]
  (let [self (.self (:primitives c))]
    (boolean (or (zero? (.-food self))
                 (and (< (.-health self) (body/risky-min-health)) (cannot-heal? c))))))

(defn or-starving
  "The status of `safe` for c, or while starving? the status of threatened when `safe` is not ok (a starving body walks by night)."
  [safe c]
  (let [s (safe c)]
    (if (and (not= :ok (first s)) (starving? c)) (threatened c) s)))

(defn when-fed
  "The checker `safe` that lifts its waits (not a hostile near) while the body is starving?: the food it works at is how it heals."
  [safe]
  (with-meta (fn [c] (or-starving safe c))
    {::exposure (fn [c] (if (starving? c) (contract/ok :starving "starving: food first") ((::exposure (meta safe)) c)))}))

(defn forage-walk
  "go-to args for a walk that serves food-finding: with :floor cost-health/spend-down (all but 1 hp may be spent, go-to's survivable budget; also at under 1 hp) while the body can not heal, else args as they are."
  [c args]
  (cond-> args (cannot-heal? c) (assoc :floor cost-health/spend-down)))

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
   'job-safe-or-starving {:args [] :type :status :doc "the frame's own safe? holds, or while starving (food 0, or hurt with food unreachable) no hostile is near"
                          :fn (fn [] #(or-starving (own-export :safe?) %))}
   'healthy {:args [] :type :status :doc "health is enough and the body is not poisoned (fit?)" :fn (fn [] fit?)}
   'armed {:args [] :type :status :doc "a weapon is carried" :fn (fn [] armed)}
   'starving {:args [] :type :status :doc "food is 0" :fn #(lcond/= (lcond/food) 0)}
   'always {:args [] :type :status :doc "always holds" :fn (fn [] (fn [_] (contract/ok :always "always")))}})
