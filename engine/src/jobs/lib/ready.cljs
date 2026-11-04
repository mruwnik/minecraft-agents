(ns jobs.lib.ready
  "The readiness checkers a job binds as its safe? and fit? exports: fns of the ctx giving a status (engine.contract).
  safe-at, safe-at-by-night and safe-if-lit are :later while the place is exposed and the stay there is not affordable (priced; the estimate of jobs.lib.spawn-light/exposure is
  above ::risk-limit; :look while the light there is unseen and a look from here would show it, else priced as unseen (unseen-status); :never when light is further off than the job waits; a dark hall no sky light reaches is exposed by its spawn chance, :never lowered by a torch; a small dark place is no exposure) or a danger is near
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
            [jobs.lib.cost.risk :as risk]
            [jobs.lib.danger :as danger]
            [jobs.lib.home-food :as home-food]
            [jobs.lib.hostile-loss :as hostile-loss]
            [jobs.lib.price :as price]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.look :as look]
            [jobs.lib.look-edge :as look-edge]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spawn-light :as spawn-light]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]
            [jobs.lib.walk-risk :as walk-risk]))

(a/defargs settings
  {::risk-limit {:default 0.001 :spec (a/num-in 0 nil)
                 :doc "Spawn chance per second (jobs.lib.spawn-light/exposure) above which a place or the walk to it is exposed: a single dark cell is 0.005, a lit or sunlit one 0."}
   ::light-patience-s {:default 1200 :spec (a/num-in 0 nil)
                       :doc "Seconds readiness waits for light (jobs.lib.shelter.sense/light-in) before it calls the darkness unfixable; 1200 is a whole day, so a full night is waited for."}
   ::hidden-below-depth {:default 2 :spec (a/num-in 0 nil)
                         :doc "Blocks under the feet from which an unseen place with a seen roof over it counts as underground: no look asks for it and waiting for dawn does not light it."}
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
  "Whether a look round asked for every one of cells from this spot: what it could not show (a dark cell beyond the body's sight) stays unseen."
  [c cells]
  (every? (sense/round-looked c) cells))

(defn look-first
  "The exposure s with :look, the {:hp :lethal-hp :secs} of walking to `goal` to look and back at the chance `dark` the look
  shows it too dark to stay (walk-risk/look-way, Q66), while `goal` is unseen: priced weighs it as a way. The job's own
  walk there is the look; there the goal is seen, so the stay is priced."
  [s c goal dark]
  (cond-> s
    (not (sh-sense/seen-cell? (:primitives c) goal)) (assoc-in [2 :look] (walk-risk/look-way c goal dark))))

(defn segment-dist
  "Distance from cell q to the segment a-b ({:x :y :z} each)."
  [a b q]
  (let [d (fn [k] (- (k b) (k a)))
        len2 (+ (* (d :x) (d :x)) (* (d :y) (d :y)) (* (d :z) (d :z)))
        t (if (zero? len2) 0 (-> (/ (+ (* (- (:x q) (:x a)) (d :x)) (* (- (:y q) (:y a)) (d :y)) (* (- (:z q) (:z a)) (d :z))) len2) (max 0) (min 1)))]
    (u/dist q {:x (+ (:x a) (* t (d :x))) :y (+ (:y a) (* t (d :y))) :z (+ (:z a) (* t (d :z)))})))

(defn next-leg
  "{:side :to}: the next walk to look of the walk-look run that began at `from` ({:x :y :z}), or nil: look-edge/far-side over the
  region round `from`, minus the sides a walk-look looked at in vain and `tried` (the cells [x y z] this run's legs went to),
  from where the body stands, one leg bought from the call's worth; sides within sight of a cell in `:stands` (walked to), and with `:route-to` those farther than a look from the segment `:route-from` to it (when it is more than a block off: a hall judged where the body stands has no way to look along), are left out."
  ([c from] (next-leg c from #{} {}))
  ([c from tried] (next-leg c from tried {}))
  ([c from tried {:keys [stands route-from route-to]}]
  (let [p (:primitives c)
        walked (into (set tried) (mapcat (comp :cells :data)) (ctx/entries c :walk-looked))
        near (or (look/near-of p) 4)
        route-to (when route-to (if (vector? route-to) (zipmap [:x :y :z] route-to) route-to))
        walks? (and route-to (not (u/within? route-from route-to 1)))
        off-route? (fn [[x y z]] (and walks? (> (segment-dist route-from route-to {:x x :y y :z z}) near)))]
    (look-edge/far-side p (sh-sense/feet p) from (fn [side] (or (walked side) (off-route? side)))
                        (fn [to] (some->> (walk-risk/look-way c to 1) (walk-risk/look-cost c) (#(look-edge/affords-walk? c % (ctx/worth c)))))
                        (or stands #{})))))

(defn walk-look
  "The exposure s with :look, the one leg of the jobs.movement.walk-look child to the cell look-edge/far-side shows the far
  side of the region round `at` (the hall judged) from, and back, sure as the job works from here (walk-risk/look-way), while
  such a side is left that no walk-look has looked at yet (:walk-looked entries: from close by it stayed unseen, so another
  walk shows nothing); priced runs it as the check's :look-first child, and the check after it offers the next leg."
  [s c at]
  (if-let [{:keys [side to]} (next-leg c at #{} {:route-from (sh-sense/feet (:primitives c)) :route-to at})]
    (assoc-in s [2 :look] (assoc (walk-risk/look-way c to 1) :child ['jobs.movement.walk-look {:side side :to to :at at}]))
    s))

(defn own-way
  "c with (f c s) among the ways its job answers a refusal s on exposure itself (its torches, a wait for dawn): seconds,
  nil none. priced takes the walk to look only while it is cheaper than each of them. stay-f (c chance), when given, is the
  same answer to a dark stay at chance, for the price of a look before any refusal exists (stay-look-price)."
  ([c f] (update c ::own-ways (fnil conj []) f))
  ([c f stay-f] (update (own-way c f) ::own-stays (fnil conj []) stay-f)))

(defn stay-look-price
  "The {:gap :delta} of a look at the unseen cells a stay exposed at chance rests on, paid at P(closed): unlooked, an unseen
  wall is an open side, so the job takes the cheapest of the stay's exposure (price/stay-parts acted on now, cost/risk-s),
  its own answers to a stay at that chance (own-way's stay-f) and giving the goal up (ctx/worth); a room seen closed spares that,
  gap 0."
  [c chance]
  (let [{:keys [hurt dies death]} (price/stay-parts c chance (price/stay-s c))
        own (keep #(% (dissoc c ::own-ways ::own-stays) chance) (::own-stays c))]
    {:gap 0
     :delta (apply min (ctx/worth c) (cost/risk-s (.-health (.self (:primitives c))) hurt (cost/lethal-hp {:chance dies :death death})) own)}))

(defn room-look
  "A :look at the unseen walls of the room round the body (look-edge/pending) not looked at yet, and at `extra`: the ones
  sense/round-plan pays for at the stay's price at chance (stay-look-price), nil when none or no wall is left: an unseen
  wall is an open side."
  [c text extra chance]
  (when-let [sides (seq (look-edge/pending c (sense/round-looked c)))]
    (when-let [cells (seq (:pay (sense/round-plan c (into (vec sides) extra) (stay-look-price c chance))))]
      (contract/look :exposed text :look-at (vec cells) :room-sides true))))

(defn air-look
  "Under a seen roof, a :look at the unseen cells beside the seen air round the body (look/room-air-unseen) a look from here
  shows and no look round looked at yet and sense/round-plan pays for at the stay's price at chance (stay-look-price),
  nil when none is left: once none is unseen, that air's own dark is no evidence for the light outside. Open ground has
  no such room to seal."
  [c text chance]
  (let [p (:primitives c)]
    (when-let [cells (and (sh-sense/roofed? p (sh-sense/room-roof-height))
                          (seq (filter #(and (not ((sense/round-looked c) %)) (look/look-shows? p %)) (look/room-air-unseen p))))]
      (when-let [paid (seq (:pay (sense/round-plan c cells (stay-look-price c chance))))]
        (contract/look :exposed text :look-at (vec paid))))))

(defn unseen-status
  "The exposure of the unseen place `to` no look from here shows, priced as unseen at its chance (Q16): spawn-light/chance-at's
  guess of its light. Dark that dawn lifts (chance-at with caves-free?: dark only the sky lights) waits for dawn
  (::light-patience-s) while what is left once it lifts is within ::risk-limit; other dark looks first at the unseen walls of
  the room round the body (room-look) and at the unseen cells beside its air (air-look), else is :never lowered by a torch;
  priced prices either for the stay, the :never also as the walk there to look (look-first)."
  [c to]
  (let [p (:primitives c)
        chance (spawn-light/chance-at p to false)
        left (- chance (spawn-light/chance-at p to true))
        light-in (or (sh-sense/sky-in p) ##Inf)
        patience (settings/get settings ::light-patience-s)
        limit (settings/get settings ::risk-limit)
        where (str "unseen " (place-text to) ", which no look from here shows")]
    (cond
      (<= chance limit) (contract/ok :unexposed (str "no spawn risk at " where ": guessed lit"))
      (> left limit) (or (room-look c (str "dark at " where ", walls of the room round the body not all seen") nil chance)
                         (air-look c (str "dark at " where ", the air round the body not seen sealed") chance)
                         (-> (contract/never :exposed (str "dark at " where ": guessed " (js/Math.round (* 100 (spawn-light/dark-share p to false 0))) "% dark even by full day, so dawn does not light it") :lowered-by #{:torch} :chance chance)
                             (look-first c to (spawn-light/dark-share p to false))))
      (<= light-in patience) (contract/later :exposed (str "dark at " where ": guessed open ground, dark until light") :lowered-by #{:light} :chance chance
                                             :recheck-after-ms (* 1000 light-in) :wait-ms (* 1000 light-in))
      :else (contract/never :exposed (str "dark at " where ", light is " (js/Math.round light-in) " s away, over the " (js/Math.round patience) " s it waits") :chance chance))))

(defn dark-status
  "The exposure of the dark place `to` no wait lights: go-to's spawn chance (spawn-light/chance-at) of the cell the body would
  stand in there (hall-cell), dark only in a dark hall; over ::risk-limit :never, lowered by a torch. Where that cell is
  the body's own, the unseen walls of its room are looked at first (room-look). A hall judged by an unseen edge
  (sh-inside/dark-hall :roofed or :sky) is also priced as the walk to look at `goal` (default `to`; look-first), back
  at the chance the edge is open (cost/hall-chance); a seen `goal`, as the walk to look at the far side of the hall's region (walk-look)."
  ([c to] (dark-status c to to))
  ([c to goal]
   (let [p (:primitives c)
         at (spawn-light/hall-cell p to)
         chance (spawn-light/chance-at p at false)
         where (place-text to)
         hall (delay (sh-inside/dark-hall p at nil))
         look (delay (when (u/within? (sh-sense/feet p) at 1)
                       (or (room-look c (str "dark at " where ", walls of the room round the body not all seen") nil chance)
                           (air-look c (str "dark at " where ", the air round the body not seen sealed") chance))))]
     (cond
       (<= chance (settings/get settings ::risk-limit))
       (contract/ok :sealed (str "dark at " where ", no dark floor reaches beyond " sh-inside/spawn-free-radius " blocks: nothing spawns near the body"))
       @look @look
       :else
       (cond-> (contract/never :exposed (str "dark at " where ", and a mob can walk from dark floor farther than " sh-inside/spawn-free-radius " blocks off (a hall, or an open side of this room): mobs spawn there, waiting never lights it, a torch does")
                               :lowered-by #{:torch} :chance chance :at at)
         (#{:roofed :sky} @hall) (as-> s (if (sh-sense/seen-cell? p goal)
                                             (walk-look s c at)
                                             (look-first s c goal (cost/hall-chance @hall)))))))))

(defn at-status
  "The status of the exposure of the walk of c's body to `to`: ok while sheltered or the chance is within ::risk-limit,
  else by what lowers it. Waiting is for dawn only where it lifts the darkness, as go-to's walk-risk/wait-lowers? has it (the sky
  of a way out of the shelter, spawn-light/exposure's :light, or spawn-light/dawn-lowers?), and the light comes within ::light-patience-s; other darkness is dark-status's. Unseen light no look from here would show is unseen-status's.
  `goal` (default `to`) is where the job goes, when its place is judged at the feet."
  ([c to] (at-status c to to))
  ([c to goal]
   (let [p (:primitives c)
         shelter (:data (ctx/latest c :shelter))
         {:keys [chance lowered-by look-at light-in]} (spawn-light/exposure p to shelter)
         patience (settings/get settings ::light-patience-s)
         where (place-text to)
         room (delay (when-not (u/within? (sh-sense/feet p) to 1)
                       (room-look c (str "walls of the room round the body not all seen; at " where)
                                  (when (and (contains? lowered-by :look) (some #(look/look-shows? p %) look-at)) look-at)
                                  chance)))
         wait-s (delay (cond (contains? lowered-by :light) light-in (spawn-light/dawn-lowers? p to) (sh-sense/light-in p to)))]
     (cond
       (and (sh-sense/dark-outside? p) (sh-inside/safe-from-night? p) (not (sh-inside/walk-leaves-shelter? p shelter to 1)))
       (contract/ok :sheltered (str "sheltered at " where))
       (<= chance (settings/get settings ::risk-limit)) (contract/ok :unexposed (str "no spawn risk at " where))
       (and (sh-sense/sky-reaches-from-above? p (sh-sense/feet p)) (sh-sense/lit-here? p (sh-sense/feet p)) (sh-inside/near-shut-in? p shelter))
       (contract/ok :sheltered (str "in the hole cut out of its shelter, the sky's light reaching it: go-to climbs out, the walk is priced once out, at " where))
       @room @room
       (and (contains? lowered-by :look) (hidden-below? p to))
       (contract/ok :sealed (str "unseen below at " where ": underground, no look shows it and dawn does not light it; only threats count"))
       (and (contains? lowered-by :look) (sense-looked? c look-at) (sh-sense/buried? p) (not (u/within? (sh-sense/feet p) to 0)))
       (at-status c (sh-sense/feet p) goal)
       (and (contains? lowered-by :look) (not-any? #(look/look-shows? p %) look-at))
       (unseen-status c to)
       (and (contains? lowered-by :look) (sense-looked? c look-at))
       (dark-status c to goal)
       (contains? lowered-by :look) (contract/look :exposed (str "light at " where " not seen") :look-at look-at)
       (and @wait-s (<= @wait-s patience))
       (contract/later :exposed (str "dark at " where " until light") :lowered-by #{:light} :chance chance :recheck-after-ms (* 1000 @wait-s) :wait-ms (* 1000 @wait-s))
       @wait-s
       (contract/never :exposed (str "dark at " where ", light is " (js/Math.round @wait-s) " s away, over the " (js/Math.round patience) " s it waits") :chance chance)
       :else (dark-status c to goal)))))

(defn act-price
  "{:now-s :now? :best} of acting now, risking hurt hp and lethal (its death in hp), against waiting where the body stands
  first, by cost alone whatever the worth: until light (light-s, nil: no wait lights it), the act then risking nothing, and
  until healed to full while the body can heal, the same risk then at full health. A wait costs its seconds, staying put
  over them (walk-risk/stay-estimate) and the act after it (cost/risk-s). now? while the hp are within what the call's worth
  (ctx/worth) buys (walk-risk/buys-hp); best the cheapest wait {:wait :secs :s} whose hp and seconds the worth affords."
  [c hurt lethal light-s]
  (let [health (.-health (.self (:primitives c)))
        worth (ctx/worth c)
        hp (+ hurt lethal)
        per-hp (cost-health/heal-s-per-hp)
        stay-in (walk-risk/stay-inputs c (sh-sense/feet (:primitives c)) 0)
        heal-s (when (and (not (price/cannot-heal? c)) (pos? per-hp) (< health 20)) (* per-hp (- 20 health)))
        ways (cond-> []
               light-s (conj (walk-risk/wait-way c :light light-s stay-in health {:hp 0}))
               heal-s (conj (walk-risk/wait-way c :heal heal-s stay-in 20 {:hp hp :lethal-hp lethal})))]
    {:now-s (cost/risk-s health hurt lethal)
     :now? (<= hp (walk-risk/buys-hp health (.-food (.self (:primitives c))) worth))
     :best (walk-risk/cheapest-affordable c ways)}))

(defn priced
  "The status s of a place check, priced where it is :later or :never on exposure alone (:exposed with a :chance): the stay's
  hp there (price/stay-parts over price/stay-s) acted on now against waiting (act-price; s's :wait-ms is its wait for light).
  ok :priced while acting now is affordable and no affordable wait is cheaper; a cheaper wait, or the only affordable one,
  holds :later with both prices (a wait to heal keeps s's data); else s. An exposure resting on a place the body has not
  seen (:look, look-first) has a third way: walk there to look, its hp priced (walk-risk/risk-s) plus its seconds, ok
  :look-first while it is affordable and cheaper than the other ways and than the job's own answers to s (own-way); a way
  with a :child (walk-look) keeps the status those ways give, with the child as its :look-first call, run before the start.
  A :never resting on such a guess whose walk to look costs more hp than the worth buys holds :later: the refusal is on
  price, not impossible. An own answer cheaper than the way taken leaves s to the job."
  [c s]
  (let [[kind _ {:keys [reason chance wait-ms] look-way :look :as data}] s]
    (if-not (and (contains? #{:later :never} kind) (= :exposed reason) (number? chance))
      s
      (let [secs (price/stay-s c)
            {:keys [hurt dies death]} (price/stay-parts c chance secs)
            lethal (cost/lethal-hp {:chance dies :death death})
            hp (+ hurt lethal)
            {:keys [now-s now? best]} (act-price c hurt lethal (when (= :later kind) (some-> wait-ms (/ 1000))))
            look-s (some->> look-way (walk-risk/look-cost c) (#(when (look-edge/affords-walk? c % (ctx/worth c)) %)))
            own-s (keep #(% (dissoc c ::own-ways ::own-stays) s) (::own-ways c))
            own? (fn [than] (some #(< % than) own-s))
            look (when (and look-s (every? #(< look-s %) own-s)) {:s look-s})
            look? (fn [than] (and look (or (nil? than) (< (:s look) than))))
            wait (fn [{:keys [wait] :as w}]
                   (if (= :light wait)
                     (update s 2 assoc :now-s now-s :wait-s (:s w))
                     (contract/status :later :exposed (str "exposed: " (price/round1 secs) " s there cost " (price/round1 hp) " hp, " (price/round1 now-s)
                                                           " s now; healing " (price/round1 (:secs w)) " s first costs " (price/round1 (:s w)) " s")
                                      (assoc data :wait :heal :now-s now-s :wait-s (:s w) :recheck-after-ms walk-risk/recheck-ms))))
            [stay stay-s] (cond
                            (and best (< (:s best) now-s)) [(wait best) (:s best)]
                            now? [(contract/ok :priced (str "exposed, but " (price/round1 secs) " s there cost " (price/round1 hp) " hp, " (price/round1 now-s) " s, no wait cheaper")) now-s]
                            best [(wait best) (:s best)]
                            :else [s nil])
            stay (if (and stay-s (own? stay-s)) s stay)
            look-text (str "the walk to look costs " (price/round1 (:hp look-way)) " hp, " (some-> (:s look) price/round1) " s")]
        (cond
          (and look-way (nil? look-s) (= :never (first stay)))
          (contract/status :later :exposed (str (second stay) "; the walk to look costs " (price/round1 (:hp look-way)) " hp, more than the goal's "
                                                (price/round1 (ctx/worth c)) " s buys")
                           (assoc data :worth (ctx/worth c) :recheck-after-ms walk-risk/recheck-ms))
          (not (look? stay-s)) stay
          (:child look-way) (-> stay
                                (update 1 str "; " look-text ", so it walks to look first")
                                (assoc-in [2 :look-first] (:child look-way)))
          :else (contract/ok :look-first (str "exposed: " (price/round1 secs) " s there cost " (price/round1 hp) " hp, but " look-text
                                              ": the stay is judged once looked")))))))

(defn hall-status
  "The exposure of `to` (nil: here) for a place no check waits for light at (underground): ok unless the body is buried, then dark-status priced like any exposure."
  [c to]
  (let [p (:primitives c)
        to (update-vals (or to (sh-sense/feet p)) js/Math.floor)]
    (priced c (if (sh-sense/buried? p)
                      (dark-status c to)
                      (contract/ok :day "an unseen place's light is no reason to wait")))))

(defn exposed
  "The status of the exposure of c's body at `to` ({:x :y :z}, nil: where it stands), cached."
  [c to]
  (let [p (:primitives c)
        to (update-vals (or to (sh-sense/feet p)) js/Math.floor)]
    (priced c (check-cache/cached c [::exposed to (sense/round-looked c) (sense/known-memory p) (:t (ctx/latest c :walk-looked))] #(at-status c to)))))

(defn trigger-args
  "The args of the register's hostile-near entry (muted or not), {} when it has none."
  [c]
  (or (some #(when (= :hostile-near (:trigger %)) (:args %)) (:register (some-> (:engine c) :state deref))) {}))

(defn nearest-text
  "\": <the nearest known hostile in words>\" for the hostile-near radii of c's trigger entry, \"\" when none is known."
  [c]
  (let [p (:primitives c)
        args (trigger-args c)]
    (if-let [e (first (danger/known-hostiles p (danger/radius-in args) {:ranged-radius (danger/ranged-radius-in args)}))]
      (str ": " (dawn/hostile-text [(dawn/describe p e)]))
      "")))

(defn heard-dangers
  "The dangers of the hostile-near trigger (its register entry's args) the body has only heard, each placed where its
  sound may come from but the body does not see clearly (danger/open-cell-from with sight): one in clear sight would be
  seen, and one with no way to the body is no danger."
  [c]
  (let [p (:primitives c)]
    (remove danger/seen-only? (danger/hostile-dangers p (ctx/view c) (trigger-args c) {:sight-at (danger/sight-lookup p)}))))

(defn heard-price
  "ok while the heard-only dangers (heard-dangers) cost no more than the call is worth over its stay: what doing nothing
  about them for price/stay-s costs (hostile-loss/loss-over, the trigger's price, as though each came), against ctx/worth.
  Over it, look :heard-unseen at the guessed ones' spots when only a way through cells the body has not seen (an unseen
  wall is no shelter) makes the price too high, else later :hostile-near, priced again after walk-risk/recheck-ms. A
  sound is priced, not waited on."
  [c]
  (let [p (:primitives c)
        view (ctx/view c)
        hs (heard-dangers c)
        worth (ctx/worth c)
        loss #(if (seq %) (hostile-loss/loss-over p view % (price/stay-s c)) 0)
        all (loss hs)
        heard (when (seq hs) (dawn/hostile-text (map #(dawn/describe p %) hs)))
        seen-way (danger/seen-way-lookup p)
        opts {:sight? (:visible-only (trigger-args c) true) :sight-at (danger/sight-lookup p)}
        sure (filter #(danger/danger-in? p seen-way % opts) hs)
        guessed (remove (set sure) hs)]
    (cond
      (empty? hs) (contract/ok :no-hostile-near "no hostile near")
      (<= all worth) (contract/ok :heard-priced (str heard ": " (price/round1 all) " s over the stay, within the call's worth " (price/round1 worth) " s"))
      (and (seq guessed) (<= (loss sure) worth))
      (contract/look :heard-unseen (str heard ": its way to the body runs through cells not seen; look first")
                     :look-at (mapv #(mapv (fn [k] (js/Math.floor (k (danger/mob-pos p %)))) [:x :y :z]) guessed))
      :else (contract/later :hostile-near (str "a hostile is near: " heard ": " (price/round1 all) " s over the stay, more than the call's worth " (price/round1 worth) " s")
                            :loss all :worth worth :recheck-after-ms walk-risk/recheck-ms))))

(defn threatened
  "Later :hostile-near while the hostile-near trigger would hold (the radii and options of its register entry) for a
  mob seen or remembered; heard-only mobs are priced (heard-price)."
  [c]
  (check-cache/cached c ::threatened
                      #(if (hostile-loss/hostile-near? (:primitives c) (ctx/view c) (assoc (trigger-args c) :ignore-heard? true))
                         (contract/later :hostile-near (str "a hostile is near" (nearest-text c) ": deal with it first"))
                         (heard-price c))))

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
  the body or a line of fire; heard mobs while muted and watched ones left out, as the hostile-near trigger does) is not the
  cheapest affordable way: its hp with the kit carried (jobs.lib.cost/fight-damage, the hostile reflex's formula) priced now
  against healing first (act-price): a player with a wooden sword does not dig on in a cave five mobs walk, one in iron does."
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
        {:keys [now-s now? best]} (if (pos? hp) (act-price c hp 0 nil) {:now? true})]
    (if (and now? (not (and best (< (:s best) now-s))))
      (contract/ok :matched "no fight known near that the kit cannot afford")
      (contract/later :outmatched (str "a fight with the hostiles known near costs " (price/round1 hp) " hp"
                                       (if now? (str ", " (price/round1 now-s) " s now; healing first costs " (price/round1 (:s best)) " s") ", over what the body can afford")
                                       ": " (dawn/hostile-text (map #(dawn/describe p %) near)))
                      :hp hp :now-s now-s :wait-s (:s best)))))

(defn unless-outmatched
  "The checker safe of a long stay, :later too while outmatched; its ::exposure half is safe's."
  [safe]
  (with-meta (fn [c] (contract/all [(safe c) (outmatched c)])) (meta safe)))

(defn melee-catches?
  "Whether known melee hostile e hits the body on its walk to goal before the walk is past it, the mob where the body takes
  it to be: on route ([{:x :y :z :t}], go-to's plan timed, cost/caught-on?) when given, else on the straight walk at the
  body's pace by its food (cost/catches?). False for a ranged one: it shoots, it does not chase."
  ([p e goal] (melee-catches? p e goal nil))
  ([p e goal route]
   (and (not (combat/ranged? e))
        (if route
          (cost/caught-on? {:chaser (cost/mob-kind e) :pos (danger/mob-pos p e) :route (mapv cost/centred route)})
          (cost/catches? {:chaser (cost/mob-kind e) :pos (danger/mob-pos p e) :from (u/pos-of (.-pos (.self p))) :to goal
                          :food (or (.-food (.self p)) 20) :step 1})))))

(defn way-nearest
  "Blocks from q to the nearest point of the walk from body to goal: body and the cells after it on route (their centres) when given, else the straight segment."
  [body goal route q]
  (if route
    (reduce min (u/dist q body) (map #(u/dist q (cost/centred %)) (rest route)))
    (danger/way-distance body goal q)))

(defn leads-away?
  "Whether the walk from the body to `goal` ({:x :y :z}), on route (go-to's plan timed, [{:x :y :z :t}]) when given, else
  the straight line, never takes it nearer to any hostile it knows within the hostile-near radii, and there is one, and
  none catches the walk (melee-catches?) or has hurt the body lately: a player walks away from a danger, and fights one at
  their heels."
  ([c goal] (leads-away? c goal nil))
  ([c goal route]
   (let [p (:primitives c)
         args (trigger-args c)
         hit (danger/hit-ids (ctx/view c))
         body (u/pos-of (.-pos (.self p)))
         goal (update goal :y #(or % (:y body)))
         near (danger/known-hostiles p (danger/radius-in args) {:ranged-radius (danger/ranged-radius-in args)})]
     (and (seq near)
          (every? (fn [e] (and (>= (way-nearest body goal route (danger/mob-pos p e)) (- (danger/mob-distance p e) 0.5))
                               (not (melee-catches? p e goal route))
                               (not (contains? hit (.-id e)))))
                  near)))))

(defn threatened-walking
  "threatened, but ok while the walk to `goal` (nil: no walk) leads away from every hostile near (leads-away?, on route
  when given)."
  ([c goal] (threatened-walking c goal nil))
  ([c goal route]
   (let [t (threatened c)]
     (if (and goal (= :later (first t)) (leads-away? c goal route))
       (contract/ok :walking-away "a hostile is near, the walk leads away from it")
       t))))

(defn safe-at
  "The checker of a job that goes to the place (f c) ({:x :y :z}, nil: here): not threatened and not exposed there.
  With walk? a hostile near is no threat to a walk that leads away from it (leads-away?).
  Its meta ::exposure is the exposed half alone, which (exposed) of a :checks form asks."
  ([f] (safe-at f false))
  ([f walk?]
   (with-meta (fn [c] (contract/all [(if walk? (threatened-walking c (f c)) (threatened c)) (exposed c (f c))]))
     {::exposure (fn [c] (exposed c (f c))) ::plain true})))

(defn leaving-sealed-shelter
  "Later :dark while a walk to `to` ({:x :y :z}, floored) leaves the body's own sealed night shelter (sh-inside/walk-leaves-shelter?
  :dug-in, `to` beyond its room) under a dark sky, else nil."
  [c to]
  (let [p (:primitives c)]
    (when (and (= :dug-in (sh-inside/walk-leaves-shelter? p (:data (ctx/latest c :shelter)) to 1)) (not (sh-inside/in-room? p to 1)))
      (contract/later :dark "sealed in the night shelter, the walk would leave it under a dark sky"
                      :recheck-after-ms (* 1000 (sh-sense/sky-in p)) :wait-ms (* 1000 (sh-sense/sky-in p))))))

(defn safe-at-by-night
  "The checker of a job that goes to the place (f c): while a mob may spawn at the place (spawn-light/exposure's chance) safe-at it, by day or when body and place are both
  underground not threatened and not in a dark hall (hall-status; an unseen place's light is no reason to wait, and darkness underground does not end
  at dawn; a walk from a tunnel to open ground is exposed). A buried body's unseen place no higher than its feet with a seen solid block over it is
  underground (Q345: a small dark cave counts only reachable hostiles, a hall its spawns); one above them, or with open sky over it (outside a hut), may be open ground. dug?: the job digs to the place, so no seen roof is needed (rock is what a player digs through). hall? false: a dark hall underground is no exposure (a job that leads out of it). A walk that leaves the body's own sealed night shelter (:dug-in) waits :dark till the sky lights, whatever the place: the night job keeps the seal till day."
  ([f] (safe-at-by-night f false))
  ([f walk?] (safe-at-by-night f walk? false))
  ([f walk? dug?] (safe-at-by-night f walk? dug? true))
  ([f walk? dug? hall?]
  (let [at (safe-at f walk?)
        to-of (fn [c] (let [at (f c)] (cond (nil? at) (sh-sense/feet (:primitives c)) (:y at) (update-vals at js/Math.floor))))
        sealed (fn [c] (some->> (to-of c) (leaving-sealed-shelter c)))
        by-night? (fn [c]
                    (let [p (:primitives c)
                          feet (sh-sense/feet p)
                          to (to-of c)]
                      (and (pos? (:chance (spawn-light/exposure p (or (f c) feet))))
                           (not (and to
                                     (or (and (sh-sense/buried? p) (sh-sense/buried-at? p to))
                                         (and (not (sh-sense/seen-cell? p to)) (<= (:y to) (:y feet))
                                              (if dug? true (and (sh-sense/buried? p) (roof-over? p to))))))))))
        threat (fn [c] (if walk? (threatened-walking c (f c)) (threatened c)))]
    (with-meta (fn [c] (cond
                         (sealed c) (contract/all [(threat c) (sealed c)])
                         (by-night? c) (at c)
                         :else (contract/all (cond-> [(threat c)] hall? (conj (hall-status c (f c)))))))
      {::exposure (fn [c] (cond
                            (sealed c) (sealed c)
                            (by-night? c) ((::exposure (meta at)) c)
                            hall? (hall-status c (f c))
                            :else (contract/ok :day "a way out of the dark is no exposure")))}))))

(defn ok-when-near
  "safe, but ok while (near? c) holds: the job makes no walk there, so it meets no exposure the body is not already in (a wall closed beside a weak body at night is the safe act)."
  [near? safe]
  (with-meta (fn [c] (if (near? c) (contract/ok :in-reach "no walk to make, nothing to wait for") (safe c)))
    {::exposure (fn [c] (if (near? c) (contract/ok :in-reach "no walk to make") ((::exposure (meta safe)) c)))}))

(defn dark-near-ok
  "safe, but a wait for light (jobs.lib.spawn-light/exposure lowered by :light, or light too far off) is lifted to the threat check alone while the place (f c) lies
  within the radius no mob spawns in round the body (cost/near-body?): a build or light job works there in the dark, which only a large dark area or a hidden pit makes a danger.
  With torch-way (c -> {:blocks :carried}, nil: the job hangs no torches) the lift is the job's own torches instead, while they cost less than the stay's exposure (cost/dark-or-lit).
  Either is an own-way: a walk to look that costs more is not taken. Without torch-way a priced :never (a spawn chance whose stay costs more than the worth) is kept, not lifted: the stay is too dear whatever the radius."
  ([f safe] (dark-near-ok nil f safe))
  ([torch-way f safe]
  (let [dark-wait? (fn [[kind _ data]] (and (#{:later :never} kind) (= :exposed (:reason data))
                                           (or (= :never kind) (some #{:light :torch} (:lowered-by data)))))
        near? (fn [c] (let [at (f c)] (or (nil? at) (cost/near-body? (:primitives c) at))))
        priced-never? (fn [[kind _ {:keys [chance]}]] (and (= :never kind) (number? chance)))
        chance-of (fn [[_ _ data]] (:chance data))
        way (fn [c chance]
              (when torch-way
                (cost/dark-or-lit (if (number? chance) (price/stay-hp c chance (price/stay-s c)) ##Inf)
                                  (.-health (.self (:primitives c))) (torch-way c))))
        own-stay (fn [c chance] (when (near? c) (let [w (way c chance)] (cond (nil? w) 0 (= :lit (:way w)) (:lit-s w)))))
        own-s (fn [c s] (when (and (dark-wait? s) (not (and (nil? torch-way) (priced-never? s))))
                          (own-stay c (chance-of s))))
        lift (fn [g lifted]
               (fn [c] (let [s (g (own-way c own-s own-stay))]
                         (if-not (and (dark-wait? s) (near? c))
                           s
                           (let [w (way c (chance-of s))]
                             (cond (and (nil? w) (priced-never? s)) s
                                   (nil? w) (lifted c (contract/ok :dark-near "dark, but within the radius no mob spawns in"))
                                   (= :lit (:way w)) (lifted c (contract/ok :lit-way (str "dark, but within the radius no mob spawns in, and its torches cost "
                                                                                          (price/round1 (:lit-s w)) " s, under the dark's " (price/round1 (:dark-s w)))))
                                   :else s))))))]
    (with-meta (lift safe (fn [c _] (threatened c)))
      (update (meta safe) ::exposure #(some-> % (lift (fn [_ ok] ok))))))))

(defn safe-at-build
  "safe-at-by-night for a job that builds at the place (f c): dark-near-ok, the stay (work-s c) seconds of the job's own work."
  [f work-s]
  (price/with-stay (dark-near-ok f (safe-at-by-night f)) work-s true))

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

(defn unseen-overhead
  "The first cell [x y z] over the feet that the body has not sensed and no look round has asked for from here, nil when none: the roof it never looked at."
  [c]
  (let [p (:primitives c)
        {:keys [x y z]} (sh-sense/feet p)
        looked (sense/round-looked c)]
    (first (for [dy (range 1 33) :let [cell [x (+ y dy) z]] :when (and (sense/unknown? p cell) (not (looked cell)))] cell))))

(defn dark-sky
  "Later :dark while the light at the feet is dark above ground (not sh-sense/lit-here?, open-to-sky?) and the day would lift it; an unseen cell over the feet is looked at first (:look), since a seen roof makes the cell underground."
  [c]
  (let [p (:primitives c)]
    (if (and (not (sh-sense/lit-here? p (sh-sense/feet p))) (open-to-sky? p) (day-would-light? p))
      (if-let [cell (unseen-overhead c)]
        (contract/look :dark "dark sky above ground unless a roof is over the feet; looks up" :look-at [cell])
        (apply contract/later :dark "dark sky above ground, waits for light" (when-let [s (sh-sense/sky-in p)] [:wait-ms (* 1000 s)])))
      (contract/ok :lit "lit or underground"))))

(def safe-if-lit
  "The checker of a job that roams: not threatened, and no dark sky over it."
  (with-meta (fn [c] (contract/all [(threatened c) (dark-sky c)])) {::exposure dark-sky}))

(defn sheltered-dark
  "Later :dark while the body is shut in a hut (a wide closed room, not a dug tunnel) or in its own night shelter and a way
  out of its room (sh-inside/way-out-cells) is dark till dawn lights it (sh-sense/light-in): a walk that has no place (a
  search, a roam) leaves the room that way. A room in rock has no way out the day lights, so it holds nothing."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        room (when (and (not (sh-sense/sleeping? p)) (or (and (sh-sense/shut-in-hut? p) (sh-inside/safe-from-night? p)) (sh-inside/sheltered-in c)))
               (or (reach-room/room p feet) {:cells #{[(:x feet) (:y feet) (:z feet)]}}))
        waits (keep #(let [s (sh-sense/light-in p %)] (when (and s (pos? s)) s)) (some->> room (sh-inside/way-out-cells p)))]
    (if (seq waits)
      (contract/later :dark "sheltered for the night, a walk with no place leaves the room into ground dark till dawn"
                      :recheck-after-ms (* 1000 (apply min waits)) :wait-ms (* 1000 (apply min waits)))
      (contract/ok :lit "no way out of the room that dawn lights"))))

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

(defn safe-at-or-roaming
  "The checker of a job that goes to the place (f c) when it has one, else leaves the room on its way: safe-at-by-night there, else safe-roaming."
  ([f] (safe-at-or-roaming f false))
  ([f dug?] (safe-at-or f safe-roaming dug?)))

(defn quoted-s
  "Seconds `seconds-of` (a fn of c, a job's work-s) answers when positive, else nil: the stay a fit? prices before the category's."
  [seconds-of c]
  (let [s (when seconds-of (seconds-of c))]
    (when (and (number? s) (pos? s)) s)))

(defn fit-for
  "The fit? of a job whose work is of category (:click, :work or :long, cost.risk/work-s): later :game-mode off survival,
  :low-health while that work costs more than the body may risk now (price/fit: none while waiting cannot heal it),
  :poisoned while poisoned. seconds-of (a fn of c, optional): the job's own quoted seconds, used while positive instead of the category's."
  ([category] (fit-for category nil))
  ([category seconds-of]
   (fn [c]
     (let [p (:primitives c)]
       (if-let [hurt (or (body/off-mode p) (price/fit c category (quoted-s seconds-of c)) (body/poison-wait p))]
         (checks/cause-status :later hurt)
         (contract/ok :healthy "health enough"))))))

(def fit? "fit-for a job that works a place for a while (:work)." (fit-for :work))

(def cannot-heal? price/cannot-heal?)

(defn hurt-cannot-heal?
  "Too hurt for a job's work of category (cost.risk/too-hurt?, at `seconds` when given) with only food able to heal it (cannot-heal?): work that risks the body should wait for food."
  ([c category] (hurt-cannot-heal? c category nil))
  ([c category seconds]
   (let [self (.self (:primitives c))]
     (and (risk/too-hurt? (.-health self) (.-food self) category (cost/death-s (:primitives c) (ctx/view c)) seconds) (cannot-heal? c)))))

(defn fed-fit-for
  "fit-for category, and later :low-health while hurt-cannot-heal? for that category: a hurt body that only food heals gets food before the work."
  ([category] (fed-fit-for category nil))
  ([category seconds-of]
   (let [fit (fit-for category seconds-of)]
     (fn [c]
       (if (hurt-cannot-heal? c category (quoted-s seconds-of c))
         (contract/later :low-health "hurt, and only food heals the body: food first")
         (fit c))))))

(def fed-fit? "fed-fit-for a job that works a place for a while (:work)." (fed-fit-for :work))

(defn own-trip
  "The trip of the job c is a check of: home-food/quote-trip of its quote (quote-fn, a fn of c that answers at once),
  ending at place (a fn of c, or nil); nil when it gives no quote."
  [c place quote-fn]
  (let [q (quote-fn c)]
    (when (map? q) (home-food/quote-trip c q place))))

(defn trip-fed
  "fit, and while that is ok, ok with a jobs.survival.fetch-trip-food child as its :look-first call while the
  trip's food (home-food/trip-fetch of own-trip, ending at place, by quote-fn) pays a fetch from home: the engine runs
  it only before the start, so a trip leaves home with its food. When going on with the carried food costs less than
  the call is worth (ctx/worth; ##Inf with no engine behind c), the fetch (jobs.survival.fetch-trip-food) is told to go on
  with the carried food when get-food falls short; otherwise get-food's ending stops the call."
  [fit place quote-fn]
  (fn [c]
    (let [s (fit c)
          {:keys [to go-on-s]} (when (= :ok (first s))
                                     (some->> (own-trip c place quote-fn) (home-food/trip-fetch c)))]
      (if to
        (contract/ok :trip-food (str "fetching food for the trip first: " to " points")
                     :look-first ['jobs.survival.fetch-trip-food {:carry to :go-on? (< go-on-s (ctx/worth c))}])
        s))))

(defn fit-to-forage
  "The fit? of a job that looks for food: fit?, but a :low-health wait is lifted while the body can not heal (cannot-heal?), else it would never end."
  [c]
  (let [s (fit? c)]
    (if (and (= :low-health (:reason (nth s 2 nil))) (cannot-heal? c))
      (contract/ok :forage "hurt, but only food heals it")
      s)))

(defn starving?
  "Whether the body must get food whatever the dark: food 0, or too hurt for the riskiest work even fed (cost.risk/too-hurt?
  of the heal wait (:heal) without the hungry cut, which the walks' own price applies) with only food able to heal it (cannot-heal?)."
  [c]
  (let [self (.self (:primitives c))]
    (boolean (or (zero? (.-food self)) (and (risk/too-hurt? (.-health self) nil :heal (cost/death-s (:primitives c) (ctx/view c))) (cannot-heal? c))))))

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
