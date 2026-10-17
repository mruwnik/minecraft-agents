(ns jobs.survival.sleep
  (:require [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [engine.contract :as contract]
            [jobs.lib.child :as child]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.places :as places]))

(def doc
  "Walk to a bed and sleep in it: the :bed argument, else the remembered :bed within :bed-radius, else the live :spawn-bed within it.
  Conditions: night and a bed, also before every round (day came or the bed gone: a listed call waits, a called one ends condition-failed); at the start only: the bed not in :bed-unreachable
  (an unexpired entry with the same position), else :bed-unreachable {:pos}; with :open-bed false and the body within 3 blocks of
  the bed, a roofed room, else :bed-open. A body already asleep is done.
  Holds (a :pause-s pause holding still with the reason, then :continue) until the call's :wall-time ends it (limit-reached): a bed
  occupied by another player (sh-beds/bed-permit; also when it became so mid-round) :bed-occupied {:pos}; a body in a shelter with the bed more
  than 16 blocks away :night-sheltered; a server refusal for monsters near the bed :monsters-near {:pos}.
  Done {:pos bed} when asleep. A server refusal \"not night\" is condition-failed :not-night; a bed block gone is :bed-missing, and a
  :bed-open bed (with :open-bed false, once the body is beside it) too. A walk that did not
  arrive is retried three times, then warns bed_unreachable and ends nothing-possible :bed-unreachable (the bed goes into
  :bed-unreachable); a bed chunk not loaded (bed_unloaded) and a server refusal (bed_unusable \"cannot sleep: <status> (<reason>)\") the same, as
  :bed-unloaded and :bed-unusable.
  A bed that is not there is not retried; if it was the remembered bed, :bed is overwritten with {:gone true :was pos}, so it no longer reads as a place.
  With a :bed argument the radius is ignored. After sleeping there it is recorded as :bed
  when none is recorded or the recorded one is gone. A live recorded :bed is kept (one place.kept event). Every sleep also records :home there unless a live :home is recorded.
  Warns spawn_not_set when it sleeps and no :spawn-set (the body event for the Respawn point set line) follows, unless one
  was seen within 3 blocks of the bed before (kept as :spawn-bed, forever, until a later :spawn-reset):
  death would then send the body to the world spawn.
  Memory: reads :bed, :bed-unreachable, :spawn-set and :spawn-reset. Writes :slept (cap 10, seven in-game days),
  :spawn-bed {:pos bed} (cap 1, forever),
  :bed (see above) and :bed-unreachable {:pos bed} after an unreachable bed (cap 5, ten minutes).
  Quote: time by the straight distance to the bed at :speed blocks a second; done 1 sleep.")

(a/defargs args
  {:bed-radius {:doc "a remembered bed farther than this many blocks from the body is not used"
                :spec (a/num-in 0 nil) :default sh-beds/default-bed-radius}
   :open-bed {:doc "false: a bed that is not shut in a closed, roofed room is not lain in (the job ends :stopped :bed-open once the body is beside it)"
              :spec boolean? :default true}
   :bed {:doc "bed position [x y z] or {:x :y :z} to sleep in instead of the remembered :bed (no radius); it is recorded as :bed when that is unset or gone" :spec ::a/pos
         :default nil}})

(def slept-policy {:cap 10 :ttl (* 7 sh-clock/ms-per-day)})

(def unreachable-policy {:cap 5 :ttl 600000})

(defn unreachable-bed?
  "Whether an unexpired :bed-unreachable entry names bed."
  [c bed]
  (boolean (some #(= bed (:pos (:data %))) (ctx/entries c :bed-unreachable))))

(defn bed-of
  "The bed to sleep in: the :bed argument (nil when it is not a position), else the remembered :bed within :bed-radius (so a
  gone one is found out and retracted), else sh-spawn/standing-bed."
  [c]
  (if-let [given (:bed (:args c))]
    (:pos (places/parse-pos given))
    (or (sh-beds/bed c (:bed-radius (:args c)))
        (sh-spawn/standing-bed (:primitives c) (ctx/view c) (:bed-radius (:args c))))))

(def leave-shelter-radius
  "A body in a shelter at night walks to a bed only when it is this near (blocks); a farther one is waited for."
  16)

(defn far-from-shelter
  "The kind of kept shelter (sh-beds/kept-shelters: not a bare roof) a walk to a bed beyond leave-shelter-radius would leave, else nil."
  [c bed]
  (let [p (:primitives c)]
    (when (> (u/dist (u/self-pos c) bed) leave-shelter-radius)
      (sh-beds/kept-shelters (sh-inside/walk-leaves-shelter? p (:data (ctx/latest c :shelter)) bed 2)))))

(a/defargs settings
  {::pause-s {:default 5 :doc "Seconds one hold pauses (jobs.time.noop) before the call looks again." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 300 :doc "Seconds the walk to the bed and the sleep may take." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 600 :doc "Seconds a sleep may last from its start, the holds for an occupied bed or the night in a shelter included." :spec (a/num-in 0 nil)}
   ::speed {:default 4.3 :doc "Blocks a second the walk to the bed covers, for the quote." :spec (a/num-in 0 nil)}})

(defn bed-block? [c bed]
  (.endsWith (or (u/seen-name (:primitives c) bed) "") "_bed"))

(defn open-bed?
  "Whether the body, standing beside its bed, would lie in the open: not shut in a closed room under a roof nor buried."
  [c]
  (let [p (:primitives c)]
    (not (or (sh-sense/shut-in-room? p) (sh-sense/buried? p)))))

(defn bed-open
  "The cause :bed-open when :open-bed is false, the body stands within 3 blocks of its bed and would lie in the open."
  [c bed]
  (when (and (false? (:open-bed (:args c))) (<= (u/dist (u/self-pos c) bed) 3) (bed-block? c bed) (open-bed? c))
    {:reason :bed-open :pos bed :text "the bed is open (not shut in a roofed room)"}))

(defn bed-status
  "Ok when asleep already or with a bed to sleep in, else later :no-bed, :bed-unreachable (given up on) or :bed-open."
  [c]
  (let [bed (bed-of c)
        open (when bed (bed-open c bed))]
    (cond
      (sh-sense/sleeping? (:primitives c)) (contract/ok :asleep "asleep")
      (nil? bed) (contract/later :no-bed "no bed given or remembered within the bed radius")
      (unreachable-bed? c bed) (contract/later :bed-unreachable "the bed was given up on as unreachable" :pos bed)
      open (contract/later :bed-open (:text open) :pos bed)
      :else (contract/ok :bed "a bed" :pos bed))))

(defn sheltered-hold
  "The hold of a body in a shelter with the bed far off: {:reason :night-sheltered ...}, else nil."
  [c bed]
  (when-let [shelter (far-from-shelter c bed)]
    {:reason :night-sheltered :shelter shelter :pos bed
     :text (str "the bed is " (js/Math.round (u/dist (u/self-pos c) bed)) " blocks away; the body stays in its shelter until day")}))

(def night (cond/because :not-night "not night" (cond/not (cond/daytime))))

(defn can-run?
  "Before every round: day came (:later :not-night) or the bed is gone (:later :no-bed)."
  [c]
  (cond
    (not (sh-sense/night? (:primitives c))) (contract/later :not-night "not night")
    (nil? (bed-of c)) (contract/later :no-bed "no bed given or remembered within the bed radius")
    :else (contract/ok :bed "night, and a bed")))

(defn can-start?
  "The night, and by night a bed; by day the bed (a room flood fill) is not looked for."
  [c]
  (let [n (contract/asked c night)]
    (if (= :ok (first n)) (contract/all [n (contract/asked c bed-status)]) n)))

(def target [])

(def defaults {:checks {:safe? '(not (threatened)) :fit? '(always)} :spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(def safe? ready/threatened)

(def fit? ready/ok)

(defn done? [c]
  (let [d (and (not (checks/running? c)) (sh-sense/sleeping? (:primitives c)))]
    (checks/goal d [:sleeping "already asleep"] [:awake "not asleep"] {:units :sleeps :done (if d 1 0) :left (if d 0 1)})))

(defn quote-of [c]
  (let [bed (bed-of c)
        s (if bed (/ (u/dist (u/self-pos c) bed) (settings/get settings ::speed)) 0)]
    {:uses {:job-time [0 s (max (* 3 s) 10)]} :done [0 1 1]}))

(defn give-up-unreachable! [c bed walk]
  (let [why (some-> (get-in walk [:data :text]) (str/replace #"^gave up walking to \[[^\]]*\]: " ""))
        text (str "cannot reach the bed" (some->> why (str ": ")))
        r (u/fail! c :bed_unreachable text)]
    (when (= :done r)
      (ctx/remember! c :bed-unreachable {:pos bed} unreachable-policy)
      (when-let [seen (u/seen-name (:primitives c) bed)]
        (when-not (.endsWith seen "_bed") (sh-spawn/lose-spawn! c bed :gone))))
    (if (= :done r) (ctx/end c :nothing-possible :bed-unreachable {:pos bed :text text}) r)))

(defn fail-ending
  "u/fail!'s answer r (:continue to retry, :done once given up) as this call's: the retry, or nothing-possible cause."
  [c r cause text bed]
  (if (= :done r) (ctx/end c :nothing-possible cause {:pos bed :text text}) r))

(declare hold!)

(defn ^:async sleep-at! [c bed]
  (let [t0 (ctx/now c)
        r (await (ctx/act c :sleep (clj->js {:pos bed})))]
    (case (.-status r)
      "sleeping" (do (ctx/remember! c :slept {:pos bed} slept-policy)
                     (if (sh-spawn/spawn-known? (ctx/view c) bed t0)
                       (ctx/remember! c :spawn-bed {:pos bed} sh-spawn/spawn-bed-policy)
                       (ctx/emit! c :spawn_not_set :warn {:pos bed :text "slept in the bed but the server did not say the respawn point was set"}))
                     (when (:bed (:args c)) (places/offer! c :bed bed))
                     (places/offer! c :home bed)
                     (ctx/end c :done nil {:pos bed}))
      "not-night" (ctx/end c :condition-failed :not-night)
      "monsters-near" (await (hold! c {:reason :monsters-near :pos bed :text "monsters are near the bed; the server refuses sleep"}))
      "missing" (if (nil? (u/sensed (:primitives c) bed))
                  (let [text "bed chunk not loaded yet"]
                    (fail-ending c (u/fail! c :bed_unloaded text) :bed-unloaded text bed))
                  (let [_ (sh-spawn/lose-spawn! c bed :missing)
                        known? (= bed (mem/place (ctx/view c) :bed))]
                    (when known?
                      (ctx/remember! c :bed {:gone true :was bed} mem/place-policy))
                    (ctx/end c :condition-failed :bed-missing
                             {:pos bed :text (if known? "no bed at the remembered place" "no bed at the given place")})))
      (let [text (str "cannot sleep: " (.-status r) (when-let [why (.-reason r)] (str " (" why ")")))]
        (fail-ending c (u/fail! c :bed_unusable text) :bed-unusable text bed)))))

(defn still-permitted?
  "sense/decide!'s retry? for the bed-taken decision: its value is true while the bed still reads permitted, so keep looking."
  [v]
  (true? v))

(defn ^:async hold!
  "Hold for why ({:reason ...}): a pause of :pause-s (child/pause!), then :continue; the call's :wall-time ends it."
  [c why]
  (await (child/pause! c (settings/get settings ::pause-s) (:reason why)))
  :continue)

(defn ^:async round [c]
  (let [p (:primitives c)
        bed (bed-of c)]
    (cond
      (sheltered-hold c bed) (await (hold! c (sheltered-hold c bed)))
      :else
      (let [taken? #(not ((sh-beds/bed-permit p (:world (:engine c))) bed))
            _ (await (sense/decide! c ::bed-taken #(not (taken?)) {:retry? still-permitted?}))
            walk (when-not (taken?) (await (child/call-walk* c :go {:pos bed :range 2})))]
        (cond
          (taken?) (await (hold! c {:reason :bed-occupied :pos bed}))
          (= :continue walk) :continue
          (not= :done (:kind walk)) (give-up-unreachable! c bed walk)
          (bed-open c bed) (ctx/end c :condition-failed :bed-open {:pos bed :text "the bed is open (not shut in a roofed room)"})
          :else (await (sleep-at! c bed)))))))
