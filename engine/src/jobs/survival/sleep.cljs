(ns jobs.survival.sleep
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.child :as child]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter :as sh]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.places :as places]))

(def doc
  "Walk to a bed and sleep in it: the :bed argument, else the remembered :bed within :bed-radius, else the live :spawn-bed within it.
  Conditions (at the start only): night, else condition-failed :not-night; a bed, else :no-bed; the bed not in :bed-unreachable
  (an unexpired entry with the same position), else :bed-unreachable {:pos}; with :open-bed false and the body within 3 blocks of
  the bed, a roofed room, else :bed-open. A body already asleep is done.
  Holds (:continue, holding still with the reason) for at most :hold-s seconds in all, then ends condition-failed with that cause: a bed
  occupied by another player (sh/bed-permit; also when it became so mid-round) :bed-occupied {:pos}; a body in a shelter with the bed more
  than 16 blocks away :night-sheltered.
  Done {:pos bed} when asleep. Day coming before it slept is condition-failed :not-night; the bed gone is :bed-missing, and a
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
                :spec (a/num-in 0 nil) :default sh/default-bed-radius}
   :open-bed {:doc "false: a bed that is not shut in a closed, roofed room is not lain in (the job ends :stopped :bed-open once the body is beside it)"
              :spec boolean? :default true}
   :bed {:doc "bed position [x y z] or {:x :y :z} to sleep in instead of the remembered :bed (no radius); it is recorded as :bed when that is unset or gone" :spec ::a/pos
         :default nil}})

(def slept-policy {:cap 10 :ttl (* 7 sh/ms-per-day)})

(def unreachable-policy {:cap 5 :ttl 600000})

(defn unreachable-bed?
  "Whether an unexpired :bed-unreachable entry names bed."
  [c bed]
  (boolean (some #(= bed (:pos (:data %))) (ctx/entries c :bed-unreachable))))

(defn bed-of
  "The bed to sleep in: the :bed argument (nil when it is not a position), else the remembered :bed within :bed-radius (so a
  gone one is found out and retracted), else sh/standing-bed."
  [c]
  (if-let [given (:bed (:args c))]
    (:pos (places/parse-pos given))
    (or (sh/bed c (:bed-radius (:args c)))
        (sh/standing-bed (:primitives c) (ctx/view c) (:bed-radius (:args c))))))

(def leave-shelter-radius
  "A body in a shelter at night walks to a bed only when it is this near (blocks); a farther one is waited for."
  16)

(defn far-from-shelter
  "The kind of kept shelter (sh/kept-shelters: not a bare roof) a walk to a bed beyond leave-shelter-radius would leave, else nil."
  [c bed]
  (let [p (:primitives c)]
    (when (> (u/dist (u/self-pos c) bed) leave-shelter-radius)
      (sh/kept-shelters (sh/walk-leaves-shelter? p (:data (ctx/latest c :shelter)) bed 2)))))

(a/defargs settings
  {::hold-s {:default 120 :doc "Seconds the call holds for an occupied bed or the night in a shelter, then ends." :spec (a/num-in 0 nil)}
   ::time-s {:default 300 :doc "Seconds the walk to the bed and the sleep may take." :spec (a/num-in 0 nil)}
   ::speed {:default 4.3 :doc "Blocks a second the walk to the bed covers, for the quote." :spec (a/num-in 0 nil)}})

(defn bed-block? [c bed]
  (.endsWith (or (u/seen-name (:primitives c) bed) "") "_bed"))

(defn open-bed?
  "Whether the body, standing beside its bed, would lie in the open: not shut in a closed room under a roof nor buried."
  [c]
  (let [p (:primitives c)]
    (not (or (sh/shut-in-room? p) (sh/buried? p)))))

(defn bed-open
  "The cause :bed-open when :open-bed is false, the body stands within 3 blocks of its bed and would lie in the open."
  [c bed]
  (when (and (false? (:open-bed (:args c))) (<= (u/dist (u/self-pos c) bed) 3) (bed-block? c bed) (open-bed? c))
    {:reason :bed-open :pos bed :text "the bed is open (not shut in a roofed room)"}))

(defn start-fact
  "The fact that bars starting, or nil: night and a bed that is not given up on and not occupied."
  [c]
  (when-not (ctx/frozen-call c)
    (let [bed (bed-of c)
          p (:primitives c)]
      (cond
        (not (sh/night? p)) :not-night
        (sh/sleeping? p) nil
        (nil? bed) {:reason :no-bed :text "no bed given or remembered within the bed radius"}
        (unreachable-bed? c bed) {:reason :bed-unreachable :pos bed}
        :else (bed-open c bed)))))

(defn sheltered-hold
  "The hold of a body in a shelter with the bed far off: {:reason :night-sheltered ...}, else nil."
  [c bed]
  (when-let [shelter (far-from-shelter c bed)]
    {:reason :night-sheltered :shelter shelter :pos bed
     :text (str "the bed is " (js/Math.round (u/dist (u/self-pos c) bed)) " blocks away; the body stays in its shelter until day")}))

(def conditions [[:fn start-fact]])

(def target [])

(def defaults {:spend {:time ::time-s}})

(defn done? [c] (let [d (sh/sleeping? (:primitives c))] {:done? d :units :sleeps :done (if d 1 0) :left (if d 0 1)}))

(defn quote-of [c]
  (let [bed (bed-of c)
        s (if bed (/ (u/dist (u/self-pos c) bed) (settings/get settings ::speed)) 0)]
    {:uses {:time [0 s (max (* 3 s) 10)]} :done [0 1 1]}))

(defn give-up-unreachable! [c bed walk]
  (let [why (some-> (get-in walk [:data :text]) (str/replace #"^gave up walking to \[[^\]]*\]: " ""))
        text (str "cannot reach the bed" (some->> why (str ": ")))
        r (u/fail! c :bed_unreachable text)]
    (when (= :done r)
      (ctx/remember! c :bed-unreachable {:pos bed} unreachable-policy)
      (when-let [seen (u/seen-name (:primitives c) bed)]
        (when-not (.endsWith seen "_bed") (sh/lose-spawn! c bed :gone))))
    (if (= :done r) (ctx/end c :nothing-possible :bed-unreachable {:pos bed :text text}) r)))

(defn fail-ending
  "u/fail!'s answer r (:continue to retry, :done once given up) as this call's: the retry, or nothing-possible cause."
  [c r cause text bed]
  (if (= :done r) (ctx/end c :nothing-possible cause {:pos bed :text text}) r))

(defn ^:async sleep-at! [c bed]
  (let [t0 (ctx/now c)
        r (await (ctx/act c :sleep (clj->js {:pos bed})))]
    (case (.-status r)
      "sleeping" (do (ctx/remember! c :slept {:pos bed} slept-policy)
                     (if (sh/spawn-known? (ctx/view c) bed t0)
                       (ctx/remember! c :spawn-bed {:pos bed} sh/spawn-bed-policy)
                       (ctx/emit! c :spawn_not_set :warn {:pos bed :text "slept in the bed but the server did not say the respawn point was set"}))
                     (when (:bed (:args c)) (places/offer! c :bed bed))
                     (places/offer! c :home bed)
                     (ctx/end c :done nil {:pos bed}))
      "not-night" (ctx/end c :condition-failed :not-night)
      "missing" (if (nil? (u/sensed (:primitives c) bed))
                  (let [text "bed chunk not loaded yet"]
                    (fail-ending c (u/fail! c :bed_unloaded text) :bed-unloaded text bed))
                  (do (sh/lose-spawn! c bed :missing)
                      (if (= bed (mem/place (ctx/view c) :bed))
                        (do (ctx/remember! c :bed {:gone true :was bed} mem/place-policy)
                            (ctx/emit! c :bed_missing :warn {:pos bed :text "no bed at the remembered place"}))
                        (ctx/emit! c :bed_missing :warn {:pos bed :text "no bed at the given place"}))
                      (ctx/end c :condition-failed :bed-missing {:pos bed})))
      (let [text (str "cannot sleep: " (.-status r) (when-let [why (.-reason r)] (str " (" why ")")))]
        (fail-ending c (u/fail! c :bed_unusable text) :bed-unusable text bed)))))

(defn still-permitted?
  "sense/decide!'s retry? for the bed-taken decision: its value is true while the bed still reads permitted, so keep looking."
  [v]
  (true? v))

(defn hold!
  "Hold for why ({:reason ...}): :continue, holding still, until :hold-s seconds since the first hold are gone, then the
  condition-failed ending with that reason."
  [c why]
  (let [now (ctx/now c)
        since (or (:held-since (ctx/mem c)) now)]
    (ctx/update-mem! c assoc :held-since since)
    (if (> (- now since) (* 1000 (settings/get settings ::hold-s)))
      (ctx/end c :condition-failed (:reason why) why)
      (do (ctx/hold-still! c (:reason why)) :continue))))

(defn ^:async round [c]
  (let [p (:primitives c)
        bed (bed-of c)]
    (cond
      (not (sh/night? p)) (ctx/end c :condition-failed :not-night)
      (nil? bed) (ctx/end c :condition-failed :no-bed)
      (sheltered-hold c bed) (hold! c (sheltered-hold c bed))
      :else
      (let [taken? #(not ((sh/bed-permit p (:world (:engine c))) bed))
            _ (await (sense/decide! c ::bed-taken #(not (taken?)) {:retry? still-permitted?}))
            walk (when-not (taken?) (await (child/call-walk* c :go {:pos bed :range 2 :at-night :go})))]
        (cond
          (taken?) (hold! c {:reason :bed-occupied :pos bed})
          (= :continue walk) :continue
          (not= :done (:kind walk)) (give-up-unreachable! c bed walk)
          (bed-open c bed) (ctx/end c :condition-failed :bed-open {:pos bed :text "the bed is open (not shut in a roofed room)"})
          :else (await (sleep-at! c bed)))))))
