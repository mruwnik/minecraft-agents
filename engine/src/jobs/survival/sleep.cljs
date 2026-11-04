(ns jobs.survival.sleep
  (:require [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [engine.contract :as contract]
            [jobs.lib.child :as child]
            [jobs.lib.pace :as pace]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.places :as places]))

(def doc
  "Walk to a bed and sleep in it: the :bed argument, else the remembered :bed within the default-bed-radius setting, else the live :spawn-bed at any distance (go-to prices the walk).
  Conditions: night and a bed, also before every round (day came or the bed gone: a listed call waits, a called one ends condition-failed); at the start only: the bed not in :bed-unreachable
  (an unexpired entry with the same position), else :bed-unreachable {:pos}; with :open-bed false and the body within 3 blocks of
  the bed, a roofed room, else :bed-open. A body already asleep is done.
  A reflex run never holds on an occupied bed: it remembers the bed in :bed-unreachable (ten minutes) and ends nothing-possible :bed-occupied {:pos}, the night then takes the next bed. Holds (a :pause-s pause holding still with the reason, then :continue) until the call's :wall-time ends it (limit-reached): a bed
  occupied by another player (sh-beds/bed-free?; also when it became so mid-round) :bed-occupied {:pos};; a server refusal for monsters near the bed :monsters-near {:pos}.
  The walk only walks (go-to :ways #{:walk :clear-seal}): it never digs, builds or pillars its way to a bed, so a bed behind someone's wall is :bed-unreachable.
  The time spent walking is bounded by the first quote's own upper bound (:job-time, go-to's :spend); a walk that outlasts it while the quote has no whole way ends it the same way. Before each walk go-to's quote is asked (while its search goes on, the round yields :continue), and again after a walk that did not arrive: when it finds no route (the land is closed, the bed walled in), the job ends :bed-unreachable at once (no retries; the entry keeps the night from calling it again for ten minutes). After a walk that did not arrive, a quote whose search still goes on, or spent its node budget (:budget), also ends it (over unseen land that search has no end).
  Done {:pos bed} when asleep. A server refusal \"not night\" is condition-failed :not-night; a bed block gone is :bed-missing, and a
  :bed-open bed (with :open-bed false, once the body is beside it) too. A walk that did not
  arrive is retried three times, then warns bed_unreachable and ends nothing-possible :bed-unreachable (the bed goes into
  :bed-unreachable); a bed chunk not loaded (bed_unloaded) and a server refusal (bed_unusable \"cannot sleep: <status> (<reason>)\") the same, as
  :bed-unloaded and :bed-unusable. A bed the server calls obstructed ends nothing-possible :bed-obstructed at once (a retry cannot change it). The server's reason is in the text (an overlay line it sends, else said-nothing with the time of day).
  A bed that is not there is not retried; if it was the remembered bed, :bed is overwritten with {:gone true :was pos}, so it no longer reads as a place.
  With a :bed argument the radius is ignored. After sleeping there it is recorded as :bed
  when none is recorded, the recorded one is gone, or it lies in the open and this bed in a closed roofed room. Another live recorded :bed is kept (one place.kept event). Every sleep also records :home there on the same terms.
  Lying down in a bed sets the respawn point (the server's chat line is not always seen, e.g. through ViaProxy), so a sleep
  always records :spawn-bed there.
  Memory: reads :bed and :bed-unreachable. Writes :slept (cap 10, seven in-game days),
  :spawn-bed {:pos bed} (cap 1, forever),
  :bed (see above) and :bed-unreachable {:pos bed} after an unreachable bed (cap 5, ten minutes).
  Quote: time by the straight distance to the bed at cost/walk-speed; done 1 sleep.")

(a/defargs args
  {:open-bed {:doc "false: a bed that is not shut in a closed, roofed room is not lain in (the job ends condition-failed :bed-open once the body is beside it)"
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
  "The bed to sleep in: the :bed argument (nil when it is not a position), else the remembered :bed within the default-bed-radius setting (so a
  gone one is found out and retracted), else sh-spawn/standing-bed."
  [c]
  (if-let [given (:bed (:args c))]
    (:pos (places/parse-pos given))
    (or (sh-beds/bed c (sh-beds/default-bed-radius))
        (sh-spawn/spawn-bed-pos (:primitives c) (ctx/view c)))))

(a/defargs settings
  {::pause-s {:default 5 :doc "Seconds one hold pauses (jobs.time.noop) before the call looks again." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 300 :doc "Seconds the walk to the bed and the sleep may take." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 600 :doc "Seconds a sleep may last from its start, the holds for an occupied bed or the night in a shelter included." :spec (a/num-in 0 nil)}})

(defn bed-block? [c bed]
  (.endsWith (or (u/seen-name (:primitives c) bed) "") "_bed"))

(defn open-bed?
  "Whether the body, standing beside its bed, would lie in the open (not sh-beds/bed-sheltered? at its feet)."
  [c]
  (let [p (:primitives c)]
    (not (sh-beds/bed-sheltered? p (sh-sense/feet p)))))

(defn bed-open
  "The cause :bed-open when :open-bed is false, the body stands within 3 blocks of its bed and would lie in the open."
  [c bed]
  (when (and (false? (:open-bed (:args c))) (<= (u/dist (u/self-pos c) bed) 3) (bed-block? c bed) (open-bed? c))
    {:reason :bed-open :pos bed :text "the bed is open (not shut in a spawn-free roofed room)"}))

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

(def meta {:category :goal :tags [:outranks-comfort]})

(def target [])

(def defaults {:checks {:safe? '(not (threatened)) :fit? '(always)} :spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(def safe? ready/threatened)

(def fit? ready/ok)

(defn done? [c]
  (let [d (sh-sense/sleeping? (:primitives c))]
    (checks/goal d [:sleeping "already asleep"] [:awake "not asleep"] {:units :sleeps :done (if d 1 0) :left (if d 0 1)})))

(defn quote-of [c]
  (let [s (sh-beds/sleep-walk-s (:primitives c) (bed-of c))]
    {:uses {:job-time [0 s (max (* 3 s) 10)]} :done [0 1 1]}))

(def walk-ways #{:walk :clear-seal})

(def no-way-reasons
  "Reasons of a go-to quote that found no route because the land the body can walk is closed, the bed walled in or cut off, or the only
  way leads to the edge of what is loaded (:partial); other reasons (the search goes on, a body mid-jump, an unloaded goal, a refused call) say nothing of the bed."
  #{:exhausted :start-enclosed :goal-enclosed :goal-cut-off :partial})

(defn no-walk-text
  "Words for a quote route that found none (its :reason), without a claim about the body being shut in."
  [route]
  (str "cannot reach the bed on foot: no walk leads there from here"
       (when-let [r (some-> (:reason route) name)] (str " (" (str/replace r "-" " ") ")"))))

(defn no-way-quote?
  "Whether go-to's quoted route says no walk reaches the bed (found false, for one of no-way-reasons)."
  [route]
  (and (false? (:found route)) (not (:unseen-floors route)) (contains? no-way-reasons (some-> (:reason route) keyword))))

(defn late-walk?
  "Whether the time spent walking (spent-ms) passed the first quote's upper bound (hi, s; nil: none) while the latest quote's route (nil: no quote) still has no whole way."
  [route hi spent-ms]
  (boolean (and route hi (not (true? (:found route))) (< (* 1000 hi) spent-ms))))

(defn give-up-unreachable-now!
  "Warn bed_unreachable, remember the bed in :bed-unreachable (so the night stops holding for it) and end nothing-possible, at once."
  [c bed text]
  (ctx/emit! c :bed_unreachable :warn {:text text})
  (ctx/remember! c :bed-unreachable {:pos bed} unreachable-policy)
  (when-let [seen (u/seen-name (:primitives c) bed)]
    (when-not (.endsWith seen "_bed") (sh-spawn/lose-spawn! c bed :gone)))
  (ctx/end c :nothing-possible :bed-unreachable {:pos bed :text text}))

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
  (let [r (await (ctx/act c :sleep (clj->js {:pos bed})))]
    (case (.-status r)
      "sleeping" (do (ctx/remember! c :slept {:pos bed} slept-policy)
                     (sh-spawn/remember-spawn-bed! c bed)
                     (places/offer! c :bed bed)
                     (places/offer! c :home bed)
                     (ctx/end c :done nil {:pos bed}))
      "not-night" (ctx/end c :condition-failed :not-night)
      "obstructed" (let [text "the bed is obstructed (the server refuses it: a block above or beside it)"]
                     (u/fail! c :bed_unusable text)
                     (ctx/end c :nothing-possible :bed-obstructed {:pos bed :text text}))
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

(defn ^:async step! [c]
  (let [p (:primitives c)
        bed (bed-of c)
        taken? #(not (sh-beds/bed-free? p (:world (:engine c)) bed))
        _ (await (sense/decide! c ::bed-taken #(not (taken?)) {:retry? still-permitted? :price (sense/fails-to c)}))
        quote-args {:pos bed :range 2 :ways walk-ways}
        q (when-not (taken?) (await (ctx/quote c 'jobs.movement.go-to quote-args)))
        route (:route q)
        hi (some-> q (get-in [:uses :job-time]) last)
        _ (when (and hi (js/isFinite hi) (pos? hi) (not (:walk-hi (ctx/mem c))))
            (ctx/update-mem! c assoc :walk-hi hi :walk-due (+ (ctx/now c) (* 1000 hi))))
        walk-hi (:walk-hi (ctx/mem c))
        walk-args (cond-> quote-args walk-hi (assoc :spend {:job-time walk-hi}))
        late? (late-walk? route walk-hi (:walk-spent (ctx/mem c) 0))
        no-way? (no-way-quote? route)
        searching? (= :searching (some-> (:reason route) keyword))
        t0 (ctx/now c)
        walk (when-not (or (taken?) no-way? late? searching?) (await (child/call-walk* c :go walk-args)))
        _ (when walk (ctx/update-mem! c update :walk-spent (fnil + 0) (- (ctx/now c) t0)))]
    (cond
      (and (taken?) (:reflex c)) (do (ctx/remember! c :bed-unreachable {:pos bed} unreachable-policy)
                                     (ctx/end c :nothing-possible :bed-occupied {:pos bed :text "the bed is occupied; skipped for ten minutes"}))
      (taken?) (await (hold! c {:reason :bed-occupied :pos bed}))
      no-way? (give-up-unreachable-now! c bed (no-walk-text route))
      late? (give-up-unreachable-now! c bed "cannot reach the bed on foot: the walk has taken longer than its first quote allowed and no whole way is known")
      (and searching? (:walk-failed (ctx/mem c))) (give-up-unreachable-now! c bed "cannot reach the bed on foot: the walk did not arrive and the search for another way does not end")
      searching? :continue
      (= :continue walk) :continue
      (not= :done (:kind walk)) (let [_ (ctx/update-mem! c assoc :walk-failed true)
                                  after (:route (await (ctx/quote c 'jobs.movement.go-to quote-args)))]
                                  (if (or (no-way-quote? after) (= :budget (some-> (:reason after) keyword)))
                                    (give-up-unreachable-now! c bed (no-walk-text after))
                                    (give-up-unreachable! c bed walk)))
      (bed-open c bed) (ctx/end c :condition-failed :bed-open {:pos bed :text "the bed is open (not shut in a spawn-free roofed room)"})
      :else (await (sleep-at! c bed)))))

(defn ^:async round
  "step! run by pace/steps!: as a reflex (the night trigger's job) a :continue (a hold, a quote still searching, a walk to go on) is another step, not a decline."
  [c]
  (await (pace/steps! c #(step! c))))
