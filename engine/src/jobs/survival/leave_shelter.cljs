(ns jobs.survival.leave-shelter
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.checks :as checks]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.waits :as waits]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.util :as u]
            [jobs.survival.night.hold :as hold]))

(def doc
  "The morning after a night: out of the shelter, the blocks it placed taken back, a bed put down picked up. The dawn
  trigger starts it; night itself ends when the body is sheltered or the night is over.
  Waits :night by night. Done once nothing is left: no shelter the body is shut in (jobs.lib.dig-in-leave/leave!: walls
  dug, a pit climbed, the shelter's valuable blocks taken back), no doorway the night filled (still holding a block it
  put there) to dig again, no bed it put down outside its own zones (not the respawn bed) to dig up, no drop to walk
  over. A shelter with no way out ends nothing-possible :no-way-out (the cell is remembered as :shelter-trapped, so the
  trigger lets go). A shelter it left is remembered the same way, so a walk back through its cell is no new dawn. A body shut in stays while a known hostile is near and the dawn grace lasts (night.dawn-hold).
  A doorway cell it could not open is warned night.doorway-stuck once and tried again quietly.
  The doorway blocks and the bed are scaffold-ledger entries (purposes :doorway, :bed); one jobs.access.cleanup call takes them back.
  Memory: job memory :dig-out, :take-back; body memory :shelter-trapped, :doorway-stuck-warned.")

(a/defargs args {})

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds leaving the shelter may take: a bound against a runaway morning." :spec (a/num-in 0 nil)}
   ::morning-s {:default 60 :doc "Seconds the morning (out of the shelter, a put-down bed picked up) is reckoned to take at most, in the quote." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s}})

(def meta {:category :reflex :tags [:shelter]})

(def safe? ready/ok)

(def fit? ready/ok)

(def can-start? (checks/day :night))

(def stuck-warned-policy
  "Policy of :doorway-stuck-warned: a cell warned about once a day."
  {:cap 4 :ttl sh-clock/ms-per-day})

(defn warn-stuck!
  "Warn night.doorway-stuck once per cell for the doorway cells cleanup left open (it holds them for a while, so they are retried quietly)."
  [c open]
  (let [cells (filterv (fn [cell] (not-any? #(= {:cell cell} (:data %)) (ctx/entries c :doorway-stuck-warned)))
                       (keep #(when (= :doorway (:purpose %)) (:cell %)) open))]
    (when (seq cells)
      (doseq [cell cells] (ctx/remember! c :doorway-stuck-warned {:cell cell} stuck-warned-policy))
      (ctx/emit! c :night.doorway-stuck :warn {:cells cells :text (str "could not open the doorway it filled at " (pr-str cells) "; trying again quietly")}))))

(defn retract-bed!
  "A bed cleanup took (an item ending _bed among the removed cells) is no longer the remembered :bed."
  [c removed]
  (when-let [{:keys [cell]} (first (filter #(.endsWith (str (:item %)) "_bed") removed))]
    (when (= (mem/place (ctx/view c) :bed) (u/cell-pos cell))
      (ctx/remember! c :bed {:gone true :was (u/cell-pos cell)} mem/place-policy))))

(defn ^:async take-back!
  "Take back what the night put down outside its own zones, the doorway blocks it filled and the bed (scaffold ledger
  purposes :doorway and :bed), with one jobs.access.cleanup call that walks to them, digs and picks up the drops. Cells it
  leaves open are held by cleanup (10 minutes) and warned about once. :again."
  [c]
  (let [r (waits/waiting-out c (await (ctx/call c :take-back 'jobs.access.cleanup
                                                {:of :all :purpose #{:doorway :bed} :ignore-zones? true
                                                 :checks {:fit? '(always) :safe? '(not (threatened))}})))]
    (when-not (= :continue r)
      (retract-bed! c (:removed (:data r)))
      (warn-stuck! c (let [held (ledger/held-cells (ctx/view c))]
                       (filterv #(held (:cell %)) (ledger/open-entries (ctx/view c))))))
    :again))

(defn pending?
  "Whether the morning still has work: a dig-out, a shelter to leave, a doorway the night filled to open, a
  bed put down to collect."
  [c]
  (let [p (:primitives c)
        m (ctx/mem c)]
    (boolean (or (:dig-out m)
                 (sh-inside/shut-in-by-day? p (:data (ctx/latest c :shelter)) (:data (ctx/latest c :shelter-trapped)))
                 (sh-beds/doorway-to-open p (ctx/view c))
                 (sh-beds/bed-to-collect p (ctx/view c))))))

(defn done?
  "Nothing left to leave or take up (result: none)."
  [c]
  (checks/goal (not (pending? c)) [:out "out of the shelter, nothing left to take up"] [:shelter "the morning is not done"] {:units :mornings}))

(defn quote-of [_]
  (let [most (settings/get settings ::morning-s)]
    {:uses {:job-time [0 (/ most 2) most]}}))

(defn ^:async step!
  "One choice of the morning: hold for a known hostile at first light, a filled doorway opened, out of the shelter, a bed
  picked up."
  [c]
  (hold/busy! c)
  (let [p (:primitives c)
        doorway (sh-beds/doorway-to-open p (ctx/view c))
        shelter (or doorway (dig-leave/sheltered-in c))]
    (cond
      (and shelter (dawn/hostile-hold? p))
      (do (when-not (:dawn-noted (ctx/mem c))
            (ctx/update-mem! c assoc :dawn-noted true)
            (ctx/emit! c :night.dawn-hold :info {:text "first light, but a hostile is known near the shelter; staying in until it is gone or the grace is over"}))
          (await (hold/hold! c "first light: hostile near")))
      doorway (await (take-back! c))
      (or (:dig-out (ctx/mem c)) shelter)
      (let [r (await (dig-leave/leave! c))]
        (cond
          (= :continue r) :again
          (= :out (:reason r))
          (do (ctx/remember! c :shelter-trapped {:pos (:pos (:data (ctx/latest c :shelter)))} sh-inside/left-policy)
              :again)
          (= :no-way-out (:reason r))
          (let [text "no way out of the shelter by day; giving the body back"]
            (ctx/remember! c :shelter-trapped {:pos (sh-sense/feet p)} sh-inside/trapped-policy)
            (ctx/forget-where! c :night-site (constantly true))
            (ctx/end c :nothing-possible :no-way-out {:text text :at (:at r) :tries (:tries r)}))
          :else (await (hold/hold! c :leaving))))
      :else
      (if (sh-beds/bed-to-collect p (ctx/view c))
        (await (take-back! c))
        :again))))

(defn ^:async round [c] (await (pace/steps! c #(step! c))))
