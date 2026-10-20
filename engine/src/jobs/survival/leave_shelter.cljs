(ns jobs.survival.leave-shelter
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.checks :as checks]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.night.hold :as hold]))

(def doc
  "The morning after a night: out of the shelter, the blocks it placed taken back, a bed put down picked up. The dawn
  trigger starts it; night itself ends when the body is sheltered or the night is over.
  Waits :night by night. Done once nothing is left: no shelter the body is shut in (jobs.lib.dig-in-leave/leave!: walls
  dug, a pit climbed, the shelter's valuable blocks taken back), no doorway the night filled (still holding a block it
  put there) to dig again, no bed it put down outside its own zones (not the respawn bed) to dig up, no drop to walk
  over. A shelter with no way out ends nothing-possible :no-way-out (the cell is remembered as :shelter-trapped, so the
  trigger lets go). A body shut in stays while a known hostile is near and the dawn grace lasts (night.dawn-hold).
  A doorway cell it could not open is warned night.doorway-stuck once and tried again quietly.
  Memory: job memory :pickup, :dig-out, :take-back; body memory :doorway-filled, :bed-placed, :shelter-trapped.")

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

(defn ^:async walk!
  "A go-to child in slot to pos within range, at night too: :continue while it walks, else whether it arrived."
  [c slot pos range]
  (let [w (await (ctx/call c slot 'jobs.movement.go-to {:pos pos :range range}))]
    (if (= :continue w) :continue (boolean (:arrived (:data w))))))

(defn ^:async collect-bed!
  "By day: dig up the bed at pos the night put down outside the body's zones, then walk over its drop. One try: the
  :bed-placed entry is dropped whatever happens, and a :bed entry at pos is retracted."
  [c pos]
  (let [arrived (await (walk! c :bed-collect pos 2))]
    (if (= :continue arrived)
      :again
      (do (when arrived
            (let [r (await (tidy/dig! c pos))]
              (when (= "dug" (.-status r))
                (when (= pos (mem/place (ctx/view c) :bed))
                  (ctx/remember! c :bed {:gone true :was pos} mem/place-policy))
                (ctx/update-mem! c assoc :pickup pos))))
          (ctx/forget-where! c :bed-placed (constantly true))
          :again))))

(defn ^:async open-doorway!
  "By day: dig the cells the night filled in a doorway (sh-beds/doorway-to-open: still holding a block it put there), after a
  walk within reach of the first, and walk over the drop. No zone check: it takes out only blocks this body put there.
  Entries seen holding none of their blocks are forgotten (sh-beds/fill-gone?); a cell it could not open (the walk or the dig
  failed) is marked :doorway-open-failed, so the night trigger tries it again once that lapses, and warned
  night.doorway-stuck once per cell and cause (walk or dig; :doorway-stuck-warned, a day)."
  [c cells]
  (let [arrived (await (walk! c :doorway-open (first cells) 3))]
    (if (= :continue arrived)
      :again
      (do (when arrived
            (loop [[cell & more] cells dug nil]
              (let [n (when cell (u/seen-name (:primitives c) cell))]
                (cond
                  (nil? cell) (when dug (ctx/update-mem! c assoc :pickup dug))
                  (not (some #{cell} (sh-beds/doorway-to-open (:primitives c) (ctx/view c)))) (recur more dug)
                  :else (let [_ (await (tools/equip-for! c n {:fast true}))
                              r (await (tidy/dig! c cell))]
                          (recur more (or dug (when (= "dug" (.-status r)) cell))))))))
          (let [held (keep #(when (sh-beds/still-filled? (:primitives c) (:data %)) (:cell (:data %))) (ctx/entries c :doorway-filled))
                stuck (filterv (set held) cells)]
            (ctx/forget-where! c :doorway-filled (partial sh-beds/fill-gone? (:primitives c)))
            (doseq [cell stuck] (ctx/remember! c :doorway-open-failed {:cell cell} sh-beds/doorway-open-failed-policy))
            (let [cause (if arrived :dig :walk)
                  new-stuck (filterv (fn [cell] (not-any? #(= {:cell cell :cause cause} (:data %)) (ctx/entries c :doorway-stuck-warned))) stuck)]
              (when (seq new-stuck)
                (doseq [cell new-stuck] (ctx/remember! c :doorway-stuck-warned {:cell cell :cause cause} sh-beds/doorway-filled-policy))
                (ctx/emit! c :night.doorway-stuck :warn {:cells new-stuck :arrived arrived
                                                         :text (str "could not open the doorway it filled at " (pr-str new-stuck) "; trying again quietly")}))))
          :again))))

(defn ^:async pick-up-drop!
  "Walk over the dug bed's drop at pos (go-to); one try, then the walk is over."
  [c pos]
  (when-not (= :continue (await (walk! c :bed-pickup pos 0))) (ctx/update-mem! c dissoc :pickup))
  :again)

(defn pending?
  "Whether the morning still has work: a pickup walk, a dig-out, a shelter to leave, a doorway the night filled to open, a
  bed put down to collect."
  [c]
  (let [p (:primitives c)
        m (ctx/mem c)]
    (boolean (or (:pickup m) (:dig-out m)
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
      (:pickup (ctx/mem c)) (await (pick-up-drop! c (:pickup (ctx/mem c))))
      (and shelter (dawn/hostile-hold? p))
      (do (when-not (:dawn-noted (ctx/mem c))
            (ctx/update-mem! c assoc :dawn-noted true)
            (ctx/emit! c :night.dawn-hold :info {:text "first light, but a hostile is known near the shelter; staying in until it is gone or the grace is over"}))
          (await (hold/hold! c "first light: hostile near")))
      doorway (await (open-doorway! c doorway))
      (or (:dig-out (ctx/mem c)) shelter)
      (let [r (await (dig-leave/leave! c))]
        (cond
          (= :continue r) :again
          (= :out (:reason r)) :again
          (= :no-way-out (:reason r))
          (let [text "no way out of the shelter by day; giving the body back"]
            (ctx/emit! c :shelter.failed :warn {:reason :no-way-out :at (:at r) :tries (:tries r) :text text})
            (ctx/remember! c :shelter-trapped {:pos (sh-sense/feet p)} sh-inside/trapped-policy)
            (ctx/forget-where! c :night-site (constantly true))
            (ctx/end c :nothing-possible :no-way-out {:text text :at (:at r) :tries (:tries r)}))
          :else (await (hold/hold! c :leaving))))
      :else
      (if-let [bed (sh-beds/bed-to-collect p (ctx/view c))]
        (await (collect-bed! c bed))
        :again))))

(defn ^:async round [c] (await (pace/steps! c #(step! c))))
