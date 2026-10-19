(ns jobs.survival.night.morning
  "The night's morning: a doorway it filled opened, out of the shelter, a bed it put down picked up, and the night's ending."
  (:require [engine.ctx :as ctx]
            [engine.memory :as mem]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spent :as spent]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.night.hold :as hold]
            [jobs.survival.dig-pit-site :as pit-site]
            [jobs.survival.night.sites :as sites]))

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

(defn fled
  "Walks to somewhere safer this night made, or nil when none."
  [c]
  (let [n (:flees (ctx/mem c))]
    (when (and n (pos? n)) n)))

(defn spent-fields
  "What the night's children placed (the niche), as the :spent, :spent-at and :text a done ending carries; {} when nothing."
  [c]
  (let [log (spent/log c)]
    (if log (assoc (spent/summary log) :text (spent/text log "night shelter")) {})))

(defn done-data
  "The data of a done night: how it sheltered, what it placed, the flees."
  [c how]
  (cond-> (spent-fields c) how (assoc :night how) (fled c) (assoc :fled (fled c))))

(defn end-night!
  "The night is over: forget tonight's failed sites (sites/forget-failed!) and the filled doorway cells someone else
  changed (sh-beds/fill-gone?), and end done {:night how}, or nothing-possible :exposed {:sites} when nothing sheltered the body."
  [c]
  (let [how (:sheltered (ctx/mem c))
        failed (pit-site/failed c)]
    (sites/forget-failed! c)
    (ctx/forget-where! c :doorway-filled (partial sh-beds/fill-gone? (:primitives c)))
    (if (= :exposed how)
      (ctx/end c :nothing-possible :exposed
               (cond-> {:sites failed
                        :text (str "no shelter tonight: no bed, nobody asleep, no site to dig in, nowhere safer known (" (sites/exposed-why failed) ")")}
                 (fled c) (assoc :fled (fled c))))
      (ctx/end c :done nil (done-data c how)))))

(defn ^:async morning-step!
  "By day, nothing to walk over: a filled doorway opened or out of a dig-in pit first, then a bed picked up, then the end."
  [c]
  (cond
    (and (or (= :dug-in (:sheltered (ctx/mem c))) (dig-leave/sheltered-in c) (sh-beds/doorway-to-open (:primitives c) (ctx/view c)))
         (dawn/hostile-hold? (:primitives c)))
    (do (when-not (:dawn-noted (ctx/mem c))
          (ctx/update-mem! c assoc :dawn-noted true)
          (ctx/emit! c :night.dawn-hold :info {:text "first light, but a hostile is known near the shelter; staying in until it is gone or the grace is over"}))
        (await (hold/hold! c "first light: hostile near")))
    (sh-beds/doorway-to-open (:primitives c) (ctx/view c))
    (await (open-doorway! c (sh-beds/doorway-to-open (:primitives c) (ctx/view c))))
    (or (#{:dug-in :exposed} (:sheltered (ctx/mem c))) (:dig-out (ctx/mem c)) (dig-leave/sheltered-in c))
    (let [r (await (dig-leave/leave! c {:take-back? (:take-back? (:args c))}))]
      (cond
        (= :continue r) :again
        (= :out (:reason r)) (do (ctx/update-mem! c dissoc :dig-out)
                                 (when-not (= :exposed (:sheltered (ctx/mem c))) (hold/sheltered! c :dug-in))
                                 (if (sh-beds/bed-to-collect (:primitives c) (ctx/view c)) :again (end-night! c)))
        (= :no-way-out (:reason r))
        (let [text "no way out of the shelter by day; giving the body back"]
          (ctx/emit! c :shelter.failed :warn {:reason :no-way-out :at (:at r) :tries (:tries r) :text text})
          (ctx/remember! c :shelter-trapped {:pos (sh-sense/feet (:primitives c))} sh-inside/trapped-policy)
          (ctx/forget-where! c :night-site (constantly true))
          (ctx/end c :nothing-possible :no-way-out {:text text :at (:at r) :tries (:tries r)}))
        :else (await (hold/hold! c :leaving))))
    :else
    (if-let [bed (sh-beds/bed-to-collect (:primitives c) (ctx/view c))]
      (await (collect-bed! c bed))
      (end-night! c))))

(defn ^:async morning!
  "By day: a dug bed's drop is walked over first, else morning-step!."
  [c]
  (hold/busy! c)
  (if-let [pos (:pickup (ctx/mem c))]
    (await (pick-up-drop! c pos))
    (await (morning-step! c))))
