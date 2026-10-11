(ns jobs.survival.bed-down
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.places :as places]
            [jobs.lib.result :as result]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter :as sh]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tidy-rules :as tidy-rules]
            [jobs.lib.util :as u]))

(def doc
  "Go to bed: sleep in a bed to use, else put a carried bed down beside the body and sleep in it. One step per call, run to its end by the caller (jobs.survival.night).
  Waits unless it is night and there is a bed to use or a bed item to put down (reasons :not-night, :no-bed).
  The bed: the :bed argument, else sh/bed-to-use (seen, or remembered within :bed-radius, :urgent-bed-radius once :max-days-awake days passed without sleep; never an occupied one or one given up on; unless that long without sleep, never one whose walk leaves an underground, dug-in or room shelter): jobs.survival.sleep on it.
  A bed in the open is slept in only by a body overdue for sleep or one that just put the bed down; a sleep that stops :bed-open writes :bed-open (shelter.bed_open info) and ends done {:bed-open true}.
  A sleep that ends without sleeping writes :sleep-failed (5 minutes, shelter.sleep_failed warn).
  A bed item carried and no bed to use: put down beside the body (roofed cells first, else in the open; never in a doorway, on a chest, table or other kept block, or in another's zone, claim or plan footprint), recorded as :bed unless a live :bed is, and as :bed-placed outside the body's own zones (info night.bed-placed {:pos :own-zone}). No room, or a failed placement, writes :bed-place-failed (10 minutes, shelter.bed_place_failed warn).
  Walks use :at-night :go. Ends done {:slept true} when asleep, else done {:slept false} (with :bed-open, or nothing to say), :continue while walking or sleeping.")

(a/defargs args
  {:bed {:doc "bed position [x y z] or {:x :y :z} to sleep in; default: the bed to use within the radius" :spec ::a/pos :default nil}
   :roof-height {:doc "a solid block within this many blocks above counts as a roof (where a carried bed is put down)" :spec (a/int-in 0 nil) :default sh/default-roof-height}
   :bed-radius {:doc "a remembered bed farther than this is not used" :spec (a/num-in 0 nil) :default sh/default-bed-radius}
   :urgent-bed-radius {:doc "the bed radius once the body is overdue for sleep" :spec (a/num-in 0 nil) :default sh/urgent-bed-radius}
   :max-days-awake {:doc "in-game days without sleep before a bed in the open is slept in and the urgent radius applies" :spec (a/num-in 0 nil) :default sh/max-days-awake}})

(defn bed-permit
  "Whether the body may use a bed (sh/bed-permit over the job's world)."
  [c]
  (sh/bed-permit (:primitives c) (:world (:engine c))))

(defn radius [c] (sh/bed-radius (ctx/view c) (:args c)))

(defn overdue? [c]
  (let [days (sh/days-awake c)]
    (and (some? days) (>= days (:max-days-awake (:args c))))))

(defn bed-to-use
  "The :bed argument, else the bed sh/bed-to-use picks."
  [c]
  (or (some-> (:bed (:args c)) places/parse-pos :pos)
      (sh/bed-to-use (:primitives c) (ctx/view c) (radius c) (bed-permit c))))

(defn check
  "Night, and a bed to use or a bed item to put down; else false, with the reason the job waits."
  [c]
  (let [p (:primitives c)]
    (cond
      (not (sh/night? p)) (ctx/wait c :not-night)
      (or (bed-to-use c) (sh/bed-place-wanted? p (ctx/view c) (radius c) (bed-permit c))) true
      :else (ctx/wait c {:reason :no-bed :text "no bed to use and none carried to put down"}))))

(defn ^:async sleep!
  "jobs.survival.sleep on bed (open-ok?: also one not shut in a roofed room: a bed the body just put down, or an overdue body)."
  [c bed open-ok?]
  (let [p (:primitives c)
        started (ctx/now c)
        s (await (ctx/call-child c :sleep 'jobs.survival.sleep {:bed bed :open-bed open-ok?}))]
    (cond
      (= :continue s) :continue
      (= :bed-open (:reason (ctx/child-result c :sleep)))
      (do (ctx/remember! c :bed-open {:pos bed} sh/bed-open-policy)
          (ctx/emit! c :shelter.bed_open :info {:pos bed :text "bed is open, dug in beside it"})
          (result/finish! c {:slept false :bed-open true}))
      (or (sh/sleeping? p) (seq (ctx/since c :slept started))) (result/finish! c {:slept true :bed bed})
      (not (sh/night? p)) (result/finish! c {:slept false})
      :else (do (ctx/remember! c :sleep-failed {:pos bed} sh/sleep-failed-policy)
                (ctx/emit! c :shelter.sleep_failed :warn {:pos bed :text "could not sleep in the bed; not trying again for a while"})
                (result/finish! c {:slept false})))))

(def bed-dirs [[1 0] [-1 0] [0 1] [0 -1]])

(defn bed-layout
  "{:stand :foot :head} for a bed set down beside the body: three cells in a row, the body standing at one end (a placed
  bed faces away from the body), the foot next to it, the head after it. All three free (air, with a solid floor that is no container, bed or other kept block, and
  the body's headroom for the stand cell) within 3 cells of the body, the foot and head not in a doorway or another's
  zone, claim or plan footprint (a missing zone list refuses nothing), and roofed within roof-height when roofed? is
  set. The stand cell nearest the body first (its own cell when it fits); nil when no row fits."
  [c roofed?]
  (let [p (:primitives c)
        roof (:roof-height (:args c))
        {:keys [x y z]} (sh/feet p)
        in (access/rules-input c)
        air? #(#{"air" "cave_air"} (u/seen-name p %))
        free? (fn [pos] (and (air? pos) (sh/solid-at? p (update pos :y dec))
                             (nil? (tidy-rules/keep-why (u/seen-name p (update pos :y dec))))))
        covered? (fn [pos] (or (not roofed?) (some #(sh/solid-at? p (update pos :y + %)) (range 1 (inc roof)))))
        permitted? #(and (nil? (access/trespass-refusal in :place %)) (not (sh/doorway? p %)))
        stands (sort-by (fn [[dx dz]] (+ (js/Math.abs dx) (js/Math.abs dz)))
                        (for [dx (range -3 4) dz (range -3 4)] [dx dz]))]
    (first (for [[sx sz] stands
                 [dx dz] bed-dirs
                 :let [stand {:x (+ x sx) :y y :z (+ z sz)}
                       foot {:x (+ x sx dx) :y y :z (+ z sz dz)}
                       head {:x (+ x sx dx dx) :y y :z (+ z sz dz dz)}]
                 :when (and (or (zero? (+ (js/Math.abs sx) (js/Math.abs sz))) (free? stand))
                            (air? (update stand :y inc))
                            (free? foot) (free? head) (covered? foot) (covered? head)
                            (permitted? foot) (permitted? head)
                            (not= foot (sh/feet p)))]
             {:stand stand :foot foot :head head}))))

(defn click-for
  "The place click that sets a bed on the foot cell facing from the stand cell toward it, so the head cell is the one
  after the foot: against the floor under the foot, looking along the row."
  [{:keys [stand foot]}]
  (let [dx (- (:x foot) (:x stand))
        dz (- (:z foot) (:z stand))]
    {:against (update foot :y dec) :cursor {:x 0.5 :y 1 :z 0.5}
     :yaw (js/Math.atan2 (- dx) (- dz)) :pitch -1}))

(defn ^:async bed-visible?
  "Whether a bed block stands at pos: a block update can arrive a moment after the place act, so look a few times."
  [c pos]
  (loop [tries 0]
    (cond
      (.endsWith (or (u/seen-name (:primitives c) pos) "") "_bed") true
      (<= 4 tries) false
      :else (do (await (ctx/act c :wait #js {:ms 250}))
                (recur (inc tries))))))

(defn ^:async put-down-bed!
  "Walk to the layout's stand cell if the body is not on it, then place the carried bed on the foot cell (facing along
  the row) and record it as :bed unless a live :bed is recorded, and as :bed-placed when outside the body's own zones.
  :continue while walking; the foot cell when a bed block stands there after; else nil."
  [c]
  (let [p (:primitives c)
        item (sh/carried-bed p)
        {:keys [stand foot] :as layout} (await (sense/decide! c ::layout #(or (bed-layout c true) (bed-layout c false))))]
    (when foot
      (if (= stand (sh/feet p))
        (let [r (await (tidy/place! c foot item false (click-for layout)))]
          (when (and (= "placed" (.-status r)) (await (bed-visible? c foot)))
            (places/offer! c :bed foot)
            (let [own? (boolean (sh/in-own-zone? p (:world (:engine c)) foot))]
              (when-not own?
                (ctx/remember! c :bed-placed {:pos foot} sh/bed-placed-policy))
              (ctx/emit! c :night.bed-placed :info {:pos foot :own-zone own?
                                                    :text (str "put the carried bed down at " (pr-str foot) (if own? " (in its own zone: it stays)" ", picked up by day unless sleeping in it made it the respawn bed"))}))
            foot))
        (let [w (await (ctx/call-child c :bed-walk 'jobs.movement.go-to {:pos stand :range 0 :at-night :go}))]
          (when (or (= :continue w) (:arrived (ctx/child-result c :bed-walk))) :continue))))))

(defn ^:async place-bed-and-sleep!
  "A bed carried and none to use: set it down beside the body and sleep in it."
  [c]
  (let [r (await (put-down-bed! c))]
    (cond
      (= :continue r) :continue
      r (await (sleep! c r true))
      :else
      (do (ctx/remember! c :bed-place-failed {:pos (sh/feet (:primitives c))} sh/bed-place-failed-policy)
          (ctx/emit! c :shelter.bed_place_failed :warn {:text "no room or permission to put the carried bed down; not sleeping in it"})
          (result/finish! c {:slept false})))))

(defn ^:async round [c]
  (let [p (:primitives c)
        bed (or (some-> (:bed (:args c)) places/parse-pos :pos)
                (await (sense/decide! c ::bed #(sh/bed-to-use p (ctx/view c) (radius c) (bed-permit c)))))]
    (cond
      bed (await (sleep! c bed (overdue? c)))
      (sh/bed-place-wanted? p (ctx/view c) (radius c) (bed-permit c)) (await (place-bed-and-sleep! c))
      :else (result/finish! c {:slept false}))))
