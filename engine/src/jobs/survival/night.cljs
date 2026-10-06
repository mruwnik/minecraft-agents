(ns jobs.survival.night
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.shelter :as sh]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.places :as places]
            [triggers.survival.hungry :as hungry]
            [triggers.survival.night :as night]
            [jobs.survival.dig-in :as dig-in]
            [jobs.survival.eat :as eat]))

(def doc
  "The night: the job owns it, so the normal job loop gets the body back only at day or once the body is safe.
  Check: the night trigger's condition, or job memory :sheltered (it holds a sleeper or a dug-in body until day).
  Each round, the first that applies:
  0. day: after a dig-in or an exposed night, dig-in/leave! gets the body out (:out ends it; :no-way-out ends it failed
     with a shelter.failed warn, a result {:status :failed :reason :no-way-out :at :tries} and a :shelter-trapped
     entry, 5 minutes; :unsafe waits hold-ms); then a bed it put down outside its own zones (:bed-placed) is dug up
     and its drop walked over; then :done.
  1. asleep, or :sheltered and roofed: hold (wait hold-ms, eat one carried food when the hungry trigger's condition
     holds: hungry? or below 7 hp). A dug-in or exposed hold logs out when someone else falls asleep (step 3).
     A first round shut in its own latest :shelter holds as dug in.
  2. a bed (sh/bed-to-use: seen, or remembered within :bed-radius, :urgent-bed-radius once :max-days-awake days without
     sleep, under any roof or none; never an occupied one or one given up on): jobs.survival.sleep on it. A sleep that
     ends without sleeping writes :sleep-failed (5 minutes, shelter.sleep_failed warn); rounds (exposed holds too) look again
     and try a bed once it expires.
     A bed item carried and no bed to use: put down beside the body (roofed cells first, else in the open; never in a
     doorway or another's zone), slept in, recorded as :bed unless a live :bed is; outside its own zones also as
     :bed-placed for the morning. No room writes :bed-place-failed (10 minutes, or until the body is 6+ blocks from there).
  3. someone else asleep (sh/log-out-for-sleepers?: the action bar's sleep count tonight, a sleeper in sight, or no
     count since the body's return): jobs.survival.log-out, a 30 s stint at most until morning, again each round
     while anyone sleeps or nothing is known; until morning. A log-out that is not ok or cut is not tried again.
  4. roofed or buried: :done (the queue runs). Otherwise jobs.survival.dig-in. When it refused because the ground is
     hollow, wet or has nothing to place against, the body walks (go-to) to the nearest cell within 16 blocks, 9+ from
     every failed site, with solid ground 3 deep (shelter.relocated info), at most 3 times a night. Nothing roofs it: held exposed
     (:sheltered :exposed, one shelter.exposed warn), each round choosing again.
  An overdue body (no sleep for :max-days-awake in-game days) warns needs_bed once an in-game day (:needs-bed).
  Job memory: :sheltered (:slept, :dug-in, :exposed), :log-out-failed and leave!'s :dig-out.")

(def args
  {:roof-height {:doc "a solid block within this many blocks above counts as a roof" :default sh/default-roof-height}
   :bed-radius {:doc "a remembered bed farther than this is not used" :default sh/default-bed-radius}
   :urgent-bed-radius {:doc "the bed radius once the body is overdue for sleep" :default sh/urgent-bed-radius}
   :max-days-awake {:doc "in-game days without sleep before finding a bed becomes urgent" :default sh/max-days-awake}})

(defn bed-permit
  "Whether the body may use a bed (sh/bed-permit over the job's world and clock)."
  [c]
  (sh/bed-permit (:primitives c) (:world (:engine c)) (ctx/now c)))

(defn check
  "The night trigger's condition, or job memory :sheltered."
  [c]
  (or (some? (:sheltered (ctx/mem c)))
      (night/holds? (:primitives c) (ctx/view c) (:args c) (bed-permit c))))

(defn radius [c] (sh/bed-radius (ctx/view c) (:args c)))

(defn overdue? [c]
  (let [days (sh/days-awake c)]
    (and (some? days) (>= days (:max-days-awake (:args c))))))

(def needs-bed-policy {:cap 1 :ttl sh/ms-per-day})

(defn note-needs-bed! [c]
  (when (empty? (ctx/entries c :needs-bed))
    (ctx/remember! c :needs-bed {} needs-bed-policy)
    (ctx/emit! c :needs_bed :warn {:days (sh/days-awake c)
                                   :text "not slept for too long; phantoms will come, find or make a bed"})))

(def hold-ms
  "How long one holding round waits (the wait primitive does not wake a sleeping body); a higher reflex cuts it."
  5000)

(defn ^:async eat-if-hungry!
  "The hungry reflex (below night in the shipped registers) cannot reach a held body, so a hold round eats one carried
  food (jobs.survival.eat's choice) when the hungry trigger's condition holds (hungry?, or eat-now? below 7 hp).
  A sleeping body does not eat."
  [c]
  (let [p (:primitives c)
        self (.self p)
        health (.-health self)
        best (when (and (not (sh/sleeping? p)) (or (hungry/hungry? (.-food self) health {}) (hungry/eat-now? self {})))
               (eat/best-food (u/inventory p) false nil health))]
    (when best
      (await (ctx/act c :equip #js {:item best}))
      (let [r (await (ctx/act c :eat #js {:item best}))]
        (when (= "ate" (.-status r))
          (ctx/emit! c :shelter.ate :info {:item best :food (.-food r) :text (str "ate " best " while holding the night")}))))))

(defn ^:async hold [c]
  (await (eat-if-hungry! c))
  (await (ctx/act c :wait #js {:ms hold-ms}))
  :continue)

(defn sheltered! [c how] (ctx/update-mem! c assoc :sheltered how))

(defn ^:async dig-in-step [c]
  (let [p (:primitives c)
        d (await (ctx/call-child c :dig-in 'jobs.survival.dig-in {:roof-height (:roof-height (:args c))}))]
    (cond
      (= :continue d) :continue
      (and (= :done d) (sh/roofed? p (:roof-height (:args c)))) (do (sheltered! c :dug-in) :continue)
      :else :declined)))

(defn log-out-wanted? [c]
  (and (not (:log-out-failed (ctx/mem c)))
       (sh/log-out-for-sleepers? (:primitives c) (ctx/view c))))

(defn ^:async log-out-step
  "Someone sleeps: a log-out stint. Back by day: :done. Ok or cut at night: the next round looks again (another stint while anyone sleeps or nothing
  is known). Declined or failed (unsupported, closed): :log-out-failed, not tried again by this job."
  [c]
  (let [l (await (ctx/call-child c :log-out 'jobs.survival.log-out {}))
        status (:status (ctx/child-result c :log-out))]
    (cond
      (= :continue l) :continue
      (and (= :done l) (#{"ok" "cut"} status)) (if (sh/night? (:primitives c)) :continue :done)
      :else (do (ctx/update-mem! c assoc :log-out-failed true) :continue))))

(defn ^:async sleep-step
  "jobs.survival.sleep on bed. :continue while it works; asleep it is :sheltered :slept and holds; a sleep that ends
  without sleeping marks :sleep-failed (a body-memory entry, 5 minutes) and the next round chooses again."
  [c bed]
  (let [p (:primitives c)
        started (ctx/now c)
        s (await (ctx/call-child c :sleep 'jobs.survival.sleep {:bed bed}))]
    (cond
      (= :continue s) :continue
      (or (sh/sleeping? p) (seq (ctx/since c :slept started))) (do (sheltered! c :slept) :continue)
      (not (sh/night? p)) :done
      :else (do (ctx/remember! c :sleep-failed {:pos bed} sh/sleep-failed-policy)
                (ctx/emit! c :shelter.sleep_failed :warn {:pos bed :text "could not sleep in the bed; not trying again for a while"})
                :continue))))

(def bed-dirs [[1 0] [-1 0] [0 1] [0 -1]])

(defn bed-layout
  "{:stand :foot :head} for a bed set down beside the body: three cells in a row, the body standing at one end (a placed
  bed faces away from the body), the foot next to it, the head after it. All three free (air, with a solid floor and
  the body's headroom for the stand cell) within 3 cells of the body, the foot and head not in a doorway or another's
  zone, claim or plan footprint (a missing zone list refuses nothing), and roofed within roof-height when roofed? is
  set. The stand cell nearest the body first (its own cell when it fits); nil when no row fits."
  [c roofed?]
  (let [p (:primitives c)
        roof (:roof-height (:args c))
        {:keys [x y z]} (sh/feet p)
        in (access/rules-input c)
        air? #(#{"air" "cave_air"} (u/block-name p %))
        free? (fn [pos] (and (air? pos) (sh/solid-at? p (update pos :y dec))))
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
      (.endsWith (or (u/block-name (:primitives c) pos) "") "_bed") true
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
        {:keys [stand foot] :as layout} (or (bed-layout c true) (bed-layout c false))]
    (when foot
      (if (= stand (sh/feet p))
        (let [r (await (tidy/place! c foot item false (click-for layout)))]
          (when (and (= "placed" (.-status r)) (await (bed-visible? c foot)))
            (places/offer! c :bed foot)
            (when-not (sh/in-own-zone? p (:world (:engine c)) foot)
              (ctx/remember! c :bed-placed {:pos foot} sh/bed-placed-policy))
            foot))
        (let [w (await (ctx/call-child c :bed-walk 'jobs.movement.go-to {:pos stand :range 0}))]
          (when (or (= :continue w) (:arrived (ctx/child-result c :bed-walk))) :continue))))))

(defn ^:async place-bed-and-sleep
  "A bed carried and none to use: set it down beside the body and sleep in it. No room, or a failed placement, writes a
  :bed-place-failed entry (10 minutes) and the next round chooses again."
  [c]
  (let [r (await (put-down-bed! c))]
    (cond
      (= :continue r) :continue
      r (await (sleep-step c r))
      :else
      (do (ctx/remember! c :bed-place-failed {:pos (sh/feet (:primitives c))} sh/bed-place-failed-policy)
          (ctx/emit! c :shelter.bed_place_failed :warn {:text "no room or permission to put the carried bed down; not sleeping in it"})
          :continue))))

(def relocate-reasons
  "dig-in's :futile reasons a better spot nearby can cure."
  #{:no-floor :hazard-below :fluid-adjacent :no-roof-support})

(def max-relocations "Spots a night tries before holding where it is." 3)

(def relocate-reach "How far (blocks) a night looks for a spot to dig in." 16)

(defn pit-site?
  "Whether feet cell f is a place a pit can be dug: standing room, solid ground 3 deep, no fluid beside the ground."
  [p {:keys [x y z] :as f}]
  (let [at (fn [dy] (u/block-name p {:x x :y (+ y dy) :z z}))]
    (and (not (sh/solid? (at 0))) (not (sh/solid? (at 1)))
         (not (dig-in/hazards (at 0))) (not (dig-in/hazards (at -1)))
         (sh/solid? (at -1)) (sh/solid? (at -2)) (sh/solid? (at -3))
         (not (dig-in/lateral-fluid p {:x x :y (dec y) :z z}))
         (not (dig-in/lateral-fluid p {:x x :y (- y 2) :z z})))))

(defn relocation-site
  "The nearest feet cell within relocate-reach, at least futile-radius+1 from every failed site, where a pit can be
  dug (pit-site?), or nil."
  [c]
  (let [p (:primitives c)
        {:keys [x y z]} (sh/feet p)
        failed (map (comp :pos :data) (ctx/entries c :dig-in-futile))
        far? (fn [f] (every? #(> (u/dist % f) dig-in/futile-radius) failed))
        offsets (sort-by (fn [[dx dz]] (+ (* dx dx) (* dz dz)))
                         (for [dx (range (- relocate-reach) (inc relocate-reach))
                               dz (range (- relocate-reach) (inc relocate-reach))
                               :when (<= (+ (* dx dx) (* dz dz)) (* relocate-reach relocate-reach))]
                           [dx dz]))]
    (some (fn [[dx dz]]
            (some (fn [dy] (let [f {:x (+ x dx) :y (+ y dy) :z (+ z dz)}]
                             (when (and (far? f) (pit-site? p f)) f)))
                  [0 -1 1 -2 2]))
          offsets)))

(defn relocatable?
  "Whether dig-in's refusal (a decline map from ctx/wait) names a cure: the ground here is hollow or wet."
  [c]
  (and (< (count (:relocations (ctx/mem c))) max-relocations)
       (some #(relocate-reasons (:reason (:data %))) (ctx/entries c :dig-in-futile))))

(defn ^:async relocate!
  "Walk (go-to) to a nearby cell where a pit can be dug, so dig-in runs again from there. :continue while walking; nil
  when there is no such cell or the walk did not arrive (the caller holds exposed)."
  [c]
  (let [site (or (:relocating (ctx/mem c)) (relocation-site c))]
    (when site
      (ctx/update-mem! c assoc :relocating site)
      (let [w (await (ctx/call-child c :relocate 'jobs.movement.go-to {:pos site :range 0}))]
        (cond
          (= :continue w) :continue
          :else (do (ctx/update-mem! c dissoc :relocating)
                    (ctx/update-mem! c update :relocations (fnil conj []) site)
                    (when (:arrived (ctx/child-result c :relocate))
                      (ctx/emit! c :shelter.relocated :info {:pos site :text "the ground here is hollow; moved to dig in elsewhere"})
                      :continue)))))))

(defn ^:async hold-exposed
  "Night, and nothing could shelter the body: hold it anyway until day. The first time, :sheltered :exposed and one
  shelter.exposed warn; every round waits hold-ms, and the next round chooses again."
  [c]
  (when-not (= :exposed (:sheltered (ctx/mem c)))
    (sheltered! c :exposed)
    (ctx/emit! c :shelter.exposed :warn {:pos (sh/feet (:primitives c))
                                         :text "cannot shelter here (no bed, nobody asleep, dig-in cannot roof); holding until day"}))
  (hold c))

(defn ^:async collect-bed!
  "By day: dig up the bed at pos the night put down outside the body's zones, then walk over its drop. One try: the
  :bed-placed entry is dropped whatever happens, and a :bed entry at pos is retracted."
  [c pos]
  (let [walk (await (ctx/call-child c :bed-collect 'jobs.movement.go-to {:pos pos :range 2}))]
    (if (= :continue walk)
      :continue
      (do (when (:arrived (ctx/child-result c :bed-collect))
            (let [r (await (tidy/dig! c pos))]
              (when (= "dug" (.-status r))
                (when (= pos (mem/place (ctx/view c) :bed))
                  (ctx/remember! c :bed {:gone true :was pos} mem/place-policy))
                (await (ctx/act c :moveTo (clj->js {:pos pos :range 0}))))))
          (ctx/forget-where! c :bed-placed (constantly true))
          :continue))))

(defn ^:async day-round
  "By day: out of a dig-in pit first (dig-in/leave!), then a bed it put down outside its zones is picked up, then :done."
  [c]
  (if (or (#{:dug-in :exposed} (:sheltered (ctx/mem c))) (:dig-out (ctx/mem c)) (dig-in/sheltered-in c))
    (let [r (await (dig-in/leave! c))]
      (cond
        (= :continue r) :continue
        (= :out (:reason r)) (if (sh/bed-to-collect (:primitives c) (ctx/view c))
                               (do (ctx/update-mem! c dissoc :sheltered :dig-out) :continue)
                               :done)
        (= :no-way-out (:reason r))
        (let [result {:status :failed :reason :no-way-out :at (:at r) :tries (:tries r)}]
          (ctx/emit! c :shelter.failed :warn (assoc result :text "no way out of the shelter by day; giving the body back"))
          (ctx/remember! c :shelter-trapped {:pos (sh/feet (:primitives c))} sh/trapped-policy)
          (ctx/result! c result)
          :done)
        :else (await (hold c))))
    (if-let [bed (sh/bed-to-collect (:primitives c) (ctx/view c))]
      (await (collect-bed! c bed))
      :done)))

(defn ^:async round [c]
  (let [p (:primitives c)
        roofed (sh/roofed? p (:roof-height (:args c)))
        sheltered (:sheltered (ctx/mem c))]
    (when (and (sh/night? p) (overdue? c)) (note-needs-bed! c))
    (cond
      (not (sh/night? p)) (await (day-round c))
      (sh/sleeping? p) (do (when-not sheltered (sheltered! c :slept)) (await (hold c)))
      (and roofed (empty? (:children (ctx/mem c))) (dig-in/sheltered-in c)) (do (sheltered! c :dug-in) (await (hold c)))
      (and roofed sheltered) (if (log-out-wanted? c) (await (log-out-step c)) (await (hold c)))
      :else
      (let [bed (sh/bed-to-use p (ctx/view c) (radius c) (bed-permit c))]
        (cond
          bed (await (sleep-step c bed))
          (sh/bed-place-wanted? p (ctx/view c) (radius c) (bed-permit c))
          (await (place-bed-and-sleep c))
          (log-out-wanted? c) (await (log-out-step c))
          (or roofed (sh/buried? p)) :done
          :else (let [r (await (dig-in-step c))]
                  (if (= :declined r)
                    (or (when (relocatable? c) (await (relocate! c)))
                        (await (hold-exposed c)))
                    r)))))))
