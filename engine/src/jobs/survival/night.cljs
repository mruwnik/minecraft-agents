(ns jobs.survival.night
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.child :as child]
            [jobs.lib.result :as result]
            [jobs.lib.shelter :as sh]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.places :as places]
            [triggers.survival.night :as night]
            [triggers.survival.hungry :as hungry]
            [jobs.survival.dig-in :as dig-in]
            [jobs.survival.eat :as eat]))

(def doc
  "The night: one round from the trigger until the night is over and the body is out of its shelter. The round repeats
  one choice, each time from the world as it is now (the first that applies), with a 50 ms timer between:
  - day: a dug-in or exposed night leaves the shelter (dig-in/leave!); :no-way-out stops the job (shelter.failed warn,
    a :shelter-trapped entry, 5 minutes); then a bed it put down outside its own zones (:bed-placed) is dug up and its
    drop walked over (go-to); then the job ends: done {:night :slept|:dug-in|:logged-out}, or stopped :exposed {:sites} after a
    night with no shelter.
  - asleep: hold (:sleeping).
  - roofed and sheltered (slept, dug in, or shut in its own latest :shelter): a log-out stint while someone else sleeps,
    else hold (:sheltered).
  - a bed (sh/bed-to-use: seen, or remembered within :bed-radius, :urgent-bed-radius once :max-days-awake days without
    sleep; never an occupied one or one given up on): jobs.survival.sleep on it. A sleep that ends without sleeping writes
    :sleep-failed (5 minutes, shelter.sleep_failed warn).
  - a bed item carried and no bed to use: put down beside the body (roofed cells first, else in the open; never in a
    doorway or another's zone), slept in, recorded as :bed unless a live :bed is; outside its own zones also as
    :bed-placed for the morning. No room writes :bed-place-failed (10 minutes, or until the body is 6+ blocks away).
  - someone else asleep (sh/log-out-for-sleepers?): jobs.survival.log-out, a 30 s stint at most until morning, again
    while anyone sleeps or nothing is known. A log-out that is not ok or cut is not tried again tonight.
  - roofed or buried: done {:night :roofed} (the queue runs).
  - else, before any pit and while no site has failed tonight: a remembered roofed place (:roofed-places, nearest first)
    within :walk-radius whose roof the body has seen, and whose route (the straight line, sampled every 2 blocks) is lit
    as far as the body has seen it (block light >= :lit-light; an unseen cell is not lit): walk there (go-to :place). A walk
    that does not arrive is not tried again tonight, and the night digs in; arrived, the next pass is roofed.
  - else a safe place: jobs.survival.dig-in here, unless a site tonight failed within 8 blocks. A dig-in that does not
    roof the body (stopped, or declined) writes a :night-site {:pos :reason} entry, and the body walks (go-to) to the
    nearest cell within 16 blocks, 9+ from every failed site, that looks dry and solid from the surface (shelter.relocated info; dig-in finds out the
    rest), to dig in there. After max-sites failed sites, or with no such cell, it cuts a niche (jobs.survival.dig-niche, once, :niche; out of a failed pit first), else holds exposed until day (one shelter.exposed
    warn, hold :exposed), still taking a bed or a sleeper's log-out when one turns up. A body held exposed digs in again (failed sites
    forgotten) when it has moved off its spot or retry-after-ms passed, at most max-retries times.
  Every hold is declared (ctx/hold-still!) and waits hold-ms at a time, eating one carried food when the hungry
  trigger's condition holds (hungry?, or below 7 hp; not asleep).
  An overdue body (no sleep for :max-days-awake in-game days) warns needs_bed once an in-game day (:needs-bed).
  Memory: job memory :sheltered (:slept, :dug-in, :logged-out, :exposed), :log-out-failed, :relocating, :digging (the
  site of a dig-in in flight), :pit-trapped and leave!'s :dig-out; body memory :night-site (failed sites, forgotten whenever the night ends, at most a day), so a
  firing after a cut does not dig a failed site again, :roof-walk-failed.
  Muting :night does not end a running night job: the agent cancels it too.")

(def args
  {:roof-height {:doc "a solid block within this many blocks above counts as a roof" :default sh/default-roof-height}
   :bed-radius {:doc "a remembered bed farther than this is not used" :default sh/default-bed-radius}
   :urgent-bed-radius {:doc "the bed radius once the body is overdue for sleep" :default sh/urgent-bed-radius}
   :roofed-places {:doc "place names walked to at night, when close and the route is lit, before digging in" :default [:home]}
   :walk-radius {:doc "a roofed place farther than this is not walked to" :default 32}
   :lit-light {:doc "block light a route cell needs to count as lit (mobs do not spawn at 1+)" :default 1}
   :max-days-awake {:doc "in-game days without sleep before finding a bed becomes urgent" :default sh/max-days-awake}})

(defn bed-permit
  "Whether the body may use a bed (sh/bed-permit over the job's world and clock)."
  [c]
  (sh/bed-permit (:primitives c) (:world (:engine c)) (ctx/now c)))

(defn check
  "The night trigger's condition, or job memory :sheltered (a listed night cut before its morning)."
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
  "How long one hold waits before the night looks again (the wait primitive does not wake a sleeping body)."
  5000)

(defn ^:async eat-if-hungry!
  "The hungry reflex (below night in the shipped registers) cannot reach a held body, so a hold eats one carried
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

(defn ^:async hold!
  "Hold the body still on purpose (reason) for hold-ms, eating first when hungry; :again."
  [c reason]
  (ctx/hold-still! c reason)
  (await (eat-if-hungry! c))
  (await (ctx/act c :wait #js {:ms hold-ms}))
  :again)

(defn busy!
  "The night acts again (walks, digs, logs out): no longer holding still."
  [c]
  (ctx/hold-still! c nil))

(defn sheltered! [c how] (ctx/update-mem! c assoc :sheltered how))

(defn log-out-wanted? [c]
  (and (not (:log-out-failed (ctx/mem c)))
       (sh/log-out-for-sleepers? (:primitives c) (ctx/view c))))

(defn ^:async log-out-step
  "Someone sleeps: a log-out stint (jobs.survival.log-out). Ok or cut: :logged-out unless already sheltered, and the
  next pass looks again. Declined or failed (unsupported, closed): :log-out-failed, not tried again by this job."
  [c]
  (busy! c)
  (let [l (await (ctx/call-child c :log-out 'jobs.survival.log-out {}))
        status (:status (ctx/child-result c :log-out))]
    (cond
      (= :continue l) :again
      (and (= :done l) (#{"ok" "cut"} status)) (do (when-not (:sheltered (ctx/mem c)) (sheltered! c :logged-out)) :again)
      :else (do (ctx/update-mem! c assoc :log-out-failed true) :again))))

(defn ^:async sleep-step
  "jobs.survival.sleep on bed. Asleep it is :sheltered :slept; a sleep that ends without sleeping marks :sleep-failed
  (a body-memory entry, 5 minutes) and the next pass chooses again."
  [c bed]
  (busy! c)
  (let [p (:primitives c)
        started (ctx/now c)
        s (await (ctx/call-child c :sleep 'jobs.survival.sleep {:bed bed}))]
    (cond
      (= :continue s) :again
      (or (sh/sleeping? p) (seq (ctx/since c :slept started))) (do (sheltered! c :slept) :again)
      (not (sh/night? p)) :again
      :else (do (ctx/remember! c :sleep-failed {:pos bed} sh/sleep-failed-policy)
                (ctx/emit! c :shelter.sleep_failed :warn {:pos bed :text "could not sleep in the bed; not trying again for a while"})
                :again))))

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
  :bed-place-failed entry (10 minutes) and the next pass chooses again."
  [c]
  (busy! c)
  (let [r (await (put-down-bed! c))]
    (cond
      (= :continue r) :again
      r (await (sleep-step c r))
      :else
      (do (ctx/remember! c :bed-place-failed {:pos (sh/feet (:primitives c))} sh/bed-place-failed-policy)
          (ctx/emit! c :shelter.bed_place_failed :warn {:text "no room or permission to put the carried bed down; not sleeping in it"})
          :again))))

;; ------------------------------------------------------------------ a safe place

(def site-policy
  "The :night-site entries: failed sites, cleared at the morning the night job sees; a day at most."
  {:cap 8 :ttl sh/ms-per-day})

(def max-sites "Failed sites a night tries before holding exposed." 4)

(def relocate-reach "How far (blocks) a night looks for a spot to dig in." 16)

(defn failed-sites
  "Tonight's failed sites [{:pos :reason}]."
  [c]
  (mapv :data (ctx/entries c :night-site)))

(defn site-failed!
  "Remember that the site at pos failed tonight, and why."
  [c pos reason]
  (ctx/remember! c :night-site {:pos pos :reason reason} site-policy))

(defn failed-here?
  "Whether a site that failed tonight lies within dig-in's futile radius of the feet."
  [c]
  (let [here (sh/feet (:primitives c))]
    (boolean (some #(<= (u/dist here %) dig-in/futile-radius)
                   (map :pos (failed-sites c))))))

(defn pit-site?
  "Whether feet cell f looks like a place a pit can be dug, by what a player sees from the surface: standing room, dry
  ground, no fluid beside it. How deep the ground goes is left to dig-in, which stops a bad site."
  [p {:keys [x y z] :as f}]
  (let [at (fn [dy] (u/block-name p {:x x :y (+ y dy) :z z}))
        below {:x x :y (dec y) :z z}]
    (and (sh/solid? (at -1)) (not (sh/solid? (at 0))) (not (sh/solid? (at 1)))
         (not (dig-in/wet? p f)) (not (dig-in/wet? p {:x x :y (inc y) :z z}))
         (not (dig-in/wet? p below))
         (not (dig-in/lateral-fluid p below)))))

(defn relocation-site
  "The nearest feet cell within relocate-reach of the failed site the body is at (its feet when none: a body in its
  pit looks from the surface), at least futile-radius+1 from every failed site (tonight's and dig-in's
  :dig-in-futile entries), where a pit can be dug (pit-site?), or nil."
  [c]
  (let [p (:primitives c)
        feet (sh/feet p)
        here (first (filter #(<= (u/dist feet %) dig-in/futile-radius) (map :pos (failed-sites c))))
        {:keys [x y z]} (or here feet)
        failed (concat (map :pos (failed-sites c)) (map (comp :pos :data) (ctx/entries c :dig-in-futile)))
        far? (fn [f] (every? #(>= (u/dist % f) (inc dig-in/futile-radius)) failed))
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


(defn route-lit?
  "Whether every sample (every 2 blocks along the straight line from a to b, over the cells around that height) has some
  seen cell with block light >= min-light. A cell the body has not seen (no light data) is not lit."
  [p a b min-light]
  (let [raw (some-> (aget p "perception") :raw)
        n (max 1 (js/Math.ceil (/ (u/dist a b) 2)))
        lit-at (fn [x y z] (and (<= 0 (.stateAt ^js raw x y z))
                                (<= min-light (bit-and (.lightAt ^js raw x y z) 15))))]
    (boolean
     (and raw
          (every? (fn [i]
                    (let [t (/ i n)
                          at (fn [k] (js/Math.floor (+ (k a) (* t (- (k b) (k a))))))]
                      (some #(lit-at (at :x) (+ (at :y) %) (at :z)) [-1 0 1 2])))
                  (range 0 (inc n)))))))

(defn roofed-place
  "The nearest remembered place of :roofed-places within :walk-radius, roofed by blocks the body has seen (within
  :roof-height) and reached over a lit route, as {:name :pos}, or nil."
  [c]
  (let [p (:primitives c)
        {:keys [roofed-places walk-radius roof-height lit-light]} (:args c)
        here (sh/feet p)
        roofed? (fn [pos] (some #(sh/solid-at? p (update pos :y + %)) (range 1 (inc roof-height))))]
    (->> roofed-places
         (keep (fn [nm] (when-let [pos (mem/place (ctx/view c) nm)] {:name nm :pos pos})))
         (filter (fn [{:keys [pos]}] (and (<= (u/dist here pos) walk-radius) (roofed? pos) (route-lit? p here pos lit-light))))
         (sort-by #(u/dist here (:pos %)))
         first)))

(defn roof-walk-wanted?
  "Whether to walk to a roofed place before digging in: none failed tonight, no dig-in in flight, no walk failed."
  [c]
  (let [m (ctx/mem c)]
    (and (not (:roof-walk-failed m)) (not (:digging m)) (empty? (failed-sites c)))))

(defn ^:async roof-walk!
  "Walk (go-to :place) to the roofed place; :again. A walk that ends without arriving is marked :roof-walk-failed."
  [c {:keys [name]}]
  (busy! c)
  (let [w (await (ctx/call-child c :roof-walk 'jobs.movement.go-to {:place name :range 0}))]
    (when (and (not= :continue w) (not (:arrived (ctx/child-result c :roof-walk))))
      (ctx/update-mem! c assoc :roof-walk-failed true))
    :again))

(defn ^:async hold-exposed!
  "Nothing shelters the body tonight: hold it anyway until day (:exposed; the first time one shelter.exposed warn).
  Each pass still looks for a bed or a sleeper first."
  [c]
  (when-not (= :exposed (:sheltered (ctx/mem c)))
    (sheltered! c :exposed)
    (ctx/update-mem! c update :exposed #(or % {:pos (sh/feet (:primitives c)) :at (ctx/now c)}))
    (ctx/emit! c :shelter.exposed :warn {:pos (sh/feet (:primitives c)) :sites (failed-sites c)
                                         :text "cannot shelter here (no bed, nobody asleep, no site to dig in); holding until day"}))
  (await (hold! c :exposed)))

(defn pit-start
  "The failed site tonight whose pit the body stands in (below it, within the futile radius), or nil."
  [c]
  (let [feet (sh/feet (:primitives c))]
    (first (filter #(and (< (:y feet) (:y %)) (<= (u/dist feet %) dig-in/futile-radius)) (map :pos (failed-sites c))))))

(def retry-after-ms "How long a body holds exposed before it tries to dig in again (past dig-in's own 10-minute give-up)." 660000)

(def max-retries "Dig-in retries after a night went exposed." 2)

(defn retry-due?
  "Whether the exposed hold digs in again: under max-retries, and the body has moved off the spot it went exposed on
  or retry-after-ms passed."
  [c]
  (let [{:keys [pos at retries]} (:exposed (ctx/mem c))]
    (and pos (< (or retries 0) max-retries)
         (or (>= (u/dist (sh/feet (:primitives c)) pos) (inc dig-in/futile-radius))
             (>= (- (ctx/now c) at) retry-after-ms)))))

(defn retry-dig-in!
  "Start over from the exposed hold: failed sites and give-ups forgotten, one retry counted."
  [c]
  (let [p (:primitives c)
        retries (inc (or (:retries (:exposed (ctx/mem c))) 0))]
    (ctx/forget-where! c :night-site (constantly true))
    (ctx/update-mem! c #(-> % (dissoc :relocating :pit-trapped :niche)
                            (assoc :exposed {:pos (sh/feet p) :at (ctx/now c) :retries retries})))))

(defn ^:async niche!
  "No pit site is left: out of the failed pit first (dig-in/climb!, a stair), then jobs.survival.dig-niche cuts a niche
  into a hillside or wall. Whatever it ends in, it is tried once (:niche :tried); the next pass holds exposed if it
  did not shut the body in."
  [c pit]
  (busy! c)
  (if pit
    (let [r (await (dig-in/climb! c {:start pit} nil))]
      (when (map? r) (ctx/update-mem! c assoc :pit-trapped true))
      :again)
    (let [d (await (ctx/call-child c :niche 'jobs.survival.dig-niche {}))]
      (when-not (= :continue d) (ctx/update-mem! c assoc :niche :tried))
      :again)))

(defn ^:async relocate!
  "Get to a nearby cell where a pit can be dug; the next pass digs in there. Out of its own failed pit first by a stair
  (dig-in/climb!; one that finds no way out holds exposed), then a walk (go-to). A walk that does not arrive marks that
  cell failed (:unreachable). With max-sites failed sites, or no such cell: hold exposed."
  [c]
  (let [site (or (:relocating (ctx/mem c))
                 (when (< (count (failed-sites c)) max-sites) (relocation-site c)))
        pit (pit-start c)]
    (cond
      (:pit-trapped (ctx/mem c)) (await (hold-exposed! c))
      (and (nil? site) (not= :tried (:niche (ctx/mem c)))) (await (niche! c pit))
      (nil? site) (await (hold-exposed! c))
      pit (do (busy! c)
              (ctx/update-mem! c assoc :relocating site)
              (let [r (await (dig-in/climb! c {:start pit} site))]
                (when (map? r) (ctx/update-mem! c assoc :pit-trapped true))
                :again))
      :else
      (do (busy! c)
          (ctx/update-mem! c #(-> % (assoc :relocating site) (dissoc :dig-out)))
          (let [w (await (ctx/call-child c :relocate 'jobs.movement.go-to {:pos site :range 0}))]
            (if (= :continue w)
              :again
              (do (ctx/update-mem! c dissoc :relocating)
                  (if (:arrived (ctx/child-result c :relocate))
                    (ctx/emit! c :shelter.relocated :info {:pos site :sites (failed-sites c)
                                                           :text "the last site failed; moved to dig in elsewhere"})
                    (site-failed! c site :unreachable))
                  :again)))))))

(defn ^:async shelter!
  "No bed, nobody asleep, not roofed: dig in here (jobs.survival.dig-in, one call), unless a site failed here tonight.
  The site is written :interrupted before the call (a firing after a cut mid-pit does not dig it deeper; a listed night
  resumes its dig-in child instead), then forgotten when the body is roofed, else marked with dig-in's stop or wait
  reason."
  [c]
  (let [resume (:digging (ctx/mem c))]
    (if (and (failed-here? c) (not resume))
      (await (relocate! c))
      (let [p (:primitives c)
            start (or resume (sh/feet p))
            _ (busy! c)
            _ (when-not resume
                (ctx/update-mem! c assoc :digging start)
                (site-failed! c start :interrupted))
            d (await (ctx/call-child c :dig-in 'jobs.survival.dig-in {:roof-height (:roof-height (:args c))}))
            res (ctx/child-result c :dig-in)]
        (if (= :continue d)
          :again
          (do (ctx/update-mem! c dissoc :digging)
              (ctx/forget-where! c :night-site #(= {:pos start :reason :interrupted} %))
              (if (and (= :done d) (sh/roofed? p (:roof-height (:args c))) (not= :stopped (:status res)))
                (sheltered! c :dug-in)
                (site-failed! c start (or (:reason res) (if (= :declined d) :declined :unsealed))))
              :again))))))

;; ------------------------------------------------------------------ morning

(defn ^:async collect-bed!
  "By day: dig up the bed at pos the night put down outside the body's zones, then walk over its drop. One try: the
  :bed-placed entry is dropped whatever happens, and a :bed entry at pos is retracted."
  [c pos]
  (let [walk (await (ctx/call-child c :bed-collect 'jobs.movement.go-to {:pos pos :range 2}))]
    (if (= :continue walk)
      :again
      (do (when (:arrived (ctx/child-result c :bed-collect))
            (let [r (await (tidy/dig! c pos))]
              (when (= "dug" (.-status r))
                (when (= pos (mem/place (ctx/view c) :bed))
                  (ctx/remember! c :bed {:gone true :was pos} mem/place-policy))
                (ctx/update-mem! c assoc :pickup pos))))
          (ctx/forget-where! c :bed-placed (constantly true))
          :again))))

(defn ^:async pick-up-drop!
  "Walk over the dug bed's drop at pos (go-to); one try, then the walk is over."
  [c pos]
  (let [w (await (ctx/call-child c :bed-pickup 'jobs.movement.go-to {:pos pos :range 0}))]
    (when-not (= :continue w) (ctx/update-mem! c dissoc :pickup))
    :again))

(defn end-night!
  "The night is over: forget tonight's failed sites and end, done {:night how}, or stopped :exposed {:sites} when
  nothing sheltered the body."
  [c]
  (let [how (:sheltered (ctx/mem c))
        sites (failed-sites c)]
    (ctx/forget-where! c :night-site (constantly true))
    (if (= :exposed how)
      (result/stop! c :exposed "no shelter tonight: no bed, nobody asleep, no site to dig in" :sites sites)
      (result/finish! c (cond-> {} how (assoc :night how))))))

(defn ^:async morning-step!
  "By day, nothing to walk over: out of a dig-in pit first, then a bed picked up, then the end."
  [c]
  (if (or (#{:dug-in :exposed} (:sheltered (ctx/mem c))) (:dig-out (ctx/mem c)) (dig-in/sheltered-in c))
    (let [r (await (dig-in/leave! c))]
      (cond
        (= :continue r) :again
        (= :out (:reason r)) (do (ctx/update-mem! c dissoc :dig-out)
                                 (when-not (= :exposed (:sheltered (ctx/mem c))) (sheltered! c :dug-in))
                                 (if (sh/bed-to-collect (:primitives c) (ctx/view c)) :again (end-night! c)))
        (= :no-way-out (:reason r))
        (let [text "no way out of the shelter by day; giving the body back"]
          (ctx/emit! c :shelter.failed :warn {:reason :no-way-out :at (:at r) :tries (:tries r) :text text})
          (ctx/remember! c :shelter-trapped {:pos (sh/feet (:primitives c))} sh/trapped-policy)
          (ctx/forget-where! c :night-site (constantly true))
          (result/stop! c :no-way-out text :at (:at r) :tries (:tries r)))
        :else (await (hold! c :leaving))))
    (if-let [bed (sh/bed-to-collect (:primitives c) (ctx/view c))]
      (await (collect-bed! c bed))
      (end-night! c))))

(defn ^:async morning!
  "By day: a dug bed's drop is walked over first, else morning-step!."
  [c]
  (busy! c)
  (if-let [pos (:pickup (ctx/mem c))]
    (await (pick-up-drop! c pos))
    (morning-step! c)))

(defn ^:async pass
  "One choice of the night (see doc): :again, or the round's end."
  [c]
  (let [p (:primitives c)
        roofed (sh/roofed? p (:roof-height (:args c)))
        sheltered (:sheltered (ctx/mem c))]
    (when (and (sh/night? p) (overdue? c)) (note-needs-bed! c))
    (cond
      (not (sh/night? p)) (await (morning! c))
      (sh/sleeping? p) (do (when-not (= :slept sheltered) (sheltered! c :slept)) (await (hold! c :sleeping)))
      (and roofed (or (#{:slept :dug-in} sheltered) (dig-in/sheltered-in c)))
      (do (when-not sheltered (sheltered! c :dug-in))
          (if (log-out-wanted? c) (await (log-out-step c)) (await (hold! c :sheltered))))
      :else
      (let [bed (sh/bed-to-use p (ctx/view c) (radius c) (bed-permit c))]
        (cond
          bed (await (sleep-step c bed))
          (sh/bed-place-wanted? p (ctx/view c) (radius c) (bed-permit c)) (await (place-bed-and-sleep c))
          (log-out-wanted? c) (await (log-out-step c))
          (or roofed (sh/buried? p)) (do (ctx/forget-where! c :night-site (constantly true))
                                         (result/finish! c {:night (if (= :logged-out sheltered) :logged-out :roofed)}))
          (and (= :exposed sheltered) (retry-due? c)) (do (retry-dig-in! c) (await (shelter! c)))
          (= :exposed sheltered) (await (hold! c :exposed))
          :else (if-let [place (and (roof-walk-wanted? c) (roofed-place c))]
                  (await (roof-walk! c place))
                  (await (shelter! c))))))))

(defn ^:async round
  "The whole night in one round (see doc): passes until one ends it."
  [c]
  (loop []
    (let [r (await (pass c))]
      (if (= :again r)
        (do (await (child/pace!)) (recur))
        r))))
