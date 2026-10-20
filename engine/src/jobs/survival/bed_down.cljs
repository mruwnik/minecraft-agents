(ns jobs.survival.bed-down
  (:require [jobs.lib.block-sets :as bs]
            [engine.game :as game]
            [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.need :as need]
            [jobs.lib.access :as access]
            [jobs.lib.places :as places]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tidy-rules :as tidy-rules]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.util :as u]))

(def doc
  "Go to bed: sleep in a bed to use, else put a carried bed down beside the body and sleep in it. One step per call, run to its end by the caller (jobs.survival.night).
  Conditions at the start (a listed call waits on them): night (:not-night; asked again before every round), and a bed to use or a bed item to put down (:no-bed; the engine looks round once first; with :fetch on, which is the default, a bed is fetched instead through jobs.lib.need: jobs.items.obtain of any colour, then put down; a failed fetch ends with the cause {:reason :need} and its ending as :because).
  The bed: the :bed argument, else sh-beds/bed-to-use (seen, or remembered within the jobs.lib.shelter.beds bed radius, the urgent radius once max-days-awake days passed without sleep; never an occupied one or one given up on; unless that long without sleep or an underground body sees another player sleep and the bed is near (courtesy), never one whose walk leaves an underground, dug-in or room shelter): jobs.survival.sleep on it.
  A bed in the open is slept in only by a body overdue for sleep or one that just put the bed down; a sleep that stops :bed-open writes :bed-open and ends condition-failed :bed-open.
  A sleep that ends otherwise without sleeping writes :sleep-failed (5 minutes) and ends nothing-possible :sleep-failed.
  A bed item carried and no bed to use: put down beside the body (roofed cells first, else in the open unless :roofed-only; never in a doorway, on a chest, table or other kept block, or in another's zone, claim or plan footprint), recorded as :bed unless a live :bed is, and as a scaffold-ledger entry (purpose :bed, for survival.leave-shelter to take back by day unless it became the respawn bed) outside the body's own zones (info night.bed-placed {:pos :own-zone}). A placement the server refuses is tried again (twice at most on one foot cell, three in all). No room, or a failed placement, writes :bed-place-failed (10 minutes) and ends nothing-possible :bed-place-failed.
  Ends done {:slept true :bed pos} when asleep, condition-failed :not-night when the day came first or :bed-open {:slept false :bed-open true}; :continue while walking or sleeping. Quote: time by the walk to the bed, done 1 sleep.")

(a/defargs args
  {:bed {:doc "bed position [x y z] or {:x :y :z} to sleep in; default: the bed to use within the radius" :spec ::a/pos :default nil}
   :roof-height {:doc "a solid block within this many blocks above counts as a roof (where a carried bed is put down)" :spec (a/int-in 0 nil) :default sh-sense/default-roof-height}
   :roofed-only {:doc "put a carried bed down on roofed cells only, never in the open (a body inside a home)" :spec boolean? :default false}
   :fetch {:doc "get a bed when none is to use or carried (jobs.lib.need): true, a set of kinds or a map of limits; false makes :no-bed a start condition" :spec need/option? :default true}})

(a/defargs settings
  {::job-time-s {:default 900 :doc "Seconds a bed-down may take: the walk, the bed put down or fetched (an obtain may take its own 600 without a change), the sleep." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 1800 :doc "Seconds a bed-down may last from its start, the jobs that run between its rounds included." :spec (a/num-in 0 nil)}
   ::bed-tries {:default 3 :doc "Placements of the carried bed the server may refuse before :bed-place-failed; a foot cell refused twice is left for another row." :spec (a/int-in 1 nil)}
   ::walk-speed {:default 4.3 :doc "Blocks a second the walk to the bed covers, for the quote." :spec (a/num-in 0 nil)}})

(defn bed-permit
  "Whether the body may use a bed (sh-beds/bed-permit over the job's world)."
  [c]
  (sh-beds/bed-permit (:primitives c) (:world (:engine c))))

(defn radius [c] (sh-beds/bed-radius (ctx/view c)))

(defn overdue? [c]
  (let [days (sh-beds/days-awake c)]
    (and (some? days) (>= days (sh-beds/max-days-awake)))))

(defn bed-to-use
  "The :bed argument, else the bed sh-beds/bed-to-use picks."
  [c]
  (or (some-> (:bed (:args c)) places/parse-pos :pos)
      (sh-beds/bed-to-use (:primitives c) (ctx/view c) (radius c) (bed-permit c))))

(def defaults {:checks {:safe? '(not (threatened)) :fit? '(always)} :spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(def meta {:category :phase :part-of '[jobs.survival.night]})

(def safe? ready/threatened)

(def fit? ready/ok)

(def target [])

(def job 'jobs.survival.bed-down)

(def bed-names
  (mapv #(str % "_bed") game/dye-colors))

(defn problem
  "The need wait for a bed while there is none to use and none carried to put down, else nil."
  [c]
  (let [p (:primitives c)]
    (when-not (or (bed-to-use c) (sh-beds/bed-place-wanted? p (ctx/view c) (radius c) (bed-permit c)))
      {:reason :need :any-of bed-names :count 1})))

(defn bed-status
  "A bed to use, a bed item to put down or a bed to fetch, else :no-bed (:look until the body looked round)."
  [c]
  (if (when-let [w (problem c)] (not (some->> (need/opts c job) (need/plan-for w))))
    (contract/status :look :no-bed "no bed to use and none carried to put down" {})
    (contract/ok :bed "a bed to use, put down or fetch")))

(defn can-start?
  "Night, and by night a bed (a room flood fill) to use, put down or fetch. Not cond/and: bed-status is skipped by day,
  where its flood fill would run for nothing."
  [c]
  (let [n (contract/asked c (checks/night :not-night))]
    (if (= :ok (first n)) (contract/all [n (contract/asked c bed-status)]) n)))

(defn can-run?
  "Before every round: day came (:later :not-night)."
  [c]
  (if (sh-sense/night? (:primitives c))
    (contract/ok :night "night")
    (contract/later :not-night "not night")))

(defn done? [c]
  (let [d (sh-sense/sleeping? (:primitives c))]
    (checks/goal d [:sleeping "already asleep"] [:awake "not asleep"] {:units :sleeps :done (if d 1 0) :left (if d 0 1)})))

(defn quote-of [c]
  (let [bed (bed-to-use c)
        s (if bed (/ (u/dist (u/self-pos c) bed) (settings/get settings ::walk-speed)) 0)]
    {:uses {:job-time [0 s (max (* 3 s) 10)]} :done [0 1 1]}))

(defn ^:async sleep!
  "jobs.survival.sleep on bed (open-ok?: also one not shut in a roofed room: a bed the body just put down, or an overdue body)."
  [c bed open-ok?]
  (let [p (:primitives c)
        started (ctx/now c)
        s (await (ctx/call c :sleep 'jobs.survival.sleep {:bed bed :open-bed open-ok?}))]
    (cond
      (= :continue s) :continue
      (= :bed-open (contract/cause-key s))
      (do (ctx/remember! c :bed-open {:pos bed} sh-beds/bed-open-policy)
          (ctx/end c :condition-failed :bed-open {:slept false :bed-open true :pos bed
                                                   :text "bed is open (not shut in a roofed room); not lying in it"}))
      (or (sh-sense/sleeping? p) (seq (ctx/since c :slept started))) (ctx/end c :done nil {:slept true :bed bed})
      (not (sh-sense/night? p)) (ctx/end c :condition-failed :not-night {:slept false})
      :else (do (ctx/remember! c :sleep-failed {:pos bed} sh-beds/sleep-failed-policy)
                (ctx/end c :nothing-possible :sleep-failed {:slept false :pos bed
                                                            :text "could not sleep in the bed; not trying again for a while"})))))

(def bed-dirs [[1 0] [-1 0] [0 1] [0 -1]])

(defn refused-twice? [c {:keys [x y z]}]
  (<= 2 (get (:bed-refused (ctx/mem c)) [x y z] 0)))

(defn bed-layout
  "{:stand :foot :head} for a bed set down beside the body: three cells in a row, the body standing at one end (a placed
  bed faces away from the body), the foot next to it, the head after it. All three free (air, with a solid floor that is no container, bed or other kept block, and
  the body's headroom for the stand cell) within 3 cells of the body, the foot and head not in a doorway or another's
  zone, claim or plan footprint (a missing zone list refuses nothing), and roofed within roof-height when roofed? is
  set, and not a foot cell the server refused twice. The stand cell nearest the body first (its own cell when it fits); nil
  when no row fits."
  [c roofed?]
  (let [p (:primitives c)
        roof (:roof-height (:args c))
        {:keys [x y z]} (sh-sense/feet p)
        in (access/rules-input c)
        air? #(bs/air (u/seen-name p %))
        free? (fn [pos] (and (air? pos) (sh-sense/solid-at? p (update pos :y dec))
                             (nil? (tidy-rules/keep-why (u/seen-name p (update pos :y dec))))))
        covered? (fn [pos] (or (not roofed?) (some #(sh-sense/solid-at? p (update pos :y + %)) (range 1 (inc roof)))))
        permitted? #(and (nil? (access/trespass-refusal in :place %)) (not (sh-sense/doorway? p %)))
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
                            (not= foot (sh-sense/feet p))
                            (not (refused-twice? c foot)))]
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
  the row) and record it as :bed unless a live :bed is recorded, and as a scaffold-ledger entry (purpose :bed) when outside the body's own zones.
  A placement the server refuses is counted (job memory :bed-refused) and tried again while fewer than ::bed-tries failed.
  :continue while walking or trying again; the foot cell when a bed block stands there after; else nil."
  [c]
  (let [p (:primitives c)
        item (sh-beds/carried-bed p)
        {:keys [stand foot] :as layout} (await (sense/decide! c ::layout #(or (bed-layout c true) (when-not (:roofed-only (:args c)) (bed-layout c false)))))]
    (when foot
      (if (= stand (sh-sense/feet p))
        (let [r (await (tidy/place! c foot item false (click-for layout)))
              refused (:bed-refused (ctx/mem c))]
          (cond
            (and (not= "placed" (.-status r)) (< (inc (reduce + 0 (vals refused))) (settings/get settings ::bed-tries)))
            (do (ctx/update-mem! c update-in [:bed-refused [(:x foot) (:y foot) (:z foot)]] (fnil inc 0))
                :continue)
            (and (= "placed" (.-status r)) (await (bed-visible? c foot)))
            (let [own? (boolean (sh-beds/in-own-zone? p (:world (:engine c)) foot))]
              (places/offer! c :bed foot)
              (when-not own?
                (ledger/remember! c (-> (ledger/intend (ledger/open-entries (ctx/view c)) {:cell (u/cell-vec foot) :item item :before "air" :job (:id c) :purpose :bed})
                                        (ledger/confirm (u/cell-vec foot)))))
              (ctx/emit! c :night.bed-placed :info {:pos foot :own-zone own?
                                                    :text (str "put the carried bed down at " (pr-str foot) (if own? " (in its own zone: it stays)" ", picked up by day unless sleeping in it made it the respawn bed"))})
              foot)))
        (let [w (await (child/call-walk* c :bed-walk {:pos stand :range 0}))]
          (when (or (= :continue w) (= :done (:kind w))) :continue))))))

(defn ^:async place-bed-and-sleep!
  "A bed carried and none to use: set it down beside the body and sleep in it."
  [c]
  (let [r (await (put-down-bed! c))]
    (cond
      (= :continue r) :continue
      r (await (sleep! c r true))
      :else
      (do (ctx/remember! c :bed-place-failed {:pos (sh-sense/feet (:primitives c))} sh-beds/bed-place-failed-policy)
          (ctx/end c :nothing-possible :bed-place-failed {:slept false
                                                          :text "no room or permission to put the carried bed down; not sleeping in it"})))))

(defn ^:async round [c]
  (let [p (:primitives c)
        bed (or (some-> (:bed (:args c)) places/parse-pos :pos)
                (await (sense/decide! c ::bed #(sh-beds/bed-to-use p (ctx/view c) (radius c) (bed-permit c)))))]
    (cond
      bed (await (sleep! c bed (overdue? c)))
      (sh-beds/bed-place-wanted? p (ctx/view c) (radius c) (bed-permit c)) (await (place-bed-and-sleep! c))
      :else (let [o (need/opts c job)
                  e (when (and (contains? (:what o) :item) (problem c)) (await (need/blocks! c bed-names 1 (select-keys o [:how :explore]))))]
              (cond
                (or (= :continue e) (nil? (problem c))) :continue
                (and e (not= :done (:kind e))) (contract/with-because (ctx/end c (:kind e) {:reason :need} {:slept false}) e)
                :else (ctx/end c :condition-failed :no-bed {:slept false}))))))
