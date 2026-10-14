(ns jobs.survival.bed-down
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.child :as child]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.look :as look]
            [jobs.lib.access :as access]
            [jobs.lib.places :as places]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter :as sh]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tidy-rules :as tidy-rules]
            [jobs.lib.util :as u]))

(def doc
  "Go to bed: sleep in a bed to use, else put a carried bed down beside the body and sleep in it. One step per call, run to its end by the caller (jobs.survival.night).
  Conditions at the start (a listed call waits on them): night (:not-night), and a bed to use or a bed item to put down (:no-bed; the engine looks round once first; with :fetch on, which is the default, a bed is fetched instead: jobs.items.obtain of any colour, then put down).
  The bed: the :bed argument, else sh/bed-to-use (seen, or remembered within :bed-radius, :urgent-bed-radius once :max-days-awake days passed without sleep; never an occupied one or one given up on; unless that long without sleep, never one whose walk leaves an underground, dug-in or room shelter): jobs.survival.sleep on it.
  A bed in the open is slept in only by a body overdue for sleep or one that just put the bed down; a sleep that stops :bed-open writes :bed-open (shelter.bed_open info) and ends condition-failed :bed-open.
  A sleep that ends otherwise without sleeping writes :sleep-failed (5 minutes, shelter.sleep_failed warn) and ends nothing-possible :sleep-failed.
  A bed item carried and no bed to use: put down beside the body (roofed cells first, else in the open; never in a doorway, on a chest, table or other kept block, or in another's zone, claim or plan footprint), recorded as :bed unless a live :bed is, and as :bed-placed outside the body's own zones (info night.bed-placed {:pos :own-zone}). A placement the server refuses is tried again (twice at most on one foot cell, three in all). No room, or a failed placement, writes :bed-place-failed (10 minutes, shelter.bed_place_failed warn) and ends nothing-possible :bed-place-failed.
  Walks use :at-night :go. Ends done {:slept true :bed pos} when asleep, condition-failed :not-night when the day came first or :bed-open {:slept false :bed-open true}; :continue while walking or sleeping. Quote: time by the walk to the bed, done 1 sleep.")

(a/defargs args
  {:bed {:doc "bed position [x y z] or {:x :y :z} to sleep in; default: the bed to use within the radius" :spec ::a/pos :default nil}
   :roof-height {:doc "a solid block within this many blocks above counts as a roof (where a carried bed is put down)" :spec (a/int-in 0 nil) :default sh/default-roof-height}
   :bed-radius {:doc "a remembered bed farther than this is not used" :spec (a/num-in 0 nil) :default sh/default-bed-radius}
   :urgent-bed-radius {:doc "the bed radius once the body is overdue for sleep" :spec (a/num-in 0 nil) :default sh/urgent-bed-radius}
   :fetch {:doc "get a bed when none is to use or carried (jobs.lib.fetch): true, a set of kinds or a map of limits; false makes :no-bed a start condition" :spec fetch/option? :default true}
   :max-days-awake {:doc "in-game days without sleep before a bed in the open is slept in and the urgent radius applies" :spec (a/num-in 0 nil) :default sh/max-days-awake}})

(a/defargs settings
  {::walk-speed {:default 4.3 :doc "Blocks a second the walk to the bed covers, for the quote." :spec (a/num-in 0 nil)}})

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

(def target [])

(def job 'jobs.survival.bed-down)

(def bed-names
  (mapv #(str % "_bed") ["white" "orange" "magenta" "light_blue" "yellow" "lime" "pink" "gray" "light_gray" "cyan" "purple" "blue" "brown" "green" "red" "black"]))

(defn problem
  "The need wait for a bed while there is none to use and none carried to put down, else nil."
  [c]
  (let [p (:primitives c)]
    (when-not (or (bed-to-use c) (sh/bed-place-wanted? p (ctx/view c) (radius c) (bed-permit c)))
      {:reason :need :any-of bed-names :count 1})))

(defn start-fact
  "The fact that bars starting, or nil: night, and a bed to use or a bed item to put down."
  [c]
  (when-not (ctx/frozen-call c)
    (let [p (:primitives c)]
      (cond
        (not (sh/night? p)) :not-night
        :else (when-let [w (problem c)]
                (when-not (fetch/check c job w)
                  (cond-> {:reason :no-bed :text "no bed to use and none carried to put down"}
                    (not (look/surveyed? c)) contract/look-first)))))))

(def conditions [[:fn start-fact]])

(defn done? [c]
  (let [d (sh/sleeping? (:primitives c))]
    {:done? d :units :sleeps :done (if d 1 0) :left (if d 0 1)}))

(defn quote-of [c]
  (let [bed (bed-to-use c)
        s (if bed (/ (u/dist (u/self-pos c) bed) (settings/get settings ::walk-speed)) 0)]
    {:uses {:time [0 s (max (* 3 s) 10)]} :done [0 1 1]}))

(defn ^:async sleep!
  "jobs.survival.sleep on bed (open-ok?: also one not shut in a roofed room: a bed the body just put down, or an overdue body)."
  [c bed open-ok?]
  (let [p (:primitives c)
        started (ctx/now c)
        s (await (ctx/call c :sleep 'jobs.survival.sleep {:bed bed :open-bed open-ok?}))]
    (cond
      (= :continue s) :continue
      (= :bed-open (contract/cause-key s))
      (do (ctx/remember! c :bed-open {:pos bed} sh/bed-open-policy)
          (ctx/emit! c :shelter.bed_open :info {:pos bed :text "bed is open, dug in beside it"})
          (ctx/end c :condition-failed :bed-open {:slept false :bed-open true}))
      (or (sh/sleeping? p) (seq (ctx/since c :slept started))) (ctx/end c :done nil {:slept true :bed bed})
      (not (sh/night? p)) (ctx/end c :condition-failed :not-night {:slept false})
      :else (do (ctx/remember! c :sleep-failed {:pos bed} sh/sleep-failed-policy)
                (ctx/emit! c :shelter.sleep_failed :warn {:pos bed :text "could not sleep in the bed; not trying again for a while"})
                (ctx/end c :nothing-possible :sleep-failed {:slept false})))))

(def bed-dirs [[1 0] [-1 0] [0 1] [0 -1]])

(def bed-tries
  "Placements of the carried bed the server may refuse before :bed-place-failed; a foot cell refused twice is left for another row."
  3)

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
                            (not= foot (sh/feet p))
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
  the row) and record it as :bed unless a live :bed is recorded, and as :bed-placed when outside the body's own zones.
  A placement the server refuses is counted (job memory :bed-refused) and tried again while fewer than bed-tries failed.
  :continue while walking or trying again; the foot cell when a bed block stands there after; else nil."
  [c]
  (let [p (:primitives c)
        item (sh/carried-bed p)
        {:keys [stand foot] :as layout} (await (sense/decide! c ::layout #(or (bed-layout c true) (bed-layout c false))))]
    (when foot
      (if (= stand (sh/feet p))
        (let [r (await (tidy/place! c foot item false (click-for layout)))
              refused (:bed-refused (ctx/mem c))]
          (cond
            (and (not= "placed" (.-status r)) (< (inc (reduce + 0 (vals refused))) bed-tries))
            (do (ctx/update-mem! c update-in [:bed-refused [(:x foot) (:y foot) (:z foot)]] (fnil inc 0))
                :continue)
            (and (= "placed" (.-status r)) (await (bed-visible? c foot)))
            (let [own? (boolean (sh/in-own-zone? p (:world (:engine c)) foot))]
              (places/offer! c :bed foot)
              (when-not own?
                (ctx/remember! c :bed-placed {:pos foot} sh/bed-placed-policy))
              (ctx/emit! c :night.bed-placed :info {:pos foot :own-zone own?
                                                    :text (str "put the carried bed down at " (pr-str foot) (if own? " (in its own zone: it stays)" ", picked up by day unless sleeping in it made it the respawn bed"))})
              foot)))
        (let [w (await (child/call-walk* c :bed-walk {:pos stand :range 0 :at-night :go}))]
          (when (or (= :continue w) (= :done (:kind w))) :continue))))))

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
          (ctx/end c :nothing-possible :bed-place-failed {:slept false})))))

(defn ^:async round [c]
  (let [p (:primitives c)
        bed (or (some-> (:bed (:args c)) places/parse-pos :pos)
                (await (sense/decide! c ::bed #(sh/bed-to-use p (ctx/view c) (radius c) (bed-permit c)))))]
    (cond
      bed (await (sleep! c bed (overdue? c)))
      (sh/bed-place-wanted? p (ctx/view c) (radius c) (bed-permit c)) (await (place-bed-and-sleep! c))
      :else (let [r (when (fetch/opts c job) (await (fetch/fetch-untimed! c job problem)))]
              (cond
                r r
                (nil? (problem c)) :continue
                :else (ctx/end c :condition-failed :no-bed {:slept false}))))))
