(ns jobs.survival.bed-down
  (:require [jobs.lib.block-sets :as bs]
            [engine.game :as game]
            [jobs.lib.ready :as ready]
            [jobs.lib.reserve :as reserve]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.cost :as cost]
            [jobs.lib.price :as price]
            [jobs.lib.threats :as threats]
            [jobs.lib.need :as need]
            [jobs.lib.access :as access]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.blocks :as lb]
            [jobs.lib.seal :as seal]
            [jobs.lib.places :as places]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.sleepers :as sh-sleepers]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tidy-rules :as tidy-rules]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.util :as u]
            [jobs.survival.shut-in-night :as sin]
            [jobs.survival.wall-in :as wall-in]))

(def doc
  "Go to bed: sleep in a bed to use, else put a carried bed down beside the body and sleep in it. One step per call, run to its end by the caller (jobs.survival.night).
  Conditions at the start (a listed call waits on them): night (:not-night; asked again before every round), and a bed to use or a bed item to put down (:no-bed; the engine looks round once first; with the default ways, a bed is fetched instead through jobs.lib.need: jobs.items.obtain of any colour, then put down; a failed fetch ends with the cause {:reason :need} and its ending as :because).
  The bed: the :bed argument, else sh-beds/bed-to-use (seen, or remembered within the jobs.lib.shelter.beds bed radius, the urgent radius once max-days-awake days passed without sleep; never an occupied one or one given up on; unless that long without sleep or an underground body sees another player sleep and the bed is near (courtesy), never one whose walk leaves an underground, dug-in or room shelter): jobs.survival.sleep on it.
  A carried bed the reserve register holds (jobs.lib.reserve, chooser :night) is not put down. Beds are priced against the other ways through the night (answers, cheapest): a bed not shut in a closed roofed room is slept in only when overdue for sleep or lying there is the cheapest; a carried bed goes down as the :answer says (else the cheapest): :bed lain in where it lies, :sealed its row walled and roofed first (seal-and-sleep!), and when another way is cheaper it is not put down. A sleep that stops :bed-open writes :bed-open and ends condition-failed :bed-open, and a carried bed not put down ends condition-failed :bed-open {:answer} too; a row left open ends nothing-possible :seal-failed.
  A sleep that ends otherwise without sleeping writes :sleep-failed (5 minutes) and ends nothing-possible :sleep-failed.
  A bed item carried and no bed to use: put down beside the body (a body outside walks first into a :home that lies in a closed roofed room; roofed cells first, else in the open unless :roofed-only; never in a doorway, on a chest, table or other kept block, or in another's zone, claim or plan footprint), recorded as :bed unless a live :bed is, and as a scaffold-ledger entry (purpose :bed, for survival.leave-shelter to take back by day; a bed at the live respawn bed is not listed) outside the body's own zones (info night.bed-placed {:pos :own-zone}). A placement the server refuses is tried again (twice at most on one foot cell, three in all). No room, or a failed placement, writes :bed-place-failed (10 minutes) and ends nothing-possible :bed-place-failed.
  Ends done {:slept true :bed pos} when asleep, condition-failed :not-night when the day came first or :bed-open {:slept false :bed-open true}; :continue while walking or sleeping. Quote: time by the walk to the bed, done 1 sleep.")

(a/defargs args
  {:bed {:doc "bed position [x y z] or {:x :y :z} to sleep in; default: the bed to use within the radius" :spec ::a/pos :default nil}
   :answer {:doc "the caller's priced choice for a carried bed (answers): :bed put it down and lie in it, :sealed shut its row in first; default: priced here" :spec #{:bed :sealed} :default nil}
   :roofed-only {:doc "put a carried bed down on roofed cells only, never in the open (a body inside a home)" :spec boolean? :default false}})

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

(defn stay-chance
  "The spawn chance a second at the feet through the night: none underground or lit by a block (the sky's light goes with
  the night), else a dark cell's (cost/spawn-chance). The cell defaults to the feet."
  ([p] (stay-chance p (sh-sense/feet p)))
  ([p cell]
   (if (or (sh-sense/buried-at? p cell) (pos? (or (sh-sense/block-light-at p cell) 0))) 0 (cost/spawn-chance [0 0 0]))))

(defn wake-cells
  "Where a body asleep in a bed may stand when it gets up: every cell round the bed it can stand in (beside the head,
  beside the foot, behind the foot, the other side), else the bed. The server takes one by the sleeper's yaw, which is
  not known: callers price the worst."
  [p]
  (let [head (sh-sense/feet p)
        bed? #(some-> (u/seen-name p %) (.endsWith "_bed"))
        at (fn [[dx dz]] (-> head (update :x + dx) (update :z + dz)))
        foot (first (filter bed? (map #(at %) [[1 0] [-1 0] [0 1] [0 -1]])))
        stand? (fn [cell] (and (bs/air (u/seen-name p cell)) (bs/air (u/seen-name p (update cell :y inc)))
                               (sh-sense/solid-at? p (update cell :y dec))))]
    (or (when foot
          (let [f [(- (:x head) (:x foot)) (- (:z head) (:z foot))]
                r [(- (second f)) (first f)]
                add (fn [[ax az] [bx bz] k] [(+ ax (* k bx)) (+ az (* k bz))])
                neg (fn [[x z]] [(- x) (- z)])]
            (not-empty (filterv stand? (map at [r (add r f -1) (add r f -2) (add [0 0] f -2) (add (neg r) f -2) (add (neg r) f -1) (neg r)])))))
        [head])))

(defn shut-in?
  "Whether a bed beside the body lies sheltered: the body is shut in a closed roofed room or buried."
  [p]
  (or (sh-sense/shut-in-room? p) (sh-sense/buried? p)))

(defn closed-home
  "The :home of a body outside it when it lies in a closed roofed room (the carried bed goes down there), else nil."
  [c]
  (let [p (:primitives c)
        home (mem/place (ctx/view c) :home)]
    (when (and home (not (sh-sense/shut-in-room? p (sh-sense/feet p))) (sh-sense/shut-in-room? p home))
      home)))

(defn place-wanted?
  "sh-beds/bed-place-wanted?, unless the reserve register holds the carried bed (a plan's, or the agent's) for something else."
  [c]
  (and (sh-beds/bed-place-wanted? (:primitives c) (ctx/view c) (radius c) (bed-permit c))
       (boolean (some #(.endsWith (:name %) "_bed") (reserve/free c :night)))))

(defn bed-sheltered?
  "Whether a bed at cell lies sheltered: shut in a closed roofed room, or the body buried."
  [p cell]
  (or (sh-sense/shut-in-room? p cell) (sh-sense/buried? p)))

(defn seal-cells
  "The cells to fill round a bed layout's row (stand, foot, head) to shut it in: its sides at feet and head height, a
  roof support on the side behind the stand, and the roof over the row; those not seal/sealed? already."
  [p {:keys [stand foot head]}]
  (let [row [stand foot head]
        in-row? (set row)
        support (-> (merge-with - (merge-with + stand stand) foot) (update :y + 2))]
    (vec (distinct (remove #(seal/sealed? p %)
                           (concat (for [r row [dx dz] [[1 0] [-1 0] [0 1] [0 -1]] dy [0 1]
                                         :let [side {:x (+ (:x r) dx) :y (:y r) :z (+ (:z r) dz)}]
                                         :when (not (in-row? side))]
                                     (update side :y + dy))
                                   [support]
                                   (map #(update % :y + 2) row)))))))

(defn night-c [c] (assoc-in c [:args :for] :night))

(defn seal-plan
  "{:layout :cells :seconds} to put the carried bed down here and shut its row in (seal-cells, each at wall-in/cell-seconds
  for the night), when a row fits in the open and the shelter blocks carried cover every cell; else nil."
  [c]
  (let [layout (sh-beds/bed-layout c false)
        cells (when layout (seal-cells (:primitives c) layout))
        have (reduce + 0 (map :count (lb/carried c (sin/blocks (night-c c)))))]
    (when (and (seq cells) (<= (count cells) have))
      {:layout layout :cells cells :seconds (* (count cells) (wall-in/cell-seconds (night-c c) (count cells)))})))

(defn exposed-price
  "Seconds secs in the open at `at` cost: spawns at chance (price/stay-hp) and the fight with the hostiles known near `at`
  (price/way-fight-hp), each hp at cost/hp-price."
  [c chance secs at]
  (* (cost/hp-price (or (.-health (.self (:primitives c))) 20))
     (+ (price/stay-hp c chance secs) (price/way-fight-hp c at))))

(defn respawn-body
  "threats/death-body, its :respawn the bed slept in last when that came after the latest :spawn-bed (lying down sets
  the respawn point)."
  [c]
  (let [body (threats/death-body c)
        slept (ctx/latest c :slept)
        marked (sh-spawn/spawn-bed-entry (ctx/view c))]
    (if (and slept (or (nil? marked) (> (:t slept) (:t marked))))
      (assoc body :respawn (:pos (:data slept)))
      body)))

(defn ^:async answers
  "{answer seconds}: the ways through the night from here, each priced as the night's seconds it loses, the hp it risks
  (exposed-price) and, where it sets the respawn point, less cost/spawn-worth there (a body asleep has set it already).
  :bed lying in `bed` (nil: the carried bed put down here, or the bed the body is asleep in): the night left lost unless
  the body's sleep skips it (sh-sleepers/sleepers-short 0), open (not bed-sheltered?) all that time, plus walk-s (the walk
  there, priced by the caller). :sealed (a carried bed in the open, seal-plan): the bed's row shut in first, its seconds
  exposed, then lain in. :shelter sheltering here, from the worst of the cells the bed is got up to (wake-cells) when asleep (sh/shelter-here-s exposed; a body sh/in-ground? works on after, else
  the night left is lost). :log-out (log-out?): jobs.survival.log-out's quoted seconds, the night left to the sleepers."
  [c {:keys [bed walk-s log-out?]}]
  (let [p (:primitives c)
        view (ctx/view c)
        feet (sh-sense/feet p)
        asleep? (sh-sense/sleeping? p)
        carried? (and (nil? bed) (not asleep?) (place-wanted? c))
        at (or bed feet)
        chance (stay-chance p)
        stands (if asleep? (wake-cells p) [feet])
        [_ left] (get-in (sh-clock/night-left-quote c) [:uses :job-time])
        lost (if (zero? (sh-sleepers/sleepers-short p view)) 0 left)
        sheltered? (if (or bed asleep?) (bed-sheltered? p at) (or (shut-in? p) (some? (closed-home c))))
        body (respawn-body c)
        sealed (when (and carried? (not sheltered?)) (seal-plan c))
        shelter-s (min left (sh/shelter-here-s))
        out-s (when log-out? (get-in (await (ctx/quote c 'jobs.survival.log-out {})) [:uses :job-time 1]))]
    (cond-> {:shelter (+ (if (sh/in-ground? p view) shelter-s left) (apply max (map #(exposed-price c (stay-chance p %) shelter-s %) stands)))}
      (or bed asleep? carried?) (assoc :bed (+ (or walk-s 0) lost (if sheltered? 0 (exposed-price c chance lost at)) (if asleep? 0 (- (cost/spawn-worth body at)))))
      sealed (assoc :sealed (+ lost (:seconds sealed) (exposed-price c chance (:seconds sealed) feet) (- (cost/spawn-worth body (:foot (:layout sealed))))))
      (number? out-s) (assoc :log-out out-s))))

(def answer-order [:bed :sealed :log-out :shelter])

(defn cheapest
  "The answer of prices ({answer seconds}) that costs least, the earlier of answer-order at a tie; :bed when overdue for
  sleep and a bed is priced (the rest keeps the phantoms off)."
  [c prices]
  (if (and (overdue? c) (:bed prices))
    :bed
    (first (sort-by prices (filter prices answer-order)))))

(defn ^:async open-sleep-ok?
  "Whether to lie in a bed at bed that is not shut in: overdue, or lying there is the cheapest of answers (no walk: the
  body is there)."
  [c bed]
  (= :bed (cheapest c (await (answers c {:bed bed :log-out? (sh-sleepers/log-out-for-sleepers? (:primitives c) (ctx/view c))})))))

(defn bed-to-use
  "The :bed argument, else the bed sh-beds/bed-to-use picks."
  [c]
  (or (some-> (:bed (:args c)) places/parse-pos :pos)
      (sh-beds/bed-to-use (:primitives c) (ctx/view c) (radius c) (bed-permit c))))

(def defaults {:checks {:safe? '(not (threatened)) :fit? '(always)} :spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(def meta {:category :phase :part-of '[jobs.survival.night]})

(def safe? ready/threatened)

(def fit? ready/ok)

(def ways need/fetch-ways)

(def target [])

(def job 'jobs.survival.bed-down)

(def bed-names
  (mapv #(str % "_bed") game/dye-colors))

(defn problem
  "The need wait for a bed while there is none to use and none carried to put down, else nil."
  [c]
  (when-not (or (bed-to-use c) (place-wanted? c))
    {:reason :need :any-of bed-names :count 1}))

(defn bed-status
  "A bed to use, a bed item to put down or a bed to fetch, else :no-bed (:look until the body looked round)."
  [c]
  (if (when-let [w (problem c)] (not (some->> (need/fetch-opts c) (need/plan-for w))))
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
  "jobs.survival.sleep on bed (open-ok?: also one not shut in a roofed room, open-sleep-ok?)."
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
                                                            :text (str "could not sleep in the bed" (some->> s :data :text (str ": ")) "; not trying again for a while")})))))

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

(defn ^:async walk-into-home!
  "A body outside, with a :home that lies in a closed roofed room: walk there so the bed goes down inside it.
  :continue while walking; nil when no such home or the walk failed (the bed then goes down where the body is)."
  [c]
  (when-let [home (closed-home c)]
    (let [w (await (child/call-walk* c :home-walk {:pos home :range 1}))]
      (when (= :continue w) :continue))))

(defn ^:async put-down-bed-here!
  "Walk to the layout's stand cell if the body is not on it, then place the carried bed on the foot cell (facing along
  the row) and record it as :bed unless a live :bed is recorded, and as a scaffold-ledger entry (purpose :bed) when outside the body's own zones.
  A placement the server refuses is counted (job memory :bed-refused) and tried again while fewer than ::bed-tries failed.
  :continue while walking or trying again; the foot cell when a bed block stands there after; else nil."
  [c]
  (let [p (:primitives c)
        item (sh-beds/carried-bed p)
        {:keys [stand foot] :as layout} (await (sense/decide! c ::layout #(or (sh-beds/bed-layout c true) (when-not (:roofed-only (:args c)) (sh-beds/bed-layout c false)))))]
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

(defn ^:async put-down-bed!
  "put-down-bed-here!, after a walk into a home that is a closed roofed room when the body is outside it."
  [c]
  (or (await (walk-into-home! c))
      (await (put-down-bed-here! c))))

(defn bed-gone?
  "Whether the cell is seen as something other than a bed."
  [p cell]
  (let [n (u/seen-name p cell)]
    (boolean (and n (not (.endsWith n "_bed"))))))

(defn ^:async seal-and-sleep!
  "The carried bed is down at job memory :seal's :bed, the body on its :stand cell: fill its row's :cells
  (jobs.blocks.fill-cells in place, ledger :shelter), write the night's :shelter entry (shut-in-night/end!, the walls
  way: dug open again by day), then sleep. A bed no longer there drops the :seal and yields; a row the fill leaves open
  ends nothing-possible :seal-failed."
  [c]
  (let [p (:primitives c)
        {:keys [bed cells]} (:seal (ctx/mem c))]
    (if (bed-gone? p bed)
      (do (ctx/update-mem! c dissoc :seal) :continue)
      (let [e (await (ctx/call c :seal 'jobs.blocks.fill-cells {:cells (mapv u/cell-vec cells) :in-place true :purpose :shelter
                                                                :use {:blocks (sin/blocks (night-c c))}}))
            sealed? (and (not= :continue e) (sh-sense/shut-in-room? p))]
        (when (not= :continue e)
          (ctx/update-mem! c dissoc :seal)
          (sin/end! (night-c c) :walls (select-keys (:data e) [:placed]) sealed?))
        (cond
          (= :continue e) :continue
          sealed? (await (sleep! c bed true))
          :else (ctx/end c :nothing-possible :seal-failed {:slept false :pos bed
                                                          :text (str "could not shut the bed's row in (" (count (:left (:data e))) " cells left open); not lying in the open")}))))))

(defn ^:async place-bed-and-sleep!
  "A bed carried and none to use: set it down beside the body and sleep in it; answer :sealed shuts its row in first
  (seal-and-sleep!)."
  [c answer]
  (let [p (:primitives c)
        r (await (put-down-bed! c))]
    (cond
      (= :continue r) :continue
      (and r (= :sealed answer))
      (let [stand (sh-sense/feet p)
            head (merge-with + r (merge-with - r stand))]
        (ctx/update-mem! c assoc :seal {:bed r :cells (seal-cells p {:stand stand :foot r :head head})})
        (await (seal-and-sleep! c)))
      r (await (sleep! c r true))
      :else
      (do (ctx/remember! c :bed-place-failed {:pos (sh-sense/feet p)} sh-beds/bed-place-failed-policy)
          (ctx/end c :nothing-possible :bed-place-failed {:slept false
                                                          :text "no room or permission to put the carried bed down; not sleeping in it"})))))

(defn ^:async carried-answer
  "The caller's :answer, else the cheapest of answers for the carried bed here."
  [c]
  (or (:answer (:args c))
      (cheapest c (await (answers c {:log-out? (sh-sleepers/log-out-for-sleepers? (:primitives c) (ctx/view c))})))))

(defn ^:async round [c]
  (let [p (:primitives c)
        bed (or (some-> (:bed (:args c)) places/parse-pos :pos)
                (await (sense/decide! c ::bed #(sh-beds/bed-to-use p (ctx/view c) (radius c) (bed-permit c)))))
        answer (when (and (not bed) (not (:seal (ctx/mem c))) (place-wanted? c)) (await (carried-answer c)))]
    (cond
      (:seal (ctx/mem c)) (await (seal-and-sleep! c))
      bed (await (sleep! c bed (await (open-sleep-ok? c bed))))
      (#{:bed :sealed} answer) (await (place-bed-and-sleep! c answer))
      answer (ctx/end c :condition-failed :bed-open {:slept false :bed-open true :answer answer
                                                     :text (str "the carried bed would lie in the open on a night it does not end; " (name answer) " is cheaper; not putting it down")})
      :else (let [o (need/fetch-opts c)
                  e (when (and o (problem c)) (await (need/blocks! c bed-names 1 o)))]
              (cond
                (or (= :continue e) (nil? (problem c))) :continue
                (and e (not= :done (:kind e))) (contract/with-because (ctx/end c (:kind e) {:reason :need} {:slept false}) e)
                :else (ctx/end c :condition-failed :no-bed {:slept false}))))))
