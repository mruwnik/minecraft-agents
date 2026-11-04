(ns jobs.survival.bed-down
  (:require [jobs.lib.shelter.fill :as fill]
            [jobs.lib.block-sets :as bs]
            [engine.game :as game]
            [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.body :as body]
            [jobs.lib.breath :as breath]
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
            [jobs.lib.spawn-light :as spawn-light]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tidy-rules :as tidy-rules]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.util :as u]
            [jobs.lib.walk-pick :as pick]
            [jobs.survival.night.sites :as sites]
            [jobs.survival.shut-in-night :as sin]
            [jobs.survival.plug-way :as way]))

(def doc
  "Go to bed: sleep in a bed to use, else put a carried bed down beside the body and sleep in it. One step per call, run to its end by the caller (jobs.survival.night).
  Conditions at the start (a listed call waits on them): night (:not-night; asked again before every round), and a bed to use or a bed item to put down (:no-bed; the engine looks round once first; with the default ways, a bed is fetched instead through jobs.lib.need: jobs.items.obtain of any colour, then put down; a failed fetch ends with the cause {:reason :need} and its ending as :because).
  The bed: the :bed argument, else sh-beds/bed-to-use (seen, or remembered within the jobs.lib.shelter.beds bed radius, the urgent radius once max-days-awake days passed without sleep; never an occupied one or one given up on; unless that long without sleep or an underground body sees another player sleep and the bed is near (courtesy), never one whose walk leaves an underground, dug-in or room shelter): jobs.survival.sleep on it.
  A carried bed the reserve register holds (jobs.lib.reserve, chooser :night) is not put down. Beds are priced against the other ways through the night (answers, cheapest): a bed not sheltered (sh-beds/bed-sheltered?: shut in a closed roofed room where no mob spawns) is slept in only when overdue for sleep or lying there is the cheapest; a carried bed goes down as the :answer says (else the cheapest): :bed lain in where it lies, :sealed its row walled and roofed first (seal-and-sleep!), and when another way is cheaper it is not put down. A sleep that stops :bed-open writes :bed-open and ends condition-failed :bed-open, and a carried bed not put down ends condition-failed :bed-open {:answer} too; a row left open ends nothing-possible :seal-failed.
  A sleep that ends :bed-unreachable (sleep wrote :bed-unreachable) yields :continue: the next round picks another bed or puts the carried one down. A sleep that ends otherwise without sleeping writes :sleep-failed (5 minutes) and ends nothing-possible :sleep-failed.
  A bed item carried and no bed to use: put down beside the body (a body outside walks first into a :home that lies in a closed roofed room, and when that walk fails writes :home-walk-failed and ends condition-failed :bed-open, the night pricing the bed again, that home only while another walk is worth its price; roofed cells first, else in the open unless :roofed-only; never in a doorway, on a chest, table or other kept block, or in another's zone, claim or plan footprint), recorded as :bed unless a live :bed is, and as a scaffold-ledger entry (purpose :bed, for survival.leave-shelter to take back by day; a bed at the live respawn bed is not listed) outside the body's own zones (info night.bed-placed {:pos :own-zone}). A placement the server refuses is tried again (twice at most on one foot cell, three in all). No room, or a failed placement, writes :bed-place-failed (10 minutes) and ends nothing-possible :bed-place-failed.
  Ends done {:slept true :bed pos} when asleep, condition-failed :not-night when the day came first or :bed-open {:slept false :bed-open true}; :continue while walking or sleeping. Quote: time by the walk to the bed, done 1 sleep.")

(a/defargs args
  {:bed {:doc "bed position [x y z] or {:x :y :z} to sleep in; default: the bed to use within the radius" :spec ::a/pos :default nil}
   :answer {:doc "the caller's priced choice for a carried bed (answers): :bed put it down and lie in it, :sealed shut its row in first; default: priced here" :spec #{:bed :sealed} :default nil}
   :roofed-only {:doc "put a carried bed down on roofed cells only, never in the open (a body inside a home)" :spec boolean? :default false}})

(a/defargs settings
  {::job-time-s {:default 900 :doc "Seconds a bed-down may take: the walk, the bed put down or fetched (an obtain may take its own 600 without a change), the sleep." :spec (a/num-in 0 nil)}
   ::wall-time-s {:default 1800 :doc "Seconds a bed-down may last from its start, the jobs that run between its rounds included." :spec (a/num-in 0 nil)}
   ::bed-tries {:default 3 :doc "Placements of the carried bed the server may refuse before :bed-place-failed; a foot cell refused twice is left for another row." :spec (a/int-in 1 nil)}})

(defn bed-permit
  "Whether the body may use a bed (sh-beds/bed-permit over the job's world)."
  [c]
  (sh-beds/bed-permit (:primitives c) (:world (:engine c)) (ctx/now c)))

(defn radius [c] (sh-beds/bed-radius (ctx/view c)))

(defn overdue? [c]
  (let [days (sh-beds/days-awake c)]
    (and (some? days) (>= days (sh-beds/max-days-awake)))))

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

(defn seal-cells
  "The cells to fill round a bed layout's row (stand, foot, head) to shut it in: its sides at feet and head height, a
  roof support on the side behind the stand, and the roof over the row; those not fill/sealed? already."
  [p {:keys [stand foot head]}]
  (let [row [stand foot head]
        in-row? (set row)
        support (-> (merge-with - (merge-with + stand stand) foot) (update :y + 2))]
    (vec (distinct (remove #(fill/sealed? p %)
                           (concat (for [r row [dx dz] [[1 0] [-1 0] [0 1] [0 -1]] dy [0 1]
                                         :let [side {:x (+ (:x r) dx) :y (:y r) :z (+ (:z r) dz)}]
                                         :when (not (in-row? side))]
                                     (update side :y + dy))
                                   [support]
                                   (map #(update % :y + 2) row)))))))

(defn night-c [c] (assoc-in c [:args :for] :night))

(defn seal-plan
  "{:layout :cells :seconds} to put the carried bed down here and shut its row in (seal-cells, each at cost.ways/place-s
  for the night), when a row fits in the open and the shelter blocks carried cover every cell; else nil."
  [c]
  (let [layout (sh-beds/bed-layout c false)
        cells (when layout (seal-cells (:primitives c) layout))
        have (reduce + 0 (map :count (lb/carried c (sin/blocks (night-c c)))))]
    (when (and (seq cells) (<= (count cells) have))
      {:layout layout :cells cells :seconds (* (count cells) (way/place-s (night-c c)))})))

(defn exposed-price
  "Seconds secs in the open at `at` cost: the hp the spawns at chance hurt (price/stay-parts) and the fight with the hostiles
  known near `at` (price/way-fight-hp), each at cost/hp-price, plus the death they risk at what cost/route-death says
  it costs (chance x seconds: a death is not scaled by health again)."
  [c chance secs at]
  (let [{:keys [hurt dies death]} (price/stay-parts c chance secs)]
    (+ (* (cost/hp-price (or (.-health (.self (:primitives c))) 20)) (+ hurt (price/way-fight-hp c at)))
       (if (pos? dies) (* dies death) 0))))

(defn open-price
  "exposed-price of secs in the open at the worst of cells, each at its night-chance."
  [c cells secs]
  (let [p (:primitives c)] (apply max (map #(exposed-price c (spawn-light/night-chance p %) secs %) cells))))

(defn open-gain
  "open-price of secs at night less the same secs by day (full sky light) at the worst of cells: only the dark the night
  adds counts, a dark hall is as dangerous by day. Never below 0."
  [c cells secs]
  (let [p (:primitives c)
        day (apply max (map #(exposed-price c (spawn-light/chance-at p % false 0 true) secs %) cells))]
    (max 0 (- (open-price c cells secs) day))))

(defn stand-cells "Where the body spends the night if it does nothing: its feet, or asleep the cells it gets up to (wake-cells)." [p]
  (if (sh-sense/sleeping? p) (wake-cells p) [(sh-sense/feet p)]))

(defn worth-of
  "The loss the night averts: the night left in the open where the body stands less the same by day (open-gain of
  stand-cells), its hurt and death at cost/hp-price, plus the night left itself when the body's own sleep skips it (sleepers-short 0: the time that
  skip saves), plus the skip the other players online wait for while it stays awake (sh-sleepers/skip-wait-s)."
  [c]
  (let [p (:primitives c)
        view (ctx/view c)
        left (second (get-in (sh-clock/night-left-quote c) [:uses :job-time]))
        skipped (if (zero? (sh-sleepers/sleepers-short p view)) left 0)]
    (+ skipped (sh-sleepers/skip-wait-s p view left) (open-gain c (stand-cells p) left))))

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

(defn ^:async dig-in-quote
  "sin/shelter-quote of shut-in's quote of digging in here (a wall or a pit), asked once a pass (walk-pick/once!)."
  [c]
  (await (pick/once! c :dig-in
                     (fn ^:async f []
                       (sin/shelter-quote (await (ctx/quote c 'jobs.survival.shut-in {:ways #{:walls :pit} :for :night})))))))

(defn ^:async dig-in-s
  "Seconds shut-in's quote of digging in here takes (dig-in-quote), its toll in."
  [c]
  (:seconds (await (dig-in-quote c))))

(defn ^:async dig-in-time-s
  "Seconds of the body's time digging in here takes (dig-in-quote, time-s): ##Inf when nothing can."
  [c]
  (sin/time-s (await (dig-in-quote c))))

(defn flee-after
  "The shelter-quote a flee to target t shelters by on arrival at a cave or natural ground: here's (sheltering here, shut-in's
  quote), or where nothing here can, shut-in's guess (::mean-s); none at a bed or a roofed place."
  [{:keys [kind]} here]
  (cond
    (not (#{:cave :ground} kind)) (sin/shelter-quote {:uses {:job-time [0 0 0]}})
    (js/isFinite (:seconds here)) here
    :else (sin/shelter-quote {:uses {:job-time [0 (sin/mean-s) 0]}})))

(defn ^:async answers
  "{answer seconds}: the ways through the night from here, each priced by cost/night-way-s (the night's work it loses, its tolls
  and hurt, its seconds exposed (exposed-price), what it costs by day) or, a bed, as the night's seconds it loses and the hp it risks
  and, where it sets the respawn point, less cost/spawn-worth there for deaths here, where the work goes on (a body asleep has
  set it already).
  :bed lying in `bed` (nil: the carried bed put down here, or in a closed home (sh-beds/closed-home) put-down-bed! walks
  into first, then left to :home when home is given; or the bed the body is asleep in): the night left lost unless
  the body's sleep skips it (sh-sleepers/sleepers-short 0) and no known hostile is near the bed (sh-sleepers/monsters-near-bed?:
  the server refuses the sleep), open (not sh-beds/bed-sheltered?) all that time or at least the time to place the bed and lie
  asleep (sh-sleepers/sleep-start-s), plus walk-s (the walk there, priced by the caller). :sealed (a carried bed in the open, seal-plan): the bed's row shut in first, its seconds
  exposed, then lain in. :shelter sheltering here (dig-in-quote, when shut-in quotes a way) and :niche (niche, the caller's shelter-quote
  of a pocket dug nearby) alike: busy their time (time-s), the toll their quote names, that time exposed from the worst of the cells the bed
  is got up to (wake-cells) when asleep, and their :morning-s. :flee and :home (flee, home: the caller's target with go-to's :seconds and
  :cost of the walk; go-to's price has its dark cells' spawns and the threats that catch it): busy the walk's seconds and, a flee to a cave or
  natural ground, the shelter's time on arrival (flee-after; here's toll is here's), the walk's cost beyond its seconds, that shelter exposed at the target
  (exposed-price: the hostiles known there once),
  and by day that shelter's morning and, in the ground (its work goes on here), the walk back (cost/commute-s's); :home with a carried bed (it goes down there) loses the night as :bed
  does, plus the work its walk loses, less spawn-worth there. :exposed the night left in the open where the body stands (or the worst cell
  it gets up to). :log-out (log-out?): jobs.survival.log-out's quoted seconds, the night left to the sleepers.
  Every way that keeps the body awake (all but :bed, :sealed and :log-out, and :bed too when refused) also costs the skip
  the other players online wait for (sh-sleepers/skip-wait-s)."
  [c {:keys [bed walk-s log-out? niche flee home]}]
  (let [p (:primitives c)
        view (ctx/view c)
        feet (sh-sense/feet p)
        asleep? (sh-sense/sleeping? p)
        carried? (and (nil? bed) (not asleep?) (sh-beds/place-wanted? c))
        at (or bed feet)
        chance (spawn-light/night-chance p (sh-sense/feet p))
        stands (stand-cells p)
        [_ left] (get-in (sh-clock/night-left-quote c) [:uses :job-time])
        lost (if (zero? (sh-sleepers/sleepers-short p view)) 0 left)
        wait (sh-sleepers/skip-wait-s p view left)
        refused? (and (not asleep?) (sh-sleepers/monsters-near-bed? p at))
        bed-lost (if refused? left lost)
        bed-open (min left (max bed-lost (if asleep? 0 (+ sh-sleepers/sleep-start-s (if carried? (sh-beds/sleep-s c nil) 0)))))
        in-ground? (sh/in-ground? p view)
        way-s #(+ wait (cost/night-way-s in-ground? left %))
        open #(open-price c stands %)
        dig (fn [q] (let [t (min left (sin/time-s q))] (way-s {:busy-s t :risk-s (:risk-s q) :open-s (open t) :morning-s (:morning-s q)})))
        walk (fn [{:keys [cost pos] :as t} after]
               (let [seconds (min cost (or (:seconds t) cost))
                     busy (min left (sin/time-s after))]
                 {:busy-s (+ seconds busy) :risk-s (- cost seconds)
                  :open-s (exposed-price c (spawn-light/night-chance p pos) busy pos) :morning-s (+ (if in-ground? seconds 0) (:morning-s after))}))
        into-home? (some? (sh-beds/closed-home c))
        sheltered? (if (or bed asleep?) (sh-beds/bed-sheltered? p at) (or (sh-beds/bed-sheltered? p feet) into-home?))
        body (respawn-body c)
        sealed (when (and carried? (not sheltered?)) (seal-plan c))
        out-s (when log-out? (get-in (await (ctx/quote c 'jobs.survival.log-out {})) [:uses :job-time 1]))
        here (await (dig-in-quote c))]
    (cond-> {:exposed (way-s {:open-s (open left)})}
      (js/isFinite (:seconds here)) (assoc :shelter (dig here))
      (or bed asleep? (and carried? (not (and home into-home?)))) (assoc :bed (+ (or walk-s 0) bed-lost (if refused? wait 0) (if sheltered? 0 (exposed-price c (spawn-light/night-chance p at) bed-open at)) (if asleep? 0 (- (cost/spawn-worth body at feet)))))
      sealed (assoc :sealed (+ lost (:seconds sealed) (exposed-price c chance (:seconds sealed) feet) (- (cost/spawn-worth body (:foot (:layout sealed)) feet))))
      (and niche (js/isFinite (:seconds niche))) (assoc :niche (dig niche))
      flee (assoc :flee (way-s (walk flee (flee-after flee here))))
      home (assoc :home (let [w (walk home (flee-after home here))]
                          (if carried?
                            (+ lost (- (way-s w) (way-s {})) (- (cost/spawn-worth body (:pos home) feet)))
                            (way-s w))))
      (number? out-s) (assoc :log-out out-s))))

(defn home-bound-s
  "Seconds of walk past which a :home answer (no bed carried) costs more than the least of prices (cost/night-walk-bound-s)."
  [c prices]
  (let [p (:primitives c)
        [_ left] (get-in (sh-clock/night-left-quote c) [:uses :job-time])]
    (cost/night-walk-bound-s (sh/in-ground? p (ctx/view c)) left (reduce min ##Inf prices))))

(defn mid-danger?
  "A hostile near (ready/threatened), burning, drowning, or off the ground (not in water, not riding): a log-out now
  leaves the danger waiting for the return."
  [c]
  (let [p (:primitives c)
        self (.self p)]
    (or (not= :ok (first (ready/threatened c)))
        (body/burning? p)
        (breath/drowning? p)
        (and (false? (.-onGround self)) (not (.-inWater self)) (nil? (.-vehicle self))))))

(defn log-out-open?
  "Whether a log-out is an answer now: another player sleeps and the body is the only one awake (sh-sleepers/log-out-for-sleepers?), and it is not mid-danger."
  [c]
  (and (sh-sleepers/log-out-for-sleepers? (:primitives c) (ctx/view c))
       (not (mid-danger? c))))

(def answer-order [:bed :sealed :log-out :home :shelter :niche :flee :exposed])

(defn cheapest-priced
  "The answer of prices ({answer seconds}) that costs least, the earlier of answer-order at a tie."
  [prices]
  (first (sort-by prices (filter prices answer-order))))

(defn cheapest
  "cheapest-priced, but :bed when overdue for sleep and a bed is priced (the rest keeps the phantoms off)."
  [c prices]
  (if (and (overdue? c) (:bed prices))
    :bed
    (cheapest-priced prices)))

(defn ^:async open-sleep-ok?
  "Whether to lie in a bed at bed that is not shut in: overdue, or lying there is the cheapest of answers (no walk: the
  body is there)."
  [c bed]
  (= :bed (cheapest c (await (answers c {:bed bed :log-out? (log-out-open? c)})))))

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
  (when-not (or (bed-to-use c) (sh-beds/place-wanted? c))
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
  (let [s (sh-beds/sleep-s c (bed-to-use c))]
    {:uses {:job-time [0 s (max (* 3 s) 10)]} :done [0 1 1]}))

(defn ^:async sleep!
  "jobs.survival.sleep on bed (open-ok?: also one not shut in a roofed room, open-sleep-ok?)."
  [c bed open-ok?]
  (let [p (:primitives c)
        started (ctx/now c)
        s (await (ctx/call c :sleep 'jobs.survival.sleep {:bed bed :open-bed open-ok?}))]
    (cond
      (= :continue s) :continue
      (= :bed-unreachable (contract/cause-key s)) :continue
      (= :bed-open (contract/cause-key s))
      (do (ctx/remember! c :bed-open {:pos bed} sh-beds/bed-open-policy)
          (ctx/end c :condition-failed :bed-open {:slept false :bed-open true :pos bed
                                                   :text "bed is open (not shut in a spawn-free roofed room); not lying in it"}))
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
  :continue while walking; :walk-failed when it failed (:home-walk-failed: closed-home counts it in the price of another walk); nil when no such home."
  [c]
  (when-let [home (sh-beds/closed-home c)]
    (let [w (await (child/call-walk* c :home-walk {:pos home :range 1}))]
      (cond
        (= :continue w) :continue
        (not= :done (:kind w)) (do (ctx/remember! c :home-walk-failed {:pos home} sh-beds/home-walk-failed-policy) :walk-failed)))))

(defn ^:async put-down-bed-here!
  "Walk to the layout's stand cell if the body is not on it, then place the carried bed on the foot cell (facing along
  the row) and record it as :bed unless a live :bed is recorded, and as a scaffold-ledger entry (purpose :bed) when outside the body's own zones.
  A placement the server refuses is counted (job memory :bed-refused) and tried again while fewer than ::bed-tries failed.
  :continue while walking or trying again; the foot cell when a bed block stands there after; else nil."
  [c]
  (let [p (:primitives c)
        item (sh-beds/carried-bed p)
        {:keys [stand foot] :as layout} (await (sense/decide! c ::layout #(or (sh-beds/bed-layout c true) (when-not (:roofed-only (:args c)) (sh-beds/bed-layout c false))) {:price (sense/fails-to c)}))]
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
  "put-down-bed-here!, after a walk into a home that is a closed roofed room when the body is outside it (:walk-failed when
  that walk failed: the bed is not put down on the price of a roofed place)."
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
      (= :walk-failed r) (ctx/end c :condition-failed :bed-open {:slept false :bed-open true :answer answer
                                                                :text "the walk into the closed home failed; the carried bed is priced again where the body stands"})
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
      (cheapest c (await (answers c {:log-out? (log-out-open? c)})))))

(defn ^:async round [c]
  (let [p (:primitives c)
        bed (or (some-> (:bed (:args c)) places/parse-pos :pos)
                (await (sense/decide! c ::bed #(sh-beds/bed-to-use p (ctx/view c) (radius c) (bed-permit c)) {:price (sense/fails-to c)})))
        answer (when (and (not bed) (not (:seal (ctx/mem c))) (sh-beds/place-wanted? c)) (await (carried-answer c)))]
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
