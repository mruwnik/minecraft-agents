(ns jobs.survival.leave-shelter
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.danger :as danger]
            [jobs.lib.dawn :as dawn]
            [jobs.lib.dig-in-leave :as dig-leave]
            [jobs.lib.escape :as escape]
            [jobs.lib.foods :as foods]
            [jobs.lib.reach :as reach]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.waits :as waits]
            [jobs.lib.shelter.clock :as sh-clock]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.shelter.spawn :as sh-spawn]
            [jobs.lib.spawn-light :as spawn-light]
            [jobs.lib.threat-field :as field]
            [jobs.lib.threats :as threats]
            [jobs.lib.util :as u]
            [engine.game :as game]
            [jobs.survival.night.hold :as hold]))

(def doc
  "The morning after a night: out of the shelter, the blocks it placed taken back, a bed put down picked up. The dawn
  trigger starts it; night itself ends when the body is sheltered or the night is over.
  Waits :night by night. Done once nothing is left: no shelter the body is shut in (jobs.lib.dig-in-leave/leave!: walls
  dug, a pit climbed, the shelter's valuable blocks taken back), no doorway the night filled (still holding a block it
  put there) to dig again, no bed it put down outside its own zones (never the live respawn bed: it stays) to pick up, no drop to walk
  over. A shelter with no way out ends nothing-possible :no-way-out (the cell is remembered as :shelter-trapped, so the
  trigger lets go). A shelter it left is remembered the same way, so a walk back through its cell is no new dawn. A body shut in stays while a known hostile is near and the dawn grace lasts (night.dawn-hold; longer with no weapon carried), and a fragile one (jobs.lib.foods/fragile?) after it too while a hostile could reach or shoot it were the shelter open, until the hold has cost as much as the cheapest way out (fragile-hold); it then leaves by that way.
  A doorway cell it could not open is warned night.doorway-stuck once and tried again quietly.
  The doorway blocks and the bed are scaffold-ledger entries (purposes :doorway, :bed); one jobs.access.cleanup call takes them back.
  Memory: job memory :dig-out, :take-back; body memory :shelter-trapped, :doorway-stuck-warned, :fragile-hold.")

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

(defn ^:async take-back!
  "Take back what the night put down outside its own zones, the doorway blocks it filled and the bed (scaffold ledger
  purposes :doorway and :bed), with one jobs.access.cleanup call that walks to them, digs and picks up the drops. Cells it
  leaves open are held by cleanup (10 minutes) and warned about once. :again."
  [c]
  (let [r (waits/waiting-out c (await (ctx/call c :take-back 'jobs.access.cleanup
                                                {:of :all :purpose #{:doorway :bed} :ignore-zones? true
                                                 :checks {:fit? '(always) :safe? '(not (threatened))}})))]
    (when-not (= :continue r)
      (sh-spawn/retract-bed! c (:removed (:data r)))
      (warn-stuck! c (let [held (ledger/held-cells (ctx/view c))]
                       (filterv #(held (:cell %)) (ledger/open-entries (ctx/view c))))))
    :again))

(def seal-purposes "Ledger purposes of the blocks that shut a body in for the night." #{:shelter :retreat-plug})

(defn seals
  "The open, unheld :shelter and :retreat-plug ledger entries within jobs.lib.ledger/seal-clear-radius of the feet while the body is enclosed, a cell it has not seen a wall (a
  seal left up shuts it in)."
  [c]
  (let [p (:primitives c)]
    (when (escape/enclosed? p nil :solid)
      (let [{:keys [x y z]} (sh-sense/feet p)
            held (ledger/held-cells (ctx/view c))]
        (seq (remove (some-fn (comp held :cell) (complement (comp seal-purposes :purpose)))
                     (ledger/near-of (ledger/open-entries (ctx/view c)) {:pos {:x x :y y :z z} :radius ledger/seal-clear-radius})))))))

(defn opened-cells
  "The cells a way out may open: the shelter's own blocks near the feet (seal-purposes) and every side and the top of
  the body's two cells, as [x y z]."
  [c]
  (let [{:keys [x y z]} (sh-sense/feet (:primitives c))
        own (keep #(when (seal-purposes (:purpose %)) (:cell %))
                  (ledger/near-of (ledger/open-entries (ctx/view c)) {:pos {:x x :y y :z z} :radius ledger/seal-clear-radius}))]
    (concat (map vec own)
            [[x (+ y 2) z]]
            (for [dy [0 1] [dx dz] [[1 0] [-1 0] [0 1] [0 -1]]] [(+ x dx) (+ y dy) (+ z dz)]))))

(defn fragile-dangers
  "The hostiles that could reach or shoot a fragile body (foods/fragile?: one hit may kill) were opened-cells open
  (jobs.lib.danger/dangers-opened, as cleanup keeps a seal)."
  [c]
  (let [p (:primitives c)]
    (when (foods/fragile? (.self p))
      (seq (danger/dangers-opened p threats/max-follow-range (danger/ranged-radius) (opened-cells c))))))

(defn exits
  "The headings out of the shelter, cheapest first, as {:heading :price :hurt}: cost/opening-price of the way through
  its cells (dig-leave/exit-cells, the mobs met spawning on them: spawn-light/spawns-over), the hurt of cost/route-danger from the hostiles that could reach or shoot the body
  were they open (jobs.lib.danger/dangers-opened); at one price (a hurt past the health), the least hurt first."
  [c]
  (let [p (:primitives c)
        self (.self p)
        body (assoc (cost/death-body p (ctx/view c)) :health (.-health self))
        weapon (combat/best-weapon p (combat/weapons c))]
    (sort-by (juxt :price :hurt)
             (for [h (keys dig-leave/headings)
                   :let [cells (dig-leave/exit-cells p h)
                         mobs (map (fn [m] {:name (.-name m) :pos (danger/mob-pos p m)})
                                   (danger/dangers-opened p threats/max-follow-range (danger/ranged-radius) cells))
                         lookups {:kind-at (danger/opened-lookup (reach/mob-lookup p) cells) :arrow-at (danger/opened-lookup (reach/arrow-lookup p) cells)}
                         hurt (:danger (cost/route-danger (game/version-of p) #(field/reach-fn (field/field p {:mob (:name %) :pos (:pos %)} lookups))
                                                          (map #(zipmap [:x :y :z] %) cells) mobs (.-equipment self)
                                                          :weapon weapon :health (.-health self)))]]
               {:heading h :price (cost/opening-price body hurt {:spawns (spawn-light/spawns-over p (spawn-light/walked (sh-sense/feet p) cells) nil)} (sh-sense/feet p)) :hurt hurt}))))

(def hold-policy
  "Policy of :fragile-hold, the start of the hold under way: it outlives the runs the dawn trigger fires again (a run's
  budget ends it, the hold does not), and step! forgets it once nothing holds the body."
  {:cap 1 :ttl :forever})

(defn fragile-hold
  "The fragile-dangers a fragile body stays shut in for after the dawn grace, while the hold since its :fragile-hold
  start (cost/hold-price: seconds held, hp lost) costs less than the cheapest of exits: waiting longer than leaving
  costs is never worth it when how long the danger stays is unknown."
  [c ds]
  (when ds
    (let [health (.-health (.self (:primitives c)))
          {:keys [since] :as start} (or (:data (ctx/latest c :fragile-hold)) {:since (ctx/now c) :health health})]
      (when (< (cost/hold-price {:held-s (/ (- (ctx/now c) since) 1000) :hp-lost (- (:health start) health) :health health})
               (:price (first (exits c))))
        (when-not (ctx/latest c :fragile-hold) (ctx/remember! c :fragile-hold start hold-policy))
        ds))))

(defn pending?
  "Whether the morning still has work (by night too, so a run then is no \"nothing left\"): a dig-out, a shelter to leave, a doorway the night filled to open, a
  bed put down to collect."
  [c]
  (let [p (:primitives c)
        m (ctx/mem c)]
    (boolean (or (:dig-out m)
                 (sh-inside/shut-in-own? p (:data (ctx/latest c :shelter)) (:data (ctx/latest c :shelter-trapped)))
                 (sh-beds/doorway-to-open p (ctx/view c))
                 (sh-beds/bed-to-collect p (ctx/view c))
                 (boolean (seals c))))))

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
        sealed (seals c)
        shelter (or doorway (dig-leave/sheltered-in c) sealed)
        fragile (when shelter (fragile-dangers c))
        grace (when shelter (seq (dawn/hostile-hold p (boolean (combat/best-weapon p (combat/weapons c))))))
        ds (when shelter (mapv #(dawn/describe p %) (or grace (fragile-hold c fragile))))]
    (when-not fragile (ctx/forget-where! c :fragile-hold (constantly true)))
    (cond
      (seq ds)
      (let [what (dawn/hostile-text ds)
            hp (.toFixed (.-health (.self p)) 1)]
        (when-not (:dawn-noted (ctx/mem c))
          (ctx/update-mem! c assoc :dawn-noted true)
          (ctx/emit! c :night.dawn-hold :info {:text (if grace
                                                       (str "first light, but a hostile is known near the shelter (" what "); staying in until it is gone or the grace is over")
                                                       (str "health " hp ": a hostile could reach or shoot the body were the shelter open (" what "); staying in until it cannot"))
                                               :hostiles ds}))
        (await (hold/hold! c (if grace (str "first light: " what) (str "health " hp ", " what)))))
      doorway (await (take-back! c))
      (and sealed fragile (not (:dig-out (ctx/mem c))) (not (dig-leave/sheltered-in c))
           (= :continue (await (dig-leave/level-exit! c (map :heading (exits c))))))
      :again
      (and sealed (not (:dig-out (ctx/mem c))) (not (dig-leave/sheltered-in c)))
      (let [r (await (ctx/call c :seal-cleanup 'jobs.access.cleanup
                               {:of :all :purpose seal-purposes :near {:pos (u/self-pos c) :radius ledger/seal-clear-radius} :checks {:fit? '(always) :safe? '(not (threatened))}}))]
        (if (and (not= :continue r) (some (comp #{:hostile-near :danger} :reason) (get-in r [:data :open])) (escape/enclosed? p nil :solid))
          (ctx/end c :nothing-possible :seal-kept {:text "still shut in by its own blocks, kept while a known hostile is near; giving the body back"})
          :again))
      (or (:dig-out (ctx/mem c)) shelter)
      (let [r (await (dig-leave/leave! c (when fragile {:order (map :heading (exits c))})))]
        (cond
          (= :continue r) :again
          (= :out (:reason r))
          (do (when-let [pos (:pos (:data (ctx/latest c :shelter)))] (ctx/remember! c :shelter-trapped {:pos pos} sh-inside/left-policy))
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
