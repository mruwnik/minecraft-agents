(ns jobs.survival.enter-room
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.checks :as checks]
            [jobs.lib.combat :as combat]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.look :as look]
            [jobs.lib.look-edge :as look-edge]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.rooms :as room-lib]
            [jobs.lib.sense :as sense]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spent :as spent]
            [jobs.lib.util :as u]
            [jobs.lib.waits :as waits]
            [jobs.survival.room-plug :as room-plug]
            [jobs.survival.respond.doors :as respond.doors]
            [jobs.survival.respond.options :as options]))

(def doc
  "Walk into a known closed room and close it: the :place (a remembered place name; default the home, or where the body
  is when it is roofed) is walked to (a go-to child, key :roof-walk), then an
  open door or gate of the room is shut (respond.doors/shut-home-door!; a door still open once clicked writes
  :night-door-failed, which ends the job nothing-possible :door-open), a doorway gap in its walls is
  filled (jobs.blocks.fill-cells: a carried wooden door, else the call's :use :blocks; the filled cells go into the scaffold ledger as :doorway,
  which survival.leave-shelter takes back by day (jobs.access.cleanup); a doorway tried once, or left open for want of blocks, is not tried again tonight, :doorway-tried) and a hole in
  the roof over the body is filled with one block (the cell is in :placed and the scaffold ledger as :shelter). Needs the
  blocks for a gap or a hole carried (none fetched; in the room already it waits :no-blocks at the start), nothing for a walk or a door.
  Done {:pos :placed :roof} when the body is in a closed room (sh-sense/shut-in-room?) with no open door and no doorway gap, at once when it is.
  Nothing-possible: :no-room (no place known and the body not roofed), :no-walk (go-to did not arrive), :door-open, :no-blocks (a gap or hole
  and none carried), :roof-not-held (a mend that did not stay), :roof-not-placed (the block was not placed; or the fill's own reason), :open (the room cannot be closed).
  Quote (a flee entry skips the room and race checks: the flight judged them this step): go-to's route seconds (dark cells priced in; ##Inf with no route), or once in the room ::door-s for a door and ::fill-s for each fill; ##Inf when
  the place is not remembered, a creeper is near, or a melee hostile reaches a door of the room first (jobs.lib.rooms/race-won?).
  Picked by jobs.survival.shut-in as its :room way, and called by the night for a door, a doorway and the walk home to a bed.")

(a/defargs args
  {:at {:doc "a cell inside the room to enter, instead of a place" :spec ::a/pos :default nil}
   :flee? {:doc "walk in as a flee step: legs of jobs.movement.run-away (:leg-s seconds, no digging or pillaring), one leg a round; for a body running from a hostile" :spec boolean? :default false}
   :leg-s {:doc "seconds one flee leg may take; nil: the respond.options flight step setting" :spec (a/num-in 0.1 120) :default nil}
   :place {:doc "the room to enter: a remembered place name (default the home)" :spec (a/or-of keyword? string?) :default nil}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds an entering may take." :spec (a/num-in 0 nil)}
   ::door-s {:default 2 :doc "Seconds shutting a door is guessed to take, for the quote." :spec (a/num-in 0 nil)}
   ::fill-s {:default 3 :doc "Seconds filling a doorway or a roof hole is guessed to take, for the quote." :spec (a/num-in 0 nil)}
   ::race-radius {:default 16 :doc "Blocks within which a hostile may reach a door of the room before the body does." :spec (a/num-in 0 nil)}
   ::max-steps {:default 40 :doc "Steps (a walk, a door, a fill) a call takes before it gives up." :spec (a/int-in 1 nil)}
   ::max-mends {:default 3 :doc "Fills of the roof hole a call tries before it gives up." :spec (a/int-in 1 nil)}})

(def target [])

(def defaults {:use {:blocks :build} :spend {:job-time ::job-time-s}})

(def safe? ready/ok)

(def fit? ready/ok)

(defn blocks [c] (vec (ctx/use c :blocks)))

(defn have-blocks? [c] (boolean (or (room-lib/carried-door (:primitives c)) (seq (lb/carried c (blocks c))))))

(defn refused?
  "Whether another's zone, claim or plan refuses a block at pos: a room there is not walked into (a fill or a dig it needs would trespass)."
  [c pos]
  (boolean (access/trespass-refusal (assoc (access/rules-input c) :feet nil) :place pos)))

(defn seen-poses
  "Cells of the rooms behind the doors the body has seen, nearest door first, none that a zone refuses at either side of a
  door (starts reads at most the jobs.lib.rooms max-doors doors)."
  [c]
  (let [starts (room-lib/starts (:primitives c) nil)
        refused-door? (fn [door] (some #(refused? c (:pos %)) (filter #(= door (:door %)) starts)))]
    (->> starts (remove #(refused-door? (:door %))) (map :pos))))

(defn nearest-seen
  "{:pos} of the first closed room behind a door the body has seen, nil when none; a room whose inside is not seen yet counts
  (the walk shows it), but only when no door side is a known closed room (the open ground outside a door reads unseen too).
  Door order, not price: the night picks by go-to's quotes (night/walkable-far-home). Read-only (a quote runs this), so nothing is cached."
  [c]
  (let [p (:primitives c)
        poses (seen-poses c)]
    (some->> (or (first (filter #(room-lib/refuge-room? p %) poses))
                 (first (filter #(reach-room/unseen-room? p %) poses)))
             (hash-map :pos))))

(defn unclosable?
  "Whether the room seen at pos can be closed by nothing the job does: a seen room that is no closed room, with an
  unroofed cell and no doorway that shuts it (a roof on open sides). An unroofed cell whose roof column is unseen is
  walked to and looked at, but only while the room is closed: a flood that leaks through seen open cells is a roof on open
  sides, whatever its unseen roof. Read-only."
  [p pos]
  (let [{:keys [cell closed?]} (reach-room/unroofed-cell p pos)
        hidden? (and cell (look-edge/unseen-roof? p cell) closed?)]
    (boolean (and (not (room-lib/refuge-room? p pos)) (not (reach-room/unseen-room? p pos))
                  cell (not hidden?) (nil? (room-lib/shut-by p pos cell any?))))))

(defn target-of
  "{:name :pos} of the room to enter: the call's :place, else the home, else (no :place, and no home remembered or the home
  open ground, like a bed set down outside the house) the nearest closed room seen ({:pos} only); nil when none, or when a
  zone refuses the place."
  [c]
  (let [nm (or (:place (:args c)) :home)
        pos (mem/place (ctx/view c) nm)]
    (cond
      (:at (:args c)) (let [at (:at (:args c))] (when-not (refused? c at) {:pos at}))
      (:place (:args c)) (when (and pos (not (refused? c pos))) {:name nm :pos pos})
      (and pos (not (unclosable? (:primitives c) pos))) (when-not (refused? c pos) {:name nm :pos pos})
      :else (nearest-seen c))))

(defn at-target?
  "Whether the body is within a step and a half of the place, or in the room that holds it (read-only: a quote runs this)."
  [c {:keys [pos]}]
  (let [p (:primitives c)]
    (boolean (or (<= (u/dist (u/self-pos c) (room-lib/mid pos)) 1.5)
                 (some->> (reach-room/room p pos) (room-lib/inside? p))))))

(defn here?
  "Whether the body is where it enters: at the target, or, with no :place, under a roof or in a closed room whose roof has a hole."
  [c t]
  (let [p (:primitives c)]
    (boolean (or (and t (at-target? c t))
                 (and (not (:place (:args c)))
                      (or (sh-sense/roofed? p (sh-sense/room-roof-height)) (sh-sense/shut-in-room? p)
                          (some? (room-plug/room-plug p (sh-sense/feet p) (sh-sense/roof-height)))))))))

(defn shut?
  "Whether the body is in a closed room (sh-sense/shut-in-room?) with no open door."
  [c]
  (let [p (:primitives c)]
    (boolean (and (not (sh-sense/head-in-block? p))
                  (empty? (sh/open-room-doors p))
                  (sh-sense/shut-in-room? p)))))

(defn judge
  "What the world asks next: {:walk target} {:door d} {:gap cells} {:plug cell} {:done true} or {:stop cause :text}."
  [c]
  (let [p (:primitives c)
        t (target-of c)]
    (if-not (here? c t)
      (if t {:walk t} {:stop :no-room :text "no room to enter: no place remembered or closed room seen that no zone refuses, and the body is not under a roof"})
      (let [door (sh/open-room-door p (ctx/view c))
            gap (room-lib/gap c)
            plug (when (and (not (shut? c)) (not gap))
                   (room-plug/room-plug p (sh-sense/feet p) (sh-sense/roof-height)))
            blocks? (have-blocks? c)]
        (cond
          door {:door door}
          (and gap blocks?) {:gap gap}
          (and plug blocks?) {:plug plug}
          (or gap plug) {:stop :no-blocks :text "no block carried to close the room with"}
          (shut? c) {:done true}
          (seq (sh/open-room-doors p)) {:stop :door-open :text "an open door of the room would not shut"}
          :else {:stop :open :text "the room cannot be closed here"})))))

(defn done? [c]
  (let [p (:primitives c)
        done (and (here? c (target-of c)) (shut? c) (nil? (room-lib/gap c)))]
    (checks/goal done [:in-room "the body is in a closed room with its doors shut"] [:open "the room is not closed"]
                 {:units :rooms :done (if done 1 0) :left (if done 0 1)
                  :result (cond-> (merge {:pos (sh-sense/feet p) :placed (:placed (ctx/mem c) [])} (spent/summary (spent/log c)))
                            (:roof (ctx/mem c)) (assoc :roof (:roof (ctx/mem c))))})))

(defn walk-args [{:keys [name pos]}]
  (if name {:place name :range 0} {:pos (room-lib/mid pos) :range 0.7}))

(defn race-lost?
  "Whether a creeper is near, or a melee hostile gets to a door of the room t before the body does."
  [c t]
  (let [p (:primitives c)
        threats (combat/hostiles p (settings/get settings ::race-radius))
        room (reach-room/room p (:pos t))
        way (when room (room-lib/way-in p room))]
    (boolean (and (seq threats)
                  (or (some combat/creeper? threats)
                      (and way (not (room-lib/race-won? p room way threats))))))))

(defn can-start?
  "A room to enter: a place, the home or a closed room seen, or the body already in one; and, in it already, a block to
  close its doorway or roof hole with unless a door is to be shut."
  [c]
  (let [t (target-of c)]
    (cond
      (not (or t (here? c nil)))
      (contract/later :no-room "no room to enter: no place remembered or closed room seen that no zone refuses, and the body is not under a roof")
      (and (here? c t) (= :no-blocks (:stop (judge c))))
      (contract/later :no-blocks "no block carried to close the room with")
      :else (contract/ok :room "a room to enter"))))

(defn ^:async quote-of [c]
  (let [p (:primitives c)
        t (target-of c)
        job-time (settings/get settings ::job-time-s)
        priced (fn [mean] {:uses {:job-time [0 mean (if (infinite? mean) mean (max (* 3 mean) job-time))]} :done [0 1 1]})]
    (cond
      (and (shut? c) (nil? (room-lib/gap c))) (priced 0)
      (not (here? c t))
      (if (or (nil? t) (and (not (:flee? (:args c))) (or (unclosable? (:primitives c) (:pos t)) (race-lost? c t))))
        (priced ##Inf)
        (let [route (await (room-lib/route-cost c (walk-args t)))]
          (priced (or route ##Inf))))
      :else
      (let [{:keys [door gap plug stop]} (judge c)]
        (priced (cond stop ##Inf
                     door (settings/get settings ::door-s)
                     :else (* (count (or gap [plug])) (settings/get settings ::fill-s))))))))

(defn ^:async flee-leg!
  "One jobs.movement.run-away leg (key :roof-walk) toward the room's middle: :again once arrived, :continue after a leg that
  got nearer, nil otherwise."
  [c t]
  (let [w (await (ctx/call c :roof-walk 'jobs.movement.run-away
                           {:pos (room-lib/mid (:pos t)) :range 0.7 :leg-s (or (:leg-s (:args c)) (options/flight-timeout-s))}))]
    (cond
      (= :continue w) :continue
      (:arrived (:data w)) :again
      (and (= :limit-reached (:kind w)) (:nearer (:data w))) :continue)))

(defn ^:async walk!
  "A go-to child to the target (key :roof-walk; with :flee? a run-away leg a round); :again once it arrived, :continue while it goes on, else the call ends :no-walk."
  [c t]
  (let [w (if (:flee? (:args c))
            (or (await (flee-leg! c t)) :stuck)
            (waits/waiting-out c (await (ctx/call c :roof-walk 'jobs.movement.go-to (walk-args t)))))]
    (cond
      (= :continue w) :continue
      (= :again w) :again
      (:arrived (:data w)) :again
      :else (ctx/end c :nothing-possible :no-walk {:text (str "could not walk into the room" (when (:name t) (str " at " (name (:name t)))))
                                                   :cause (:cause w)}))))

(defn ^:async shut-door!
  "Shut an open door of the closed room the body stands in (respond.doors/shut-home-door!: from where it stands within a hand's
  reach, else a step to the room cell inside it first). No zone rule applies: shutting a door changes no block. A door still
  open once clicked writes :night-door-failed for this door, so the night does not call the body safe
  behind it. :again."
  [c door]
  (await (respond.doors/shut-home-door! c door))
  (when (and (some #(= (sh/door-key (:cell door)) (sh/door-key (:cell %))) (sh/open-doors (:primitives c) (ctx/view c)))
             (contains? (:doors-clicked (ctx/mem c) #{}) (respond.doors/door-key (:cell door))))
    (ctx/remember! c :night-door-failed {:door (sh/door-key (:cell door))} {:cap 1 :ttl (sh/door-failed-ms)}))
  :again)

(defn ^:async fill-gap!
  "Fill the doorway gap (jobs.blocks.fill-cells: a carried wooden door, else the call's blocks), the placed blocks entered in the
  ledger as :doorway. Once it ends, the doorway is tried for tonight, filled or not, except after a hostile stood in it (the hostile reflex deals with that, the doorway is tried again). :again."
  [c cells]
  (let [door (room-lib/carried-door (:primitives c))
        blocks (if door [door] (blocks c))
        r (waits/waiting-out c (await (ctx/call c :shut-doorway 'jobs.blocks.fill-cells (cond-> {:cells (vec cells) :danger? true :ignore-zones? true :purpose :doorway} (seq blocks) (assoc :use {:blocks blocks})))))]
    (when-not (or (= :continue r) (= :hostile-in-cell (get-in r [:cause :reason])))
      (ctx/remember! c :doorway-tried (first cells) room-lib/doorway-tried-policy))
    (if (= :continue r) :continue :again)))

(defn ^:async mend!
  "Fill the hole in the roof over the body with one block (fill-cells); the cell goes to :placed and the ledger as :shelter. :again."
  [c plug]
  (let [item (lb/pick c (blocks c))
        mends (:mends (ctx/mem c) 0)]
    (ctx/update-mem! c update :mends (fnil inc 0))
    (if (>= mends (settings/get settings ::max-mends))
      (ctx/end c :nothing-possible :roof-not-held {:text "cannot mend the roof: the block does not stay" :cell (u/cell-vec plug)})
      (let [r (waits/waiting-out c (await (ctx/call c :mend 'jobs.blocks.fill-cells {:cells [plug] :danger? true :ignore-zones? true :use {:blocks [item]}})))
            at (u/cell-vec plug)]
        (cond
          (= :continue r) :continue
          (not= (u/seen-name (:primitives c) plug) item)
          (ctx/end c :nothing-possible (or (get-in r [:cause :reason]) :roof-not-placed) {:cell at :text (str "cannot mend the roof at " (pr-str at) ": " (or (:text (:data r)) "the block was not placed"))})
          :else (do (ctx/update-mem! c update :placed (fnil conj []) {:cell at :item item})
                    (spent/note! c item at)
                    (ctx/update-mem! c assoc :roof plug)
                    (ledger/remember! c (-> (ledger/intend (ledger/open-entries (ctx/view c)) {:cell at :item item :before "air" :job (:id c) :purpose :shelter})
                                            (ledger/confirm at)))
                    :again))))))

(defn ^:async step-judged!
  "The step after the looks: judge, then act on it."
  [c]
  (let [j (judge c)
        n (:steps (ctx/mem c) 0)]
    (ctx/update-mem! c assoc :steps (inc n))
    (cond
      (>= n (settings/get settings ::max-steps)) (ctx/end c :nothing-possible :stuck {:text "gave up: too many steps without closing the room"})
      (:walk j) (await (walk! c (:walk j)))
      (:door j) (await (shut-door! c (:door j)))
      (:gap j) (await (fill-gap! c (:gap j)))
      (:plug j) (await (mend! c (:plug j)))
      (:done j) (ctx/end c :done nil (cond-> (merge {:pos (sh-sense/feet (:primitives c)) :placed (:placed (ctx/mem c) [])} (spent/summary (spent/log c)))
                                         (:roof (ctx/mem c)) (assoc :roof (:roof (ctx/mem c)))))
      :else (ctx/end c :nothing-possible (:stop j) (merge {:text (:text j) :placed (:placed (ctx/mem c) [])} (spent/summary (spent/log c)))))))

(defn ^:async step!
  "One step of the entering (judge): the walk, a door, the doorway or the roof, the end."
  [c]
  (when (here? c (target-of c))
    (when-let [roof (sh/unseen-roof-cell (:primitives c))]
      (when-not (= roof (:roof-looked (ctx/mem c)))
        (ctx/update-mem! c assoc :roof-looked roof)
        (await (sense/look-at! c roof)))))
  (if (and (here? c (target-of c)) (not (look/surveyed? c :cell)) (reach-room/unseen-room? (:primitives c) (sh-sense/feet (:primitives c))))
    (await (look/look-around! c))
    (await (step-judged! c))))

(defn ^:async round [c] (await (pace/steps! c (fn ^:async enter-step [] (await (step! c))))))
