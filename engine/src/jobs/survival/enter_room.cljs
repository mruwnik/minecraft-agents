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
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spent :as spent]
            [jobs.lib.util :as u]
            [jobs.lib.waits :as waits]
            [jobs.survival.room-plug :as room-plug]
            [jobs.survival.plug-way :as way]
            [jobs.survival.retreat-doors :as retreat-doors]))

(def doc
  "Walk into a known closed room and close it: the :place (a remembered place name; default the home, or where the body
  is when it is roofed) is walked to (a go-to child, key :roof-walk), then an
  open door or gate of the room is shut (retreat-doors/shut-home-door!; a door still open once clicked writes
  :night-door-failed, which ends the job nothing-possible :door-open), a doorway gap in its walls is
  filled (jobs.blocks.fill-cells: a carried wooden door, else the call's :use :blocks; the filled cells are remembered as
  :doorway-filled, which night.morning digs again by day; a doorway tried once, or left open for want of blocks, is not tried again tonight, :doorway-tried) and a hole in
  the roof over the body is filled with one block (the cell is in :placed and the scaffold ledger as :shelter). Needs the
  blocks for a gap or a hole carried (none fetched), nothing for a walk or a door.
  Done {:pos :placed :roof} when the body is in a closed room (sh-sense/shut-in-room?) with no open door and no doorway gap, at once when it is.
  Nothing-possible: :no-room (no place known and the body not roofed), :no-walk (go-to did not arrive), :door-open, :no-blocks (a gap or hole
  and none carried), :failed (a fill that did not hold), :open (the room cannot be closed).
  Quote: go-to's route seconds (dark cells priced in; ##Inf with no route), or once in the room ::door-s for a door and ::fill-s for each fill; ##Inf when
  the place is not remembered, a creeper is near, or a melee hostile reaches a door of the room first (jobs.lib.rooms/race-won?).
  Picked by jobs.survival.shut-in as its :room way, and called by the night for a door, a doorway and the walk home to a bed.")

(a/defargs args
  {:place {:doc "the room to enter: a remembered place name (default the home)" :spec (a/or-of keyword? string?) :default nil}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds an entering may take." :spec (a/num-in 0 nil)}
   ::door-s {:default 2 :doc "Seconds shutting a door is guessed to take, for the quote." :spec (a/num-in 0 nil)}
   ::fill-s {:default 3 :doc "Seconds filling a doorway or a roof hole is guessed to take, for the quote." :spec (a/num-in 0 nil)}
   ::race-radius {:default 16 :doc "Blocks within which a hostile may reach a door of the room before the body does." :spec (a/num-in 0 nil)}
   ::max-steps {:default 40 :doc "Steps (a walk, a door, a fill) a call takes before it gives up." :spec (a/int-in 1 nil)}
   ::max-judged {:default 3 :doc "Doors seen whose room is judged (flooded) to find the nearest closed room when no place is remembered." :spec (a/int-in 0 nil)}
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

(defn nearest-seen
  "{:pos} of the nearest closed room behind one of the first ::max-judged doors the body has seen, nil when none. A door
  either side of which a zone refuses is skipped; a room whose inside is not seen yet counts (the walk shows it).
  Read-only (a quote runs this), so nothing is cached."
  [c]
  (let [p (:primitives c)
        starts (take (settings/get settings ::max-judged) (room-lib/starts p nil))
        refused-door? (fn [door] (some #(refused? c (:pos %)) (filter #(= door (:door %)) starts)))]
    (->> starts
         (remove #(refused-door? (:door %)))
         (keep (fn [{:keys [pos]}] (when (or (room-lib/refuge-room? p pos) (reach-room/unseen-room? p pos)) {:pos pos})))
         first)))

(defn target-of
  "{:name :pos} of the room to enter: the call's :place, else the home, else (no :place and no home remembered) the nearest
  closed room seen ({:pos} only); nil when none, or when a zone refuses the place."
  [c]
  (let [nm (or (:place (:args c)) :home)
        pos (mem/place (ctx/view c) nm)]
    (cond
      pos (when-not (refused? c pos) {:name nm :pos pos})
      (:place (:args c)) nil
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
                          (some? (room-plug/room-plug p (sh-sense/feet p) (way/roof-height)))))))))

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
                   (room-plug/room-plug p (sh-sense/feet p) (way/roof-height)))
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

(defn can-start?
  "A room to enter: a place, the home or a closed room seen, or the body already in one."
  [c]
  (if (or (target-of c) (here? c nil))
    (contract/ok :room "a room to enter")
    (contract/later :no-room "no room to enter: no place remembered or closed room seen that no zone refuses, and the body is not under a roof")))

(defn ^:async quote-of [c]
  (let [p (:primitives c)
        t (target-of c)
        job-time (settings/get settings ::job-time-s)
        priced (fn [mean] {:uses {:job-time [0 mean (if (infinite? mean) mean (max (* 3 mean) job-time))]} :done [0 1 1]})]
    (cond
      (and (shut? c) (nil? (room-lib/gap c))) (priced 0)
      (not (here? c t))
      (if (or (nil? t) (unclosable? (:primitives c) (:pos t)) (race-lost? c t))
        (priced ##Inf)
        (let [route (await (room-lib/route-cost c (walk-args t)))]
          (priced (or route ##Inf))))
      :else
      (let [{:keys [door gap plug stop]} (judge c)]
        (priced (cond stop ##Inf
                     door (settings/get settings ::door-s)
                     :else (* (count (or gap [plug])) (settings/get settings ::fill-s))))))))

(defn ^:async walk!
  "A go-to child to the target (key :roof-walk); :again once it arrived, :continue while it goes on, else the call ends :no-walk."
  [c t]
  (let [w (waits/waiting-out c (await (ctx/call c :roof-walk 'jobs.movement.go-to (walk-args t))))]
    (cond
      (= :continue w) :continue
      (:arrived (:data w)) :again
      :else (ctx/end c :nothing-possible :no-walk {:text (str "could not walk into the room" (when (:name t) (str " at " (name (:name t)))))
                                                   :cause (:cause w)}))))

(defn ^:async shut-door!
  "Shut an open door of the closed room the body stands in (retreat-doors/shut-home-door!: from where it stands within a hand's
  reach, else a step to the room cell inside it first). No zone rule applies: shutting a door changes no block. A door still
  open once clicked writes :night-door-failed for this door, so the night does not call the body safe
  behind it. :again."
  [c door]
  (await (retreat-doors/shut-home-door! c door))
  (when (and (some #(= (sh/door-key (:cell door)) (sh/door-key (:cell %))) (sh/open-doors (:primitives c) (ctx/view c)))
             (contains? (:doors-clicked (ctx/mem c) #{}) (retreat-doors/door-key (:cell door))))
    (ctx/remember! c :night-door-failed {:door (sh/door-key (:cell door))} {:cap 1 :ttl (sh/door-failed-ms)}))
  :again)

(defn remember-cells!
  "Remember each of cells as :doorway-filled {:cell :blocks}, once, before it is filled with one of blocks: a fill cut
  short is opened by day too."
  [c cells blocks]
  (doseq [cell cells
          :when (not-any? #(= cell (:cell (:data %))) (ctx/entries c :doorway-filled))]
    (ctx/remember! c :doorway-filled {:cell cell :blocks blocks} sh-beds/doorway-filled-policy)))

(defn ^:async fill-gap!
  "Fill the doorway gap (jobs.blocks.fill-cells: a carried wooden door, else the call's blocks). Once it ends, the doorway is
  tried for tonight, filled or not, and each cell's :doorway-filled entry keeps the block it now holds, or is forgotten when it
  holds none of them. :again."
  [c cells]
  (let [door (room-lib/carried-door (:primitives c))
        blocks (if door [door] (blocks c))
        _ (remember-cells! c cells blocks)
        r (await (ctx/call c :shut-doorway 'jobs.blocks.fill-cells (cond-> {:cells (vec cells) :ignore-zones? true} (seq blocks) (assoc :use {:blocks blocks}))))]
    (when-not (= :continue r)
      (ctx/forget-where! c :doorway-filled #(some #{(:cell %)} cells))
      (doseq [cell cells
              :let [n (u/seen-name (:primitives c) cell)]
              :when (some #{n} blocks)]
        (ctx/remember! c :doorway-filled {:cell cell :blocks [n]} sh-beds/doorway-filled-policy))
      (ctx/remember! c :doorway-tried (first cells) room-lib/doorway-tried-policy))
    (if (= :continue r) :continue :again)))

(defn ^:async mend!
  "Fill the hole in the roof over the body with one block (fill-cells); the cell goes to :placed and the ledger as :shelter. :again."
  [c plug]
  (let [item (lb/pick c (blocks c))
        mends (:mends (ctx/mem c) 0)]
    (ctx/update-mem! c update :mends (fnil inc 0))
    (if (>= mends (settings/get settings ::max-mends))
      (ctx/end c :nothing-possible :failed {:text "cannot mend the roof: the block does not stay"})
      (let [r (await (ctx/call c :mend 'jobs.blocks.fill-cells {:cells [plug] :ignore-zones? true :use {:blocks [item]}}))
            at (u/cell-vec plug)]
        (cond
          (= :continue r) :continue
          (not= (u/seen-name (:primitives c) plug) item)
          (ctx/end c :nothing-possible :failed {:text (str "cannot mend the roof at " (pr-str at) ": " (or (:text (:data r)) "the block was not placed"))})
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
      :else (do (when-let [g (and (= :no-blocks (:stop j)) (room-lib/gap c))]
                  (ctx/remember! c :doorway-tried (first g) room-lib/doorway-tried-policy))
                (ctx/end c :nothing-possible (:stop j) (merge {:text (:text j) :placed (:placed (ctx/mem c) [])} (spent/summary (spent/log c))))))))

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
