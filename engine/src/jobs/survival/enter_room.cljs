(ns jobs.survival.enter-room
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as lb]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.checks :as checks]
            [jobs.lib.choose :as choose]
            [jobs.lib.combat :as combat]
            [jobs.lib.cost :as cost]
            [jobs.lib.cost.planner :as cost-planner]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.look :as look]
            [jobs.lib.look-edge :as look-edge]
            [jobs.lib.reach :as reach]
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
  A room walked to by :at whose inside was not seen emits enter-room.arrived {:seen :unseen :open-sides :closed-sides :closed} once its sides are all seen.
  Done {:pos :placed :roof} when the body is in a closed room (sh-sense/shut-in-room?) with no open door and no doorway gap, at once when it is.
  Nothing-possible: :no-room (no place known and the body not roofed), :no-walk (go-to did not arrive), :door-open, :no-blocks (a gap or hole
  and none carried), :roof-not-held (a mend that did not stay), :roof-not-placed (the block was not placed; or the fill's own reason), :open (the room cannot be closed).
  Quote (a flee entry skips the room and race checks: the flight judged them this step): go-to's route seconds (dark cells priced in; ##Inf with no route; a quote whose pick of the rooms seen searched this call prices the blocks to the room as a quote prices blocks not planned, cost.planner/left-s), or once in the room ::door-s for a door and ::fill-s for each fill; ##Inf when
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
   ::max-steps {:default 40 :doc "Steps (a walk, a door, a fill) a call takes before it gives up." :spec (a/int-in 1 nil)}
   ::max-mends {:default 3 :doc "Fills of the roof hole a call tries before it gives up." :spec (a/int-in 1 nil)}})

(def target [])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? ready/ok)

(def fit? ready/ok)

(defn blocks
  "The block names to close with: the call's :use :blocks, else every :build filler (read when needed: the entering digs in, and what it picks up there counts)."
  [c]
  (vec (or (ctx/use c :blocks) (choose/filler-names c :build))))

(defn have-blocks? [c] (boolean (or (room-lib/carried-door (:primitives c)) (seq (lb/carried c (blocks c))))))

(defn refused?
  "Whether another's zone, claim or plan refuses a block at pos: a room there is not walked into (a fill or a dig it needs would trespass)."
  [c pos]
  (boolean (access/trespass-refusal (assoc (access/rules-input c) :feet nil) :place pos)))

(defn seen-starts
  "The rooms behind the doors the body has seen as {:pos :door}, nearest door first, none that a zone refuses at either side
  of a door."
  [c]
  (let [starts (room-lib/starts (:primitives c) nil)
        refused-door? (fn [door] (some #(refused? c (:pos %)) (filter #(= door (:door %)) starts)))]
    (remove #(refused-door? (:door %)) starts)))

(defn seen-poses
  "Cells of the rooms behind the doors the body has seen (seen-starts)."
  [c]
  (map :pos (seen-starts c)))

(defn seen-doors "The doors seen behind which a room may be entered (seen-starts)." [c] (set (map :door (seen-starts c))))

(defn side-verdict
  "{:closed? :unseen?} of the room behind start {:pos}: a known closed room (refuge-room?), and that or one whose inside
  is not seen yet (the walk shows it; the open ground outside a door reads unseen too). Both from the floods of
  reach-room/look, kept per body: what room-lib/side-s prices."
  [p {:keys [pos]}]
  (let [look (reach-room/look p pos)
        closed? (room-lib/refuge-room? p pos look)]
    {:closed? closed? :unseen? (or closed? (some? @(:unseen look)))}))

(defn candidate?
  "Whether the room behind start {:pos} may be a closed room to enter (side-verdict's :closed?), or with unseen? one not
  seen inside yet (its :unseen?); the unseen flood only with unseen?."
  [p {:keys [pos]} unseen?]
  (let [look (reach-room/look p pos)]
    (or (room-lib/refuge-room? p pos look) (and unseen? (some? @(:unseen look))))))

(defn nearest-seen
  "{:pos} of the closed room to enter: the room picked by pick-seen (in the call's memory, or :room-pick of c for a quote)
  while it is still a candidate? of the doors seen, else the first seen start that is a known closed room, else the first
  whose inside is unseen (the nearest door by straight line, only until pick-seen has priced them), nil when none.
  Its floods are one search (room-lib/search-meter): starts past where it runs out are no candidate. Read-only (a quote
  runs this), so nothing is cached."
  [c]
  (let [p (:primitives c)
        starts (seen-starts c)
        picked (or (:room-pick c) (:picked (ctx/mem c)))]
    (binding [reach/*search* (room-lib/search-meter 0)]
      (some->> (or (some #(when (and (= picked (:pos %)) (candidate? p % true)) %) starts)
                   (some #(when (candidate? p % false) %) starts)
                   (some #(when (candidate? p % true) %) starts))
               :pos
               (hash-map :pos)))))

(defn ^:async pick-seen
  "{:pick {:pos} :done? :state}: the room whose door go-to quotes the cheapest route to, with the ways the entering walk has
  (digging through glass or a wall included, so a door two blocks off behind glass can beat one far round): a known closed
  room (candidate?), else one whose inside is unseen; :pick nil when none has a route. Doors nearest first: a door's floods of
  its sides (room-lib/side-s each) and its quote (room-lib/way-s) are paid only while they could save more than they cost
  (cost/search-pays?) over the cheapest closed room quoted, no route to the door quicker than its straight line at
  cost/least-route-s. A call's floods are one search (room-lib/search-meter) and its quote one of its own, the call's
  last, in a call no flood ran in: the rest waits, :done? false, and :state (kept by the caller, given back as kept)
  carries the doors judged and priced, each side judged (side-verdict, :side-js) and the picks on to the next call while
  the doors seen stay the same: the sides of a door a cut left unjudged are judged at the next call. :searched? when a
  flood or the quote ran."
  [c kept]
  (let [p (:primitives c)
        me (u/self-pos c)
        seen (seen-doors c)
        m (room-lib/search-meter 0)
        {:keys [priced best picks judged side-js]} (if (= seen (:doors kept)) kept {:priced #{} :best ##Inf :picks {} :judged {} :side-js {}})
        doors (->> (seen-starts c) (partition-by :door) (remove #(priced (:door (first %)))))
        answer (fn [done? priced best picks judged side-js]
                 {:pick (some->> (or (:closed picks) (:unseen picks)) first (hash-map :pos)) :done? done? :searched? (pos? (reach/spent-s m))
                  :state {:doors seen :priced priced :best best :picks picks :judged judged :side-js side-js}})]
    (loop [[sides & more :as left] doors best best picks picks priced priced judged judged side-js side-js]
      (if-not sides
        (answer true priced best picks judged side-js)
        (let [d (:door (first sides))
              j (get judged d)
              spent (reach/spent-s m)
              lb (cost/least-route-s (u/flat-dist me (room-lib/mid d)) 0.7)
              cost (+ (if j 0 (reduce + (map #(room-lib/side-s p (:pos %)) (remove #(contains? side-js (:pos %)) sides)))) (room-lib/way-s))]
          (cond
            (not (cost/under-ceiling? spent)) (answer false priced best picks judged side-js)
            (not (cost/search-pays? spent cost (- best lb))) (recur more best picks (conj priced d) judged side-js)
            (nil? j) (let [vs (binding [reach/*search* m]
                                (reduce (fn [vs {:keys [pos] :as s}]
                                          (if (contains? vs pos)
                                            vs
                                            (let [n (reach/cuts) v (side-verdict p s)]
                                              (if (= n (reach/cuts)) (assoc vs pos v) (reduced (vary-meta vs assoc :cut? true))))))
                                        side-js sides))
                           first-of (fn [k] (:pos (first (filter #(k (get vs (:pos %))) sides))))]
                       (if (:cut? (meta vs))
                         (answer false priced best picks judged (vary-meta vs dissoc :cut?))
                         (recur left best picks priced (assoc judged d {:closed (first-of :closed?) :unseen (first-of :unseen?)}) vs)))
            (not (or (:closed j) (and (:unseen j) (not (:closed picks))))) (recur more best picks (conj priced d) judged side-js)
            (pos? spent) (answer false priced best picks judged side-js)
            :else
            (let [start (or (:closed j) (:unseen j))
                  route (await (room-lib/route-cost c {:pos (room-lib/mid d) :range 0.7}))
                  k (if (:closed j) :closed :unseen)
                  picks (if (and route (< route (second (get picks k [nil ##Inf])))) (assoc picks k [start route]) picks)
                  best (if (and (:closed j) route) (min best route) best)]
              (assoc (answer (empty? more) (conj priced d) best picks judged side-js) :searched? true))))))))

(defn unclosable?
  "Whether the room seen at pos can be closed by nothing the job does: a seen room with an unroofed cell, no doorway that
  shuts it and, for a closed room, more than the one unroofed cell a plug mends (a roof on open sides, or walls under the
  sky). An unroofed cell whose roof column is unseen is walked to and looked at, but only while the room is closed: a flood
  that leaks through seen open cells is a roof on open sides, whatever its unseen roof. Read-only."
  [p pos]
  (let [{:keys [cell closed?]} (reach-room/unroofed-cell p pos)
        hidden? (and cell (look-edge/unseen-roof? p cell) closed?)]
    (boolean (and (not (reach-room/unseen-room? p pos))
                  cell (not hidden?) (nil? (room-lib/shut-by p pos cell any?))
                  (or (not (room-lib/refuge-room? p pos))
                      (< 1 (count (remove (fn [[x y z]] (sh-sense/roofed? p (sh-sense/room-roof-height) {:x x :y y :z z}))
                                          (:cells (reach-room/room p pos))))))))))

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

(defn picks-seen?
  "Whether the target is the nearest room seen (no :at, no :place, no usable home), of several doors, and no finished pick of it stands
  (pick-seen goes on while it is not done; a pick is made again whenever the doors seen change, so a newly seen cheaper door is priced)."
  [c]
  (let [t (target-of c)
        doors (seen-doors c)]
    (boolean (and t (nil? (:name t)) (not (:at (:args c)))
                  (next doors)
                  (not= doors (:picked-doors (ctx/mem c)))))))

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
  "Whether a creeper is known, or a melee hostile the body knows of (seen or heard, whatever its distance: race-won? prices the race) gets to a door of the room t before the body does."
  [c t]
  (let [p (:primitives c)
        threats (combat/hostiles p ##Inf)
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

(defn ^:async quote-of [c0]
  (let [k [::pick (seen-doors c0) (mapv js/Math.floor (map (sh-sense/feet (:primitives c0)) [:x :y :z]))]
        prev (when (picks-seen? c0) (check-cache/kept-value c0 k))
        ;; a pick not done goes on at the next quote from the state it left
        pick (when (picks-seen? c0) (if (:done? prev) prev (let [r (await (pick-seen c0 (:state prev)))] (check-cache/keep! c0 k r) r)))
        ;; a quote's searches are one (Q141): after the pick's floods or quote, the walk is priced as unplanned blocks
        searched? (and pick (not (:done? prev)) (:searched? pick))
        c (cond-> c0 pick (assoc :room-pick (:pos (:pick pick))))
        p (:primitives c)
        t (target-of c)
        job-time (settings/get settings ::job-time-s)
        priced (fn [mean] {:uses {:job-time [0 mean (if (infinite? mean) mean (max (* 3 mean) job-time))]} :done [0 1 1]})]
    (cond
      (and (shut? c) (nil? (room-lib/gap c))) (priced 0)
      (not (here? c t))
      (if (or (nil? t) (and (not (:flee? (:args c))) (or (unclosable? (:primitives c) (:pos t)) (race-lost? c t))))
        (priced ##Inf)
        (let [route (if searched?
                      (cost-planner/left-s (u/dist (u/self-pos c) (room-lib/mid (:pos t))) false)
                      (await (room-lib/route-cost c (walk-args t))))]
          (priced (or route ##Inf))))
      :else
      (let [{:keys [door gap plug stop]} (judge c)]
        (priced (cond stop ##Inf
                     door (settings/get settings ::door-s)
                     :else (* (count (or gap [plug])) (settings/get settings ::fill-s))))))))

(defn note-sides!
  "With :at, once a call: the sides of the room there seen and unseen as the walk starts (reach-room/boundary), when it is
  not seen inside yet; what enter-room.arrived reports beside how the room turned out."
  [c]
  (let [p (:primitives c)
        at (:at (:args c))]
    (when (and at (not (contains? (ctx/mem c) :sides)))
      (ctx/update-mem! c assoc :sides (when (reach-room/unseen-room? p at)
                                        (let [{:keys [seen unseen]} (reach-room/boundary p at)] {:seen seen :unseen (count unseen) :cells unseen}))))))

(defn arrived!
  "Once a call, in a room walked to not seen inside (note-sides!), once every side of it is seen (at the walk's arrival or
  after the look round): enter-room.arrived {:seen :unseen :open-sides :closed-sides :closed}, the sides as the walk started, how many of the
  unseen ones turned out open and walled, and whether the room is closed; the counts that measure cost/closed-chance's prior."
  [c]
  (let [p (:primitives c)
        feet (sh-sense/feet p)
        {:keys [sides arrived]} (ctx/mem c)]
    (when (and sides (not arrived) (here? c (target-of c)) (not (reach-room/unseen-room? p feet)))
      (ctx/update-mem! c assoc :arrived true)
      (let [closed (room-lib/refuge-room? p feet)
            {:keys [open] sealed :closed} (reach-room/open-sides p (:cells sides))]
        (ctx/emit! c :enter-room.arrived :info (assoc (dissoc sides :cells) :open-sides open :closed-sides sealed :closed closed :at (u/cell-vec (:at (:args c)))
                                                      :text (str "arrived in the room seen from outside: " (if closed "closed" "open"))))))))

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
      (:arrived (:data w)) (do (arrived! c) :again)
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
  (arrived! c)
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

(defn ^:async pick!
  "One call of pick-seen, its pick and state kept in mem (:picked, :pick-state; :picked-doors once done): whether it is done."
  [c]
  (let [doors (seen-doors c)
        {:keys [pick done? state]} (await (pick-seen c (:pick-state (ctx/mem c))))]
    (ctx/update-mem! c assoc :picked (:pos pick) :pick-state state :picked-doors (when done? doors))
    done?))

(defn ^:async act!
  "The step once the room is picked."
  [c]
  (when (here? c (target-of c))
    (when-let [roof (sh/unseen-roof-cell (:primitives c))]
      (when-not (= roof (:roof-looked (ctx/mem c)))
        (ctx/update-mem! c assoc :roof-looked roof)
        (await (sense/look-at! c roof)))))
  (if (and (here? c (target-of c)) (not (look/surveyed? c :cell)) (reach-room/unseen-room? (:primitives c) (sh-sense/feet (:primitives c))))
    (await (look/look-around! c))
    (await (step-judged! c))))

(defn ^:async step!
  "One step of the entering (judge): a slice of the pick of the room seen (pick!, the next step goes on with it), the
  walk, a door, the doorway or the roof, the end."
  [c]
  (note-sides! c)
  (if (and (picks-seen? c) (not (await (pick! c))))
    :again
    (await (act! c))))

(defn ^:async round [c] (await (pace/steps! c (fn ^:async enter-step [] (await (step! c))))))
