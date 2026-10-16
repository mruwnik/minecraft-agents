(ns jobs.access.clear-path
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.access :as access]
            [jobs.lib.blocks :as b]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.reach :as reach]
            [jobs.lib.child :as child]
            [jobs.lib.escape :as escape]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(def doc
  "Dig a door 1 wide and 2 high through the wall straight ahead along :heading, then step through it.
  - The wall is at most :max-thick blocks thick, starts right in front of the body, and has a floor under every
    row and room for the body beyond it (jobs.lib.escape/door). A cell it has not seen is rock (one not loaded ends :no-door); a far side it cannot
    see is guessed, and once the cells before it are dug it plans on from there with what it sees.
  - Never digs a door, gate, trapdoor, bed, container, sign or an unbreakable block.
  - Each cell is a jobs.blocks.dig child (zones, claims, hazards and tools are its rules). It picks up the drop
    (:collect) when there is room for it, so the block can be put back; with no room it digs on and leaves it.
  - Then it walks into the cell beyond (a go-to child).
  One call is the whole door; it yields :continue only while a dig child waits on the world.
  Needs nothing carried (a tool the dig child cannot find is its :dig-waits). Quote: the door's cells times the time of a
  cell, ending at the far side.
  :bad-args and :no-door are conditions, checked at the start. Ends :done, or with a cause {:reason r ...}: :impossible for :bad-args (needs :heading and :max-thick > 0), :condition-failed for
  :no-door (no such wall here), :protected (a block it never digs), :footprint (a cell of a plan this body made,
  unless :spare-own-builds is false) and :dig-waits (the dig child's wait or decline, as :wait); :nothing-possible for
  :dig-failed (its ending, as :dig), :refills (a cell dug max-cell-digs times, e.g. sand falling in) and :step-failed
  (the walk through did not arrive). Data {:dug [{:cell :block}] :at [x y z] :through [x y z]}, also as a
  clear-path.done info or clear-path.stopped warn event.
  It does not put the blocks back: jobs.movement.go-to does that once through.
  :need-drop false: a block no carried tool harvests is dug bare-handed (slow, the drop is lost) instead of stopping :dig-waits
  :no-tool; go-to's last resort when it has no pickaxe. A tool the dig lacks is fetched (:fetch, jobs.lib.need) unless :fetch is off.")

(a/defargs args
  {:heading {:doc ":north :east :south or :west" :spec #{:north :east :south :west} :default nil}
   :max-thick {:doc "the thickest wall it digs through, in blocks" :spec (a/int-in 0 nil) :default 3}
   :note {:doc "a map: each cell dug is written to the tidy ledger at once with it (jobs.lib.escape/note-hole!; go-to's escalation)" :spec map? :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}
   :fetch {:doc "get a missing tool for the dig children with jobs.lib.need: true, a set of kinds or a map of limits; false waits :dig-waits" :spec need/option? :default true}
   :need-drop {:doc "false: dig a block no carried tool harvests bare-handed (slow, the drop is lost)" :spec boolean? :default true}
   :spare-own-builds {:doc "a cell of a plan this body made is never dug (stops :footprint naming the plan); false: it may be" :spec boolean? :default true}})

(def headings {:north [0 -1] :south [0 1] :east [1 0] :west [-1 0]})

(a/defargs settings
  {::job-time-s {:default 300 :doc "Seconds one call may take before it gives up (:limit-reached)." :spec (a/num-in 0 nil)}
   ::max-cell-digs {:default 8 :doc "Digs of one cell (a falling block refills it) before it stops :refills." :spec (a/int-in 0 nil)}
   ::cell-s {:default 1.5 :doc "Mean seconds one cell of the door takes (dig, pick up, pace), for the quote." :spec (a/num-in 0 nil)}})
(defn max-cell-digs [] (settings/get settings ::max-cell-digs))

(def safe? ready/safe-here-by-night)

(def fit? ready/ok)

(def target [:heading])

(def bad-args
  "Never :bad-args unless :heading and a positive :max-thick are given."
  (checks/args-fit "needs :heading (:north :east :south :west) and :max-thick > 0"
                   (fn [c] (let [{:keys [heading max-thick]} (:args c)] (and (headings heading) (pos-int? max-thick))))))

(defn no-door-why [heading max-thick p]
  (str "no wall at most " max-thick " thick with a floor beyond, " (name heading) " of " (pr-str (reach/standing-vec p))))

(defn no-door
  "Later :no-door when no wall of at most :max-thick with a floor beyond lies ahead on the map as seen."
  [c]
  (let [{:keys [heading max-thick]} (:args c)
        p (:primitives c)]
    (if (and (headings heading) (pos-int? max-thick)
             (nil? (escape/door (escape/block-at-of p) (escape/unseen-of p) (reach/standing-vec p) (headings heading) max-thick)))
      (contract/later :no-door (no-door-why heading max-thick p) :why (no-door-why heading max-thick p))
      (contract/ok :door "a wall to clear"))))

(def defaults {:spend {:job-time ::job-time-s}})

(def can-start? (cond/and bad-args no-door))

(defn quote-of [c]
  (let [{:keys [heading max-thick]} (:args c)
        p (:primitives c)
        {:keys [cells through]} (when (headings heading)
                                  (escape/door (escape/block-at-of p) (escape/unseen-of p) (reach/standing-vec p) (headings heading) max-thick))
        s (* (count cells) (settings/get settings ::cell-s))]
    (cond-> {:uses {:job-time [s (* 1.5 s) (* 4 s)]}}
      through (assoc :moves-to through))))

(def facts
  "The stops that name a wall or a rule that bars the door (condition-failed); the others are attempts that failed."
  #{:no-door :protected :footprint :dig-waits})

(defn result-of
  "The data of the door now: {:dug :at} and the :through cell once planned, plus detail."
  [c detail]
  (let [m (ctx/mem c)]
    (merge {:dug (:dug m []) :at (reach/standing-vec (:primitives c))}
           (when-let [t (:through m)] {:through t})
           detail)))

(defn done?
  "Ok once the body stands on the far cell and every cell the call planned is seen open now (a fresh call has no plan:
  later); else the cells dug against the cells planned."
  [c]
  (let [m (ctx/mem c)
        p (:primitives c)
        cells (:cells m)
        open? (fn [cell] (b/air (u/seen-name p (zipmap [:x :y :z] cell))))]
    (if (and (seq cells) (:through m) (= (:through m) (reach/standing-vec p)) (every? open? cells))
      (contract/ok :done (str "cleared a way through to " (pr-str (:through m))) :units :cells :done (count cells) :left 0
                   :result (result-of c {}))
      (contract/later :digging "the door's cells are being dug"
                      :units :cells :done (count (:dug m)) :left (max 0 (- (count cells) (count (:dug m))))))))

(defn ^:async cleanup
  "After a door that ended :done: clear-path.done told."
  [c ending]
  (when (= :done (:kind ending))
    (let [data (:data ending)]
      (ctx/emit! c :clear-path.done :info (assoc data :status :done :reason :done
                                                 :text (str "cleared a way through to " (pr-str (:through data)))))))
  (ctx/end c :done nil {}))

(defn finish!
  "End the door: a stop tells its event and ends by reason a kind (facts condition-failed, the rest nothing possible),
  the old result without :status and :reason as the ending's data. :done ends with the data; its event is cleanup's."
  [c reason detail]
  (let [data (result-of c detail)]
    (if (= :done reason)
      (ctx/end c :done nil data)
      (do (ctx/emit! c :clear-path.stopped :warn
                     (assoc data :status :stopped :reason reason
                            :text (str "clear-path stopped: " (name reason)
                                       (some->> (:cell detail) pr-str (str " at ")))))
          (ctx/end c (if (facts reason) :condition-failed :nothing-possible) (assoc detail :reason reason) data)))))

(defn ^:async plan! [c]
  (let [dir (headings (:heading (:args c)))
        max-thick (:max-thick (:args c))
        p (:primitives c)]
    (if-let [{:keys [cells through unseen]} (escape/door (escape/block-at-of p) (escape/unseen-of p) (reach/standing-vec (:primitives c)) dir max-thick)]
      (do (ctx/update-mem! c assoc :cells cells :through through :unseen unseen :from (reach/standing-vec (:primitives c)) :dug [])
          :again)
      (finish! c :no-door {:why (no-door-why (:heading (:args c)) max-thick p)}))))

(defn room-for-drop?
  "A free slot, or a carried stack of what block drops (b/drops-of) under 64."
  [p block]
  (let [drops (set (b/drops-of p block))]
    (boolean (or (pos? (u/free-slots p))
                 (some #(and (drops (:name %)) (< (:count %) 64)) (u/inventory p))))))

(defn own-build
  "The refusal naming the plan this body made that covers cell, unless :spare-own-builds is false; else nil."
  [c cell]
  (when-not (false? (:spare-own-builds (:args c)))
    (access/own-keep (access/rules-input c {:spare-own? true}) (zipmap [:x :y :z] cell))))

(defn declined?
  "Whether the dig child's ending e is a refusal at its start (no data), not a dig that failed."
  [e]
  (and (= :condition-failed (:kind e)) (nil? (:data e))))

(defn wait-of [e] (let [cause (:cause e)] (if (map? cause) cause {:reason cause})))

(defn ^:async dig-cell!
  "One dig child call on the cell being dug (:digging {:cell :block :collect}, kept until the child ends, so a resumed
  call goes on with it). :again, :continue while the child waits on the world, or the ending."
  [c {:keys [cell block collect]}]
  (let [hand? (and (false? (:need-drop (:args c))) (not (tools/can-harvest? (:primitives c) block)))
        args {:pos cell :collect (and collect (not hand?)) :need-drop (not hand?) :fetch (:fetch (:args c))
              :ignore-zones? (:ignore-zones? (:args c))}
        done! (fn [] (ctx/update-mem! c dissoc :digging))]
    (cond
      (escape/protected? block) (finish! c :protected {:cell cell :block block})
      (own-build c cell) (finish! c :footprint {:cell cell :block block :plan (:plan (own-build c cell))})
      (and (not (:started (:digging (ctx/mem c))))
           (>= (get-in (ctx/mem c) [:cell-tries cell] 0) (max-cell-digs)))
      (finish! c :refills {:cell cell :block block})
      :else
      (let [resumed? (:started (:digging (ctx/mem c)))
            _ (ctx/update-mem! c #(cond-> (assoc-in % [:digging :started] true)
                                    (not resumed?) (update-in [:cell-tries cell] (fnil inc 0))))
            e (await (ctx/call c :dig 'jobs.blocks.dig args))]
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) (do (done!)
                                  (when (:dug (:data e))
                                    (ctx/update-mem! c update :dug conj {:cell cell :block block})
                                    (when-let [tag (:note (:args c))] (escape/note-hole! c tag cell block)))
                                  :again)
          (declined? e) (do (done!) (finish! c :dig-waits {:cell cell :block block :wait (wait-of e)}))
          :else (do (done!)
                    (finish! c :dig-failed {:cell cell :block block
                                            :dig (cond-> {:reason (contract/cause-key e)}
                                                   (:primitive (:data e)) (assoc :primitive (:primitive (:data e))))})))))))

(defn ^:async step-through! [c through]
  (let [r (await (child/walk-into!* c :walk through))]
    (cond
      (= :continue r) :continue
      (and (:arrived (:data r)) (= through (reach/standing-vec (:primitives c)))) (finish! c :done {})
      :else (finish! c :step-failed {:cell through
                                     :walk (merge (select-keys (:data r) [:why :arrived])
                                                  {:status (if (= :done (:kind r)) :done :stopped) :reason (contract/cause-key r)})}))))

(defn ^:async replan!
  "The far side was a guess (:unseen) and the cells before it are dug: look at that row's unseen cells (memory
  :looked-at), then plan on from it with what the body sees (escape/door-from); a row still unseen is taken as
  guessed. :again, or the :no-door ending."
  [c]
  (let [{:keys [through from]} (ctx/mem c)
        dir (headings (:heading (:args c)))
        p (:primitives c)
        k (+ (js/Math.abs (- (first through) (first from))) (js/Math.abs (- (nth through 2) (nth from 2))))]
    (if (not= through (:looked-at (ctx/mem c)))
      (do (ctx/update-mem! c assoc :looked-at through)
          (loop [cells [(escape/up through 1) through (escape/up through -1)]]
            (when-let [cell (first cells)]
              (when (dig-look/unknown? p cell) (await (dig-look/look-at! c cell)))
              (recur (rest cells))))
          :again)
      (let [gap (escape/door-from (escape/block-at-of p) (escape/unseen-of p) from dir (:max-thick (:args c)) k)]
        (cond
          (nil? gap) (finish! c :no-door {:why (str "the wall goes on past " (pr-str through) ": thicker than "
                                                     (:max-thick (:args c)) " or no floor beyond")})
          (= through (:through gap)) (do (ctx/update-mem! c dissoc :unseen) :again)
          :else (do (ctx/update-mem! c assoc :cells (:cells gap) :through (:through gap) :unseen (:unseen gap))
                    :again))))))

(defn ^:async start-cell!
  "Look at the first cell not seen open, then say what to do with it: {:digging d} (kept in memory), :open (the look
  showed it open: plan again) or :unloaded (cannot be dug); nil when every cell is open."
  [c cells]
  (let [p (:primitives c)
        pos #(zipmap [:x :y :z] %)]
    (when-let [cell (first (remove #(b/air (u/seen-name p (pos %))) cells))]
      (when (dig-look/unknown? p cell) (await (dig-look/look-at! c cell)))
      (let [seen (u/seen-name p (pos cell))]
        (cond
          (b/air seen) :open
          (and (nil? seen) (some? (.-sensedAt p)) (nil? (u/sensed p (pos cell)))) {:unloaded cell}
          :else (let [block (or seen b/hidden-guess)
                      d {:cell cell :block block :collect (room-for-drop? p block)}]
                  (ctx/update-mem! c assoc :digging d)
                  {:digging d}))))))

(defn ^:async next!
  "One piece of the door: plan it, dig a cell, plan on past a far side it could not see, or step through. :again,
  :continue (a child waits) or the ending."
  [c]
  (let [{:keys [cells through unseen digging]} (ctx/mem c)]
    (if-not through
      (await (plan! c))
      (let [start (if digging {:digging digging} (await (start-cell! c cells)))]
        (cond
          (= :open start) :again
          (:unloaded start) (finish! c :no-door {:why (str "the cell " (pr-str (:unloaded start)) " is not loaded")
                                                 :cell (:unloaded start)})
          (:digging start) (await (dig-cell! c (:digging start)))
          unseen (await (replan! c))
          :else (await (step-through! c through)))))))

(defn ^:async round
  "The whole door: next! until it ends, a pace between pieces."
  [c]
  (await (pace/steps! c #(next! c))))
