(ns jobs.access.clear-path
  (:require [engine.ctx :as ctx]
            [jobs.lib.blocks :as b]
            [jobs.lib.dig-look :as dig-look]
            [jobs.access.stair :as stair]
            [jobs.lib.escape :as escape]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]))

(def doc
  "Dig a door 1 wide and 2 high through the wall straight ahead along :heading, then step through it.
  - The wall is at most :max-thick blocks thick, starts right in front of the body, and has a floor under every
    row and room for the body beyond it (jobs.lib.escape/door). A cell it has not seen is rock; a far side it cannot
    see is guessed, and once the cells before it are dug it plans on from there with what it sees.
  - Never digs a door, gate, trapdoor, bed, container, sign or an unbreakable block.
  - Each cell is a jobs.blocks.dig child (zones, claims, hazards and tools are its rules). It picks up the drop
    (:collect) when there is room for it, so the block can be put back; with no room it digs on and leaves it.
  - Then it walks into the cell beyond (a go-to child).
  One call is the whole door; it yields :continue only while a dig child waits on the world.
  Ends with {:status :done|:stopped :reason :dug [{:cell :block}] :at [x y z] :through [x y z]}, also as a
  clear-path.done info or clear-path.stopped warn event. Stops: :bad-args, :no-door (no such wall here),
  :protected (a block it never digs), :dig-waits (the dig child's wait or decline, as :wait), :dig-failed (its
  :dig result), :refills (a cell dug max-cell-digs times, e.g. sand falling in), :step-failed (the walk through did
  not arrive).
  It does not put the blocks back: jobs.movement.go-to does that once through.")

(def args
  {:heading {:doc ":north :east :south or :west" :default nil}
   :max-thick {:doc "the thickest wall it digs through, in blocks" :default 3}
   :note {:doc "a map: each cell dug is written to the tidy ledger at once with it (jobs.lib.escape/note-hole!; go-to's escalation)" :default nil}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def headings {:north [0 -1] :south [0 1] :east [1 0] :west [-1 0]})

(def max-cell-digs "Digs of one cell (a falling block refills it) before it stops :refills." 8)

(defn check [_c] true)


(defn finish!
  [c reason detail]
  (let [m (ctx/mem c)
        result (merge {:status (if (= :done reason) :done :stopped) :reason reason :dug (:dug m [])
                       :at (stair/feet-of c)}
                      (when-let [t (:through m)] {:through t})
                      detail)]
    (ctx/result! c result)
    (if (= :done reason)
      (ctx/emit! c :clear-path.done :info (assoc result :text (str "cleared a way through to " (pr-str (:through m)))))
      (ctx/emit! c :clear-path.stopped :warn
                 (assoc result :text (str "clear-path stopped: " (name reason)
                                          (some->> (:cell detail) pr-str (str " at "))))))
    :done))

(defn ^:async plan! [c]
  (let [dir (headings (:heading (:args c)))
        max-thick (:max-thick (:args c))
        p (:primitives c)]
    (if-not (and dir (pos-int? max-thick))
      (finish! c :bad-args {:why "needs :heading (:north :east :south :west) and :max-thick > 0"})
      (if-let [{:keys [cells through unseen]} (escape/door (escape/block-at-of p) (escape/unseen-of p) (stair/feet-of c) dir max-thick)]
        (do (ctx/update-mem! c assoc :cells cells :through through :unseen unseen :from (stair/feet-of c) :dug [])
            :again)
        (finish! c :no-door {:why (str "no wall at most " max-thick " thick with a floor beyond, "
                                       (name (:heading (:args c))) " of " (pr-str (stair/feet-of c)))})))))

(defn room-for-drop?
  "A free slot, or a carried stack of what block drops (b/drops-of) under 64."
  [p block]
  (let [drops (set (b/drops-of p block))]
    (boolean (or (pos? (u/free-slots p))
                 (some #(and (drops (:name %)) (< (:count %) 64)) (u/inventory p))))))

(defn ^:async dig-cell!
  "One dig child call on the cell being dug (:digging {:cell :block :collect}, kept until the child ends, so a resumed
  call goes on with it). :again, :continue while the child waits on the world, or :done."
  [c {:keys [cell block collect]}]
  (let [args {:pos cell :collect collect :fetch false :ignore-zones? (:ignore-zones? (:args c))}
        done! (fn [] (ctx/update-mem! c dissoc :digging))]
    (cond
      (escape/protected? block) (finish! c :protected {:cell cell :block block})
      (and (not (:started (:digging (ctx/mem c))))
           (>= (get-in (ctx/mem c) [:tries cell] 0) max-cell-digs))
      (finish! c :refills {:cell cell :block block})
      :else
      (if-let [wait (when-not (:started (:digging (ctx/mem c))) (b/child-wait c :dig 'jobs.blocks.dig args))]
        (finish! c :dig-waits {:cell cell :block block :wait wait})
        (let [resumed? (:started (:digging (ctx/mem c)))
              _ (ctx/update-mem! c #(cond-> (assoc-in % [:digging :started] true)
                                      (not resumed?) (update-in [:tries cell] (fnil inc 0))))
              r (await (ctx/call-child c :dig 'jobs.blocks.dig args))
              res (ctx/child-result c :dig)]
          (cond
            (= :continue r) :continue
            (= :declined r) (do (done!)
                                (finish! c :dig-waits {:cell cell :block block
                                                       :wait (or (b/child-wait c :dig 'jobs.blocks.dig args) {:reason :declined})}))
            (:dug res) (do (done!)
                           (ctx/update-mem! c update :dug conj {:cell cell :block block})
                           (when-let [tag (:note (:args c))] (escape/note-hole! c tag cell block))
                           :again)
            (= :already-clear (:reason res)) (do (done!) :again)
            :else (finish! c :dig-failed {:cell cell :block block :dig (select-keys res [:reason :primitive])})))))))

(defn ^:async step-through! [c through]
  (let [r (await (stair/walk-into! c :walk through))]
    (cond
      (= :continue r) :continue
      (and (:arrived r) (= through (stair/feet-of c))) (finish! c :done {})
      :else (finish! c :step-failed {:cell through :walk (select-keys r [:status :reason :why :arrived])}))))

(defn ^:async replan!
  "The far side was a guess (:unseen) and the cells before it are dug: look at that row's unseen cells (memory
  :looked-at), then plan on from it with what the body sees (escape/door-from); a row still unseen is taken as
  guessed. :again, or :done (:no-door)."
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

(defn ^:async next!
  "One piece of the door: plan it, dig a cell, plan on past a far side it could not see, or step through. :again,
  :continue (a child waits) or :done."
  [c]
  (let [{:keys [cells through unseen]} (ctx/mem c)
        p (:primitives c)]
    (if-not through
      (await (plan! c))
      (if-let [digging (or (:digging (ctx/mem c))
                           (when-let [cell (first (remove #(b/air (u/seen-name p (zipmap [:x :y :z] %))) cells))]
                             (when (dig-look/unknown? p cell) (await (dig-look/look-at! c cell)))
                             (let [block (u/block-name-or p (zipmap [:x :y :z] cell) b/hidden-guess)
                                   d {:cell cell :block block :collect (room-for-drop? p block)}]
                               (ctx/update-mem! c assoc :digging d)
                               d)))]
        (await (dig-cell! c digging))
        (if unseen
          (await (replan! c))
          (await (step-through! c through)))))))

(defn ^:async round
  "The whole door: next! until it ends, a pace between pieces."
  [c]
  (await (pace/steps! c #(next! c))))
