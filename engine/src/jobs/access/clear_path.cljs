(ns jobs.access.clear-path
  (:require [engine.ctx :as ctx]
            [engine.jobs.blocks :as b]
            [engine.jobs.escape :as escape]
            [engine.jobs.util :as u]))

(def doc
  "Dig a door 1 wide and 2 high through the wall straight ahead along :heading, then step through it.
  - The wall is at most :max-thick blocks thick, starts right in front of the body, and has a floor under every
    row and room for the body beyond it (engine.jobs.escape/door).
  - Never digs a door, gate, trapdoor, bed, container, sign or an unbreakable block.
  - Each cell is a jobs.blocks.dig child (zones, claims, hazards and tools are its rules). The drops are left
    where they fall; the body walks over them.
  - Then it walks into the cell beyond (jobs.debug.walk-plan).
  Ends with {:status :done|:stopped :reason :dug [{:cell :block}] :at [x y z] :through [x y z]}, also as a
  clear-path.done info or clear-path.stopped warn event. Stops: :bad-args, :no-door (no such wall here),
  :protected (a block it never digs), :dig-waits (the dig child's wait, as :wait), :dig-failed (its :dig
  result), :step-failed (the walk through did not arrive).
  It does not put the blocks back: jobs.movement.go-to does that once through.")

(def args
  {:heading {:doc ":north :east :south or :west" :default nil}
   :max-thick {:doc "the thickest wall it digs through, in blocks" :default 3}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}})

(def headings {:north [0 -1] :south [0 1] :east [1 0] :west [-1 0]})

(defn check [_c] true)

(defn feet-of [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn finish!
  [c reason detail]
  (let [m (ctx/mem c)
        result (merge {:status (if (= :done reason) :done :stopped) :reason reason :dug (:dug m [])
                       :at (feet-of c)}
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
      (if-let [{:keys [cells through]} (escape/door (escape/block-at-of p) (feet-of c) dir max-thick)]
        (do (ctx/update-mem! c assoc :cells cells :through through :dug [])
            :continue)
        (finish! c :no-door {:why (str "no wall at most " max-thick " thick with a floor beyond, "
                                       (name (:heading (:args c))) " of " (pr-str (feet-of c)))})))))

(defn ^:async dig-cell! [c cell]
  (let [p (:primitives c)
        block (u/block-name p (zipmap [:x :y :z] cell))
        args {:pos cell :collect false :ignore-zones? (:ignore-zones? (:args c))}]
    (if (escape/protected? block)
      (finish! c :protected {:cell cell :block block})
      (if-let [wait (b/child-wait c :dig 'jobs.blocks.dig args)]
        (finish! c :dig-waits {:cell cell :block block :wait wait})
        (let [r (await (ctx/call-child c :dig 'jobs.blocks.dig args))
              res (ctx/child-result c :dig)]
          (cond
            (not= :done r) :continue
            (:dug res) (do (ctx/update-mem! c update :dug conj {:cell cell :block block}) :continue)
            (= :already-clear (:reason res)) :continue
            :else (finish! c :dig-failed {:cell cell :block block :dig (select-keys res [:reason :status])})))))))

(defn ^:async step-through! [c through]
  (if (= :done (await (ctx/call-child c :walk 'jobs.debug.walk-plan {:to through})))
    (let [r (ctx/child-result c :walk)]
      (if (and (= :arrived (:status r)) (= through (feet-of c)))
        (finish! c :done {})
        (finish! c :step-failed {:cell through :walk (select-keys r [:status :reason :why])})))
    :continue))

(defn ^:async round [c]
  (let [{:keys [cells through]} (ctx/mem c)
        p (:primitives c)]
    (if-not through
      (await (plan! c))
      (if-let [cell (first (remove #(b/air (u/block-name p (zipmap [:x :y :z] %))) cells))]
        (await (dig-cell! c cell))
        (await (step-through! c through))))))
