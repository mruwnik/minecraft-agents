(ns jobs.build.pen
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.jobs.apiary :as apiary]
            [engine.jobs.pen :as pen]
            [jobs.animals.pen-check :as pen-check]
            [jobs.build.from-plan :as build]))

(def doc
  "Build the fence a plan wants (:plan, optionally only its :part) and prove it holds animals.
  The pen is the bounding box of the plan's cells that want a fence, a fence gate or a wall.
  Phase 1 runs jobs.build.from-plan on the plan. Its args :reach :give-up :accept pass through, and its rules
  (zones, other plans' footprints, materials, cuts) apply. Phase 2 checks the pen over that box with
  engine.jobs.pen (see jobs.animals.pen-check): a step out of the box is a leak.
  Nothing is placed that the plan does not ask for. A leak left after the build is a hole in the plan, or a cell
  the build refused, gave up on or lacked material for.
  Always ends :done with {:closed? :reason :cells :leaks :gates :built {:placed :missing :short :given-up
  :wrong :refused}}. :reason is nil when closed, else :leak, :unbounded, :unloaded or :no-start.
  Closed gives info pen-build.done. Not closed gives one warn pen-build.leaky naming the leaks {:pos :why}
  (:gap :open-gate :climb :open :unloaded; at most 12) and what the build left (:refused, :given-up, :short).
  Declines:
  - with one pen-build.declined warn while the plan is missing, unreadable, has no cells (in :part), has no
    fence cells, or no zone list has been read (unless :ignore-zones?).
  - without a warn when nothing is missing and the pen already holds. Nothing is rebuilt after a restart: the
    world is the memory.
  - through the builder's own build.declined while materials are not carried.
  Once begun, the check stays true.")

(def args
  {:plan {:doc "id of a plan of the body's world" :default nil}
   :part {:doc "only the cells of this part" :default nil}
   :reach {:doc "as jobs.build.from-plan" :default 4.2}
   :give-up {:doc "as jobs.build.from-plan" :default 3}
   :accept {:doc "as jobs.build.from-plan" :default [:fluid-adjacent]}
   :max-cells {:doc "most cells the pen check visits before it gives up with :unbounded" :default pen/default-max-cells}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it (passed to jobs.build.from-plan)" :default false}})

;; ------------------------------------------------------------------ the pen from the plan

(defn want-blocks
  "The block names a want can place: one for a block or a state map, the choices of an :any."
  [want]
  (cond
    (string? want) [want]
    (vector? want) (mapcat #(want-blocks (if (string? %) % (:block %))) (rest want))
    (and (map? want) (:block want)) [(:block want)]))

(defn barrier?
  "True when a want places a fence, a fence gate or a wall."
  [want]
  (boolean (some #(or (str/ends-with? % "_fence") (str/ends-with? % "_fence_gate") (str/ends-with? % "_wall"))
                 (want-blocks want))))

(defn barrier-box
  "The inclusive box {:min :max} around the fence, gate and wall cells of cells ({:pos :want}), or nil without any."
  [cells]
  (let [ps (map :pos (filter #(barrier? (:want %)) cells))]
    (when (seq ps)
      (let [lo (apply map min ps)
            hi (apply map max ps)]
        {:min (zipmap [:x :y :z] lo) :max (zipmap [:x :y :z] hi)}))))

(defn planned
  "{:cells judged} for the plan in the args, or {:trouble text} (warned once per reason)."
  [c]
  (let [{:keys [plan part]} (:args c)
        p (:primitives c)
        answer (ctx/plan c plan)
        cells (when (and answer (not (:broken answer)))
                (build/judged p answer part (set (keys (build/carried-counts p)))))
        trouble (or (build/plan-trouble answer cells)
                    (when-not (barrier-box cells) "the plan has no fence, wall or gate cells")
                    (when (and (nil? (ctx/zones c)) (not (:ignore-zones? (:args c)))) "no zone list has been read"))]
    (if-not trouble
      {:cells cells}
      (do (ctx/warn-once! c [plan trouble] :pen-build.declined
                          {:plan plan :part part :reason trouble
                           :text (str "pen build declines plan " plan (when part (str " part " part)) ": " trouble)})
          {:trouble trouble}))))

(defn read-pen [c cells]
  (pen/check {:block-at (apiary/block-at-fn (:primitives c))
              :box (barrier-box cells)
              :max-cells (:max-cells (:args c))}))

(defn sound?
  "True when nothing the plan wants is missing and the pen holds."
  [c cells]
  (and (empty? (build/owed cells))
       (:closed? (read-pen c cells))))

(defn build-args [c]
  (select-keys (:args c) [:plan :part :reach :give-up :accept :ignore-zones?]))

;; ------------------------------------------------------------------ check

(defn check [c]
  (let [{:keys [cells trouble]} (planned c)]
    (boolean
     (and (not trouble)
          (or (:phase (ctx/mem c))
              (and (not (sound? c cells))
                   (ctx/check-child c :build 'jobs.build.from-plan (build-args c))))))))

;; ------------------------------------------------------------------ rounds

(defn leak-text [{:keys [pos why]}]
  (str (name why) " " (pr-str [(:x pos) (:y pos) (:z pos)])))

(defn left-text
  "What the build left, in words: refused and given-up cells, the material short."
  [{:keys [refused given-up short]}]
  (str/join "; " (concat (when (seq refused)
                           [(str "refused " (str/join ", " (map #(str (pr-str (:pos %)) " " (name (:reason %))) refused)))])
                         (when (seq given-up)
                           [(str "gave up " (str/join ", " (map (fn [[pos why]] (str (pr-str pos) " " (name why))) given-up)))])
                         (when (seq short)
                           [(str "short of " (build/shortage-text short))]))))

(defn finish!
  "Read the pen, emit the outcome, hand the answer to the parent and end."
  [c cells]
  (let [plan (:plan (:args c))
        built (:built (ctx/mem c))
        answer (pen-check/summary (read-pen c cells))
        result (assoc answer :built built)
        left (left-text built)]
    (if (:closed? answer)
      (ctx/emit! c :pen-build.done :info {:plan plan :cells (:cells answer) :placed (:placed built)
                                          :text (str "pen of " plan " holds, " (:cells answer) " cells, placed " (:placed built))})
      (ctx/emit! c :pen-build.leaky :warn
                 (merge {:plan plan :reason (:reason answer) :leaks (:leaks answer)
                         :refused (:refused built) :given-up (:given-up built) :short (:short built)
                         :text (str "pen of " plan " is not closed (" (name (:reason answer)) "): "
                                    (str/join ", " (map leak-text (:leaks answer)))
                                    (when (seq left) (str "; " left)))}
                        (select-keys answer [:leaks-total]))))
    (ctx/result! c result)
    :done))

(defn ^:async build-step!
  "One round of the builder as the child; its result is kept when it ends and the phase moves to the check."
  [c]
  (let [r (await (ctx/call-child c :build 'jobs.build.from-plan (build-args c)))]
    (when (= :done r)
      (ctx/update-mem! c assoc :phase :check :built (ctx/child-result c :build)))
    (if (= :done r) :continue r)))

(defn ^:async round [c]
  (let [{:keys [cells trouble]} (planned c)
        phase (:phase (ctx/mem c))]
    (cond
      trouble :declined
      (= :check phase) (finish! c cells)
      :else (do (when-not phase (ctx/update-mem! c assoc :phase :build))
                (await (build-step! c))))))
