(ns jobs.build.pen
  (:require [engine.args :as a] [jobs.lib.fetch :as fetch]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.apiary :as apiary]
            [jobs.lib.look :as look]
            [jobs.lib.pen :as pen]
            [jobs.build.from-plan.plan :as plan]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]
            [jobs.lib.storage :as storage]
            [jobs.lib.world :as known]))

(def doc
  "Build the fence a plan wants (:plan, optionally only its :part) and prove it holds animals.
  The pen is the bounding box of the plan's cells that want a fence, a fence gate or a wall.
  With :surface (default) each column of the plan is put on the ground the body has seen there, so a pen on a slope follows it.
  Phase 1 runs jobs.build.from-plan on the plan. Its args :reach :give-up :accept pass through, and its rules
  (zones, other plans' footprints, materials, cuts) apply. Phase 2 checks the pen over that box with
  jobs.lib.pen (see jobs.animals.pen-check): a step out of the box is a leak.
  Nothing is placed that the plan does not ask for. A leak left after the build is a hole in the plan, or a cell
  the build refused, gave up on or lacked material for.
  Always ends :done with {:closed? :reason :cells :leaks :gates :built {:placed :missing :short :given-up
  :wrong :refused}}. :reason is nil when closed, else :leak, :unbounded, :unloaded or :no-start.
  Closed gives info pen-build.done. Not closed gives one warn pen-build.leaky naming the leaks {:pos :why}
  (:gap :open-gate :climb :open :unloaded; at most 12) and what the build left (:refused, :given-up, :short).
  Declines:
  - with a pen-build.declined warn naming the cells when the ring steps more than 1 between neighbouring fences, or a
    gate is more than 1 above the ground of an inside neighbour (animals will not step down 2), unless :uneven? true.
  - with one pen-build.declined warn while the plan is missing, unreadable, has no cells (in :part), has no
    fence cells, or no zone list has been read (unless :ignore-zones?).
  - without a warn when nothing is missing and the pen already holds. Nothing is rebuilt after a restart: the
    world is the memory.
  When the builder waits (e.g. :no-items while materials are not carried), the check also waits.
  Once begun, the check stays true.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world" :spec a/name? :default nil}
   :part {:doc "only the cells of this part" :spec a/name? :default nil}
   :reach {:doc "as jobs.build.from-plan" :spec (a/num-in 0 nil) :default u/eye-reach}
   :give-up {:doc "as jobs.build.from-plan" :spec (a/int-in 1 nil) :default 3}
   :accept {:doc "as jobs.build.from-plan" :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default [:fluid-adjacent]}
   :max-cells {:doc "most cells the pen check visits before it gives up with :unbounded" :spec (a/int-in 1 nil) :default pen/default-max-cells}
   :fetch {:doc "as jobs.build.from-plan: get the blocks the plan lacks; false builds with what is carried" :spec fetch/option? :default {:explore true}}
   :surface {:doc "as jobs.build.from-plan: each column of the pen on the seen ground (a pen on a slope)" :spec boolean? :default true}
   :uneven? {:doc "build although the ring steps more than 1 between neighbouring fences or the inside drops more than 1 at the gate" :spec boolean? :default false}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it (passed to jobs.build.from-plan)" :spec boolean? :default false}})

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

(defn gate? [want] (boolean (some #(str/ends-with? % "_fence_gate") (want-blocks want))))

(def max-step "The most an animal steps up or down between neighbouring cells of a pen." 1)

(defn uneven
  "Where the pen is too uneven for animals: [{:why :step :at :to :step}] for neighbouring barrier columns whose
  lowest cells differ by more than max-step, [{:why :gate-drop :gate :inside [x z] :drop}] for a gate more than
  max-step above the seen ground (top-of x z y, nil when unknown) of an inside neighbour column."
  [cells top-of]
  (let [low (reduce (fn [m {[x y z] :pos :keys [want]}]
                      (if (barrier? want) (update m [x z] #(min y (or % y))) m))
                    {} cells)
        box (barrier-box cells)
        inside? (fn [[x z]] (and (<= (:x (:min box)) x (:x (:max box))) (<= (:z (:min box)) z (:z (:max box)))))
        near (fn [[x z]] [[(inc x) z] [x (inc z)]])
        steps (for [[col y] (sort low) n (near col) :let [y2 (low n)] :when (and y2 (> (js/Math.abs (- y y2)) max-step))]
                {:why :step :at [(first col) y (second col)] :to [(first n) y2 (second n)] :step (js/Math.abs (- y y2))})
        drops (for [{[x y z] :pos :keys [want]} cells
                    :when (gate? want)
                    [nx nz :as n] [[(inc x) z] [(dec x) z] [x (inc z)] [x (dec z)]]
                    :when (and (not (low n)) (inside? n))
                    :let [top (top-of nx nz y)]
                    :when (and top (> (- y (inc top)) max-step))]
                {:why :gate-drop :gate [x y z] :inside n :drop (- y (inc top))})]
    (vec (concat steps drops))))

(defn uneven-text [problems]
  (str "the pen is uneven (steps over " max-step "): "
       (str/join ", " (map (fn [{:keys [why at to step gate inside drop]}]
                             (if (= :step why)
                               (str (pr-str at) " to " (pr-str to) " steps " step)
                               (str "gate " (pr-str gate) " drops " drop " to " (pr-str inside))))
                           (take 4 (sort-by #(- (or (:step %) (:drop %))) problems))))))

(defn planned
  "{:cells judged} for the plan in the args, or {:trouble text} (warned once per reason)."
  [c]
  (let [{:keys [plan part]} (:args c)
        p (:primitives c)
        answer (plan/surfaced c (known/plan c plan))
        cells (when (and answer (not (:broken answer)))
                (plan/judged p answer part (set (keys (storage/carried-totals (u/inventory p)))) nil))
        trouble (or (plan/plan-trouble answer cells)
                    (when-not (barrier-box cells) "the plan has no fence, wall or gate cells")
                    (when-not (:uneven? (:args c))
                      (when-let [bad (seq (uneven cells (plan/top-fn c)))] (uneven-text bad)))
                    (when (and (nil? (known/zones c)) (not (:ignore-zones? (:args c)))) "no zone list has been read"))]
    (if-not trouble
      {:cells cells}
      (do (ctx/warn-once! c [plan trouble] :pen-build.declined
                          {:plan plan :part part :reason trouble
                           :text (str "pen build declines plan " plan (when part (str " part " part)) ": " trouble)})
          {:trouble trouble}))))

(defn read-pen
  "The pen check over the cells the body has seen: an unseen cell is an :unloaded leak."
  [c cells]
  (pen/check {:block-at (apiary/seen-block-at-fn (:primitives c))
              :box (barrier-box cells)
              :max-cells (:max-cells (:args c))}))

(defn sound?
  "True when nothing the plan wants is missing and the pen holds."
  [c cells]
  (and (empty? (plan/owed cells))
       (:closed? (read-pen c cells))))

(defn build-args [c]
  (select-keys (:args c) [:plan :part :reach :give-up :accept :fetch :ignore-zones? :surface]))

;; ------------------------------------------------------------------ check

(defn check [c]
  (let [{:keys [cells trouble]} (planned c)]
    (cond
      trouble (ctx/wait c {:reason :plan-trouble :why trouble})
      (:phase (ctx/mem c)) true
      (sound? c cells) (ctx/wait c {:reason :already-sound})
      :else (boolean (ctx/check-child c :build 'jobs.build.from-plan (build-args c))))))

;; ------------------------------------------------------------------ rounds

(defn leak-text [{:keys [pos why]}]
  (str (name why) " " (pr-str [(:x pos) (:y pos) (:z pos)])))

(defn finish!
  "Read the pen, emit the outcome, hand the answer to the parent and end."
  [c cells]
  (let [plan (:plan (:args c))
        built (:built (ctx/mem c))
        answer (pen/summary (read-pen c cells))
        result (assoc answer :built built)
        left (plan/left-text built)]
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
    (if (= :done r) :again r)))

(defn ^:async step [c]
  (let [{:keys [cells trouble]} (planned c)
        phase (:phase (ctx/mem c))]
    (cond
      trouble :declined
      (= :check phase) (do (await (look/survey! c)) (finish! c cells)) ; cells behind the body are not seen until it looks
      :else (do (when-not phase (ctx/update-mem! c assoc :phase :build))
                (await (build-step! c))))))

(defn ^:async round
  "The whole attempt: build, then check the pen, in steps (pace/steps!); :continue only while the builder waits."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
