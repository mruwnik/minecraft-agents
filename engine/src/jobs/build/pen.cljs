(ns jobs.build.pen
  (:require [engine.args :as a] [jobs.lib.need :as need]
            [engine.contract :as contract]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.build.from-plan :as from-plan]
            [jobs.lib.pen :as pen]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.sense :as sense]
            [jobs.build.from-plan.plan :as plan]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.util :as u]
            [jobs.lib.storage :as storage]
            [jobs.lib.world :as known]))

(def doc
  "Build the fence a plan wants (:plan, optionally only its :part) and prove it holds animals.
  The pen is the bounding box of the plan's cells that want a fence, a fence gate or a wall.
  A plan with :surface true is put on the ground the body has seen (a pen on a slope follows it).
  Phase 1 runs jobs.build.from-plan on the plan. Its args :reach :give-up :accept pass through, and its rules
  (zones, other plans' footprints, materials, cuts) apply. Phase 2: a body still inside the box walks out through the
  gate (a jobs.movement.go-to child to the cell outside it) and jobs.animals.shut-gate shuts it. Phase 3 checks the pen
  over that box with jobs.lib.pen (see jobs.animals.pen-check): a step out of the box is a leak.
  Nothing is placed that the plan does not ask for. A leak left after the build is a hole in the plan, or a cell
  the build refused, gave up on or lacked material for.
  Needs: the plan, a zone list (unless :ignore-zones?), and the blocks of the fence (fetched by the builder unless
  :fetch false). Returns, as the data of its ending, {:closed? :reason :cells :leaks :gates :built {:placed :missing
  :short :given-up :wrong :refused}}. :reason is nil when closed, else :leak, :unbounded, :unloaded or :no-start.
  The pen ends with the body outside and the gate shut.
  Endings: done when the pen holds; nothing-possible with the :reason as cause when it does not
  (one warn pen-build.leaky naming the leaks {:pos :why} (:gap :open-gate :climb :perch-in :open :unloaded; at most 12) and what
  the build left (:refused, :given-up, :short)); nothing-possible :inside when the body cannot get out of the finished
  pen. An ending of the builder's can-start? (no data: :no-items, :no-zones) is this job's; a builder that ended
  short or refused after its work (data) goes on to the check, which reports the shortage as leaks with :built. A
  shut-gate child that did not end done is named in the data as :shut.
  Start facts, all waits or refusals of can-start? (one pen-build.declined warn naming the cells or the reason):
  - {:reason :plan-trouble :why text}: a wait while the plan is missing or unreadable; never when it has no cells (in :part) or no fence cells.
  - {:reason :uneven :why text}: its ring steps more than 1 between neighbouring fences, or a gate is more than 1 above
    the ground of an inside neighbour (animals will not step down 2), unless :uneven? true.
  - {:reason :no-zones}: no zone list has been read (unless :ignore-zones?).
  - {:reason :no-items}: the builder's own (jobs.build.from-plan): nothing to place is carried or fetchable; also the
    ending (condition-failed) when its look round finds that, with nothing placed.
  A pen already sound with the body outside it is done at once with nothing placed. Nothing is rebuilt after a restart: the world is the memory.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world" :spec a/name? :default nil}
   :part {:doc "only the cells of this part" :spec a/name? :default nil}
   :reach {:doc "as jobs.build.from-plan" :spec (a/num-in 0 nil) :default u/eye-reach}
   :give-up {:doc "as jobs.build.from-plan" :spec (a/int-in 1 nil) :default 3}
   :accept {:doc "as jobs.build.from-plan" :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default [:fluid-adjacent]}
   :max-cells {:doc "most cells the pen check visits before it gives up with :unbounded" :spec (a/int-in 1 nil) :default pen/default-max-cells}
   :fetch {:doc "as jobs.build.from-plan: get the blocks the plan lacks; false builds with what is carried" :spec need/option? :default {:explore true}}
   :uneven? {:doc "build although the ring steps more than 1 between neighbouring fences or the inside drops more than 1 at the gate" :spec boolean? :default false}
   :ignore-zones? :shared})

;; ------------------------------------------------------------------ the pen from the plan

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one pen build may take." :spec (a/int-in 1 nil)}})

(defn gate? [want] (boolean (some #(str/ends-with? % "_fence_gate") (pen-read/want-blocks want))))

(def max-step "The most an animal steps up or down between neighbouring cells of a pen." 1)

(defn uneven
  "Where the pen is too uneven for animals: [{:why :step :at :to :step}] for neighbouring barrier columns whose
  lowest cells differ by more than max-step, [{:why :gate-drop :gate :inside [x z] :drop}] for a gate more than
  max-step above the seen ground (top-of x z y, nil when unknown) of an inside neighbour column."
  [cells top-of]
  (let [low (reduce (fn [m {[x y z] :pos :keys [want]}]
                      (if (pen-read/barrier? want) (update m [x z] #(min y (or % y))) m))
                    {} cells)
        box (pen-read/barrier-box cells)
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
                    (when-not (pen-read/barrier-box cells) "the plan has no fence, wall or gate cells")
                    (when (and (nil? (known/zones c)) (not (:ignore-zones? (:args c)))) plan/no-zones))]
    (if-not trouble
      {:cells cells}
      (do (ctx/warn-once! c [plan trouble] :pen-build.declined
                          {:plan plan :part part :reason trouble
                           :text (str "pen build declines plan " plan (when part (str " part " part)) ": " trouble)})
          {:trouble trouble :later? (and (not= plan/no-zones trouble) (known/unreadable? answer))}))))

(defn pen-opts [c cells]
  {:box (pen-read/barrier-box cells) :max-cells (:max-cells (:args c))})

(defn read-pen
  "The pen check over what the body sees or remembers (pen-read/read; the round's finish! looks first): an unseen cell is an :unloaded leak."
  [c cells]
  (pen-read/read (:primitives c) (pen-opts c cells)))

(defn stale-in-sight
  "The cells of the pen the body knows from memory only that lie within sight range: finish! looks at them before it
  answers, done? cannot, so it does not call the pen finished on them."
  [c cells]
  (let [p (:primitives c)]
    (sense/targets p (u/self-pos c) (:remembered (read-pen c cells)) #{})))

(defn build-args [c]
  (select-keys (:args c) [:plan :part :reach :give-up :accept :fetch :ignore-zones?]))

;; ------------------------------------------------------------------ conditions

(def target [:plan :part])

(defn judged-plan
  "planned, once per check-cache window."
  [c]
  (check-cache/cached c :planned #(planned c)))

(defn plan-problem
  "Later for a plan that is missing or unreadable, or no zone list read (the plan may be saved or fixed later); never
  for a readable plan without cells or fence cells."
  [c]
  (let [{:keys [trouble later?]} (judged-plan c)]
    (cond
      (nil? trouble) (contract/ok :plan "the plan can be built")
      (= plan/no-zones trouble) (contract/later :no-zones "no zone list has been read")
      later? (contract/later :plan-trouble trouble :why trouble)
      :else (contract/never :plan-trouble trouble :why trouble))))

(def uneven-problem
  "Later :uneven for a ring that steps over max-step or a gate that drops too far onto the seen ground, unless :uneven?
  (ground the body sees changes, so at the start only)."
  (checks/refusing
   :even "the ground is even"
   (fn [c]
     (let [{:keys [cells trouble]} (judged-plan c)]
       (when-not (or trouble (:uneven? (:args c)))
         (when-let [bad (seq (uneven cells (plan/top-fn c)))]
           (let [why (uneven-text bad)]
             (ctx/warn-once! c [(:plan (:args c)) why] :pen-build.declined
                             {:plan (:plan (:args c)) :part (:part (:args c)) :reason why
                              :text (str "pen build declines plan " (:plan (:args c)) ": " why)})
             {:reason :uneven :why why})))))))

(def defaults {:spend {:job-time ::job-time-s}})
(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)

(def can-start? (cond/and plan-problem uneven-problem from-plan/no-items))
(def can-run? plan-problem)

(defn inside?
  "Whether the body's column is in the box (a little above or below it too)."
  [box self]
  (let [[x y z] (map #(int (js/Math.floor %)) [(:x self) (:y self) (:z self)])]
    (and (<= (:x (:min box)) x (:x (:max box)))
         (<= (:z (:min box)) z (:z (:max box)))
         (<= (- (:y (:min box)) 2) y (+ (:y (:max box)) 2)))))

(defn outside?
  "Whether the body stands outside the pen's box."
  [c cells]
  (not (inside? (pen-read/barrier-box cells) (u/self-pos c))))

(defn result-of
  "The data of the call's ending: the pen's summary, what was built and the shut-gate cause when it did not end done."
  [c answer]
  (cond-> (assoc answer :built (:built (ctx/mem c)))
    (:shut (ctx/mem c)) (assoc :shut (:shut (ctx/mem c)))))

(defn done?
  "Ok when nothing is owed, the pen holds and the body is outside it; else later, with the cells placed and still owed."
  [c]
  (let [{:keys [cells trouble]} (judged-plan c)
        plan (:plan (:args c))
        answer (when-not trouble (pen/summary (read-pen c cells)))
        holds? (boolean (and (not trouble) (empty? (plan/owed cells)) (:closed? answer) (empty? (stale-in-sight c cells)) (outside? c cells)
                             (or (:built (ctx/mem c)) (not (checks/running? c)))))]
    (checks/goal holds?
                 [:holds (str "pen of " plan " holds, " (:cells answer) " cells, placed " (get-in (ctx/mem c) [:built :placed] 0))]
                 [:owed (str "pen of " plan " is not finished")]
                 {:units :cells :done (get-in (ctx/mem c) [:built :placed] 0) :left (if trouble 0 (count (plan/owed cells)))
                  :result (when answer (result-of c answer))})))

(defn ^:async quote-of [c]
  (select-keys (await (ctx/quote c 'jobs.build.from-plan (build-args c))) [:uses :yields]))

;; ------------------------------------------------------------------ rounds

(defn leak-text [{:keys [pos why]}]
  (str (name why) " " (pr-str [(:x pos) (:y pos) (:z pos)])))

(defn ^:async finish!
  "Read the pen (looking at the remembered cells in sight first), emit the outcome and end: done when it holds, else nothing-possible with the reason."
  [c cells]
  (let [plan (:plan (:args c))
        built (:built (ctx/mem c))
        answer (pen/summary (await (pen-read/read! c (pen-opts c cells))))
        result (result-of c answer)
        left (plan/left-text built)]
    (if (:closed? answer)
      (ctx/end c :done nil (assoc result :text (str "pen of " plan " holds, " (:cells answer) " cells, placed " (:placed built))))
      (do (ctx/emit! c :pen-build.leaky :warn
                     (merge {:plan plan :reason (:reason answer) :leaks (:leaks answer)
                             :refused (:refused built) :given-up (:given-up built) :short (:short built)
                             :text (str "pen of " plan " is not closed (" (name (:reason answer)) "): "
                                        (str/join ", " (map leak-text (:leaks answer)))
                                        (when (seq left) (str "; " left)))}
                            (select-keys answer [:leaks-total])))
          (ctx/end c :nothing-possible (:reason answer) result)))))

(defn ^:async build-step!
  "One round of the builder as the child; the phase moves to the exit when its round ended with its data (done, or it
  left cells refused, given up or short: the check reports them as leaks); an ending of its conditions, or of a limit, is
  this job's, and so is a :no-items with nothing placed (the look round found the fences short)."
  [c]
  (let [r (await (ctx/call c :build 'jobs.build.from-plan (build-args c)))]
    (cond
      (or (= :continue r) (= :cut-off (:kind r))) :continue
      (and (= :condition-failed (:kind r)) (= :no-items (get-in r [:cause :reason])) (zero? (get-in r [:data :placed] 0)))
      (ctx/end c :condition-failed (:cause r) (:data r))
      (or (= :done (:kind r)) (and (:data r) (#{:nothing-possible :condition-failed} (:kind r)))) (do (ctx/update-mem! c assoc :phase :exit :built (:data r))
                                                 :again)
      :else (ctx/end c (:kind r) (:cause r)))))

(defn exit-cell
  "The cell {:x :y :z} just outside the box beside a gate of cells, or nil without a gate on the box's edge."
  [box cells]
  (first (for [{[x y z] :pos :keys [want]} cells
               :when (gate? want)
               [nx nz] [[(inc x) z] [(dec x) z] [x (inc z)] [x (dec z)]]
               :when (not (inside? box {:x nx :y y :z nz}))]
           {:x nx :y y :z nz})))

(defn ^:async exit-step!
  "While the body is inside the box: walk out beside the gate. Then on to shutting the gate, or the check if the body
  was never inside. Nothing-possible :inside when it cannot get out."
  [c cells]
  (let [box (pen-read/barrier-box cells)]
    (if-not (inside? box (u/self-pos c))
      (do (ctx/update-mem! c assoc :phase (if (:exited (ctx/mem c)) :shut :check))
          :again)
      (if-let [out (exit-cell box cells)]
        (let [r (await (child/call-walk* c :exit {:pos out :range 0 :ways #{:walk :clear-seal}}))]
          (cond
            (= :continue r) :continue
            (= :done (:kind r)) (do (ctx/update-mem! c assoc :exited true) :again)
            :else (ctx/end c :nothing-possible {:reason :inside :why "the walk out of the pen failed"} {:built (:built (ctx/mem c))})))
        (ctx/end c :nothing-possible {:reason :inside :why "the pen has no gate to leave by"} {:built (:built (ctx/mem c))})))))

(defn ^:async shut-step!
  "Shut the plan's gates the walk out left open, then on to the check."
  [c]
  (let [r (await (ctx/call c :shut 'jobs.animals.shut-gate {:plan (:plan (:args c))}))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c #(cond-> (assoc % :phase :check)
                                (not= :done (:kind r)) (assoc :shut (contract/cause-key r))))
          :again))))

(defn ^:async step [c]
  (let [{:keys [cells trouble]} (planned c)
        phase (or (:phase (ctx/mem c)) :build)]
    (cond
      trouble :continue
      (= :exit phase) (await (exit-step! c cells))
      (= :shut phase) (await (shut-step! c))
      (= :check phase) (do (await (look/survey! c)) (await (finish! c cells))) ; cells behind the body are not seen until it looks
      :else (await (build-step! c)))))

(defn ^:async round
  "The whole attempt: build, walk out, shut the gate, check the pen, in steps (pace/steps!); :continue only while a child waits."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
