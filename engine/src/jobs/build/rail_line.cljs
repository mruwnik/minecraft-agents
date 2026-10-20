(ns jobs.build.rail-line
  (:require [engine.settings :as settings]
            [engine.args :as a]
            [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.contract :as contract]
            [jobs.lib.check-cache :as check-cache]
            [jobs.lib.child :as child]
            [jobs.lib.checks :as checks]
            [jobs.lib.cond :as cond]
            [jobs.lib.near :as near]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.build.rail :as builder]
            [jobs.build.rail-spans :as spans]
            [jobs.lib.placement :as placement]
            [jobs.lib.build.plan :as plan]
            [plan.rail :as rail]
            [plan.shape :as shape]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.util :as u]
            [jobs.lib.storage :as storage]
            [jobs.lib.world :as known]))

(def doc
  "Build the rail line a plan wants (:plan, optionally only its :part) and prove a ridden cart can run it.
  The plan's rail cells (wants naming a *rail block) must form one chain (plan.rail/line). plan.rail/layout writes
  such plans, with corners and slopes.
  Four phases:
  0. :spans (first, before anything is built): each stretch of at least 3 straight level rail cells that the plan's :spans
     (a vector of {:kind :tunnel|:bridge :from :to}, as plan.rail-route/connect gives them) names and the world still shows
     solid or unseen (tunnel) or floorless (bridge) is cut by jobs.gather.strip-tunnel (torches hung as it does) or built by
     jobs.access.bridge, each from the rail cell before it, nearest first, up to jobs.build.rail-spans/::max-tries walks
     and children for each; a span that stays is left to the builder.
  1. :build, the head-first builder (jobs.build.rail). Cells go in along the line from the end nearer the
     body. Each station's bed and power go under before its rail, and the rail before the torch or lever beside
     it, so every rail takes the shape its neighbours give it. The body stands on the rail behind the cell it
     places. A rail that settled in the wrong shape is dug and placed again (jobs.build.rail's ::fix-times), then given up as
     :shape. Placing, zones, other plans' footprints, refusals, give-ups and the build.* events are
     jobs.build.from-plan's (its arg :accept applies).
     With nothing left to build the builder is skipped and the line goes straight to the proof.
  2. :switch turns on every planned lever that is off (jobs.access.toggle as a child, once per lever).
  3. :check runs plan.rail/judge-line over the world. A powered rail found unlit (or lit) only in the body's memory is
     first walked to once, to read its power as it is now; a line broken only by power states is re-read for a short while before it counts as broken. A sturdy block where the plan wants bed or buffer fill is
     no fault.
  Needs: the plan, a zone list (unless :ignore-zones?), and the items for every cell still to build carried (unless
  the call's :least is below :all); those lacking are fetched first (jobs.lib.need; a failed fetch ends the job with its cause). Returns, as the data of its ending, {:ok? :breaks :hazards :built {:placed :missing :short
  :given-up :wrong :refused}}. :hazards lists falling beds (gravel, sand).
  Endings: done when the line is sound (a line sound already is done at once, placed 0);
  nothing-possible :broken when it is not (the data's :breaks {:pos :why} (:gap :shape :unlit :lit-brake :no-bed
  :blocked :wet :no-buffer :launch-trap :unloaded) and :built say why; the text names the first 12 and what the build
  left (:refused :given-up :short)). A build that stopped short of finishing (its ending carries the result) goes on to the proof, which names what is missing.
  Start facts:
  - impossible: its rail cells are not one chain (:not-a-line, :why :gap|:branch|:two-chains|:loop|:no-rails); the plan has no cells to build (:no-cells).
  - condition-failed: the plan is missing or unreadable (:plan, a wait); no zone list has been read (:no-zones); a cell wanting
    a redstone block holds another block (:source-blocked, :cells; the builder never digs, so on natural ground put a
    torch or lever in the plan, or use a raised bed); a cell still to build is refused by the access rules
    (:refused, with the list [{:pos :reason :zone|:claim|:footprint|:hazard}]); with :least :all (the default) and the ways only :carried, items for
    the cells still to build are not all carried (:short {item n}, :up-to; an :any want counts every
    choice carried; only cells seen empty are owed, the warn's :up-to {item n} counts the unseen cells too). With
    :least 1 it builds what is carried.
  A restart resumes in the phase it was in: the world is the memory.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world" :spec a/name? :default nil}
   :part {:doc "only the cells of this part" :spec a/name? :default nil}
   :accept {:doc "as jobs.build.from-plan" :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default [:fluid-adjacent]}
   :ignore-zones? :shared})

(def ways need/fetch-ways)

;; ------------------------------------------------------------------ the plan

(defn planned
  "{:cells judged} for the plan in the args, or {:trouble {:reason ...}}."
  [c]
  (let [{:keys [plan part]} (:args c)
        p (:primitives c)
        answer (known/plan c plan)
        cells (when (and answer (not (:broken answer)))
                (plan/judged p answer part (set (keys (storage/carried-totals (u/inventory p)))) nil))
        chain (when (seq cells) (rail/line cells))
        trouble (cond
                  (plan/plan-trouble answer cells) {:reason (if (known/unreadable? answer) :plan :no-cells) :why (plan/plan-trouble answer cells)
                                                                    :text (plan/plan-trouble answer cells)}
                  (:error chain) (merge {:reason :not-a-line :why (:error chain)
                                         :text (str "its rail cells are not one line: " (name (:error chain))
                                                    (when (:at chain) (str " at " (pr-str (:at chain)))))}
                                        (select-keys chain [:at]))
                  (and (nil? (known/zones c)) (not (:ignore-zones? (:args c)))) {:reason :no-zones :text "no zone list has been read"})]
    (if-not trouble
      {:cells cells}
      {:trouble trouble})))

(defn block-at [c] (partial plan/world-block (:primitives c)))

(defn sound?
  "True when nothing the plan wants is still to place and the line passes the proof."
  [c cells]
  (and (empty? (plan/owed cells))
       (:ok? (rail/judge-line cells (block-at c)))))

(defn refused
  "The cells still to build that the access rules refuse now: [{:pos :reason ...}]."
  [c cells]
  (let [in (plan/rules-input c)
        accept (:accept (:args c))]
    (into [] (keep (fn [{:keys [pos]}]
                     (let [d (plan/decide in accept pos)]
                       (when (vector? d) (assoc (second d) :pos pos)))))
          (plan/owed cells))))

(defn choices
  "The items that place cell: every choice of an :any want, else its one item."
  [{:keys [want item]}]
  (if (vector? want)
    (mapv #(placement/item-of (if (string? %) % (:block %))) (rest want))
    [item]))

(defn shortages
  "[[key n choices]] still lacking for cells, carried {item n}; an :any want is keyed by its text and counts every choice."
  [cells carried]
  (into []
        (keep (fn [[k group]]
                (let [n (- (count group) (reduce + (map #(get carried % 0) (choices (first group)))))]
                  (when (pos? n) [k n (choices (first group))]))))
        (sort-by key (group-by #(if (vector? (:want %)) (shape/want-text (:want %)) (:item %)) cells))))

(defn lacking
  "{item n} still lacking for cells, carried {item n}."
  [cells carried]
  (into (sorted-map) (map (fn [[k n]] [k n])) (shortages cells carried)))

(defn short-of
  "{item n} certainly lacking: only cells seen to be empty are owed; a cell nobody has seen may hold ground already."
  [cells carried]
  (lacking (filter #(= :missing (:answer %)) cells) carried))

(defn up-to-of
  "{item n} lacking if every cell nobody has seen turns out to be empty as well (counted with the seen ones)."
  [cells carried]
  (lacking cells carried))

(defn blocked-sources
  "The cells wanting a redstone block that hold another block (the builder never digs), in order."
  [cells]
  (vec (sort (keep #(when (and (= "redstone_block" (shape/want-block (:want %))) (= :wrong (:answer %))) (:pos %))
                   cells))))

(defn fetch-opts
  "The limits {:how :explore :minutes} for fetching the items still to place, nil when the ways leave only :carried."
  [c]
  (need/fetch-opts c))

(defn whole?
  "Whether the call wants the whole line: its :least is :all (the default; before the call is frozen, the arg)."
  [c]
  (= :all (or (:least (ctx/frozen-call c)) (:least (:args c)) :all)))

(defn not-ready
  "Why a line that is not sound may not be begun, as a wait reason {:reason kw :why text}, or nil when it may: no
  redstone block cell taken by ground, nothing refused, and with :least :all everything certainly needed carried."
  [c cells]
  (let [all-carried (whole? c)
        blocked (blocked-sources cells)
        no (refused c cells)
        owed (plan/owed cells)
        carried (storage/carried-totals (u/inventory (:primitives c)))
        short (short-of owed carried)]
    (cond
      (seq blocked) (let [text (str "a redstone block cannot go at " (str/join ", " (map pr-str blocked))
                                    ": the cell holds ground and this job never digs; put a torch or lever in the plan, "
                                    "or lay the line on a raised bed")]
                      {:reason :source-blocked :cells blocked :why text})
      (seq no) (let [text (str "cells refused: " (str/join ", " (map #(str (pr-str (:pos %)) " " (name (:reason %))) no)))]
                 {:reason :refused :refused no :why text})
      (and all-carried (not (fetch-opts c)) (seq short))
      (let [text (str "short of " (plan/shortage-text short))]
        {:reason :short :short short :up-to (up-to-of owed carried) :why text}))))

;; ------------------------------------------------------------------ conditions

(def target [:plan :part])

(defn judged-plan
  "planned, once per check-cache window."
  [c]
  (check-cache/cached c :planned #(planned c)))

(defn plan-problem
  "The trouble of the plan: later for no zone list read, or a plan missing or unreadable (it may be saved or fixed
  later); never for rail cells that are not one chain."
  [c]
  (if-let [trouble (:trouble (judged-plan c))]
    (let [later? (#{:no-zones :plan} (:reason trouble))]
      (apply (if later? contract/later contract/never) (:reason trouble) (checks/cause-text trouble)
             (mapcat identity (dissoc trouble :reason :text))))
    (contract/ok :plan "the plan can be built")))

(def ready-problem
  "Later (not-ready) for a line not yet begun and not sound that may not be begun."
  (checks/refusing
   :ready "the line may be begun"
   (fn [c]
     (let [{:keys [cells trouble]} (judged-plan c)]
       (when-not (or trouble (sound? c cells))
         (check-cache/cached c :ready #(not-ready c cells)))))))

(def defaults {:least :all :spend {:job-time ::job-time-s}})

(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)

(def can-start? (cond/and plan-problem ready-problem))
(def can-run? plan-problem)

(defn done?
  "Ok when the line is sound; else later, with the cells placed and the cells still owed."
  [c]
  (let [{:keys [cells trouble]} (judged-plan c)
        proof (when-not trouble (rail/judge-line cells (block-at c)))
        plan (:plan (:args c))
        placed (get-in (ctx/mem c) [:built :placed] 0)]
    (checks/goal (boolean (and proof (empty? (plan/owed cells)) (:ok? proof) (or (:built (ctx/mem c)) (not (checks/running? c)))))
                 [:sound (str "rail line " plan " is sound, " (count (filter rail/rail-cell? cells)) " rails, placed " placed)]
                 [:owed (str "rail line " plan " is not sound")]
                 {:units :cells :done placed :left (if trouble 0 (count (plan/owed cells)))
                  :result (when proof (assoc proof :built (:built (ctx/mem c))))})))

(defn ^:async quote-of [c]
  (select-keys (await (ctx/quote c 'jobs.build.from-plan (select-keys (:args c) [:plan :part :accept :ignore-zones?]))) [:uses :yields]))

;; ------------------------------------------------------------------ rounds

(def builder-args [:plan :part :accept :ignore-zones?])

(defn ^:async build-step!
  "One round of the head-first builder (jobs.build.rail child); its result is kept when it ends and the phase moves on. A
  builder not yet begun with nothing to do is skipped: the proof says what is missing."
  [c cells]
  (if (and (not (:building (ctx/mem c))) (builder/idle? c cells))
    (let [left (plan/owed cells)]
      (ctx/update-mem! c assoc :phase :switch
                       :built {:placed 0 :missing (mapv :pos left)
                               :short (short-of left (storage/carried-totals (u/inventory (:primitives c))))
                               :given-up {} :wrong [] :refused []})
      :again)
    (do (when-not (:building (ctx/mem c)) (ctx/update-mem! c assoc :building true))
        (let [e (await (ctx/call c :build 'jobs.build.rail (select-keys (:args c) builder-args)))]
          (cond
            (or (= :continue e) (= :cut-off (:kind e))) :continue
            (or (= :done (:kind e)) (contains? (:data e) :missing)) (do (ctx/update-mem! c assoc :phase :switch :built (:data e)) :again)
            :else (ctx/end c (:kind e) (:cause e)))))))

(defn unlit-levers
  "The planned levers standing in the world switched off and not yet tried."
  [c cells]
  (let [tried (set (concat (:switched (ctx/mem c)) (:tried (ctx/mem c))))]
    (filterv (fn [{:keys [pos want]}]
               (let [b ((block-at c) pos)]
                 (and (= "lever" (shape/want-block want)) (= "lever" (:name b))
                      (not (rail/lit? b)) (not (tried pos)))))
             cells)))

(defn ^:async switch-step!
  "Switch on the next planned lever that is off (once each; counted switched only when the toggle ended done, a cut
  waits), then move on to the proof."
  [c cells]
  (if-let [{:keys [pos]} (first (unlit-levers c cells))]
    (let [r (await (ctx/call c :lever 'jobs.access.toggle {:pos pos :state :on}))]
      (cond
        (or (= :continue r) (= :cut-off (:kind r))) :continue
        (= :done (:kind r)) (do (ctx/update-mem! c update :switched (fnil conj []) pos) :again)
        :else (do (ctx/update-mem! c update :tried (fnil conj []) pos) :again)))
    (do (ctx/update-mem! c assoc :phase :check)
        :again)))

(defn still-wrong
  "The builder's result with only the :wrong cells that are wrong now (cells judged this round; a powered rail lit by
  a lever switched on after the build is right now) and are not ground (rail/ground) holding a sturdy block."
  [built cells block-at]
  (let [ground (rail/ground cells)
        wrong-now (set (keep #(when (#{:wrong :extra} (:answer %)) (:pos %)) cells))
        kept? #(and (wrong-now %) (not (and (ground %) (rail/sturdy? (block-at %)))))]
    (update built :wrong (fn [wrong] (filterv #(kept? (:pos %)) wrong)))))

(defn stale-power
  "The first powered-rail cell the proof finds on the wrong side of its power whose state the body only remembers (not
  in view, not touched) and has not gone to look at: a torch placed after the rail lights it out of sight."
  [c cells]
  (let [p (:primitives c)
        looked (set (:looked (ctx/mem c)))
        stale? (fn [pos] (when-let [b (u/sensed p (zipmap [:x :y :z] pos))]
                           (not (or (.-felt b) (.-visible b) (.-unknown b)))))]
    (->> (:breaks (rail/judge-line cells (block-at c)))
         (filter #(and (#{:unlit :lit-brake} (:why %)) (not (looked (:pos %))) (stale? (:pos %))))
         first :pos)))

(defn ^:async look-step!
  "Walk to within sight of a powered rail whose remembered power state is the only fault, once each, so the proof reads
  it as it is now. :continue while the walk waits, :again once looked, nil when nothing is due."
  [c cells]
  (when-let [pos (stale-power c cells)]
    (let [r (await (near/go-near! c (zipmap [:x :y :z] pos) 3 {:see true}))]
      (if (= :partial r)
        :continue
        (do (ctx/update-mem! c update :looked (fnil conj []) pos)
            :again)))))

(defn break-text [{:keys [pos why]}] (str (name why) " " (pr-str pos)))

(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds one rail line may take." :spec (a/int-in 1 nil)}
   ::settle-reads {:default 8 :doc "How many times, 250 ms apart, a line whose only fault is a powered rail's power state is re-read before it counts as broken (the power spreads and its updates arrive after the last block is placed)." :spec (a/int-in 0 nil)}
   ::max-breaks {:default 12 :doc "Breaks of a broken line named in the ending's text (the data lists all)." :spec (a/int-in 0 nil)}})
(defn max-breaks [] (settings/get settings ::max-breaks))
(defn settle-reads [] (settings/get settings ::settle-reads))

(defn ^:async settle-step!
  "While the proof's only faults are power states, re-read it up to settle-reads times after a short wait, so power still
  spreading from the last torch or rail is read once it arrived. :continue after a wait, nil when nothing is due."
  [c cells]
  (let [breaks (:breaks (rail/judge-line cells (block-at c)))
        n (or (:settled (ctx/mem c)) 0)]
    (when (and (seq breaks) (every? #(#{:unlit :lit-brake} (:why %)) breaks) (< n (settle-reads)))
      (ctx/update-mem! c assoc :settled (inc n))
      (await (child/pause! c 0.25 :power-spreading))
      :continue)))

(defn finish!
  "Prove the line, emit the outcome, hand the answer to the parent and end."
  [c cells]
  (let [plan (:plan (:args c))
        at (block-at c)
        built (still-wrong (:built (ctx/mem c)) cells at)
        proof (rail/judge-line cells at)
        rails (count (filter rail/rail-cell? cells))
        left (plan/left-text built)]
    (ctx/end c (if (:ok? proof) :done :nothing-possible) (when-not (:ok? proof) :broken)
             (assoc proof :built built
                    :text (if (:ok? proof)
                            (str "rail line " plan " is sound, " rails " rails, placed " (:placed built))
                            (str "rail line " plan " is broken: "
                                 (str/join ", " (map break-text (take (max-breaks) (:breaks proof))))
                                 (when (seq left) (str "; " left))))))))

(defn ^:async fetch-step!
  "Before the build is begun with :least :all, fetch the first item certainly lacking (jobs.lib.need): :continue while
  the child waits, :again once got, the failed ending, nil when nothing is due."
  [c cells]
  (when-let [[_ n names] (and (whole? c) (not (:building (ctx/mem c))) (fetch-opts c)
                              (first (shortages (filter #(= :missing (:answer %)) (plan/owed cells))
                                                (storage/carried-totals (u/inventory (:primitives c))))))]
    (let [o (fetch-opts c)
          e (await (if (= 1 (count names)) (need/item! c (first names) n o) (need/blocks! c names n o)))]
      (cond (= :continue e) :continue
            (= :done (:kind e)) :again
            :else e))))

(defn ^:async step [c]
  (let [{:keys [cells trouble]} (planned c)
        phase (:phase (ctx/mem c))]
    (cond
      trouble :continue
      (= :check phase) (or (await (look-step! c cells)) (await (settle-step! c cells)) (finish! c cells))
      (= :switch phase) (await (switch-step! c cells))
      :else (do (when-not phase (ctx/update-mem! c assoc :phase :build))
                (or (when-not (:building (ctx/mem c)) (await (spans/step! c cells)))
                    (await (fetch-step! c cells))
                    (await (build-step! c cells)))))))

(defn ^:async round
  "The whole attempt: build, switch the levers, prove the line, in steps (pace/steps!); :continue only while a walk, a dig
  or a lever child waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))
