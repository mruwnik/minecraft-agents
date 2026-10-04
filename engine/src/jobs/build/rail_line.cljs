(ns jobs.build.rail-line
  (:require [clojure.string :as str]
            [engine.ctx :as ctx]
            [engine.placement :as placement]
            [jobs.build.from-plan :as build]
            [plan.rail :as rail]
            [plan.shape :as shape]))

(def doc
  "Build the rail line a plan of the body's world wants (:plan, optionally only its :part) and prove a ridden cart can
  run it. The plan's rail cells (wants naming a *rail block) must form one chain (plan.rail/line); plan.rail/layout
  writes such plans. Phase :build runs jobs.build.from-plan on the plan as a child (its args :reach :give-up :accept
  pass through, and :sturdy-ground is set: a sturdy block on the ground of the line is no wrong block; zones, other plans' footprints, cuts and resuming are its rules; it places lowest first, so beds and
  redstone blocks go in before the rails on them). Slice 1: straight flat lines only, as from-plan places in no
  particular order and stands at the body's feet height. Phase :switch then switches on every planned lever that is
  off (jobs.access.toggle as a child, once per lever). Phase :check runs plan.rail/judge-line over the world. Any
  sturdy block where the plan wants bed or buffer fill is kept and is no fault. Sound: info rail-build.done. Not
  sound: ONE warn rail-build.broken with the :breaks {:pos :why} (:gap :shape :unlit :lit-brake :no-bed :blocked :wet
  :no-buffer :launch-trap :unloaded; at most 12) and what the build left (:refused :given-up :short). Either way it
  ends :done with {:ok? :breaks :hazards :built {:placed :missing :short :given-up :wrong :refused}}; :hazards lists
  falling beds (gravel, sand). The check declines with one rail-build.declined warn (:plan :reason) while the plan is
  missing, not :active or unreadable (:plan), its rail cells are not one chain (:not-a-line, :why :gap|:branch|
  :two-chains|:loop|:no-rails), no zone list has been read (:no-zones), or a cell still to build is refused by the
  access rules (:refused, the whole list [{:pos :reason :zone|:plan}]), or a cell wanting a redstone block holds
  another block (:source-blocked, the :cells; the builder never digs, so on natural ground :power :block cannot be
  built: use :torch or :lever, or a raised bed); with :all-carried, while the items for every cell still to build
  are not all carried (ONE warn rail-build.short {item n}; an :any want counts every choice carried; only cells
  seen empty are owed, and the warn's :up-to {item n} counts the cells nobody has seen as well). Without :all-carried it builds what is carried, and with nothing to build
  it goes straight to the proof. It declines without a warn while the line is sound and nothing is missing. Once
  begun the check stays true; a restart resumes in the phase it was in (the world is the memory).")

(def args
  {:plan {:doc "id of a plan of the body's world" :default nil}
   :part {:doc "only the cells of this part" :default nil}
   :reach {:doc "as jobs.build.from-plan" :default 4.2}
   :give-up {:doc "as jobs.build.from-plan" :default 3}
   :accept {:doc "as jobs.build.from-plan" :default [:fluid-adjacent]}
   :all-carried {:doc "start only while every item still to place is carried (false: build what is carried)" :default true}
   :fix {:doc "times a rail whose settled shape is wrong is dug and placed again; not used yet (slice 2)" :default 1}
   :dig {:doc "dig :clear cells and wrong blocks in bed and rail cells; not used yet (slice 3)" :default false}})

;; ------------------------------------------------------------------ the plan

(defn decline!
  "Warn rail-build.declined once per plan and trouble; trouble {:reason kw :text t ...}."
  [c trouble]
  (let [{:keys [plan part]} (:args c)]
    (ctx/warn-once! c [plan (dissoc trouble :text)] :rail-build.declined
                    (merge {:plan plan :part part}
                           (update trouble :text #(str "rail build declines plan " plan (when part (str " part " part)) ": " %))))))

(defn planned
  "{:cells judged} for the plan in the args, or {:trouble {:reason ...}} (warned once per reason)."
  [c]
  (let [{:keys [plan part]} (:args c)
        p (:primitives c)
        answer (ctx/plan c plan)
        cells (when (and answer (not (:broken answer)))
                (build/judged p answer part (set (keys (build/carried-counts p)))))
        chain (when (seq cells) (rail/line cells))
        trouble (cond
                  (build/plan-trouble answer cells) {:reason :plan :detail (build/plan-trouble answer cells)
                                                     :text (build/plan-trouble answer cells)}
                  (:error chain) (merge {:reason :not-a-line :why (:error chain)
                                         :text (str "its rail cells are not one line: " (name (:error chain))
                                                    (when (:at chain) (str " at " (pr-str (:at chain)))))}
                                        (select-keys chain [:at]))
                  (nil? (ctx/zones c)) {:reason :no-zones :text "no zone list has been read"})]
    (if-not trouble
      {:cells cells}
      (do (decline! c trouble)
          {:trouble trouble}))))

(defn block-at [c] (partial build/world-block (:primitives c)))

(defn sound?
  "True when nothing the plan wants is still to place and the line passes the proof."
  [c cells]
  (and (empty? (build/owed cells))
       (:ok? (rail/judge-line cells (block-at c)))))

(defn refused
  "The cells still to build that the access rules refuse now: [{:pos :reason ...}]."
  [c cells]
  (let [in (build/rules-input c)
        accept (:accept (:args c))]
    (into [] (keep (fn [{:keys [pos]}]
                     (let [d (build/decide in accept pos)]
                       (when (vector? d) (assoc (second d) :pos pos)))))
          (build/owed cells))))

(defn choices
  "The items that place cell: every choice of an :any want, else its one item."
  [{:keys [want item]}]
  (if (vector? want)
    (mapv #(placement/item-of (if (string? %) % (:block %))) (rest want))
    [item]))

(defn lacking
  "{item n} still lacking for cells, carried {item n}; an :any want is keyed by its text and counts every choice."
  [cells carried]
  (into (sorted-map)
        (keep (fn [[k group]]
                (let [n (- (count group) (reduce + (map #(get carried % 0) (choices (first group)))))]
                  (when (pos? n) [k n]))))
        (group-by #(if (vector? (:want %)) (shape/want-text (:want %)) (:item %)) cells)))

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

(defn ready?
  "Whether a line that is not sound may be begun: no redstone block cell taken by ground, nothing refused, and with
  :all-carried everything certainly needed carried."
  [c cells]
  (let [{:keys [plan part all-carried]} (:args c)
        blocked (blocked-sources cells)
        no (refused c cells)
        owed (build/owed cells)
        carried (build/carried-counts (:primitives c))
        short (short-of owed carried)]
    (cond
      (seq blocked) (do (decline! c {:reason :source-blocked :cells blocked
                                     :text (str "a redstone block cannot go at " (str/join ", " (map pr-str blocked))
                                                ": the cell holds ground and this job never digs; use :power :torch or "
                                                ":lever, or lay the line on a raised bed")})
                        false)
      (seq no) (do (decline! c {:reason :refused :refused no
                                :text (str "cells refused: " (str/join ", " (map #(str (pr-str (:pos %)) " " (name (:reason %))) no)))})
                   false)
      (and all-carried (seq short))
      (do (ctx/warn-once! c [plan :short] :rail-build.short
                          {:plan plan :part part :short short :up-to (up-to-of owed carried)
                           :text (str "rail build of " plan " waits: short of " (build/shortage-text short))})
          false)
      :else true)))

;; ------------------------------------------------------------------ check

(defn check [c]
  (let [{:keys [cells trouble]} (planned c)]
    (boolean
     (and (not trouble)
          (or (:phase (ctx/mem c))
              (and (not (sound? c cells))
                   (ready? c cells)))))))

;; ------------------------------------------------------------------ rounds

(defn build-args [c]
  (assoc (select-keys (:args c) [:plan :part :reach :give-up :accept]) :sturdy-ground true))

(defn nothing-to-place?
  "Whether the builder has nothing it could place with what is carried."
  [c cells]
  (let [carried (build/carried-counts (:primitives c))]
    (and (empty? (build/buildable cells carried {}))
         (not-any? #(pos? (get carried (:item %) 0)) (build/unseen cells {})))))

(defn ^:async build-step!
  "One round of the builder as the child; its result is kept when it ends and the phase moves on. A builder not yet
  begun with nothing to place is skipped: the proof says what is missing."
  [c cells]
  (if (and (not (:building (ctx/mem c))) (nothing-to-place? c cells))
    (let [left (build/owed cells)]
      (ctx/update-mem! c assoc :phase :switch
                       :built {:placed 0 :missing (mapv :pos left)
                               :short (short-of left (build/carried-counts (:primitives c)))
                               :given-up {} :wrong [] :refused []})
      :continue)
    (do (when-not (:building (ctx/mem c)) (ctx/update-mem! c assoc :building true))
        (let [r (await (ctx/call-child c :build 'jobs.build.from-plan (build-args c)))]
          (when (= :done r)
            (ctx/update-mem! c assoc :phase :switch :built (ctx/child-result c :build)))
          (if (= :done r) :continue r)))))

(defn unlit-levers
  "The planned levers standing in the world switched off and not yet tried."
  [c cells]
  (let [tried (set (:switched (ctx/mem c)))]
    (filterv (fn [{:keys [pos want]}]
               (let [b ((block-at c) pos)]
                 (and (= "lever" (shape/want-block want)) (= "lever" (:name b))
                      (not (rail/lit? b)) (not (tried pos)))))
             cells)))

(defn ^:async switch-step!
  "Switch on the next planned lever that is off (once each), then move on to the proof."
  [c cells]
  (if-let [{:keys [pos]} (first (unlit-levers c cells))]
    (let [r (await (ctx/call-child c :lever 'jobs.access.toggle {:pos pos :state :on}))]
      (when (= :done r) (ctx/update-mem! c update :switched (fnil conj []) pos))
      (if (= :done r) :continue r))
    (do (ctx/update-mem! c assoc :phase :check)
        :continue)))

(defn still-wrong
  "The builder's result with only the :wrong cells that are wrong now (cells judged this round; a powered rail lit by
  a lever switched on after the build is right now) and are not ground (rail/ground) holding a sturdy block."
  [built cells block-at]
  (let [ground (rail/ground cells)
        wrong-now (set (keep #(when (#{:wrong :extra} (:answer %)) (:pos %)) cells))
        kept? #(and (wrong-now %) (not (and (ground %) (rail/sturdy? (block-at %)))))]
    (update built :wrong (fn [wrong] (filterv #(kept? (:pos %)) wrong)))))

(defn break-text [{:keys [pos why]}] (str (name why) " " (pr-str pos)))

(defn left-text
  "What the build left, in words: refused and given-up cells, the material short."
  [{:keys [refused given-up short]}]
  (str/join "; " (concat (when (seq refused)
                           [(str "refused " (str/join ", " (map #(str (pr-str (:pos %)) " " (name (:reason %))) refused)))])
                         (when (seq given-up)
                           [(str "gave up " (str/join ", " (map (fn [[pos why]] (str (pr-str pos) " " (name why))) given-up)))])
                         (when (seq short)
                           [(str "short of " (build/shortage-text short))]))))

(def max-breaks 12)

(defn finish!
  "Prove the line, emit the outcome, hand the answer to the parent and end."
  [c cells]
  (let [plan (:plan (:args c))
        at (block-at c)
        built (still-wrong (:built (ctx/mem c)) cells at)
        proof (rail/judge-line cells at)
        rails (count (filter rail/rail-cell? cells))
        left (left-text built)]
    (if (:ok? proof)
      (ctx/emit! c :rail-build.done :info {:plan plan :cells rails :placed (:placed built)
                                           :text (str "rail line " plan " is sound, " rails " rails, placed " (:placed built))})
      (ctx/emit! c :rail-build.broken :warn
                 {:plan plan :breaks (vec (take max-breaks (:breaks proof))) :breaks-total (count (:breaks proof))
                  :refused (:refused built) :given-up (:given-up built) :short (:short built)
                  :text (str "rail line " plan " is broken: "
                             (str/join ", " (map break-text (take max-breaks (:breaks proof))))
                             (when (seq left) (str "; " left)))}))
    (ctx/result! c (assoc proof :built built))
    :done))

(defn ^:async round [c]
  (let [{:keys [cells trouble]} (planned c)
        phase (:phase (ctx/mem c))]
    (cond
      trouble :declined
      (= :check phase) (finish! c cells)
      (= :switch phase) (await (switch-step! c cells))
      :else (do (when-not phase (ctx/update-mem! c assoc :phase :build))
                (await (build-step! c cells))))))
