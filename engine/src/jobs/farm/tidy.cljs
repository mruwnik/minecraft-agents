(ns jobs.farm.tidy
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.plan-field :as pf]
            [jobs.lib.ready :as ready]
            [jobs.lib.strays :as strays]
            [jobs.lib.waits :as waits]
            [jobs.lib.tidy-rules :as tr]
            [jobs.lib.work-place :as work-place]))

(def doc
  "Dig the stray blocks over a plan's cells (:plan, optionally only its :part) and pick up the drops.
  The plan does not say what a stray is, this job does:
  - a cell the plan wants :clear holds a stray when anything is in it.
  - a crop cell (want {:crop c}) holds one when it holds anything but that crop (weeds, saplings, leaves,
    stone, dirt ...).
  - the one cell of air above each crop cell counts like a crop cell, unless the plan names it.
  Never dug, only listed:
  - the planned crop itself (ripe or not) and any block the plan wants.
  - water and lava, so the plan's own water is never drained. A cell that wants water is never dug.
  - farmland where a crop cell or a :clear cell wants something else (listed as :wrong).
  - a container or workstation, a bed, sign, banner, head or light where the plan wants something else.
    Result :kept {:pos :block :why :container|:owned|:light|:fluid}.
  - a block in a cell that wants another block, and a crop of another kind in a crop cell. Result :wrong
    {:pos :found :want}. Fixing these is a build, till or plant job's work.
  Cells that are not loaded hold no strays.
  The strays are listed by jobs.lib.strays and cleared by a jobs.blocks.clear-cells child (at most 400 cells a round, the
  plan's own footprint no obstacle): it walks, digs top-down, picks up the drops, and refuses a zone, claim, other plan's
  footprint, undiggable, unreachable or hazard cell (::accept (a setting) names the hazards taken). Refused cells are in
  the result's :refused [{:pos :block :reason}].
  Ends :done with data {:dug n :kept [...] :wrong [...] :refused [...]}; over a tidy
  field it digs nothing and is done. When strays were found, none dug and some refused, it ends condition-failed
  :refused with the same data.
  {:reason :plan-trouble :why text}: a wait while the plan is
  missing or unreadable or no zone list has been read; impossible when it has no cells (in :part), also later in the call.
  Quote: time, and the strays found now.")

(a/defargs args
  {:plan {:doc "id of a plan of the body's world" :spec a/name? :default nil}
   :part {:doc "only the cells of this part (and the air above its crop cells)" :spec a/name? :default nil}
   :ignore-zones? :shared})

(a/defargs settings
  {::min-health {:default 0 :spec (a/num-in 0 20)
                :doc "Health (of 20) below which the job waits to heal; 0 = never (its acts are short and in the body's own field). Poison still holds."}
   ::accept {:default #{:fluid-adjacent} :doc "Dig hazards accepted: :fluid-adjacent (water beside; lava beside is :lava-adjacent and is not accepted by default), :falling-block."
             :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block})}
   ::job-time-s {:default 600 :doc "Seconds a tidy run may take: the digs, the walks and the drops." :spec (a/num-in 0 nil)}})
(defn fit? [c] (ready/fit-min (settings/get settings ::min-health) c))
(defn accepted [] (settings/get settings ::accept))

;; ------------------------------------------------------------------ conditions

(defn planned
  "{:answer plan-answer} for the plan in the args, or {:trouble text :later? bool}."
  [c]
  (pf/read-plan c {:cells pf/part-cells :what "cells" :zones? true}))

(defn can-run? [c] (pf/plan-can-run? (planned c)))

(defn found-in
  "{:cells the plan's work cells :found (strays/strays over them)} for the plan answer."
  [c answer]
  (let [cells (strays/work-cells (:cells answer) (:part (:args c)))]
    {:cells cells :found (strays/strays cells #(tr/world-block (:primitives c) %))}))

;; ------------------------------------------------------------------ steps

(def max-per-call 400)

(defn book-clear
  "m with the end e of a clear-cells child over the strays todo booked: its digs counted, each cell it skipped (or all of
  them, when it declined to start) refused with the reason."
  [m e todo]
  (let [block-of (into {} (map (juxt :pos :block)) todo)
        refusals (strays/skips e (map (fn [{[x y z] :pos}] {:x x :y y :z z}) todo))]
    (-> m
        (update :dug (fnil + 0) (get-in e [:data :dug] 0))
        (update :refused (fnil into []) (map (fn [[{:keys [x y z]} reason]] {:pos [x y z] :block (block-of [x y z]) :reason reason})) refusals))))

(defn ^:async clear!
  "A round of a clear-cells child over the strays todo (at most max-per-call); booked when it ends."
  [c todo]
  (let [todo (vec (take max-per-call todo))
        {:keys [plan ignore-zones?]} (:args c)
        e (waits/waiting-out c (await (ctx/call c :clear 'jobs.blocks.clear-cells
                                                {:area {:cells (mapv :pos todo)} :for-plan plan :accept (accepted) :ignore-zones? ignore-zones?})))]
    (if (= :continue e)
      :continue
      (do (ctx/update-mem! c book-clear e todo) :again))))

(defn finish! [c found]
  (let [m (ctx/mem c)
        plan (:plan (:args c))
        refused (vec (:refused m))
        result {:dug (:dug m 0) :kept (:kept found) :wrong (:wrong found) :refused refused}
        done-text (str "tidy of " plan " done: "
                       (if (and (zero? (:dug m 0)) (empty? refused)) "nothing to dig" (str "dug " (:dug m 0)))
                       (when (seq refused) (str ", refused " (count refused))))]
    (if (and (zero? (:dug result)) (seq refused))
      (ctx/end c :condition-failed :refused result)
      (ctx/end c :done nil (assoc result :text done-text)))))

(defn cells-unseen?
  "Whether a cell of the work was never seen (the body has not looked at it), so no stray is known there."
  [c cells]
  (look/unseen? (:primitives c) (map (fn [{[x y z] :pos}] {:x x :y y :z z}) cells)))

(defn ^:async step [c]
  (let [{:keys [answer trouble]} (planned c)]
    (if trouble
      :continue ; the plan went bad after can-run?; the next check reports it
      (let [{:keys [cells found]} (found-in c answer)
            refused (set (map :pos (:refused (ctx/mem c))))
            todo (remove #(refused (:pos %)) (:dig found))]
        (cond
          (seq todo) (await (clear! c todo))
          (and (not (look/surveyed? c)) (cells-unseen? c cells)) (do (await (look/survey! c)) :again)
          :else (finish! c found))))))

(defn ^:async round
  "The whole attempt: loop the steps (clear the strays, look) until no stray is left; :continue only while the clear-cells child waits on the world."
  [c]
  (await (pace/steps! c (fn ^:async s [] (await (step c))))))

;; ------------------------------------------------------------------ the contract

(def target [:plan :part])

(def defaults {:spend {:job-time ::job-time-s}})
(def safe? (ready/safe-at-by-night work-place/spot))

(def can-start? can-run?)

(defn done?
  "Ok once no stray is left in the plan's cells, none is refused or wrong (the ending reports them), no child is under way
  and the cells were seen; else later with the strays dug and left."
  [c]
  (let [{:keys [answer trouble]} (planned c)
        m (ctx/mem c)
        dug (:dug m 0)
        {:keys [cells found]} (when-not trouble (found-in c answer))
        refused (set (map :pos (:refused m)))
        left (count (remove #(refused (:pos %)) (:dig found)))
        data [:units :strays :done dug :left left :result {:dug dug :kept (:kept found) :wrong (:wrong found) :refused (vec (:refused m))}]
        waiting (cond
                  trouble :plan-trouble
                  (pos? left) :tidying
                  (or (seq (:wrong found)) (seq (:refused m))) :reporting
                  (checks/child-under-way? c) :wrapping-up
                  (and (not (look/surveyed? c)) (cells-unseen? c cells)) :looking)]
    (if waiting
      (apply contract/later waiting (str "tidy: " dug " strays dug, " left " left") data)
      (apply contract/ok :tidied (str "tidy: no stray left, " dug " dug") data))))

(defn quote-of [c]
  (let [{:keys [answer trouble]} (planned c)
        n (if trouble 0 (count (:dig (:found (found-in c answer)))))]
    {:uses {:job-time [1 30 (settings/get settings ::job-time-s)]}
     :done [0 n n]}))
