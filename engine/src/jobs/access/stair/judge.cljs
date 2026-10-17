(ns jobs.access.stair.judge
  "What the next stair step needs and what refuses it, judged from the feet. Shared by jobs.access.stair, jobs.access.tunnel and jobs.access.leave-tunnel."
  (:require [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.blocks :as blocks]
            [engine.ctx :as ctx]
            [jobs.access.stair.cells :as geo]
            [jobs.access.stair.memory :as smem]
            [jobs.access.stair.step :as sstep]
            [jobs.lib.dig-look :refer [unknown?]]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.need :as need]
            [jobs.lib.reach :as reach]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [plan.shape :as shape]))

(defn access-world
  "The social half of the rules' input (jobs.lib.access/zone-input): zones, claims, footprints, the body's name and
  the clock, and the job's :ignore-zones? arg."
  [c]
  (access/zone-input c {:ignore-zones? (:ignore-zones? (:args c)) :own-plans-ok? (false? (:spare-own-builds (:args c))) :except (:through (:args c))}))

(defn rules-in
  "The rules' input at feet: :block-at reads unsensed cells as hidden-guess; :column-at reads them as air (a column
  scanned from the sky down for a stand, jobs.access.tunnel.line/surface)."
  [c feet]
  (merge {:floor-at (access/seen-at (:primitives c)) :block-at (access/sensed-at (:primitives c) blocks/hidden-guess) :column-at (access/sensed-at (:primitives c) "air")
          :feet feet :ledger #{} :ways (ledger/ways-of c) :keep-base? true
          :plugs (if (:dig-plugs (:args c)) #{} (smem/plugs-of c))}
         (access-world c)))

(defn fetch-limits
  "The limits map of :fetch when it allows a tool, else nil."
  [c]
  (let [o (need/opts c 'jobs.access.stair)]
    (when (and o (contains? (:what o) :tool)) o)))

(defn room-due?
  "Whether lack (from need) is a full pack that a make-room child may cure: :make-room is on and the pack has not
  changed since make-room last stopped short."
  [c lack]
  (and (= :no-free-slot (:reason lack))
       (:make-room (:args c))
       (not= (u/inventory (:primitives c)) (:room-failed (ctx/mem c)))))

(defn fetchable?
  "Whether lack is a missing pickaxe that :fetch lets the call get."
  [c lack]
  (and (= :no-tool (:reason lack)) (some? (fetch-limits c))))

(defn next-cut
  "The cells the next step cuts when it is judged from the feet (the body is at the stair's start or on it), else nil."
  [c]
  (let [{:keys [dir heading]} (:args c)
        feet (reach/standing-vec (:primitives c))
        {:keys [origin target]} (ctx/mem c)
        on-stair? (or (nil? origin) (some? (geo/stair-index origin feet dir heading target)))]
    (when (and on-stair? (geo/rises dir) (shape/headings heading))
      (:cut (geo/step-cells feet dir heading)))))

(defn need
  "What the next cell to dig lacks, as a reason map for ctx/wait, or nil: :no-tool (a pickaxe), :no-free-slot (no room
  for the drop). Judged for the step from the feet when the body is at the stair's start or on it; bad args and the
  rules' refusals are left to the round."
  [c]
  (let [p (:primitives c)
        block-at (:block-at (rules-in c (reach/standing-vec p)))
        block (some #(let [n (when-not (unknown? p %) (block-at %))] (when (and n (not (rules/air n)) (not (rules/unbreakable n))) n)) (next-cut c))]
    (cond
      (nil? block) nil
      (sstep/lacks-tool? c block) {:reason :no-tool :tool (tools/needed-kind p block) :block block}
      (not (sstep/drop-room? c block)) {:reason :no-free-slot :block block})))

(defn unmet
  "What the next dig lacks that this call cannot make good (no room made, no tool fetched), or nil."
  [c]
  (when-not (:backoff (ctx/mem c))
    (when-let [lack (need c)]
      (when-not (or (room-due? c lack) (fetchable? c lack)) lack))))

(defn refusal
  "Before anything is cut: the stop of the first step when a zone, claim or plan refuses its cut, else nil."
  [c]
  (let [{:keys [dir heading]} (:args c)
        {:keys [origin target]} (ctx/mem c)
        feet (reach/standing-vec (:primitives c))]
    (when (and (or (nil? origin) (and (= feet origin) (empty? (:dug (ctx/mem c)))))
               (geo/rises dir) (shape/headings heading))
      (let [in (rules-in c feet)
            stepc (-> (geo/step-cells feet dir heading) (assoc :up? (not= :down dir)))
            stop (geo/stop-of in stepc (set (:accept (:args c))))]
        (when (#{:zone :claim :footprint} (:reason stop)) stop)))))
