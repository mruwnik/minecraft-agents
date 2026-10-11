(ns jobs.animals.escort
  "Walking a led or lured animal along: the legs of the walk, the looks at the animal after each, the wait for it, and shutting the gates left open. State lives in the caller's job memory."
  (:require [engine.ctx :as ctx]
            [engine.memory :as mem]
            [clojure.string :as str]
            [jobs.animals.herd-run :as hr]
            [jobs.lib.animals :as animals]
            [jobs.lib.child :as child]
            [jobs.lib.doors :as doors]
            [jobs.lib.pass :as pass]
            [jobs.lib.places :as places]
            [jobs.lib.reach :as reach]
            [jobs.lib.util :as u]
            [jobs.lib.walk :as walk]
            [jobs.lib.walk.search :as wsearch]
            [jobs.lib.walk.world :as wworld]))

(defn walk-entries
  "The :opened entries written by this job's go-to legs."
  [c]
  (filterv #(str/starts-with? (str (:by %)) (str (:id c) "/")) (map :data (mem/entries (ctx/view c) :opened))))

(defn shut-radius
  "Blocks from the body at self that cover every entry's cell (the floor is shut-doors' own default radius)."
  [self entries]
  (->> entries
       (map #(js/Math.ceil (doors/distance self (:cell %))))
       (reduce max 16)))

(defn ^:async close-up!
  "Shut the gates the :leave-open legs left open (the animal followed through them): their entries become shut-able and a
  jobs.maintenance.shut-doors child shuts them. :yield while that child waits, else (conclude! c reason) ends the job."
  [c conclude!]
  (let [mine (walk-entries c)]
    (doseq [e mine :when (false? (:shut? e))]
      (ctx/forget-where! c :opened #(= (:cell e) (:cell %)))
      (ctx/remember! c :opened (assoc e :shut? true) pass/opened-policy))
    (if (empty? mine)
      (conclude! c (:ending (ctx/mem c)))
      (if (= :done (await (ctx/call-child c :shut 'jobs.maintenance.shut-doors {:radius (shut-radius (doors/self-pos (:primitives c)) mine)})))
        (conclude! c (:ending (ctx/mem c)))
        :yield))))

(defn animal-now
  "The led animal as the sensing shows it, or nil when it is not seen."
  [c]
  (animals/find-by-key (:primitives c) (:mob (:args c)) (:watch-radius (:args c)) (:animal (ctx/mem c))))

(defn ^:async plan-or-nil
  "The walk plan to the cell of target (floored by places/parse-pos, as go-to does; a promise: the search yields to
  the event loop), nil when there is none or planning throws a RangeError (a far target the planner cannot index):
  the check never fails the job."
  ([c pw target] (plan-or-nil c pw target nil))
  ([c pw target opts]
  (when-let [{:keys [x y z]} (:pos (places/parse-pos target))]
    (try (await (walk/plan-walk! c pw [x y z] 1 walk/default-weight opts))
         (catch js/RangeError _ nil)))))

(def leg-steps
  "How many path steps one leg of a walk covers."
  4)

(def follow-reach
  "The animal trailing the body by more than this many blocks is about to break the lead (it breaks past 12)."
  10)

(defn snap-to-ground
  "target with its y moved to the nearest standable cell of its column (down first, then a little up), unchanged when
  it already is one or the column has none: a spot in the air or underground still plans legs."
  [c target]
  (let [{:keys [x y z]} (:pos (places/parse-pos target))
        p (:primitives c)
        y' (when y
             (some #(when (reach/standable-cell? p {:x x :y % :z z}) %)
                   (concat (range y (- y 12) -1) (range (inc y) (+ y 4)))))]
    (if y' {:x x :y y' :z z} target)))

(defn ^:async leg-target
  "Where the next leg of a walk to target (within range) goes: the planned path's cell leg-steps on, or nil when the
  rest is within one leg, there is no plan, or the leg would not move the body (the caller walks the whole way). The
  search is one round of go-to's budget (the path to where it has got to): a far target never leaves the body silent
  for seconds, which body.idle would report."
  [c target range]
  (let [pw (wworld/path-world (:primitives c))
        steps (when pw (:steps (await (plan-or-nil c pw (snap-to-ground c target) {:budget wsearch/round-budget}))))
        step (when (< leg-steps (count steps)) (nth steps leg-steps))
        me (u/self-pos c)]
    (when (and step (< 1 (u/flat-dist me {:x (:px step) :z (:pz step)})))
      (select-keys step [:x :y :z]))))

(def safe-gap
  "The body does not walk on while a seen animal trails it by more than this many blocks: the lead breaks past 10."
  7)

(def max-wait-ms
  "Time spent waiting for an animal to catch up or come into sight before the job gives it up as :lagging."
  10000)

(def wait-look-ms
  "One look's wait while the body holds still for the animal."
  250)

(def unseen-lost-ms
  "Time the animal must stay out of sight, from the first look that missed it, before it counts as :lost. A led animal
  behind a ledge or a tree is not in the sensing, though it is still on the lead; rounds can come within ms, so time
  rather than looks decides."
  3000)

(defn following?
  "True when the animal is on this body's lead, or (a lure-to run, memory :method :lure) the body still holds its food."
  [c a]
  (if (= :lure (:method (ctx/mem c)))
    (hr/holds-food? c)
    (animals/led-by-me? a)))

(defn escort-problem
  "Why the walk must stop after a leg: :lost (not seen for unseen-lost-ms), :lead-broke or :lagging (the animal
  farther than follow-reach), :wait (not seen yet, or seen more than gap behind: the body holds still), else nil."
  [c gap]
  (let [a (animal-now c)]
    (if (nil? a)
      (let [now (ctx/now c)
            since (or (:unseen-since (ctx/mem c)) now)]
        (ctx/update-mem! c assoc :unseen-since since)
        (if (<= unseen-lost-ms (- now since)) :lost :wait))
      (do (ctx/update-mem! c dissoc :unseen-since)
          (let [d (u/flat-dist (u/pos-of (.-pos a)) (u/self-pos c))]
            (cond
              (not (following? c a)) :lead-broke
              (> d follow-reach) :lagging
              (> d gap) :wait))))))

(defn escort-verdict
  "escort-problem with the wait timed: :wait becomes :lagging after max-wait-ms of waiting in a row, and a clear look resets the clock."
  [c gap]
  (let [p (escort-problem c gap)
        now (ctx/now c)
        since (or (:wait-since (ctx/mem c)) now)]
    (if (= :wait p)
      (do (ctx/update-mem! c assoc :wait-since since)
          (if (> (- now since) max-wait-ms) :lagging :wait))
      (do (ctx/update-mem! c dissoc :wait-since)
          p))))

(defn ^:async wait-for-animal!
  "Hold still on purpose for one look's wait; the caller then looks again. Always :again."
  [c]
  (ctx/hold-still! c :waiting-for-animal)
  (await (ctx/act c :wait #js {:ms wait-look-ms}))
  (ctx/hold-still! c nil)
  :again)

(defn ^:async walk-legs!
  "Walk to target (within range) in short go-to legs in child slot, looking at the animal after each (a pull passes
  follow-reach as gap: it strains the lead on purpose; a plain walk safe-gap). The body does not
  outrun the animal: when it is out of sight or trails too far the walk answers :catching-up and looks again at the next
  round before the next leg. Answer :arrived, :failed (a leg did not arrive), :limit (limit-ms from started passed),
  :waiting (a leg's child is waiting on the world: the job yields), :catching-up (the caller waits for the animal) or
  {:problem :lost|:lead-broke|:lagging}."
  [c slot target range started limit-ms gap]
  (let [pre (when (:catching-up (ctx/mem c)) (escort-verdict c gap))]
    (cond
      (= :wait pre) :catching-up
      pre {:problem pre}
      :else
      (do
        (ctx/update-mem! c dissoc :catching-up)
        (loop []
          (let [leg (await (leg-target c target range))
                r (await (child/call-walk c slot
                                         {:pos (or leg target) :range (if leg 1 range) :doors :leave-open :escalate false :zone-tolls true :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
            (cond
              (not= :done r) :waiting
              (not (:arrived (ctx/child-result c slot))) :failed
              :else (let [problem (escort-verdict c gap)]
                      (cond
                        (= :wait problem) (if leg
                                            (do (ctx/update-mem! c assoc :catching-up true) :catching-up)
                                            :arrived)
                        problem {:problem problem}
                        (not leg) :arrived
                        (and limit-ms (>= (- (ctx/now c) started) limit-ms)) :limit
                        :else (recur))))))))))
