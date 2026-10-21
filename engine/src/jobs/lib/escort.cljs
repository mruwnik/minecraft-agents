(ns jobs.lib.escort
  "Walking a led or lured animal along: the legs of the walk, the looks at the animal after each, the wait for it, and shutting the gates left open. Each leg is planned here (leg-plan) to choose where it ends and to keep the animal within the lead; the go-to child of the leg plans its walk again, as it must for doors and danger. State lives in the caller's job memory."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [clojure.string :as str]
            [jobs.lib.animals :as animals]
            [jobs.lib.bait :as bait]
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
  "Blocks from the body at self that cover every entry's cell (the floor is the :walked radius)."
  [self entries]
  (->> entries
       (map #(js/Math.ceil (doors/distance self (:cell %))))
       (reduce max 16)))

(defn ^:async close-up!
  "Shut the gates the :leave-open legs left open (the animal followed through them): their entries become shut-able and a
  jobs.maintenance.shut child shuts them. :yield while that child waits, else (conclude! c reason) ends the job
  (a shut child that ended short of done is told in a warn escort.shut-failed)."
  [c conclude!]
  (let [mine (walk-entries c)]
    (doseq [e mine :when (false? (:shut? e))]
      (ctx/forget-where! c :opened #(= (:cell e) (:cell %)))
      (ctx/remember! c :opened (assoc e :shut? true) pass/opened-policy))
    (if (empty? mine)
      (conclude! c (:ending (ctx/mem c)))
      (let [e (await (ctx/call c :shut 'jobs.maintenance.shut {:radius (shut-radius (doors/self-pos (:primitives c)) mine)}))]
        (if (= :continue e)
          :yield
          (do (when-not (= :done (:kind e))
                (ctx/emit! c :escort.shut-failed :warn {:kind (:kind e) :reason (contract/cause-key e)
                                                        :text (str "the gates the legs opened were not all shut: " (name (:kind e)))}))
              (conclude! c (:ending (ctx/mem c)))))))))

(a/defargs settings
  {::watch-radius {:default 64 :doc "Blocks from the body within which the lured or led animal is looked for." :spec (a/num-in 0 nil)}
   ::stall-s {:default 10 :doc "Seconds a seen animal beyond the safe gap may make no progress toward the body before the walk ends :animal-stalled." :spec (a/num-in 0 nil)}
   ::lure-gap {:default 5 :doc "The body does not walk on while a seen lured animal trails it by more than this many blocks: a cow follows food held within about 8-10 blocks." :spec (a/num-in 0 nil)}
   ::turn-back-s {:default 3 :doc "Seconds a lured animal beyond the gap may make no progress before the body turns back toward it (it has stopped following)." :spec (a/num-in 0 nil)}
   ::max-turns {:default 2 :doc "Times the body turns back to a lured animal that stopped following before the walk ends :animal-stalled." :spec (a/int-in 0 nil)}
   ::safe-gap {:default 7 :doc "The body does not walk on while a seen animal trails it by more than this many blocks: the lead breaks past engine.game/leash-break." :spec (a/num-in 0 nil)}
   ::max-wait-s {:default 10 :doc "Seconds spent waiting for an animal to come into sight at the destination before the job gives it up." :spec (a/num-in 0 nil)}
   ::unseen-lost-s {:default 3 :doc "Seconds the animal must stay out of sight, from the first look that missed it, before it counts as lost (a ledge or tree can hide a led animal)." :spec (a/num-in 0 nil)}
   ::back-limit-s {:default 15 :doc "Seconds from the first miss after which the walk back toward the last sighting gives up and the animal is lost." :spec (a/num-in 0 nil)}
   ::back-near {:default 4 :doc "Blocks from the last sighting at which the body counts as back there." :spec (a/num-in 0 nil)}
   ::closing-blocks {:default 0.2 :doc "Blocks nearer the body an animal must come to count as making progress (a cow swimming in covers a block in 20-30 s)." :spec (a/num-in 0 nil)}
   ::max-search-rounds {:default 40 :doc "Calls in a row a walk may spend on the plan search of its next leg (about 80 ms each) before it counts as failed." :spec (a/int-in 0 nil)}
   ::leg-steps {:default 4 :doc "How many path steps one leg of a walk covers." :spec (a/int-in 1 nil)}
   ::follow-reach {:default 10 :doc "A pull passes this as the gap: an animal trailing the body by more blocks is about to break the lead (past engine.game/leash-break)." :spec (a/num-in 0 nil)}
   ::wait-look-s {:default 0.25 :doc "Seconds of one look's wait while the body holds still for the animal." :spec (a/num-in 0 nil)}
   ::lead-deeper-gap {:default 3.5 :doc "An animal this near the body (flat) and outside the box has stopped tempted: lead on." :spec (a/num-in 0 nil)}
   ::lure-reach {:default 6 :doc "A lured animal is waited for once the body is this near it, in blocks." :spec (a/num-in 0 nil)}
   ::back-blocks {:default 20 :doc "An animal out of sight for unseen-lost-ms is looked for by walking back toward where it was last seen, when that is no farther from the body than this." :spec (a/num-in 0 nil)}})
(defn leg-steps [] (settings/get settings ::leg-steps))
(defn wait-look-s [] (settings/get settings ::wait-look-s))
(defn follow-reach [] (settings/get settings ::follow-reach))
(defn lead-deeper-gap [] (settings/get settings ::lead-deeper-gap))
(defn lure-reach [] (settings/get settings ::lure-reach))
(defn safe-gap [] (settings/get settings ::safe-gap))
(defn max-turns [] (settings/get settings ::max-turns))
(defn lure-gap [] (settings/get settings ::lure-gap))
(defn turn-back-ms [] (* 1000 (settings/get settings ::turn-back-s)))
(defn max-wait-ms [] (* 1000 (settings/get settings ::max-wait-s)))
(defn unseen-lost-ms [] (* 1000 (settings/get settings ::unseen-lost-s)))
(defn back-limit-ms [] (* 1000 (settings/get settings ::back-limit-s)))
(defn back-near [] (settings/get settings ::back-near))
(defn closing-blocks [] (settings/get settings ::closing-blocks))
(defn max-search-rounds [] (settings/get settings ::max-search-rounds))
(defn watch-radius [] (settings/get settings ::watch-radius))
(defn stall-ms [] (* 1000 (settings/get settings ::stall-s)))
(defn back-blocks [] (settings/get settings ::back-blocks))

(def end-of
  "The :end a bring result names for a lead-to or lure-to reason: :tie (tied to the post), :free (let go at the spot), :led (it stands at the spot, still following)."
  {:tied :tie :unleashed :free :arrived :led})

(defn animal-now
  "The led animal as the sensing shows it, or nil when it is not seen."
  [c]
  (animals/find-by-key (:primitives c) (:mob (:args c)) (watch-radius) (:animal (ctx/mem c))))

(defn ^:async plan-or-nil
  "The walk plan to the cell of target (floored by places/parse-pos, as go-to does; a promise: the search yields to
  the event loop), nil when there is none or planning throws a RangeError (a far target the planner cannot index):
  the check never fails the job. The plan may open gates and doors, as the go-to legs that walk it do."
  ([c pw target] (plan-or-nil c pw target nil))
  ([c pw target opts]
  (when-let [{:keys [x y z]} (:pos (places/parse-pos target))]
    (try (await (walk/plan-walk! c pw [x y z] 1 (walk/default-weight)
                                 (assoc opts :policy (update (wworld/body-policy c) :moves conj :open))))
         (catch js/RangeError _ nil)))))

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

(defn ^:async leg-plan
  "Where the next leg of a walk to target goes: {:leg cell} the planned path's cell leg-steps on,
  {} while the plan search is still going on (the caller looks at the animal and asks again), else {}
  (the rest is within one leg, there is no plan, or the leg would not move the body: the caller walks the whole way).
  The search is one round of go-to's budget (the path to where it has got to): a far target never leaves the body silent
  for seconds, which body.idle would report."
  [c target]
  (let [pw (wworld/path-world (:primitives c))
        plan (when pw (await (plan-or-nil c pw (snap-to-ground c target) {:budget wsearch/round-budget})))
        steps (:steps plan)
        step (when (< (leg-steps) (count steps)) (nth steps (leg-steps)))
        me (u/self-pos c)]
    (cond
      (and (empty? steps) (= "searching" (:status plan))) {:searching true}
      (and step (< 1 (u/flat-dist me {:x (:px step) :z (:pz step)}))) {:leg (select-keys step [:x :y :z])}
      :else {})))

(defn ^:async leg-target
  "The :leg of leg-plan, nil when none."
  [c target]
  (:leg (await (leg-plan c target))))

(defn following?
  "True when the animal is on this body's lead, or (a lure-to run, memory :method :lure) the body still holds its food."
  [c a]
  (if (= :lure (:method (ctx/mem c)))
    (bait/holds-food? c)
    (animals/led-by-me? a)))

(defn waiting-detail [c a me]
  (let [pos (some-> a .-pos u/pos-of)]
    {:animal-pos (or pos (:last-seen (ctx/mem c))) :body-pos me :seen (some? a)
     :distance (some->> (or pos (:last-seen (ctx/mem c))) (u/flat-dist me))}))

(defn note-wait!
  "The first look of a wait in a row emits escort.waiting with both positions and the distance."
  [c a me]
  (when-not (:waiting (ctx/mem c))
    (ctx/update-mem! c assoc :waiting true)
    (let [d (waiting-detail c a me)]
      (ctx/emit! c :escort.waiting :info
                 (assoc d :text (str "waiting for the " (:mob (:args c)) (when-not (:seen d) " (out of sight)")
                                     ": animal at " (pr-str (some-> (:animal-pos d) (select-keys [:x :z])))
                                     ", body at " (pr-str (select-keys me [:x :z]))
                                     (some->> (:distance d) js/Math.round (str ", ") (#(str % " blocks")))))))))

(defn follow-verdict
  "Why a walk with the animal a (nil: not seen) must stop: :lost (unseen for unseen-lost-ms and no walk back to where
  it was last seen is left), :lead-broke (following? false), :animal-stalled (seen beyond gap and not closing in for
  stall-ms), :wait (the body holds still: unseen, or beyond gap but coming; a lured animal that stopped coming for
  turn-back-ms sets :back-to to it instead, so the body walks back to it), else nil. The state is in the caller's
  memory: :unseen-since, :last-seen, :back-to (where wait-for-animal! walks back to), :progress {:best :at}."
  [c a following? gap]
  (let [now (ctx/now c)
        me (u/self-pos c)
        m (ctx/mem c)]
    (if (nil? a)
      (let [since (or (:unseen-since m) now)
            elapsed (- now since)
            seen (:last-seen m)
            away (some->> seen (u/flat-dist me))
            back? (and away (< (back-near) away (inc (back-blocks))) (< elapsed (back-limit-ms)))]
        (ctx/update-mem! c #(-> % (assoc :unseen-since since) (dissoc :progress)
                                (assoc :back-to (when (and back? (<= (unseen-lost-ms) elapsed)) seen))))
        (cond
          (< elapsed (unseen-lost-ms)) (do (note-wait! c nil me) :wait)
          back? (do (note-wait! c nil me) :wait)
          :else :lost))
      (let [pos (u/pos-of (.-pos a))
            d (u/flat-dist pos me)]
        (ctx/update-mem! c #(-> % (dissoc :unseen-since) (assoc :last-seen pos)))
        (cond
          (not following?) :lead-broke
          (<= d gap) (do (ctx/update-mem! c dissoc :progress :waiting :back-to :no-turn) nil)
          :else (let [{:keys [best]} (:progress m)
                      prog (if (or (nil? best) (< d (- best (closing-blocks)))) {:best d :at now} (:progress m))
                      turn? (and (= :lure (:method m)) (not (:no-turn m)) (or (:back-to m) (> (- now (:at prog)) (turn-back-ms))))]
                  (ctx/update-mem! c #(cond-> (assoc % :progress prog :back-to (when turn? pos))
                                        (and turn? (not (:back-to m))) (update :turns (fnil inc 0))))
                  (note-wait! c a me)
                  (if (or (> (- now (:at prog)) (stall-ms)) (< (max-turns) (:turns (ctx/mem c) 0))) :animal-stalled :wait)))))))

(defn escort-problem
  "follow-verdict for the led animal of the caller's memory (:animal)."
  [c gap]
  (let [a (animal-now c)]
    (follow-verdict c a (or (nil? a) (following? c a)) gap)))

(defn ^:async wait-for-animal!
  "Hold still on purpose for one look's wait; the caller then looks again. When follow-verdict set :back-to the body
  walks back toward the last sighting instead (one round of a go-to child, with doors per the caller: :leave-open by
  default, :never for a caller that does not shut gates after; :yield while it is not done). A walk back that did not
  arrive is not tried again until the animal is near (:no-turn) and the wait is held. Else :again."
  ([c] (wait-for-animal! c :leave-open))
  ([c doors]
  (let [to (:back-to (ctx/mem c))
        r (when to
            (await (child/call-walk* c :back {:pos to :range 2 :doors doors :ways #{:walk :clear-seal}
                                              :ignore-zones? (boolean (:ignore-zones? (:args c)))})))]
    (cond
      (= :continue r) :yield
      (nil? to) (do (await (child/pause! c (wait-look-s) :waiting-for-animal)) (ctx/hold-still! c nil) :again)
      (and (= :done (:kind r)) (:arrived (:data r))) :again
      :else (do (ctx/update-mem! c #(-> % (dissoc :back-to) (assoc :no-turn true)))
                (await (child/pause! c (wait-look-s) :waiting-for-animal))
                (ctx/hold-still! c nil)
                :again)))))

(defn deep-cell
  "The middle of the box cell farthest from the animal, one cell in from its far edge when the box is wider than two."
  [{:keys [min max]} animal-pos]
  (let [axis (fn [lo hi a] (let [far (if (< a (/ (+ lo hi) 2)) hi lo)
                                 in (if (< 2 (- hi lo)) (+ far (if (= far hi) -1 1)) far)]
                             (+ in 0.5)))]
    {:x (axis (:x min) (:x max) (:x animal-pos)) :y (:y min) :z (axis (:z min) (:z max) (:z animal-pos))}))

(defn ^:async lead-deeper!
  "A tempted animal stops a couple of blocks behind the food, so a body that stopped at the box edge leaves it outside:
  walk to the far side of the box once (slot :deeper). :yield while the walk goes on, else :again."
  [c box animal-pos]
  (if (= :continue (await (child/call-walk* c :deeper {:pos (deep-cell box animal-pos) :range 1 :doors :leave-open :ways #{:walk :clear-seal} :ignore-zones? (boolean (:ignore-zones? (:args c)))})))
    :yield
    (do (ctx/update-mem! c assoc :led-deeper true) :again)))

(defn ^:async walk-legs!
  "Walk to target (within range) in short go-to legs in child slot, looking at the animal after each (a pull passes
  follow-reach as gap: it strains the lead on purpose; a plain walk safe-gap). The body does not
  outrun the animal: when it is out of sight or trails too far the walk answers :catching-up and looks again at the next
  round before the next leg. Answer :arrived, :failed (a leg did not arrive),
  :waiting (a leg's child is waiting on the world: the job yields), :catching-up (the caller waits for the animal) or
  {:problem :lost|:lead-broke|:animal-stalled}. :searching: the plan search for the next leg is still
  going on (nothing was walked; the caller calls again), at most max-search-rounds calls in a row, then :failed."
  [c slot target range gap]
  (let [pre (when (:catching-up (ctx/mem c)) (escort-problem c gap))]
    (cond
      (= :wait pre) :catching-up
      pre {:problem pre}
      :else
      (do
        (ctx/update-mem! c dissoc :catching-up)
        (loop []
          (let [{:keys [leg searching]} (await (leg-plan c target))
                r (when-not searching
                    (await (child/call-walk* c slot
                                             {:pos (or leg target) :range (if leg 1 range) :doors :leave-open :ways #{:walk :clear-seal} :ignore-zones? (boolean (:ignore-zones? (:args c)))})))]
            (when-not searching (ctx/update-mem! c dissoc :search-rounds))
            (cond
              searching (let [n (inc (:search-rounds (ctx/mem c) 0))]
                          (ctx/update-mem! c assoc :search-rounds n)
                          (if (< (max-search-rounds) n) :failed :searching))
              (= :continue r) :waiting
              (not (and (= :done (:kind r)) (:arrived (:data r)))) :failed
              :else (let [problem (escort-problem c gap)]
                      (cond
                        (= :wait problem) (if leg
                                            (do (ctx/update-mem! c assoc :catching-up true) :catching-up)
                                            :arrived)
                        problem {:problem problem}
                        (not leg) :arrived
                        :else (recur))))))))))
