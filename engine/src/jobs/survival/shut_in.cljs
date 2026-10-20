(ns jobs.survival.shut-in
  (:require [jobs.lib.ready :as ready]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.need :as need]
            [jobs.lib.combat :as combat]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spent :as spent]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-pit :as dig-pit]
            [jobs.survival.plug-way :as way]
            [jobs.survival.shut-in-night :as night]
            [jobs.survival.wall-in :as wall-in]))

(def doc
  "Shut the body in where it stands, atomically: no hostile logic, little walking (only dig-pocket, to a wall, dig-pit for :night, to ground where a pit fits, and the :room way walk); whoever calls it decides when.
  Needs nothing to start. One call is a whole attempt: it ends :done {:pos :way :placed} once the body is enclosed (a roof within
  :roof-height over no open cell round the feet, or with the :room way a roof over a closed room), at once when it already is.
  Ways (:ways) are child jobs: jobs.survival.wall-in (the open sides, a roof support and the roof with carried blocks),
  dig-pit (dig down under a solid side and plug the hole), dig-pocket (dig a pocket beside the body, step in, wall the way in), and :room
  (jobs.survival.enter-room: walk into the :place, else the home or the closed room the body is in, shut its doors, fill a doorway or a hole in its roof; walks, which the others do not).
  It quotes the ways the call allows, starts the cheapest (ties in the order room, walls, pit, pocket) and, when that one ends other than
  :done, the next cheapest (after a :continue step when a hostile is within 2 blocks); a way that cannot be done here (quote ##Inf) is dropped. Blocks are the call's :use :blocks, else every
  :build filler (for :night the shelter fillers), carried or dug. None left is nothing-possible :no-way {:text :tried [ways that failed] :dropped [ways that cannot be done]
  :why {way the child's cause} :stops {way {:reason :needs}} (dig-pit's stop reason)}.
  :placed the blocks placed ({:cell [x y z] :item}), also with an ending other than :done. Every placed block goes to the scaffold ledger with :for (default :danger, ledger :retreat-plug, taken back by jobs.access.cleanup once no
  hostile is near; :night: ledger :shelter, left alone). A cell in another's zone is used only as a last resort (each way's quote adds plug-way's ::trespass-s).
  :for :night adds, on every end: the :shelter {:pos :state :built} entry that jobs.lib.dig-in-leave/leave! reads (:sealed false when the body
  is open; :roof, :door (the cells of one side placed, or of the pocket's way in), :start (the cell dug from, or the top of the shaft walled
  at the bottom of), :take-back (placed blocks worth more than :take-back-value, and any near a gate a plan wants, which leave! digs
  again by day), :room true; a shelter sealed again where the latest entry stood keeps its :start and :door); the event
  shut-in.sealed (when it placed blocks and the world shows the body shut in, with :spent; a warning when :resealed) or shut-in.unsealed (warning, naming the open
  cells); and for a way that failed on the site a :shut-in-failed {:pos :way :reason} entry (cap 5, ten
  minutes; none for a lack of blocks or of a tool), after which that way is dropped within ::futile-radius of it (jobs.survival.shut-in-night), except the pit, which walks to another site (jobs.survival.dig-pit-site).
  Quote: time.")

(a/defargs args
  {:ignore-zones? :shared
   :ways {:doc "the ways to enclose the body, tried by quote and, at a tie, in the order :room, :walls, :pit, :pocket (:room only when named)" :spec (a/set-of #{:room :walls :pit :pocket}) :default #{:walls :pit :pocket}}
   :for {:doc "why the body shuts itself in: :danger (blocks are taken back by the cleanup once no hostile is near; a hostile in a cell fails it), :night (blocks stay for the night; a mob in a cell is waited out)" :spec #{:danger :night} :default :danger}
   :place {:doc "the :room way: the room to enter, a remembered place name (default the home)" :spec (a/or-of keyword? string?) :default nil}
   :fetch {:doc "dig-pocket: get a missing tool (jobs.lib.need): true, a set of kinds or a map of limits" :spec need/option? :default false}
   :roof-height {:doc "a solid block within this many blocks above counts as a roof" :spec (a/int-in 0 nil) :default sh-sense/default-roof-height}
   :radius {:doc "melee hostiles within this many blocks decide which side is walled first and stand in the way of a cell; nil: the jobs.lib.danger ::hostile-radius setting" :spec (a/num-in 0 nil) :default nil}
   :ranged-radius {:doc "ranged hostiles within this many blocks decide which side is walled first; nil: the ::ranged-radius setting" :spec (a/num-in 0 nil) :default nil}
   :max-places {:doc "placements per step; nil: the jobs.survival.retreat-seal ::max-places setting" :spec (a/int-in 1 nil) :default nil}
   :hand-clear-ms {:doc "a pit digs a block the carried tools cannot harvest by hand, once a block to roof with is in hand, when the hand breaks it within this many ms (stone by hand takes 7500); default jobs.lib.tools quick-clear-ms" :spec (a/int-in 0 nil)}
   :on-lava {:doc ":seal: lava a dig lays open is filled with a carried block; :stop: the pit or pocket is dropped and the lava left" :spec #{:seal :stop} :default :seal}
   :wall-value {:doc ":night: a block worth more than this each (jobs.lib.cost/item-value; dirt 0.35, cobblestone 0.85, stone 2.85, planks 3.4, log 12.8) is dug again at dawn, which the walls' quote counts" :spec (a/num-in 0 nil) :default 3}
   :take-back-value {:doc ":night: a placed block worth more than this each (jobs.lib.cost/item-value; dirt 0.35, cobblestone 0.85) is dug again and collected when the shelter is left by day" :spec (a/num-in 0 nil) :default 0.5}})

(a/defargs settings
  {::job-time-s {:default 300 :doc "Seconds a shut-in may take." :spec (a/num-in 0 nil)}
   ::mean-s {:default 12 :doc "Seconds an enclosing is guessed to take, for the quote." :spec (a/num-in 0 nil)}
   ::night-hand-clear-ms {:default 10000 :doc "For :night, the :hand-clear-ms when the call gives none: a block the carried tools cannot harvest is dug by hand when the hand breaks it within this many ms (stone by hand takes 7500)." :spec (a/int-in 0 nil)}})

(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s} :ways #{:walls :pit :pocket}})

(def safe? ready/ok)

(def fit? ready/ok)

(def ways [:room :walls :pit :pocket])

(def children
  "Each way's job and the args of this call it takes."
  {:room ['jobs.survival.enter-room [:place :roof-height]]
   :walls ['jobs.survival.wall-in [:for :ignore-zones? :roof-height :radius :ranged-radius :max-places :wall-value]]
   :pit ['jobs.survival.dig-pit [:for :ignore-zones? :roof-height :radius :on-lava :hand-clear-ms]]
   :pocket ['jobs.survival.dig-pocket [:for :ignore-zones? :fetch :roof-height :radius :ranged-radius :max-places :on-lava]]})

(defn done? [c] (way/done? c (:way (ctx/mem c)) (merge (spent/summary (spent/log c)) (select-keys (ctx/mem c) [:placed :roof :start]))))

(defn quote-of [c]
  (let [mean (if (way/enclosed? c) 0 (settings/get settings ::mean-s))]
    {:uses {:job-time [0 mean (max (* 3 mean) (settings/get settings ::job-time-s))]} :done [0 1 1]}))

(defn with-defaults
  "c with the night's default :hand-clear-ms when the call gives none."
  [c]
  (cond-> c
    (and (night/night? c) (nil? (:hand-clear-ms (:args c)))) (assoc-in [:args :hand-clear-ms] (settings/get settings ::night-hand-clear-ms))))

(defn child-args [c way]
  (merge (select-keys (:args (with-defaults c)) (second (children way)))
         (when-let [blocks (night/blocks c)] {:use {:blocks blocks}})))

(defn ^:async cost-of
  "Seconds the child of way quotes, ##Inf when it cannot be done here or cannot be quoted."
  [c way]
  (let [q (await (ctx/quote c (first (children way)) (child-args c way)))]
    (if (contract/ending? q) ##Inf (get-in q [:uses :job-time 1] ##Inf))))

(defn ^:async choose
  "{:way the cheapest allowed way not failed (nil when none) :dropped [ways that cannot be done]}."
  [c]
  (let [failed (set (:failed-ways (ctx/mem c)))
        allowed (remove failed (filter #(ctx/way? c %) ways))
        barred (if (night/night? c) (filter #(and (not= :pit %) (night/failed-near c %)) allowed) [])
        left (remove (set barred) allowed)
        costs (loop [[w & more] left acc []]
                (if (nil? w) acc (recur more (conj acc [w (await (cost-of c w))]))))
        able (remove (comp infinite? second) costs)]
    (let [shown (into {} (map (fn [[w s]] [w (if (infinite? s) :inf (/ (js/Math.round (* 10 s)) 10))]) costs))]
      (ctx/emit! c :shut-in.quotes :debug {:costs shown :text (str "ways quoted (seconds): " (pr-str shown))}))
    {:way (first (first (sort-by second able)))
     :barred (vec barred)
     :dropped (into (vec barred) (map first (filter (comp infinite? second) costs)))}))

(defn end!
  "The call's ending with what the children placed; with :for :night the shelter entry and events first."
  [c kind cause data]
  (let [data (merge data (spent/summary (spent/log c)) (select-keys (ctx/mem c) [:placed :roof :start]))]
    (when (night/night? c) (ctx/forget-where! c :shut-in-failed #(= :interrupted (:reason %))))
    (when (and (night/night? c) (or (seq (:placed data)) (not (way/enclosed? c))))
      (night/end! c (or (:way data) (:last-way (ctx/mem c))) data (way/enclosed? c)))
    (ctx/end c kind cause data)))

(defn absorb!
  "A child's ending: what it placed goes in the log and in :placed."
  [c data]
  (spent/absorb! c data)
  (ctx/update-mem! c #(-> % (update :placed (fnil into []) (:placed data)) (merge (select-keys data [:roof :start])))))

(def adjacent-reach
  "Blocks within which a hostile is at the body: a way that failed ends the step so the round and the triggers see it before the next way starts."
  2)

(defn ^:async prepare!
  "Once, before anything is placed: the top of the 1x1 shaft the body stands at the bottom of (the shelter entry's :start), and for
  the night a look for the pit (dig-pit/look!)."
  [c]
  (when-not (contains? (ctx/mem c) :shaft-start)
    (let [{:keys [x z] :as feet} (sh-sense/feet (:primitives c))
          y (dig-cells/shaft-top (:primitives c) feet)]
      (ctx/update-mem! c assoc :shaft-start (when y {:x x :y y :z z}))
      (when (and (night/night? c) (ctx/way? c :pit)) (await (dig-pit/look! (with-defaults c)))))))

(defn ^:async run-way!
  "One step of way: its child job."
  [c way]
  (await (ctx/call c way (first (children way)) (child-args c way))))

(defn failed-way!
  "A way ended other than :done: dropped for this call, remembered for the night, its cause kept for the :no-way ending."
  [c way e]
  (night/fail! c way (cond (and (= :walls way) (= "failed" (get-in e [:data :status]))) :walls-server-refused :else (or (get-in e [:data :reason]) (:cause e))) (:start (ctx/mem c)))
  (ctx/update-mem! c #(-> % (dissoc :way) (update :failed-ways (fnil conj []) way) (assoc-in [:why way] (or (:text (:data e)) (:cause e))) (assoc-in [:statuses way] (:status (:data e))) (assoc-in [:stops way] (select-keys (:data e) [:reason :needs])))))

(defn why-not
  "The stop {:stop reason :text} that rules way out here, nil when unknown."
  [c way]
  (case way
    :walls (wall-in/why-not c)
    :pit (dig-pit/why-not (with-defaults c))
    nil))

(defn note-dropped!
  "Nothing is left to try: each way dropped for its quote says why (its stop), and for the night is remembered like a failed one."
  [c]
  (doseq [way (remove (set (:failed-ways (ctx/mem c))) (:dropped (ctx/mem c)))
          :let [before (when (night/night? c) (night/failed-near c way))
                {:keys [stop text]} (if before {:stop (:reason before) :text (str "the " (name way) " failed here before: " (name (:reason before)))} (why-not c way))]
          :when stop]
    (when-not before (night/fail! c way stop))
    (ctx/update-mem! c #(-> % (assoc-in [:why way] text) (assoc-in [:stops way] {:reason stop})))))

(defn no-way!
  "Nothing left to try: the :no-way ending."
  [c]
  (note-dropped! c)
  (let [m (ctx/mem c)
        text (str "cannot enclose the body: " (if-let [why (seq (vals (:why m)))] (str/join "; " why) "no way left that can be done here"))]
    (end! c :nothing-possible :no-way {:text text :tried (vec (:failed-ways m)) :dropped (:dropped m) :why (:why m) :statuses (:statuses m) :stops (:stops m)})))

(defn ^:async round
  "Start the cheapest way; one that ends other than :done is dropped for this call and the next is chosen on the next step when a hostile is at the body, else at once."
  [c]
  (loop []
    (if (and (way/enclosed? c) (not (:way (ctx/mem c))))
      (end! c :done nil {:pos (sh-sense/feet (:primitives c)) :way (:way (ctx/mem c))})
      (let [_ (await (prepare! c))
            chosen (when-not (:way (ctx/mem c)) (await (choose c)))
            _ (when chosen (ctx/update-mem! c #(-> % (assoc :way (:way chosen) :last-way (:way chosen)) (update :dropped (fn [d] (vec (distinct (into (or d []) (:dropped chosen))))))
                                                                    (update :barred (fn [b] (vec (distinct (into (or b []) (:barred chosen)))))))))
            way (:way (ctx/mem c))]
        (if (nil? way)
          (no-way! c)
          (let [e (await (run-way! c way))]
            (cond
              (= :continue e) :continue
              (and (= :done (:kind e)) (way/enclosed? c)) (do (absorb! c (:data e))
                                                              (end! c :done nil {:pos (sh-sense/feet (:primitives c)) :way way}))
              :else (do (absorb! c (:data e))
                        (failed-way! c way (if (= :done (:kind e)) {:kind :nothing-possible :cause :unsealed :data {:reason :unsealed :text (str "the " (name way) " did not leave the body enclosed")}} e))
                        (if (seq (combat/hostiles (:primitives c) adjacent-reach)) :continue (recur))))))))))
