(ns jobs.survival.plug-in
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.combat :as combat]
            [jobs.lib.shelter :as sh]
            [jobs.lib.spent :as spent]
            [jobs.survival.plug-way :as way]))

(def doc
  "Shut the body in where it stands, atomically: no night or hostile logic, no walking; whoever calls it decides when.
  Needs nothing to start. One call is a whole attempt: it ends :done {:pos :way :start :placed} once the body is enclosed (a roof within
  :roof-height over no open cell round the feet), at once when it already is.
  Ways (:ways) are child jobs: jobs.survival.plug-walls (the open sides, a roof support and the roof with carried blocks),
  plug-pit (dig down under a solid side and plug the hole), plug-pocket (dig a pocket beside the body, step in, wall the way in).
  It quotes the ways the call allows, starts the cheapest (ties in the order walls, pit, pocket) and, when that one ends other than
  :done, the next cheapest (after a :continue step when a hostile is within 2 blocks); a way that cannot be done here (quote ##Inf) is dropped. Blocks are the call's :use :blocks, else every
  :build filler, carried or dug. None left is nothing-possible :no-way {:text :tried [ways that failed] :dropped [ways that cannot be done]
  :why {way the child's cause}}.
  :start is the cell the call began at, :placed the blocks placed ({:cell [x y z] :item}), also with an ending other than :done. Every placed block goes to the scaffold ledger with :purpose (default :retreat-plug, taken back by jobs.access.cleanup once no
  hostile is near; :shelter is left alone). A cell in another's zone is used only as a last resort (retreat.trespass-last-resort).
  Quote: time.")

(a/defargs args
  {:ways {:doc "the ways to enclose the body, tried in the order :walls, :pit, :pocket" :spec (a/set-of #{:walls :pit :pocket}) :default #{:walls :pit :pocket}}
   :purpose {:doc "ledger purpose of the placed blocks (:retreat-plug: taken back by the cleanup near no hostile; :shelter: left alone)" :spec #{:retreat-plug :shelter} :default :retreat-plug}
   :roof-height {:doc "a solid block within this many blocks above counts as a roof" :spec (a/int-in 0 nil) :default sh/default-roof-height}
   :radius {:doc "melee hostiles within this many blocks decide which side is walled first and stand in the way of a cell" :spec (a/num-in 0 nil) :default 8}
   :ranged-radius {:doc "ranged hostiles within this many blocks decide which side is walled first" :spec (a/num-in 0 nil) :default 16}
   :max-places {:doc "placements per step" :spec (a/int-in 1 nil) :default 4}
   :on-lava {:doc ":seal: lava a dig lays open is filled with a carried block; :stop: the pit or pocket is dropped and the lava left" :spec #{:seal :stop} :default :seal}})

(a/defargs settings
  {::job-time-s {:default 120 :doc "Seconds a plug-in may take." :spec (a/num-in 0 nil)}
   ::mean-s {:default 12 :doc "Seconds an enclosing is guessed to take, for the quote." :spec (a/num-in 0 nil)}})

(def target [])

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s} :ways #{:walls :pit :pocket}})

(def ways [:walls :pit :pocket])

(def children
  "Each way's job and the args of this call it takes."
  {:walls ['jobs.survival.plug-walls [:purpose :roof-height :radius :ranged-radius :max-places]]
   :pit ['jobs.survival.plug-pit [:purpose :roof-height :radius :on-lava]]
   :pocket ['jobs.survival.plug-pocket [:purpose :roof-height :radius :ranged-radius :max-places :on-lava]]})

(defn done? [c] (way/done? c (:way (ctx/mem c))))

(defn quote-of [c]
  (let [mean (if (way/enclosed? c) 0 (settings/get settings ::mean-s))]
    {:uses {:job-time [0 mean (max (* 3 mean) (settings/get settings ::job-time-s))]} :done [0 1 1]}))

(defn child-args [c way]
  (merge (select-keys (:args c) (second (children way)))
         (when-let [blocks (ctx/use c :blocks)] {:use {:blocks blocks}})))

(defn ^:async cost-of
  "Seconds the child of way quotes, ##Inf when it cannot be done here or cannot be quoted."
  [c way]
  (let [q (await (ctx/quote c (first (children way)) (child-args c way)))]
    (if (contract/ending? q) ##Inf (get-in q [:uses :job-time 1] ##Inf))))

(defn ^:async choose
  "{:way the cheapest allowed way not failed (nil when none) :dropped [ways that cannot be done]}."
  [c]
  (let [failed (:failed-ways (ctx/mem c) #{})
        left (remove failed (filter #(ctx/way? c %) ways))
        costs (loop [[w & more] left acc []]
                (if (nil? w) acc (recur more (conj acc [w (await (cost-of c w))]))))
        able (remove (comp infinite? second) costs)]
    {:way (first (first (sort-by second able)))
     :dropped (mapv first (filter (comp infinite? second) costs))}))

(defn end!
  "The call's ending with what the children placed."
  [c kind cause data]
  (ctx/end c kind cause (merge data (spent/summary (spent/log c)) (select-keys (ctx/mem c) [:placed :start]))))

(defn absorb!
  "A child's ending: what it placed goes in the log and in :placed."
  [c data]
  (spent/absorb! c data)
  (ctx/update-mem! c update :placed (fnil into []) (:placed data)))

(def adjacent-reach
  "Blocks within which a hostile is at the body: a way that failed ends the step so the round and the triggers see it before the next way starts."
  2)

(defn ^:async round
  "Start the cheapest way; one that ends other than :done is dropped for this call and the next is chosen on the next step when a hostile is at the body, else at once."
  [c]
  (ctx/update-mem! c update :start #(or % (sh/feet (:primitives c))))
  (loop []
    (if (way/enclosed? c)
      (end! c :done nil {:pos (sh/feet (:primitives c)) :way (:way (ctx/mem c))})
      (let [chosen (when-not (:way (ctx/mem c)) (await (choose c)))
            _ (when chosen (ctx/update-mem! c #(-> % (assoc :way (:way chosen)) (update :dropped (fn [d] (vec (distinct (into (or d []) (:dropped chosen)))))))))
            way (:way (ctx/mem c))]
        (if (nil? way)
          (end! c :nothing-possible :no-way {:text (str "cannot enclose the body: " (if-let [why (seq (vals (:why (ctx/mem c))))] (str/join "; " why) "no way left that can be done here"))
                                             :tried (vec (:failed-ways (ctx/mem c))) :dropped (:dropped (ctx/mem c)) :why (:why (ctx/mem c)) :statuses (:statuses (ctx/mem c))})
          (let [e (await (ctx/call c way (first (children way)) (child-args c way)))]
            (cond
              (= :continue e) :continue
              (= :done (:kind e)) (do (absorb! c (:data e))
                                      (end! c :done nil {:pos (sh/feet (:primitives c)) :way way}))
              :else (do (absorb! c (:data e))
                        (ctx/update-mem! c #(-> % (dissoc :way) (update :failed-ways (fnil conj #{}) way) (assoc-in [:why way] (or (:text (:data e)) (:cause e))) (assoc-in [:statuses way] (:status (:data e)))))
                        (if (seq (combat/hostiles (:primitives c) adjacent-reach)) :continue (recur))))))))))
