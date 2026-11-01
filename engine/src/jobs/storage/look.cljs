(ns jobs.storage.look
  (:require [jobs.lib.ready :as ready]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.checks :as checks]
            [jobs.lib.chests :as chests]
            [jobs.lib.pace :as pace]
            [jobs.lib.stock :as stock]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(def doc
  "Open each chest of :chests (when nil: the storage group plus every chest whose contents the body has booked, as ./bin/observe chests lists) and book what it holds, taking nothing out: a player's look
  into a chest. Walks into reach (one go-to), opens it (jobs.lib.chests/look-in!, which digs a seen block off its lid
  once) and records the stacks in body memory :fetch/stock, which ./bin/observe chests lists.
  Needs: some chest, else waits :no-chest; a chest that zones or claims of another refuse :take, else waits
  :refused {:zones :claims}. :ignore-zones? true skips it.
  Ends done {:looked {[x y z] {name n}} :gone [cells]} once every chest was opened or found gone (the message counts them); else nothing-possible (cause the open status,
  :unreachable ..) or condition-failed :missing (every chest was gone). A chest that is no container any more is noted gone (its booking
  and a recorded :chest place are retracted) and the look carries on; :gone lists them.")

(a/defargs args
  {:chests {:doc "[x y z] or {:x :y :z} of each chest to open; the storage group when nil" :spec (a/coll-of ::a/pos) :default nil}
   :ignore-zones? :shared})

(a/defargs settings
  {::chest-s {:default 15 :doc "Seconds a quote counts for one chest: the walk and the look." :spec (a/num-in 0 nil)}
   ::job-time-s {:default 600 :doc "Seconds a look may last: every walk and open." :spec (a/num-in 0 nil)}})

(def target [:chests])
(def safe? ready/threatened)
(def fit? ready/ok)
(def defaults {:spend {:job-time ::job-time-s}})

(defn cells
  "The chests to open, [x y z] each once: the :chests argument, else the storage group and the booked chests."
  [c]
  (let [view (ctx/view c)]
    (vec (distinct (map storage/cell-of (or (:chests (:args c)) (concat (storage/group view) (keys (stock/stock-of view)))))))))

(defn gone [c] (:gone (ctx/mem c) []))

(defn pending [c] (remove (into (set (keys (:looked (ctx/mem c)))) (gone c)) (cells c)))

(defn can-start? [c]
  (contract/all [(if (seq (cells c)) (contract/ok :chest "a chest is known") (contract/later :no-chest "no chest known"))
                 ((checks/container-refused some :take #(map chests/cell-pos (cells %))) c)]))
(def can-run? can-start?)

(defn done? [c]
  (let [left (count (pending c))
        data [:units :chests :done (- (count (cells c)) left) :left left :result {:looked (:looked (ctx/mem c) {})}]]
    (if (and (zero? left) (seq (cells c)))
      (apply contract/ok :looked (str "look: " (count (:looked (ctx/mem c))) " chest(s) opened, " (count (gone c)) " gone") data)
      (apply contract/later :looking (str "look: " left " chest(s) to open") data))))

(defn quote-of [c]
  (let [t (* (count (pending c)) (settings/get settings ::chest-s))]
    {:uses {:job-time [(/ t 4) t (max t (settings/get settings ::job-time-s))]}}))

(defn stop! [c kind cause]
  (ctx/end c kind cause {:gave-up true :looked (:looked (ctx/mem c) {})}))

(defn ^:async step!
  "Open the next chest: :again, :continue while go-to waits, or the ending."
  [c]
  (if-let [cell (first (pending c))]
    (let [seen (await (chests/look-in! c (chests/cell-pos cell)))
          status (:status seen)]
      (cond
        (= :continue seen) :continue
        (= "ok" status) (do (ctx/update-mem! c assoc-in [:looked cell] (stock/stacks->items (:items seen)))
                            (u/progress! c)
                            :again)
        (= "missing" status) (do (ctx/forget-where! c stock/stock-kind #(= cell (:pos %)))
                                 (ctx/update-mem! c update :gone (fnil conj []) cell)
                                 :again)
        :else (stop! c :nothing-possible (keyword status))))
    (if (and (empty? (:looked (ctx/mem c))) (seq (gone c)))
      (stop! c :condition-failed :missing)
      (ctx/end c :done nil {:looked (:looked (ctx/mem c) {}) :gone (gone c)}))))

(defn ^:async round [c]
  (await (pace/steps! c (fn ^:async look-step [] (await (step! c))))))
