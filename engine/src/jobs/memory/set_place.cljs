(ns jobs.memory.set-place
  (:require [engine.args :as a]
            [jobs.lib.checks :as checks]
            [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.util :as u]
            [engine.memory :as mem]
            [jobs.lib.places :as places]))

(def doc
  "Record a named place (:bed, :chest, :home, any name) in body memory, in one round.
  :pos is [x y z] or {:x :y :z}, floored to its cell. Without it the cell the body stands in is used.
  With :block (a block name such as \"white_bed\" or \"chest\"; \"bed\" is any colour of bed) the block must stand
  at the position or within 1 of it. The place is recorded at the block's own cell.
  A place already recorded under that name is moved. Places are this body's own memory. Readers (sleep, deposit,
  the (place :name) condition fact) find them by name.
  Refused (ends :condition-failed with cause {:reason :text}): :bad-name, :reserved-name (a
  memory kind the engine uses), :not-a-place (the name holds other memory), :bad-pos, :bad-block,
  :no-such-block, :ambiguous (two such blocks lie within 1 and neither at the position: give the exact one),
  :not-loaded.
  A name, :pos or :block wrong whatever the world holds is refused before any round: ends :impossible {:reason :bad-args :why <that reason> :text}.
  Success: ends :done with data {:name :pos}; the memory layer emits info place.set (with :was).
  To run it on a body that is running: {:op :submit :next? true :spec (jobs.memory.set-place {...})}. The
  outcome is in the event stream.")

(a/defargs args
  {:name {:doc "the place's name: a keyword or string of 1 to 32 lowercase letters, digits and dashes" :spec (a/or-of keyword? string?) :default nil}
   :pos {:doc "[x y z] or {:x :y :z}; nil: where the body stands" :spec ::a/pos :default nil}
   :block {:doc "block name that must stand at or within 1 of :pos (\"bed\" matches any *_bed); the place is recorded at it" :spec a/name? :default nil}})

(def target [:name])

(a/defargs settings
  {::job-time-s {:default 5 :doc "Seconds one call may take." :spec (a/int-in 1 nil)}})

(def defaults {:spend {:job-time ::job-time-s}})

(defn standing
  "The cell the body stands in."
  [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    {:x (js/Math.floor x) :y (js/Math.floor y) :z (js/Math.floor z)}))

(def arg-refusals #{:bad-name :reserved-name :not-a-place :bad-pos :bad-block})

(defn bad-args
  "The refusal cause when the name, :pos or :block of the call is wrong whatever the world holds."
  [c]
  (let [r (places/resolve-set (ctx/view c) (constantly nil) (standing c) (:args c))]
    (when (arg-refusals (:reason r))
      {:reason (:reason r) :text (:message r)})))

(defn can-start?
  "Never for a name, :pos or :block that is wrong whatever the world holds; later for a name that is no place yet."
  [c]
  (let [cause (bad-args c)]
    (if (= :not-a-place (:reason cause))
      (checks/cause-status :later cause)
      (checks/cause-status :never (some-> cause (assoc :reason :bad-args :why (:reason cause)))))))

(defn done?
  "Done when the place is already recorded at the cell the call resolves to."
  [c]
  (let [p (:primitives c)
        r (places/resolve-set (ctx/view c) #(u/seen-name p %) (standing c) (:args c))
        set? (and (not (:reason r)) (= (:pos r) (mem/place (ctx/view c) (:name r))))]
    (if set?
      (contract/ok :set (str "place " (:name r) " is at " (pr-str (:pos r))) :units :places :done 1 :left 0 :result {:name (:name r) :pos (:pos r)})
      (contract/later :not-set (str "place " (or (:name r) (:name (:args c))) " not at the cell asked") :units :places :done 0 :left 1
                      :result (select-keys r [:name :pos])))))

(defn quote-of [_c] {:uses {:job-time [0 0 1]}})

(def safe? ready/ok)
(def fit? ready/ok)

(defn round [c]
  (let [p (:primitives c)
        r (places/resolve-set (ctx/view c) #(u/seen-name p %) (standing c) (:args c))]
    (if (:reason r)
      (ctx/end c :condition-failed {:reason (:reason r) :text (:message r)})
      (let [{place-name :name :keys [pos]} r]
        (ctx/remember-asked! c place-name {:pos pos} mem/place-policy)
        (ctx/end c :done nil {:name place-name :pos pos})))))
