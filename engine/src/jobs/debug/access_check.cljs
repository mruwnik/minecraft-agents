(ns jobs.debug.access-check
  (:require [engine.args :as a]
            [jobs.lib.access.rules :as rules]
            [engine.ctx :as ctx]
            [jobs.lib.util :as u]))

(def doc
  "Debug job: report whether the body may dig each given cell and whether it may place a block there
  (jobs.lib.access.rules). Digs and places nothing.
  The cells come from :cells, or from the box :from/:to (inclusive, at most 400 cells).
  The rules' inputs are :zones, :footprints, :claims and :ledger, all empty by default. Pass :zones nil to see the
  no-zone-list refusal. :self is who the zones and claims are judged for (default the body's name).
  :ignore-zones? judges as a job that acts regardless.
  One round ends :done with {:verdicts [{:cell [x y z] :block name-or-nil :dig verdict :place verdict} ...]}. An
  ok dig verdict may carry :hazards. One :access-check.result event gives the counts of ok and refused digs and
  places. Bad arguments end with {:status :bad-args :reason text}.")

(a/defargs args
  {:cells {:doc "cells [[x y z] ...] to check" :spec (a/coll-of a/position?) :default nil}
   :from {:doc "box corner [x y z] (inclusive), with :to, instead of :cells" :spec a/position? :default nil}
   :to {:doc "opposite box corner [x y z]; at most 400 cells in all" :spec a/position? :default nil}
   :zones {:doc "zone boxes [{:name :min [x y z] :max [x y z] :allow #{:dig :place}}], nil = no zone list loaded" :spec (a/coll-of map?) :default []}
   :footprints {:doc "cells [[x y z] ...] other plans claim" :spec (a/coll-of a/position?) :default []}
   :ledger {:doc "cells [[x y z] ...] holding this body's own scaffold blocks" :spec (a/coll-of a/position?) :default []}
   :claims {:doc "area claims [{:id :owner :status :active :until ms :min :max}] to judge against" :spec (a/coll-of map?) :default []}
   :self {:doc "the name zones and claims are judged for; nil: this body's name" :spec a/name? :default nil}
   :now {:doc "the clock in ms for the claims; nil: the body's clock" :spec (a/num-in 0 nil) :default nil}
   :ignore-zones? {:doc "judge as a job that acts regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(def max-cells 400)

(defn check [_c] true)

(defn span [a b] (range (min a b) (inc (max a b))))

(defn box-cells [[x0 y0 z0] [x1 y1 z1]]
  (vec (for [x (span x0 x1) y (span y0 y1) z (span z0 z1)] [x y z])))

(defn cells-of
  "The cells to check from the args, or {:error text}."
  [{:keys [cells from to]}]
  (cond
    (seq cells) (vec cells)
    (and (= 3 (count from)) (= 3 (count to)))
    (let [n (* (count (span (from 0) (to 0))) (count (span (from 1) (to 1))) (count (span (from 2) (to 2))))]
      (if (> n max-cells)
        {:error (str "box covers " n " cells, at most " max-cells)}
        (box-cells from to)))
    :else {:error "give :cells or a box :from/:to"}))

(defn feet-cell [c]
  (let [{:keys [x y z]} (u/self-pos c)]
    [(js/Math.floor x) (js/Math.floor y) (js/Math.floor z)]))

(defn verdicts
  "The dig and place verdict of every cell."
  [block-at feet {:keys [zones footprints ledger claims self now ignore-zones?]} cells]
  (let [in {:block-at block-at :feet feet :zones zones :footprints (set footprints) :ledger (set ledger)
            :claims claims :self self :now now :ignore-zones? (boolean ignore-zones?)}]
    (mapv (fn [cell]
            (let [in (assoc in :cell cell)]
              {:cell cell :block (block-at cell) :dig (rules/may-dig? in) :place (rules/may-place? in)}))
          cells)))

(defn tally [vs action]
  (let [{ok true no false} (frequencies (map #(:ok (action %)) vs))]
    {:ok (or ok 0) :refused (or no 0)}))

(defn ^:async round [c]
  (let [p (:primitives c)
        cells (cells-of (:args c))]
    (if (:error cells)
      (do (ctx/result! c {:status :bad-args :reason (:error cells)})
          :done)
      (let [block-at (fn [[x y z]] (u/block-name p {:x x :y y :z z}))
            args (:args c)
            vs (verdicts block-at (feet-cell c)
                         (assoc args :self (or (:self args) (ctx/self-name c)) :now (or (:now args) (ctx/now c)))
                         cells)
            dig (tally vs :dig)
            place (tally vs :place)]
        (ctx/result! c {:verdicts vs})
        (ctx/emit! c :access-check.result :info
                   {:verdicts vs :dig dig :place place
                    :text (str (count vs) " cells: dig " (:ok dig) " ok " (:refused dig) " refused, place "
                               (:ok place) " ok " (:refused place) " refused")})
        :done))))
