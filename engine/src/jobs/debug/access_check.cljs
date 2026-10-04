(ns jobs.debug.access-check
  (:require [engine.access.rules :as rules]
            [engine.ctx :as ctx]
            [engine.jobs.util :as u]))

(def doc
  "Debug job: report, for each cell given, whether the body may dig it and whether it may place a block there
  (engine.access.rules), without digging or placing anything. One round ends :done with the result
  {:verdicts [{:cell [x y z] :block name-or-nil :dig verdict :place verdict} ...]}, also emitted as one
  :access-check.result event (counts of ok and refused digs and places). The cells come from :cells or from
  the box :from/:to (inclusive, at most 400 cells). :zones, :footprints, :claims and :ledger are the rules' inputs
  and default to empty; pass :zones nil to see the no-zone-list refusal; :self (default the body's name) is who
  the zones and claims are judged for, :ignore-zones? the opt-out. An ok dig verdict may carry :hazards. Bad arguments end with
  {:status :bad-args :reason text}.")

(def args
  {:cells {:doc "cells [[x y z] ...] to check" :default nil}
   :from {:doc "box corner [x y z] (inclusive), with :to, instead of :cells" :default nil}
   :to {:doc "opposite box corner [x y z]; at most 400 cells in all" :default nil}
   :zones {:doc "zone boxes [{:name :min [x y z] :max [x y z] :allow #{:dig :place}}], nil = no zone list loaded" :default []}
   :footprints {:doc "cells [[x y z] ...] other plans claim" :default []}
   :ledger {:doc "cells [[x y z] ...] holding this body's own scaffold blocks" :default []}
   :claims {:doc "area claims [{:id :owner :status :active :until ms :min :max}] to judge against" :default []}
   :self {:doc "the name zones and claims are judged for; nil: this body's name" :default nil}
   :now {:doc "the clock in ms for the claims; nil: the body's clock" :default nil}
   :ignore-zones? {:doc "judge as a job that acts regardless of zones and claims; the rules of the game allow it" :default false}})

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
