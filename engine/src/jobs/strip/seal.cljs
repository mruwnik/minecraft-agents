(ns jobs.strip.seal
  (:require [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.blocks :as b]
            [jobs.lib.choose :as choose]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.found :as found]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach :as reach]
            [jobs.lib.access.rules :as rules]))


(def doc
  "Block a corridor cross-section off from the base side (a mineshaft corridor, 3 wide and 3 high). One call does it
  all (:continue only while a child waits on the world); a cut or restart reads the world again and goes on.

  :at is the bottom centre cell of the section (where a rail lies), :axis the way the corridor runs, :side the side the
  body must end on (:from the lower coordinate, :to the higher). The body works from the stand cell, one step back from
  :at on that side. A body on the far side first walks to it (go-to child); no way there ends :wrong-side.

  Every cell of the section that is not a solid block is closed with a carried block of the call's :use {:blocks} (default the :seal fillers; a
  jobs.blocks.place child).
  Loose things in it (rails, cobwebs, torches, fences, ladders ...) are dug first (jobs.blocks.dig child, no fetch) and
  the drops picked up (torches come back).
  With none of the blocks carried they are fetched first (jobs.lib.need, :fetch; a failed fetch ends the call with its
  ending as the :because), before the walk to the stand; with :fetch off for items it stops :no-blocks at once. The rows go up from the bottom, the centre bottom cell last, so the body
  keeps its view. A container in the section is never dug (:blocked). Zones, claims and plans are the place rules'.

  done? is :ok when the section is shut, its drops picked up and no child under way: ends :done {:placed n} (0 =
  already shut). Else strip.seal.stopped (warn) with data {:placed n
  :cell [x y z] :short n :why}: impossible :bad-args; condition-failed :refused (zones, claims, plans), :blocked (a container in
  the section); nothing-possible :no-blocks (:short names the cells still open), :unreachable, :wrong-side. Spends
  :time (default ::job-time-s).")

(a/defargs settings
  {::job-time-s {:default 600 :doc "Seconds a seal may take before it ends." :spec (a/num-in 0 nil)}
   ::s-per-cell {:default 4 :doc "Seconds a quote counts for one open cell of the section." :spec (a/num-in 0 nil)}})

(def target [:at :axis :side])

(def defaults {:spend {:job-time ::job-time-s} :use {:blocks :seal}})

(def safe? (ready/safe-at-by-night #(a/cell (:at (:args %)))))

(def fit? ready/ok)

(a/defargs args
  {:at {:doc "the bottom centre cell of the section, [x y z]" :spec ::a/pos :default nil}
   :axis {:doc "the way the corridor runs" :spec #{:x :z} :default nil}
   :side {:doc "the side the body ends on: :from (lower coordinate) or :to" :spec #{:from :to} :default nil}
   :width {:doc "cells across the corridor" :spec (a/int-in 1 9) :default 3}
   :height {:doc "cells up the corridor" :spec (a/int-in 1 9) :default 3}
   :fetch {:doc "get the blocks to close with (jobs.lib.need): true, a set of kinds or a map of limits; false (or without :item) stops :no-blocks" :spec need/option? :default true}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}})

(def loose #"(^|_)(rail|torch|fence|fence_gate|ladder|lantern|button|lever|carpet|sign|banner|pressure_plate)$|^cobweb$")

(defn kind
  "What a block name means for the seal: :solid (done), :open (replaceable or not read yet), :loose (dig it first)
  or :container (never touched)."
  [n]
  (cond
    (or (nil? n) (rules/replaceable n)) :open
    (found/containers n) :container
    (re-find loose n) :loose
    :else :solid))

(defn at-of [at] (some-> at b/cell))

(defn shift [[x y z] axis k]
  (if (= axis :x) [(+ x k) y z] [x y (+ z k)]))

(defn stand
  "The cell the body works from: one step back from at, on the side."
  [at axis side]
  (shift at axis (if (= side :from) -1 1)))

(defn section
  "The cells of the cross-section at at, rows from the bottom, the outer cells of a row first, the centre bottom last."
  [[x y z] axis width height]
  (let [offs (range (- (quot width 2)) (+ (quot width 2) (mod width 2)))
        cell (fn [row off] (let [[cx cy cz] (if (= axis :x) [x (+ y row) (+ z off)] [(+ x off) (+ y row) z])] [cx cy cz]))
        cells (for [row (range height) off (sort-by #(- (js/Math.abs %)) offs)] [row off (cell row off)])]
    (mapv #(% 2) (sort-by (fn [[row off]] [(if (and (zero? row) (zero? off)) 1 0) row]) cells))))

(defn bad-args [c]
  (let [{:keys [at axis side]} (:args c)]
    (cond
      (nil? at) (contract/never :bad-args "needs :at [x y z]")
      (nil? axis) (contract/never :bad-args "needs :axis :x or :z")
      (nil? side) (contract/never :bad-args "needs :side :from or :to")
      :else (contract/ok :args "at, axis and side given"))))

(def can-start? bad-args)

(def kinds
  "The kind of ending each stop reason is."
  {:refused :condition-failed :blocked :condition-failed :no-blocks :nothing-possible :unreachable :nothing-possible :wrong-side :nothing-possible})

(defn finish!
  [c kind reason detail]
  (let [result (merge {:placed (:placed (ctx/mem c) 0)} detail)
        text (str "seal " (name (or reason :sealed)) ": placed " (:placed result)
                  (when (:short result) (str ", " (:short result) " cells short"))
                  (when (:cell result) (str " at " (pr-str (:cell result)))))]
    (when-not (= :done kind)
      (ctx/emit! c :strip.seal.stopped :warn (assoc result :kind kind :cause reason :text text)))
    (ctx/end c kind reason (assoc result :text text))))

(defn stop! [c reason detail] (finish! c (kinds reason) reason detail))

(defn on-side? [c [x _ z] axis side]
  (let [[fx _ fz] (reach/standing-vec (:primitives c))
        k (if (= axis :x) fx fz)
        plane (if (= axis :x) x z)]
    (if (= side :from) (< k plane) (> k plane))))

(defn ^:async go-stand! [c at axis side]
  (let [stand-cell (stand at axis side)
        r (await (child/call-walk* c :walk {:pos stand-cell :range 0 :ways #{:walk :clear-seal}}))]
    (cond
      (= :continue r) :continue
      (= stand-cell (reach/standing-vec (:primitives c))) :again
      (on-side? c at axis side) (stop! c :unreachable {:cell stand-cell})
      :else (stop! c :wrong-side {:cell stand-cell}))))

(defn ^:async collect! [c]
  (let [r (await (ctx/call c :collect 'jobs.forestry.collect-drops {:radius 3}))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c dissoc :collect) :again))))

(defn ^:async dig! [c cell]
  (let [o (await (b/dig-cell! c {:x (cell 0) :y (cell 1) :z (cell 2)} (select-keys (:args c) [:ignore-zones?])))]
    (case o
      :continue :continue
      (:dug :missing) (do (ctx/update-mem! c assoc :collect true) :again)
      :refused (stop! c :refused {:cell cell})
      (stop! c :unreachable {:cell cell :why o}))))

(defn blocks
  "The block names the call closes with: its :use {:blocks} (the engine fills the :seal fillers carried when the caller gives none)."
  [c]
  (ctx/use c :blocks))

(defn fetch-opts
  "The limits {:how :explore :minutes} for fetching blocks, nil when :fetch does not allow items."
  [c]
  (when-let [o (need/opts c 'jobs.strip.seal)]
    (when (contains? (:what o) :item) (select-keys o [:how :explore :minutes]))))

(defn ^:async fetch-blocks!
  "With none of the blocks carried: :continue while the fetch waits, :again once got, the failed ending, or the :no-blocks
  stop with :fetch off. nil when one is carried."
  [c open]
  (when-not (b/chosen c (blocks c))
    (if-let [o (fetch-opts c)]
      (let [e (await (need/blocks! c (or (seq (blocks c)) (choose/fillers :seal)) open o))]
        (cond (= :continue e) :continue
              (= :done (:kind e)) :again
              :else e))
      (stop! c :no-blocks {:short open}))))

(defn ^:async place! [c cell open]
  (let [{:keys [ignore-zones?]} (:args c)
        item (b/chosen c (blocks c))]
    (if-not item
      (stop! c :no-blocks {:short open})
      (let [o (await (b/place-cell! c {:x (cell 0) :y (cell 1) :z (cell 2)} item {:ignore-zones? ignore-zones?}))]
        (case o
          :continue :continue
          :placed (do (ctx/update-mem! c update :placed (fnil inc 0)) :again)
          :already :again
          :refused (stop! c :refused {:cell cell})
          :need (stop! c :no-blocks {:short open})
          (stop! c :unreachable {:cell cell :why o}))))))

(defn open-cells
  "The cells of the section that are not solid, [cell kind] each."
  [c]
  (let [{:keys [at axis side width height]} (:args c)
        p (:primitives c)]
    (->> (section (at-of at) axis width height)
         (map (fn [cell] [cell (kind (b/target-name p {:x (cell 0) :y (cell 1) :z (cell 2)} nil))]))
         (filterv #(not= :solid (second %))))))

(defn ^:async step! [c]
  (let [{:keys [at axis side]} (:args c)
        at (at-of at)
        todo (open-cells c)
        [cell k] (first todo)]
    (cond
      (:collect (ctx/mem c)) (await (collect! c))
      (empty? todo) (finish! c :done nil {})
      (some #(= :container (second %)) todo) (stop! c :blocked {:cell (first (first (filter #(= :container (second %)) todo)))})
      (not (b/chosen c (blocks c))) (await (fetch-blocks! c (count todo)))
      (not= (stand at axis side) (reach/standing-vec (:primitives c))) (await (go-stand! c at axis side))
      (= :loose k) (await (dig! c cell))
      :else (await (place! c cell (count todo))))))

(defn done?
  "Every cell of the section is solid, the drops are picked up and no child is under way; the cells shut and open."
  [c]
  (let [{:keys [width height]} (:args c)
        n (if (= :ok (first (bad-args c))) (count (open-cells c)) (* width height))
        placed (:placed (ctx/mem c) 0)
        data [:units :cells :done (- (* width height) n) :left n :result {:placed placed}]]
    (cond
      (pos? n) (apply contract/later :open (str "seal: " n " cells open") data)
      (or (:collect (ctx/mem c)) (checks/child-under-way? c)) (apply contract/later :wrapping-up "seal: the section is shut, wrapping up" data)
      :else (apply contract/ok :sealed (str "seal sealed: placed " placed) data))))

(defn quote-of [c]
  (let [t (* (settings/get settings ::s-per-cell) (if (= :ok (first (bad-args c))) (count (open-cells c)) 0))]
    {:uses {:job-time [0 t (max t (settings/get settings ::job-time-s))]}}))

(defn ^:async round [c]
  (await (pace/steps! c #(step! c))))
