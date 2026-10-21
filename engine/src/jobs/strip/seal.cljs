(ns jobs.strip.seal
  (:require [jobs.lib.ready :as ready]
            [engine.contract :as contract]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.seal :as seal]
            [jobs.lib.waits :as waits]
            [jobs.lib.blocks :as b]
            [jobs.lib.choose :as choose]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
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

  The section is filled by a jobs.blocks.fill-cells child (:order :given, :clear-loose, :least 1: one cell a call, so the body stands on its side again before each): every cell that is not a solid block is
  closed with a carried block of the call's :use {:blocks} (default the :seal fillers). Loose things in it (rails, cobwebs,
  torches, fences, ladders ...) are dug first and the drops picked up (torches come back). The rows go up from the bottom, the centre
  bottom cell last, so the body keeps its view. A container in the section is never dug (:blocked). Zones, claims and plans are the
  place rules'.
  With none of the blocks carried they are fetched first (jobs.lib.need, from the call's :ways; a failed fetch ends the call with its
  ending as the :because), before the walk to the stand; with :ways #{:carried} it stops :no-blocks at once.

  done? is :ok when the section is shut, its drops picked up and no child under way: ends :done {:placed n} (0 =
  already shut). Else it ends with data {:placed n
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

(def ways need/fetch-ways)

(a/defargs args
  {:at {:doc "the bottom centre cell of the section, [x y z]" :spec ::a/pos :default nil}
   :axis {:doc "the way the corridor runs" :spec #{:x :z} :default nil}
   :side {:doc "the side the body ends on: :from (lower coordinate) or :to" :spec #{:from :to} :default nil}
   :width {:doc "cells across the corridor" :spec (a/int-in 1 9) :default 3}
   :height {:doc "cells up the corridor" :spec (a/int-in 1 9) :default 3}
   :ignore-zones? :shared})

(defn kind
  "What a block name means for the seal: :solid (done), :open (replaceable or not read yet), :loose (dig it first)
  or :container (never touched)."
  [n]
  (if (nil? n) :open (seal/kind n)))

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

(defn blocks
  "The block names the call closes with: its :use {:blocks} (the engine fills the :seal fillers carried when the caller gives none)."
  [c]
  (ctx/use c :blocks))

(defn fetch-opts
  "The limits {:how} for fetching blocks, nil when the call's ways leave only :carried."
  [c]
  (when-let [how (seq (need/fetch-how c))]
    {:how (set how)}))

(defn ^:async fetch-blocks!
  "With none of the blocks carried: :continue while the fetch waits, :again once got, the :no-blocks stop (the failed fetch's
  ending as its :because), also with :ways #{:carried}. nil when one is carried."
  [c open]
  (when-not (b/chosen c (blocks c))
    (if-let [o (fetch-opts c)]
      (let [e (await (need/blocks! c (or (seq (blocks c)) (choose/filler-names c :seal)) open o))]
        (cond (= :continue e) :continue
              (= :done (:kind e)) :again
              :else (contract/with-because (stop! c :no-blocks {:short open}) e)))
      (stop! c :no-blocks {:short open}))))

(defn cell-map [cell] {:x (cell 0) :y (cell 1) :z (cell 2)})

(defn ^:async fill! [c todo]
  (let [{:keys [at axis width height ignore-zones?]} (:args c)
        names (or (seq (blocks c)) (choose/filler-names c :seal))
        e (waits/waiting-out c (await (ctx/call c :fill 'jobs.blocks.fill-cells
                           (cond-> {:cells (section (at-of at) axis width height) :order :given :least 1 :clear-loose true
                                    :use {:blocks (vec names)} :ways #{:carried}}
                             ignore-zones? (assoc :ignore-zones? true)))))
        short (count todo)
        cell (some-> (get-in e [:cause :cell]) (->> ((juxt :x :y :z))) vec)]
    (cond
      (= :continue e) :continue
      (= :done (:kind e)) (if (or (empty? todo) (pos? (:mended (:data e) 0)))
                            (do (ctx/update-mem! c update :placed (fnil + 0) (:mended (:data e))) :again)
                            (stop! c :unreachable {:cell (ffirst todo)}))
      :else
      (do (ctx/update-mem! c update :placed (fnil + 0) (:mended (:data e) 0))
          (case (get-in e [:cause :reason])
            :need (contract/with-because (stop! c :no-blocks {:short short}) e)
            :refused (stop! c :refused {:cell cell})
            :blocked (stop! c :blocked {:cell cell})
            (stop! c :unreachable {:cell (some-> (first (:left (:data e))) ((juxt :x :y :z)) vec)}))))))

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
        todo (open-cells c)]
    (cond
      (checks/child-under-way? c) (await (fill! c todo))
      (empty? todo) (finish! c :done nil {})
      (some #(= :container (second %)) todo) (stop! c :blocked {:cell (first (first (filter #(= :container (second %)) todo)))})
      (not (b/chosen c (blocks c))) (await (fetch-blocks! c (count todo)))
      (not= (stand at axis side) (reach/standing-vec (:primitives c))) (await (go-stand! c at axis side))
      :else (await (fill! c todo)))))

(defn done?
  "Every cell of the section is solid, the drops are picked up and no child is under way; the cells shut and open."
  [c]
  (let [{:keys [width height]} (:args c)
        n (if (= :ok (first (bad-args c))) (count (open-cells c)) (* width height))
        placed (:placed (ctx/mem c) 0)
        data [:units :cells :done (- (* width height) n) :left n :result {:placed placed}]]
    (cond
      (pos? n) (apply contract/later :open (str "seal: " n " cells open") data)
      (checks/child-under-way? c) (apply contract/later :wrapping-up "seal: the section is shut, wrapping up" data)
      :else (apply contract/ok :sealed (str "seal sealed: placed " placed) data))))

(defn quote-of [c]
  (let [t (* (settings/get settings ::s-per-cell) (if (= :ok (first (bad-args c))) (count (open-cells c)) 0))]
    {:uses {:job-time [0 t (max t (settings/get settings ::job-time-s))]}}))

(defn ^:async round [c]
  (await (pace/steps! c #(step! c))))
