(ns jobs.strip.seal
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
                        [jobs.lib.blocks :as b]
            [jobs.lib.child :as child]
            [jobs.lib.found :as found]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach :as reach]
            [jobs.lib.access.rules :as rules]))

(def max-steps "Pieces of work of one call before it gives the round back with :continue." 400)

(defn feet-of
  "The cell [x y z] the body stands on."
  [c]
  (let [{:keys [x y z]} (reach/standing-cell (:primitives c))]
    [x y z]))

(def doc
  "Block a corridor cross-section off from the base side (a mineshaft corridor, 3 wide and 3 high). One call does it
  all (:continue only while a child waits on the world); a cut or restart reads the world again and goes on.

  :at is the bottom centre cell of the section (where a rail lies), :axis the way the corridor runs, :side the side the
  body must end on (:from the lower coordinate, :to the higher). The body works from the stand cell, one step back from
  :at on that side. A body on the far side first walks to it (go-to child); no way there ends :wrong-side.

  Every cell of the section that is not a solid block is closed with a carried :with block (jobs.blocks.place child).
  Loose things in it (rails, cobwebs, torches, fences, ladders ...) are dug first (jobs.blocks.dig child, no fetch) and
  the drops picked up (torches come back). The rows go up from the bottom, the centre bottom cell last, so the body
  keeps its view. A container in the section is never dug (:blocked). Zones, claims and plans are the place rules'.

  Result {:status :done|:stopped :reason :sealed|:no-blocks|:refused|:unreachable|:wrong-side|:blocked|:bad-args
  :placed n :cell [x y z] :short n :why}. :done :sealed with :placed 0 means it was already shut. :no-blocks names
  the cells still open as :short. Events: strip.seal.done (info), strip.seal.stopped (warn).")

(a/defargs args
  {:at {:doc "the bottom centre cell of the section, [x y z]" :spec ::a/pos :default nil}
   :axis {:doc "the way the corridor runs" :spec #{:x :z} :default nil}
   :side {:doc "the side the body ends on: :from (lower coordinate) or :to" :spec #{:from :to} :default nil}
   :with {:doc "block items to close with, the first carried is used" :spec (a/coll-of a/item?)
          :default ["cobblestone" "cobbled_deepslate" "deepslate" "stone" "dirt"]}
   :width {:doc "cells across the corridor" :spec (a/int-in 1 9) :default 3}
   :height {:doc "cells up the corridor" :spec (a/int-in 1 9) :default 3}
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

(defn bad-args [{:keys [at axis side with]}]
  (cond
    (nil? at) "needs :at [x y z]"
    (nil? axis) "needs :axis :x or :z"
    (nil? side) "needs :side :from or :to"
    (empty? with) "needs a block in :with"))

(defn finish!
  [c status reason detail]
  (let [result (merge {:status status :reason reason :placed (:placed (ctx/mem c) 0)} detail)
        text (str "seal " (name reason) ": placed " (:placed result)
                  (when (:short result) (str ", " (:short result) " cells short"))
                  (when (:cell result) (str " at " (pr-str (:cell result)))))]
    (ctx/result! c result)
    (ctx/emit! c (if (= :done status) :strip.seal.done :strip.seal.stopped) (if (= :done status) :info :warn) (assoc result :text text))
    :done))

(defn stop! [c reason detail] (finish! c :stopped reason detail))

(defn on-side? [c [x _ z] axis side]
  (let [[fx _ fz] (feet-of c)
        k (if (= axis :x) fx fz)
        plane (if (= axis :x) x z)]
    (if (= side :from) (< k plane) (> k plane))))

(defn ^:async go-stand! [c at axis side]
  (let [stand-cell (stand at axis side)
        r (await (child/call-walk c :walk {:pos stand-cell :range 0 :escalate false}))]
    (cond
      (= :continue r) :continue
      (= stand-cell (feet-of c)) :again
      (on-side? c at axis side) (stop! c :unreachable {:cell stand-cell})
      :else (stop! c :wrong-side {:cell stand-cell}))))

(defn ^:async collect! [c]
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius 3}))]
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

(defn ^:async place! [c cell open]
  (let [{:keys [with ignore-zones?]} (:args c)
        item (b/chosen c with)]
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

(defn ^:async step! [c]
  (let [{:keys [at axis side width height]} (:args c)
        at (at-of at)
        p (:primitives c)
        named (mapv (fn [cell] [cell (kind (b/target-name p {:x (cell 0) :y (cell 1) :z (cell 2)} nil))])
                    (section at axis width height))
        todo (filterv #(not= :solid (second %)) named)
        [cell k] (first todo)]
    (cond
      (:collect (ctx/mem c)) (await (collect! c))
      (empty? todo) (finish! c :done :sealed {})
      (some #(= :container (second %)) todo) (stop! c :blocked {:cell (first (first (filter #(= :container (second %)) todo)))})
      (not= (stand at axis side) (feet-of c)) (await (go-stand! c at axis side))
      (= :loose k) (await (dig! c cell))
      :else (await (place! c cell (count todo))))))

(defn check [_] true)

(defn ^:async round [c]
  (if-let [why (bad-args (:args c))]
    (stop! c :bad-args {:why why})
    (let [n (atom 0)]
      (await (pace/steps! c #(if (< (swap! n inc) max-steps) (step! c) :continue))))))
