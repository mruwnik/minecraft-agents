(ns jobs.village.roll-gap
  "The slab over a workstation cell (a lectern under a top slab keeps the villager in and the body out) lifted for a drop that
  slid into the villager's cell, out of pickup reach from both sides, and put back."
  (:require [engine.ctx :as ctx]
            [jobs.lib.block-sets :as bs]
            [jobs.lib.placement :as placement]
            [jobs.lib.util :as u]))

(defn above [pos] (update pos :y inc))

(defn ^:async run!
  "A dig or place child in its own slot: ::ok when it is done, :continue, or the stop."
  [c slot job cargs]
  (let [r (await (ctx/call c slot job (cond-> cargs (:ignore-zones? (:args c)) (assoc :ignore-zones? true))))]
    (cond (= :continue r) :continue
          (= :done (:kind r)) ::ok
          :else r)))

(defn ^:async lift!
  "Dig the slab above pos (once per call) so the body can stand in the cell and take a drop from anywhere in the villager's
  cell; the slab's name and type are booked in mem :slab first and the drop search starts afresh. :again, :continue, a stop,
  or nil when there is no slab or it was lifted already."
  [c pos n]
  (let [m (ctx/mem c)
        b (when-not (or (:slab m) (:lift-refused m)) (u/seen-block (:primitives c) (above pos)))]
    (when (and b (re-find #"_slab$" (.-name b)))
      (ctx/update-mem! c assoc :slab {:name (.-name b) :type (some-> b .-properties (aget "type"))} :reach-drops 0)
      (ctx/emit! c :roll.slab-lifted :info {:pos (above pos) :slab (.-name b) :text (str "lifting the slab over the workstation cell at " (:x pos) "," (:y pos) "," (:z pos))})
      (let [r (await (run! c (keyword (str "lift-slab-" n)) 'jobs.blocks.dig {:pos (above pos) :collect true}))]
        (cond (= ::ok r) :again
              (= :continue r) :continue
              :else (do (ctx/update-mem! c #(-> % (dissoc :slab) (assoc :lift-refused (or (:reason r) :refused))))
                        (ctx/emit! c :roll.slab-lift-refused :warn {:pos (above pos) :why (:reason r)
                                                                    :text (str "the slab over " (:x pos) "," (:y pos) "," (:z pos) " cannot be lifted (" (some-> (:reason r) name) "): the drop cannot be fetched from the villager's cell")})
                        :again))))))

(defn open-cell
  "Ends the call with the cell named when the slab cannot be put back: the villager could walk out."
  [c pos why]
  (ctx/emit! c :roll.slab-not-restored :warn {:pos (above pos) :why why
                                              :text (str "CELL OPEN: the slab over " (:x pos) "," (:y pos) "," (:z pos) " is not back (" (name why) ")")})
  (ctx/end c :nothing-possible :cell-open {:cell (above pos) :why why}))

(defn ^:async restore!
  "Place the lifted slab back in its cell with its type (from the stand the body has left the workstation cell for), up to three
  tries. :again when it is placed or the cell is filled, :continue, or the ending :cell-open when the slab is not carried or
  three tries failed (the cell stays open, loudly)."
  [c pos n]
  (let [{:keys [name type]} (:slab (ctx/mem c))
        p (:primitives c)
        up (above pos)
        here (u/seen-name p up)
        tries (:slab-tries (ctx/mem c) 0)]
    (cond
      (and here (not (bs/air here))) (do (ctx/update-mem! c dissoc :slab :slab-tries) :again)
      (zero? (u/carried-count p name)) (open-cell c pos :slab-not-carried)
      (>= tries 3) (open-cell c pos :slab-place-failed)
      :else
      (let [click (:click (placement/click {:block name :type type} [(:x up) (:y up) (:z up)] (placement/eye (u/self-pos c)) (fn [cell] (some-> (u/seen-block p (zipmap [:x :y :z] cell)) .-name (->> (hash-map :name))))))
            r (if (and type (nil? click))
                :no-click
                (await (run! c (keyword (str "restore-slab-" n "-" tries))
                           'jobs.blocks.place
                           (cond-> {:pos up :item name :through false} click (assoc :click click)))))]
        (cond
          (= ::ok r) (do (ctx/emit! c :roll.slab-restored :info {:pos up :slab name :type type
                                                                 :text (str "slab back over the workstation cell at " (:x pos) "," (:y pos) "," (:z pos))})
                         (ctx/update-mem! c dissoc :slab :slab-tries)
                         :again)
          (= :continue r) :continue
          :else (do (ctx/update-mem! c update :slab-tries (fnil inc 0)) :again))))))
