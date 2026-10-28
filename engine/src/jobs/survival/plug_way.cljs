(ns jobs.survival.plug-way
  "What the plug ways share (jobs.survival.wall-in, dig-pit, dig-pocket; picked between by jobs.survival.shut-in):
  the enclosed test, the goal, the quote and the stepping round."
  (:require [jobs.lib.rooms :as room-lib]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.checks :as checks]
            [jobs.lib.pace :as pace]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spent :as spent]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.lib.util :as u]
            [jobs.survival.wall-placer :as wall-placer]))

(a/defargs settings
  {::place-s {:default 0.72 :doc "Seconds a placement takes, for a way's quote: the mean gap between two placements of one walls attempt of a flight, n=62 gaps in the trial bodies' logs on 2026-10-28. Recount: node tools/event-counts.mjs wall-places (:gap)." :spec (a/num-in 0 nil)}
   ::night-place-s {:default 1 :doc "Seconds a placement (aim, place, settle) takes for a night's shelter; the dig back at dawn is daylight work, so only placing is priced." :spec (a/num-in 0 nil)}
   ::trespass-s {:default 1000 :doc "Seconds a way's quote adds when a cell it digs or fills is another's (zone, claim or plan): a last resort, after every way that keeps off." :spec (a/num-in 0 nil)}})


(defn refused?
  "Whether the zone rules refuse a cell in {:dig :place :plug} (a plug is placed from a cell the body is not at)."
  [c {:keys [dig place plug]}]
  (let [in (access/rules-input c)]
    (boolean (or (some #(access/trespass-refusal in :dig %) dig)
                 (some #(access/trespass-refusal in :place %) place)
                 (some #(access/trespass-refusal (assoc in :feet nil) :place %) plug)))))

(defn place-s
  "Seconds a placement is priced at in a way's quote: only the placing for a night's shelter."
  [c]
  (settings/get settings (if (= :night (:for (:args c))) ::night-place-s ::place-s)))


(defn zone-penalty
  "Seconds to add to a way's quote for the cells in {:dig :place :plug} that the zone rules refuse, 0 when none."
  [c cells]
  (if (refused? c cells) (settings/get settings ::trespass-s) 0))

(defn rim-open?
  "Whether, over some cell of the room (cells [x y z]), below the first solid or unseen block of its column, a cell beside the column is
  open over a floor a mob stands on or one not yet seen: a mob on that rim drops in. Unseen counts as open."
  [p cells]
  (let [open? (fn [cell] (not (sh-sense/solid-at? p cell)))
        floor? (fn [cell] (or (nil? (u/seen-block p cell)) (dig-cells/stand-on? p cell)))]
    (boolean
     (some (fn [[x y z]]
             (->> (range (+ y 2) (+ y 2 (sh-sense/room-roof-height)))
                  (take-while #(let [cell {:x x :y % :z z}] (and (some? (u/seen-block p cell)) (open? cell))))
                  (some (fn [h] (some (fn [[dx dz]] (let [n {:x (+ x dx) :y h :z (+ z dz)}]
                                                      (and (open? n) (floor? (update n :y dec)))))
                                      [[1 0] [-1 0] [0 1] [0 -1]])))))
           cells))))

(defn pocket-open?
  "Whether the body stands in a pit (a one-cell room, or a room roofed over every cell, the roof far over, that a mob on the rim drops into: rim-open?) with a fill cell still open: a mob on the rim or a spider
  reaches it though no walker's flood does."
  [p]
  (let [feet (sh-sense/feet p)
        cells (:cells (reach-room/room p feet))]
    (and (boolean (seq (dig-cells/open-cells p feet)))
         (or (= 1 (count cells)) (and (every? (fn [[x y z]] (sh-sense/roofed? p (sh-sense/room-roof-height) {:x x :y y :z z})) cells) (rim-open? p cells))))))

(defn enclosed?
  "Whether the world shows the body shut in: for :night safe from the night (sh-inside/safe-from-night?) and, in a pit, no open fill cell (pocket-open?); else a roof within ::roof-height over no open cell round the feet, or, with the call's
  :room way, a roof over a room that is not breached, with no open door."
  [c]
  (let [p (:primitives c)
        roof (sh-sense/roof-height)]
    (boolean (if (= :night (:for (:args c)))
               (and (sh-inside/safe-from-night? p) (not (pocket-open? p)))
               (and (not (sh-sense/head-in-block? p))
                    (or (dig-cells/sealed-in? p :walls roof)
                        (and (ctx/way? c :room) (sh-sense/roofed? p roof) (not (sh/breached-home? p (ctx/view c))) (empty? (sh/open-room-doors p)) (nil? (room-lib/gap c)))))))))

(defn placed
  "The spent summary and the blocks placed so far ({:cell [x y z] :item name}), for an ending's data."
  [c]
  (assoc (spent/summary (spent/log c)) :placed (:placed-cells (ctx/mem c) [])))

(defn result
  "What a way's ending carries beyond the position: the blocks placed (placed) and, for a pit, its :roof and :start (the cell dug from)."
  [c]
  (merge (placed c) (when-let [pit (:pit (ctx/mem c))] {:roof (:roof pit) :start (:anchor pit)})))

(defn done?
  "The goal of a way: enclosed. Its result is the position and way with what extra (default the way's own result) adds."
  ([c way] (done? c way (result c)))
  ([c way extra]
  (let [done (enclosed? c)]
    (checks/goal done [:enclosed "the body is enclosed"] [:open "the body is not enclosed"]
                 {:units :enclosures :done (if done 1 0) :left (if done 0 1)
                  :result (merge {:pos (sh-sense/feet (:primitives c)) :way way} extra)}))))

(defn quote-of
  "Time of a way: 0 once enclosed, else seconds (##Inf when it cannot be done here, which shut-in drops)."
  [c seconds job-time-s]
  (let [mean (if (enclosed? c) 0 seconds)]
    {:uses {:job-time [0 mean (if (infinite? mean) mean (max (* 3 mean) job-time-s))]} :done [0 1 1]}))

(defn ^:async walls-step!
  "One step of the walls (wall-placer/seal!): :done once enclosed, :again, :hit (a melee mob hit the body: run! yields), or nil when they cannot be placed."
  [c]
  (case (await (wall-placer/seal! c))
    :sealed :done
    :again :again
    :hit :hit
    nil))

(defn stop!
  "Job memory: why the way ends nothing-possible ({:stop reason :text :needs} of pit-cells/stop), for run!."
  [c {:keys [stop text needs]}]
  (ctx/update-mem! c assoc :stop (cond-> {:reason stop :text text} needs (assoc :needs needs)))
  nil)

(defn ^:async run!
  "Steps of the way (step!: :done, :again, or nil when it fails) with a timer between, to the call's end. A place refused for a mob
  yields (:continue) so the world moves and the mob may clear."
  [c way step!]
  (await (pace/steps! c (fn ^:async way-step []
                          (case (await (step! c))
                            :done (ctx/end c :done nil (merge {:pos (sh-sense/feet (:primitives c)) :way way} (result c)))
                            :hit :continue
                            :again (if (:seal-wait (ctx/mem c)) :continue :again)
                            (let [{:keys [reason text needs]} (:stop (ctx/mem c))]
                                     (ctx/end c :nothing-possible (or reason :step-failed)
                                              (merge {:text (or text (:seal-refusal (ctx/mem c)) (str "the " (name way) " cannot enclose the body here")) :status (:seal-status (ctx/mem c))}
                                                     (when reason {:reason reason})
                                                     (when needs {:needs needs})
                                                     (result c)))))))))
