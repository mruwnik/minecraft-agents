(ns jobs.survival.plug-way
  "What the plug ways share (jobs.survival.wall-in, dig-pit, dig-pocket; picked between by jobs.survival.shut-in):
  the enclosed test, the goal, the quote and the stepping round."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.access :as access]
            [jobs.lib.checks :as checks]
            [jobs.lib.pace :as pace]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.spent :as spent]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.retreat-seal :as seal]))

(a/defargs settings
  {::roof-height {:default sh-sense/default-roof-height :doc "A solid block within this many blocks above counts as a roof (the ways, enter-room and the enclosed test; a call overrides it with :settings)." :spec (a/int-in 0 nil)}
   ::trespass-s {:default 1000 :doc "Seconds a way's quote adds when a cell it digs or fills is another's (zone, claim or plan): a last resort, after every way that keeps off." :spec (a/num-in 0 nil)}})

(defn roof-height [] (settings/get settings ::roof-height))

(defn zone-penalty
  "Seconds to add to a way's quote for the cells in {:dig :place :plug} that the zone rules refuse (a plug is placed from a
  cell the body is not at), 0 when none."
  [c {:keys [dig place plug]}]
  (let [in (access/rules-input c)]
    (if (or (some #(access/trespass-refusal in :dig %) dig)
            (some #(access/trespass-refusal in :place %) place)
            (some #(access/trespass-refusal (assoc in :feet nil) :place %) plug))
      (settings/get settings ::trespass-s)
      0)))

(defn enclosed?
  "Whether the world shows the body shut in: for :night safe from the night (sh-inside/safe-from-night?); else a roof within ::roof-height over no open cell round the feet, or, with the call's
  :room way, a roof over a room that is not breached, with no open door."
  [c]
  (let [p (:primitives c)
        roof (roof-height)]
    (boolean (if (= :night (:for (:args c)))
               (sh-inside/safe-from-night? p)
               (and (not (sh-sense/head-in-block? p))
                    (or (dig-cells/sealed-in? p :walls roof)
                        (and (ctx/way? c :room) (sh-sense/roofed? p roof) (not (sh/breached-home? p (ctx/view c))) (empty? (sh/open-room-doors p)))))))))

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
  "One step of the walls (retreat-seal/seal!): :done once enclosed, :again, :hit (a melee mob hit the body: run! yields), or nil when they cannot be placed."
  [c]
  (case (await (seal/seal! c))
    :sealed :done
    :again :again
    :hit :hit
    nil))

(defn stop!
  "Job memory: why the way ends nothing-possible ({:stop reason :text :needs} of retreat-pit/stop), for run!."
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
                            :again (if (:seal-mob-since (ctx/mem c)) :continue :again)
                            (let [{:keys [reason text needs]} (:stop (ctx/mem c))]
                                     (ctx/end c :nothing-possible (or reason :failed)
                                              (merge {:text (or text (:seal-refusal (ctx/mem c)) (str "the " (name way) " cannot enclose the body here")) :status (:seal-status (ctx/mem c))}
                                                     (when reason {:reason reason})
                                                     (when needs {:needs needs})
                                                     (result c)))))))))
