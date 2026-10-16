(ns jobs.survival.dig-in.pit
  "dig-in's pit mode: jobs.survival.plug-in with the pit way, and the stops it names."
  (:require [engine.ctx :as ctx]
            [jobs.lib.shelter :as sh]
            [jobs.survival.dig-in.site :as site]
            [jobs.survival.dig-in.walls :as walls]
            [jobs.survival.dig-in.mode :as mode]))

(defn stop-round
  "A pit that cannot be dug or went on no further (a stop: {:reason :text :needs}): its reason is the call's ending, a
  :dig-in-futile entry is left (a lack of blocks or of a tool retried once that changes), and the stop is said. :done."
  [c {:keys [reason text needs]}]
  (if (#{:no-blocks :no-tool} reason)
    (site/remember-material! c (cond-> {:pos (or (:roof (ctx/mem c)) (sh/feet (:primitives c)))} needs (assoc :needs needs)))
    (site/remember-failed-site! c reason))
  (ctx/emit! c :dig_in_failed :warn {:text text})
  :done)

(defn pit-stop-round
  "The pit was ruled out when the mode was chosen (retreat-pit/stop-of): its stop."
  [c]
  (let [{:keys [stop text needs]} (:pit-stop (ctx/mem c))]
    (stop-round c {:reason stop :text text :needs needs})))

(defn ^:async pit-round
  "One attempt of pit mode: jobs.survival.plug-in with the pit way and the shelter's blocks. A pit that stops names its reason."
  [c]
  (let [{:keys [roof-height on-lava hand-clear-ms]} (:args c)
        e (await (ctx/call c :pit 'jobs.survival.plug-in
                           {:ways #{:pit} :purpose :shelter :roof-height roof-height :on-lava on-lava :hand-clear-ms hand-clear-ms :use {:blocks (mode/blocks c)}}))]
    (if (= :continue e)
      :yield
      (do (walls/note-placed! c (:data e))
          (if (= :done (:kind e))
            (do (ctx/update-mem! c merge (select-keys (:data e) [:roof :start]))
                :done)
            (stop-round c (merge {:reason :no-floor :text (str "cannot dig the pit: " (:text (:data e)))}
                                 (get-in e [:data :stops :pit])
                                 (when-let [why (get-in e [:data :why :pit])] {:text why}))))))))
