(ns dashboard.ui.detail-model
  "Everything the body popup's header, view and side panels show, as plain data."
  (:require [dashboard.ui.logic :as logic]
            [dashboard.ui.trouble :as trouble]))

(defn pos-text [pos]
  (if-not (map? pos)
    "-"
    (->> [(:x pos) (:y pos) (:z pos)]
         (map #(if (number? %) (js/Math.floor %) "?"))
         (interpose ", ")
         (apply str))))

(defn offline-text [at age-ms]
  (str "offline since " (logic/clock-ms-text at) (when (number? age-ms) (str " (" (logic/time-ago-text age-ms) ")"))))

(defn detail-model
  "The popup's model for a /api/state body at time now; action is the running action's name from the log, if any."
  [body now action]
  (if-not body
    {:online? false}
    (let [{:keys [name engine view up world at]} body
          card (trouble/card-model body now)
          at (or at (:at engine))
          age (:age-ms engine)]
      {:name name
       :status (:status card)
       :reason (:reason card)
       :severity (:severity card)
       :online? (boolean up)
       :job (:job card)
       :action action
       :pos-text (pos-text (or (:pos view) (:pos engine)))
       :dimension (:dimension view)
       :world world
       :last-seen (when age (logic/time-ago-text age))
       :thumb (:thumb card)
       :iframe-src (when up (str "/view?agent=" name "&embed=1&who=dashboard"))
       :offline-text (when-not up (offline-text at age))
       :hud (:hud view)
       :jobs (:jobs engine)
       :reflexes (:reflexes engine)})))
