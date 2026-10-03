(ns dashboard.ui.trouble
  "Who is in trouble, and what each body is doing: pure rules over a body of /api/state
  (engine signals, hud from the view)."
  (:require [clojure.string :as str]
            [dashboard.ui.logic :as logic]))

(def hurt-window-ms 30000)
(def died-window-ms 300000)
(def stuck-window-ms 300000)
(def low-health 6)
(def low-food 6)

(def status-order {:trouble 0 :working 1 :idle 2 :offline 3})

(defn ago [now t] (logic/time-ago-text (- now t)))

(defn within? [now t ms] (and (number? t) (<= (- now t) ms)))

(defn reason [severity text] {:severity severity :text text})

(defn manual [{:keys [takeover?]} _ _]
  (when takeover? (reason :danger "manual control")))

(defn died [{:keys [died-t]} _ now]
  (when (within? now died-t died-window-ms) (reason :danger (str "died " (ago now died-t)))))

(defn health [_ hud _]
  (let [h (:health hud)]
    (when (and (number? h) (<= h low-health)) (reason :danger (str "health " (js/Math.ceil h))))))

(defn hurt [{:keys [hurt-t]} _ now]
  (when (within? now hurt-t hurt-window-ms) (reason :warn (str "hurt " (ago now hurt-t)))))

(defn backoff [{:keys [backoffs]} _ _]
  (when (seq backoffs) (reason :warn (str "backoff: " (str/join ", " (sort (keys backoffs)))))))

(defn stuck [{:keys [stuck-open? stuck-t]} _ now]
  (when (or stuck-open? (within? now stuck-t stuck-window-ms)) (reason :warn "stuck")))

(defn food [_ hud _]
  (let [f (:food hud)]
    (when (and (number? f) (<= f low-food)) (reason :warn (str "food " (js/Math.ceil f))))))

;; most severe first; the first one is the card's one-line reason
(def rules [manual died health hurt backoff stuck food])

(defn reasons
  "The reasons a body is in trouble, [{:severity :danger|:warn :text}], most important first; none for an offline body."
  [{:keys [up engine view]} now]
  (if-not up
    []
    (let [signals (:signals engine)
          hud (:hud view)]
      (into [] (keep #(% signals hud now)) rules))))

(defn status [body now]
  (cond
    (not (:up body)) :offline
    (seq (reasons body now)) :trouble
    (get-in body [:engine :job]) :working
    :else :idle))

(defn sort-bodies [bodies now]
  (vec (sort-by (juxt #(status-order (status % now)) :name) bodies)))

(defn counts [bodies now]
  (merge {:working 0 :idle 0 :trouble 0 :offline 0}
         (frequencies (map #(status % now) bodies))))

(defn short-name [s]
  (str/replace (str s) #"(^|[ (])jobs\." "$1"))

(defn job-text
  "One line for the job: its spec label and round from engine.edn when known, else the name from events."
  [job edn-job]
  (cond
    (and (nil? job) (nil? edn-job)) nil
    edn-job (str (short-name (:label edn-job)) (when (:round edn-job) (str ", round " (:round edn-job))))
    :else (str/replace (short-name (:name job)) #"^\((.*)\)$" "$1")))

(defn offline-text [age-ms]
  (if-not (number? age-ms)
    "offline"
    (str "offline " (str/replace (logic/time-ago-text age-ms) " ago" ""))))

(defn thumb-src [name view]
  (when-let [v (:poseMtimeMs view)]
    (str "/api/thumb/" name ".png?v=" v)))

(defn card-model
  "Everything a body card shows, as plain data."
  [{:keys [name up engine view] :as body} now]
  (let [st (status body now)
        top (first (reasons body now))
        last-event (peek (vec (:recent engine)))
        edn-job (first (filter :current? (:jobs engine)))]
    {:name name
     :status st
     :reason (:text top)
     :severity (:severity top)
     :thumb (thumb-src name view)
     :health (get-in view [:hud :health])
     :food (get-in view [:hud :food])
     :job (job-text (:job engine) edn-job)
     :event (:text last-event)
     :event-age (when last-event (ago now (:t last-event)))
     :event-level (:level last-event)
     :offline (when-not up (offline-text (:age-ms engine)))}))
