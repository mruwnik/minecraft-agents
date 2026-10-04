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

(def status-order {:manual 0 :trouble 1 :working 2 :idle 3 :offline 4})

(defn ago [now t] (logic/time-ago-text (- now t)))

(defn within? [now t ms] (and (number? t) (<= (- now t) ms)))

(defn reason [severity text] {:severity severity :text text})

(defn died [{:keys [died-t]} _ now]
  (when (within? now died-t died-window-ms) (reason :danger (str "died " (ago now died-t)))))

(defn health [_ hud _]
  (let [h (:health hud)]
    (when (and (number? h) (<= h low-health)) (reason :danger (str "health " (js/Math.ceil h))))))

(defn hurt [{:keys [hurt-t]} _ now]
  (when (within? now hurt-t hurt-window-ms) (reason :warn (str "hurt " (ago now hurt-t)))))

(defn backoff [{:keys [backoffs]} _ _]
  (when (seq backoffs) (reason :warn (str "backoff: " (str/join ", " (sort-by str (keys backoffs)))))))

(defn stuck [{:keys [stuck-open? stuck-t]} _ now]
  (when (or stuck-open? (within? now stuck-t stuck-window-ms)) (reason :warn "stuck")))

(defn food [_ hud _]
  (let [f (:food hud)]
    (when (and (number? f) (<= f low-food)) (reason :warn (str "food " (js/Math.ceil f))))))

;; most severe first; the first one is the card's one-line reason
(def rules [died health hurt backoff stuck food])

(defn reasons
  "The reasons a body is in trouble, [{:severity :danger|:warn :text}], most important first; none for an offline body."
  [{:keys [up engine view]} now]
  (if-not up
    []
    (let [signals (:signals engine)
          hud (:hud view)]
      (into [] (keep #(% signals hud now)) rules))))

(defn manual?
  "Is an online body under manual control (takeover_started without a later takeover_ended)? Not a trouble."
  [{:keys [up engine]}]
  (boolean (and up (get-in engine [:signals :takeover?]))))

(defn since-text
  "HH:MM (local time) of an epoch-ms time, nil for anything else."
  [t]
  (when (number? t)
    (let [d (js/Date. t)
          two #(.padStart (str %) 2 "0")]
      (str (two (.getHours d)) ":" (two (.getMinutes d))))))

(defn manual-text
  "\"driven by <who> since HH:MM\", nil when the body is not under manual control."
  [body]
  (when (manual? body)
    (let [{:keys [takeover-who takeover-t]} (get-in body [:engine :signals])]
      (str "driven by " (or takeover-who "someone") (when-let [s (since-text takeover-t)] (str " since " s))))))

(defn status [body now]
  (cond
    (not (:up body)) :offline
    (manual? body) :manual
    (seq (reasons body now)) :trouble
    (get-in body [:engine :job]) :working
    :else :idle))

(defn sort-bodies [bodies now]
  (vec (sort-by (juxt #(status-order (status % now)) :name) bodies)))

(defn counts [bodies now]
  (merge {:manual 0 :working 0 :idle 0 :trouble 0 :offline 0}
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

(defn thumb-src [world name view]
  (when-let [v (and world (:poseMtimeMs view))]
    (str "/api/thumb/" world "/" name ".png?v=" v)))

(def stale-thumb-ms 10000)

(defn thumb-age-mark
  "\"12 s old\" / \"3 min old\" for an online body whose view is older than 10 s, else nil (offline bodies keep their badge)."
  [up mtime now]
  (when (and up (number? mtime) (> (- now mtime) stale-thumb-ms))
    (let [s (quot (- now mtime) 1000)]
      (if (< s 60) (str s " s old") (str (quot s 60) " min old")))))

(defn mine?
  "Does this page (its own `who`) hold the body? Only then does the card get the red edge."
  [body who]
  (boolean (and who (manual? body) (= who (get-in body [:engine :signals :takeover-who])))))

(defn card-model
  "Everything a body card shows, as plain data. `who`: this page load's name, to tell its own takeover from another's."
  ([body now] (card-model body now nil))
  ([{:keys [name world up engine view] :as body} now who]
  (let [st (status body now)
        top (first (reasons body now))
        last-event (peek (vec (:recent engine)))
        edn-job (first (filter :current? (:jobs engine)))]
    {:name name
     :world world
     :status st
     :reason (:text top)
     :manual (manual-text body)
     :mine? (mine? body who)
     :severity (:severity top)
     :thumb (thumb-src world name view)
     :pose-mtime (:poseMtimeMs view)
     :thumb-age (thumb-age-mark up (:poseMtimeMs view) now)
     :health (get-in view [:hud :health])
     :food (get-in view [:hud :food])
     :job (job-text (:job engine) edn-job)
     :event (:text last-event)
     :event-age (when last-event (ago now (:t last-event)))
     :event-attention (keyword (clojure.core/name (or (:attention last-event) "none")))
     :offline (when-not up (offline-text (:age-ms engine)))})))
