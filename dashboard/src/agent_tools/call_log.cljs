(ns agent-tools.call-log
  "One EDN line per bin/ tool event in <worlds>/<world>/agents/<body>/logs/tool-calls.edn: a :start line when the
  call begins (so a crash or kill leaves an open call), an :end line with exit and, for waits, the :wake reason."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]
            ["node:fs" :as fs]
            ["node:path" :as path]))

(defn log-file [{:keys [worlds world body]}]
  (.join path worlds world "agents" body "logs" "tool-calls.edn"))

(defn append!
  "Never throws: a log that cannot be written must not fail the tool. A body folder that does not exist is not made."
  [file entry]
  (try
    (let [logs (.dirname path file)]
      (when (.existsSync fs (.dirname path logs))
        (.mkdirSync fs logs #js {:recursive true})
        (.appendFileSync fs file (str (pr-str entry) "\n"))))
    (catch :default _ nil)))

(defn observer-of [argv]
  (some (fn [[a b]] (cond (= "--observer" a) b
                          (str/starts-with? a "--observer=") (subs a 11)))
        (partition 2 1 [nil] (take-while #(not= "--" %) argv))))

(defn wake-of [text]
  (some-> (re-seq #":wake (:[A-Za-z0-9_-]+)" (or text "")) last second))

(defn begin!
  "Log the :start line; the returned call map goes to finish!."
  [ctx tool argv]
  (let [now (js/Date.now)
        call {:file (log-file ctx) :id (str now "-" (.-pid js/process)) :start now}]
    (append! (:file call) (cond-> {:t :start :id (:id call) :ms now :pid (.-pid js/process)
                                   :tool tool :argv (vec argv)}
                            (observer-of argv) (assoc :observer (observer-of argv))))
    call))

(defn finish! [call exit text]
  (let [now (js/Date.now)]
    (append! (:file call) (cond-> {:t :end :id (:id call) :ms now :dur-ms (- now (:start call))
                                   :exit exit :ok (= 0 exit)}
                            (wake-of text) (assoc :wake (keyword (subs (wake-of text) 1)))))))

(defn begin-js
  "JS entry: nil when the context cannot be read."
  [context-file tool argv]
  (try (clj->js (begin! (reader/read-string (.readFileSync fs context-file "utf8")) tool (vec argv)))
       (catch :default _ nil)))

(defn finish-js [call exit text]
  (when call (finish! (js->clj call :keywordize-keys true) exit text)))
