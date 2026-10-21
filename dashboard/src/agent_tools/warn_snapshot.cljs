(ns agent-tools.warn-snapshot
  "A picture of what the body saw when `progress --wait` woke the agent for trouble (needs-you, a job giving up, no
  progress). Drawn here, in the tool process, from the view dump: the body only dumps while a watch marker is fresh, so
  the tool touches it, waits for the dump to catch up and renders as ./bin/snapshot does. Files are
  <workspace>/snapshots/warn-<reason>-<UTC time>.png, one per reason per rate window."
  (:require [agent-tools.snapshot :as snap]
            [agent-tools.world-data :as data]
            [clojure.string :as str]
            ["node:fs" :as fs]
            ["node:path" :as path]))

(def wakes #{:needs-you :gave-up :stall})
(def window-ms (* 5 60 1000))

(defn reason-key
  "The file-name key of a wake: gave-up names the job, so two jobs giving up each get a picture."
  [{:keys [wake job]}]
  (str (name wake) (when (and (= :gave-up wake) job) (str "-" job))))

(defn stamp-ms [name]
  (let [[_ y mo d h mi s ms] (re-find #"-(\d{4})-(\d\d)-(\d\d)T(\d\d)-(\d\d)-(\d\d)-(\d{3})Z\.png$" name)]
    (when y (js/Date.parse (str y "-" mo "-" d "T" h ":" mi ":" s "." ms "Z")))))

(defn recent?
  "Whether `dir` holds a picture of `key` taken less than window-ms before `now`."
  [dir key now]
  (let [prefix (str "warn-" key "-")]
    (boolean (when (.existsSync fs dir)
               (some #(and (str/starts-with? % prefix)
                           (re-matches (re-pattern (str "warn-" key "-" snap/stamp-pattern)) %)
                           (< (- now (or (stamp-ms %) 0)) window-ms))
                     (array-seq (.readdirSync fs dir)))))))

(defn ^:async capture!
  "A promise of the picture's workspace-relative path for `wake`, or nil (not a trouble wake, one taken lately, body
  not dumping, any failure). Never rejects: the wake line must come out."
  [{:keys [state world agent workspace render now sleep!] :or {now #(js/Date.now) sleep! #(js/Promise. (fn [ok] (js/setTimeout ok %)))}} wake]
  (try
    (when (and render (contains? wakes (:wake wake)))
      (let [ctx (data/context {:state state :world world})
            dir (.join path workspace "snapshots")
            key (reason-key wake)
            t (now)]
        (when-not (recent? dir key t)
          (snap/touch-watch! ctx agent t)
          (when (await (snap/wait-for-dump! ctx agent t sleep! now))
            (let [out (await (snap/execute! {:ctx ctx :body agent :workspace workspace :width 640 :height 360 :max-dist 64
                                             :prefix (str "warn-" key)}
                                            render (now)))]
              (when (:ok out) (:png out)))))))
    (catch :default _ nil)))
