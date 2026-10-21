(ns agent-tools.warn-snapshot-test
  (:require [cljs.test :refer [deftest is async]]
            [clojure.string :as str]
            [agent-tools.progress :as progress]
            [agent-tools.snapshot :as snap]
            [agent-tools.snapshot-test :as st]
            [agent-tools.warn-snapshot :as warn]
            ["node:fs" :as fs]
            ["node:path" :as path]))

(def t0 (js/Date.now))

(defn setup
  "{:opts capture options :calls render calls :clock fake clock atom :f fixture}. `dump?`: the body answers the watch
  marker by rewriting its pose when the tool sleeps."
  [dump?]
  (let [{:keys [worlds view workspace] :as f} (st/world-fixture)
        r (progress/request-for ["B" "--world" "w" "--worlds" worlds "--workspace" workspace])
        clock (atom t0)
        calls (atom [])
        pose-file (.join path view "pose.json")
        sleep! (fn [ms]
                 (swap! clock + ms)
                 (when dump?
                   (.writeFileSync fs pose-file (js/JSON.stringify (clj->js (st/pose {:t @clock}))))
                   (.utimesSync fs pose-file (/ @clock 1000) (/ @clock 1000)))
                 (js/Promise.resolve nil))]
    {:f f :clock clock :calls calls
     :opts (assoc (:base r) :workspace (:workspace r) :render (st/fake-render calls) :now #(deref clock) :sleep! sleep!)}))

(deftest a-trouble-wake-touches-the-watch-marker-and-leaves-one-picture-per-window
  (async done
    (let [{:keys [f opts calls clock]} (setup true)
          wake {:wake :needs-you :text "body idle: x"}]
      (-> (warn/capture! opts wake)
          (.then (fn [png]
                   (is (re-matches #"snapshots/warn-needs-you-.*Z\.png" png))
                   (is (.existsSync fs (.join path (:workspace f) png)))
                   (is (.existsSync fs (.join path (:worlds f) "w" "agents" "B" "view" "watching")))
                   (is (= 1 (count @calls)))
                   (swap! clock + 60000)
                   (warn/capture! opts wake)))
          (.then (fn [again]
                   (is (nil? again) "a second picture of the same reason inside the window is not drawn")
                   (is (= 1 (count @calls)))
                   (swap! clock + warn/window-ms)
                   (warn/capture! opts wake)))
          (.then (fn [later] (is (some? later) "after the window another is drawn")))
          (.finally done)))))

(deftest only-trouble-wakes-draw
  (async done
    (let [{:keys [opts calls f]} (setup true)]
      (-> (js/Promise.all (clj->js (map #(warn/capture! opts {:wake %}) [:goal-ended :died :chat :timeout])))
          (.then (fn [results]
                   (is (every? nil? (array-seq results)))
                   (is (empty? @calls))
                   (is (not (.existsSync fs (.join path (:worlds f) "w" "agents" "B" "view" "watching"))))))
          (.finally done)))))

(deftest a-body-that-never-dumps-gets-no-picture-and-no-error
  (async done
    (let [{:keys [opts calls]} (setup false)]
      (-> (warn/capture! opts {:wake :gave-up :job "j3"})
          (.then (fn [png]
                   (is (nil? png))
                   (is (empty? @calls))))
          (.finally done)))))

(deftest reasons-are-keyed-and-pruned-apart-from-plain-snapshots
  (is (= "gave-up-j3" (warn/reason-key {:wake :gave-up :job "j3"})))
  (is (= "stall" (warn/reason-key {:wake :stall :job "j3"})))
  (let [warns (mapv #(snap/file-name % "warn-gave-up-j3") (range 1000 26000 1000))
        snaps (mapv snap/file-name (range 1000 5000 1000))]
    (is (= (take 5 warns) (snap/prune (shuffle (into warns snaps)) 20 "warn")))
    (is (empty? (snap/prune (into warns snaps) 30 "snap")) "plain snapshots keep their own count")))

(deftest the-wake-line-names-the-picture
  (is (str/includes? (progress/wake-text {:wake :needs-you :text "t" :snapshot "snapshots/warn-x.png"}) "SNAPSHOT snapshots/warn-x.png"))
  (is (not (str/includes? (progress/wake-text {:wake :needs-you :text "t"}) "SNAPSHOT"))))
