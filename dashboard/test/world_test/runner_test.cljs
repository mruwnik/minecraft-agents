(ns world-test.runner-test
  (:require [cljs.test :refer [deftest is async]]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [world-test.runner :as r]))

(deftest the-body-launch-argv-caps-new-space
  (let [argv (vec (r/body-argv {:body "B" :world "w"} "/tmp/s.edn"))]
    (is (some #{"--max-semi-space-size=4"} argv))
    (is (< (.indexOf argv "--max-semi-space-size=4") (.indexOf argv "out/body.cjs")))
    (is (= ["--agent" "B" "--world" "w" "--scenario" "/tmp/s.edn" "--fresh"]
           (vec (drop (inc (.indexOf argv "out/body.cjs")) argv))))))

(deftest ensure-at-start-retries-the-tp-then-fails-with-a-message
  (let [origin [20000 150 20000]
        c {:body {:at [16.5 0 16.5]}}
        at "x has the following entity data: [20016.5d, 150.0d, 20016.5d]"
        spawn "x has the following entity data: [9.5d, 68.0d, 0.5d]"
        run (fn [replies]
              (let [sent (atom []) left (atom replies)]
                (.then (r/ensure-at-start!
                        {:send (fn [cmds] (swap! sent into cmds)
                                 (js/Promise.resolve (mapv (fn [_] (let [x (first @left)] (swap! left rest) x)) (filter #(re-find #"^data get" %) cmds))))
                         :sleep (fn [_] (js/Promise.resolve nil))}
                        origin "B" c)
                       (fn [res] {:res res :sent @sent}))))]
    (async done
      (-> (js/Promise.resolve)
          (.then #(run [at]))
          (.then (fn [{:keys [res sent]}]
                   (is (nil? res))
                   (is (= 0 (count (filter #(re-find #"^tp " %) sent))) "already there: no extra tp")))
          (.then #(run [spawn at]))
          (.then (fn [{:keys [res sent]}]
                   (is (nil? res))
                   (is (= 1 (count (filter #(re-find #"^tp " %) sent))) "one retry tp")))
          (.then #(run [spawn spawn spawn spawn spawn]))
          (.then (fn [{:keys [res]}]
                   (is (re-find #"not at its start" res))
                   (done)))))))

(deftest a-register-case-counts-events-from-before-the-register-was-put
  (let [pre {:offset 10 :from-ms 1000} post {:offset 90 :from-ms 9000}]
    (is (= pre (r/watch-window true pre post)) "a trigger firing inside the settle is counted")
    (is (= post (r/watch-window false pre post)) "no register: the settle still hides leftover events")))

(deftest stop-on-fail-is-an-option-and-ends-the-batch-after-a-non-pass
  (is (true? (:stop-on-fail (r/parse-args #js ["--stop-on-fail"]))))
  (is (nil? (:stop-on-fail (r/parse-args #js []))))
  (is (r/stop-batch? {:stop-on-fail true} [{:status :pass} {:status :fail}]))
  (is (r/stop-batch? {:stop-on-fail true} [{:status :error}]))
  (is (not (r/stop-batch? {:stop-on-fail true} [{:status :pass} {:status :skipped}])))
  (is (not (r/stop-batch? {} [{:status :fail}]))))

(deftest the-event-reader-follows-a-log-rotation
  (let [dir (fs/mkdtempSync (path/join (os/tmpdir) "wt-rot-"))
        file (path/join dir "events.edn")
        seg (str file ".1")
        ev (fn [& ks] (apply str (map #(str "{:k " % "}\n") ks)))
        ks (fn [evs] (mapv :k evs))]
    (fs/writeFileSync file (ev 1 2))
    (let [cur (r/log-cursor file)]
      (fs/appendFileSync file (ev 3))
      (is (= [3] (ks (r/read-events-from file cur))) "no rotation: the tail")
      (fs/renameSync file seg)
      (fs/writeFileSync file (ev 4 5 6 7 8 9 10 11))
      (is (= [3 4 5 6 7 8 9 10 11] (ks (r/read-events-from file cur))) "rotated, new file longer than the offset: tail of .1 then the new file")
      (fs/renameSync seg (str file ".2"))
      (fs/renameSync file seg)
      (fs/writeFileSync file (ev 12))
      (is (= [3 4 5 6 7 8 9 10 11 12] (ks (r/read-events-from file cur))) "rotated twice"))
    (fs/rmSync dir #js {:recursive true :force true})))
