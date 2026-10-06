(ns world-test.runner-test
  (:require [cljs.test :refer [deftest is async]]
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
