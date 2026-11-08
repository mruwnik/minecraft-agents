(ns jobs.debug.ready-probe-test
  "jobs.debug.ready-probe: the world fixtures' stand-in for a job that binds a readiness checker."
  (:require [cljs.test :refer [deftest are is]]
            [engine.ready-test :as rt]
            [engine.registry :as registry]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [jobs.debug.ready-probe :as probe]
            [jobs.lib.reach.room :as reach-room]))

(defn kind [args world]
  (first ((:safe? (get registry/jobs 'jobs.debug.ready-probe)) (assoc (rt/ctx-of world) :args args))))

(deftest safe-binds-the-checker-its-args-choose
  (rt/lit 0 15
    #(are [args world expected] (= expected (kind args world))
       {} rt/night-hut :ok
       {:at [15 64 0]} rt/night-hut :later
       {:at [5 64 0]} rt/night-hut :ok
       {:lit? true} rt/open-night :later
       {:lit? true} rt/night-hut :ok)))

(deftest the-report-carries-the-status-of-the-checker-run-before-it-without-running-it-again
  (let [c (assoc (rt/ctx-of rt/night-hut) :args {})
        runs (atom 0)
        checker probe/checker]
    (with-redefs [probe/checker (fn [c] (let [f (checker c)] (fn [c] (swap! runs inc) (f c))))]
      (let [s (probe/safe? c)]
        (is (= s (:status (probe/report c))))
        (is (= 1 @runs))))))

(deftest a-night-check-floods-the-room-round-the-body-once-while-its-memory-holds
  (let [c (assoc (rt/remembering-ctx {:time st/night :floor [-40 -40 40 40] :self {:pos {:x 6.5 :y 64 :z 0.5}}
                                      :blocks (tu/box 10 64 -4 10 68 4 "stone")})
                 :args {:at [11 64 0] :stay-s 3600})
        floods (atom [])
        flood reach-room/room-flood
        note! (fn [p args] (swap! floods conj (conj args (reach-room/steady-key (aget p "perception")))))]
    (with-redefs [reach-room/room-flood (let [f (fn [p k x y z & more] (when (= 3 (count more)) (note! p (into [x y z] more))) (apply flood p k x y z more))]
                                          (fn ([a b c d e] (f a b c d e)) ([a b c d e g] (f a b c d e g)) ([a b c d e g h] (f a b c d e g h)) ([a b c d e g h i] (f a b c d e g h i))))]
      (dotimes [_ 2] (is (= :later (first (probe/safe? (assoc c :id (str (random-uuid)))))))))
    (is (seq @floods))
    (is (= (distinct (map #(vector (take 3 %) (last %)) @floods)) (map #(vector (take 3 %) (last %)) @floods))
        "each start cell is flooded once per memory epoch, by every reader of the room (look-edge/pending, boundary, room)")))
