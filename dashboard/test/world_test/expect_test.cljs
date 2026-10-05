(ns world-test.expect-test
  (:require [cljs.test :refer [deftest is are]]
            [world-test.expect :as x]))

(def fired {:time-ms 1500 :source :reflex :kind :fired :context {:job-id "j3" :reflex-id :hostile-near}
            :data {:pos {:x 20016.5 :y 150 :z 20016.5}} :message "hostile-near → respond"})
(def herd-done {:time-ms 9000 :source :job :kind :herd.done :context {:job-id "j7" :chain ["j7"]}
                :data {:reason :brought :inside 1 :gate {:x 1 :y 2 :z 3}} :message "herd done"})
(def dig-child {:time-ms 4000 :source :job :kind :blocks.dig.done :context {:job-id "j9/dig" :chain ["j9" "j9/dig"]}
                :data {:reason :dug :pos {:x 5 :y 150 :z 6}}})

(deftest patterns-match-partially-and-by-operator
  (are [pattern value] (x/matches? pattern value)
    {:kind :fired} fired
    {:context {:reflex-id :hostile-near}} fired
    {:kind #{:fired :ended}} fired
    {:data {:inside [:>= 1]}} herd-done
    {:data {:pos [:near [20016 150 20016] 1]}} fired
    {:data {:gate [:near [1 2 3] 0]}} herd-done
    {:message [:contains "herd"]} herd-done
    {:data {:reason [:not :failed]}} herd-done
    {:data {:gate [:any]}} herd-done
    [1 2 3] [1 2 3])
  (are [pattern value] (not (x/matches? pattern value))
    {:kind :ended} fired
    {:context {:reflex-id :pen-gate}} fired
    {:data {:inside [:> 1]}} herd-done
    {:data {:pos [:near [20020 150 20016] 1]}} fired
    {:data {:missing [:any]}} herd-done
    {:data {:inside [:>= 1]}} {:data {:inside "1"}}
    [1 2] [1 2 3]))

(def opts {:t0-ms 1000 :job-ids #{"j7" "j9"}})

(deftest an-event-passes-when-it-comes-in-time-and-fails-after-the-deadline
  (let [e {:event {:source :reflex :kind :fired} :within-s 9}]
    (is (= :pass (:status (x/judge e [fired] (assoc opts :now-ms 2000)))))
    (is (= 0.5 (:at-s (x/judge e [fired] (assoc opts :now-ms 2000)))))
    (is (= :pending (:status (x/judge e [] (assoc opts :now-ms 5000)))))
    (is (= :fail (:status (x/judge e [] (assoc opts :now-ms 10001)))))
    (is (= :fail (:status (x/judge {:event {:kind :fired} :within-s 0.2} [fired] (assoc opts :now-ms 3000))))
        "an event after the deadline does not count")))

(deftest a-no-event-fails-at-once-and-passes-only-after-its-window
  (let [e {:no-event {:kind :fired} :for-s 10}]
    (is (= :fail (:status (x/judge e [fired] (assoc opts :now-ms 2000)))))
    (is (= :pending (:status (x/judge e [] (assoc opts :now-ms 2000)))))
    (is (= :pass (:status (x/judge e [] (assoc opts :now-ms 11001)))))
    (is (= :pass (:status (x/judge {:no-event {:kind :fired} :for-s 0.1} [fired] (assoc opts :now-ms 3000))))
        "an event after the window does not count")))

(deftest a-no-event-with-until-ends-its-window-at-the-first-until-event
  (let [e {:no-event {:kind :fired} :for-s 100 :until {:kind :herd.done}}]
    (is (= :pass (:status (x/judge e [herd-done] (assoc opts :now-ms 9500)))) "herd done at 9 s and nothing fired")
    (is (= :pass (:status (x/judge e [herd-done (assoc fired :time-ms 9500)] (assoc opts :now-ms 9600))))
        "a fire after the until event is outside the window")
    (is (= :fail (:status (x/judge e [fired herd-done] (assoc opts :now-ms 9500)))))
    (is (= :pending (:status (x/judge e [] (assoc opts :now-ms 9500)))))
    (is (= :pass (:status (x/judge e [] (assoc opts :now-ms 101001)))) "without the until event the window is :for-s")))

(deftest of-job-limits-matches-to-the-submitted-jobs-and-their-children
  (is (= :pass (:status (x/judge {:event {:kind :blocks.dig.done} :within-s 60 :of-job true} [dig-child] (assoc opts :now-ms 5000)))))
  (is (= :pending (:status (x/judge {:event {:kind :blocks.dig.done} :within-s 60 :of-job true} [dig-child]
                                    (assoc opts :now-ms 5000 :job-ids #{"j1"}))))))

(deftest a-run-is-decided-when-nothing-is-pending-and-passes-when-all-pass
  (let [es [{:event {:kind :herd.done :data {:reason :brought}} :within-s 120}
            {:no-event {:source :reflex :kind :fired :context {:reflex-id :pen-gate}} :for-s 5}]
        early (x/judge-all es [herd-done] (assoc opts :now-ms 3000))
        late (x/judge-all es [herd-done] (assoc opts :now-ms 9000))
        bad (x/judge-all es [herd-done (assoc fired :context {:reflex-id :pen-gate})] (assoc opts :now-ms 9000))]
    (is (not (x/decided? early)))
    (is (and (x/decided? late) (x/passed? late)))
    (is (and (x/decided? bad) (not (x/passed? bad))))
    (is (= 120 (x/deadline-s es)))))
