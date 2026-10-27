(ns world-test.kit-trial-test
  (:require [cljs.test :refer [deftest is async]]
            [world-test.kit-trial :as k]))

(def t0 1000000)
(defn at [min] (+ t0 (* min 60000)))
(defn ev [min kind & {:as more}] (merge {:time-ms (at min) :kind kind :source :job :generation-id "g1"} more))

(def queued-goal (ev 0 :queued :context {:chain ["j1"]} :data {:name "jobs.items.get-kit" :by :agent}))

(def clean-events
  [queued-goal
   (ev 2 :picked-up :data {:item "oak_log"})
   (ev 5 :child_ended :data {:slot :craft :ending {:kind :done :data {:item "wooden_pickaxe" :made 1}}})
   (ev 9 :picked-up :data {:item "cobblestone"})
   (ev 20 :child_ended :data {:slot :smelt :ending {:kind :done :data {:smelted 3}}})
   (ev 40 :kit.progress :data {:item "iron_pickaxe"})
   (ev 60 :completed :data {:name "jobs.items.get-kit"})])

(deftest milestones-are-the-first-event-that-proves-them
  (let [m (k/milestones (conj clean-events (ev 70 :picked-up :data {:item "birch_log"})))]
    (is (= (at 2) (:logs m)) "the first log, not the later one")
    (is (= [(at 5) (at 9) (at 20) (at 40) (at 60)]
           (map m [:wooden-pickaxe :cobblestone :first-ingot :iron_pickaxe :kit-done])))))

(deftest a-smelt-that-made-nothing-is-no-ingot
  (is (nil? (:first-ingot (k/milestones [(ev 1 :child_ended :data {:slot :smelt :ending {:kind :done :data {:smelted 0}}})])))))

(deftest a-clean-run
  (let [rep (k/report {:started-ms t0} clean-events (at 61))]
    (is (= {:clean? true :failed []} (select-keys (k/clean rep) [:clean? :failed])))
    (is (= 3 (:kit-days rep)) "60 wall minutes are 3 in-game days: the limit itself is clean")))

(deftest the-clean-criterion-names-every-failure
  (let [events (into clean-events
                     [(ev 10 :died :data {:cause "drown"})
                      (ev 11 :queued :context {:chain ["j9"]} :data {:name "jobs.gather.mine" :by :agent})
                      (ev 12 :stopped :level :warn :data {})
                      (ev 13 :picked-up :generation-id "g2" :data {:item "dirt"})])
        rep (k/report {:started-ms t0} (sort-by :time-ms events) (at 61))]
    (is (= #{:death :intervention :crash :give-up-without-reason} (set (:failed (k/clean rep)))))))

(deftest a-kit-after-three-days-is-too-slow-and-no-kit-is-not-done
  (is (= [:kit-too-slow] (:failed (k/clean (k/report {:started-ms t0} [queued-goal (ev 100 :completed :data {:name "jobs.items.get-kit"})] (at 101))))))
  (is (= [:kit-not-done] (:failed (k/clean (k/report {:started-ms t0} [queued-goal] (at 5)))))))

(deftest stalls-are-gaps-over-five-minutes-between-progress-events
  (let [events [(ev 1 :picked-up) (ev 3 :picked-up) (ev 20 :picked-up)]]
    (is (= [{:from-ms (at 3) :to-ms (at 20)}] (k/stalls events t0 (at 21))))))

(deftest a-dig-ending-is-progress
  (let [dug (ev 5 :child_ended :data {:ending {:data {:dug true}}})
        other (ev 5 :child_ended :data {:ending {:data {:count 2}}})]
    (is (= [] (k/stalls [(ev 1 :picked-up) dug (ev 9 :picked-up)] t0 (at 10))))
    (is (= [{:from-ms (at 1) :to-ms (at 9)}] (k/stalls [(ev 1 :picked-up) other (ev 9 :picked-up)] t0 (at 10))))))

(deftest run-names-are-valid-player-names
  (doseq [s k/spots n [1 12]]
    (is (re-matches #"[A-Za-z0-9_]{3,16}" (k/run-name s n)))))

(deftest steps-need-coordinates-and-end-with-the-one-goal
  (let [oak (first (filter #(= :oak (:id %)) k/spots))
        ss (k/steps {:spot oak :n 2 :build "/b" :now t0})]
    (is (= [:whitelist "KitOak02"] (first ss)))
    (is (= [:submit "KitOak02" k/goal-spec] (last ss)))
    (is (some #(= [:rcon "spreadplayers 674 -408 0 24 false KitOak02"] %) ss))
    (is (not-any? #(= [:rcon "time set 1000"] %) ss) "no server clock change without the flag")
    (is (some #(= [:rcon "time set 1000"] %) (k/steps {:spot oak :n 2 :build "/b" :now t0 :time? true})))
    (is (thrown-with-msg? js/Error #"no coordinates" (k/steps {:spot (first (filter #(= :hills (:id %)) k/spots)) :n 1 :build "/b" :now t0})))))

(deftest given-items-follow-the-clear-and-are-recorded
  (let [oak (first (filter #(= :oak (:id %)) k/spots))
        give (k/parse-give "white_bed,bread:8")
        ss (k/steps {:spot oak :n 2 :build "/b" :now t0 :give give})
        clear-i (.indexOf ss [:rcon "clear KitOak02"])]
    (is (= [["white_bed" 1] ["bread" 8]] give))
    (is (= [:rcon "give KitOak02 minecraft:white_bed 1"] (nth ss (+ clear-i 1))))
    (is (= [:rcon "give KitOak02 minecraft:bread 8"] (nth ss (+ clear-i 2))))
    (is (= give (:given (nth (first (filter #(= :record (first %)) ss)) 2))))
    (is (= give (:given (k/report {:started-ms t0 :given give} [] (at 1)))))))

(deftest debug-level-logs-are-read-too
  (is (every? k/log-file? ["events.edn" "events.edn.2" "events.edn.debug" "events.edn.debug.1"]))
  (is (not-any? k/log-file? ["events.edn.meta.edn" "memory.edn"])))

(deftest a-cancelled-goal-is-its-own-failure-and-ends-the-stall-window
  (let [cancelled (ev 4 :cancelled :context {:chain ["j1"]} :data {:kind :cut-off :cause :death})
        rep (k/report {:started-ms t0} [queued-goal (ev 2 :picked-up :data {:item "oak_log"}) cancelled] (at 61))]
    (is (= {:at-ms (- (at 4) t0) :cause :death} (:goal-cancelled rep)))
    (is (= [] (:stalls rep)) "nothing after the goal's end is a stall")
    (is (contains? (set (:failed (k/clean rep))) :goal-cancelled))))

(deftest the-setup-waits-for-the-join-before-any-rcon
  (let [oak (first (filter #(= :oak (:id %)) k/spots))
        ss (k/steps {:spot oak :n 2 :build "/b" :now t0})
        kinds (mapv first ss)]
    (is (= [:wait-join "KitOak02" t0 k/join-timeout-ms] (nth ss (inc (.indexOf kinds :body-start)))))
    (is (< (.indexOf kinds :wait-join) (.indexOf kinds :rcon)))))

(deftest a-join-is-a-spawned-or-online-event-since-the-start
  (is (k/joined? [(ev 1 :spawned)] t0))
  (is (k/joined? [(ev 1 :online)] t0))
  (is (not (k/joined? [(ev -5 :spawned) (ev 1 :planned)] t0))))

(deftest wait-until-sees-a-late-join-and-gives-up-on-none
  (let [calls (atom 0)
        slept (atom [])
        sleep (fn [ms] (swap! slept conj ms) (js/Promise.resolve nil))
        opts {:timeout-ms 10 :interval-ms 2 :sleep sleep}]
    (async done
      (.then (k/wait-until #(> (swap! calls inc) 3) opts)
             (fn [late]
               (is (true? late))
               (is (= [2 2 2] @slept) "polled until the fourth probe")
               (.then (k/wait-until (constantly false) opts)
                      (fn [never]
                        (is (false? never))
                        (done))))))))
