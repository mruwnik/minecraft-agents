(ns dashboard.takeover-test
  (:require [cljs.test :refer [deftest is are async]]
            [dashboard.takeover :as t]))

(defn body-argv [name world]
  ["node" "--max-old-space-size=256" "out/body-release.cjs" "--agent" name "--world" world "--worlds" "/w"])

(defn res-slot-chain
  "A body under tools/res-slot: res-slot node, flock, its sh -c, the body. Pids from base, parent ppid."
  [base ppid name world]
  (let [body (body-argv name world)]
    {base {:ppid ppid :start "1" :argv (into ["node" "../tools/res-slot.mjs" "body" "--"] body)}
     (+ base 1) {:ppid base :start "1" :argv (into ["flock" "-n" "-E" "213" "/tmp/mc-res/body.0" "sh" "-c" "printf x; exec \"$@\"" "/tmp/mc-res/body.0"] body)}
     (+ base 2) {:ppid (+ base 1) :start "1" :argv (into ["sh" "-c" "printf x; exec \"$@\"" "/tmp/mc-res/body.0"] body)}
     (+ base 3) {:ppid (+ base 2) :start "1" :argv body}}))

(def table
  (merge
   {1 {:ppid 0 :start "1" :argv ["/sbin/init"]}
    10 {:ppid 1 :start "1" :argv ["/usr/lib/systemd/systemd" "--user"]}
    20 {:ppid 10 :start "1" :argv ["java" "-jar" "server.jar" "nogui"]}
    30 {:ppid 10 :start "1" :argv ["bash"]}
    40 {:ppid 10 :start "1" :argv ["claude" "-p" "start it: npm run body -- --agent Tiberius --world Leo"]}
    50 {:ppid 10 :start "1" :argv ["node" "dashboard/out/server.cjs"]}}
   (res-slot-chain 100 10 "Tiberius" "Leo")
   (res-slot-chain 200 30 "Tiber" "Leo")
   (res-slot-chain 300 10 "Tiberius" "Leo2")
   (res-slot-chain 400 10 "Tiberius" "test")
   (res-slot-chain 500 10 "Leo2" "Leo")
   {600 {:ppid 30 :start "1" :argv ["bash" "-c" "while true; do npm run body -- --agent Leo --world Leo; sleep 1; done"]}
    601 {:ppid 600 :start "1" :argv ["node" "/usr/bin/npm" "run" "body" "--" "--agent" "Leo" "--world" "Leo"]}
    602 {:ppid 601 :start "1" :argv ["sh" "-c" "../tools/compile engine body-release && node out/body-release.cjs --agent Leo --world Leo"]}
    603 {:ppid 602 :start "1" :argv (body-argv "Leo" "Leo")}
    604 {:ppid 603 :start "1" :argv ["some-helper"]}
    700 {:ppid 10 :start "1" :argv ["node" "tools/world-test-pool.mjs" "--body" "Probe" "--bodies" "2"]}
    701 {:ppid 700 :start "1" :argv ["node" "tools/world-test.mjs" "--body" "ProbeA" "x.edn"]}
    702 {:ppid 701 :start "1" :argv (body-argv "ProbeA" "test")}
    703 {:ppid 700 :start "1" :argv ["node" "tools/world-test.mjs" "--body" "ProbeB" "x.edn"]}
    704 {:ppid 703 :start "1" :argv (body-argv "ProbeB" "test")}}))

(defn planned [name world self] (mapv :pid (t/plan table name world self)))

(deftest plan-takes-exactly-the-body-chain-top-down
  (are [name world pids] (= pids (planned name world 50))
    "Tiberius" "Leo" [100 101 102 103]
    "Tiber" "Leo" [200 201 202 203]
    "Tiberius" "Leo2" [300 301 302 303]
    "Leo2" "Leo" [500 501 502 503]
    "Leo" "Leo" [600 601 602 603 604]
    "ProbeA" "test" [701 702]
    "Nobody" "Leo" []))

(deftest plan-roles
  (is (= [:supervisor :body] (mapv :role (t/plan table "ProbeA" "test" 50))))
  (is (= [:body :body :body :body :child] (mapv :role (t/plan table "Leo" "Leo" 50)))))

(deftest plan-spares-the-dashboard-and-its-ancestors
  (is (= [] (planned "Tiberius" "Leo" 103)))
  (is (= [603 604] (planned "Leo" "Leo" 602))))

(deftest launcher-is-the-first-ancestor-left-alive
  (is (= {:pid 30 :argv ["bash"]} (t/launcher table (t/plan table "Leo" "Leo" 50))))
  (is (nil? (t/launcher table (t/plan table "Nobody" "Leo" 50)))))

(deftest parse-stat-reads-past-a-comm-with-parens
  (is (= {:state "S" :ppid 42 :start "9876"}
         (t/parse-stat "123 (a) (b c) S 42 123 123 0 -1 4194560 1 0 0 0 5 6 0 0 20 0 11 0 9876 1000 50\n"))))

(defn fake-io
  "A process table in an atom; a killed pid leaves it; while a `respawner` pid lives, a dead body child is restarted
  under it with a fresh pid."
  [procs respawner]
  (let [next-pid (atom 9000)
        kills (atom [])]
    {:procs procs :kills kills
     :io {:self 50
          :read-table #(deref procs)
          :read-proc #(get @procs %)
          :sleep (fn [_] (js/Promise.resolve nil))
          :kill! (fn [pid]
                   (swap! kills conj pid)
                   (let [dead (get @procs pid)]
                     (swap! procs dissoc pid)
                     (when (and respawner (contains? @procs respawner) (= respawner (:ppid dead)))
                       (swap! procs assoc (swap! next-pid inc) dead))))}}))

(deftest takeover-kills-the-loop-first-so-nothing-respawns
  (async done
    (let [{:keys [io procs kills]} (fake-io (atom table) 602)]
      (-> (t/takeover! io "Leo" "Leo")
          (.then (fn [r]
                   (is (:ok? r))
                   (is (= [600 601 602 603 604] @kills))
                   (is (= [] (:respawned r) (:survivors r)))
                   (is (= [] (t/plan @procs "Leo" "Leo" 50)))
                   (is (= (dissoc table 600 601 602 603 604) @procs))
                   (done)))))))

(deftest takeover-reports-and-kills-a-respawn-it-could-not-see
  (async done
    ;; the respawner is an interactive shell (pid 30): left alive, it restarts Tiber's res-slot wrapper once
    (let [{:keys [io]} (fake-io (atom table) 30)]
      (-> (t/takeover! io "Tiber" "Leo")
          (.then (fn [r]
                   (is (not (:ok? r)))
                   (is (= [200 201 202 203] (mapv :pid (:killed r))))
                   (is (= [9001] (mapv :pid (:respawned r))))
                   (is (= {:pid 30 :argv ["bash"]} (:launcher r)))
                   (done)))))))

(deftest takeover-never-kills-a-reused-pid
  (async done
    (let [{:keys [io procs kills]} (fake-io (atom table) nil)
          io (assoc io :read-proc (fn [pid] (cond-> (get @procs pid) (= pid 103) (assoc :start "2"))))]
      (-> (t/takeover! io "Tiberius" "Leo")
          (.then (fn [_]
                   (is (= [100 101 102] @kills))
                   (done)))))))

(deftest takeover-reports-a-survivor
  (async done
    (let [{:keys [io]} (fake-io (atom table) nil)
          io (assoc io :kill! (fn [_]))]
      (-> (t/takeover! io "Tiber" "Leo")
          (.then (fn [r]
                   (is (not (:ok? r)))
                   (is (= [200 201 202 203] (mapv :pid (:survivors r))))
                   (done)))))))
