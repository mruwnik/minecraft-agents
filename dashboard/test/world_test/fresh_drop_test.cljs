(ns world-test.fresh-drop-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.fresh-drop :as d]))

(def t0 1000000)
(defn at [min] (+ t0 (* min 60000)))
(defn ev [min kind & {:as more}] (merge {:time-ms (at min) :kind kind :source :job :generation-id "g1"} more))
(defn completed [min name data] (ev min :completed :data (assoc data :name name)))
(defn at-pos [x z] {:pos {:x x :y 70 :z z}})

(def iron-8 ["iron_pickaxe" "iron_sword" "iron_axe" "iron_shovel" "iron_helmet" "iron_chestplate" "iron_leggings" "iron_boots"])

(def run
  [(ev 30 :spawn-set :data (at-pos 0 0))
   (ev 31 :place.set :data {:name :home})
   (ev 50 :place.set :data {:name :chest})
   (completed 60 "jobs.farm.harvest" {:cut 12 :replanted 12})
   (completed 90 "jobs.farm.harvest" {:cut 10 :replanted 6})
   (completed 100 "jobs.farm.harvest" {:cut 4 :replanted 4})
   (completed 120 "jobs.animals.breed" {:babies 0})
   (completed 130 "jobs.animals.breed" {:babies 1})
   (ev 200 :kit.progress :message "got iron_pickaxe, iron_sword, iron_axe, iron_shovel")
   (ev 235 :kit.progress :message (str "got " (apply str (interpose ", " (take 2 (drop 4 iron-8))))))
   (completed 240 "jobs.items.craft" {:item "iron_leggings"})
   (completed 241 "jobs.items.craft" {:item "iron_boots"})])

(deftest milestones-are-the-first-event-that-proves-them
  (let [m (d/milestones run)]
    (is (= (at 30) (:bed m)))
    (is (= (at 31) (:base m)) "the later of bed and home")
    (is (= (at 50) (:chest m)))
    (is (= (at 100) (:wheat m)) "20 cut and 20 replanted, summed over harvests: 22 cut but 18 replanted at 90 min is not enough")
    (is (= (at 130) (:bred m)) "a breed that made no baby does not count")
    (is (= (at 130) (:farm m)))
    (is (= (at 241) (:kit m)) "all 8 pieces, from reports and crafts")))

(deftest an-unmet-goal-is-absent
  (let [m (d/milestones (take 3 run))]
    (is (= #{:bed :home :chest :base} (set (keys m))))
    (is (= {} (d/milestones [])))))

(deftest report-counts-deaths-kit-losses-and-agent-jobs
  (let [events (sort-by :time-ms
                        (into run [(ev 210 :died :data {:cause "zombie" :inventory [{:name "iron_sword" :count 1} {:name "dirt" :count 3}]})
                                   (ev 220 :died :data {:cause "drown" :inventory [{:name "dirt" :count 3}]})
                                   (ev 1 :queued :context {:chain ["j1"]} :data {:name "jobs.x" :by :agent})
                                   (ev 2 :queued :context {:chain ["j1" "j1/c"]} :data {:name "jobs.y" :by :agent})
                                   (ev 3 :queued :context {:chain ["j2"]} :data {:name "jobs.z" :by :night})]))
        r (d/report "B" events t0 nil)]
    (is (= {:deaths 2 :kit-losses 1 :agent-jobs 1 :teleports 0} (select-keys r [:deaths :kit-losses :agent-jobs :teleports])))
    (is (= (* 241 60000) (get-in r [:milestones :kit])) "times are relative to the start")))

(deftest events-before-the-start-are-ignored
  (is (= {} (:milestones (d/report "B" run (at 1000) nil)))))

(deftest a-position-jump-is-a-teleport-unless-a-respawn-explains-it
  (let [events [(ev 1 :hurt :data (at-pos 0 0))
                (assoc (ev 1 :hurt :data (at-pos 500 0)) :time-ms (+ (at 1) 2000))
                (ev 5 :hurt :data (at-pos 510 0))
                (ev 6 :died :data {})
                (assoc (ev 6 :respawned :data (at-pos 9000 9000)) :time-ms (+ (at 6) 100))
                (assoc (ev 6 :hurt :data (at-pos 9001 9000)) :time-ms (+ (at 6) 200))
                (ev 20 :hurt :data (at-pos 9020 9000))
                (ev 21 :hurt :data (at-pos 9060 9000))]]
    (is (= [{:at-ms (+ (at 1) 2000) :blocks 500}] (d/teleports events))
        "a 500-block jump in 2 s; the respawn far away and a 40-block walk in a minute are not")))

(deftest the-spot-is-far-from-every-base
  (let [bases [[6000 0] [0 -9000] [-7000 7000]]
        spots (mapcat #(d/pick-spots (d/seeded-rnd %) bases 8) (range 30))]
    (is (= 240 (count spots)))
    (is (every? (fn [[x z]] (every? (fn [[bx bz]] (>= (Math/hypot (- x bx) (- z bz)) d/min-base-distance)) bases)) spots))
    (is (= (d/pick-spots (d/seeded-rnd 7) bases 8) (d/pick-spots (d/seeded-rnd 7) bases 8)) "the same seed gives the same spot")))

(deftest the-drop-wipes-then-places-and-sets-spawn
  (let [ss (d/steps {:bodies ["ProbeX"] :world "test" :build "/b" :spots [[1200 -3400] [5 6]] :now 5})
        rcons (map second (filter #(= :rcon (first %)) ss))]
    (is (some #{[:body-start "ProbeX" {:world "test" :build "/b"}]} ss))
    (is (< (.indexOf ss [:wait-online "ProbeX"]) (.indexOf ss [:rcon "clear ProbeX"])) "commands wait for the body to be online")
    (is (= ["gamemode survival ProbeX" "clear ProbeX"] (take 2 rcons)))
    (is (some #{[:place ["ProbeX"] [[1200 -3400] [5 6]]]} ss) "all candidate spots go to the placing step")
    (is (some #(re-find #"spawnpoint ProbeX ~ ~ ~" %) rcons))
    (is (< (.indexOf ss [:place ["ProbeX"] [[1200 -3400] [5 6]]]) (.indexOf ss [:rcon "effect give ProbeX minecraft:slow_falling 60 0 true"])) "a body placed in the air while chunks load survives the fall")
    (is (= [:record "worlds/test/fresh-drops/ProbeX-5.edn" {:body "ProbeX" :world "test" :spots [[1200 -3400] [5 6]] :build "/b" :started-ms 5}] (last ss)))))

(deftest together-places-all-bodies-once-and-sets-each-spawn
  (let [ss (d/steps {:bodies ["A" "B" "C"] :world "test" :build "/b" :spots [[1 2]] :now 5})
        rcons (set (map second (filter #(= :rcon (first %)) ss)))]
    (is (= 1 (count (filter #(= :place (first %)) ss))))
    (is (some #{[:place ["A" "B" "C"] [[1 2]]]} ss))
    (is (= 3 (count (filter #(= :body-start (first %)) ss))))
    (is (every? #(< (.indexOf ss [:wait-online %]) (.indexOf ss [:place ["A" "B" "C"] [[1 2]]])) ["A" "B" "C"]))
    (is (every? #(rcons (str "execute as " % " at " % " run spawnpoint " % " ~ ~ ~")) ["A" "B" "C"]))
    (is (= 3 (count (filter #(= :record (first %)) ss))))))

(deftest rotated-logs-read-oldest-first-and-the-plain-file-wins
  (is (= ["events.edn.3" "events.edn.2.gz" "events.edn.1" "events.edn"]
         (d/log-files ["events.edn" "events.edn.1" "events.edn.1.gz" "events.edn.2.gz" "events.edn.3" "events.edn.debug" "events.edn.meta.edn"]))))

(deftest a-row-shows-dashes-for-unreached-goals
  (is (= "| B | 1970-01-01T00:16 | 0h50 | - | - | 2 | 1 | 0 | 3 | - |"
         (d/row {:body "B" :start-ms t0 :milestones {:base (* 50 60000)} :deaths 2 :kit-losses 1 :teleports 0 :agent-jobs 3}))))

(deftest log-lines-keep-the-kind-time-and-body-position
  (is (= {:time-ms 5 :kind :hurt :seq 9 :generation-id "g" :data {:pos {:x 1.5 :z -3.0}}}
         (d/read-line "{:generation-id \"g\", :source :job, :kind :hurt, :seq 9, :time-ms 5, :data {:pos {:x 1.5, :y 70, :z -3.0}}}")))
  (is (nil? (d/read-line "{:kind :player-joined, :time-ms 5, :data {:player \"T\", :pos {:x 1, :y 2, :z 3}}}")) "another player's position is not the body's")
  (is (= :died (:kind (d/read-line "{:kind :died, :time-ms 5, :data {:cause \"drown\"}}"))) "a full kind is read whole"))

(deftest several-bodies-are-placed-by-one-spreadplayers-each
  (is (= [["spreadplayers" "1" "2" "4" "8" "false" "A"]
          ["execute" "at" "A" "run" "spreadplayers" "~" "~" "2" "4" "false" "B"]
          ["execute" "at" "A" "run" "spreadplayers" "~" "~" "2" "4" "false" "C"]]
         (d/place-commands ["A" "B" "C"] [1 2])))
  (is (= [["spreadplayers" "1" "2" "0" "100" "false" "A"]] (d/place-commands ["A"] [1 2]))))

(deftest only-the-success-line-counts-as-placed
  (is (d/placed-ok? "Spread 1 player(s) around 1.0, 2.0 with an average distance of 3.1 blocks apart"))
  (is (not (d/placed-ok? "Incorrect argument for command at ...lse Oriel Bramble Teasel<--[HERE]")))
  (is (not (d/placed-ok? "Could not spread 1 player around 1.0, 2.0 (too many players for space - try using spread of at most 4)")))
  (is (not (d/placed-ok? ""))))
