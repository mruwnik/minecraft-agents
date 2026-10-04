(ns dashboard.rcon-tools-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.rcon-tools :as tools]))

(def t "ClaudeProbe")

(def hostile ["zombie" "skeleton" "creeper" "spider" "enderman" "witch" "drowned" "husk" "stray" "zombie_villager"
              "phantom" "slime" "pillager" "vindicator"])

(defn build [argv] (vec (tools/build-command (clj->js argv) [t])))

(defn refuses? [argv pattern]
  (try (build argv) false
       (catch :default e (boolean (re-find pattern (.-message e))))))

(deftest build-command-allowed
  (are [argv expected] (= expected (build argv))
    ["list"] ["list"]
    ["time" "day"] ["time set day"]
    ["time" "noon"] ["time set noon"]
    ["time" "night"] ["time set night"]
    ["time" "midnight"] ["time set midnight"]
    ["time" "6000"] ["time set 6000"]
    ["weather" "thunder"] ["weather thunder"]
    ["weather" "clear"] ["weather clear"]
    ["weather" "rain"] ["weather rain"]
    ["tp" t "10" "64" "-5.5"] [(str "tp " t " 10 64 -5.5")]
    ["give" t "bread"] [(str "give " t " minecraft:bread 1")]
    ["give" t "iron_sword" "64"] [(str "give " t " minecraft:iron_sword 64")]
    ["clear" t] [(str "clear " t)]
    ["effect" t "poison"] [(str "effect give " t " minecraft:poison 30 0")]
    ["effect" t "wither" "120" "4"] [(str "effect give " t " minecraft:wither 120 4")]
    ["effect-clear" t] [(str "effect clear " t)]
    ["damage" t "4"] [(str "damage " t " 4")]
    ["damage" t "20"] [(str "damage " t " 20")]
    ["heal" t] [(str "effect give " t " minecraft:instant_health 1 10")]
    ["feed" t] [(str "effect give " t " minecraft:saturation 1 10")]
    ["fire" t] [(str "damage " t " 1 minecraft:on_fire")]
    ["summon" "zombie" "1" "64" "2"] ["summon minecraft:zombie 1 64 2"]
    ["summon" "cow" "1" "64" "2" "3"] (vec (repeat 3 "summon minecraft:cow 1 64 2"))
    ["kill-mobs" "0" "64" "0" "16"] (mapv #(str "kill @e[type=minecraft:" % ",x=0,y=64,z=0,distance=..16]") hostile)
    ["setblock" "1" "2" "3" "stone"] ["setblock 1 2 3 minecraft:stone"]
    ["fill" "0" "0" "0" "7" "7" "6" "glass"] ["fill 0 0 0 7 7 6 minecraft:glass"]
    ["fill" "5" "5" "5" "0" "0" "0" "air"] ["fill 5 5 5 0 0 0 minecraft:air"]))

(deftest extra-target-is-accepted-via-targets
  (is (= ["clear Bob_1"] (vec (tools/build-command #js ["clear" "Bob_1"] [t "Bob_1"])))))

(deftest build-command-refuses
  (are [argv pattern] (refuses? argv pattern)
    ["gamemode" "creative" t] #"unknown or forbidden subcommand"
    ["op" t] #"unknown or forbidden subcommand"
    ["deop" t] #"unknown or forbidden subcommand"
    ["gamerule" "x"] #"unknown or forbidden subcommand"
    ["nonsense"] #"unknown or forbidden subcommand"
    ["tp" t "1,5" "1" "2"] #"number"
    ["summon" "zombie" "^" "1" "2"] #"number"
    ["op" t] #"unknown or forbidden subcommand"
    ["stop"] #"unknown or forbidden subcommand"
    ["execute" "as" t] #"unknown or forbidden subcommand"
    ["constructor"] #"unknown or forbidden subcommand"
    [] #"unknown or forbidden subcommand"
    ["clear" "Mallory"] #"ClaudeProbe"
    ["tp" "Mallory" "1" "2" "3"] #"not an allowed target.*ClaudeProbe"
    ["tp" t "abc" "1" "2"] #"number"
    ["tp" t "~" "1" "2"] #"number"
    ["setblock" "~1" "2" "3" "stone"] #"number"
    ["tp" t "1" "2"] #"usage"
    ["give" t "command_block"] #"not allowed"
    ["give" t "bread" "65"] #"count"
    ["give" t "bread" "0"] #"count"
    ["summon" "zombie" "1" "2" "3" "6"] #"count"
    ["summon" "item" "1" "2" "3"] #"not allowed"
    ["fill" "0" "0" "0" "7" "7" "7" "stone"] #"volume"
    ["fill" "0" "0" "0" "1.5" "1" "1" "stone"] #"integer"
    ["setblock" "1" "2" "3" "bedrock"] #"not allowed"
    ["kill-mobs" "0" "0" "0" "33"] #"radius"
    ["kill-mobs" "0" "0" "0" "5" "@e"] #"usage"
    ["kill-mobs" "@e" "0" "0" "5"] #"number"
    ["effect" t "poison" "121"] #"seconds"
    ["effect" t "poison" "10" "5"] #"amplifier"
    ["effect" t "speed"] #"not allowed"
    ["damage" t "21"] #"amount"
    ["fire" t "5"] #"usage"
    ["time" "dusk"] #"time"
    ["weather" "snow"] #"weather"))

(def defaults ["ClaudeProbe" "ProbeWater" "ProbeFight" "ProbeNight" "ProbeStuck"])

(defn env-targets [value] (vec (tools/targets-from-env (doto (js-obj) (aset "RCON_TEST_TARGETS" value)))))

(deftest targets-from-env
  (are [value expected] (= expected (env-targets value))
    nil defaults
    "" defaults
    "Bob_1" (conj defaults "Bob_1")
    "Bob_1, Carol" (into defaults ["Bob_1" "Carol"])))

(deftest targets-from-env-rejects-bad-names
  (doseq [bad ["ab" "has space" "Bob;op" "Bob,@a"]]
    (is (thrown-with-msg? js/Error #"not a valid player name" (tools/targets-from-env #js {:RCON_TEST_TARGETS bad})))))

(deftest default-targets-include-the-probe-bodies
  (let [targets (tools/targets-from-env #js {})]
    (is (= [["tp ProbeWater 1 2 3"] ["tp ProbeStuck 1 2 3"]]
           (mapv #(vec (tools/build-command #js ["tp" % "1" "2" "3"] targets)) ["ProbeWater" "ProbeStuck"])))))

(deftest kill-mobs-never-selects-protected-types
  (let [types (set (map #(second (re-find #"type=minecraft:(\w+)" %)) (build ["kill-mobs" "0" "64" "0" "16"])))]
    (doseq [type ["villager" "wolf" "cat" "item_frame" "armor_stand" "player" "item"]]
      (is (not (contains? types type))))))

(deftest whitelist-command-only-adds-valid-names
  (is (= "whitelist add Aviendha" (tools/whitelist-command "Aviendha")))
  (doseq [bad ["" "ab" "x y" "Avi\nstop" "Avi;op Claude" (apply str (repeat 17 "a")) "remove Steve" "../x"]]
    (is (thrown-with-msg? js/Error #"not a valid player name" (tools/whitelist-command bad)))))

(deftest build-raw-command
  (are [argv expected] (= expected (tools/build-raw-command (clj->js argv)))
    ["list"] "list"
    ["time" "set" "day"] "time set day"
    ["/list"] "list"
    ["/" "say" "hi"] "say hi"
    ["//foo"] "/foo"
    ["say  hi"] "say  hi"))

(deftest build-raw-command-rejects-empty
  (doseq [argv [[] [""] ["/"] ["  "]]]
    (is (thrown-with-msg? js/Error #"usage" (tools/build-raw-command (clj->js argv))))))

(deftest port-from-properties
  (are [text expected] (= expected (tools/port-from-properties text))
    "a=1\nrcon.port=25580\nb=2" 25580
    "rcon.port=25575x" 25575
    "other=1" 25575
    "" 25575))
