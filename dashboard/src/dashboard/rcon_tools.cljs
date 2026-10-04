(ns dashboard.rcon-tools
  "The three RCON command-line tools (tools/rcon.mjs, rcon-raw.mjs, rcon-test.mjs) as one ahead-of-time compiled
  bundle (shadow-cljs build :rcon-tools). The .mjs files are thin entry points. Each *-main takes the argv array
  and resolves to a process exit code; the codec and client are dashboard.rcon."
  (:require ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [clojure.string :as str]
            [dashboard.rcon :as rcon]))

(def default-port 25575)
(def name-pattern #"^[A-Za-z0-9_]{3,16}$")
(def number-pattern #"^[+-]?\d+(\.\d+)?$")
(def integer-pattern #"^[+-]?\d+$")
(def id-pattern #"^[a-z_]+$")

(defn fail [& parts] (throw (js/Error. (apply str parts))))
(defn quoted [v] (js/JSON.stringify v))

(defn port-from-properties
  "The rcon.port line of a server.properties text, else the default."
  [text]
  (if-let [[_ port] (re-find #"(?m)^rcon\.port=(\d+)" text)]
    (js/Number port)
    default-port))

(defn read-port [properties-file]
  (port-from-properties (.readFileSync fs properties-file "utf8")))

;; ---- rcon.mjs: whitelist add only ----

(defn valid-name [value]
  (when-not (re-matches name-pattern value) (fail "not a valid player name: " (quoted value)))
  value)

(defn whitelist-command [value] (str "whitelist add " (valid-name value)))

;; ---- rcon-raw.mjs: any command ----

(defn build-raw-command
  "argv -> one console command string; strips a single leading slash; throws on empty."
  [argv]
  (let [command (-> (str/join " " (array-seq argv)) str/trim (str/replace #"^/" "") str/trim)]
    (when (str/blank? command) (fail "usage: node tools/rcon-raw.mjs <command...>"))
    command))

;; ---- rcon-test.mjs: allow-list ----

(def default-targets ["ClaudeProbe" "ProbeWater" "ProbeFight" "ProbeNight" "ProbeStuck"])
(def items ["bread" "cooked_beef" "apple" "water_bucket" "bucket" "cobblestone" "dirt" "oak_planks" "iron_sword"
            "diamond_sword" "iron_axe" "torch" "oak_sapling" "wheat_seeds" "red_bed" "shield" "leather_helmet" "iron_helmet"])
(def effects ["poison" "wither" "hunger" "instant_damage" "regeneration" "saturation" "fire_resistance"
              "water_breathing" "slowness"])
(def entities ["zombie" "skeleton" "spider" "creeper" "cow" "pig" "sheep" "chicken"])
(def blocks ["air" "water" "lava" "stone" "dirt" "cobblestone" "oak_planks" "fire" "glass" "torch" "red_bed" "chest"])
;; kill-mobs targets only these: a selector cannot or-together types, so it is one kill command per type, and
;; villagers, pets, item frames and players are never in the list
(def hostile-mobs ["zombie" "skeleton" "creeper" "spider" "enderman" "witch" "drowned" "husk" "stray"
                   "zombie_villager" "phantom" "slime" "pillager" "vindicator"])

(def max-give 64)
(def max-effect-seconds 120)
(def max-amplifier 4)
(def max-damage 20)
(def max-summon 5)
(def max-radius 32)
(def max-fill-volume 500)

(defn targets-from-env [env]
  (let [extra (->> (str/split (or (.-RCON_TEST_TARGETS env) "") #",") (map str/trim) (remove str/blank?))]
    (run! valid-name extra)
    (into default-targets extra)))

(defn target [value targets]
  (when-not (and (re-matches name-pattern value) (some #{value} targets))
    (fail "not an allowed target: " (quoted value) " (allowed: " (str/join ", " targets) ")"))
  value)

(defn plain-number [value what]
  (when-not (re-matches number-pattern value) (fail what " must be a plain number, got " (quoted value)))
  value)

(defn integer [value what]
  (when-not (re-matches integer-pattern value) (fail what " must be an integer, got " (quoted value)))
  (js/Number value))

(defn bounded-int [value what lo hi]
  (let [n (integer value what)]
    (when (or (< n lo) (> n hi)) (fail what " must be between " lo " and " hi ", got " n))
    n))

(defn one-of [value allowed what]
  (when-not (and (re-matches id-pattern value) (some #{value} allowed))
    (fail what " not allowed: " (quoted value) " (allowed: " (str/join ", " allowed) ")"))
  value)

(defn coords [values] (mapv #(plain-number % "coordinate") values))
(defn join-coords [values] (str/join " " (coords values)))

(defn positive-at-most [value what limit]
  (let [v (plain-number value what)]
    (when-not (and (> (js/Number v) 0) (<= (js/Number v) limit))
      (fail what " must be positive and at most " limit ", got " v))
    v))

(defn target-only [prefix] (fn [[t] targets] [(str prefix (target t targets))]))

;; each builder: {:arity [min max] :usage string :build (fn [args targets] -> [command ...])}; arity is checked first
(def builders
  {"list" {:arity [0 0] :usage "" :build (fn [_ _] ["list"])}
   "time" {:arity [1 1] :usage "<day|night|midnight|noon|N>"
           :build (fn [[v] _]
                    (when-not (or (#{"day" "night" "midnight" "noon"} v) (re-matches #"\d+" v))
                      (fail "time must be day, night, midnight, noon or a non-negative integer, got " (quoted v)))
                    [(str "time set " v)])}
   "weather" {:arity [1 1] :usage "<clear|rain|thunder>"
              :build (fn [[v] _]
                       (when-not (#{"clear" "rain" "thunder"} v)
                         (fail "weather must be clear, rain or thunder, got " (quoted v)))
                       [(str "weather " v)])}
   "tp" {:arity [4 4] :usage "<target> <x> <y> <z>"
         :build (fn [[t & xyz] targets] [(str "tp " (target t targets) " " (join-coords xyz))])}
   "give" {:arity [2 3] :usage "<target> <item> [count<=64]"
           :build (fn [[t item count] targets]
                    [(str "give " (target t targets) " minecraft:" (one-of item items "item") " "
                          (bounded-int (or count "1") "count" 1 max-give))])}
   "clear" {:arity [1 1] :usage "<target>" :build (target-only "clear ")}
   "effect" {:arity [2 4] :usage "<target> <effect> [seconds<=120] [amplifier<=4]"
             :build (fn [[t effect seconds amplifier] targets]
                      [(str "effect give " (target t targets) " minecraft:" (one-of effect effects "effect") " "
                            (bounded-int (or seconds "30") "seconds" 1 max-effect-seconds) " "
                            (bounded-int (or amplifier "0") "amplifier" 0 max-amplifier))])}
   "effect-clear" {:arity [1 1] :usage "<target>" :build (target-only "effect clear ")}
   "damage" {:arity [2 2] :usage "<target> <amount<=20>"
             :build (fn [[t amount] targets]
                      [(str "damage " (target t targets) " " (positive-at-most amount "amount" max-damage))])}
   "heal" {:arity [1 1] :usage "<target>"
           :build (fn [[t] targets] [(str "effect give " (target t targets) " minecraft:instant_health 1 10")])}
   "feed" {:arity [1 1] :usage "<target>"
           :build (fn [[t] targets] [(str "effect give " (target t targets) " minecraft:saturation 1 10")])}
   "fire" {:arity [1 1] :usage "<target>  (1 fire damage only; does not ignite)"
           :build (fn [[t] targets] [(str "damage " (target t targets) " 1 minecraft:on_fire")])}
   "summon" {:arity [4 5] :usage "<entity> <x> <y> <z> [count<=5]"
             :build (fn [[entity x y z count] _]
                      (let [id (one-of entity entities "entity")
                            pos (join-coords [x y z])]
                        (vec (repeat (bounded-int (or count "1") "count" 1 max-summon)
                                     (str "summon minecraft:" id " " pos)))))}
   "kill-mobs" {:arity [4 4] :usage "<x> <y> <z> <radius<=32>"
                :build (fn [[x y z radius] _]
                         (let [[cx cy cz] (coords [x y z])
                               r (positive-at-most radius "radius" max-radius)]
                           (mapv #(str "kill @e[type=minecraft:" % ",x=" cx ",y=" cy ",z=" cz ",distance=.." r "]")
                                 hostile-mobs)))}
   "setblock" {:arity [4 4] :usage "<x> <y> <z> <block>"
               :build (fn [[x y z block] _]
                        [(str "setblock " (join-coords [x y z]) " minecraft:" (one-of block blocks "block"))])}
   "fill" {:arity [7 7] :usage "<x1> <y1> <z1> <x2> <y2> <z2> <block>"
           :build (fn [args _]
                    (let [block (one-of (nth args 6) blocks "block")
                          [x1 y1 z1 x2 y2 z2] (mapv #(integer % "fill coordinate") (take 6 args))
                          volume (* (inc (js/Math.abs (- x2 x1))) (inc (js/Math.abs (- y2 y1))) (inc (js/Math.abs (- z2 z1))))]
                      (when (> volume max-fill-volume) (fail "fill volume " volume " exceeds " max-fill-volume))
                      [(str "fill " x1 " " y1 " " z1 " " x2 " " y2 " " z2 " minecraft:" block)]))}})

(defn build-command
  "argv (array) -> vector of command strings; throws Error on anything not allowed."
  [argv targets]
  (let [[sub & args] (array-seq argv)
        {[lo hi] :arity :keys [usage build]} (get builders (or sub ""))]
    (when-not build (fail "unknown or forbidden subcommand"))
    (when-not (<= lo (count args) hi) (fail "usage: " (str/trim (str sub " " usage))))
    (build (vec args) targets)))

;; ---- running ----

(def password-file (path/join (os/homedir) ".config" "minecraft-claude" "rcon-password"))
(def server-properties "/home/dan/minecraft/claude/server.properties")

(defn exit-code-after
  "Resolves to 0 after (run!) succeeds, to 1 after printing `label failed: message` when it rejects."
  [label p]
  (-> p
      (.then (fn [_] 0))
      (.catch (fn [e] (js/console.error (str label " failed: " (.-message e))) 1))))

(defn whitelist-main [argv]
  (exit-code-after
   "whitelist add"
   (-> (js/Promise.resolve nil)
       (.then #(whitelist-command (or (aget argv 0) "")))
       (.then #(rcon/send-commands! {:port (read-port server-properties)} [%]))
       (.then #(js/console.log (first %))))))

(defn raw-main [argv]
  (let [command (try (build-raw-command argv) (catch :default e (js/console.error (.-message e)) nil))]
    (cond
      (nil? command) (js/Promise.resolve 2)
      (not (.existsSync fs password-file)) (do (js/console.error "rcon password file missing") (js/Promise.resolve 1))
      :else (-> (rcon/send-commands! {:port default-port} [command])
                (.then (fn [replies] (js/console.log (first replies)) 0))
                (.catch (fn [e] (js/console.error (str "rcon-raw failed: " (.-message e))) 1))))))

(defn test-main [argv]
  (exit-code-after
   "rcon-test"
   (-> (js/Promise.resolve nil)
       (.then #(build-command argv (targets-from-env js/process.env)))
       (.then #(rcon/send-commands! {:port (read-port server-properties)} %))
       (.then (fn [replies] (run! js/console.log (remove str/blank? replies)))))))

