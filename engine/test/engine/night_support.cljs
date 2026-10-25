(ns engine.night-support
  "Shared helpers of the engine.night test namespaces."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.fake :as fake]
            [engine.job-api :as job-api]
            [engine.events :as events]
            [engine.fake.raw-world :as fake-raw]
            [engine.memory :as mem]
            [engine.perception :as perception]
            [engine.perception.rays :as rays]
            [engine.settings :as settings]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.shelter.beds :as sh-beds]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.targets :as targets]
            [jobs.survival.night :as night]
            [jobs.survival.night.sites :as sites]))

(def night st/night)

(def noon st/noon)

(def roof {"0,66,0" "stone"})

(defn holds?
  "The :night condition in world after the night's look pass and seed! wrote body memory; :later moves the clock on after seed!."
  [world seed! & {:keys [later] :or {later 0}}]
  (let [{:keys [eng clock]} (st/setup {})
        p (tu/fake world)]
    (st/looked-round! eng p)
    (seed! eng)
    (swap! clock + later)
    (boolean ((:when (get triggers/all :night)) p (mem/view (:store eng)) {}))))

(defn leaves?
  "The :dawn condition in world after seed! wrote body memory."
  [world seed!]
  (let [{:keys [eng]} (st/setup {})
        p (tu/fake world)]
    (seed! eng)
    (boolean ((:when (get triggers/all :dawn)) p (mem/view (:store eng)) {} nil))))

(defn write! [eng kind data] (mem/write! (:store eng) kind data nil))

(defn status! [sleeping needed] (fn [eng] (write! eng :sleep-status {:sleeping sleeping :needed needed})))

(def closed-hut
  "A 3x3 room around the body with walls to y 69 and a roof at y 70 (out of :roof-height's reach), floor at y 63."
  (into {} (for [x (range -2 3) z (range -2 3) y (range 63 71)
                 :let [wall? (or (= 2 (Math/abs x)) (= 2 (Math/abs z)))]
                 :when (or wall? (= y 63) (= y 70))]
             [(str x "," y "," z) "stone"])))

(defn holds-after?
  "The :night condition in world after steps: [kind data] writes, each 1 s after the last."
  [world steps]
  (let [{:keys [eng clock]} (st/setup {})]
    (doseq [[kind data] steps]
      (swap! clock + 1000)
      (write! eng kind data))
    (boolean ((:when (get triggers/all :night)) (tu/fake world) (mem/view (:store eng)) {}))))

(def sleeper {:id 5 :name "Alex" :kind "player" :sleeping true :pos {:x 4 :y 64 :z 0}})

;; ------------------------------------------------------------------ job: sleepers
(defn offline-why [p] (mapv #(.-why (.-args %)) (st/calls p "offline")))

(defn on-return!
  "Each return from offline moves the engine clock on by the time away and calls (f n), n the return's count."
  [p clock f]
  (let [n (atom 0)]
    (.override (.-world p) "offline"
               (fn ^:async g [token a impl]
                 (let [r (await (impl token a))]
                   (swap! clock + (.-ms a))
                   (.emit (.-world p) #js {:kind "online" :pos #js {:x 0 :y 64 :z 0}})
                   (f (swap! n inc))
                   r)))))

(defn sleep-count! [p sleeping needed]
  (.emit (.-world p) #js {:kind "sleep-status" :sleeping sleeping :needed needed}))

(defn ^:async log-out-child
  "The child result of a log-out call from a recording parent, run in world until the parent is done."
  [world args]
  (let [{:keys [eng p]} (st/setup world)
        out (atom nil)
        parent (tu/recording-parent 'jobs.survival.log-out args out)
        eng (assoc eng :jobs (assoc (:jobs eng) 'out-parent parent))]
    (sleep-count! p 1 2)
    (core/submit! eng '(out-parent) {})
    (await (core/tick! eng))
    @out))

;; ------------------------------------------------------------------ one whole attempt per night
(defn fired [seen] (count (filterv #(= [:reflex :fired :night] [(:source %) (:kind %) (:reflex %)]) @seen)))

(def wide-ground
  "Dirt from y 60 to 63 over x -14..14, z -2..2: pit sites a relocation can reach."
  (into {} (for [x (range -14 15) z (range -2 3) y (range 60 64)] [(str x "," y "," z) "dirt"])))

;; ------------------------------------------------------------------ a niche in a hillside
(def hillside
  "Dirt ground over x -2..8, z -2..2 with a stone hill (x 4..8, y 64..66) on it, and a ring of obsidian (columns the world built, so the stair's sides read as seen air) with a 1-wide obsidian road (no pit site: the iron pickaxe cannot dig it) west of it so the ground is no closed room: no pit site 9+ from a failed one, a niche in the hill."
  (merge (into {} (for [x (range -2 9) z (range -2 3) y (range 60 64)] [(str x "," y "," z) "dirt"]))
         (into {} (for [x (range 4 9) z (range -2 3) y (range 64 67)] [(str x "," y "," z) "stone"]))
         (into {} (for [x (range -5 12) z (range -5 6) :when (not (and (<= -2 x 8) (<= -2 z 2)))] [(str x ",63," z) "obsidian"]))
         (into {} (for [x (range -1100 -2)] [(str x ",63,0") "obsidian"]))))

(def breached-room (merge st/floor (dissoc st/bed-room "-2,64,0" "-2,65,0")))

(defn run-night-in
  "Night in world with a fresh :bed-open entry at the bed; calls (f {:p :seen}) once the queue is empty."
  [world f]
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (st/setup (merge {:time night :inventory st/dirt-stack} world))]
          (mem/write! (:store eng) :bed-open {:pos {:x 6 :y 64 :z 0}} sh-beds/bed-open-policy)
          (core/submit! eng '(jobs.survival.night) {})
          (await (tu/run-until-empty eng 40))
          (f {:eng eng :p p :seen seen}))))))

;; ------------------------------------------------------------------ job: a roofed place close by
(def home-roof
  "A one-cell stone room at x 10, z 0 with a doorway in its west wall and a roof over the cell: a roof on open sides is no shelter, nothing quotes a walk to it."
  (into {"10,66,0" "stone"}
        (for [x [9 10 11] y [64 65] z [-1 0 1] :when (not (#{[10 0] [9 0]} [x z]))] [(str x "," y "," z) "stone"])))

(def lit-world
  "A lit route home with the body's own cell dark: darkness is the light of the cell it stands in, so a lit cell is no night to shelter from."
  {:time night :inventory st/dirt-stack :blocks (merge wide-ground home-roof) :light-default [15 4] :light {[0 64 0] [15 0]}})

(defn know-home! [eng pos] (mem/write! (:store eng) :home {:pos pos} mem/place-policy))

(defn night-at-home
  "Run the night job (with args) in world with :home at pos (none with :home? false; pos is still what the body has seen around), the body seeing through its perception: {:eng :p}."
  ([world pos] (night-at-home world pos {}))
  ([world pos args] (night-at-home world pos args {}))
  ([world pos args {:keys [unseen rand ray-deg radius home?] :or {ray-deg 2 radius 24 home? true}}]
  (let [[seen sink] (tu/legacy-capture-sink)
        raw-p (tu/fake (merge {:offlineScale 0.0001 :floor tu/walk-floor} (dissoc world :light-default :light)))
        _ (swap! (fake/state raw-p) merge (select-keys world [:light-default :light]))
        per (perception/create (fake-raw/create raw-p) {:ray-deg ray-deg :radius radius :now (constantly 1000000) :rand (or rand (constantly 0.5))})
        p (perception/wrap raw-p per)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now (constantly 1000000)
                          :events (events/make {:body "Fake" :sinks [sink] :now (constantly 1000000)})})]
    (when-not unseen
      (aset p "seenBlockAt" (fn [pos] #js {:name (.-name (.blockAt raw-p pos)) :pos pos :age-ms 0}))
      (doseq [yaw [0 90 180 270]] ; what the body has seen by now (reads are sensed: felt, in view or remembered)
        (swap! (fake/state raw-p) assoc :yaw yaw)
        (perception/pass! per))
      (doseq [x (range (- (:x pos) 3) (+ (:x pos) 4)) y (range 62 68) z (range (- (:z pos) 3) (+ (:z pos) 4))] ; the inside of the home it has lived in, which no ray from outside reaches
        (rays/touch! per [x y z])))
    (swap! (fake/state raw-p) assoc :yaw 0)
    (st/dawn-after! p st/default-dawn)
    (when home? (know-home! eng pos))
    (core/submit! eng (list 'jobs.survival.night args) {})
    (js/Promise.resolve {:eng eng :p p :seen seen :look-round! #(doseq [yaw [0 90 180 270]]
                                                                (swap! (fake/state raw-p) assoc :yaw yaw)
                                                                (perception/pass! per))}))))

(defn ^:async with-child-log
  "Run (f), a promise, with ctx/call logging [key sym args] of every child call; the log. (on-call c k sym a) may
  answer an ending or :continue to stand for the child."
  ([f] (with-child-log f (fn [_ _ _ _] nil)))
  ([f on-call]
   (let [log (atom [])
         orig ctx/call]
     (set! ctx/call (fn ^:async g [c k sym a]
                            (swap! log conj [k sym a])
                            (let [r (on-call c k sym a)]
                              (if (some? r) r (await (orig c k sym a))))))
     (await (f))
     (set! ctx/call orig)
     @log)))

(def walled-home
  "The home walled in with a built block, which go-to never digs to reach it."
  (into home-roof (for [x [9 10 11] y [64 65] z [-1 0 1] :when (not= [x z] [10 0])] [(str x "," y "," z) "stone_bricks"])))

(def walled-pen
  "A cell at [10 64 4] walled in with a built block, which go-to never digs to reach it."
  (into {} (for [x [9 10 11] y [64 65] z [3 4 5] :when (not= [x z] [10 4])] [(str x "," y "," z) "stone_bricks"])))

(def bed-world (update lit-world :inventory conj {:name "red_bed" :count 1}))

(defn with-room
  "bed-world with room built in place of the home-roof cell: its walls would close a doorway or fill a room that room opens."
  [room]
  (-> bed-world (update :blocks #(apply dissoc % (keys (dissoc home-roof "10,66,0")))) (update :blocks merge room)))

(defn bed-places [p]
  (filterv #(= "red_bed" (.-item (.-args %))) (st/calls p "place")))

(defn ^:async dug-into-home
  "The night with a carried bed and a natural-stone home (go-to digs into it) at x 10; a looping night is cut off at the
  third walk in (a walk that fails ends it), so a red run ends. {:p :log}."
  [home & carried]
  (let [{:keys [eng p]} (await (night-at-home (-> (with-room home) (update :inventory into carried)) {:x 10 :y 64 :z 0}))
        walks (atom 0)
        log (await (with-child-log #(tu/run-until-empty eng 60)
                     (fn [_ k _ _] (when (and (= :roof-walk k) (< 2 (swap! walks inc)))
                                     {:kind :nothing-possible :data {}}))))]
    (core/submit! eng '(jobs.survival.leave-shelter) {})
    (await (tu/run-until-empty eng 60))
    {:p p :log log}))

(def stone-room
  "A stone room of three by two cells (x 10..12, z 0..1), roofed over those cells only, with no way in."
  (let [inside? (fn [x z] (and (<= 10 x 12) (<= 0 z 1)))]
    (merge (into {} (for [x (range 9 14) y [64 65] z (range -1 3) :when (not (inside? x z))] [(str x "," y "," z) "stone"]))
           (into {} (for [x (range 10 13) z [0 1]] [(str x ",66," z) "stone"])))))

(defn block-at [p pos] (.-name (.blockAt p (clj->js pos))))

(def room-with-doorway
  "stone-room with a doorway in its north wall at 11,64,-1, open to the sky."
  (dissoc stone-room "11,64,-1" "11,65,-1"))

(defn ^:async doorway-shut-early
  "The night shuts stone-room's dug doorway, then ends before dawn (no room for the bed) and it turns noon. {:eng :p :seen :filled}."
  []
  (let [{:keys [eng p] :as r} (await (night-at-home (with-room stone-room) {:x 10 :y 64 :z 0}))]
    (await (with-child-log #(tu/run-until-empty eng 60)
             (fn [c k _ _] (when (= :bed-down k)
                             (ctx/remember! c :bed-place-failed {:pos (sh-sense/feet p)} sh-beds/bed-place-failed-policy)
                             {:kind :nothing-possible :data {:reason :bed-place-failed}}))))
    (is (= [] (st/calls p "sleep")) "no sleep")
    (.setTime (.-world p) noon)
    (assoc r :filled (set (map st/arg-pos (st/calls p "place"))))))

(defn dawn-holds-now? [eng p] ((:when (get triggers/all :dawn)) p (mem/view (:store eng)) {} nil))

;; ------------------------------------------------------------------ flee somewhere safer
(def dark-world (assoc lit-world :light-default [15 0]))

(defn flee-home-night
  "night-at-home in the dark with a home at x 10 the time left does not cover (home-walk-margin-s 0), no placing or digging, nothing but the home to see: the
  shut-in fails everywhere, so the night has to flee to the roof. {:eng :p}."
  [world args]
  (js/Promise.resolve
   (.then (night-at-home world {:x 10 :y 64 :z 0} (update args :settings #(merge {:jobs.survival.night.sites/home-walk-margin-s 0} %)))
          (fn [{:keys [p] :as r}]
            (st/refuse-placing! p)
            (.override (.-world p) "dig" (fn ^:async f [_ _ _] #js {:status "failed"}))
            (aset p "seenBlocks" (fn [_] #js []))
            r))))

(def stone-and-dirt-patch
  (merge (into {} (for [x (range 28 33) z (range -2 3)] [(str x ",63," z) "dirt"]))))

(defn ^:async flee-with
  "The home flee night (a roofed home the time left does not cover) with targets/nearest! answering by answer-of (a fn of the
  targets it is given); [log seen calls]: the child log, the events and the targets each call was given."
  [answer-of]
  (let [calls (atom [])
        real targets/nearest!]
    (set! targets/nearest! (fn ([c ts r] (targets/nearest! c ts r nil))
                               ([_ ts _ _] (swap! calls conj ts) (js/Promise.resolve (answer-of ts)))))
    (try
      (let [{:keys [eng seen]} (await (flee-home-night (update dark-world :blocks merge {"10,66,0" "stone"}) {}))]
        [(await (with-child-log #(tu/run-until-empty eng 80))) seen @calls])
      (finally (set! targets/nearest! real)))))

;; ------------------------------------------------------------------ caves and overhangs
(def cave-blocks
  "An overhang at x 22..26, z -2..2: a stone floor, a roof at y 66, a back wall at x 26."
  (merge (into {} (for [x (range 22 27) z (range -2 3)] [(str x ",63," z) "stone"]))
         (into {} (for [x (range 22 27) z (range -2 3)] [(str x ",66," z) "stone"]))
         (into {} (for [y [64 65] z (range -2 3)] [(str "26," y "," z) "stone"]))))

(defn cave-night
  "A night at the origin where nothing can be placed west of x 20 (no pit there), over cave-blocks; {:eng :p :seen}."
  ([inventory] (cave-night inventory {}))
  ([inventory args]
  (let [{:keys [p] :as r} (st/setup {:time night :inventory inventory :blocks cave-blocks})]
    (tu/seeing-all p)
    (.override (.-world p) "place"
               (fn ^:async f [token a impl]
                 (if (< (.-x (.-pos a)) 20)
                   #js {:status "no-support"}
                   (await (impl token a)))))
    (st/dawn-after! p 12)
    (core/submit! (:eng r) (list 'jobs.survival.night args) {})
    r)))

;; ------------------------------------------------------------------ starving while held
(defn held-starving
  "(The night trigger fires the night as a reflex, as a body's night runs.) A held (exposed, nothing to build with) night at food 0 with nothing to eat; the clock moves 5 ms a read and 20 s a wait, and day comes at the 12th wait or the first attack. Every wait counts: with a moving clock a hold's pause waits a little under hold-ms."
  [entities & [self inventory]]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake {:offlineScale 0.0001 :floor tu/walk-floor :time night :blocks st/hard-ground
                                   :self (or self {:food 0 :health 15}) :inventory (or inventory []) :entities entities}))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(swap! clock + 5)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    (core/load-scenario! eng (scenario/parse "{:register [{:trigger :night}]}"))
    (st/refuse-placing! p)
    (let [waits (atom 0)]
      (.override (.-world p) "wait" (fn ^:async w [token a impl]
                                      (if (>= (swap! waits inc) 12) (.setTime (.-world p) st/noon) (swap! clock + 20000))
                                      (await (impl token a)))))
    (.override (.-world p) "attack" (fn ^:async g [token a impl]
                                      (.setTime (.-world p) st/noon)
                                      (await (impl token a))))
    {:eng eng :p p :seen seen}))

(def zombie {:id 8 :name "zombie" :kind "hostile" :pos {:x 4 :y 64 :z 2} :health 20})
