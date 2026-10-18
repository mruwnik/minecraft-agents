(ns engine.loose-items-test
  "The :loose-items trigger and its job jobs.items.pick-up-loose: seen items worth the detour are picked up; the
  ones thrown, owed to a live job, in another's zone or not worth it are left."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.event-api :as event-api]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.scenario :as scenario]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.loose :as loose]
            [jobs.lib.world-files :as ew]
            [triggers.items.loose-items :as li]))

(def now 1000000)

(defn item
  "An item entity of name at [x y z], seen, with extra fields."
  ([id name pos] (item id name pos {}))
  ([id name pos extra] (merge {:id id :name "item" :kind "item" :pos pos :item {:name name :count 1} :visible true} extra)))

(def log-near (item 11 "oak_log" [4.5 64 0.5]))
(def dirt-down (item 12 "dirt" [2.5 57 0.5]))

(defn world [& entities] {:floor [-10 -10 10 10] :entities (vec entities)})

(defn view
  "A memory view at now holding the entries [[kind data] ...]."
  [entries]
  (let [s (mem/open (tu/tmp-dir) {:now (constantly now)})]
    (doseq [[kind data] entries] (mem/write! s kind data {:cap 8 :ttl 600000}))
    (mem/view s)))

(defn holds?
  ([w] (holds? w {}))
  ([w {:keys [args memory live zones] :or {args {} memory [] live #{}}}]
   (li/loose-items (tu/fake-on-floor w) (view memory) args (ew/of-data {} {} zones) live)))

(deftest a-seen-log-a-few-blocks-off-holds
  (is (true? (holds? (world log-near)))))

(def live-j1 [(mem/job-kind "j1") {:args {} :children {}}])

(defn holds-with [overrides w & [opts]]
  (settings/with-settings (update-keys overrides #(keyword "jobs.lib.loose" (name %))) #(holds? w opts)))

(deftest dirt-down-a-drop-is-not-worth-it
  (is (false? (holds? (world dirt-down))))
  (is (true? (holds-with {:always ["dirt"]} (world dirt-down))) ":always takes it whatever the ratio")
  (is (false? (holds-with {:never ["oak_log"]} (world log-near))) ":never leaves it"))

(deftest only-seen-items-within-the-radius-count
  (is (false? (holds? (world (assoc log-near :visible false)))) "hidden behind a wall")
  (is (false? (holds-with {:radius 3} (world log-near)))))

(def log-ten-off (item 13 "oak_log" [10.5 64 0.5]))

(deftest register-args-are-ignored-the-settings-rule-trigger-and-job-alike
  (is (false? (holds? (world log-ten-off) {:args {:radius 12}})) "the job it fires reads the settings: radius 8")
  (is (true? (holds-with {:radius 12} (world log-ten-off)))))

(deftest trigger-and-job-count-the-same-jobs-live
  (let [fresh (assoc log-near :spawn {:at (- now 5000)})]
    (is (false? (holds? (world fresh) {:memory [live-j1]}))
        "a listed job's memory counts as live, as the job counts it")
    (is (true? (holds? (world fresh) {:live #{"j1"}})) "not the engine's own list, which the job cannot see")))

(deftest a-nearly-full-inventory-holds-nothing
  (is (false? (holds? (assoc (world log-near) :inventory (vec (for [i (range 34)] {:name "dirt" :count 64 :slot (+ 9 i)})))))))

(deftest a-danger-near-is-left-to-the-jobs-check
  (is (true? (holds? (update (world log-near) :entities conj {:id 7 :name "zombie" :kind "hostile" :pos [-3 64 0]})))))

(defn spawn
  "A spawn record: the item appeared at pos (dx dy dz from the feet of player name at feet), tossed ms before (a toss of item, default oak_log)."
  [player feet [dx dy dz] & [{:keys [ago tossed item]}]]
  (cond-> {:at (- now (or ago 1000)) :pos (mapv + feet [dx dy dz]) :player {:name player :pos feet}}
    tossed (assoc :tosses [{:at (- now (or ago 1000) tossed) :item (or item "oak_log") :count 1}])))

(deftest thrown-items-are-left
  (is (false? (holds? (world (assoc log-near :spawn (spawn "Fake" [4.5 64 0.5] [0 1.32 0] {:ago 600000 :tossed 200}))))) "its own toss, ever")
  (is (false? (holds? (world (assoc log-near :spawn (spawn "Ann" [4.5 64 0.5] [0.05 1.3 0] {:ago 10000}))))) "a player's toss, for a while")
  (is (true? (holds? (world (assoc log-near :spawn (spawn "Ann" [4.5 64 0.5] [0 1.32 0] {:ago 70000}))))) "a player's toss, after :thrown-s"))

(deftest items-make-room-dumped-are-left-whatever-their-spawn
  (let [dumped [[:make-room-rest {:items ["oak_log"]}]]]
    (is (false? (holds? (world log-near) {:memory dumped})) "a dumped item name, with no spawn record")
    (is (false? (holds? (world (assoc log-near :spawn {:at (- now 500000)})) {:memory dumped})) "lying long after the toss window")
    (is (true? (holds? (world dirt-down log-near) {:memory [[:make-room-rest {:items ["dirt"]}]]})) "another item name is still taken")
    (is (true? (holds-with {:dumped-s 0} (world log-near) {:memory dumped})) "past :dumped-s")))

(defn xyz [[x y z]] {:x x :y y :z z})

(defn thrower [s] (loose/thrower (loose/options {}) "Fake" "oak_log" (clj->js (-> s (update :pos xyz) (update-in [:player :pos] xyz)))))

(deftest who-threw-it-is-told-from-the-spawn-point
  (let [feet [3.95 64 0.5]]
    (is (nil? (thrower (spawn "Fake" feet [0.45 1.375 0] {:tossed 300 :item "dirt"})))
        "a log broken at head height beside the body is not its toss of dirt")
    (is (= "Fake" (thrower (spawn "Fake" feet [0.9 1.2 -0.7] {:tossed 300})))
        "a toss while walking at sprint speed lands off the 0.1 box and is still its own")
    (is (nil? (thrower (spawn "Fake" feet [4 1.32 0] {:tossed 300}))) "a toss of that item but the drop far from the body")
    (is (= "Fake" (thrower (spawn "Fake" feet [0 1.32 0] {:tossed 300}))) "at its drop point just after it tossed")
    (is (nil? (thrower (spawn "Fake" feet [0 1.32 0]))) "at its drop point with no toss: a block's drop that landed there")
    (is (nil? (thrower (spawn "Fake" feet [0 1.32 0] {:tossed 5000}))) "its last toss long before")
    (is (= "Ann" (thrower (spawn "Ann" feet [0.05 1.3 0.05]))) "another player's drop point")
    (is (nil? (thrower (spawn "Ann" feet [0.3 1.32 0]))) "beside it")
    (is (nil? (thrower (spawn "Ann" feet [0 0.6 0]))) "at its knees")))

(deftest a-log-broken-at-head-height-beside-the-body-is-picked-up
  (let [feet [3.95 64 0.5]
        e (assoc log-near :spawn (spawn "Fake" feet [0.55 1.375 0] {:ago 200000 :tossed 100 :item "dirt"}))]
    (is (true? (holds? (world e))))))

(deftest another-bodys-zone-or-claim-is-left
  (let [zone (fn [owner] [{:name "yard" :owner owner :min [0 60 -3] :max [8 70 3]}])]
    (is (false? (holds? (world log-near) {:zones (zone "Miles")})))
    (is (true? (holds? (world log-near) {:zones (zone "Fake")})))
    (is (true? (holds? (world log-near) {:zones nil})) "no zone list never blocks a pick-up")))

(deftest items-a-live-job-collects-are-left
  (let [hold [:collecting {:job "j1" :anchor {:x 3 :y 64 :z 0} :radius 4}]]
    (is (false? (holds? (world log-near) {:memory [hold live-j1]})) "inside a running collect-drops' area")
    (is (true? (holds? (world log-near) {:memory [hold]})) "its job ended"))
  (let [fresh (assoc log-near :spawn {:at (- now 5000)})]
    (is (false? (holds? (world fresh) {:memory [live-j1]})) "just dropped while a job runs: that job's to collect")
    (is (true? (holds? (world fresh))) "just dropped with no job running")
    (is (true? (holds? (world (assoc log-near :spawn {:at (- now 200000)})) {:memory [live-j1]})) "lying past :owed-s")))

(deftest items-a-run-left-are-not-tried-again
  (is (false? (holds? (world log-near) {:memory [[:loose-left {:ids [11]}]]}))))

;; ------------------------------------------------------------------ the job and the register

(defn setup [w]
  (let [clock (atom now)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor (merge {:offlineScale 0.0001} w)))
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :backoff false :world (ew/of-data {} {} [])
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn carried [p n] (reduce + 0 (keep #(when (= n (:name %)) (:count %)) (:inventory @(fake/state p)))))

(deftest loose-items-is-a-default-reflex-below-the-survival-ones
  (let [order (vec triggers/order)
        at #(.indexOf order %)]
    (is (= :loose-items (peek order)))
    (doseq [id [:suffocating :burning :wedged :hostile-near :night :hungry :inventory-nearly-full :died]]
      (is (< (at id) (at :loose-items)) (str id)))))

(deftest the-reflex-picks-up-the-log-says-so-once-and-leaves-the-dirt
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (world log-near dirt-down))]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :loose-items}]}"))
          (await (tu/run-until-empty eng 1))
          (dotimes [_ 30] (await (core/tick! eng)))
          (is (= 1 (carried p "oak_log")))
          (is (= 0 (carried p "dirt")))
          (let [picked (filterv #(= :loose-items.picked (:kind %)) @seen)]
            (is (= 1 (count picked)))
            (is (= :info (:level (first picked))))
            (is (= 1 (:collected (first picked))))))))))

(deftest a-child-with-nothing-worth-it-fails-its-condition
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (world dirt-down))
              r (await (tu/child-outcome eng 'jobs.items.pick-up-loose {} 20))]
          (is (= [:condition-failed :nothing-worth-it] [(:kind (:ending r)) (:reason r)]))
          (is (= 0 (carried p "dirt"))))))))

(deftest the-condition-hint-says-why-nothing-was-taken
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [text (fn ^:async text* [w args]
                     (get-in (await (tu/child-outcome (:eng (setup w)) 'jobs.items.pick-up-loose args 6)) [:ending :cause :text]))
              far (await (text (world log-ten-off) {:radius 8}))
              unworthy (await (text (world dirt-down) {}))
              none (await (text (world) {}))]
          (is (re-find #"within 8 blocks" far))
          (is (re-find #"oak_log 1[01] blocks away" far) "the nearest one beyond the radius is named")
          (is (re-find #"dirt not worth the detour" unworthy) "what was seen and why it was left")
          (is (re-find #"no loose item" none)))))))

(deftest a-held-job-with-nothing-worth-it-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup (world dirt-down))]
          (core/submit! eng '(jobs.items.pick-up-loose) {:hold? true})
          (core/submit! eng '(jobs.items.pick-up-loose) {})
          (dotimes [_ 6] (await (core/tick! eng)))
          (is (= 2 (count (:list (core/state eng)))))
          (is (empty? (filterv #(= :stopped (:kind %)) @seen)))
          (is (= #{:nothing-worth-it} (into #{} (map :reason) (filterv #(= :waiting (:kind %)) @seen)))))))))

(deftest a-waiting-held-job-lets-the-next-listed-job-run
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup (world dirt-down))]
          (core/submit! eng '(jobs.items.pick-up-loose) {:hold? true})
          (core/submit! eng '(jobs.movement.go-to {:pos [4.5 64 0.5] :range 1}) {})
          (dotimes [_ 40] (await (core/tick! eng)))
          (is (= 1 (count (:list (core/state eng)))) "the go-to ran and left; only the waiting holder is listed")
          (is (some #(and (= :result (:kind %)) (true? (:arrived %))) @seen) (pr-str (take-last 6 @seen))))))))

(deftest register-args-cut-the-running-job-at-most-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (setup (world log-ten-off))]
          (core/load-scenario! eng (scenario/parse "{:register [{:trigger :loose-items :args {:radius 12}}]}"))
          (dotimes [_ 6]
            (swap! clock + 31000)
            (dotimes [_ 5] (await (core/tick! eng))))
          (is (<= (count (filterv #(and (= :reflex (:source %)) (= :fired (:kind %))) @seen)) 1)))))))

(deftest collect-drops-holds-its-area-while-it-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (world log-near))
              holds (atom nil)
              held #(mapv (comp (juxt :anchor :radius) :data) (mem/entries (mem/view (:store eng)) loose/hold-kind))]
          (.override (.-world p) "collect" (fn [_ _] (reset! holds (held)) #js {:status "failed"}))
          (core/submit! eng '(jobs.items.collect-drops {:radius 6 :near {:x 1 :y 64 :z 0}}) {})
          (await (tu/run-until-empty eng 20))
          (is (= [[{:x 1 :y 64 :z 0} 6]] @holds) "held while it collects")
          (is (= [] (held)) "released at the end"))))))

(deftest the-job-is-a-contract-job-quoting-the-items-it-would-take
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup (world log-near dirt-down))
              entry (get (:jobs eng) 'jobs.items.pick-up-loose)
              {:keys [ok quote]} (await (event-api/quote-view eng "jobs.items.pick-up-loose" "{}"))]
          (is (some? (:can-start? entry)))
          (is (true? ok))
          (is (= {"oak_log" [0 1 1]} (get-in quote [:yields :items])) "the dirt is not worth the detour")
          (is (= [0 1 1] (:done quote))))))))
