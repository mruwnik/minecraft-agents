(ns engine.apiary-maintain-test
  "jobs.apiary.maintain against the fake world."
  (:require [engine.contract :as contract]
            [engine.breed-test :as bt]
            [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.hostile-test :as h]
            [engine.registry :as registry]
            [engine.fake :as fake]
            [engine.test-util :as tu]
            [jobs.apiary.maintain :as maintain]))

(defn inv [& pairs] (mapv (fn [[n c]] {:name n :count c}) (partition 2 pairs)))

(defn around
  "The four horizontal neighbours of x,y,z as stone cells."
  [x y z]
  (into {} (map (fn [[dx dz]] [(str (+ x dx) "," y "," (+ z dz)) "stone"]) [[1 0] [-1 0] [0 1] [0 -1]])))

(def chest {:x 8 :y 64 :z 0})

(defn world
  "A lit campfire at 2,64,0 walled on four sides over walled ground, a hive at 2,66,0 (ripe by default); :raised? opens one side of the fire."
  [{:keys [inventory entities level raised? chest? time raining] :or {level 5}}]
  (cond-> {:inventory inventory
           :entities entities
           :time (or time 1000)
           :raining (boolean raining)
           :self {:pos {:x 0 :y 64 :z 0}}
           :blocks (cond-> (merge (around 2 64 0) (around 2 63 0) {"2,64,0" "campfire" "2,63,0" "stone" "2,66,0" "beehive"})
                     raised? (dissoc "3,64,0"))
           :states {"2,64,0" {:lit true} "2,66,0" {:honey_level level}}}
    chest? (assoc :containers {"8,64,0" []})))

(defn bee
  ([id x] (bee id x {}))
  ([id x more] (merge {:id id :uuid (str "b" id) :name "bee" :kind "passive" :pos {:x x :y 64 :z 2}} more)))

(def box {:min {:x -2 :y 60 :z -6} :max {:x 6 :y 70 :z 6}})

(defn spec [args] (list 'jobs.apiary.maintain (merge {:box box} args)))

(defn ^:async scenario
  "Submit the job with args in a world; run n ticks 700 ms apart; the setup map."
  ([args w n] (scenario args w n identity))
  ([args w n tweak-jobs]
   (let [s (doto (h/setup w) (-> :p tu/seeing-all) (-> :p bt/calf-per-pair!))
         s (update s :eng #(update % :jobs tweak-jobs))]
     (core/submit! (:eng s) (spec args) {})
     (await (tu/run-ticks (:eng s) (:clock s) n 700))
     s)))

(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn done-event [s] (first (events-of s :completed)))
(defn step [s k] (get-in (done-event s) [:steps k]))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn calls [{:keys [p]} name] (h/calls p name))
(defn carried [{:keys [p]} name]
  (->> (array-seq (.-inventory (.self p))) (filter #(= name (.-name %))) (map #(.-count %)) (reduce + 0)))
(defn chest-items [{:keys [p]}]
  (into {} (map (juxt :name :count))
        (get-in @(fake/state p) [:containers [8 64 0]])))
(defn block-name [{:keys [p]} x y z] (.-name (.blockAt p #js {:x x :y y :z z})))
(defn honey [{:keys [p]}] (js/Number (.-honey_level (.-properties (.blockAt p #js {:x 2 :y 66 :z 0})))))

(def kit (inv "shears" 1 "white_carpet" 2))

(deftest the-fire-is-guarded-before-the-hive-is-harvested
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world {:inventory kit}) 60))
              names (mapv #(.-name %) (.-calls (.-world (:p s))))
              first-of (fn [n] (.indexOf names n))]
          (is (= "white_carpet" (block-name s 2 65 0)))
          (is (= 0 (honey s)))
          (is (< (first-of "place") (first-of "useOn")) "carpet down before the shears are used")
          (is (= 1 (:carpeted (step s :guard))))
          (is (= 1 (:harvested (step s :harvest))))
          (is (true? (finished? s))))))))

(deftest a-raised-but-carpeted-fire-is-harvested-as-the-harvest-job-allows
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:ways #{:carried}} (world {:inventory kit :raised? true}) 40))]
          (is (= 1 (:carpeted (step s :guard))))
          (is (= :no-campfire (:reason (step s :guard))))
          (is (= 1 (:harvested (step s :harvest))))
          (is (true? (finished? s))))))))

(deftest nothing-that-can-be-done-declines-the-job
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:ways #{:carried}} (world {:inventory (inv "shears" 1) :raised? true}) 10))]
          (is (nil? (done-event s)))
          (is (empty? (calls s "moveTo")))
          (is (empty? (calls s "useOn"))))))))

(deftest produce-stays-carried-without-a-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {} (world {:inventory kit}) 60))]
          (is (= 3 (carried s "honeycomb")))
          (is (= {:skipped :no-chest} (step s :deposit)))
          (is (true? (finished? s))))))))

(deftest produce-goes-into-the-chest-and-the-gear-stays
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:chest chest} (world {:inventory kit :chest? true}) 100))]
          (is (= {"honeycomb" 3} (chest-items s)))
          (is (= 1 (carried s "shears")))
          (is (= 1 (carried s "white_carpet")))
          (is (= 0 (carried s "honeycomb")))
          (is (true? (finished? s))))))))

(deftest with-a-chest-but-no-produce-nothing-is-deposited-and-the-chest-is-not-visited
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:chest chest} (world {:inventory kit :level 2 :chest? true}) 40))]
          (is (= {:skipped :nothing-to-store} (step s :deposit)))
          (is (empty? (calls s "transfer")))
          (is (empty? (calls s "inspectContainer")))
          (is (true? (finished? s))))))))

(deftest a-safe-apiary-with-nothing-ripe-is-done-already-and-nothing-moves
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [carpeted (-> (world {:inventory kit :level 2}) (update :blocks assoc "2,65,0" "white_carpet"))
              s (await (scenario {} carpeted 10))]
          (is (= :already (:cause (done-event s))))
          (is (= {:skipped :not-ripe} (step s :harvest)))
          (is (empty? (calls s "moveTo")))
          (is (empty? (calls s "place")))
          (is (empty? (calls s "useOn"))))))))

(def flowers (inv "dandelion" 4))
(def bees [(bee 1 3) (bee 2 4)])
(def carpet-and-flowers (into (inv "white_carpet" 1) flowers))

(defn quiet-world [w] (update w :blocks assoc "2,65,0" "white_carpet"))

(deftest bees-below-target-in-daylight-with-flowers-are-fed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} (quiet-world (world {:inventory flowers :level 2 :entities bees})) 30))]
          (is (= ["dandelion" "dandelion"] (mapv #(.-item (.-args %)) (calls s "interact"))))
          (is (= 2 (:fed (step s :breed))))
          (is (true? (finished? s))))))))

(deftest breeding-is-skipped-with-its-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label args w reason]
                [["no target" {} {:inventory carpet-and-flowers :entities bees} :no-target]
                 ["at target" {:target 2} {:inventory carpet-and-flowers :entities bees} :at-target]
                 ["night" {:target 4 :checks {:safe? '(always)}} {:inventory carpet-and-flowers :entities bees :time 15000} :night]
                 ["rain" {:target 4} {:inventory carpet-and-flowers :entities bees :raining true} :raining]
                 ["no flowers" {:target 4 :ways #{:carried}} {:inventory (inv "white_carpet" 1) :entities bees} :no-food]
                 ["one bee" {:target 4} {:inventory carpet-and-flowers :entities [(bee 1 3)]} :too-few-adults]]]
          (let [s (await (scenario args (world (assoc w :level 2)) 12))]
            (is (empty? (calls s "interact")) label)
            (is (= {:skipped reason} (step s :breed)) label)))))))

(deftest breeding-without-flowers-is-called-so-breed-fetches-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:target 4} (world {:inventory (inv "white_carpet" 1) :entities bees :level 2}) 12))]
          (is (nil? (:skipped (step s :breed))) "breed is called, not skipped :no-food"))))))

(deftest babies-count-toward-the-bee-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (world {:inventory carpet-and-flowers :level 2 :entities (conj bees (bee 3 4 {:baby true}))})
              s (await (scenario {:target 3} w 12))]
          (is (empty? (calls s "interact")))
          (is (= {:skipped :at-target} (step s :breed))))))))

(deftest a-step-without-its-tool-is-skipped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:ways #{:carried}} (world {:inventory (inv "white_carpet" 1)}) 30))]
          (is (= {:skipped :no-tool} (step s :harvest)))
          (is (= 5 (honey s)))
          (is (true? (finished? s))))))))

(deftest an-apiary-never-seen-is-looked-at-before-nothing-to-do
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (doto (h/setup (world {:inventory kit})) (-> :p tu/seeing-after-look))]
          (core/submit! (:eng s) (spec {}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 60 700))
          (is (seq (calls s "look")))
          (is (= 1 (:harvested (step s :harvest))))
          (is (true? (finished? s))))))))

(deftest an-apiary-with-nothing-to-do-waits-after-one-look
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (doto (h/setup (world {:inventory (inv "shears" 1) :raised? true})) (-> :p tu/seeing-after-look))]
          (core/submit! (:eng s) (spec {:ways #{:carried}}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 10 700))
          (is (= 8 (count (calls s "look"))) "one look around")
          (is (= [:nothing-to-do] (distinct (map :reason (events-of s :waiting)))))
          (is (nil? (done-event s))))))))

(deftest a-body-that-sees-nothing-even-after-looking-finds-the-apiary-in-order
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (doto (h/setup (world {:inventory (inv "shears" 1) :raised? true}))
                  (-> :p (doto tu/blind (aset "sensedAt" (fn [_] #js {:unknown true})))))]
          (core/submit! (:eng s) (spec {:ways #{:carried}}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 10 700))
          (is (= 12 (count (calls s "look"))) "one look around, not one per run")
          (is (= :already (:cause (done-event s)))))))))

(deftest a-fully-seen-idle-apiary-does-not-look
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (doto (h/setup (world {:inventory (inv "shears" 1) :raised? true})) (-> :p tu/seeing-all))]
          (core/submit! (:eng s) (spec {:ways #{:carried}}) {})
          (await (tu/run-ticks (:eng s) (:clock s) 10 700))
          (is (empty? (calls s "look")) "every cell of the area is already seen")
          (is (= [:nothing-to-do] (distinct (map :reason (events-of s :waiting))))))))))

(deftest a-waiting-child-is-booked-declined-with-its-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [waits (fn [jobs] (assoc jobs 'jobs.apiary.harvest (tu/job [(constantly (contract/later :boom "boom"))] nil)))
              s (await (scenario {} (world {:inventory kit}) 30 waits))]
          (is (= {:skipped :declined :reason :boom} (step s :harvest))))))))

;; the tool of a harvest is fetched unless the ways are only :carried; the children get the caller's ways
(deftest the-children-get-the-callers-ways-and-a-missing-tool-is-fetched-by-default
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [got (atom {})
              spy (fn [jobs] (reduce (fn [jobs k]
                                       (assoc jobs (symbol (str "jobs.apiary." (name k)))
                                              {:round (fn [c] (swap! got assoc k (:ways (:args c))) (ctx/end c :done nil))}))
                                     jobs [:guard :harvest]))
              w (quiet-world (world {:inventory (inv "stick" 1)}))
              _ (await (scenario {} w 10 spy))
              default @got
              _ (reset! got {})
              _ (await (scenario {:ways #{:carried}} w 10 spy))]
          (is (= {:harvest nil} default) "the harvest step ran with no ways of its own")
          (is (= {} @got) "with ways #{:carried} no step is called for a missing item"))))))

(deftest a-declined-child-is-booked-with-the-reason-of-the-ending-it-gave
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [n (atom 0)
              waits (fn [jobs] (assoc jobs 'jobs.apiary.harvest
                                      (tu/job [(fn [_] (contract/later (keyword (str "wait-" (swap! n inc))) "waits"))] nil)))
              s (await (scenario {} (world {:inventory kit}) 30 waits))
              reason (:reason (step s :harvest))]
          (is (= "wait-" (subs (name reason) 0 5)))
          (is (= 1 @n) "the reason is not asked for again"))))))

(deftest a-pass-where-no-step-ran-is-not-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [waits (fn [jobs] (assoc jobs 'jobs.apiary.harvest (tu/job [(constantly (contract/later :boom "boom"))] nil)))
              s (await (scenario {} (quiet-world (world {:inventory (inv "shears" 1)})) 30 waits))]
          (is (nil? (done-event s)) "the report is not a maintain.done")
          (is (true? (finished? s)))
          (is (= [:stopped :nothing-to-do]
                 ((juxt :kind :reason) (first (filter #(#{:completed :stopped} (:kind %)) @(:seen s)))))))))))

(def harvest-worlds
  "Worlds and args where harvest could be chosen, or looks it could."
  [[{} (world {:inventory kit})]
   [{} (world {:inventory (inv "white_carpet" 1)})]
   [{} (world {:inventory kit :level 2})]
   [{} (world {:inventory kit :raised? true})]
   [{:with :bottle} (world {:inventory (inv "shears" 1 "white_carpet" 1)})]
   [{:with :bottle} (world {:inventory (inv "glass_bottle" 1 "white_carpet" 1)})]
   [{:ignore-zones? true} (assoc (world {:inventory kit}) :zones [{:name "bees" :min [2 60 0] :max [2 70 0] :owner "Miles"}])]
   [{} (assoc (world {:inventory kit}) :zones [{:name "bees" :min [2 60 0] :max [2 70 0] :owner "Miles"}])]])

(deftest maintain-never-calls-a-harvest-that-would-wait
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[args w] harvest-worlds]
          (let [s (await (scenario args w 30))]
            (is (not= :declined (:skipped (step s :harvest))) (pr-str args))))))))

(deftest the-fetch-kinds-allowed-decide-whether-a-missing-tool-or-flower-is-fetched
  (let [facts {:bees 2 :adults 2 :day true :raining false :flower nil :reach 5 :tool nil :hives {:ripe 1 :todo [1]}}]
    (is (= :no-food (:skip (maintain/decide :breed {:target 4 :ways #{:carried}} facts))))
    (is (= :no-tool (:skip (maintain/decide :harvest {:ways #{:carried}} facts))))
    (is (some? (:call (maintain/decide :breed {:target 4 :ways #{:carried :chest}} facts))))
    (is (some? (:call (maintain/decide :harvest {:ways #{:carried :chest}} facts))))))

(def tidy-facts
  {:fires {:fires 1 :todo [] :left {}} :hives {:ripe 0 :todo [] :declined {}} :unsafe []
   :tool nil :bees 4 :adults 2 :day true :raining false :flower nil :stored [] :reach 5})

(deftest the-apiary-is-in-order-only-when-every-steps-part-of-the-goal-holds
  (doseq [[args facts in-order?] [[{:target 4} tidy-facts true]
                                  [{} tidy-facts true]
                                  [{:target 6} tidy-facts false]
                                  [{:target 6} (assoc tidy-facts :day false) false]
                                  [{} (assoc tidy-facts :hives {:ripe 1 :todo [1] :declined {}}) false]
                                  [{} (assoc tidy-facts :unsafe [[2 64 0]] :hives {:ripe 1 :todo [] :declined {}}) false]
                                  [{} (assoc-in tidy-facts [:fires :todo] [[2 64 0]]) false]
                                  [{:chest {:x 8 :y 64 :z 0}} (assoc tidy-facts :stored ["honeycomb"]) false]
                                  [{:chest {:x 8 :y 64 :z 0}} tidy-facts true]]]
    (is (= in-order? (maintain/in-order? args facts)) (pr-str args (select-keys facts [:day :hives :unsafe :stored])))))
