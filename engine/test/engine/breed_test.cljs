(ns engine.breed-test
  "engine.jobs.animals and jobs.animals.breed against the fake world."
  (:require [cljs.test :refer [deftest is async]]
            [engine.backoff :as backoff]
            [engine.core :as core]
            [engine.hostile-test :as h]
            [engine.jobs.animals :as animals]
            [engine.takeover :as takeover]
            [engine.test-util :as tu]))

(defn ent
  ([id name x] (ent id name x {}))
  ([id name x more] (merge {:id id :uuid (str "u" id) :name name :kind "passive" :pos {:x x :y 64 :z 0}} more)))

(defn cow [id x & [more]] (ent id "cow" x more))

(defn wheat [n] [{:name "wheat" :count n}])

(defn ^:async run-ticks
  [{:keys [eng clock]} n step]
  (dotimes [_ n]
    (swap! clock + step)
    (await (core/tick! eng))))

(defn ^:async scenario
  "Submit (jobs.animals.breed args) in a world; run n ticks 700 ms apart; the setup map."
  [args world n]
  (let [s (h/setup world)]
    (core/submit! (:eng s) (list 'jobs.animals.breed args) {})
    (await (run-ticks s n 700))
    s))

(defn interacts [{:keys [p]}] (h/calls p "interact"))
(defn done-event [{:keys [seen]}] (first (filter #(= :breed.done (:kind %)) @seen)))
(defn events-of [{:keys [seen]} kind] (filterv #(= kind (:kind %)) @seen))
(defn finished? [{:keys [eng]}] (empty? (:list (core/state eng))))
(defn items-total [inv] (reduce + (map :count inv)))
(defn count-of [{:keys [p]} item]
  (reduce + (map :count (filter #(= item (:name %)) (js->clj (.-inventory (.self p)) :keywordize-keys true)))))

;; ------------------------------------------------------------ pure helpers

(deftest food-carried-takes-the-first-food-of-the-list
  (doseq [[inv mob expected] [[[{:name "wheat" :count 1}] "cow" "wheat"]
                              [[] "cow" nil]
                              [[{:name "potato" :count 1} {:name "carrot" :count 1}] "pig" "carrot"]
                              [[{:name "wheat" :count 1}] "pig" nil]
                              [[{:name "poppy" :count 1} {:name "dandelion" :count 1}] "bee" "dandelion"]
                              [[{:name "wheat" :count 1}] "zombie" nil]]]
    (is (= expected (animals/food-carried (tu/fake {:inventory inv}) mob)) (str mob inv))))

(deftest herd-adults-and-babies
  (let [p (tu/fake {:entities [(cow 1 5) (cow 2 2) (cow 3 3 {:baby true}) (cow 4 40) (ent 5 "pig" 1)]})
        ids (fn [es] (mapv #(.-id %) es))]
    (is (= [2 3 1] (ids (animals/herd p "cow" 16))) "nearest first, within radius, only cows")
    (is (= [2 1] (ids (animals/adults p "cow" 16))))
    (is (= [3] (ids (animals/babies p "cow" 16))))
    (is (= [2] (ids (animals/herd p "cow" 2))))))

;; ------------------------------------------------------------------ the job

(deftest feeds-two-adults
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "cow"} {:inventory (wheat 2) :entities [(cow 1 2) (cow 2 3)]} 4))]
          (is (finished? s))
          (is (= :fed (:reason (done-event s))))
          (is (= ["u1" "u2"] (:fed (done-event s))))
          (is (= 0 (count-of s "wheat")))
          (is (= ["wheat" "wheat"] (mapv #(.-item (.-args %)) (interacts s))))
          (is (empty? (events-of s :breed.gave-up))))))))

(deftest feeds-four-when-asked
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "cow" :count 4} {:inventory (wheat 4) :entities (mapv #(cow % 2) [1 2 3 4])} 8))]
          (is (= :fed (:reason (done-event s))))
          (is (= 4 (count (:fed (done-event s)))))
          (is (= 0 (count-of s "wheat"))))))))

(deftest walks-to-an-animal-out-of-reach
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "cow"} {:inventory (wheat 2) :entities [(cow 1 8) (cow 2 9)]} 6))]
          (is (pos? (count (h/calls (:p s) "moveTo"))))
          (is (= :fed (:reason (done-event s)))))))))

(deftest ends-without-feeding
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[label args world reason babies]
                [["no wheat" {:mob "cow"} {:entities [(cow 1 2) (cow 2 3)]} :no-food 0]
                 ["one adult" {:mob "cow"} {:inventory (wheat 2) :entities [(cow 1 2)]} :too-few 0]
                 ["only babies" {:mob "cow"} {:inventory (wheat 2) :entities [(cow 1 2 {:baby true}) (cow 2 3 {:baby true})]} :too-few 2]
                 ["both on cooldown" {:mob "cow"} {:inventory (wheat 2) :entities [(cow 1 2 {:cooldown true}) (cow 2 3 {:cooldown true})]} :refused 0]
                 ["unknown mob" {:mob "zombie"} {:inventory (wheat 2) :entities [(ent 1 "zombie" 2) (ent 2 "zombie" 3)]} :unknown-mob 0]
                 ["bee at night" {:mob "bee"} {:time 15000 :inventory [{:name "dandelion" :count 2}] :entities [(ent 1 "bee" 2) (ent 2 "bee" 3)]} :bees-indoors 0]
                 ["bee in rain" {:mob "bee"} {:raining true :inventory [{:name "dandelion" :count 2}] :entities [(ent 1 "bee" 2) (ent 2 "bee" 3)]} :bees-indoors 0]]]
          (let [s (await (scenario args world 4))]
            (is (finished? s) label)
            (is (= reason (:reason (done-event s))) label)
            (is (= babies (:babies (done-event s))) label)
            (is (= (items-total (:inventory world)) (items-total (js->clj (.-inventory (.self (:p s))) :keywordize-keys true))) label)))))))

(deftest unfed-reasons-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "cow"} {:inventory (wheat 2) :entities [(cow 1 2)]} 4))]
          (is (= [:too-few] (mapv :reason (events-of s :breed.gave-up))))
          (is (= :warn (:level (first (events-of s :breed.gave-up))))))))))

(deftest feeds-only-the-ready-animals
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "cow"} {:inventory (wheat 4) :entities [(cow 1 2 {:cooldown true}) (cow 2 3) (cow 3 4)]} 8))]
          (is (= :fed (:reason (done-event s))))
          (is (= ["u2" "u3"] (:fed (done-event s))))
          (is (= ["u1"] (:refused (done-event s))))
          (is (= 2 (count-of s "wheat"))))))))

(deftest gives-up-on-unreachable-animals
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "cow"} {:inventory (wheat 4) :entities [(cow 1 8) (cow 2 9) (cow 3 10)]
                                               :unreachable ["8,64,0" "9,64,0" "10,64,0"]} 5))]
          (is (finished? s))
          (is (= :unreachable (:reason (done-event s))))
          (is (empty? (interacts s)))
          (is (<= (count (h/calls (:p s) "moveTo")) 3)))))))

(deftest chooses-the-food-from-the-list
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "pig"} {:inventory [{:name "carrot" :count 2}] :entities [(ent 1 "pig" 2) (ent 2 "pig" 3)]} 4))]
          (is (= :fed (:reason (done-event s))))
          (is (= "carrot" (:food (done-event s))) "the food carried at the end")
          (is (= ["carrot" "carrot"] (mapv #(.-item (.-args %)) (interacts s)))))))))

(deftest feeds-bees-in-daylight
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "bee"} {:time 1000 :inventory [{:name "dandelion" :count 2}] :entities [(ent 1 "bee" 2) (ent 2 "bee" 3)]} 4))]
          (is (= :fed (:reason (done-event s))))
          (is (= 0 (count-of s "dandelion"))))))))

(deftest resumes-after-a-cut-mid-feed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (h/setup {:inventory (wheat 2) :entities [(cow 1 2) (cow 2 3)]})
              release (.hold (.-world p) "interact")]
          (core/submit! eng '(jobs.animals.breed {:mob "cow"}) {})
          (let [round (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (is (= 1 (count (interacts s))) "the first feed is in flight")
            (is (= {:ok true} (takeover/take! eng {:who "claude" :why "test"})))
            (await round)
            (release))
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (await (run-ticks s 4 700))
          (is (finished? s))
          (is (= :fed (:reason (done-event s))))
          (is (= ["u1" "u2"] (:fed (done-event s))))
          (is (= 0 (count-of s "wheat")) "each cow ate one wheat, none twice"))))))

;; ------------------------------------------------------------ the hand

(def axe [{:name "iron_axe" :count 1}])

(defn held-at-end [{:keys [p]}] (.-held (.self p)))
(defn call-names [{:keys [p]}] (mapv #(.-name %) (.-calls (.-world p))))
(defn args-of [{:keys [p]} name] (mapv #(js->clj (.-args %) :keywordize-keys true) (h/calls p name)))

(deftest puts-the-previous-item-back-in-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "cow"} {:self {:held "iron_axe"} :inventory (into (wheat 2) axe) :entities [(cow 1 2) (cow 2 3)]} 4))
              names (call-names s)]
          (is (= :fed (:reason (done-event s))))
          (is (= "iron_axe" (held-at-end s)))
          (is (= :restored (:hand (done-event s))))
          (is (= [{:item "iron_axe"}] (args-of s "equip")))
          (is (< (.lastIndexOf names "interact") (.lastIndexOf names "equip"))))))))

(deftest empties-the-hand-when-it-was-empty
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "cow"} {:inventory (wheat 2) :entities [(cow 1 2) (cow 2 3)]} 4))]
          (is (= :fed (:reason (done-event s))))
          (is (nil? (held-at-end s)))
          (is (= :emptied (:hand (done-event s))))
          (is (= 1 (count (h/calls (:p s) "unequip"))))
          (is (empty? (h/calls (:p s) "equip"))))))))

(deftest empties-the-hand-when-the-previous-item-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "cow"} {:self {:held "stick"} :inventory (wheat 2) :entities [(cow 1 2) (cow 2 3)]} 4))]
          (is (= :fed (:reason (done-event s))))
          (is (nil? (held-at-end s)))
          (is (= :emptied (:hand (done-event s))))
          (is (empty? (h/calls (:p s) "equip")))
          (is (= 1 (count (h/calls (:p s) "unequip")))))))))

(deftest a-full-inventory-keeps-the-food-in-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [others (mapv #(hash-map :name (str "item_" %) :count 1) (range 35))
              s (await (scenario {:mob "cow"} {:inventory (into (wheat 3) others) :entities [(cow 1 2) (cow 2 3)]} 4))]
          (is (= :fed (:reason (done-event s))))
          (is (= :full (:hand (done-event s))))
          (is (= "wheat" (held-at-end s)))
          (is (= 1 (count (events-of s :breed.hand-full))))
          (is (= :warn (:level (first (events-of s :breed.hand-full))))))))))

(deftest touches-no-hand-when-nothing-was-fed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "cow"} {:self {:held "iron_axe"} :inventory axe :entities [(cow 1 2) (cow 2 3)]} 4))]
          (is (= :no-food (:reason (done-event s))))
          (is (empty? (h/calls (:p s) "equip")))
          (is (empty? (h/calls (:p s) "unequip")))
          (is (not (contains? (done-event s) :hand)))
          (is (= "iron_axe" (held-at-end s))))))))

(deftest a-cut-job-restores-the-hand-when-it-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (h/setup {:self {:held "iron_axe"} :inventory (into (wheat 2) axe) :entities [(cow 1 2) (cow 2 3)]})
              release (.hold (.-world p) "interact")]
          (core/submit! eng '(jobs.animals.breed {:mob "cow"}) {})
          (let [round (core/tick! eng)]
            (await (js/Promise. (fn [resolve] (js/setTimeout resolve 20))))
            (is (= {:ok true} (takeover/take! eng {:who "claude" :why "test"})))
            (await round)
            (release))
          (takeover/release! eng {:who "claude" :reason "released" :held-ms 5})
          (await (run-ticks s 4 700))
          (is (finished? s))
          (is (= :fed (:reason (done-event s))))
          (is (= "iron_axe" (held-at-end s)))
          (is (= :restored (:hand (done-event s))))
          (is (= [{:item "iron_axe"}] (args-of s "equip"))))))))

;; ------------------------------------------------------------ keys, babies, refusals

(deftest no-effect-is-a-failure-for-backoff
  (is (true? (backoff/failure? "no-effect")))
  (is (false? (backoff/failure? "used"))))

(deftest an-animal-whose-id-changed-is-not-fed-twice
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (h/setup {:inventory (wheat 2) :entities [(cow 1 2) (cow 2 3)]})]
          (.override (.-world p) "interact"
                     (fn [token args impl]
                       (let [r (impl token args)]
                         (doseq [e (.. p -world -state -entities)] (set! (.-id e) (+ 100 (.-id e))))
                         r)))
          (core/submit! eng '(jobs.animals.breed {:mob "cow"}) {})
          (await (run-ticks s 4 700))
          (is (= :fed (:reason (done-event s))))
          (is (= ["u1" "u2"] (:fed (done-event s))))
          (is (= 2 (count (interacts s)))))))))

(deftest a-baby-the-sensing-missed-is-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p] :as s} (h/setup {:inventory (wheat 2) :entities [(cow 1 2) (cow 2 3)]})]
          (.override (.-world p) "interact"
                     (fn [_token _args _impl]
                       (js/Promise.resolve #js {:status "used" :consumed 1 :love false :worn 0 :leash nil :changed #js {}})))
          (core/submit! eng '(jobs.animals.breed {:mob "cow"}) {})
          (await (run-ticks s 4 700))
          (is (= [] (:fed (done-event s))))
          (is (= {"u1" :baby "u2" :baby} (:given-up (done-event s))))
          (is (= 2 (count (events-of s :breed.baby))))
          (is (= :warn (:level (first (events-of s :breed.baby))))))))))

(deftest three-refusals-end-refused-without-backoff
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "cow"} {:inventory (wheat 4)
                                               :entities [(cow 1 1 {:cooldown true}) (cow 2 2 {:cooldown true}) (cow 3 3 {:cooldown true})]} 3))]
          (is (finished? s))
          (is (= :refused (:reason (done-event s))))
          (is (empty? (events-of s :job.backoff))))))))

(deftest refusals-after-one-feeding-end-unpaired
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (await (scenario {:mob "cow"} {:inventory (wheat 4)
                                               :entities [(cow 1 1 {:cooldown true}) (cow 2 2 {:cooldown true}) (cow 3 3)]} 6))]
          (is (finished? s))
          (is (= :unpaired (:reason (done-event s))))
          (is (= ["u3"] (:fed (done-event s))))
          (is (empty? (events-of s :job.backoff))))))))
