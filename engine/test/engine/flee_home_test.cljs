(ns engine.flee-home-test
  "A chased body near its own closed hut runs home and shuts the door when it gets there before the chasers (Juniper,
  2026-10-08: in its doorway with a zombie outside, the flight's straight lines ran out past the zombie and the body
  died far from the hut). A zombie standing at the door is not run into."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.hut-shelter-test :as hs]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.cost :as cost]
            [jobs.lib.reach :as reach]))

;; The hut of hut-shelter-test: walls x 3..7, z -2..2, roof y 67, room x 4..6 z -1..1, a door at (5,64,2) facing south.

(def home {:x 5 :y 64 :z 0})

(def open-door
  {"5,64,2" {:open true :half "lower" :facing "south"} "5,65,2" {:open true :half "upper" :facing "south"}})

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/seeing-all (tu/fake-on-floor world))
        now (tu/act-clock clock p 1000)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now now
                          :events (events/make {:body "Fake" :sinks [sink] :now now})})]
    (mem/write! (:store eng) :home {:pos home} {:cap 1 :ttl :forever})
    {:eng eng :p p :seen seen}))

(defn ^:async flee! [world]
  (let [{:keys [eng] :as s} (setup world)]
    (core/submit! eng '(jobs.survival.retreat) {})
    (await (tu/run-until-empty eng 6))
    s))

(defn zombie [x z chase] (cond-> {:id 7 :uuid "u7" :name "zombie" :kind "hostile" :pos {:x x :y 64 :z z} :visible true}
                           chase (assoc :chase {:speed chase})))

(def hut-floor
  "The fake lays no floor under columns a test built in: the hut's own floor, under the room and the door."
  (tu/box 3 63 -2 7 63 2 "cobblestone"))

(defn hut
  ([pos states entities] (hut pos states entities {}))
  ([pos states entities extra]
   (assoc (hs/hut-world pos (merge hut-floor extra)) :states states :entities entities :floor [-40 -40 40 40])))

(defn body [p] (fake/vec-pos (:pos (fake/self p))))

(defn in-room? [[x _ z]] (and (<= 4 x 7) (<= -1 z 2)))

(defn walk-targets [eng] (mapv #(select-keys % [:x :z]) (tu/walked-to eng)))

(deftest the-race-to-a-refuge-is-won-with-a-margin
  (are [body-dist mob-dists first?] (= first? (cost/reach-first? body-dist mob-dists))
    2 [3] true
    6.5 [13] true
    8 [3] false
    8 [] true
    5 [3 30] false))

(deftest a-hut-is-a-closed-room-once-its-doors-are-shut-and-open-ground-is-not
  (let [p (tu/seeing-all (tu/fake-on-floor (hut {:x 5.5 :y 64 :z 0.5} open-door [])))]
    (is (true? (reach/closed-room? p home)) "an open door counts as one that can be shut")
    (is (false? (reach/closed-room? p {:x 15 :y 64 :z 0})) "the open field outside")
    (is (false? (reach/closed-room? (tu/seeing-all (tu/fake-on-floor (hut {:x 5.5 :y 64 :z 0.5} {} []
                                                                          {"5,64,2" "air" "5,65,2" "air"})))
                                    home))
        "a doorway with no door")))

(deftest a-body-in-its-doorway-with-a-zombie-outside-goes-in-and-shuts-the-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (flee! (hut {:x 5.5 :y 64 :z 2.5} open-door [(zombie 5.5 4.6 0.4)])))]
          (is (in-room? (body p)) (str "inside the hut: " (body p)))
          (is (false? (hs/door-open? p)) "the door is shut on the zombie")
          (is (every? #(<= (:z %) 2) (walk-targets eng)) (str "never out past the door: " (walk-targets eng)))
          (is (= 1 (count (filter #(= :retreat.home (:kind %)) @seen))) "says once it runs home")
          (is (= [] (:list (core/state eng))) "the shut door ends the flight"))))))

(deftest a-body-outside-with-a-zombie-far-behind-runs-home-through-its-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (await (flee! (hut {:x 5.5 :y 64 :z 6.5} hs/door-states [(zombie 5.5 13.5 0.4)])))]
          (is (in-room? (body p)) (str "inside the hut: " (body p)))
          (is (false? (hs/door-open? p)) "the door is shut behind it")
          (is (= [] (:list (core/state eng)))))))))

(deftest a-zombie-at-the-door-is-not-run-into
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (hut {:x 5.5 :y 64 :z 9.5} hs/door-states [(zombie 5.5 3.5 nil)]))]
          (core/submit! eng '(jobs.survival.retreat) {})
          (await (core/tick! eng))
          (is (> (:z (first (walk-targets eng))) 9) (str "the first step is not toward the zombie: " (walk-targets eng)))
          (is (not (in-room? (body p)))))))))

(deftest a-zombie-between-the-body-and-its-door-is-gone-round-and-the-body-gets-in
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (flee! (hut {:x 5.5 :y 64 :z 12.5} hs/door-states [(zombie 5.5 7.5 0.4)])))]
          (is (in-room? (body p)) (str "inside the hut: " (body p)))
          (is (false? (hs/door-open? p))))))))
