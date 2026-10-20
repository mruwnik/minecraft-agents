(ns engine.enter-room-test
  "jobs.survival.enter-room: walk into the home (or stay in the roofed room the body is in), shut its open door, fill its
  doorway; end for what it cannot do."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.events :as events]
            [jobs.lib.world-files :as ew]
            [engine.triggers :as triggers]
            [engine.hut-shelter-test :refer [hut-world door-states door-open?]]
            [engine.memory :as mem]
            [jobs.lib.ledger :as ledger]
            [engine.registry :as registry]
            [engine.shelter-support :as st]
            [engine.test-util :as tu]))

(def open-door-states (update-vals door-states #(assoc % :open true)))

(defn ^:async enter!
  "Run (jobs.survival.enter-room args) over world, with :home at home when given: {:eng :p :seen}."
  [world args & [home]]
  (let [{:keys [eng p seen] :as s} (st/setup (merge {:time st/night} world))]
    (when home (mem/write! (:store eng) :home {:pos home} mem/place-policy))
    (core/submit! eng (list 'jobs.survival.enter-room args) {})
    (await (tu/run-until-empty eng 30))
    s))

(deftest a-body-in-the-hut-shuts-its-open-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (enter! (assoc (hut-world {:x 5 :y 64 :z 0} {}) :states open-door-states) {}))]
          (is (false? (door-open? p)) "the door is shut")
          (is (empty? (st/calls p "place")) "nothing placed")
          (is (empty? (st/ended-because seen :door-open))))))))

(deftest a-door-that-stays-open-is-named-and-ends-the-job-door-open
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [world (assoc (hut-world {:x 5 :y 64 :z 0} {}) :states open-door-states)
              {:keys [eng p seen]} (st/setup (assoc world :time st/night))]
          (.override (.-world p) "useOn" (fn [token a impl] (impl token (clj->js {:pos {:x 3 :y 64 :z 0}}))))
          (core/submit! eng '(jobs.survival.enter-room {}) {})
          (await (tu/run-until-empty eng 30))
          (is (true? (door-open? p)) "the click did nothing")
          (is (= [{:door [5 64 2]}] (st/entries eng :night-door-failed)))
          (is (empty? (st/emitted seen :shelter.door-failed)) "the ending says it, no warn beside it")
          (is (seq (st/ended-because seen :door-open))))))))

(deftest a-forgotten-home-and-no-roof-ends-no-room
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (enter! {:floor tu/walk-floor :self {:pos {:x 0 :y 64 :z 0}}} {}))]
          (is (seq (st/ended-because seen :no-room)))
          (is (empty? (st/calls p "place"))))))))

(deftest a-body-outside-walks-into-the-home-and-shuts-the-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (enter! (assoc (hut-world {:x 5 :y 64 :z 6} {}) :states open-door-states) {} {:x 5 :y 64 :z 0}))]
          (is (< (:z (st/pos-of p)) 2) "inside the hut")
          (is (false? (door-open? p)) "the door is shut"))))))

(deftest a-listed-enter-room-waits-while-go-to-says-later
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup (assoc (hut-world {:x 5 :y 64 :z 6} {}) :time st/night
                                                  :entities [{:id 8 :name "zombie" :kind "hostile" :pos {:x 5 :y 64 :z 3} :health 20}]))]
          (mem/write! (:store eng) :home {:pos {:x 5 :y 64 :z 0}} mem/place-policy)
          (core/submit! eng '(jobs.survival.enter-room {}) {})
          (await (tu/run-until-empty eng 5))
          (is (= [:hostile-near] (distinct (map :reason (filter #(= :waiting (:kind %)) @seen)))))
          (is (empty? (filter #(= :stopped (:kind %)) @seen)) "it waits, it does not end no-walk"))))))

(def stones st/dirt-stack)

(def day {:time st/noon})

(def doorway-hut
  "The hut with its door cells open air: a roofed room with a doorway gap."
  (merge day (hut-world {:x 5 :y 64 :z 0} {"5,64,2" "air" "5,65,2" "air"}) {:states {}}))

(def holed-hut (merge day (hut-world {:x 5 :y 64 :z 0} {"5,67,0" "air"})))

(defn ^:async enter-with
  "enter! with the body carrying inventory: {:eng :p :seen}."
  [world inventory]
  (await (enter! (assoc world :inventory inventory) {})))

(deftest a-doorway-gap-is-filled-and-entered-in-the-ledger-for-the-morning
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (enter-with doorway-hut stones))]
          (is (seq (filter #(= 2 (:z (st/arg-pos %))) (st/calls p "place"))) "the doorway was filled")
          (is (seq (filter #(= :doorway (:purpose %)) (ledger/open-entries (mem/view (:store eng))))))
          (is (seq (st/entries eng :doorway-tried)))
          (is (empty? (st/ended-because seen :no-blocks))))))))

(deftest a-roof-hole-is-plugged-and-the-cell-recorded
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (enter-with holed-hut stones))
              placed (st/calls p "place")]
          (is (= [67] (distinct (map #(:y (st/arg-pos %)) placed))) "one block over the body"))))))

(deftest a-room-with-a-gap-or-hole-and-no-block-ends-no-blocks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [world [doorway-hut holed-hut]]
          (let [{:keys [p seen]} (await (enter-with world []))]
            (is (seq (st/ended-because seen :no-blocks)))
            (is (empty? (st/calls p "place")))))
        (let [{:keys [eng]} (await (enter-with doorway-hut []))]
          (is (empty? (st/entries eng :doorway-tried)) "the night, not enter-room, marks a doorway it could not fill"))))))

(deftest the-quote-is-infinite-when-the-room-cannot-be-closed-and-free-when-it-is-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [quote-of (:quote-of (get registry/jobs 'jobs.survival.enter-room))
              mean (fn ^:async mean-of [world inventory]
                     (let [{:keys [eng p]} (st/setup (assoc world :inventory inventory))
                           q (await (quote-of {:primitives p :args {} :view (fn [] (mem/view (:store eng)))
                                               :quote-call {:use {:blocks ["dirt"]}}}))]
                       (get-in q [:uses :job-time 1])))]
          (is (= 0 (await (mean (hut-world {:x 5 :y 64 :z 0} {}) []))))
          (is (= ##Inf (await (mean doorway-hut []))))
          (is (= 6 (await (mean doorway-hut stones)))))))))

(deftest a-walk-to-a-seen-roof-with-open-sides-is-not-quoted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [quote-of (:quote-of (get registry/jobs 'jobs.survival.enter-room))
              {:keys [eng p]} (st/setup {:floor tu/walk-floor :self {:pos {:x 5 :y 64 :z 12}} :inventory stones
                                         :blocks (tu/box 3 67 -2 7 67 2 "cobblestone")})
              _ (mem/write! (:store eng) :home {:pos {:x 5 :y 64 :z 0}} mem/place-policy)
              at (.-sensedAt p)
              mean (fn ^:async mean-of [] (get-in (await (quote-of {:primitives p :args {} :view (fn [] (mem/view (:store eng)))
                                                              :quote-call {:use {:blocks ["dirt"]}}}))
                                           [:uses :job-time 1]))]
          (is (= ##Inf (await (mean))))
          ;; the dark hides the sky beside the slab; the slab over the home is still seen
          (aset p "sensedAt" (fn [pos] (if (and (> (.-z pos) 2) (>= (.-y pos) 66))
                                         #js {:unknown true :pos pos}
                                         (.call at p pos))))
          (is (= ##Inf (await (mean))) "an unseen sky beside a seen roof on open sides is no reason to walk"))))))

(deftest a-home-in-anothers-zone-is-not-walked-into
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [clock (atom 1000000)
              [seen sink] (tu/legacy-capture-sink)
              world (assoc (hut-world {:x 5 :y 64 :z 6} {}) :states open-door-states :time st/night)
              p (tu/fake (merge {:offlineScale 0.0001 :floor tu/walk-floor} world))
              _ (st/dawn-after! p st/default-dawn)
              eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                                :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})
                                :world (ew/of-data {} {} [{:name "yard" :min [3 60 -2] :max [7 70 2] :owner "Miles"}])})]
          (mem/write! (:store eng) :home {:pos {:x 5 :y 64 :z 0}} mem/place-policy)
          (core/submit! eng '(jobs.survival.enter-room {}) {})
          (await (tu/run-until-empty eng 30))
          (is (seq (st/ended-because seen :no-room)))
          (is (> (:z (st/pos-of p)) 3) "stayed outside"))))))

(deftest with-no-home-the-nearest-closed-room-seen-is-entered
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (enter! (assoc (hut-world {:x 5 :y 64 :z 6} {}) :states open-door-states) {}))]
          (is (< (:z (st/pos-of p)) 2) "inside the hut")
          (is (false? (door-open? p)) "the door is shut"))))))

(deftest a-flee-entry-walks-legs-into-the-room-named-by-a-cell-and-shuts-the-door
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p]} (await (enter! (assoc (hut-world {:x 5 :y 64 :z 6} {}) :states open-door-states) {:at {:x 5 :y 64 :z 1} :flee? true}))]
          (is (< (:z (st/pos-of p)) 2) "inside the hut, no home remembered")
          (is (false? (door-open? p)) "the door is shut"))))))
