(ns engine.dusk-home-test
  "The :dusk-home trigger: before night, a body whose bed or home is a walk away leaves for it in time."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.job-api :as job-api]
            [engine.shelter-support :as st]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [triggers.survival.dusk-home :as dh]))

(defn holds?
  "The trigger over a fake body at (0 64 0) at time-of-day t, with places [[kind x z] ..] remembered."
  [t places & [over]]
  (let [store (mem/open (tu/tmp-dir) {:now (constantly 1000)})]
    (doseq [[kind x z y] places]
      (mem/write! store kind {:pos {:x x :y (or y 64) :z z}} mem/place-policy))
    (dh/dusk-home (tu/seeing-all (tu/fake-on-floor (merge {:floor [-4 -4 4 4] :time t :self {:pos {:x 0.5 :y 64 :z 0.5}}} over)))
                  (mem/view store) {})))

(deftest leaves-when-the-walk-takes-as-long-as-the-day-has-left
  (are [t places expected] (= expected (holds? t places))
    1000 [[:home 100 0]] false
    11000 [[:home 100 0]] true
    11000 [[:bed 100 0]] true
    11000 [[:home 20 0]] false
    11000 [[:home 150 0]] true
    11000 [[:home 300 0]] false
    11000 [[:home 900 0]] false
    11000 [] false))

(deftest it-does-not-fire-at-night-or-once-there
  (are [t places expected] (= expected (holds? t places))
    13000 [[:home 100 0]] false
    11000 [[:home 2 0]] false))

(def hut
  "A closed stone hut around the body at (0 64 0): 3x3 inside, roof at y 67."
  (apply dissoc (tu/box -2 64 -2 2 67 2 "stone") (keys (tu/box -1 64 -1 1 66 1 "stone"))))

(deftest a-body-underground-or-shut-in-a-hut-has-no-reason-to-go-home
  (are [blocks] (false? (holds? 11000 [[:home 100 0]] {:blocks blocks}))
    {"0,66,0" "stone"}
    hut))

(deftest a-body-under-a-lone-overhead-block-still-goes-home
  (is (true? (holds? 11000 [[:home 100 0]] {:floor [-20 -20 20 20] :blocks {"0,67,0" "stone"}}))))

(def stair-home-world
  "A roof over the body at x 0 and a home at x 60, y 70, up a stair from x 55: the body is in the ground (6 under home)."
  {:time 11500 :floor [-20 -20 120 20]
   :blocks (into {"0,67,0" "stone"} (for [k (range 1 7)] [(str (+ 54 k) "," (+ 63 k) ",0") "stone"]))})

(defn ^:async go-home-from-the-ground
  "[blocks the body ends from the home at x, why go-home gave up] with the home at x y."
  [x y args]
  (let [{:keys [eng p seen]} (st/setup stair-home-world)]
    (mem/write! (:store eng) :home {:pos {:x x :y y :z 0}} mem/place-policy)
    (core/submit! eng (list 'jobs.survival.go-home args) {})
    (await (tu/run-until-empty eng 30))
    [(js/Math.abs (- x (.-x (.-pos (.self p))))) (some #(when (#{:completed :stopped} (:kind %)) (:reason %)) @seen)]))

(deftest go-home-in-the-ground-stays-when-the-quoted-walk-there-and-back-costs-more-than-sheltering-here
  ;; the trigger fires for a buried body too; go-home, called with :commute?, prices go-to's own quote
  (is (true? (holds? 11500 [[:home 60 0 70]] stair-home-world)))
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [far (await (go-home-from-the-ground 60 70 {:commute? true}))
              near (await (go-home-from-the-ground 6 64 {:commute? true}))
              plain (await (go-home-from-the-ground 60 70 {}))]
          (is (> (first far) 50) "the far home is not walked to")
          (is (= :shelter-cheaper (second far)))
          (is (< (first near) 3) "a close home is walked to")
          (is (< (first plain) 3) "without :commute? the call walks home"))))))

(deftest go-home-walks-to-the-home-and-waits-with-none
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (st/setup {:time st/noon :floor [-30 -30 30 30]})
              waiting (fn [world know]
                        (let [{:keys [eng]} (st/setup world)]
                          (when know (mem/write! (:store eng) :home {:pos {:x 12 :y 64 :z 0}} mem/place-policy))
                          (let [id (core/submit! eng '(jobs.survival.go-home) {})]
                            (core/tick! eng)
                            (:reason (:waiting (job-api/summary eng id))))))]
          (is (= :no-home (waiting {:time st/noon} false)))
          (mem/write! (:store eng) :home {:pos {:x 12 :y 64 :z 0}} mem/place-policy)
          (core/submit! eng '(jobs.survival.go-home) {})
          (await (tu/run-until-empty eng 30))
          (is (< (js/Math.abs (- 12 (.-x (.-pos (.self p))))) 3) "the body stands within 2 of the home"))))))

(deftest go-home-beyond-its-limit-waits-too-far
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup {:time st/noon :floor [-30 -30 30 30]})]
          (mem/write! (:store eng) :home {:pos {:x 300 :y 64 :z 0}} mem/place-policy)
          (do (core/submit! eng '(jobs.survival.go-home) {})
            (await (tu/run-until-empty eng 10))
            (is (= [:too-far] (distinct (map :reason (filter #(= :waiting (:kind %)) @seen)))))
            (is (= 1 (count (:list (core/state eng)))) "the order waits in the list, once")))))))

(deftest go-home-arriving-at-the-bed-records-it-as-home
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (st/setup {:time st/noon :floor [-30 -30 30 30]})]
          (mem/write! (:store eng) :bed {:pos {:x 12 :y 64 :z 0}} mem/place-policy)
          (core/submit! eng '(jobs.survival.go-home) {})
          (await (tu/run-until-empty eng 30))
          (is (= {:x 12 :y 64 :z 0} (mem/place (mem/view (:store eng)) :home))))))))

(deftest the-home-due-setting-retunes-the-trigger
  (is (true? (holds? 11000 [[:home 150 0]])))
  (settings/with-settings {:jobs.lib.shelter/home-due {:margin-s 0 :speed 4.3 :slack 0 :arrive 4}}
    #(is (false? (holds? 11000 [[:home 150 0]])))))
