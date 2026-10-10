(ns engine.dusk-home-test
  "The :dusk-home trigger: before night, a body whose bed or home is a walk away leaves for it in time."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.expr :as expr]
            [engine.job-api :as job-api]
            [engine.shelter-test :as st]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [triggers.survival.dusk-home :as dh]))

(defn holds?
  "The trigger over a fake body at (0 64 0) at time-of-day t, with places [[kind x z] ..] remembered; args default to dh/defaults."
  [t places & [over args]]
  (let [store (mem/open (tu/tmp-dir) {:now (constantly 1000)})]
    (doseq [[kind x z] places]
      (mem/write! store kind {:pos {:x x :y 64 :z z}} mem/place-policy))
    (dh/dusk-home (tu/seeing-all (tu/fake-on-floor (merge {:floor [-200 -200 200 200] :time t :self {:pos {:x 0.5 :y 64 :z 0.5}}} over)))
                  (mem/view store) (or args dh/defaults))))

(deftest leaves-when-the-walk-takes-as-long-as-the-day-has-left
  (are [t places expected] (= expected (holds? t places))
    1000 [[:home 100 0]] false
    11000 [[:home 100 0]] true
    11000 [[:bed 100 0]] true
    11000 [[:home 20 0]] false
    11000 [[:home 400 0]] true
    11000 [[:home 900 0]] false
    11000 [] false))

(deftest it-does-not-fire-at-night-or-once-there
  (are [t places expected] (= expected (holds? t places))
    13000 [[:home 100 0]] false
    11000 [[:home 2 0]] false))

(deftest a-body-underground-has-no-reason-to-go-home
  (is (false? (holds? 11000 [[:home 100 0]]
                      {:blocks {"0,66,0" "stone"}}))))

(deftest the-trigger-args-are-valid-go-home-args
  (is (nil? (expr/problem registry/jobs (list 'jobs.survival.go-home {:limit 160})))))

(deftest go-home-walks-to-the-home-by-day-and-waits-at-night-or-with-none
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
          (is (= :night (waiting {:time st/night} true)))
          (mem/write! (:store eng) :home {:pos {:x 12 :y 64 :z 0}} mem/place-policy)
          (core/submit! eng '(jobs.survival.go-home) {})
          (await (tu/run-until-empty eng 30))
          (is (< (js/Math.abs (- 12 (.-x (.-pos (.self p))))) 3) "the body stands within 2 of the home"))))))

(deftest go-home-beyond-its-limit-warns-and-stops-too-far
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (st/setup {:time st/noon :floor [-30 -30 30 30]})]
          (mem/write! (:store eng) :home {:pos {:x 300 :y 64 :z 0}} mem/place-policy)
          (do (core/submit! eng '(jobs.survival.go-home) {})
            (await (tu/run-until-empty eng 10))
            (is (= [:too-far] (mapv :reason (filter #(= :stopped (:kind %)) @seen))))
            (is (some #(and (= :go-home.too-far (:kind %)) (= :warn (:level %))) @seen) "one warn names the distance")))))))

(deftest go-home-arriving-at-the-bed-records-it-as-home
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (st/setup {:time st/noon :floor [-30 -30 30 30]})]
          (mem/write! (:store eng) :bed {:pos {:x 12 :y 64 :z 0}} mem/place-policy)
          (core/submit! eng '(jobs.survival.go-home) {})
          (await (tu/run-until-empty eng 30))
          (is (= {:x 12 :y 64 :z 0} (mem/place (mem/view (:store eng)) :home))))))))

(deftest a-stored-entry-with-the-old-limit-args-still-fires
  (is (true? (holds? 11000 [[:home 400 0]] nil {:limit 160 :margin-s 60 :speed 4.3 :slack 1.4 :arrive 4}))
      "a key the stored args lack falls back to the trigger's default"))
