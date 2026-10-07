(ns engine.game-rate-test
  (:require [cljs.test :refer [deftest is use-fixtures]]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            ["node:events" :as events]))

(defn new-clock [] (.createGameClock (tu/require-here "./js/game-clock.mjs")))
(defn new-client [] (new events/EventEmitter))

(use-fixtures :each {:before #(settings/set-clock! 20 false :assumed)
                     :after #(settings/set-clock! 20 false :assumed)})

(deftest no-packet-means-20-assumed
  (is (= 20 (settings/game-rate)))
  (is (false? (settings/frozen?)))
  (is (= :assumed (settings/rate-source))))

(deftest wiring-a-clock-without-a-packet-emits-one-assumed-info
  (let [events (atom [])
        gc (new-clock)]
    (settings/wire-game-clock! gc #(swap! events conj %))
    (is (= 20 (settings/game-rate)))
    (is (= [:info] (mapv :level @events)))
    (is (= "game-rate.assumed" (name (:kind (first @events)))))))

(deftest a-packet-before-wiring-is-read-at-once-without-an-event
  (let [events (atom [])
        gc (new-clock)
        client (new-client)]
    (.attach gc client)
    (.emit client "set_ticking_state" #js {:tick_rate 40 :is_frozen false})
    (settings/wire-game-clock! gc #(swap! events conj %))
    (is (= 40 (settings/game-rate)))
    (is (= :packet (settings/rate-source)))
    (is (empty? @events))))

(deftest later-packets-update-the-rate-and-frozen
  (let [gc (new-clock)
        c (new-client)]
    (.attach gc c)
    (settings/wire-game-clock! gc (fn [_]))
    (.emit c "set_ticking_state" #js {:tick_rate 40 :is_frozen true})
    (is (= 40 (settings/game-rate)))
    (is (true? (settings/frozen?)))
    (is (= :packet (settings/rate-source)))))

(deftest ticks-and-ms-follow-the-live-rate
  (is (= 6000 (settings/ms->ticks 300000)))
  (is (= 300000 (settings/ticks->ms 6000)))
  (settings/set-clock! 40 false :packet)
  (is (= 150000 (settings/ticks->ms 6000)) "at 40 TPS the same ticks are half the time")
  (is (= 12000 (settings/ms->ticks 300000))))

(deftest physics-ms-reads-the-primitives-else-50
  (is (= 50 (settings/physics-ms #js {})))
  (is (= 25 (settings/physics-ms #js {:physicsMs (fn [] 25)}))))

(deftest the-perception-save-interval-is-an-engine-setting
  (settings/with-settings {:engine.perception/save-ms 1000}
    (fn []
      (is (= 1000 (settings/get settings/settings :engine.perception/save-ms)))))
  (is (= 60000 (settings/get settings/settings :engine.perception/save-ms))))
