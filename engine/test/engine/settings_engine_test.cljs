(ns engine.settings-engine-test
  "The engine family's tuning numbers: each keeps today's default and answers to a settings layer."
  (:require [cljs.test :refer [deftest is]]
            [engine.args :as a]
            [engine.backoff :as backoff]
            [engine.entity-observations :as obs]
            [engine.event-api :as event-api]
            [engine.events :as events]
            [engine.hurt :as hurt]
            [engine.lease :as lease]
            [engine.perception :as perception]
            [engine.senses :as senses]
            [engine.settings :as settings]))

(def families
  {:lease lease/settings :events events/settings :event-api event-api/settings :backoff backoff/settings
   :hurt hurt/settings :entities obs/settings :perception perception/settings})

(def defaults-then
  "The values the constants had before they became settings."
  {:engine.lease/max-ms 10000 :engine.lease/max-idle-s 3600 :engine.lease/default-release-ms 1000
   :engine.lease/default-idle-ms 15000
   :engine.events/max-bytes 67108864 :engine.events/recent-count 2048 :engine.events/recent-bytes 4194304
   :engine.events/default-page-size 256 :engine.events/max-page-size 2000
   :engine.event-api/max-body-bytes 16384 :engine.event-api/max-limit 1000 :engine.event-api/status-job-limit 4
   :engine.event-api/attention-limit 4 :engine.event-api/catalog-page-limit 20
   :engine.event-api/max-catalog-page-limit 64 :engine.event-api/catalog-doc-limit 1200
   :engine.event-api/inventory-stack-limit 46 :engine.event-api/enchant-limit 8
   :engine.backoff/after 3 :engine.backoff/first-s 1 :engine.backoff/max-s 30 :engine.backoff/moved-min 1
   :engine.backoff/walk-moved-min 8
   :engine.hurt/window-ms 1000
   :engine.entities/ttl-ms 120000 :engine.entities/sample-ms 1000 :engine.entities/max-entities 10000
   :engine.entities/max-snapshot-bytes 4194304
   :engine.perception/radius 48 :engine.perception/fov 70 :engine.perception/aspect (/ 16 9)
   :engine.perception/ray-deg 1 :engine.perception/pass-ms 1500 :engine.perception/step-ms 50
   :engine.perception/idle-ms 3000 :engine.perception/still-ms 30000 :engine.perception/move-blocks 0.5
   :engine.perception/turn-deg 2 :engine.perception/seeing-min 0.2 :engine.perception/near 4
   :engine.perception/near-torch 7 :engine.perception/cap-bytes 33554432 :engine.perception/stats-ms 60000
   :engine.perception/error-every-ms 60000 :engine.perception/mob-ms 250 :engine.perception/mob-scan 64
   :engine.perception/hearing 16 :engine.perception/dark-sight 4 :engine.perception/mob-drift 16
   :engine.perception/mutable-max-ms 10000})

(def all-specs (apply merge (vals families)))

(deftest every-moved-value-keeps-its-old-default
  (is (= defaults-then (into {} (map (fn [[k spec]] [k (:default spec)])) (dissoc all-specs :engine.perception/save-ms)))))

(deftest every-key-is-declared-in-its-own-family-namespace-and-documented
  (doseq [[k spec] all-specs]
    (is (some? (namespace k)))
    (is (seq (:doc spec)) (str k " has a doc"))
    (is (nil? (a/problem k (:default spec))) (str k " default fits its spec"))))

(deftest a-layer-overrides-each-key-through-get
  (doseq [[fam specs] families [k spec] specs :let [v (inc (:default spec))]]
    (settings/with-settings {k v}
      (fn [] (is (= v (settings/get specs k)) (str fam " " k))))))

(deftest lease-limits-follow-the-settings
  (is (nil? (lease/ms-error 10000)))
  (settings/with-settings {:engine.lease/max-ms 50}
    (fn []
      (is (some? (lease/ms-error 51)))
      (is (nil? (lease/ms-error 50)))))
  (settings/with-settings {:engine.lease/max-idle-s 10}
    (fn [] (is (false? (lease/valid-idle-s? 11))))))

(deftest events-paging-follows-the-settings
  (is (= 256 (events/bounded-page-size nil)))
  (settings/with-settings {:engine.events/default-page-size 7 :engine.events/max-page-size 9}
    (fn []
      (is (= 7 (events/bounded-page-size nil)))
      (is (= 9 (events/bounded-page-size 500))))))

(deftest backoff-defaults-and-neutral-moves-follow-the-settings
  (is (= {:after 3 :first-s 1 :max-s 30} (backoff/defaults)))
  (settings/with-settings {:engine.backoff/after 5 :engine.backoff/max-s 60 :engine.backoff/moved-min 3}
    (fn []
      (is (= {:after 5 :first-s 1 :max-s 60} (backoff/defaults)))
      (is (= {:after 5 :first-s 1 :max-s 60} (backoff/config nil {})))
      (is (false? (backoff/neutral? :moveTo "failed" 2)))
      (is (true? (backoff/neutral? :moveTo "failed" 3))))))

(deftest hurt-window-follows-the-settings
  (is (= 1000 (:window-ms @(hurt/new-state))))
  (settings/with-settings {:engine.hurt/window-ms 40}
    (fn [] (is (= 40 (:window-ms @(hurt/new-state)))))))

(deftest perception-defaults-follow-the-settings
  (is (= 48 (:radius (perception/defaults))))
  (is (= 60000 (:save-ms (perception/defaults))))
  (settings/with-settings {:engine.perception/radius 20 :engine.perception/save-ms 5000}
    (fn []
      (is (= 20 (:radius (perception/defaults))))
      (is (= 5000 (:save-ms (perception/defaults)))))))

(deftest vanilla-sensing-facts-are-constants-not-settings
  (is (= {:hit-range 6 :monster-range 8 :monster-height 5}
         {:hit-range senses/hit-range :monster-range senses/monster-range :monster-height senses/monster-height})))

(deftest events-max-bytes-is-validated-on-its-setting
  (doseq [ok [1024 4096 67108864]]
    (is (nil? (a/problem :engine.events/max-bytes ok)) (str ok)))
  (doseq [bad [1023 0 -1 1.5 "4096" nil]]
    (is (some? (a/problem :engine.events/max-bytes bad)) (str (pr-str bad)))))

(deftest events-make-rejects-a-cap-below-the-minimum
  (is (thrown? js/Error (events/make {:file "/nonexistent/x.ndjson" :max-bytes 1023})))
  (is (thrown? js/Error (events/make {:file "/nonexistent/x.ndjson" :max-bytes 1.5}))))
