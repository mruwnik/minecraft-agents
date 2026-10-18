(ns engine.settings-engine-test
  "The engine family's tuning numbers: each keeps today's default and answers to a settings layer."
  (:require [cljs.test :refer [deftest is]]
            [engine.args :as a]
            [engine.backoff :as backoff]
            [engine.entity-observations :as obs]
            [engine.event-api :as event-api]
            [engine.events :as events]
            [engine.hurt :as hurt]
            [engine.perception :as perception]
            [engine.senses :as senses]
            [engine.settings :as settings]))

(def families
  {:events events/settings :event-api event-api/settings :backoff backoff/settings
   :hurt hurt/settings :entities obs/settings :perception perception/settings})

(def all-specs (apply merge (vals families)))

(deftest every-key-is-declared-in-its-own-family-namespace-and-documented
  (doseq [[k spec] all-specs]
    (is (some? (namespace k)))
    (is (seq (:doc spec)) (str k " has a doc"))
    (is (nil? (a/problem k (:default spec))) (str k " default fits its spec"))))

(deftest a-layer-overrides-each-key-through-get
  (doseq [[fam specs] families [k spec] specs :when (number? (:default spec)) :let [v (inc (:default spec))]]
    (settings/with-settings {k v}
      (fn [] (is (= v (settings/get specs k)) (str fam " " k))))))

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

(deftest events-max-bytes-is-validated-on-its-setting
  (doseq [ok [1024 4096 67108864]]
    (is (nil? (a/problem :engine.events/max-bytes ok)) (str ok)))
  (doseq [bad [1023 0 -1 1.5 "4096" nil]]
    (is (some? (a/problem :engine.events/max-bytes bad)) (str (pr-str bad)))))

(deftest events-make-rejects-a-cap-below-the-minimum
  (is (thrown? js/Error (events/make {:file "/nonexistent/x.ndjson" :max-bytes 1023})))
  (is (thrown? js/Error (events/make {:file "/nonexistent/x.ndjson" :max-bytes 1.5}))))
