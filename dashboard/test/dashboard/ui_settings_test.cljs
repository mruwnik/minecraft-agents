(ns dashboard.ui-settings-test
  (:require [cljs.test :refer [deftest is]]
            [dashboard.ui.settings :as settings]))

(def view {:game-rate 40 :rate-source :packet :frozen? false :physics-ms 50
           :settings {:b/x {:value 1 :layer :code} :a/y {:value 2 :layer :body} :a/z {:value 3 :layer :world}}})

(deftest only-keys-off-their-default-are-listed-sorted
  (is (= [{:key :a/y :value 2 :layer :body} {:key :a/z :value 3 :layer :world}] (settings/changed-rows view)))
  (is (= [] (settings/changed-rows nil))))

(deftest the-rate-line-names-the-source-and-a-freeze
  (is (= "40 ticks/s (packet), physics step 50 ms" (settings/rate-text view)))
  (is (= "20 ticks/s (assumed), physics step 50 ms, frozen" (settings/rate-text {:game-rate 20 :rate-source :assumed :physics-ms 50 :frozen? true})))
  (is (nil? (settings/rate-text nil))))

(deftest the-set-request-parses-both-drafts-or-says-why-not
  (is (= {:op :set :key :a/b :value 5000} (settings/set-request ":a/b" "5000")))
  (is (= {:op :set :key :a/b :value "x"} (settings/set-request ":a/b" "\"x\"")))
  (is (string? (:error (settings/set-request "a/b" "1"))))
  (is (string? (:error (settings/set-request ":a/b" "{:x")))))

(deftest loading-another-bodys-settings-drops-the-previous-view
  (let [db {:detail-body "B" :settings {:body "A" :view view :error "e" :key ":a/b"}}]
    (is (= {:body "B"} (:settings (settings/start-load db))))
    (is (= (:settings db) (:settings (settings/start-load (assoc db :detail-body "A")))))))
