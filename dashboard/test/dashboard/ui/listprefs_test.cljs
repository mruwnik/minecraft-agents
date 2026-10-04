(ns dashboard.ui.listprefs-test
  (:require [cljs.test :refer [deftest is are]]
            [dashboard.ui.listprefs :as prefs]))

(def defaults {:chat-open? true :places-open? true :players-open? false})

(deftest no-stored-text-gives-the-defaults
  (are [text] (= defaults (prefs/open-flags defaults text))
    nil "" "not json" "[1,2]" "null" "42" "{"))

(deftest stored-booleans-override-the-defaults-per-list
  (are [text expected] (= expected (prefs/open-flags defaults text))
    "{\"chat\":false}" (assoc defaults :chat-open? false)
    "{\"players\":true,\"places\":false}" {:chat-open? true :places-open? false :players-open? true}
    "{\"chat\":false,\"places\":false,\"players\":true}" {:chat-open? false :places-open? false :players-open? true}))

(deftest rubbish-values_and_unknown_lists_are_ignored
  (are [text] (= defaults (prefs/open-flags defaults text))
    "{\"chat\":\"no\",\"places\":1,\"players\":null}"
    "{\"sidebar\":false}"))

(deftest the-stored-text-round-trips-the-flags
  (let [flags {:chat-open? false :places-open? false :players-open? true}]
    (is (= flags (prefs/open-flags defaults (prefs/stored-text flags))))))

(deftest the-stored-text-names-lists-not-db-keys
  (is (= {"chat" false "places" true "players" true}
         (js->clj (js/JSON.parse (prefs/stored-text {:chat-open? false :places-open? true :players-open? true}))))))
