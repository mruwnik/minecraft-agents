(ns world-test.respond-calls-test
  (:require [cljs.test :refer [deftest is]]
            [world-test.respond-calls :as r]))

(defn line [kind job t] (pr-str {:generation-id "g" :source :reflex :kind kind :context {:job-id job :reflex-id :hostile-near} :time-ms t}))

(deftest a-call-lasts-from-its-firing-to-its-end
  (is (= {:key ["g" "j1"] :t 1000 :edge :fired} (r/scan-line (line :fired "j1" 1000))))
  (is (nil? (r/scan-line (pr-str {:kind :fired :context {:job-id "j2" :reflex-id :burning} :time-ms 0}))) "another reflex")
  (is (nil? (r/scan-line (line :cut-off "j1" 1000))))
  (let [{:keys [n deciles]} (r/report (keep r/scan-line (concat (mapcat (fn [i] [(line :fired (str "j" i) 0) (line :ended (str "j" i) (* 1000 (inc i)))]) (range 10))
                                                                [(line :fired "lone" 0)])))]
    (is (= 10 n) "a call still on is no length")
    (is (= [1 2 3 4 5 6 7 8 9 10] deciles))))
