(ns dashboard.shared-map-test
  (:require [cljs.test :refer-macros [deftest is]]
            ["fs" :as fs] ["os" :as os] ["path" :as path]
            [dashboard.shared-map :as shared]))
(deftest canonical-zones-and-live-claims-are-displayed-without-changing-authority
  (let [dir (.mkdtempSync fs (.join path (.tmpdir os) "shared-map-"))
        zone {:name "protected" :min [1 2 3] :max [4 5 6] :owner "Alice" :allow #{:harvest}}
        claim {:id "reserved" :min [10 20 30] :max [11 21 31] :owner "Bob" :status :active :until 2000}]
    (try
      (is (= [{:name "legacy"}] (:zones (shared/read-overlays dir [{:name "legacy"}] 1000))))
      (.writeFileSync fs (.join path dir "zones.edn") (pr-str [zone]))
      (.writeFileSync fs (.join path dir "claims.edn") (pr-str [claim (assoc claim :id "old" :until 900) (assoc claim :id "released" :status :released)]))
      (let [result (shared/read-overlays dir [{:name "legacy"}] 1000)]
        (is (empty? (:errors result)))
        (is (= ["protected" "claim: reserved (Bob)"] (mapv :name (:zones result))))
        (is (= [1 4] ((juxt :x1 :x2) (first (:zones result)))))
        (is (= [:zone :claim] (mapv :kind (:zones result)))))
      (.writeFileSync fs (.join path dir "zones.edn") "[{:name \"bad\"}]")
      (let [result (shared/read-overlays dir [{:name "legacy"}] 3000)]
        (is (empty? (:zones result))) (is (seq (:errors result))))
      (finally (.rmSync fs dir #js {:recursive true :force true})))))
