(ns agent-tools.world-data-test
  (:require [cljs.test :refer [deftest is are async]]
            [agent-tools.world-data :as data]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]))

(defn fixture-ctx []
  (let [dir (.mkdtempSync fs (.join path (os/tmpdir) "world-data-"))]
    (.mkdirSync fs (.join path dir "worlds" "test") #js {:recursive true})
    (.writeFileSync fs (.join path dir "worlds" "test" "world.json") "{}")
    (.mkdirSync fs (.join path dir "blueprints"))
    {:dir dir :ctx (data/context {:state dir :world "test" :repo-root dir})}))

(deftest read-changes-leaves-an-unchanged-ledger-alone
  (async done
    (let [{:keys [dir ctx]} (fixture-ctx)
          ledger (.join path (:metadata-dir ctx) "changes.edn")
          mtime #(.-mtimeMs (.statSync fs ledger))]
      (-> (data/read-changes ctx)
          (.then (fn [_]
                   (.utimesSync fs ledger 1 1)
                   (data/read-changes ctx)))
          (.then (fn [_] (is (= 1000 (mtime)) "no write when the scan found nothing new")))
          (.catch (fn [e] (is (nil? e) (str e))))
          (.finally (fn []
                      (.rmSync fs dir #js {:recursive true :force true})
                      (done)))))))

(defn message-of [thunk]
  (try (thunk) nil (catch :default e (.-message e))))

(deftest limit-messages-keep-their-spaces
  (are [thunk expected] (= expected (message-of thunk))
    #(data/query [] {:limit 101}) "limit 1..100 and nonnegative offset required"
    #(data/query [] {:limit 1 :center [0]}) "center needs [x y z], radius nonnegative"))
