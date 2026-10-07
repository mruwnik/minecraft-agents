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

(deftest an-unchanged-poll-takes-no-global-lock-and-sees-later-edits
  (async done
    (let [{:keys [dir ctx]} (fixture-ctx)
          global-lock (.join path (:blueprint-dir ctx) ".agent-transactions.lock")
          zones (.join path (:world-dir ctx) "zones.edn")
          cursor (atom nil)]
      (-> (data/read-changes ctx)
          (.then (fn [r] (reset! cursor (:cursor r))
                   (.writeFileSync fs global-lock (str (.-pid js/process) "\nheld"))
                   (data/read-changes ctx {:cursor @cursor})))
          (.then (fn [r] (is (= [] (:items r)) "poll answered while the global lock is held")
                   (.rmSync fs global-lock)
                   (.writeFileSync fs zones "[{:name \"z1\" :min [0 0 0] :max [1 1 1]}]")
                   (data/read-changes ctx {:cursor @cursor})))
          (.then (fn [r] (is (= 1 (count (:items r))) "an external edit is still found")))
          (.catch (fn [e] (is (nil? e) (str e))))
          (.finally (fn []
                      (.rmSync fs dir #js {:recursive true :force true})
                      (done)))))))

(deftest an-edit-landing-during-a-scan-is-found-by-the-next-poll
  (async done
    (let [{:keys [dir ctx]} (fixture-ctx)
          zones (.join path (:world-dir ctx) "zones.edn")
          real-scan data/scan
          cursor (atom nil)]
      (-> (data/read-changes ctx)
          (.then (fn [r] (reset! cursor (:cursor r))
                   ;; a source change that adds no object, so the next poll scans
                   (.writeFileSync fs (.join path (:world-dir ctx) "claims.edn") "[]")
                   (set! data/scan (fn [c l]
                                     (let [scanned (real-scan c l)]
                                       (.writeFileSync fs zones "[{:name \"z1\" :min [0 0 0] :max [1 1 1]}]")
                                       scanned)))
                   (data/read-changes ctx {:cursor @cursor})))
          (.then (fn [r] (reset! cursor (:cursor r))
                   (data/read-changes ctx {:cursor @cursor})))
          (.then (fn [r] (is (= 1 (count (:items r))) "the edit made during the scan is not recorded as seen")))
          (.catch (fn [e] (is (nil? e) (str e))))
          (.finally (fn []
                      (set! data/scan real-scan)
                      (.rmSync fs dir #js {:recursive true :force true})
                      (done)))))))

(defn message-of [thunk]
  (try (thunk) nil (catch :default e (.-message e))))

(deftest limit-messages-keep-their-spaces
  (are [thunk expected] (= expected (message-of thunk))
    #(data/query [] {:limit 101}) "limit 1..100 and nonnegative offset required"
    #(data/query [] {:limit 1 :center [0]}) "center needs [x y z], radius nonnegative"))

(deftest busy-lock-held-by-a-live-process-does-not-blame-a-reaper
  (async done
    (let [dir (.mkdtempSync fs (.join path (os/tmpdir) "world-lock-"))
          file (.join path dir "x.edn")]
      (.writeFileSync fs (str file ".lock") (str (.-pid js/process) "\nother"))
      (-> (data/with-file-lock file (fn [] :ran) {:timeout-ms 50})
          (.then (fn [_] (is false "expected busy")))
          (.catch (fn [e] (is (= "busy" (.-reason e)) (.-message e))
                          (is (nil? (re-find #"reaper" (.-message e))) (.-message e))))
          (.finally (fn [] (.rmSync fs dir #js {:recursive true :force true}) (done)))))))
