(ns dashboard.server-library-test
  (:require [cljs.reader :as reader]
            [cljs.test :refer [deftest is async]]
            ["stream" :as stream]
            [dashboard.server :as server]
            [dashboard.village-data :as village-data]))

(defn response-recorder []
  (let [answer (atom {})
        response #js {}
        promise (js/Promise. (fn [resolve]
                              (aset response "end"
                                    (fn [body]
                                      (swap! answer assoc :body (reader/read-string body))
                                      (resolve @answer)))))]
    (aset response "writeHead" (fn [status headers]
                                (swap! answer assoc :status status :headers (js->clj headers))))
    {:response response :promise promise :answer answer}))

(def source "{:id \"cell\" :front :south :key {\"S\" \"stone\"} :layers [[\"S\"]]}")

(defn preview-request [content-type body]
  (let [request (stream/PassThrough.)
        response (response-recorder)]
    (set! (.-method request) "POST")
    (set! (.-headers request) #js {:host (str "127.0.0.1:" server/port)
                                 :content-type content-type})
    (server/preview! request (:response response))
    (.end request body)
    (:promise response)))

(deftest preview-accepts-native-edn-and-refuses-json
  (async done
    (-> (preview-request "application/edn" (pr-str {:source source}))
        (.then (fn [{:keys [status headers body]}]
                 (is (= 200 status))
                 (is (= "application/edn; charset=utf-8" (get headers "content-type")))
                 (is (= "cell" (:name body)))
                 (is (empty? (:errors body)))
                 (preview-request "application/json" "{\"source\":\"{}\"}")))
        (.then (fn [{:keys [status headers body]}]
                 (is (= 415 status))
                 (is (= "application/edn; charset=utf-8" (get headers "content-type")))
                 (is (= "Content-Type must be application/edn" (:error body)))
                 (preview-request "application/edn" (pr-str {:source source :stock 5}))))
        (.then (fn [{:keys [status body]}]
                 (is (= 400 status))
                 (is (string? (:error body)))))
        (.catch (fn [e] (is (nil? e) (str e))))
        (.finally done))))

(deftest villagers-route-preserves-uuid-string-keys-over-edn
  (let [uuid "12345678-aaaa-bbbb-cccc-123456789abc"
        response (response-recorder)]
    (with-redefs [server/refresh-entities! (fn [world] (is (= "a" world)))
                  server/entity-snapshot (fn [world dimension]
                                           (is (= "a" world))
                                           {:world world :dimension dimension :sources []
                                            :entities [{:uuid uuid :type "villager" :observed-at 10 :expires-at 20}]})
                  server/world-names (fn [] ["a" "b"])]
      (server/handle! #js {:url "/api/villagers?world=a"} (:response response)))
    (is (= 200 (:status @(:answer response))))
    (is (= uuid (get-in @(:answer response) [:body :villagers uuid :uuid])))
    (is (= 1 (get-in @(:answer response) [:body :count])))))

(deftest village-markers-attach-only-their-own-world
  (let [worlds [{:name "a" :places [{:name "home"}]} {:name "b" :places [{:name "home"}]}]
        villages [{:name "home" :world "a" :state "observed"}
                  {:name "home" :world "b" :state "unknown"}
                  {:name "home" :world nil :state "historical"}]]
    (with-redefs [village-data/attach-status (fn [places observations]
                                             (mapv #(assoc % :village (first observations)) places))]
      (let [[a b] (server/attach-villages worlds villages)]
        (is (= "a" (get-in a [:places 0 :village :world])))
        (is (= "b" (get-in b [:places 0 :village :world])))
        (is (= "observed" (get-in a [:places 0 :village :state])))
        (is (= "unknown" (get-in b [:places 0 :village :state])))))))

(deftest villagers-route-refuses-another-world-before-reading-entities
  (let [response (response-recorder)]
    (with-redefs [server/refresh-entities! (fn [& _] (throw (js/Error. "must not read an unknown world")))
                  server/world-names (fn [] ["a"])]
      (server/handle! #js {:url "/api/villagers?world=b"} (:response response)))
    (is (= 400 (:status @(:answer response))))
    (is (= "no world called b" (get-in @(:answer response) [:body :error])))
    (is (= "application/edn; charset=utf-8" (get-in @(:answer response) [:headers "content-type"])))))
