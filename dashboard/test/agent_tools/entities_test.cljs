(ns agent-tools.entities-test
  (:require [cljs.test :refer [deftest is testing async]]
            [agent-tools.entities :as entities]
            [agent-tools.fake-socket :as fake]
            [agent-tools.world-data :as data]
            ["node:fs" :as fs]
            ["node:os" :as os]
            ["node:path" :as path]))

(defn fixture []
  (let [state (.mkdtempSync fs (.join path (.tmpdir os) "entities-cli-"))
        world-dir (.join path state "worlds" "w")]
    (.mkdirSync fs world-dir #js {:recursive true})
    (.writeFileSync fs (.join path world-dir "world.json") "{}\n")
    {:state state :close #(.rmSync fs state #js {:recursive true :force true})}))

(defn write-pose! [state body pose]
  (let [file (.join path state "worlds" "w" "agents" body "view" "pose.json")]
    (.mkdirSync fs (.dirname path file) #js {:recursive true})
    (.writeFileSync fs file (js/JSON.stringify (clj->js pose)))))

(defn entity
  ([key type dimension pos now age ttl]
   {:key (str type "/" key) :uuid (str "uuid-" key) :type type :dimension dimension :pos pos
    :observed-at (- now age) :expires-at (+ now ttl)})
  ([key type dimension pos now age ttl extra]
   (merge (entity key type dimension pos now age ttl) extra)))

(defn snapshot [now rows]
  {:ok true :world "w" :body "ProbeMove" :now now :ttl-ms 120000 :online? false
   :entities rows :count (count rows) :cached-count (count rows) :truncated? false :dropped 0})

(defn error-reason [run]
  (try (run) nil (catch :default error (keyword (.-reason error)))))

(deftest options-are-world-scoped-and-bounded
  (let [{:keys [state close]} (fixture)]
    (try
      (let [request (entities/options ["ProbeMove" "--world" "w" "--state" state])]
        (is (= "ProbeMove" (:body request)))
        (is (= 64 (:radius request)))
        (is (= 10 (:limit request)))
        (is (nil? (:center request))))
      (is (= :invalid-world (error-reason #(entities/options ["ProbeMove"]))))
      (is (= :invalid-limit (error-reason #(entities/options ["ProbeMove" "--world" "w" "--state" state "--limit" "51"]))))
      (is (= :invalid-center (error-reason #(entities/options ["ProbeMove" "--world" "w" "--state" state "--center" "1,2"]))))
      (is (= :invalid-center (error-reason #(entities/options ["ProbeMove" "--world" "w" "--state" state "--center" "1,,3"]))))
      (is (= :invalid-radius (error-reason #(entities/options ["ProbeMove" "--world" "w" "--state" state "--radius" ""]))))
      (finally (close)))))

(deftest compact-query-centres-on-self-includes-players-and-uses-real-ages
  (let [now 1700000000000
        rows [(entity "self" "player" "overworld" {:x 0.45 :y 64 :z 0.52} now 100 119900 {:self? true :username "ProbeMove"})
              (entity "old-self" "player" "the_nether" {:x 100 :y 64 :z 0} now 5000 115000 {:self? true :username "ProbeMove"})
              (entity "cow" "cow" "overworld" {:x 3.24 :y 64 :z 0.54} now 1234 118766 {:sense :heard})
              (entity "player" "player" "overworld" {:x 7 :y 64 :z 0} now 850 119150 {:username "Alex" :sense :seen})
              (entity "far" "zombie" "overworld" {:x 65 :y 64 :z 0} now 1 119999)
              (entity "other-dimension" "cow" "the_nether" {:x 1 :y 64 :z 1} now 1 119999)
              (entity "expired" "zombie" "overworld" {:x 1 :y 64 :z 1} now 150000 0)]
        result (entities/project {:body "ProbeMove" :radius 64 :limit 10 :raw? false}
                                 (snapshot now rows))]
    (is (= true (:ok result)))
    (is (= false (:online? result)))
    (is (= 2 (:total result)))
    (is (false? (:online? result)))
    (is (= ["uuid-cow" "uuid-player"] (mapv :uuid (:items result))))
    (is (= [3.2 64 0.5] (:pos (first (:items result)))))
    (is (= 1234 (:age-ms (first (:items result)))))
    (is (= "Alex" (:player (second (:items result)))))
    (is (= [:heard :seen] (mapv :sense (:items result))))
    (is (not (contains? (first (:items result)) :ttl-left-ms)))
    (is (not (contains? result :world)))))

(deftest explicit-center-and-type-filter-are-bounded-and-raw-keeps-server-times
  (let [now 1700000000000
        rows (into [(entity "zombie" "zombie" "overworld" {:x 10.125 :y 64 :z 1} now 12 119988)]
                   (map (fn [i] (entity (str "crowd-" i) "cow" "overworld"
                                          {:x 10.0 :y 64 :z 1} now i (- 120000 i))) (range 12)))
        result (entities/project {:body "ProbeMove" :center {:x 0 :y 64 :z 0} :dimension "overworld"
                                  :type "cow" :radius 64 :limit 3 :raw? true}
                                 (snapshot now rows))]
    (is (= 12 (:total result)))
    (is (= 3 (:returned result)))
    (is (= true (:more? result)))
    (is (= "cow" (:type (first (:items result)))))
    (is (= now (:observed-at (first (:items result)))))
    (is (= 10.0 (get-in (first (:items result)) [:pos :x])))))

(deftest no-default-origin-never-turns-into-a-worldwide-scan
  (let [now 1700000000000
        rows [(entity "cow" "cow" "overworld" {:x 0 :y 64 :z 0} now 5 119995)]
        request {:body "ProbeMove" :radius 64 :limit 10 :raw? false}]
    (is (= "no fresh self position; pass --center X,Y,Z"
           (.-message (try (entities/project request (snapshot now rows)) nil (catch :default e e)))))
    (is (= true (:ok (entities/project (assoc request :center {:x 0 :y 64 :z 0} :dimension "overworld")
                                       (snapshot now rows)))))
    (is (= :dimension-origin-mismatch
           (error-reason #(entities/project (assoc request :dimension "the_nether")
                                            (snapshot now [(entity "self" "player" "overworld" {:x 0 :y 64 :z 0} now 1 119999 {:self? true})])))))))

(deftest fresh-online-body-pose-supplies-default-origin-when-entity-cache-omits-self
  (let [{:keys [state close]} (fixture)
        now 1700000000000
        pose {:world "w" :status "online" :dimension "minecraft:overworld"
              :pos {:x 18.5 :y 69 :z 9.5} :t (- now 1000)}
        rows [(entity "near" "cow" "overworld" {:x 20.2 :y 69 :z 9} now 350 119650)
              (entity "elsewhere" "cow" "the_nether" {:x 19 :y 69 :z 9} now 250 119750)]
        live-snapshot (assoc (snapshot now rows) :online? true)
        request (entities/options ["ProbeMove" "--world" "w" "--state" state])]
    (try
      (write-pose! state "ProbeMove" pose)
      (let [result (entities/project request live-snapshot)]
        (is (= true (:ok result)))
        (is (= "overworld" (:dimension result)))
        (is (= [18.5 69 9.5] (:center result)))
        (is (= ["uuid-near"] (mapv :uuid (:items result)))))
      ;; The pose is read after the entity snapshot; a few seconds of future skew is expected.
      (write-pose! state "ProbeMove" (assoc pose :t (+ now 1000)))
      (let [centered (entities/project (assoc request :center {:x 20 :y 69 :z 9}) live-snapshot)]
        (is (= "overworld" (:dimension centered)))
        (is (= [20 69 9] (:center centered))))
      (write-pose! state "ProbeMove" (assoc pose :t (- now 90001)))
      (is (= :origin-unavailable
             (error-reason #(entities/project request live-snapshot))))
      (write-pose! state "ProbeMove" (assoc pose :world "other"))
      (is (= :origin-unavailable
             (error-reason #(entities/project request live-snapshot))))
      (write-pose! state "ProbeMove" (assoc pose :status "offline"))
      (is (= :origin-unavailable
             (error-reason #(entities/project request live-snapshot))))
      (is (= :origin-unavailable
             (error-reason #(entities/project request (snapshot now rows)))))
      (finally (close)))))

;; The transport: execute and main! take the get function (socket path, url, {:max-bytes}) as a seam.
(defn fake-get [reply]
  (let [calls (atom [])]
    [(fn [socket url {:keys [max-bytes]}]
       (swap! calls conj [socket url max-bytes])
       (js/Promise.resolve reply))
     calls]))

(defn edn-reply [value] {:status 200 :content-type "application/edn" :text (data/write-edn value)})

(deftest execute-reads-only-entities-over-the-body-control-socket
  (let [{:keys [state close]} (fixture)
        now (.now js/Date)
        rows [(entity "self" "player" "overworld" {:x 0.45 :y 64 :z 0.52} now 100 119900 {:self? true :username "ProbeMove"})
              (entity "cow" "cow" "overworld" {:x 3.24 :y 64 :z 0.54} now 1234 118766)]
        [get-fn calls] (fake-get (edn-reply (snapshot now rows)))
        request (entities/options ["ProbeMove" "--world" "w" "--state" state])]
    (async done
      (-> (entities/execute request get-fn)
          (.then (fn [result]
                   (is (= [[(.join path state "worlds" "w" "agents" "ProbeMove" "engine" "control.sock") "/entities" (+ (* 4 1024 1024) 4096)]]
                          @calls))
                   (is (= true (:ok result)))
                   (is (= ["uuid-cow"] (mapv :uuid (:items result))))))
          (.then (fn [_] (close) (done)))))))

(deftest execute-turns-bad-answers-into-failures
  (let [{:keys [state close]} (fixture)
        request (entities/options ["ProbeMove" "--world" "w" "--state" state])]
    (async done
      (-> (js/Promise.all
           (clj->js
            (for [[reply reason] [[{:status 200 :content-type "text/plain" :text "x"} :bad-response]
                                  [{:status 200 :content-type "application/edn" :text "{:ok"} :bad-response]
                                  [{:status 404 :content-type "application/edn" :text "{:ok false :reason :not-found}"} :entities-unavailable]
                                  [{:status 500 :content-type "application/edn" :text "{:x 1}"} :entities-unavailable]]]
              (let [[get-fn] (fake-get reply)]
                (-> (entities/execute request get-fn) (.then (fn [result] [reply reason result])))))))
          (.then (fn [results]
                   (doseq [[reply reason result] (js->clj results)]
                     (is (false? (:ok result)) (pr-str reply))
                     (is (= reason (:reason result)) (pr-str reply)))))
          (.then (fn [_] (close) (done)))))))

(defn run-main! [argv get-fn]
  (let [lines (atom [])]
    (-> (entities/main! argv #(swap! lines conj %) get-fn)
        (.then (fn [code] {:code code :out (apply str @lines)})))))

(deftest transport-failure-exits-2-like-the-other-tools
  (let [{:keys [state close]} (fixture)]
    (async done
      (-> (run-main! ["ProbeMove" "--world" "w" "--state" state]
                     (fn [_ _ _] (js/Promise.reject (doto (js/Error. "boom") (aset "code" "ECONNREFUSED")))))
          (.then (fn [{:keys [code]}] (is (= 2 code))))
          (.then (fn [_] (close) (done)))))))

(deftest main-validates-arguments-compactly-and-maps-transport-errors
  (let [{:keys [state close]} (fixture)
        now (.now js/Date)
        failing (fn [code] (fn [_ _ _] (js/Promise.reject (doto (js/Error. "boom") (aset "code" code)))))
        [get-fn] (fake-get (edn-reply (snapshot now [(entity "cow" "cow" "overworld" {:x 0 :y 64 :z 0} now 5 119995)])))]
    (async done
      (-> (run-main! ["ProbeMove"] get-fn)
          (.then (fn [{:keys [code out]}]
                   (is (= 2 code))
                   (is (= false (:ok (data/read-edn out))))))
          (.then (fn [_] (run-main! ["--help"] get-fn)))
          (.then (fn [{:keys [code out]}] (is (= 0 code)) (is (= entities/usage out))))
          (.then (fn [_] (run-main! ["ProbeMove" "--world" "w" "--state" state] get-fn)))
          (.then (fn [{:keys [code out]}]
                   (is (= 1 code))
                   (is (= :origin-unavailable (:reason (data/read-edn out))))))
          (.then (fn [_] (js/Promise.all (clj->js (for [code ["ECONNREFUSED" "ENOENT" "ETIMEDOUT" "ERESPONSETOOLARGE" "EACCES" "EOTHER"]]
                                                    (run-main! ["ProbeMove" "--world" "w" "--state" state] (failing code)))))))
          (.then (fn [results]
                   (is (= [:no-running-body :no-running-body :timeout :response-too-large :socket-access-denied :transport-error]
                          (mapv #(:reason (data/read-edn (:out %))) (js->clj results :keywordize-keys true))))))
          (.then (fn [_] (close) (done)))))))

(deftest main-refuses-output-over-the-byte-cap
  (let [{:keys [state close]} (fixture)
        now (.now js/Date)
        rows (into [(entity "self" "player" "overworld" {:x 0 :y 64 :z 0} now 1 119999 {:self? true})]
                   (map #(entity (str "cow-" %) "cow" "overworld" {:x 1 :y 64 :z 0} now 5 119995
                                 {:note (apply str (repeat 2000 "x"))}) (range 60)))
        [get-fn] (fake-get (edn-reply (snapshot now rows)))]
    (async done
      (-> (run-main! ["ProbeMove" "--world" "w" "--state" state "--raw" "--limit" "50"] get-fn)
          (.then (fn [{:keys [code out]}]
                   (is (= 1 code))
                   (is (= :output-too-large (:reason (data/read-edn out))))))
          (.then (fn [_] (close) (done)))))))

(deftest a-heard-row-has-a-direction-and-band-and-no-place
  (let [now 1700000000000
        rows [(entity "self" "player" "overworld" {:x 0 :y 64 :z 0} now 100 119900 {:self? true :username "ProbeMove"})
              (-> (entity "sheep" "sheep" "overworld" nil now 500 119500 {:sense :heard :direction :east :band :far})
                  (dissoc :pos))
              (entity "cow" "cow" "overworld" {:x 3 :y 64 :z 0} now 100 119900 {:sense :seen})]
        result (entities/project {:body "ProbeMove" :radius 64 :limit 10 :raw? false} (snapshot now rows))
        sheep (first (filter #(= "sheep" (:type %)) (:items result)))]
    (is (= 2 (:total result)))
    (is (= {:sense :heard :direction :east :band :far} (select-keys sheep [:sense :direction :band])))
    (is (not (contains? sheep :pos)))
    (is (= ["cow" "sheep"] (mapv :type (:items result))) "heard rows sort after those with a place")))
