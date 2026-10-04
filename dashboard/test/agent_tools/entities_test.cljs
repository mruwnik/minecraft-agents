(ns agent-tools.entities-test
  (:require [cljs.test :refer [deftest is testing]]
            [agent-tools.entities :as entities]
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
      (finally (close)))))

(deftest compact-query-centres-on-self-includes-players-and-uses-real-ages
  (let [now 1700000000000
        rows [(entity "self" "player" "overworld" {:x 0.45 :y 64 :z 0.52} now 100 119900 {:self? true :username "ProbeMove"})
              (entity "old-self" "player" "the_nether" {:x 100 :y 64 :z 0} now 5000 115000 {:self? true :username "ProbeMove"})
              (entity "cow" "cow" "overworld" {:x 3.24 :y 64 :z 0.54} now 1234 118766)
              (entity "player" "player" "overworld" {:x 7 :y 64 :z 0} now 850 119150 {:username "Alex"})
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
    (is (= "no cached self position; pass --center X,Y,Z after the body observes itself"
           (.-message (try (entities/project request (snapshot now rows)) nil (catch :default e e)))))
    (is (= true (:ok (entities/project (assoc request :center {:x 0 :y 64 :z 0} :dimension "overworld")
                                       (snapshot now rows)))))
    (is (= :dimension-origin-mismatch
           (error-reason #(entities/project (assoc request :dimension "the_nether")
                                            (snapshot now [(entity "self" "player" "overworld" {:x 0 :y 64 :z 0} now 1 119999 {:self? true})])))))))
