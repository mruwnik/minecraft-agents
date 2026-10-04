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
