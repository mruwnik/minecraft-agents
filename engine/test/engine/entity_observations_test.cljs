(ns engine.entity-observations-test
  (:require [cljs.test :refer [deftest is]]
            [cljs.reader :as reader]
            [engine.entity-observations :as seen]
            [engine.takeover :as takeover]))

(defn entity [id type uuid x]
  #js {:id id :name type :type "mob" :uuid uuid :position #js {:x x :y 64 :z 0}})
(defn sample [source entities & [online dim]]
  #js {:source source :online (if (nil? online) true online) :dimension (or dim "overworld") :entities (into-array entities)})
(defn cache [& [opts]] (seen/open (merge {:world "w" :body "Probe" :sense (constantly :seen)} opts)))

(defn mob [id type y & [x z]]
  #js {:id id :name type :type "mob" :uuid (str type id) :height 1.8 :position #js {:x (or x 0.5) :y y :z (or z 0.5)}})

(defn ground-world
  "A raw world: the eye at (0.5 65.62 0.5) over solid stone below y 64, open air above; `walls` cells are stone too."
  [& [walls]]
  (let [stone 1 table (js/Uint8Array. #js [0 1]) walls (set walls)]
    #js {:eye (fn [] #js {:x 0.5 :y 65.62 :z 0.5 :yaw 0 :pitch 0 :dimension "overworld"})
         :sightTable (fn [] table)
         :stateAt (fn [x y z] (if (or (< y 64) (contains? walls [x y z])) stone 0))}))

(defn listed [raw source entities]
  (let [c (seen/open {:world "w" :body "Probe" :sense (partial seen/sense raw)})]
    (seen/observe! c (sample source entities) 0)
    (into {} (map (juxt :type :sense)) (:entities (seen/snapshot c 0)))))

(deftest underground-mobs-out-of-hearing-are-not-listed
  (let [bot #js {:entity #js {:id 1 :type "player" :username "Probe" :position #js {:x 0.5 :y 64 :z 0.5}}}]
    (is (= {"player" :self "cow" :seen "zombie" :heard}
           (listed (ground-world) bot
                   [(.-entity bot)
                    (mob 2 "cow" 64 40.5 0.5)          ; on the surface 40 blocks off, nothing between
                    (mob 3 "zombie" 54)                ; 10 blocks down through rock: heard
                    (mob 4 "skeleton" 44 10.5 0.5)     ; deep under the floor: neither
                    (mob 5 "enderman" 52 20.5 0.5)
                    (mob 6 "spider" 38)
                    (mob 7 "pig" 64 70.5 0.5)])))))    ; open line but beyond sight range

(deftest a-wall-hides-a-mob-past-hearing-and-silent-drops-are-only-seen
  (let [wall (for [y (range 64 70) z (range -3 4)] [5 y z])
        bot #js {:entity #js {:id 1 :type "player" :username "Probe" :position #js {:x 0.5 :y 64 :z 0.5}}}
        drop #js {:id 9 :name "item" :type "object" :uuid "i9" :height 0.25 :position #js {:x 8.5 :y 64 :z 0.5}}]
    (is (= {"player" :self "cow" :heard}
           (listed (ground-world wall) bot [(.-entity bot) (mob 2 "cow" 64 12.5 0.5) (mob 3 "zombie" 64 30.5 0.5) drop])))
    (is (= {"player" :self "cow" :seen "zombie" :seen "item" :seen}
           (listed (ground-world) bot [(.-entity bot) (mob 2 "cow" 64 12.5 0.5) (mob 3 "zombie" 64 30.5 0.5) drop])))))

(deftest without-an-eye-only-the-body-itself-is-listed
  (let [raw #js {:eye (fn [] nil) :sightTable (fn [] (js/Uint8Array. #js [0 1])) :stateAt (fn [_ _ _] 0)}
        bot #js {:entity #js {:id 1 :type "player" :username "Probe" :position #js {:x 0.5 :y 64 :z 0.5}}}]
    (is (= {"player" :self} (listed raw bot [(.-entity bot) (mob 2 "cow" 64 3.5 0.5)])))
    (is (= {"player" :self} (listed nil bot [(.-entity bot) (mob 2 "cow" 64 3.5 0.5)])))))

(deftest loaded-entities-refresh-and-unloaded-entities-expire
  (let [c (cache) bot (js-obj) e (entity 1 "villager" "v-1" 1)]
    (seen/observe! c (sample bot [e]) 1000)
    (is (= "villager/v-1" (:key (first (:entities (seen/snapshot c 1000))))))
    (is (= 121000 (:expires-at (first (:entities (seen/snapshot c 1000))))))
    (set! (.. e -position -x) 5)
    (seen/observe! c (sample bot [e]) 60000)
    (is (= 5 (get-in (seen/snapshot c 60000) [:entities 0 :pos :x])))
    (is (= 180000 (:expires-at (first (:entities (seen/snapshot c 60000))))))
    (seen/observe! c (sample bot []) 61000)
    (is (= 1 (:count (seen/snapshot c 179999))))
    (is (= 0 (:count (seen/snapshot c 180000))))))

(deftest disconnected-or-stalled-samples-do-not-fabricate-sightings
  (let [c (cache) bot (js-obj) e (entity 1 "cow" "cow" 1)]
    (seen/observe! c (sample bot [e]) 0)
    (seen/observe! c (sample bot [e] false) 100000)
    (is (false? (:online? (seen/snapshot c 100000))))
    (is (= 0 (:observed-at (first (:entities (seen/snapshot c 100000))))))
    (is (= 0 (:count (seen/snapshot c 120000))))))

(deftest uuid-identities-survive-runtime-id-and-connection-changes
  (let [c (cache) a (js-obj) b (js-obj)
        mob-a (entity 1 "villager" "v" 1) mob-b (entity 99 "villager" "v" 2)
        player #js {:id 4 :type "player" :username "Human" :uuid "human-uuid" :position #js {:x 0 :y 64 :z 2}}]
    (seen/observe! c (sample a [mob-a player]) 0)
    (seen/observe! c (sample b [mob-b player]) 1000)
    (let [result (seen/snapshot c 1000)]
      (is (= 2 (:count result)))
      (is (= #{"villager/v" "player/human-uuid"} (set (map :key (:entities result)))))
      (is (= "Human" (:username (first (filter #(= "player" (:type %)) (:entities result))))))
      (is (= 99 (:id (first (filter #(= "villager" (:type %)) (:entities result)))))))))

(deftest own-player-uses-its-player-list-uuid-without-mutating-mineflayer
  (let [c (cache)
        self #js {:id 1 :type "player" :username "Probe" :position #js {:x 0 :y 64 :z 0}}
        bot #js {:entity self :player #js {:uuid "self-uuid"}}
        other #js {:id 2 :type "player" :username "Probe" :uuid "self-uuid" :position #js {:x 1 :y 64 :z 0}}]
    (seen/observe! c (sample bot [self]) 0)
    (let [observed (first (:entities (seen/snapshot c 0)))]
      (is (= "player/self-uuid" (:key observed)))
      (is (= :uuid (:identity observed)))
      (is (true? (:self? observed)))
      (is (nil? (.-uuid self))))
    (seen/observe! c (sample (js-obj) [other]) 1)
    (is (= 1 (:count (seen/snapshot c 1))))))

(deftest ephemeral-identity-is-body-session-connection-and-object-scoped
  (let [c (cache) d (cache) a (js-obj) b (js-obj)
        old (entity 1 "cow" nil 1) replacement (entity 1 "cow" nil 2)]
    (seen/observe! c (sample a [old]) 0)
    (let [key (:key (first (:entities (seen/snapshot c 0))))]
      (seen/observe! c (sample a [old]) 1)
      (is (= key (:key (first (:entities (seen/snapshot c 1))))))
      (seen/observe! c (sample a [replacement]) 2)
      (seen/observe! c (sample b [old]) 3)
      (seen/observe! d (sample a [old]) 0)
      (is (= 3 (:count (seen/snapshot c 3))))
      (is (not= key (:key (first (:entities (seen/snapshot d 0))))))
      (is (every? #(= :ephemeral (:identity %)) (:entities (seen/snapshot c 3)))))))

(deftest dimension-and-world-are-explicit
  (let [c (cache) other (cache {:world "other"}) bot (js-obj) e (entity 1 "pig" "pig" 1)]
    (seen/observe! c (sample bot [e] true "minecraft:overworld") 0)
    (seen/observe! c (sample bot [e] true "minecraft:the_nether") 10)
    (seen/observe! other (sample bot [e]) 10)
    (is (= ["the_nether"] (mapv :dimension (:entities (seen/snapshot c 10)))))
    (is (every? #(= "w" (:world %)) (:entities (seen/snapshot c 10))))
    (is (= "other" (:world (first (:entities (seen/snapshot other 10))))))))

(deftest explicit-death-does-not-get-resampled-from-the-old-cache
  (let [c (cache) bot (js-obj) e (entity 1 "cow" "cow" 0)]
    (seen/observe! c (sample bot [e]) 0)
    (seen/dead! c #js {:source bot :entity e :dimension "overworld"})
    (seen/observe! c (sample bot [e]) 1)
    (is (= 0 (:count (seen/snapshot c 1))))
    (seen/observe! c (sample bot [(entity 99 "cow" "cow" 1)]) 2)
    (is (= 1 (:count (seen/snapshot c 2))))))

(deftest item-removal-forgets-collected-drops-but-not-other-or-far-entities
  (let [c (cache) bot (js-obj) drop (entity 1 "item" "i1" 0) far (entity 2 "item" "i2" 40) cow (entity 3 "cow" "c1" 1)
        remove! #(seen/dead! c #js {:source bot :entity % :dimension "overworld" :removal "gone"})]
    (seen/observe! c (sample bot [drop far cow]) 0)
    (remove! drop)
    (remove! cow)
    (is (= #{"item/i2" "cow/c1"} (set (map :key (:entities (seen/snapshot c 1))))))
    (seen/observe! c (sample bot [far cow]) 2)
    (is (= 2 (:count (seen/snapshot c 2))))))

(deftest collected-stack-that-remains-is-resampled
  (let [c (cache) bot (js-obj) drop (entity 1 "item" "i1" 0)]
    (seen/observe! c (sample bot [drop]) 0)
    (seen/dead! c #js {:source bot :entity drop :dimension "overworld" :removal "collect"})
    (is (= 0 (:count (seen/snapshot c 1))))
    (seen/observe! c (sample bot [drop]) 2)
    (is (= 1 (:count (seen/snapshot c 2))))))

(deftest death-before-first-observation-suppresses-anonymous-corpses
  (let [c (cache) bot (js-obj) e (entity 1 "cow" nil 0)]
    (seen/dead! c #js {:source bot :entity e :dimension "overworld"})
    (seen/observe! c (sample bot [e]) 1)
    (is (= 0 (:count (seen/snapshot c 1))))))

(deftest snapshots-and-input-are-bounded-and-truncation-is-explicit
  (let [c (cache {:cap 2}) bot (js-obj)
        entities (mapv #(entity % "cow" (str %) %) (range 4))]
    (seen/observe! c (sample bot entities) 0)
    (let [result (seen/snapshot c 0)]
      (is (= 2 (:count result)))
      (is (true? (:truncated? result)))
      (is (= 2 (:dropped result))))
    (with-redefs [seen/max-snapshot-bytes 1100]
      (let [result (seen/snapshot c 0)]
        (is (= 0 (:count result)))
        (is (true? (:truncated? result)))))
    (is (false? (:truncated? (seen/snapshot c 120000))))))

(deftest endpoint-returns-edn-without-a-manual-lease
  (let [c (cache {:now (constantly 500)}) bot (js-obj)]
    (seen/observe! c (sample bot [(entity 1 "villager" "v" 0)]) 0)
    (let [response (takeover/handle {:seen-entities c} {} "GET" "/entities" nil "")
          body (reader/read-string (.-text response))]
      (is (= 200 (.-status response)))
      (is (= "application/edn" (.-contentType response)))
      (is (= "w" (:world body)))
      (is (= "Probe" (:body body)))
      (is (= 120000 (:ttl-ms body)))
      (is (= 1 (:count body))))
    (is (= 405 (.-status (takeover/handle {:seen-entities c} {} "POST" "/entities" nil ""))))
    (is (= 503 (.-status (takeover/handle {} {} "GET" "/entities" nil ""))))))

(deftest recorder-capability-failure-is-explicit-and-listeners-clean-up
  (doseq [provider [#js {} #js {:entityObservation (fn [] (throw (js/Error. "provider unavailable")))}]]
    (let [c (seen/start! provider {:world "w" :body "Probe"})]
      (try (is (= 503 (.-status (seen/request c "GET"))))
           (is (false? (:online? (seen/snapshot c))))
           (finally ((:stop c))))))
  (let [listeners (atom 0) bot (js-obj)
        provider #js {:entityObservation (fn [] (sample bot [(entity 1 "cow" "cow" 0)]))
                      :onEntityDeath (fn [_] (swap! listeners inc) #(swap! listeners dec))}
        c (seen/start! provider {:world "w" :body "Probe"})]
    (is (= 1 @listeners))
    (is (= 200 (.-status (seen/request c "GET"))))
    ((:stop c))
    (is (= 0 @listeners))))
