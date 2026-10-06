(ns engine.entity-observations-test
  (:require [cljs.test :refer [deftest is]]
            [cljs.reader :as reader]
            [engine.entity-observations :as seen]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [engine.takeover :as takeover]))

(defn entity [id type uuid x]
  #js {:id id :name type :type "mob" :uuid uuid :position #js {:x x :y 64 :z 0}})
(defn sample [source entities & [online dim]]
  #js {:source source :online (if (nil? online) true online) :dimension (or dim "overworld") :entities (into-array entities)})
(defn cache [& [opts]] (seen/open (merge {:world "w" :body "Probe" :sense (constantly :seen)} opts)))

;; The body stands at (0.5 64 0.5) on open stone and faces south (+z): north (-z) is behind it (as engine.known-mobs-test).
(def body-pos {:x 0.5 :y 64 :z 0.5})

(defn mob [id type y & [x z]]
  #js {:id id :name type :type (if (#{"zombie" "creeper" "skeleton"} type) "hostile" "mob") :uuid (str type id) :height 1.8
       :position #js {:x (or x 0.5) :y y :z (or z 0.5)}})

(defn rig
  "{:p wrapped fake primitives, :per perception, :clock}: the body on open stone with the perception over it."
  [spec]
  (let [clock (atom 1000000)
        raw-p (tu/fake (merge {:self {:pos body-pos} :floor [-80 -80 80 80]} (dissoc spec :light)))
        _ (when-let [light (:light spec)] (swap! (fake/state raw-p) assoc :light light))
        per (perception/create (fake-raw/create raw-p) {:now #(deref clock)})]
    {:p (perception/wrap raw-p per) :raw raw-p :per per :clock clock}))

(defn fake-mob [id name x z] {:id id :name name :kind "hostile" :pos {:x x :y 64 :z z}})

(defn listed
  "type -> :sense of what the cache lists after one sample of entities es, over a rig."
  [{:keys [p per]} es]
  (let [bot #js {:entity #js {:id 1 :type "player" :username "Probe" :position #js {:x 0.5 :y 64 :z 0.5}}}
        c (seen/open {:world "w" :body "Probe" :sense (partial seen/sense per)
                      :known #(array-seq (.knownMobs p))})]
    (seen/observe! c (sample bot (cons (.-entity bot) es)) 0)
    (into {} (map (juxt :type :sense)) (:entities (seen/snapshot c 0)))))

(deftest a-thing-is-listed-by-perceptions-rule-line-light-cone-or-hearing
  (let [dark {[0 64 10] [0 0] [0 65 10] [0 0]}]
    (is (= {"player" :self "cow" :seen "item" :seen}
           (listed (rig {}) [(mob 2 "cow" 64 0.5 40.5) (mob 3 "item" 64 0.5 3.5)])) "in front: seen")
    (is (= {"player" :self "cow" :seen}
           (listed (rig {}) [(mob 2 "cow" 64 0.5 -3.5) (mob 3 "item" 64 0.5 -3.5)]))
        "behind: a cow is heard (and turned to), a drop makes no sound and is out of the cone")
    (is (= {"player" :self} (listed (rig {}) [(mob 2 "cow" 64 0.5 -40.5) (mob 3 "item" 64 0.5 -30.5)]))
        "behind and out of hearing")
    (is (= {"player" :self "cow" :heard}
           (listed (rig {:light dark}) [(mob 2 "cow" 64 0.5 10.5) (mob 3 "item" 64 0.5 10.5)]))
        "in a dark room in front: the cow is only heard, the drop is not made out")
    (is (= {"player" :self "cow" :heard}
           (listed (rig {:blocks (tu/box 5 64 -3 5 69 3 "stone")}) [(mob 2 "cow" 64 12.5 0.5)])) "through a wall: heard")))

(deftest hostile-mobs-come-from-the-known-mobs-memory-only
  (is (= {"player" :self} (listed (rig {:entities [(fake-mob 2 "creeper" 0.5 -2.5)]}) [(mob 2 "creeper" 64 0.5 -2.5)]))
      "a silent creeper behind the body is not perceived")
  (is (= {"player" :self "zombie" :seen}
          (listed (rig {:entities [(fake-mob 2 "zombie" 0.5 -3.5)]}) [(mob 2 "zombie" 64 0.5 -3.5)]))
      "a zombie heard behind the body is turned to")
  (is (= {"player" :self "zombie" :heard}
          (listed (rig {:entities [(fake-mob 2 "zombie" 6.5 -3.5)] :blocks (tu/box 5 64 -6 5 65 -1 "stone")})
                  [(mob 2 "zombie" 64 6.5 -3.5)]))
      "a zombie heard through a wall")
  (is (= {"player" :self "creeper" :seen}
          (listed (rig {:entities [(fake-mob 2 "creeper" 0.5 4.5)]}) [(mob 2 "creeper" 64 0.5 4.5)])))
  (is (= {"player" :self}
          (listed (rig {:entities [(fake-mob 2 "zombie" 0.5 30.5)] :light {[0 64 30] [0 0] [0 65 30] [0 0]}})
                  [(mob 2 "zombie" 64 0.5 30.5)]))
      "a zombie standing in the dark, far off, is neither seen nor heard"))

(deftest a-hostile-mob-gone-out-of-sense-is-remembered-with-its-age
  (let [{:keys [p clock] :as r} (rig {:entities [(fake-mob 2 "creeper" 0.5 4.5)]})
        bot #js {:entity #js {:id 1 :type "player" :username "Probe" :position #js {:x 0.5 :y 64 :z 0.5}}}
        c (seen/open {:world "w" :body "Probe" :sense (partial seen/sense (:per r)) :known #(array-seq (.knownMobs p))})
        creeper (mob 2 "creeper" 64 0.5 4.5)
        row #(first (filter (comp #{"creeper"} :type) (:entities (seen/snapshot c %))))]
    (seen/observe! c (sample bot [(.-entity bot) creeper]) 1000)
    (is (= :seen (:sense (row 1000))))
    ;; the creeper walks behind the body: silent, so unsensed; perception still remembers it where it was
    (swap! (fake/state (:raw r)) update :entities (fn [es] (mapv #(assoc % :pos [0.5 64 -6.5]) es)))
    (set! (.. creeper -position -z) -6.5)
    (swap! clock + 2000)
    (seen/observe! c (sample bot [(.-entity bot) creeper]) 3000)
    (is (= :remembered (:sense (row 3000))))
    (is (= 4.5 (get-in (row 3000) [:pos :z])) "the place last sensed, not its place now")
    (is (= 2000 (:age-ms (row 3000))))))

(defn listed-raw [per source entities]
  (let [c (seen/open {:world "w" :body "Probe" :sense (partial seen/sense per)})]
    (seen/observe! c (sample source entities) 0)
    (into {} (map (juxt :type :sense)) (:entities (seen/snapshot c 0)))))

(deftest without-an-eye-only-the-body-itself-is-listed
  (let [raw #js {:eye (fn [] nil) :sightTable (fn [] (js/Uint8Array. #js [0 1])) :stateAt (fn [_ _ _] 0)}
        per (perception/create raw {})
        bot #js {:entity #js {:id 1 :type "player" :username "Probe" :position #js {:x 0.5 :y 64 :z 0.5}}}]
    (is (= {"player" :self} (listed-raw per bot [(.-entity bot) (mob 2 "cow" 64 3.5 0.5)])))
    (is (= {"player" :self} (listed-raw nil bot [(.-entity bot) (mob 2 "cow" 64 3.5 0.5)])))))

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

(deftest a-loaded-mob-that-leaves-the-senses-is-dropped-at-once
  (let [bot #js {:entity #js {:id 1 :type "player" :username "Probe" :position #js {:x 0.5 :y 64 :z 0.5}}}
        cow (mob 2 "cow" 64 12.5 0.5)
        c (seen/open {:world "w" :body "Probe" :sense (partial seen/sense (:per (rig {:blocks (tu/box 5 64 -3 5 69 3 "stone")})))})
        types (fn [t] (into #{} (map :type) (:entities (seen/snapshot c t))))]
    (seen/observe! c (sample bot [(.-entity bot) cow]) 0)
    (is (= #{"player" "cow"} (types 0)))
    ;; the cow is teleported 40 blocks behind the wall: still loaded, no longer sensed
    (set! (.-x (.-position cow)) 40.5)
    (seen/observe! c (sample bot [(.-entity bot) cow]) 1000)
    (is (= #{"player"} (types 1000)))))

(deftest an-unloaded-mob-keeps-its-last-observation
  (let [bot #js {:entity #js {:id 1 :type "player" :username "Probe" :position #js {:x 0.5 :y 64 :z 0.5}}}
        c (seen/open {:world "w" :body "Probe" :sense (partial seen/sense (:per (rig {})))})]
    (seen/observe! c (sample bot [(.-entity bot) (mob 2 "cow" 64 12.5 0.5)]) 0)
    (seen/observe! c (sample bot [(.-entity bot)]) 1000)
    (is (= #{"player" "cow"} (into #{} (map :type) (:entities (seen/snapshot c 1000)))))))

;; ---- a sound gives a rough direction and a band, never a place
(defn rows-of [r es]
  (let [bot #js {:entity #js {:id 1 :type "player" :username "Probe" :position #js {:x 0.5 :y 64 :z 0.5}}}
        c (seen/open {:world "w" :body "Probe" :sense (partial seen/sense (:per r))
                      :known #(array-seq (.knownMobs (:p r)))})]
    (seen/observe! c (sample bot (cons (.-entity bot) es)) 0)
    (into {} (map (juxt :type identity)) (:entities (seen/snapshot c 0)))))

(deftest a-heard-only-sheep-has-a-direction-and-a-band-but-no-position
  (let [rows (rows-of (rig {:blocks (tu/box 5 64 -3 5 69 3 "stone")}) [(mob 2 "sheep" 64 12.5 0.5)])
        sheep (get rows "sheep")]
    (is (= :heard (:sense sheep)))
    (is (not (contains? sheep :pos)) "a heard sheep has no exact position")
    (is (= :east (:direction sheep)))
    (is (= :far (:band sheep)))
    (is (= :near (:band (get (rows-of (rig {:blocks (tu/box 3 64 -3 3 69 3 "stone")}) [(mob 2 "sheep" 64 6.5 0.5)]) "sheep"))))
    (is (= :north-west (:direction (get (rows-of (rig {:blocks (tu/box -6 64 -9 -6 69 -2 "stone")}) [(mob 2 "sheep" 64 -8.5 -7.5)]) "sheep"))))))

(deftest a-seen-sheep-keeps-its-exact-position
  (let [sheep (get (rows-of (rig {}) [(mob 2 "sheep" 64 0.5 10.5)]) "sheep")]
    (is (= :seen (:sense sheep)))
    (is (= {:x 0.5 :y 64 :z 10.5} (:pos sheep)))
    (is (not (contains? sheep :direction)))))
