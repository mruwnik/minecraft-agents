(ns engine.known-mobs-test
  "The danger checks take only the mobs the body knows of (card 80f25a40): engine.perception's mob memory (seen in the
  view cone or heard, remembered while likely still near) feeds jobs.lib.reach's dangers, nearest-danger and the
  hostile-near trigger; reach reads blocks from the raw world's state ids when there is one; the mobs of one query share
  their walk proofs."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [jobs.lib.danger :as danger-q]
            [jobs.lib.look :as look]
            [jobs.lib.reach :as reach]
            [jobs.lib.reach.room :as reach-room]
            [jobs.lib.reach.proofs :as proofs]
            [engine.perception :as perception]
            [engine.perception.mobs :as mobs]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.survival.recover-drops :as recover-drops]))

;; The body stands at (0.5 64 0.5) on open stone and faces south (+z): north (-z) is behind it.

(def body {:x 0.5 :y 64 :z 0.5})

(defn mob [id name x z] {:id id :name name :kind "hostile" :pos {:x (+ x 0.5) :y 64 :z (+ z 0.5)}})

(defn rig
  "{:p wrapped primitives, :per perception, :raw p, :clock}: a fake on a floor with the perception over it, its clock in ms."
  ([spec] (rig spec {}))
  ([spec opts]
   (let [clock (atom 1000000)
         raw-p (tu/fake (merge {:self {:pos body} :floor [-50 -50 50 50]} spec))
         per (perception/create (fake-raw/create raw-p) (merge {:now #(deref clock)} opts))]
     {:p (perception/wrap raw-p per) :per per :raw raw-p :clock clock})))

(defn move! [p id {:keys [x y z]}]
  (swap! (fake/state p) update :entities (fn [es] (mapv #(if (= id (:id %)) (assoc % :pos [x y z]) %) es))))
(defn remove-mob! [p id] (swap! (fake/state p) update :entities (fn [es] (filterv #(not= id (:id %)) es))))
(defn later! [clock ms] (swap! clock + ms))
(defn sightings [p] (mapv #(vector (.-id %) (.-name %) (.-x (.-pos %)) (.-z (.-pos %)) (.-ageMs %)) (.fadedMobs p)))

(defn danger-ids [p] (mapv #(.-id %) (danger-q/dangers p 8 {:ranged-radius 16} {})))
(defn nearest-id [p] (some-> (danger-q/nearest-danger p 8 {:ranged-radius 16} {}) .-id))
(defn hostile-near? [p] ((:when (:hostile-near triggers/all)) p {} (:args (:hostile-near triggers/all))))

(def hidden-spot
  "A creeper's hiding place east of the body, round the corner of a stone wall: no line from the eye, but a way round."
  {:x 6.5 :y 64 :z -3.5})

(def wall
  "Stone two high at x 5, z -6..-1: hides hidden-spot from the body; open both ends, so a walker goes round."
  (tu/box 5 64 -6 5 65 -1 "stone"))

;; ---- what counts

(deftest an-unseen-silent-creeper-behind-the-body-is-no-danger
  (let [{:keys [p raw]} (rig {:entities [(mob 1 "creeper" 0 -2)]})]
    (is (true? (.-visible (first (.entities raw #js {:kind "hostile"})))) "in the clear: a turn would show it")
    (is (= [] (danger-ids p)))
    (is (nil? (nearest-id p)))
    (is (false? (hostile-near? p)) "the trigger does not fire")
    (is (nil? (danger-q/nearest-danger p 8 {} {:sight? false})) "unknown even without the sight test")))

(deftest a-sight-pass-facing-a-silent-creeper-remembers-it-after-the-body-turns-away
  (let [{:keys [p]} (rig {:entities [(mob 1 "creeper" 0 -2)]})]
    (swap! (fake/state p) assoc :yaw 180)
    (look/see! {:primitives p})
    (swap! (fake/state p) assoc :yaw 0)
    (is (= [1] (danger-ids p)))))

(deftest a-glance-is-skipped-where-a-known-enderman-stands
  (let [{:keys [p per]} (rig {:entities [(mob 1 "enderman" 0 -6)]})]
    (swap! (fake/state p) assoc :yaw 180)
    (look/see! {:primitives p})
    (swap! (fake/state p) assoc :yaw 0)
    (is (true? (mobs/enderman-at-yaw? per 0)) "north, where it was seen")
    (is (false? (mobs/enderman-at-yaw? per js/Math.PI)) "south")))

(deftest the-raw-primitives-still-count-every-mob
  (let [{:keys [raw]} (rig {:entities [(mob 1 "creeper" 0 -2)]})]
    (is (= [1] (danger-ids raw)) "no perception: every tracked mob (the old rule)")))

(deftest a-creeper-in-front-is-seen-and-a-danger
  (let [{:keys [p]} (rig {:entities [(mob 1 "creeper" 0 4)]})]
    (is (= [1] (danger-ids p)))
    (is (true? (hostile-near? p)))))

(deftest a-zombie-heard-behind-the-body-is-turned-to-and-counts
  (let [{:keys [p]} (rig {:entities [(mob 1 "zombie" 0 -3)]})]
    (is (= [1] (danger-ids p)))
    (is (true? (.-seen (first (.knownMobs p)))))))

(deftest a-zombie-heard-through-a-wall-is-known-not-seen-and-counts
  (let [{:keys [p]} (rig {:entities [(mob 1 "zombie" 6 -3)] :blocks wall})
        [m] (.knownMobs p)]
    (is (true? (.-heard m)))
    (is (false? (.-seen m)))
    (is (= [1] (danger-ids p)) "a heard melee mob with a way round the wall counts (card 99f833e8)")))

(deftest a-creeper-seen-three-seconds-ago-round-a-corner-still-counts
  (let [{:keys [p clock]} (rig {:entities [(mob 1 "creeper" 2 4)] :blocks wall})]
    (is (= [1] (danger-ids p)) "seen in front")
    (later! clock 3000)
    (move! p 1 hidden-spot)
    (let [[m] (.knownMobs p)]
      (is (true? (.-remembered m)))
      (is (= 2.5 (.-x (.-pos m))) "at the place it was last seen"))
    (is (= [1] (danger-ids p)))
    (is (true? (hostile-near? p)))))

(deftest a-creeper-seen-three-minutes-ago-is-forgotten
  (let [{:keys [p clock]} (rig {:entities [(mob 1 "creeper" 2 4)] :blocks wall})]
    (is (= [1] (danger-ids p)))
    (move! p 1 hidden-spot)
    (later! clock (* 3 60 1000))
    (is (= [] (danger-ids p)))
    (is (= 0 (alength (.knownMobs p))))
    (is (= [[1 "creeper" 2.5 4.5 180000]] (sightings p)) "kept as a sighting where it was last seen")
    (later! clock (* 12 60 1000))
    (is (= [[1 "creeper" 2.5 4.5 900000]] (sightings p)) "still a sighting at 15 min")
    (later! clock (* 105 60 1000))
    (is (= [[1 "creeper" 2.5 4.5 7200000]] (sightings p)) "and at 2 h: under the cap the jobs' price decides")))

(deftest sightings-past-the-cap-drop-the-oldest
  (let [{:keys [p clock]} (rig {:entities [(mob 1 "creeper" 2 4)] :blocks wall} {:max-sightings 2})]
    (is (= [1] (danger-ids p)))
    (move! p 1 hidden-spot)
    (later! clock (* 3 60 1000))
    (is (= [[1 "creeper" 2.5 4.5 180000]] (sightings p)))
    (swap! (fake/state p) update :entities conj {:id 2 :name "creeper" :kind "hostile" :pos [2.5 64 4.5]} {:id 3 :name "creeper" :kind "hostile" :pos [-1.5 64 4.5]})
    (is (= [2 3] (danger-ids p)))
    (move! p 2 hidden-spot)
    (move! p 3 hidden-spot)
    (later! clock (* 3 60 1000))
    (is (= [2 3] (mapv first (sightings p))) "the oldest went")))

(deftest a-mob-lost-out-of-sight-is-a-sighting-until-sensed-again
  (let [{:keys [p clock]} (rig {:entities [(mob 1 "creeper" 2 4)] :blocks wall})]
    (is (= [1] (danger-ids p)))
    (move! p 1 hidden-spot)
    (later! clock 3000)
    (is (= [] (sightings p)) "remembered, not yet a sighting")
    (remove-mob! p 1)
    (later! clock 1000)
    (is (= [[1 "creeper" 2.5 4.5 4000]] (sightings p)) "the client stopped tracking it out of sight: a player would not know it is gone")
    (swap! (fake/state p) update :entities conj (assoc (mob 1 "creeper" 2 4) :pos [2.5 64 4.5]))
    (is (= [1] (danger-ids p)))
    (is (= [] (sightings p)) "sensed again: known, no sighting")))

(deftest a-mob-seen-far-off-long-ago-is-no-danger
  (let [{:keys [p clock]} (rig {:entities [(mob 1 "creeper" 0 40)]})]
    (is (= 1 (alength (.knownMobs p))) "seen 40 blocks off")
    (later! clock (* 3 60 1000))
    (move! p 1 hidden-spot)
    (is (= [] (danger-ids p)))
    (is (false? (hostile-near? p)))))

(deftest a-mob-the-client-stops-tracking-is-dropped
  (let [{:keys [p]} (rig {:entities [(mob 1 "zombie" 0 4)]})]
    (is (= [1] (danger-ids p)))
    (remove-mob! p 1)
    (is (= 0 (alength (.knownMobs p))))
    (is (= [] (sightings p)) "it vanished in sight: dead or despawned")
    (is (= [] (danger-ids p)))))

(deftest recover-drops-weighs-only-known-hostiles
  (let [{:keys [p]} (rig {:entities [(mob 1 "creeper" 0 -2) (mob 2 "zombie" 0 5)]})]
    (is (= [2] (mapv #(.-id %) (danger-q/seen-hostiles p))))))

;; ---- blocks from state ids

(def sealed
  "Walls two high on the eight cells around (0 64 0) and a roof: the body is sealed in."
  (into {"0,66,0" "stone"} (for [x [-1 0 1] z [-1 0 1] :when (not= 0 x z) y [64 65]] [(str x "," y "," z) "stone"])))

(defn with-raw-world
  "Fake primitives p with a rawWorld (the fake's raw world reader) and a blockAt that counts its calls."
  [p]
  (let [calls (atom 0)
        q (js/Object.create p)]
    (set! (.-rawWorld q) (fake-raw/create p))
    (set! (.-blockAt q) (fn [pos] (swap! calls inc) (.blockAt p pos)))
    [q calls]))

(deftest the-searches-read-state-ids-from-the-raw-world
  (let [mobs [(mob 1 "zombie" 6 0) (mob 2 "zombie" 0 6)]
        [open-p open-calls] (with-raw-world (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :entities mobs}))
        [sealed-p sealed-calls] (with-raw-world (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks sealed :entities mobs}))]
    (is (= [1 2] (sort (map #(.-id %) (danger-q/dangers open-p 8 {} {:sight? false})))))
    (is (= [] (danger-q/dangers sealed-p 8 {} {:sight? false})))
    (is (false? (reach-room/enclosed? open-p :raw)))
    (is (= 0 @open-calls @sealed-calls) "no blockAt")))

;; ---- proofs shared between the mobs of one query

(def pit
  "A pit 3 deep at (6 64 0), walled on its four sides: a mob in it has no way out; one on the rim may drop in."
  (into {} (for [[x z] [[5 0] [7 0] [6 1] [6 -1]] y [64 65 66]] [(str x "," y "," z) "stone"])))

(deftest a-mob-that-can-drop-into-a-dead-pit-still-walks-to-the-body
  (let [rim (assoc (mob 2 "zombie" 6 0) :pos {:x 6.5 :y 67 :z 1.5} :visible true)
        p (tu/fake {:self {:pos body} :floor [-20 -20 20 20]
                    :blocks (merge pit (tu/box 4 64 1 8 66 3 "stone"))
                    :entities [(assoc (mob 1 "zombie" 6 0) :visible true) rim]})]
    (is (false? (reach/walkable-way? p {:x 6.5 :y 64 :z 0.5} body)) "the pit is closed")
    (is (true? (reach/walkable-way? p {:x 6.5 :y 67 :z 1.5} body)) "the rim walks down")
    (is (= [2] (mapv #(.-id %) (danger-q/dangers p 8 {} {:sight? false})))
        "the pit's cells, proved dead by the first mob, do not hide the second mob's way")))

(deftest the-mobs-of-one-sealed-pocket-share-one-proof
  (let [pocket (merge (tu/box -6 64 4 6 66 10 "stone") (tu/box -5 64 5 5 65 9 "air"))
        mobs (for [i (range 6)] (mob (inc i) "zombie" (- i 3) 7))
        p (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks pocket :entities mobs})
        pr (danger-q/query-proofs p)
        kind-at (reach/lookup p)]
    (is (every? false? (map #(reach/walkable-way? p (:pos %) body) mobs)) "each is walled in")
    (is (false? (proofs/proved-way? kind-at pr [-3 64 7] [0 64 0])))
    (let [dead (.-size (.-dead pr))]
      (is (pos? dead))
      (is (false? (proofs/proved-way? kind-at pr [2 64 7] [0 64 0])) "settled by the proof")
      (is (= dead (.-size (.-dead pr))) "no new cell searched"))))

;; ---- light (card 01b5462f): a mob in the dark is seen only close up

(defn dark! [p cells] (swap! (fake/state p) assoc :light cells))
(defn dark-around! [p mob-x mob-z]
  (dark! p {[mob-x 64 mob-z] [0 0] [mob-x 65 mob-z] [0 0]}))
(defn known [p] (vec (.knownMobs p)))

(deftest a-silent-creeper-in-the-dark-beyond-the-threshold-is-not-seen
  (let [{:keys [p]} (rig {:entities [(mob 1 "creeper" 0 8)]})]
    (dark-around! p 0 8)
    (is (= [] (known p)))
    (is (= [] (danger-ids p)))))

(deftest a-silent-creeper-in-the-dark-within-the-threshold-is-seen
  (let [{:keys [p]} (rig {:entities [(mob 1 "creeper" 0 3)]})]
    (dark-around! p 0 3)
    (is (= [1] (danger-ids p)))
    (is (true? (.-seen (first (known p)))))))

(deftest a-creeper-in-the-dark-is-seen-to-seven-blocks-with-a-torch-in-hand
  (let [{:keys [p]} (rig {:entities [(mob 1 "creeper" 0 6)]})]
    (dark-around! p 0 6)
    (is (= [] (danger-ids p)))
    (swap! (fake/state p) update :self merge {:offhand "torch"})
    (is (= [1] (danger-ids p)))))

(deftest a-creeper-lit-by-a-torch-in-the-dark-is-seen-far
  (let [{:keys [p]} (rig {:entities [(mob 1 "creeper" 0 8)]})]
    (dark! p {[0 64 8] [0 14] [0 65 8] [0 14]})
    (is (= [1] (danger-ids p)))))

(deftest a-zombie-in-the-dark-is-heard-not-seen
  (let [{:keys [p]} (rig {:entities [(mob 1 "zombie" 0 8)]})]
    (dark-around! p 0 8)
    (let [m (first (known p))]
      (is (some? m) "heard within 16 as before")
      (is (false? (.-seen m)))
      (is (true? (.-heard m))))))

(deftest a-seen-mob-that-walks-into-the-dark-stays-remembered
  (let [{:keys [p clock]} (rig {:entities [(mob 1 "creeper" 0 8)]})]
    (is (= [1] (danger-ids p)) "seen in the light")
    (dark-around! p 0 8)
    (later! clock 1000)
    (let [m (first (known p))]
      (is (some? m))
      (is (true? (.-remembered m)))
      (is (true? (.-seen m))))))

;; ---- cards 99f833e8, 271c2a45: heard melee mobs count; a fusing creeper hisses

(deftest a-zombie-heard-in-the-dark-with-a-way-is-a-danger
  (let [{:keys [p]} (rig {:entities [(mob 1 "zombie" 0 8)]})]
    (dark-around! p 0 8)
    (is (= [1] (danger-ids p)))
    (is (true? (hostile-near? p)))))

(def glass-box
  "A zombie boxed in glass at (0 64 8): seen through it, with no way to the body."
  (concat (tu/box -1 64 7 1 65 7 "glass") (tu/box -1 64 9 1 65 9 "glass")
          (tu/box -1 64 7 -1 65 9 "glass") (tu/box 1 64 7 1 65 9 "glass")))

(deftest a-zombie-seen-in-a-sealed-glass-box-is-no-danger
  (let [{:keys [p]} (rig {:entities [(assoc (mob 1 "zombie" 0 8) :visible true)] :blocks glass-box})]
    (is (= [] (danger-ids p)))))

(deftest a-fusing-creeper-behind-the-body-is-heard-and-a-danger
  (let [{:keys [p]} (rig {:entities [(assoc (mob 1 "creeper" 0 -3) :fusing true)]})]
    (is (= [1] (danger-ids p)))
    (is (some? (first (known p))) "known although behind the body")))

(deftest a-fusing-creeper-out-of-hearing-is-unknown
  (let [{:keys [p]} (rig {:entities [(assoc (mob 1 "creeper" 0 -20) :fusing true)]})]
    (is (= [] (known p)))))

(deftest an-idle-creeper-behind-the-body-stays-unknown
  (let [{:keys [p]} (rig {:entities [(assoc (mob 1 "creeper" 0 -3) :fusing false)]})]
    (is (= [] (known p)))))

;; ---- card 10a36e29: a heard-only mob that ended a flight blocked is muted, growing; sight still fires

(defn muted-memory
  "A memory view at now holding one :heard-mute entry that lasts until the given ms."
  [now until]
  {:now now :data (mem/add-entry mem/empty-data :heard-mute {:t 0 :data {:until until}} {:cap 8 :ttl :forever})})

(defn near-with-memory? [p memory] ((:when (:hostile-near triggers/all)) p memory (:args (:hostile-near triggers/all))))

(deftest a-muted-heard-mob-does-not-fire-the-trigger-until-the-mute-ends
  (let [{:keys [p]} (rig {:entities [(mob 1 "zombie" 6 -3)] :blocks wall})]
    (is (true? (near-with-memory? p {:now 10 :data mem/empty-data})) "no mute set: a heard melee mob with a way counts")
    (is (false? (near-with-memory? p (muted-memory 100 5000))))
    (is (true? (near-with-memory? p (update (muted-memory 100 5000) :data mem/add-entry :hurt {:t 50 :data {}} {:cap 8 :ttl :forever})))
        "hurt since the mute began: the unseen mob may be the one hitting")
    (is (true? (near-with-memory? p (muted-memory 6000 5000))) "the mute is over")))

(deftest a-seen-mob-fires-the-trigger-through-a-mute
  (let [{:keys [p]} (rig {:entities [(mob 1 "zombie" 0 4)]})]
    (is (true? (near-with-memory? p (muted-memory 100 5000))))))

;; ---- card d56da453: a creeper that follows the body up its own dark stair and fuses behind it

(def stair-rock
  "Rock round a stair the body cut up to its feet cell (0 64 0): steps k = 1..4 have feet (0 64-k -k), clear three
  high; no light anywhere."
  (merge (tu/box -2 56 -7 2 68 2 "stone")
         (into {} (for [k (range 0 5) y (range (- 64 k) (- 67 k))] [(str "0," y "," (- k)) "air"]))))

(deftest a-fusing-creeper-behind-on-the-dark-stair-fires-the-trigger
  (doseq [k [1 2] visible [nil false]
          :let [why (str k " steps below, line " (if (false? visible) "blocked" "clear"))
                {:keys [p]} (rig {:entities [(cond-> {:id 1 :name "creeper" :kind "hostile" :pos {:x 0 :y (- 64 k) :z (- k)} :fusing true}
                                               (some? visible) (assoc :visible visible))]
                                  :blocks stair-rock :light-default [0 0]})]]
    (is (some? (first (known p))) why)
    (is (= [1] (danger-ids p)) why)
    (is (true? (hostile-near? p)) why)))


(deftest a-fusing-creeper-beyond-swell-range-or-a-quiet-one-in-rock-is-no-danger
  (let [far (:p (rig {:entities [(assoc (mob 1 "creeper" 0 -12) :fusing true :visible false)] :light-default [0 0]}))
        quiet (:p (rig {:entities [{:id 1 :name "creeper" :kind "hostile" :pos {:x 0 :y 62 :z -2} :fusing false :visible false}]
                        :blocks stair-rock :light-default [0 0]}))]
    (is (false? (.-seen (first (known far)))) "heard, not seen, 12 blocks away")
    (is (= [] (mapv #(.-id %) (danger-q/dangers far 16 {} {:ignore-heard? true}))) "heard beyond swell range: muted like any heard mob")
    (is (= [1] (mapv #(.-id %) (danger-q/dangers (:p (rig {:entities [(assoc (mob 1 "creeper" 0 -5) :fusing true :visible false)] :light-default [0 0]})) 16 {} {:ignore-heard? true}))) "within swell range: not muted")
    (is (= [] (danger-ids quiet)))
    (is (false? (hostile-near? quiet)))))
