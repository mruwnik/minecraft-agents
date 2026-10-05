(ns engine.known-mobs-test
  "The danger checks take only the mobs the body knows of (card 80f25a40): engine.perception's mob memory (seen in the
  view cone or heard, remembered while likely still near) feeds engine.jobs.reach's dangers, nearest-danger and the
  hostile-near trigger; reach reads blocks from the raw world's state ids when there is one; the mobs of one query share
  their walk proofs."
  (:require [cljs.test :refer [deftest is]]
            [engine.fake :as fake]
            [engine.fake.raw-world :as fake-raw]
            [engine.jobs.reach :as reach]
            [engine.perception :as perception]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.survival.recover-drops :as recover-drops]))

;; The body stands at (0.5 64 0.5) on open stone and faces south (+z): north (-z) is behind it.

(def body {:x 0.5 :y 64 :z 0.5})

(defn mob [id name x z] {:id id :name name :kind "hostile" :pos {:x (+ x 0.5) :y 64 :z (+ z 0.5)}})

(defn rig
  "{:p wrapped primitives, :raw p, :clock}: a fake on a floor with the perception over it, its clock in ms."
  ([spec] (rig spec {}))
  ([spec opts]
   (let [clock (atom 1000000)
         raw-p (tu/fake (merge {:self {:pos body} :floor [-50 -50 50 50]} spec))
         per (perception/create (fake-raw/create raw-p) (merge {:now #(deref clock)} opts))]
     {:p (perception/wrap raw-p per) :raw raw-p :clock clock})))

(defn move! [p id {:keys [x y z]}]
  (swap! (fake/state p) update :entities (fn [es] (mapv #(if (= id (:id %)) (assoc % :pos [x y z]) %) es))))
(defn remove-mob! [p id] (swap! (fake/state p) update :entities (fn [es] (filterv #(not= id (:id %)) es))))
(defn later! [clock ms] (swap! clock + ms))

(defn danger-ids [p] (mapv #(.-id %) (reach/dangers p 8 {:ranged-radius 16} {})))
(defn nearest-id [p] (some-> (reach/nearest-danger p 8 {:ranged-radius 16} {}) .-id))
(defn hostile-near? [p] ((:when triggers/hostile-near) p {} (:args triggers/hostile-near)))

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
    (is (nil? (reach/nearest-danger p 8 {} {:sight? false})) "unknown even without the sight test")))

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

(deftest a-zombie-heard-through-a-wall-is-known-but-not-seen
  (let [{:keys [p]} (rig {:entities [(mob 1 "zombie" 6 -3)] :blocks wall})
        [m] (.knownMobs p)]
    (is (true? (.-heard m)))
    (is (false? (.-seen m)))
    (is (= [] (danger-ids p)) "a melee mob must have been seen")
    (is (= 1 (some-> (reach/nearest-danger p 8 {} {:sight? false}) .-id)) "heard counts without the sight test")))

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
    (is (= 0 (alength (.knownMobs p))))))

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
    (is (= [] (danger-ids p)))))

(deftest recover-drops-weighs-only-known-hostiles
  (let [{:keys [p]} (rig {:entities [(mob 1 "creeper" 0 -2) (mob 2 "zombie" 0 5)]})]
    (is (= [2] (mapv #(.-id %) (recover-drops/seen-hostiles p))))))

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
    (is (= [1 2] (sort (map #(.-id %) (reach/dangers open-p 8 {} {:sight? false})))))
    (is (= [] (reach/dangers sealed-p 8 {} {:sight? false})))
    (is (false? (reach/enclosed? open-p)))
    (is (= 0 @open-calls @sealed-calls) "no blockAt")))

;; ---- proofs shared between the mobs of one query

(def pit
  "A pit 3 deep at (6 64 0), walled on its four sides: a mob in it has no way out; one on the rim may drop in."
  (into {} (for [[x z] [[5 0] [7 0] [6 1] [6 -1]] y [64 65 66]] [(str x "," y "," z) "stone"])))

(deftest a-mob-that-can-drop-into-a-dead-pit-still-walks-to-the-body
  (let [rim (assoc (mob 2 "zombie" 6 0) :pos {:x 6.5 :y 67 :z 1.5})
        p (tu/fake {:self {:pos body} :floor [-20 -20 20 20]
                    :blocks (merge pit (tu/box 4 64 1 8 66 3 "stone"))
                    :entities [(mob 1 "zombie" 6 0) rim]})]
    (is (false? (reach/walkable-way? p {:x 6.5 :y 64 :z 0.5} body)) "the pit is closed")
    (is (true? (reach/walkable-way? p {:x 6.5 :y 67 :z 1.5} body)) "the rim walks down")
    (is (= [2] (mapv #(.-id %) (reach/dangers p 8 {} {:sight? false})))
        "the pit's cells, proved dead by the first mob, do not hide the second mob's way")))

(deftest the-mobs-of-one-sealed-pocket-share-one-proof
  (let [pocket (merge (tu/box -6 64 4 6 66 10 "stone") (tu/box -5 64 5 5 65 9 "air"))
        mobs (for [i (range 6)] (mob (inc i) "zombie" (- i 3) 7))
        p (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks pocket :entities mobs})
        pr (reach/query-proofs p)
        kind-at (reach/lookup p)]
    (is (every? false? (map #(reach/walkable-way? p (:pos %) body) mobs)) "each is walled in")
    (is (false? (reach/proved-way? kind-at pr [-3 64 7] [0 64 0])))
    (let [dead (.-size (.-dead pr))]
      (is (pos? dead))
      (is (false? (reach/proved-way? kind-at pr [2 64 7] [0 64 0])) "settled by the proof")
      (is (= dead (.-size (.-dead pr))) "no new cell searched"))))
