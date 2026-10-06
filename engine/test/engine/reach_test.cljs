(ns engine.reach-test
  "jobs.lib.reach: the walk searches the hostile trigger, retreat, respond-to-hostile, restore-broken and tidy run.
  Their answers, and that one query reads each block at most once (a query over several mobs shares its reads)."
  (:require [cljs.test :refer [deftest is]]
            [jobs.lib.reach :as reach]
            [engine.test-util :as tu]))

(def body {:x 0.5 :y 64 :z 0.5})

(def walls
  "Walls two high on the eight cells around the cell (0 64 0) (a mob at a corner can hit the body) and a roof: a body
  there is sealed in."
  (into {"0,66,0" "stone"} (for [x [-1 0 1] z [-1 0 1] :when (not= 0 x z) y [64 65]] [(str x "," y "," z) "stone"])))

(defn zed [id x z] {:id id :name "zombie" :kind "hostile" :pos {:x (+ x 0.5) :y 64 :z (+ z 0.5)}})

(def mobs [(zed 1 6 0) (zed 2 0 6) (zed 3 -6 0)])

(defn counting
  "[p' reads]: p' answers as primitives p does, reads an atom of {[x y z] times blockAt was asked for that cell}."
  [p]
  (let [reads (atom {})
        q (js/Object.create p)]
    (set! (.-blockAt q) (fn [pos]
                          (swap! reads update [(.-x pos) (.-y pos) (.-z pos)] (fnil inc 0))
                          (.blockAt p pos)))
    [q reads]))

(defn most-reads [reads] (apply max 0 (vals @reads)))

(deftest a-mob-on-open-ground-has-a-way-to-the-body
  (let [p (tu/fake {:self {:pos body} :floor [-20 -20 20 20]})]
    (is (true? (reach/walkable-way? p {:x 6.5 :y 64 :z 0.5} body)))))

(deftest a-sealed-body-has-no-way-from-a-mob-outside
  (let [p (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks walls})]
    (is (false? (reach/walkable-way? p {:x 6.5 :y 64 :z 0.5} body)))))

(deftest a-mob-in-a-deep-pit-has-no-way-out
  (let [pit (into {} (for [[x z] [[5 0] [7 0] [6 1] [6 -1]] y [64 65 66 67]] [(str x "," y "," z) "stone"]))
        p (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks pit})]
    (is (false? (reach/walkable-way? p {:x 6.5 :y 64 :z 0.5} body)))))

(deftest a-cell-counted-solid-closes-the-only-way
  (let [gap (dissoc walls "0,64,1" "0,65,1")
        p (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks gap})]
    (is (true? (reach/walkable-way? p {:x 0.5 :y 64 :z 6.5} body)) "through the gap")
    (is (false? (reach/walkable-way? p {:x 0.5 :y 64 :z 6.5} body #{[0 64 1] [0 65 1]})) "the gap filled")))

(deftest a-cell-counted-open-opens-a-way-through-a-seal
  (let [p (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks walls})]
    (is (true? (reach/walkable-way? p {:x 0.5 :y 64 :z 6.5} body #{} #{[0 64 1] [0 65 1]})) "the seal's side counted open")
    (is (false? (reach/walkable-way? p {:x 0.5 :y 64 :z 6.5} body #{} #{})) "sealed")))

(deftest a-step-up-and-a-drop-are-walked
  (let [step {"3,64,0" "stone" "4,64,0" "stone" "4,65,0" "stone" "5,64,0" "stone" "5,65,0" "stone" "5,66,0" "stone"}
        p (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks step})]
    (is (true? (reach/walkable-way? p {:x 5.5 :y 67 :z 0.5} body)) "down a stair of one-block steps")))

(deftest dangers-and-nearest-danger-of-mobs-that-can-reach
  (let [p (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :entities mobs})]
    (is (= [1 2 3] (sort (map #(.-id %) (reach/dangers p 8 {} {:sight? false})))))
    (is (some? (reach/nearest-danger p 8 {} {:sight? false})))
    (is (= 2 (some-> (reach/nearest-danger p 8 {} {:sight? false :skip #{1 3}}) .-id)) "skip leaves those out")))

(deftest no-danger-to-a-sealed-body
  (let [p (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks walls :entities mobs})]
    (is (= [] (reach/dangers p 8 {} {:sight? false})))
    (is (nil? (reach/nearest-danger p 8 {} {:sight? false})))))

(deftest one-walk-search-reads-each-block-once
  (let [[p reads] (counting (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks walls}))]
    (is (false? (reach/walkable-way? p {:x 6.5 :y 64 :z 0.5} body)))
    (is (= 1 (most-reads reads)))))

(deftest a-danger-query-over-several-mobs-reads-each-block-once
  (let [[p reads] (counting (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks walls :entities mobs}))]
    (is (= [] (reach/dangers p 8 {} {:sight? false})))
    (is (= 1 (most-reads reads)) "the mobs' searches share the blocks they read"))
  (let [[p reads] (counting (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks walls :entities mobs}))]
    (is (nil? (reach/nearest-danger p 8 {} {:sight? false})))
    (is (= 1 (most-reads reads)))))

(deftest enclosed-reads-each-block-once
  (let [[p reads] (counting (tu/fake {:self {:pos body} :floor [-30 -30 30 30]}))]
    (is (false? (reach/enclosed? p)))
    (is (= 1 (most-reads reads)))))

(defn ring
  "One-high ring of block name at radius 1 around the cell (6 64 0)."
  [name]
  (into {} (for [x [5 6 7] z [-1 0 1] :when (not= 0 (- x 6) z)] [(str x ",64," z) name])))

(def mob-in-ring {:x 6.5 :y 64 :z 0.5})

(deftest a-mob-hops-out-of-a-one-high-ring
  (let [p (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks (ring "stone")})]
    (is (true? (reach/walkable-way? p mob-in-ring body)))))

(deftest a-mob-cannot-jump-a-fence-ring-or-a-wall-ring
  (doseq [name ["oak_fence" "cobblestone_wall" "oak_fence_gate"]]
    (let [p (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks (ring name)})]
      (is (false? (reach/walkable-way? p mob-in-ring body)) name))))

(deftest a-fenced-in-zombie-is-no-danger-but-a-skeleton-behind-the-fence-is
  (let [skel {:id 9 :name "skeleton" :kind "hostile" :pos {:x 6.5 :y 64 :z 0.5}}
        p (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks (ring "oak_fence") :entities [(zed 1 6 0)]})
        q (tu/fake {:self {:pos body} :floor [-20 -20 20 20] :blocks (ring "oak_fence") :entities [skel]})]
    (is (= [] (reach/dangers p 8 {} {:sight? false})))
    (is (= [9] (map #(.-id %) (reach/dangers q 16 {:ranged-radius 16} {:sight? false}))))))
