(ns engine.access.ledger-test
  "engine.access.ledger: the scaffold ledger's lifecycle as pure functions, and its place in body memory."
  (:require [cljs.test :refer [deftest is are]]
            [engine.access.ledger :as ledger]
            [engine.memory :as mem]
            [engine.test-util :as tu]))

(def a [0 64 0])
(def b [0 65 0])

(defn entry [cell & {:as more}]
  (merge {:cell cell :item "dirt" :before "air" :job "j1" :purpose :pillar} more))

(defn world
  "A lookup over alternating cells and names; every other cell is not loaded (nil)."
  [& cells]
  (let [m (apply hash-map cells)]
    (fn [pos] (get m pos))))

(deftest an-intent-is-written-before-the-placement
  (is (= [(assoc (entry a) :state :intent)] (ledger/intend [] (entry a)))))

(deftest a-cell-has-at-most-one-entry
  (let [l (-> [] (ledger/intend (entry a)) (ledger/intend (entry b)) (ledger/intend (entry a :item "cobblestone")))]
    (is (= [b a] (mapv :cell l)))
    (is (= "cobblestone" (:item (ledger/entry-at l a))))))

(deftest confirm-marks-the-intent-placed
  (let [l (-> [] (ledger/intend (entry a)) (ledger/intend (entry b)) (ledger/confirm a))]
    (is (= :placed (:state (ledger/entry-at l a))))
    (is (= :intent (:state (ledger/entry-at l b))))))

(deftest confirm-of-an-unknown-cell-changes-nothing
  (let [l (ledger/intend [] (entry a))]
    (is (= l (ledger/confirm l b)))))

(deftest drop-cell-removes-the-entry
  (let [l (-> [] (ledger/intend (entry a)) (ledger/intend (entry b)) (ledger/drop-cell a))]
    (is (= [b] (mapv :cell l)))))

(deftest an-intent-is-decided-by-looking-at-the-cell
  (are [found decision] (= decision (ledger/decide (assoc (entry a) :state :intent) (world a found)))
    "dirt" :placed
    "air" :not-placed
    "short_grass" :not-placed
    "stone" :not-placed
    nil :unknown))

(deftest reconcile-settles-every-intent-it-can-see
  (let [l (-> []
              (ledger/intend (entry a))
              (ledger/intend (entry b))
              (ledger/intend (entry [0 66 0]))
              (ledger/intend (entry [5 64 5])))
        settled (ledger/reconcile l (world a "dirt" b "air" [0 66 0] "dirt"))]
    (is (= [[a :placed] [[0 66 0] :placed] [[5 64 5] :intent]] (mapv (juxt :cell :state) settled)))))

(deftest reconcile-leaves-placed-entries-alone
  (let [l (-> [] (ledger/intend (entry a)) (ledger/confirm a))]
    (is (= l (ledger/reconcile l (world a "air"))) "a placed block that is gone is cleanup's business")))

(deftest the-cells-and-a-jobs-entries
  (let [l (-> [] (ledger/intend (entry a)) (ledger/intend (entry b :job "j2")) (ledger/confirm a))]
    (is (= #{a b} (ledger/cells l)))
    (is (= [a] (mapv :cell (ledger/of-job l "j1" :pillar))))
    (is (= [] (ledger/of-job l "j1" :bridge)))))

;; ------------------------------------------------------------------ body memory

(defn store [dir] (mem/open dir {:now (constantly 1000)}))

(deftest the-ledger-lives-in-body-memory-and-survives-a-restart-between-intent-and-placement
  (let [dir (tu/tmp-dir)
        s (store dir)
        l (-> [] (ledger/intend (entry a)) (ledger/confirm a) (ledger/intend (entry b)))]
    (ledger/write! s l)
    (mem/save! s)
    (let [again (store dir)
          view (mem/view again)]
      (is (= l (ledger/open-entries view)))
      (is (= [[a :placed] [b :placed]]
             (mapv (juxt :cell :state) (ledger/reconcile (ledger/open-entries view) (world a "dirt" b "dirt"))))
          "the block went in before the restart")
      (is (= [[a :placed]]
             (mapv (juxt :cell :state) (ledger/reconcile (ledger/open-entries view) (world a "dirt" b "air"))))
          "the block never went in"))))

(deftest no-ledger-is-an-empty-one
  (is (= [] (ledger/open-entries (mem/view (store (tu/tmp-dir)))))))

(deftest the-ledger-never-expires
  (let [clock (atom 1000)
        dir (tu/tmp-dir)
        s (mem/open dir {:now #(deref clock)})]
    (ledger/write! s (ledger/intend [] (entry a)))
    (swap! clock + (* 30 24 60 60 1000))
    (mem/save! s)
    (is (= [a] (mapv :cell (ledger/open-entries (mem/view (mem/open dir {:now #(deref clock)}))))))))
