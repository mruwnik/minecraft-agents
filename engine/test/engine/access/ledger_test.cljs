(ns engine.access.ledger-test
  "jobs.lib.ledger: the scaffold ledger's lifecycle as pure functions, and its place in body memory."
  (:require [cljs.test :refer [deftest is are]]
            [jobs.lib.ledger :as ledger]
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

;; ------------------------------------------------------------------ cleanup

(deftest begin-removal-marks-the-entry
  (let [l (-> [] (ledger/intend (entry a)) (ledger/confirm a) (ledger/intend (entry b)) (ledger/begin-removal a))]
    (is (= [[a :removing] [b :intent]] (mapv (juxt :cell :state) l)))))

(deftest settle-decides-each-picked-entry-from-its-cell
  (are [state found what] (= what (first (ledger/settle-entry (assoc (entry a) :state state) (world a found))))
    :placed "dirt" :keep
    :placed "stone" :dropped
    :placed "air" :dropped
    :placed nil :keep
    :intent "dirt" :keep
    :intent "air" :dropped
    :intent nil :keep
    :removing "dirt" :keep
    :removing "air" :removed
    :removing "cave_air" :removed
    :removing "stone" :dropped
    :removing nil :keep))

(deftest settle-confirms-a-kept-entry-holding-its-item-and-names-what-a-dropped-one-found
  (is (= [:keep (assoc (entry a) :state :placed)] (ledger/settle-entry (assoc (entry a) :state :removing) (world a "dirt"))))
  (is (= [:keep (assoc (entry a) :state :intent)] (ledger/settle-entry (assoc (entry a) :state :intent) (world))))
  (is (= [:dropped (assoc (entry a) :state :placed :found "stone")]
         (ledger/settle-entry (assoc (entry a) :state :placed) (world a "stone")))))

(deftest settle-leaves-entries-it-was-not-asked-about
  (let [l [(assoc (entry a) :state :removing) (assoc (entry b :job "j2") :state :placed) (assoc (entry [0 66 0]) :state :placed)]
        r (ledger/settle l #(= "j1" (:job %)) (world a "air" b "air" [0 66 0] "stone"))]
    (is (= [b] (mapv :cell (:ledger r))))
    (is (= [a] (mapv :cell (:removed r))))
    (is (= [[[0 66 0] "stone"]] (mapv (juxt :cell :found) (:dropped r))))))

(deftest the-owner-is-the-root-instance
  (are [job root] (= root (ledger/owner {:job job}))
    "j4" "j4"
    "j4/kid" "j4"
    "j4/c0/pillar" "j4"))

(deftest an-entry-belongs-to-an-instance-and-its-children
  (are [job yes?] (= yes? (ledger/of-instance? "j4" {:job job}))
    "j4" true
    "j4/kid" true
    "j41" false
    "j41/kid" false
    "j5" false))

(defn view-with-jobs [& ids]
  (let [s (store (tu/tmp-dir))]
    (doseq [id ids] (mem/write! s (mem/job-kind id) {:args {} :children {}} {:cap 1 :ttl :forever}))
    (mem/view s)))

(deftest the-owner-is-live-while-its-job-memory-exists
  (let [v (view-with-jobs "j4")]
    (is (ledger/owner-live? v {:job "j4/kid"}))
    (is (not (ledger/owner-live? v {:job "j5"})))))

(deftest select-picks-by-the-job-argument
  (let [v (view-with-jobs "j4")
        l [(entry a :job "j4/kid") (entry b :job "j5") (entry [0 66 0] :job "j41")]]
    (are [job cells] (= cells (mapv :cell (ledger/select l v job)))
      nil [b [0 66 0]]
      :all [a b [0 66 0]]
      "j4" [a]
      "j41" [[0 66 0]])))

(deftest offered-entries-are-selected-loaded-and-not-held
  (let [s (store (tu/tmp-dir))
        l [(entry a :job "j9") (entry b :job "j9") (entry [0 66 0] :job "j9")]]
    (ledger/write! s l)
    (is (= [a b] (mapv :cell (ledger/offered (mem/view s) (world a "dirt" b "air") nil))) "an unloaded cell is not offered")
    (ledger/hold! s [b])
    (is (= [a] (mapv :cell (ledger/offered (mem/view s) (world a "dirt" b "air") nil))) "a held cell is not offered")
    (is (= [] (ledger/offered (mem/view s) (world a "dirt" b "air") "j8")))))

(deftest held-cells-are-forgotten-after-a-while
  (let [clock (atom 1000)
        s (mem/open (tu/tmp-dir) {:now #(deref clock)})]
    (ledger/hold! s [a])
    (is (= #{a} (ledger/held-cells (mem/view s))))
    (swap! clock + (:ttl ledger/held-policy) 1)
    (is (= #{} (ledger/held-cells (mem/view s))))))
