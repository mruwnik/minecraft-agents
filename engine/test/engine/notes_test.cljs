(ns engine.notes-test
  "engine.notes: the pure merge and expiry, then the store over a temp world folder, then ctx."
  (:require [cljs.test :refer [deftest is are]]
            ["fs" :as fs]
            ["path" :as path]
            [engine.notes :as notes]
            [engine.test-util :as tu]))

(defn log [x z & [more]]
  (merge {:kind :seen :what "oak_log" :pos [x 64 z] :by "A" :t 100 :until 10000} more))

(defn searched [x z what & [more]]
  (merge {:kind :searched :what what :pos [x 64 z] :r 24 :by "A" :t 100 :until 10000} more))

;; ------------------------------------------------------------------ pure

(deftest a-note-is-keyed-by-kind-what-and-geometry
  (are [note k] (= k (notes/note-key note))
    (log 1 2) [:seen "oak_log" [1 64 2]]
    (log 1 2 {:what "cow" :id "u1"}) [:seen "cow" "u1"]
    (searched 0 0 ["iron_ore" "oak_log"]) [:searched ["iron_ore" "oak_log"] [0 64 0]]))

(deftest two-bodies-seeing-one-thing-make-one-item-the-newest-kept
  (let [merged (notes/merge-notes [[(log 1 2 {:by "A" :t 100}) (log 5 5 {:by "A"})]
                                   [(log 1 2 {:by "B" :t 200})]])]
    (is (= 2 (count merged)))
    (is (= "B" (:by (first (filter #(= [1 64 2] (:pos %)) merged)))))))

(deftest a-tie-in-time-goes-to-the-lowest-body-name
  (is (= ["A"] (map :by (notes/merge-notes [[(log 1 2 {:by "B"})] [(log 1 2 {:by "A"})]])))))

(deftest an-entity-seen-in-two-places-is-one-item
  (is (= [[3 64 3]] (map :pos (notes/merge-notes [[(log 1 1 {:what "cow" :id "u1" :t 1})]
                                                   [(log 3 3 {:what "cow" :id "u1" :t 2})]])))))

(deftest expired-notes-are-not-live
  (is (= [[1 64 1]] (map :pos (notes/live [(log 1 1 {:until 5001}) (log 2 2 {:until 5000})] 5000)))))

(deftest the-bound-drops-expired-then-the-oldest
  (let [ns [(log 1 1 {:t 10}) (log 2 2 {:t 30}) (log 3 3 {:t 20}) (log 4 4 {:t 40 :until 50})]]
    (is (= [[2 64 2] [3 64 3]] (map :pos (notes/bound ns 100 2))))
    (is (= 3 (count (notes/bound ns 100 10))))))

(deftest a-file-renders-and-parses-back
  (let [ns [(log 1 1) (searched 0 0 ["oak_log"])]]
    (is (= {:value ns} (notes/parse-file (notes/render "A" ns))))))

(deftest a-bad-file-is-an-error-not-notes
  (are [text] (seq (:errors (notes/parse-file text)))
    "{:body \"A\" :notes [{:kind :seen"
    "[1 2 3]"
    "{:body \"A\"}"
    "{:body \"A\" :notes [{:kind :seen :what \"oak_log\" :pos [1 2] :t 1 :until 2}]}"
    "{:body \"A\" :notes [{:kind \"seen\" :what \"oak_log\" :pos [1 2 3] :t 1 :until 2}]}"
    "{:body \"A\" :notes [{:kind :seen :what \"oak_log\" :pos [1 2 3] :t 1}]}"))

(deftest a-searched-note-covers-near-points-for-the-names-it-lists
  (are [note x z targets expected] (= expected (notes/covers? note [x z] targets))
    (searched 0 0 ["iron_ore" "oak_log"]) 12 0 #{"oak_log"} true
    (searched 0 0 ["iron_ore" "oak_log"]) 13 0 #{"oak_log"} false
    (searched 0 0 ["oak_log"]) 0 0 #{"oak_log" "iron_ore"} false
    (log 0 0) 0 0 #{"oak_log"} false))

;; ------------------------------------------------------------------ the store over a disk

(defn world-dir [] (tu/tmp-dir))

(defn store [dir body & [more]]
  (let [said (atom [])]
    [(notes/open (merge {:world-dir dir :body body :emit #(swap! said conj %)} more)) said]))

(defn write-other! [dir body text]
  (fs/mkdirSync (path/join dir "notes") #js {:recursive true})
  (fs/writeFileSync (path/join dir "notes" (str body ".edn")) text))

(defn file-of [dir body] (path/join dir "notes" (str body ".edn")))

(deftest the-paths-go-through-one-seam
  (is (= {:dir (path/join "/w" "notes") :file (path/join "/w" "notes" "A.edn")} (notes/paths "/w" "A"))))

(deftest a-write-lands-whole-and-is-read-back-by-a-restarted-body
  (let [dir (world-dir)
        [s _] (store dir "A")]
    (is (= :ok (:status (notes/add! s 1000 [(log 1 1)]))))
    (is (= {:value [(log 1 1)]} (notes/parse-file (fs/readFileSync (file-of dir "A") "utf8"))))
    (is (= ["A.edn"] (vec (fs/readdirSync (path/join dir "notes")))) "no temp file left")
    (let [[again _] (store dir "A")]
      (is (= [(log 1 1)] (notes/all again 1000))))))

(deftest own-writes-show-at-once-and-other-bodies-are-merged
  (let [dir (world-dir)
        _ (write-other! dir "B" (notes/render "B" [(log 1 1 {:by "B" :t 50}) (log 7 7 {:by "B"})]))
        [s _] (store dir "A")]
    (notes/add! s 1000 [(log 1 1 {:t 100})])
    (is (= #{[1 64 1] [7 64 7]} (set (map :pos (notes/all s 1000)))))
    (is (= "A" (:by (first (filter #(= [1 64 1] (:pos %)) (notes/all s 1000))))))))

(deftest the-folder-is-stat-ed-at-most-every-few-seconds
  (let [dir (world-dir)
        _ (write-other! dir "B" (notes/render "B" [(log 1 1 {:by "B"})]))
        [s _] (store dir "A")]
    (is (= 1 (count (notes/all s 1000))))
    (write-other! dir "B" (notes/render "B" [(log 1 1 {:by "B"}) (log 2 2 {:by "B"}) (log 3 3 {:by "B"})]))
    (is (= 1 (count (notes/all s 2000))))
    (is (= 3 (count (notes/all s 4000))))))

(deftest a-broken-file-keeps-its-last-good-copy-with-one-warn
  (let [dir (world-dir)
        _ (write-other! dir "B" (notes/render "B" [(log 1 1 {:by "B"})]))
        [s said] (store dir "A")]
    (is (= 1 (count (notes/all s 1000))))
    (write-other! dir "B" "{:body \"B\" :notes [{:kind")
    (is (= 1 (count (notes/all s 5000))))
    (is (= 1 (count (notes/all s 9000))))
    (is (= [[:world.notes-unreadable "B" true]] (map (juxt :kind :body :kept) @said)))))

(deftest a-file-never-readable-adds-nothing-and-warns
  (let [dir (world-dir)
        _ (write-other! dir "B" "nonsense {")
        [s said] (store dir "A")]
    (is (= [] (notes/all s 1000)))
    (is (= [[:world.notes-unreadable "B" false]] (map (juxt :kind :body :kept) @said)))))

(deftest a-broken-own-file-starts-empty-and-is-moved-aside-on-the-first-write
  (let [dir (world-dir)
        _ (write-other! dir "A" "{:body \"A\" :notes [")
        [s said] (store dir "A")]
    (is (= [] (notes/all s 1000)))
    (is (= [[:world.notes-unreadable "A" false]] (map (juxt :kind :body :kept) @said)))
    (notes/add! s 1000 [(log 1 1)])
    (is (= "{:body \"A\" :notes [" (fs/readFileSync (str (file-of dir "A") ".broken") "utf8")))
    (is (= {:value [(log 1 1)]} (notes/parse-file (fs/readFileSync (file-of dir "A") "utf8"))))))

(deftest expired-notes-are-not-answered-nor-written
  (let [dir (world-dir)
        [s _] (store dir "A")]
    (notes/add! s 1000 [(log 1 1 {:until 2000}) (log 2 2)])
    (is (= [[1 64 1] [2 64 2]] (map :pos (notes/all s 1500))))
    (is (= [[2 64 2]] (map :pos (notes/all s 2500))))
    (notes/add! s 2500 [(log 3 3)])
    (is (= [[2 64 2] [3 64 3]] (map :pos (:value (notes/parse-file (fs/readFileSync (file-of dir "A") "utf8"))))))))

(deftest a-body-file-holds-at-most-cap-notes-the-oldest-dropped
  (let [dir (world-dir)
        [s _] (store dir "A" {:cap 2})]
    (notes/add! s 1000 [(log 1 1 {:t 10}) (log 2 2 {:t 30})])
    (notes/add! s 1000 [(log 3 3 {:t 20})])
    (is (= #{[2 64 2] [3 64 3]} (set (map :pos (notes/all s 1000)))))))

(deftest a-write-that-fails-warns-and-keeps-the-notes
  (let [dir (world-dir)
        _ (fs/writeFileSync (path/join dir "notes") "not a folder")
        [s said] (store dir "A")]
    (is (= :error (:status (notes/add! s 1000 [(log 1 1)]))))
    (is (= [(log 1 1)] (notes/all s 1000)))
    (is (= [:world.notes-unwritable] (map :kind @said)))))

;; ------------------------------------------------------------------ through a job's ctx

(defn ctx-over [s now]
  {:engine {:world {:notes s}} :view (fn [] {:now now :data {}})})

(deftest ctx-note-stamps-the-body-the-time-and-the-expiry
  (let [dir (world-dir)
        [s _] (store dir "A")
        c (ctx-over s 5000)]
    (notes/note! c [{:kind :seen :what "oak_log" :pos [1 64 1] :ttl-ms 1000}])
    (is (= [{:kind :seen :what "oak_log" :pos [1 64 1] :by "A" :t 5000 :until 6000}] (notes/notes c)))
    (is (= [] (notes/notes (ctx-over s 6000))))))

(deftest ctx-without-a-store-has-no-notes
  (let [c {:engine {:world {}} :view (fn [] {:now 1 :data {}})}]
    (is (= [] (notes/notes c)))
    (is (nil? (notes/note! c [{:kind :seen :what "x" :pos [0 0 0] :ttl-ms 1}])))))
