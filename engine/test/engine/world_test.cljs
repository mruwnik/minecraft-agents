(ns engine.world-test
  "jobs.lib.world-files: the pure bookkeeping without a disk, then the reader over a temp dir."
  (:require [cljs.test :refer [deftest is are]]
            ["fs" :as fs]
            ["path" :as path]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as world]
            [plan.shape :as shape]
            [engine.file-sync :as fsync]))

(def wheat {:id "field" :parts [{:id "rows" :box [[0 64 0] [1 64 0]] :want {:crop "wheat"}}]})
(def hut-bp {:id "hut" :front :south :key {"S" "stone"} :layers [["S"]]})
(def huts {:id "huts" :parts [{:id "h1" :blueprint "hut" :at [5 64 5]}]})

;; ------------------------------------------------------------------ pure

(deftest a-check-is-due-every-few-seconds
  (are [checked-at now expected] (= expected (fsync/due? {:checked-at checked-at :every-ms 3000} now))
    nil 0 true
    1000 3999 false
    1000 4000 true))

(deftest only-new-and-changed-files-are-read-again
  (is (= #{"b" "c"}
         (set (fsync/stale-ids {"a" {:stamp [1 10]} "b" {:stamp [1 10]}}
                               {"a" [1 10] "b" [2 10] "c" [5 3]})))))

(deftest a-good-file-replaces-the-entry
  (let [[entries warn] (fsync/absorb {"field" {:stamp [1 1] :value {:old true} :error "x"}} "field" [2 2] {:value wheat})]
    (is (= {"field" {:stamp [2 2] :value wheat}} entries))
    (is (nil? warn))))

(deftest a-broken-edit-keeps-the-last-good-copy-and-warns-once
  (let [[entries warn] (fsync/absorb {"field" {:stamp [1 1] :value wheat}} "field" [2 2] {:errors ["unreadable EDN: eof"]})]
    (is (= wheat (get-in entries ["field" :value])))
    (is (= "unreadable EDN: eof" (get-in entries ["field" :error])))
    (is (= {:id "field" :error "unreadable EDN: eof" :kept true} warn))))

(deftest a-file-never-readable-is-broken-not-absent
  (let [[entries warn] (fsync/absorb {} "field" [1 1] {:errors ["a" "b"]})]
    (is (= {"field" {:stamp [1 1] :error "a; b"}} entries))
    (is (= {:id "field" :error "a; b" :kept false} warn))
    (is (= {:id "field" :broken "a; b"} (world/answer {:plans entries} "field")))))

(deftest a-deleted-file-is-forgotten
  (is (= {"a" {:stamp [1 1]}} (fsync/drop-gone {"a" {:stamp [1 1]} "b" {:stamp [1 1]}} {"a" [1 1]}))))

(deftest answer-gives-the-plan-its-status-and-cells
  (let [state (world/expand-all {:plans {"field" {:value wheat} "huts" {:value huts :error "bad edit"}}
                                 :blueprints {"hut" {:value hut-bp}}})]
    (are [id expected] (= expected (dissoc (world/answer state id) :cells))
      "field" {:id "field" :plan wheat :errors []}
      "huts" {:id "huts" :plan huts :errors [] :error "bad edit"}
      "none" nil)
    (is (= [{:pos [0 64 0] :want {:crop "wheat"} :part "rows"} {:pos [1 64 0] :want {:crop "wheat"} :part "rows"}]
           (:cells (world/answer state "field"))))
    (is (= [[5 64 5]] (map :pos (:cells (world/answer state "huts")))))))

(deftest a-world-from-data-answers-without-a-disk
  (let [w (world/of-data {"field" wheat} {})]
    (is (= wheat (:plan (world/plan w "field"))))
    (is (nil? (world/plan w "other")))
    (world/set-data! w {"field" (assoc wheat :note "later")} {})
    (is (= "later" (:note (:plan (world/plan w "field")))))))

(deftest a-key-is-new-only-once
  (let [w (world/of-data {} {})]
    (is (= [true false true] [(world/first-time! w [:a]) (world/first-time! w [:a]) (world/first-time! w [:b])]))))

;; ------------------------------------------------------------------ over a disk

(defn write! [dir id text] (fs/writeFileSync (path/join dir (str id ".edn")) text))

(defn reader
  "A world over fresh plan and blueprint dirs with a clock atom; warns are collected in seen."
  []
  (let [plans (tu/tmp-dir)
        bps (tu/tmp-dir)
        clock (atom 0)
        seen (atom [])
        w (world/open {:plans-dir plans :blueprint-dir bps :now #(deref clock) :every-ms 3000
                       :emit #(swap! seen conj %)})]
    {:plans plans :bps bps :clock clock :seen seen :w w}))

(defn touch-later!
  "Rewrite a file with a modification time one minute on, so the stamp changes even within one millisecond."
  [dir id text]
  (let [file (path/join dir (str id ".edn"))]
    (fs/writeFileSync file text)
    (let [t (+ 60 (/ (.-mtimeMs (fs/statSync file)) 1000))] (fs/utimesSync file t t))))

(deftest the-reader-sees-plans-and-blueprints-of-the-folder
  (let [{:keys [plans bps w]} (reader)]
    (write! plans "field" (pr-str wheat))
    (write! plans "huts" (pr-str huts))
    (write! bps "hut" (pr-str hut-bp))
    (fs/writeFileSync (path/join plans "notes.txt") "not a plan")
    (is (= wheat (:plan (world/plan w "field"))))
    (is (= 1 (count (:cells (world/plan w "huts")))))
    (is (nil? (world/plan w "notes")))))

(deftest an-edit-is-seen-after-the-recheck-interval-only
  (let [{:keys [plans clock w]} (reader)]
    (write! plans "field" (pr-str wheat))
    (is (nil? (:note (:plan (world/plan w "field")))))
    (touch-later! plans "field" (pr-str (assoc wheat :note "later")))
    (swap! clock + 1000)
    (is (nil? (:note (:plan (world/plan w "field")))))
    (swap! clock + 3000)
    (is (= "later" (:note (:plan (world/plan w "field")))))))

(deftest a-broken-edit-keeps-the-last-good-copy-with-one-warn
  (let [{:keys [plans clock seen w]} (reader)]
    (write! plans "field" (pr-str wheat))
    (world/plan w "field")
    (touch-later! plans "field" "{:id ")
    (dotimes [_ 3] (swap! clock + 5000) (world/plan w "field"))
    (is (= wheat (:plan (world/plan w "field"))))
    (is (re-find #"unreadable EDN" (:error (world/plan w "field"))))
    (is (= 1 (count @seen)))
    (is (= {:source :system :kind :world.plan-unreadable :level :warn :plan "field" :kept true}
           (select-keys (first @seen) [:source :kind :level :plan :kept])))))

(deftest a-plan-broken-from-the-start-is-reported-broken
  (let [{:keys [plans seen w]} (reader)]
    (write! plans "field" "{:id \"other\" :parts []}")
    (is (re-find #"file name" (:broken (world/plan w "field"))))
    (is (= [false] (map :kept @seen)))))

(deftest a-deleted-plan-is-missing-and-a-missing-folder-has-no-plans
  (let [{:keys [plans clock w]} (reader)]
    (write! plans "field" (pr-str wheat))
    (world/plan w "field")
    (fs/unlinkSync (path/join plans "field.edn"))
    (swap! clock + 3000)
    (is (nil? (world/plan w "field"))))
  (let [w (world/open {:plans-dir "/nonexistent/plans" :blueprint-dir "/nonexistent/bps" :now (constantly 0)
                       :every-ms 3000 :emit identity})]
    (is (nil? (world/plan w "field")))))

(deftest a-changed-blueprint-changes-the-plans-cells
  (let [{:keys [plans bps clock w]} (reader)]
    (write! plans "huts" (pr-str huts))
    (write! bps "hut" (pr-str hut-bp))
    (is (= 1 (count (:cells (world/plan w "huts")))))
    (touch-later! bps "hut" (pr-str (assoc hut-bp :layers [["SS"]])))
    (swap! clock + 3000)
    (is (= 2 (count (:cells (world/plan w "huts")))))))

;; ------------------------------------------------------------------ zones

(def farm-zone {:name "farm" :min [0 60 0] :max [9 70 9] :owner "Miles"})

(defn zone-reader
  "A world over fresh dirs and a zones file path (not written yet); warns are collected in seen."
  []
  (let [dir (tu/tmp-dir)
        clock (atom 0)
        seen (atom [])
        file (path/join dir "zones.edn")
        w (world/open {:plans-dir (path/join dir "plans") :blueprint-dir (path/join dir "bps") :zones-file file
                       :now #(deref clock) :every-ms 3000 :emit #(swap! seen conj %)})]
    {:file file :clock clock :seen seen :w w}))

(defn write-zones! [file text]
  (fs/writeFileSync file text)
  (let [t (+ 60 (rand-int 1000) (/ (.-mtimeMs (fs/statSync file)) 1000))] (fs/utimesSync file t t)))

(defn later! [clock w] (swap! clock + 3000) (world/zones w))

(deftest a-missing-zone-file-is-never-read-with-one-warn-naming-it
  (let [{:keys [file clock seen w]} (zone-reader)]
    (is (nil? (world/zones w)))
    (is (nil? (later! clock w)))
    (is (= [{:source :system :kind :world.zones-missing :level :warn :path file}]
           (map #(select-keys % [:source :kind :level :path]) @seen)))))

(deftest an-empty-zone-list-is-no-zones-and-a-good-one-is-read
  (let [{:keys [file clock seen w]} (zone-reader)]
    (write-zones! file "[]")
    (is (= [] (world/zones w)))
    (write-zones! file (pr-str [farm-zone]))
    (is (= [farm-zone] (later! clock w)))
    (is (= [] @seen))))

(deftest a-broken-edit-keeps-the-last-good-zones-with-one-warn-until-fixed
  (let [{:keys [file clock seen w]} (zone-reader)]
    (write-zones! file (pr-str [farm-zone]))
    (world/zones w)
    (write-zones! file (pr-str [(dissoc farm-zone :owner)]))
    (is (= [farm-zone] (later! clock w)))
    (is (= [farm-zone] (later! clock w)))
    (is (= [{:kind :world.zones-unreadable :level :warn :path file :kept true
             :error "zone farm: :owner must be a non-empty string"}]
           (map #(select-keys % [:kind :level :path :kept :error]) @seen)))
    (write-zones! file (pr-str [(assoc farm-zone :name "pen")]))
    (is (= "pen" (:name (first (later! clock w)))))
    (is (= 1 (count @seen)))))

(deftest a-zone-file-broken-from-the-start-is-never-read-until-fixed
  (let [{:keys [file clock seen w]} (zone-reader)]
    (write-zones! file "[{:name ")
    (is (nil? (world/zones w)))
    (is (nil? (later! clock w)))
    (is (= [false] (map :kept @seen)))
    (write-zones! file (pr-str [farm-zone]))
    (is (= [farm-zone] (later! clock w)))))

(deftest a-zone-file-moved-away-is-never-read-again-and-warned-again
  (let [{:keys [file clock seen w]} (zone-reader)]
    (write-zones! file (pr-str [farm-zone]))
    (world/zones w)
    (fs/unlinkSync file)
    (is (nil? (later! clock w)))
    (is (nil? (later! clock w)))
    (is (= [:world.zones-missing] (map :kind @seen)))))

(deftest a-world-from-data-has-no-zones-unless-given
  (let [w (world/of-data {} {})]
    (is (= [] (world/zones w)))
    (world/set-zones! w nil)
    (is (nil? (world/zones w)))
    (world/set-zones! w [farm-zone])
    (world/set-data! w {"field" wheat} {})
    (is (= [farm-zone] (world/zones w))))
  (is (nil? (world/zones nil))))

;; ------------------------------------------------------------------ footprints

(def pad {:id "pad" :parts [{:id "p" :box [[0 64 0] [1 64 0]] :want "stone"}]})
(def wall {:id "wall" :parts [{:id "w" :box [[1 64 0] [1 65 0]] :want "stone"}]})

(deftest footprints-are-the-cells-of-the-plans-by-plan
  (let [w (world/of-data {"pad" pad "wall" wall} {})
        fps (world/footprints w nil)]
    (is (= #{[0 64 0] [1 64 0] [1 65 0]} (set (keys fps))))
    (is (= "pad" (fps [0 64 0])))
    (is (= "wall" (fps [1 65 0])))))

(deftest footprints-except-a-plan-leave-out-only-its-own-cells
  (let [w (world/of-data {"pad" pad "wall" wall} {})]
    (is (= {[1 64 0] "wall" [1 65 0] "wall"} (world/footprints w "pad")))
    (is (= {[0 64 0] "pad" [1 64 0] "pad"} (world/footprints w "wall")))
    (is (= 3 (count (world/footprints w "other"))))
    (is (= {} (world/footprints nil nil)))))

(deftest plan-authors-lists-the-plans-that-name-a-maker
  (let [w (world/of-data {"pad" (shape/with-author pad "Fake") "wall" wall} {})]
    (is (= {"pad" "Fake"} (world/plan-authors w)))
    (is (= {} (world/plan-authors nil)))))

;; ------------------------------------------------------------------ claims

(def a-claim {:id "c1" :owner "Miles" :status :active :until 5000 :min [0 60 0] :max [9 70 9]})

(defn claim-reader
  "A world over fresh dirs and a claims file path (not written yet); warns are collected in seen."
  []
  (let [dir (tu/tmp-dir)
        clock (atom 0)
        seen (atom [])
        file (path/join dir "claims.edn")
        w (world/open {:plans-dir (path/join dir "plans") :blueprint-dir (path/join dir "bps")
                       :claims-file file :now #(deref clock) :every-ms 3000 :emit #(swap! seen conj %)})]
    {:file file :clock clock :seen seen :w w}))

(defn later-claims! [clock w] (swap! clock + 3000) (world/area-claims w))

(deftest a-missing-claims-file-is-no-claims-without-a-warn
  (let [{:keys [clock seen w]} (claim-reader)]
    (is (= [] (world/area-claims w)))
    (is (= [] (later-claims! clock w)))
    (is (= [] @seen))))

(deftest a-good-claims-file-is-read-and-a-broken-edit-keeps-the-last-good-copy-with-one-warn
  (let [{:keys [file clock seen w]} (claim-reader)]
    (write-zones! file (pr-str [a-claim]))
    (is (= [a-claim] (world/area-claims w)))
    (write-zones! file (pr-str [(dissoc a-claim :owner)]))
    (is (= [a-claim] (later-claims! clock w)))
    (is (= [a-claim] (later-claims! clock w)))
    (is (= [:world.claims-unreadable] (map :kind @seen)))
    (write-zones! file (pr-str [(assoc a-claim :id "c2")]))
    (is (= "c2" (:id (first (later-claims! clock w)))))
    (is (= 1 (count @seen)))))

(deftest a-claims-file-moved-away-is-no-claims
  (let [{:keys [file clock w]} (claim-reader)]
    (write-zones! file (pr-str [a-claim]))
    (world/area-claims w)
    (fs/unlinkSync file)
    (is (= [] (later-claims! clock w)))))

(deftest a-world-from-data-has-no-claims-unless-given
  (let [w (world/of-data {} {})]
    (is (= [] (world/area-claims w)))
    (world/set-area-claims! w [a-claim])
    (world/set-data! w {"field" wheat} {})
    (is (= [a-claim] (world/area-claims w))))
  (is (= [] (world/area-claims nil))))

(deftest only-active-unexpired-claims-are-live
  (are [claims now expected] (= expected (map :id (world/live-claims claims now)))
    [a-claim] 1000 ["c1"]
    [a-claim] 5000 []
    [(assoc a-claim :status :released)] 1000 []
    [(assoc a-claim :status "active")] 1000 ["c1"]
    [a-claim (assoc a-claim :id "c2" :until 100)] 1000 ["c1"]))

;; ------------------------------------------------------------------ shared markers

(def a-marker {:name "hut" :kind "base" :x 116 :y 69 :z -141 :by "Claude" :note "bed inside"})

(defn marker-reader
  "A world over fresh dirs; the markers file places.json sits beside the zones file (not written yet)."
  []
  (let [dir (tu/tmp-dir)
        clock (atom 0)
        seen (atom [])
        file (path/join dir "places.json")
        w (world/open {:plans-dir (path/join dir "plans") :blueprint-dir (path/join dir "bps")
                       :zones-file (path/join dir "zones.edn") :now #(deref clock) :every-ms 3000
                       :emit #(swap! seen conj %)})]
    {:file file :clock clock :seen seen :w w}))

(defn marker-warns [seen] (filter #(= :world.markers-unreadable (:kind %)) @seen))

(defn later-markers! [clock w] (swap! clock + 3000) (world/markers w))

(defn write-markers! [file text] (write-zones! file text))

(deftest a-missing-markers-file-is-no-markers-without-a-warn
  (let [{:keys [clock seen w]} (marker-reader)]
    (is (= [] (world/markers w)))
    (is (= [] (later-markers! clock w)))
    (is (= [] (filter #(= :world.markers-unreadable (:kind %)) @seen)))))

(deftest a-good-markers-file-is-read-and-a-marker-found-by-name
  (let [{:keys [file clock w]} (marker-reader)]
    (write-markers! file (js/JSON.stringify (clj->js [a-marker])))
    (is (= [a-marker] (world/markers w)))
    (is (= a-marker (world/marker w "hut")))
    (is (nil? (world/marker w "mine")))
    (write-markers! file (js/JSON.stringify (clj->js [a-marker (assoc a-marker :name "mine" :x 1)])))
    (is (= 1 (:x (do (swap! clock + 3000) (world/marker w "mine")))))))

(deftest a-broken-markers-edit-keeps-the-last-good-copy-with-one-warn
  (let [{:keys [file clock seen w]} (marker-reader)]
    (write-markers! file (js/JSON.stringify (clj->js [a-marker])))
    (world/markers w)
    (write-markers! file "[{\"name\": ")
    (is (= [a-marker] (later-markers! clock w)))
    (is (= [a-marker] (later-markers! clock w)))
    (is (= [[:world.markers-unreadable true]] (map (juxt :kind :kept) (marker-warns seen))))
    (write-markers! file (js/JSON.stringify (clj->js [(assoc a-marker :name "pen")])))
    (is (= "pen" (:name (first (later-markers! clock w)))))
    (is (= 1 (count (marker-warns seen))))))

(deftest a-marker-without-a-name-or-position-makes-the-file-bad
  (let [{:keys [file clock seen w]} (marker-reader)]
    (write-markers! file (js/JSON.stringify (clj->js [(dissoc a-marker :x)])))
    (is (= [] (world/markers w)))
    (is (= [[:world.markers-unreadable false]] (map (juxt :kind :kept) (marker-warns seen))))))

(deftest a-world-from-data-has-no-markers-unless-given
  (let [w (world/of-data {} {})]
    (is (= [] (world/markers w)))
    (world/set-markers! w [a-marker])
    (is (= a-marker (world/marker w "hut"))))
  (is (= [] (world/markers nil)))
  (is (nil? (world/marker nil "hut"))))

(def far-marker {:name "mine" :kind "mine" :x 500 :y 12 :z 500 :note "iron seam"})
(def near-marker {:name "farm-north" :kind "farm" :x 10 :y 64 :z 10})

(deftest markers-are-searched-by-name-kind-or-note-nearest-first-and-bounded
  (let [ms [far-marker near-marker a-marker]
        names #(mapv :name (world/find-markers ms %))]
    (is (= ["mine"] (names {:text "iron"})) "note")
    (is (= ["farm-north"] (names {:text "FARM"})) "name and kind, any case")
    (is (= ["mine"] (names {:kind "mine"})))
    (is (= ["farm-north" "hut" "mine"] (names {:near {:x 0 :y 64 :z 0}})) "nearest first")
    (is (= ["farm-north"] (names {:near {:x 0 :y 64 :z 0} :limit 1})))
    (is (= 10 (count (world/find-markers (repeat 50 near-marker) {}))) "default limit 10")
    (is (= [] (names {:text "nothing"})))))
