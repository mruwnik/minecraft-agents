(ns engine.migrate-test
  (:require [cljs.test :refer [deftest is]]
            [engine.memory :as mem]
            [engine.migrate :as migrate]
            [engine.test-util :as tu]
            ["fs" :as fs]))

(def cfg {:username "Ann" :apiPort 1 :world "w"})
(defn place [name kind x by] {:name name :kind kind :x x :y 64 :z 2 :by by :note ""})
(defn ev [seq t & kvs] (into {:seq seq :t t :type "x"} (map vec (partition 2 kvs))))
(defn run [m] (migrate/convert (merge {:name "Ann" :config cfg :places [] :events [] :places-mtime 5000} m)))

(deftest no-places-for-this-body
  (let [r (run {:places [(place "b" "bed" 1 "Bob") (place "base" "base" 1 "Ann")]})]
    (is (= mem/empty-data (:memory r)) "an empty memory, so the body still gets engine/")
    (is (= [0 0] [(:beds r) (:chests r)]))))

(deftest one-bed
  (let [r (run {:places [(place "b" "bed" 1 "Ann")]})]
    (is (= {:entries {:bed [{:t 5000 :wt 0 :data {:pos {:x 1 :y 64 :z 2}}}]}
            :policies {:bed {:cap 1 :ttl :forever}}}
           (:memory r)))
    (is (= [] (:beds-dropped r)))))

(deftest several-beds-last-wins
  (let [r (run {:places [(place "a" "bed" 1 "Ann") (place "b" "bed" 2 "Ann") (place "c" "bed" 3 "Ann")]})]
    (is (= {:x 3 :y 64 :z 2} (get-in r [:memory :entries :bed 0 :data :pos])))
    (is (= [{:x 1 :y 64 :z 2} {:x 2 :y 64 :z 2}] (:beds-dropped r)))
    (is (= 1 (:beds r)))))

(deftest chests-are-cap-one-too
  (let [r (run {:places [(place "a" "chest" 1 "Ann") (place "b" "chest" 7 "Ann") (place "c" "bed" 9 "Ann")]})]
    (is (= {:x 7 :y 64 :z 2} (get-in r [:memory :entries :chest 0 :data :pos])))
    (is (= [{:x 1 :y 64 :z 2}] (:chests-dropped r)))
    (is (= mem/place-policy (get-in r [:memory :policies :chest])))
    (is (= [1 1] [(:beds r) (:chests r)]))))

(deftest t-falls-back-to-now
  (is (= 77 (get-in (run {:places [(place "a" "bed" 1 "Ann")] :places-mtime nil :now 77})
                    [:memory :entries :bed 0 :t]))))

(deftest events-without-position-give-no-pose
  (is (nil? (:pose (run {:events [(ev 1 "2026-09-27T17:00:00.000Z") (ev 2 "2026-09-27T17:00:01.000Z" :position 3)]})))))

(deftest pose-from-the-last-positioned-event
  (let [r (run {:events [(ev 1 "2026-09-27T17:00:00.000Z" :pos {:x 1 :y 2 :z 3})
                         (ev 2 "2026-09-27T17:00:01.000Z" :position {:x 4.5 :y 5 :z 6})
                         (ev 3 "2026-09-27T17:00:02.000Z" :position 1)
                         (ev 4 "2026-09-27T17:00:03.000Z" :pos {:x "a" :y 1 :z 1})]})]
    (is (= {:v 1 :t (js/Date.parse "2026-09-27T17:00:01.000Z") :world "w" :status "offline"
            :pos {:x 4.5 :y 5 :z 6}}
           (:pose r)))))

(deftest a-malformed-file-is-skipped-and-named
  (let [r (run {:events {:parse-error "bad json at line 3"}
                :places [(place "a" "bed" 1 "Ann")]})]
    (is (= [{:file "events.jsonl" :error "bad json at line 3"}] (:skipped r)))
    (is (nil? (:pose r)))
    (is (= 1 (:beds r))))
  (let [r (run {:places {:parse-error "oops"} :events [(ev 1 "2026-09-27T17:00:00.000Z" :pos {:x 1 :y 2 :z 3})]})]
    (is (= ["places.json"] (map :file (:skipped r))))
    (is (= mem/empty-data (:memory r)))
    (is (= {:x 1 :y 2 :z 3} (:pos (:pose r)))))
  (let [r (run {:config {:parse-error "no"} :events [(ev 1 "2026-09-27T17:00:00.000Z" :pos {:x 1 :y 2 :z 3})]})]
    (is (nil? (:pose r)) "no world, no pose")))

(deftest converted-memory-loads-and-survives-a-sweep
  (let [dir (tu/tmp-dir)
        r (run {:places [(place "a" "bed" 1 "Ann") (place "c" "chest" 4 "Ann")]})
        year (* 365 24 60 60 1000)]
    (fs/writeFileSync (mem/file dir) (pr-str (:memory r)))
    (let [store (mem/open dir {:now (constantly (+ 5000 (* 50 year)))})
          view (mem/view store)]
      (is (= {:x 1 :y 64 :z 2} (mem/place view :bed)))
      (is (= {:x 4 :y 64 :z 2} (mem/place view :chest)))
      (is (= (:memory r) (:data @store)) "the open's sweep dropped nothing"))))

(deftest the-pose-carries-the-dimension-of-its-event
  (let [nether (run {:events [(ev 1 "2026-09-27T17:00:00.000Z" :pos {:x 1 :y 2 :z 3} :dimension "overworld")
                              (ev 2 "2026-09-27T17:00:01.000Z" :pos {:x 8 :y 9 :z 10} :dimension "the_nether")]})
        none (run {:events [(ev 1 "2026-09-27T17:00:00.000Z" :pos {:x 1 :y 2 :z 3})]})]
    (is (= {:x 8 :y 9 :z 10} (get-in nether [:pose :pos])))
    (is (= "the_nether" (get-in nether [:pose :dimension])))
    (is (not (contains? (:pose none) :dimension)) "an event without a dimension gives none")))
