(ns engine.memory-test
  (:require [cljs.test :refer [deftest is]]
            [cljs.reader :as reader]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            ["fs" :as fs]
            ["path" :as path]))

(def hour (* 60 60 1000))

(defn store
  "A store over a fresh dir with a settable clock; returns [store clock dir]."
  ([] (store {}))
  ([opts]
   (let [dir (tu/tmp-dir)
         clock (atom 1000000)]
     [(mem/open dir (merge {:now #(deref clock) :world-time (constantly 6000)} opts)) clock dir])))

(defn on-disk [dir]
  (reader/read-string (fs/readFileSync (path/join dir "memory.edn") "utf8")))

(deftest an-entry-is-t-wt-and-data-newest-last
  (let [[s clock] (store)]
    (mem/write! s :hurt {:health 5})
    (swap! clock + 10)
    (mem/write! s :hurt {:health 4})
    (is (= [{:t 1000000 :wt 6000 :data {:health 5}} {:t 1000010 :wt 6000 :data {:health 4}}]
           (mem/entries (mem/view s) :hurt)))
    (is (= {:health 4} (:data (mem/latest (mem/view s) :hurt))))
    (is (nil? (mem/latest (mem/view s) :chat)))))

(deftest a-new-kind-without-a-policy-gets-the-default
  (let [[s] (store)]
    (mem/write! s :chat {:message "hi"})
    (is (= {:cap 50 :ttl hour} (mem/policy (mem/view s) :chat)))
    (mem/write! s :bed {:pos {:x 1 :y 64 :z 1}} {:cap 1 :ttl :forever})
    (mem/write! s :bed {:pos {:x 2 :y 64 :z 2}})
    (is (= {:cap 1 :ttl :forever} (mem/policy (mem/view s) :bed)) "a later write without one keeps it")
    (is (= [{:pos {:x 2 :y 64 :z 2}}] (mapv :data (mem/entries (mem/view s) :bed))) "the cap drops the oldest")))

(deftest a-policy-needs-a-cap-and-a-spelled-out-ttl
  (let [[s] (store)]
    (is (thrown? js/Error (mem/write! s :bed {} {:cap 1})) "an omitted ttl is not forever")
    (is (thrown? js/Error (mem/write! s :bed {} {:ttl :forever})))
    (is (thrown? js/Error (mem/write! s :bed {} {:cap 1 :ttl :always})))))

(deftest the-cap-keeps-the-newest
  (let [[s] (store)]
    (doseq [i (range 5)] (mem/write! s :seen {:i i} {:cap 3 :ttl hour}))
    (is (= [2 3 4] (mapv #(get-in % [:data :i]) (mem/entries (mem/view s) :seen))))))

(deftest reads-are-time-filtered-before-any-sweep
  (let [[s clock] (store)]
    (mem/write! s :hurt {:n 1} {:cap 50 :ttl 1000})
    (swap! clock + 600)
    (mem/write! s :hurt {:n 2})
    (swap! clock + 500)
    (let [v (mem/view s)]
      (is (= [{:n 2}] (mapv :data (mem/entries v :hurt))) "the first expired at 1000 ms")
      (is (= 1 (mem/count-in v :hurt 10000)))
      (is (= {:n 2} (:data (mem/latest v :hurt))))
      (is (= [] (mem/since v :hurt (+ 1000000 700)))))
    (is (= 2 (count (get-in (:data @s) [:entries :hurt]))) "still stored until a sweep")))

(deftest since-and-count-in
  (let [[s clock] (store)]
    (doseq [dt [0 100 200 300]]
      (reset! clock (+ 1000000 dt))
      (mem/write! s :chat {:dt dt}))
    (let [v (mem/view s)]
      (is (= [200 300] (mapv #(get-in % [:data :dt]) (mem/since v :chat 1000200))))
      (is (= 2 (mem/count-in v :chat 150)) "within the last 150 ms: 200 and 300"))))

(deftest the-sweep-drops-expired-entries-and-empty-kinds
  (let [[s clock dir] (store)]
    (mem/write! s :hurt {:n 1} {:cap 50 :ttl 1000})
    (mem/write! s :bed {:pos {:x 0 :y 64 :z 0}} {:cap 1 :ttl :forever})
    (swap! clock + 5000)
    (mem/save! s)
    (is (= #{:bed} (set (keys (get-in (on-disk dir) [:entries])))))
    (is (= #{:bed} (set (keys (get-in (on-disk dir) [:policies])))) "its policy goes with it")))

(deftest save-writes-edn-with-keywords-and-open-reads-it-back
  (let [[s _ dir] (store)]
    (mem/write! s :forestry/replant {:pos {:x 1 :y 64 :z 2} :species :oak} {:cap 50 :ttl :forever})
    (mem/save! s)
    (let [again (mem/open dir {:now (constantly 1000000)})]
      (is (= {:pos {:x 1 :y 64 :z 2} :species :oak}
             (:data (mem/latest (mem/view again) :forestry/replant))))
      (is (= {:cap 50 :ttl :forever} (mem/policy (mem/view again) :forestry/replant))))))

(deftest open-sweeps-on-boot
  (let [[s clock dir] (store)]
    (mem/write! s :hurt {:n 1} {:cap 50 :ttl 1000})
    (mem/save! s)
    (let [again (mem/open dir {:now #(+ @clock 2000)})]
      (is (nil? (get-in (:data @again) [:entries :hurt]))))))

(deftest forget-a-kind-up-to-a-time-or-by-predicate
  (let [[s clock] (store)]
    (doseq [dt [0 100 200]]
      (reset! clock (+ 1000000 dt))
      (mem/write! s :hurt {:dt dt}))
    (mem/forget-until! s :hurt 1000100)
    (is (= [200] (mapv #(get-in % [:data :dt]) (mem/entries (mem/view s) :hurt))) "a hit after the handled time stays")
    (mem/forget-where! s :hurt #(= 200 (:dt %)))
    (is (= [] (mem/entries (mem/view s) :hurt)))
    (mem/write! s :chat {})
    (mem/forget! s :chat)
    (is (nil? (get-in (:data @s) [:entries :chat])))))

;; ------------------------------------------------------------- job memory

(deftest job-memory-is-one-forever-entry-under-job-id
  (let [[s clock] (store)]
    (mem/create-job! s "j7" {:species :oak})
    (is (= {:args {:species :oak} :children {}} (mem/job-mem (mem/view s) "j7" [])))
    (mem/update-job! s "j7" [] assoc :phase :fell)
    (mem/update-job! s "j7" [:fell] assoc :tree-base [12 64 -30])
    (mem/update-job! s "j7" [:fell :walk] assoc :tries 1)
    (swap! clock + (* 100 hour))
    (let [v (mem/view s)]
      (is (= {:cap 1 :ttl :forever} (mem/policy v :job/j7)))
      (is (= 1 (count (mem/entries v :job/j7))))
      (is (= :fell (:phase (mem/job-mem v "j7" []))))
      (is (= [12 64 -30] (:tree-base (mem/job-mem v "j7" [:fell]))))
      (is (= {:tries 1} (mem/job-mem v "j7" [:fell :walk])))
      (is (= {:tree-base [12 64 -30] :children {:walk {:tries 1}}}
             (get-in (mem/job-mem v "j7" []) [:children :fell])) "children nest under :children by slot"))))

(deftest delete-job-takes-the-subtree
  (let [[s] (store)]
    (mem/create-job! s "j3" {})
    (mem/update-job! s "j3" [:a] assoc :x 1)
    (mem/delete-job! s "j3")
    (is (nil? (get-in (:data @s) [:entries :job/j3])))
    (is (= {} (mem/job-mem (mem/view s) "j3" [:a])))))

(deftest the-sweep-drops-job-kinds-not-on-the-list
  (let [live (atom #{"j1"})
        [s _ dir] (store {:live-jobs #(deref live)})]
    (mem/create-job! s "j1" {})
    (mem/create-job! s "j2" {})
    (mem/save! s)
    (is (= #{:job/j1} (set (keys (get-in (on-disk dir) [:entries])))))))

(deftest ids-render-paths
  (is (= "j1" (mem/path->id "j1" [])))
  (is (= "j1/fell/walk" (mem/path->id "j1" [:fell :walk]))))
