(ns engine.events-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [cljs.reader :as reader]
            [engine.events :as events]
            [engine.test-util :as tu]
            ["fs" :as fs]
            ["path" :as path]))

(defn read-lines [file]
  (->> (str/split-lines (fs/readFileSync file "utf8"))
       (remove str/blank?)
       (mapv reader/read-string)))

(defn total-bytes [file]
  (reduce + 0
          (for [n (range events/segment-count)
                :let [segment (events/segment-file file n)]
                :when (fs/existsSync segment)]
            (.-size (fs/statSync segment)))))

(deftest canonical-edn-normalizes-legacy-input
  (let [[seen sink] (tu/capture-sink)
        stream (events/make {:generation-id "generation-a" :sinks [sink]
                             :now (constantly 1234)
                             :pos-fn (constantly {:x 1.7 :y 64 :z -0.2})})]
    (is (= 1 (events/emit! stream {:source :job :kind :queued :level :info
                                   :body "discarded" :job "j1" :round 7
                                   :chain "c1" :reflex "r1" :name "go-to"})))
    (let [event (first @seen)]
      (is (= {:seq 1 :generation-id "generation-a" :time-ms 1234
              :source :job :kind :queued
              :context {:job-id "j1" :round 7 :chain "c1" :reflex-id "r1"}
              :data {:name "go-to" :pos {:x 1.7 :y 64 :z -0.2}}}
             event))
      (is (not (contains? event :body)))
      (is (not (contains? event :level)))
      (is (= {:stream-id (:stream-id (events/cursor stream)) :seq 1}
             (events/cursor stream))))))

(deftest edn-file-restarts-with-stable-stream-and-new-generation
  (let [file (path/join (tu/tmp-dir) "events.edn")
        first-run (events/make {:file file :generation-id "generation-a"})]
    (events/emit! first-run {:source :system :kind :started :text "hello"})
    (events/emit! first-run {:source :job :kind :queued :job "j1"})
    (let [second-run (events/make {:file file :generation-id "generation-b"})
          sid (:stream-id (events/cursor first-run))]
      (is (= {:stream-id sid :seq 2}
             (events/cursor second-run)))
      (events/emit! second-run {:source :system :kind :restored})
      (is (= [1 2 3] (mapv :seq (read-lines file))))
      (is (= ["generation-a" "generation-a" "generation-b"]
             (mapv :generation-id (read-lines file))))
      (is (= "hello" (:message (first (read-lines file)))))
      (is (= {:stream-id sid :seq 3}
             (:cursor (events/read-after second-run
                                         {:stream-id sid :after 2 :limit 10})))))))

(deftest read-after-pages-and-detects-stream-replacement
  (let [stream (events/make {:generation-id "g"})
        sid (:stream-id (events/cursor stream))]
    (doseq [n (range 5)]
      (events/emit! stream {:source :action :kind :finished :data {:n n}}))
    (let [page (events/read-after stream {:stream-id sid :after 0 :limit 2})]
      (is (false? (:gap? page)))
      (is (= [1 2] (mapv :seq (:events page))))
      (is (= {:stream-id sid :seq 2} (:cursor page))))
    (let [replacement (events/read-after stream {:stream-id "old-stream" :after 0})]
      (is (true? (:gap? replacement)))
      (is (empty? (:events replacement))))))

(deftest rotation-is-aggregate-bounded-and-old-cursors-report-gap
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g" :max-bytes 1024})
        sid (:stream-id (events/cursor stream))]
    (doseq [n (range 18)]
      (events/emit! stream {:source :action :kind :finished
                            :data {:payload (apply str (repeat 100 (str n)))}}))
    (is (<= (total-bytes file) 1024))
    (let [page (events/read-after stream {:stream-id sid :after 0 :limit 10})]
      (is (true? (:gap? page)))
      (is (empty? (:events page))))
    (let [latest (:seq (events/cursor stream))
          restarted (events/make {:file file :generation-id "g" :max-bytes 1024})
          page (events/read-after restarted {:stream-id sid :after (dec latest) :limit 10})]
      (is (false? (:gap? page)))
      (is (= [latest] (mapv :seq (:events page)))))))

(deftest lowering-cap-trims-oldest-active-records-and-keeps-high-water
  (let [file (path/join (tu/tmp-dir) "lower-cap.edn")
        initial (events/make {:file file :generation-id "g" :max-bytes 8192})
        sid (:stream-id (events/cursor initial))]
    (doseq [n (range 5)]
      (events/emit! initial {:source :action :kind :finished
                             :data {:payload (apply str (repeat 240 (str n)))}}))
    (let [last-seq (:seq (events/cursor initial))
          lowered (events/make {:file file :generation-id "g" :max-bytes 1024})]
      (is (<= (total-bytes file) 1024))
      (is (= last-seq (:seq (events/cursor lowered))))
      (is (true? (:gap? (events/read-after lowered {:stream-id sid :after 0 :limit 10}))))
      (is (= [last-seq]
             (mapv :seq (:events (events/read-after lowered
                                                    {:stream-id sid :after (dec last-seq)
                                                     :limit 10}))))))))

(deftest startup-repairs-incomplete-tail-but-rejects-malformed-complete-line
  (testing "an incomplete final record is discarded before appending"
    (let [file (path/join (tu/tmp-dir) "partial.edn")
          first-run (events/make {:file file})]
      (events/emit! first-run {:source :system :kind :started})
      (fs/appendFileSync file "{:seq 2 :incomplete")
      (let [restarted (events/make {:file file})]
        (is (= 2 (events/emit! restarted {:source :system :kind :restored})))
        (is (= [1 2] (mapv :seq (read-lines file)))))))
  (testing "a malformed complete record is an explicit startup error"
    (let [file (path/join (tu/tmp-dir) "malformed.edn")]
      (fs/writeFileSync file "{not-edn}\n")
      (is (try
            (events/make {:file file})
            false
            (catch :default _ true))))))

(deftest oversized-event-is-reported-without-writing-or-reusing-a-sequence
  (let [file (path/join (tu/tmp-dir) "oversized.edn")
        stream (events/make {:file file :max-bytes 1024})
        result (events/emit! stream {:source :action :kind :finished
                                     :data {:payload (apply str (repeat 2000 "x"))}})]
    (is (= :event-too-large (:error result)))
    (is (= 0 (:seq (events/cursor stream))))
    (is (empty? (read-lines file)))
    (is (= 1 (events/emit! stream {:source :system :kind :started})))))

;; ---------------------------------------------------------------- disk reads

(defn emit-n! [stream n]
  (doseq [i (range n)]
    (events/emit! stream {:source :action :kind :finished :data {:i i :pad (apply str (repeat 40 "p"))}})))

(defn disk-only
  "A copy of the stream's reader state with an empty recent buffer, so every read goes to disk."
  [stream]
  (swap! stream assoc :recent [] :recent-byte-count 0)
  stream)

(defn without-whole-file-reads
  "Run f with fs.readFileSync failing for file: a reader that loads a segment whole throws."
  [file f]
  (let [original (.-readFileSync fs)]
    (set! (.-readFileSync fs)
          (fn [p & more]
            (when (= p file) (throw (js/Error. "whole-file read")))
            (apply original p more)))
    (try (f)
         (finally (set! (.-readFileSync fs) original)))))

(deftest disk-reads-do-not-load-the-whole-segment
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g"})
        sid (:stream-id (events/cursor stream))]
    (emit-n! stream 400)
    (disk-only stream)
    (doseq [after [0 1 100 250 390 398]]
      (let [page (without-whole-file-reads
                  file #(events/read-after stream {:stream-id sid :after after :limit 10}))
            want (range (inc after) (inc (min 400 (+ after 10))))]
        (is (false? (:gap? page)) (str "after " after))
        (is (= want (mapv :seq (:events page))) (str "after " after))))))

(deftest disk-reads-equal-recent-reads-across-segments
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g" :max-bytes 8192})
        sid (:stream-id (events/cursor stream))]
    (emit-n! stream 60)
    (let [latest (:seq (events/cursor stream))
          oldest (:oldest-seq (events/read-after stream {:stream-id sid :after latest}))
          afters (range (dec oldest) latest)
          from-recent (mapv #(events/read-after stream {:stream-id sid :after % :limit 7}) afters)]
      (is (< 1 oldest) "retention dropped the oldest events")
      (is (< 1 (count (filter #(fs/existsSync (events/segment-file file %)) (range events/segment-count))))
          "the events span several segments")
      (disk-only stream)
      (is (= from-recent
             (mapv #(events/read-after stream {:stream-id sid :after % :limit 7}) afters))))))

(deftest disk-reads-report-a-hole-in-the-sequence-as-a-gap
  (let [file (path/join (tu/tmp-dir) "events.edn")
        line (fn [n] (str (pr-str {:seq n :generation-id "g" :time-ms 0 :source :a :kind :b}) "\n"))
        _ (fs/mkdirSync (path/dirname file) #js {:recursive true})
        _ (fs/writeFileSync file (apply str (map line (concat (range 1 11) (range 12 21)))))
        _ (events/write-meta! file {:stream-id "s" :last-seq 20})
        stream (disk-only (events/make {:file file :generation-id "g" :stream-id "s"}))]
    (is (true? (:gap? (events/read-after stream {:stream-id "s" :after 5 :limit 100}))))
    (is (true? (:gap? (events/read-after stream {:stream-id "s" :after 10 :limit 100}))))
    (is (= (range 12 21)
           (mapv :seq (:events (events/read-after stream {:stream-id "s" :after 11 :limit 100})))))))

(deftest make-seeds-the-recent-buffer-from-the-active-file-tail
  (let [file (path/join (tu/tmp-dir) "events.edn")
        first-run (events/make {:file file :generation-id "g"})]
    (emit-n! first-run 3000)
    (let [restarted (events/make {:file file :generation-id "g"})
          recent (:recent @restarted)]
      (is (= 3000 (:seq (last recent))))
      (is (= events/recent-count (count recent)))
      (is (= (range (inc (- 3000 events/recent-count)) 3001) (mapv :seq recent)))
      (is (= (:seq (last (:recent @first-run))) (:seq (last recent))))
      (is (pos? (:recent-byte-count @restarted))))))

(deftest make-seeds-a-short-file-completely
  (let [file (path/join (tu/tmp-dir) "events.edn")]
    (emit-n! (events/make {:file file :generation-id "g"}) 12)
    (is (= (range 1 13) (mapv :seq (:recent @(events/make {:file file :generation-id "g"})))))))
