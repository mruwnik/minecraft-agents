(ns engine.events-test
  (:require [cljs.test :refer [deftest is testing]]
            [clojure.string :as str]
            [cljs.reader :as reader]
            [engine.events :as events]
            [engine.settings :as settings]
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
              :source :job :kind :queued :level :info
              :context {:job-id "j1" :round 7 :chain "c1" :reflex-id "r1"}
              :data {:name "go-to" :pos {:x 1.7 :y 64 :z -0.2}}}
             event))
      (is (not (contains? event :body)))
      (is (= {:stream-id (:stream-id (events/cursor stream)) :seq 1}
             (events/cursor stream))))))

(deftest a-debug-event-keeps-its-level
  (let [[seen sink] (tu/capture-sink)
        stream (events/make {:generation-id "g" :sinks [sink] :now (constantly 1)})]
    (events/emit! stream {:source :job :kind :round_started :level :debug})
    (events/emit! stream {:source :job :kind :queued})
    (is (= [:debug nil] (mapv :level @seen)))))

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

(deftest a-restart-starts-with-no-recent-buffer-and-readers-read-the-old-events-from-disk
  (let [file (path/join (tu/tmp-dir) "events.edn")
        first-run (events/make {:file file :generation-id "g"})
        sid (:stream-id (events/cursor first-run))]
    (emit-n! first-run 3000)
    (let [restarted (events/make {:file file :generation-id "g"})
          page (events/read-after restarted {:stream-id sid :after 2990 :limit 100})]
      (is (empty? (:recent @restarted)))
      (is (false? (:gap? page)))
      (is (= (range 2991 3001) (mapv :seq (:events page)))))))

(deftest the-recent-buffer-stays-small-and-lagging-readers-read-disk
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g"})
        sid (:stream-id (events/cursor stream))]
    (emit-n! stream 1000)
    (is (<= (count (:recent @stream)) 256) "a few hundred events in memory, not thousands")
    (is (<= (:recent-byte-count @stream) (* 256 1024)))
    (let [page (events/read-after stream {:stream-id sid :after 100 :limit 2000})]
      (is (false? (:gap? page)))
      (is (= (range 101 1001) (mapv :seq (:events page)))))))

(deftest a-body-cause-stays-in-data-and-a-cause-seq-in-context
  (let [[seen sink] (tu/capture-sink)
        stream (events/make {:generation-id "g" :sinks [sink] :now (constantly 1)})]
    (events/emit! stream {:source :body :kind :died :cause "lava"})
    (events/emit! stream {:source :job :kind :done :cause 7})
    (is (= [{:data {:cause "lava"}} {:context {:cause-seq 7}}]
           [(select-keys (first @seen) [:data :context]) (select-keys (second @seen) [:data :context])]))))

(deftest a-partial-append-is-rolled-back-so-the-log-stays-bootable
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "generation-a"})
        real-append (.-appendFileSync fs)]
    (events/emit! stream {:source :system :kind :started})
    (set! (.-appendFileSync fs)
          (fn [f text] (real-append f (subs text 0 20)) (throw (js/Error. "ENOSPC"))))
    (try (events/emit! stream {:source :job :kind :queued :job "j1"})
         (catch :default _ nil)
         (finally (set! (.-appendFileSync fs) real-append)))
    (events/emit! stream {:source :job :kind :queued :job "j2"})
    (is (= [1 3] (mapv :seq (read-lines file))) "no half record is left between the lines")
    (is (= 3 (:seq (events/cursor (events/make {:file file :generation-id "generation-b"})))) "the log boots")))

(defn write-lines!
  "Write one event line per seq in seqs (pad chars of :pad each) to a fresh file; returns the file."
  ([seqs pad] (write-lines! seqs pad "p"))
  ([seqs pad ch]
   (let [file (path/join (tu/tmp-dir) "events.edn")
         line (fn [n] (str (pr-str {:seq n :pad (apply str (repeat pad ch))}) "\n"))]
     (fs/mkdirSync (path/dirname file) #js {:recursive true})
     (fs/writeFileSync file (apply str (map line seqs)))
     file)))

(def one-page-bytes (* 8 1024 1024))

(defn seqs-after [file after n]
  (mapv :seq (:records (events/segment-records-after file after n one-page-bytes))))

(deftest a-line-split-across-the-chunk-edge-is-read-whole
  (doseq [pad [(- events/read-chunk-bytes 40) (- events/read-chunk-bytes 20) events/read-chunk-bytes]]
    (let [file (write-lines! (range 1 6) pad)]
      (is (= [1 2 3 4 5] (seqs-after file 0 10)) (str "pad " pad))
      (is (= [4 5] (seqs-after file 3 10)) (str "pad " pad)))))

(defn parsing-whole
  "Run f with the oversize-line stub switched off, for tests of how long lines are read."
  [f]
  (settings/with-settings {:engine.events/max-line-bytes (* 100 1024 1024)} f))

(deftest multi-byte-utf8-at-the-chunk-edge-is-not-split
  (parsing-whole
   #(doseq [pad (range 20000 20003)]
      (let [file (write-lines! (range 1 5) (quot events/read-chunk-bytes 3) "é")
            file2 (write-lines! (range 1 5) pad "日")]
        (is (every? (fn [r] (= (quot events/read-chunk-bytes 3) (count (:pad r))))
                    (:records (events/segment-records-after file 0 10 one-page-bytes))))
        (is (every? (fn [r] (= pad (count (:pad r)))) (:records (events/segment-records-after file2 0 10 one-page-bytes)))
            (str "pad " pad))))))

(deftest a-line-longer-than-one-chunk-is-read-whole
  (let [file (write-lines! [1 2 3] (* 5 events/read-chunk-bytes))]
    (is (= [1 2 3] (seqs-after file 0 10)))
    (is (= [2 3] (seqs-after file 1 10)))
    (is (= [3] (seqs-after file 2 10)))
    (is (= (* 5 events/read-chunk-bytes)
           (parsing-whole #(count (:pad (first (:records (events/segment-records-after file 0 1 one-page-bytes))))))))))

(defn all-pages
  "Every event read-after returns, page after page from cursor 0, reading from disk only."
  [stream sid]
  (loop [after 0 out []]
    (let [p (events/read-after (disk-only stream) {:stream-id sid :after after :limit 100})]
      (if (empty? (:events p)) out (recur (:seq (:cursor p)) (into out (:events p)))))))

(deftest a-page-of-huge-events-stops-at-the-byte-budget-and-still-pages-on
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g"})
        sid (:stream-id (events/cursor stream))
        pad (apply str (repeat 200000 "p"))]
    (doseq [i (range 12)]
      (events/emit! stream {:source :job :kind :memory_written :data {:i i :pad pad}}))
    (let [page (events/read-after (disk-only stream) {:stream-id sid :after 0 :limit 100})
          seqs (mapv :seq (:events page))]
      (is (false? (:gap? page)) "a page cut short by bytes is no gap")
      (is (<= 1 (count seqs) 6) "far fewer than the 12 on disk, at least one")
      (is (= seqs (vec (range 1 (inc (count seqs))))))
      (is (= (last seqs) (:seq (:cursor page))))
      (is (= (vec (range 1 13)) (mapv :seq (all-pages stream sid))) "paging on reaches the last event"))))

(deftest byte-capped-pages-cross-a-segment-boundary-without-a-hole
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g" :max-bytes (* 4 1024 1024)})
        sid (:stream-id (events/cursor stream))
        pad (apply str (repeat 150000 "p"))]
    (doseq [i (range 20)]
      (events/emit! stream {:source :job :kind :memory_written :data {:i i :pad pad}}))
    (let [oldest (:oldest-seq (events/read-after stream {:stream-id sid :after 20}))
          pages (loop [after (dec oldest) out []]
                  (let [p (events/read-after (disk-only stream) {:stream-id sid :after after :limit 100})]
                    (if (empty? (:events p)) out (recur (:seq (:cursor p)) (conj out p)))))]
      (is (< 1 (count (filter #(fs/existsSync (events/segment-file file %)) (range events/segment-count)))) "several segments")
      (is (< 1 (count pages)) "capped into several pages")
      (is (every? (comp false? :gap?) pages))
      (is (= (vec (range oldest 21)) (mapv :seq (mapcat :events pages)))))))

(deftest a-cursor-older-than-every-segment-reports-a-gap
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g" :max-bytes 8192})
        sid (:stream-id (events/cursor stream))]
    (emit-n! stream 80)
    (let [oldest (:oldest-seq (events/read-after stream {:stream-id sid :after 80}))]
      (is (< 2 oldest))
      (disk-only stream)
      (is (true? (:gap? (events/read-after stream {:stream-id sid :after 0}))))
      (is (true? (:gap? (events/read-after stream {:stream-id sid :after (- oldest 2)}))))
      (is (false? (:gap? (events/read-after stream {:stream-id sid :after (dec oldest) :limit 3})))))))

(deftest an-empty-file-yields-no-records
  (let [file (write-lines! [] 0)]
    (is (= [] (seqs-after file 0 10)))
    (is (= [] (:records (events/records-from (fs/openSync file "r") 0 file 0 10 one-page-bytes))))))

(deftest a-corrupt-line-is-reported-with-its-file
  (let [file (write-lines! [1 2] 10)]
    (fs/appendFileSync file "{:seq 3 :pad\n")
    (fs/appendFileSync file (str (pr-str {:seq 4}) "\n"))
    (is (thrown-with-msg? js/Error #"malformed complete event record" (seqs-after file 0 10)))))

(deftest a-line-ending-at-a-probe-size-is-read-whole
  (doseq [pad [500 520 1000 1030 2000 40000]]
    (let [file (write-lines! (range 1 6) pad)]
      (is (= [1 2 3 4 5] (seqs-after file 0 10)) (str "pad " pad))
      (is (= [3 4 5] (seqs-after file 2 10)) (str "pad " pad)))))

(defn write-exact-lines!
  "Write one line per seq whose newline sits at byte offset nl-at of its own line (seqs must be one digit)."
  [seqs nl-at trailing-newline?]
  (let [file (path/join (tu/tmp-dir) "events.edn")
        pad (- nl-at (count (pr-str {:seq 1 :pad ""})))
        body (str/join "\n" (map #(pr-str {:seq % :pad (apply str (repeat pad "p"))}) seqs))]
    (fs/mkdirSync (path/dirname file) #js {:recursive true})
    (fs/writeFileSync file (if trailing-newline? (str body "\n") body))
    file))

(deftest a-newline-at-an-exact-probe-or-chunk-offset-is-found
  (doseq [nl-at [511 512 513 1023 1024 1025 65535 65536 65537]]
    (let [file (write-exact-lines! (range 1 6) nl-at true)]
      (is (= [1 2 3 4 5] (seqs-after file 0 10)) (str "newline at " nl-at))
      (is (= [4 5] (seqs-after file 3 10)) (str "newline at " nl-at)))))

(deftest a-last-line-without-a-newline-is-read
  (doseq [nl-at [100 511 512 1024 65536]]
    (let [file (write-exact-lines! (range 1 4) nl-at false)]
      (is (= [1 2 3] (seqs-after file 0 10)) (str "line length " nl-at))
      (is (= [3] (seqs-after file 2 10)) (str "line length " nl-at)))))

(defn bytes-read-by
  "Run f counting the bytes it reads from the segments of file (readSync and
  readFileSync); returns the count."
  [file f]
  (let [segments (set (events/segment-files file))
        fds (atom #{})
        total (atom 0)
        {:keys [openSync readSync readFileSync]} {:openSync (.-openSync fs) :readSync (.-readSync fs)
                                                  :readFileSync (.-readFileSync fs)}]
    (set! (.-openSync fs) (fn [p & more]
                            (let [fd (apply openSync p more)]
                              (when (segments p) (swap! fds conj fd))
                              fd)))
    (set! (.-readSync fs) (fn [fd & more]
                            (let [n (apply readSync fd more)]
                              (when (@fds fd) (swap! total + n))
                              n)))
    (set! (.-readFileSync fs) (fn [p & more]
                                (let [r (apply readFileSync p more)]
                                  (when (segments p) (swap! total + (.-size (fs/statSync p))))
                                  r)))
    (try (f) @total
         (finally (set! (.-openSync fs) openSync)
                  (set! (.-readSync fs) readSync)
                  (set! (.-readFileSync fs) readFileSync)))))

(deftest make-reads-only-the-ends-of-large-segments
  (let [line (fn [n] (str (pr-str {:seq n :pad (apply str (repeat 1000 "p"))}) "\n"))
        dir (tu/tmp-dir)
        file (path/join dir "events.edn")
        rotated (events/segment-file file 1)]
    (fs/mkdirSync dir #js {:recursive true})
    (fs/writeFileSync rotated (apply str (map line (range 1 6001))))
    (fs/writeFileSync file (apply str (map line (range 6001 12001))))
    (let [stream (atom nil)
          read (bytes-read-by file #(reset! stream (events/make {:file file :generation-id "g"})))]
      (is (< read (+ (events/setting "recent-bytes") (* 4 events/read-chunk-bytes)))
          (str "make read " read " bytes of two ~6 MB segments"))
      (is (= 12000 (:seq (events/cursor @stream))))
      (is (= 1 (:oldest-seq @@stream)))
      (is (= [6001 12000] ((juxt :first-seq :last-seq) (last (:segments @@stream)))))
      (is (= 12001 (events/emit! @stream {:source :system :kind :started}))))))

(deftest an-active-segment-over-the-cap-is-cut-at-a-line-start
  (let [file (write-lines! (range 1 41) 100)
        stream (events/make {:file file :generation-id "g" :max-bytes 2048})
        kept (read-lines file)]
    (is (<= (total-bytes file) 2048))
    (is (= (range (:seq (first kept)) 41) (mapv :seq kept)) "whole lines, the newest kept")
    (is (= (:seq (first kept)) (:oldest-seq @stream)))
    (is (= 40 (:seq (events/cursor stream))))))

(deftest an-oversize-line-between-normal-ones-is-a-stub-that-keeps-the-seq-run
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g" :now (constantly 77)})
        sid (:stream-id (events/cursor stream))
        pad (apply str (repeat (* 2 1024 1024) "p"))]
    (events/emit! stream {:source :job :kind :queued :data {:i 1}})
    (events/emit! stream {:source :job :kind :memory_written :level :info :message "has :seq 99 in it" :data {:seq 98 :pad pad}})
    (events/emit! stream {:source :job :kind :done :data {:i 3}})
    (let [page (events/read-after (disk-only stream) {:stream-id sid :after 0 :limit 100})
          [a b c] (all-pages stream sid)]
      (is (false? (:gap? page)))
      (is (= [1 2 3] (mapv :seq (all-pages stream sid))) "no hole at the oversize line")
      (is (= {:seq 2 :time-ms 77 :source :job :kind :memory_written :level :info :truncated true}
             (dissoc b :data))
          "seq, time, source, kind and level survive; the payload is gone")
      (is (< (count (pr-str b)) 300))
      (is (= {:i 3} (:data c)) "normal lines still parse")
      (is (= {:i 1} (:data a))))
    (is (= [3] (mapv :seq (:events (events/read-after (disk-only stream) {:stream-id sid :after 2 :limit 100})))))))

(deftest a-restart-over-an-oversize-last-line-keeps-counting
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g"})
        pad (apply str (repeat (* 2 1024 1024) "p"))]
    (events/emit! stream {:source :job :kind :memory_written :data {:pad pad}})
    (let [again (events/make {:file file :generation-id "g2"})]
      (is (= 1 (:seq (events/cursor again))))
      (is (= 2 (events/emit! again {:source :job :kind :queued}))))))

(deftest a-job-id-and-a-chain-are-capped-when-written
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g"})
        id (apply str "j1" (repeat 5000 "/spare"))
        chain (mapv #(str "j" %) (range 2303))]
    (events/emit! stream {:source :job :kind :stopped :job id :chain chain})
    (let [ctx (:context (first (read-lines file)))]
      (is (<= (count (:job-id ctx)) 300))
      (is (str/starts-with? (:job-id ctx) "j1/spare"))
      (is (str/ends-with? (:job-id ctx) "/spare"))
      (is (<= (count (:chain ctx)) 8))
      (is (= ["j0" "j1"] (take 2 (:chain ctx))))
      (is (= "j2302" (last (:chain ctx)))))))

;; ---------------------------------------------------------------- :debug chain

(defn emit-pairs!
  "n rounds of one padded :debug event and one :info event."
  [stream n]
  (doseq [i (range n)]
    (events/emit! stream {:source :job :kind :glance :level :debug :data {:i i :pad (apply str (repeat 100 "d"))}})
    (events/emit! stream {:source :job :kind :done :level :info :data {:i i}})))

(defn kinds-after [stream after]
  (let [page (events/read-after stream {:stream-id (:stream-id (events/cursor stream)) :after after :limit 500})]
    [(:gap? page) (mapv :kind (:events page))]))

(deftest debug-events-rotate-by-size-in-their-own-chain-and-leave-the-main-chain-whole
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g" :debug-max-bytes 2048})]
    (emit-pairs! stream 60)
    (is (<= (total-bytes (events/debug-file file)) 2048))
    (is (= 60 (count (filter #{"done"} (map (comp name :kind) (read-lines file))))) "every :info event is kept")
    (is (empty? (filter #(= :glance (:kind %)) (read-lines file))) "no :debug event in the main chain")
    (disk-only stream)
    (let [[gap? kinds] (kinds-after stream 1)]
      (is (false? gap?) "expired :debug events are no gap")
      (is (= 60 (count (filter #{:done} kinds))))
      (is (< 0 (count (filter #{:glance} kinds)) 60)))))

(deftest debug-events-older-than-the-age-limit-are-dropped-on-restart
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g" :debug-max-age-ms 60000})
        old (/ (- (js/Date.now) 120000) 1000)]
    (emit-pairs! stream 5)
    (doseq [f (events/segment-files (events/debug-file file))
            :when (fs/existsSync f)]
      (fs/utimesSync f old old))
    (let [restarted (disk-only (events/make {:file file :generation-id "g" :debug-max-age-ms 60000}))]
      (is (zero? (.-size (fs/statSync (events/debug-file file)))))
      (is (= [false (vec (repeat 5 :done))] (kinds-after restarted 1)))
      (is (= 10 (:seq (events/cursor restarted))) "the high-water mark survives")
      (events/emit! restarted {:source :job :kind :glance :level :debug})
      (is (= [false [:done :glance]] (kinds-after restarted 9))))))

(deftest a-hole-below-the-newest-debug-event-is-no-gap
  (let [file (path/join (tu/tmp-dir) "events.edn")
        line (fn [n] (str (pr-str {:seq n :generation-id "g" :time-ms 0 :source :a :kind :b}) "\n"))
        _ (fs/mkdirSync (path/dirname file) #js {:recursive true})
        _ (fs/writeFileSync file (apply str (map line (concat (range 1 11) (range 12 21)))))
        _ (events/write-meta! file {:stream-id "s" :last-seq 20 :debug-seq 11})
        stream (disk-only (events/make {:file file :generation-id "g" :stream-id "s"}))]
    (is (false? (:gap? (events/read-after stream {:stream-id "s" :after 5 :limit 100}))))))

(deftest cursor-zero-on-a-log-that-starts-with-a-debug-event-is-no-gap
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g"})]
    (events/emit! stream {:source :job :kind :glance :level :debug})
    (events/emit! stream {:source :job :kind :done :level :info})
    (disk-only stream)
    (is (= [false [:glance :done]] (kinds-after stream 0)))))

(deftest the-metadata-is-not-rewritten-per-event-but-keeps-the-high-water-mark
  (let [file (path/join (tu/tmp-dir) "events.edn")
        stream (events/make {:file file :generation-id "g"})
        meta-seq #(:last-seq (events/read-edn-file (events/metadata-file file)))]
    (emit-n! stream 5)
    (is (<= (meta-seq) 1) "no rewrite per event")
    (emit-n! stream 3000)
    (is (< (meta-seq) 3005) "only rotations rewrite it")
    (is (= 3005 (:seq (events/cursor (events/make {:file file :generation-id "g2"})))))))
