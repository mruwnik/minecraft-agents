(ns engine.settings-test
  (:require-macros [engine.registry :refer [settings-registry]])
  (:require [cljs.test :refer [deftest is async]]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.expr :as expr]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.settings-registry :as settings-registry]
            [engine.test-util :as tu]
            [settings-demo.declares :as demo]
            ["fs" :as fs]
            ["path" :as path]))

(def specs
  (a/declare-settings!
   {:jobs.demo/wait-ms {:default 5000 :doc "How long to wait." :spec (a/int-in 0 nil)}
    :jobs.demo/label {:default "a" :doc "A label." :spec string?}}))

(defn write! [dir name text]
  (let [f (path/join dir name)]
    (fs/writeFileSync f text)
    f))

(defn load-with
  "load! with the files given as EDN text (nil = no file); [bad-events resolved]."
  ([world body] (load-with world body "Bob"))
  ([world body name]
   (let [dir (tu/tmp-dir)
         events (atom [])
         files {:world-file (path/join dir "world.edn") :body-file (path/join dir "body.edn")}]
     (when world (write! dir "world.edn" world))
     (when body (write! dir "body.edn" body))
     (settings/load! (assoc files :specs specs :body name :emit #(swap! events conj %)))
     [@events (settings/resolved) files])))

(deftest a-scope-overrides-get-for-its-awaits-and-nests-then-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [wait #(settings/get specs :jobs.demo/wait-ms)
              later (fn ^:async later [] (await (js/Promise. (fn [ok] (js/setTimeout ok 5)))) (wait))]
          (is (= 5000 (wait)))
          (is (= [1 1 2 1 5000]
                 [(settings/scoped {:jobs.demo/wait-ms 1} wait)
                  (await (settings/scoped {:jobs.demo/wait-ms 1} later))
                  (settings/scoped {:jobs.demo/wait-ms 1} #(settings/scoped {:jobs.demo/wait-ms 2} wait))
                  (settings/scoped {:jobs.demo/wait-ms 1} #(settings/scoped {} wait))
                  (wait)])
              "own await keeps it; an inner scope wins; an empty one inherits; none outlives its run")
          (is (= [1 2] (js->clj (await (js/Promise.all #js [(settings/scoped {:jobs.demo/wait-ms 1} later)
                                                   (settings/scoped {:jobs.demo/wait-ms 2} later)]))))
              "interleaved runs keep their own"))))))

(deftest get-reads-the-default-and-throws-on-an-unknown-key
  (settings/with-settings {}
    (fn []
      (is (= 5000 (settings/get specs :jobs.demo/wait-ms)))
      (is (thrown-with-msg? js/Error #"unknown setting" (settings/get specs :jobs.demo/typo))))))

(deftest layers-code-world-body-with-provenance
  (settings/with-settings {}
    (fn []
      (let [[events res] (load-with "{:jobs.demo/wait-ms 10 :jobs.demo/label \"w\"}" "{:jobs.demo/wait-ms 20}")]
        (is (empty? events))
        (is (= 20 (settings/get specs :jobs.demo/wait-ms)))
        (is (= "w" (settings/get specs :jobs.demo/label)))
        (is (= {:value 20 :layer :body :doc "How long to wait."} (get res :jobs.demo/wait-ms)))
        (is (= {:value "w" :layer :world :doc "A label."} (get res :jobs.demo/label)))))))

(deftest a-bad-value-keeps-the-lower-layer-and-emits-one-info
  (settings/with-settings {}
    (fn []
      (let [[events res] (load-with "{:jobs.demo/wait-ms 10}" "{:jobs.demo/wait-ms -5 :jobs.demo/label \"b\"}")]
        (is (= 10 (settings/get specs :jobs.demo/wait-ms)))
        (is (= :world (get-in res [:jobs.demo/wait-ms :layer])))
        (is (= "b" (settings/get specs :jobs.demo/label)))
        (is (= 1 (count events)))
        (is (= {:kind (keyword "settings.bad") :level :info :source :system} (select-keys (first events) [:kind :level :source])))
        (is (= :jobs.demo/wait-ms (:key (first events))))))))

(deftest a-wrong-type-and-an-unknown-key-are-bad
  (settings/with-settings {}
    (fn []
      (let [[events _] (load-with nil "{:jobs.demo/wait-ms \"x\" :jobs.demo/nope 1}")]
        (is (= 5000 (settings/get specs :jobs.demo/wait-ms)))
        (is (= #{:jobs.demo/wait-ms :jobs.demo/nope} (set (map :key events))))
        (is (every? #(= :info (:level %)) events))
        (is (str/ends-with? (:text (first (filter #(= :jobs.demo/nope (:key %)) events))) ":jobs.demo/nope: unknown key; ignored"))))))

(deftest an-unreadable-file-emits-one-info-and-keeps-the-lower-layer
  (settings/with-settings {}
    (fn []
      (let [[events _] (load-with "{:jobs.demo/wait-ms 10}" "{:jobs.demo/wait-ms ")]
        (is (= 10 (settings/get specs :jobs.demo/wait-ms)))
        (is (= 1 (count events)))
        (is (str/includes? (:text (first events)) "body.edn")))
      (let [[events _] (load-with "[1 2]" nil)]
        (is (= 1 (count events)))
        (is (= 5000 (settings/get specs :jobs.demo/wait-ms)))))))

(deftest reload-picks-up-an-edit
  (settings/with-settings {}
    (fn []
      (let [[_ _ files] (load-with nil "{:jobs.demo/wait-ms 20}")]
        (is (= 20 (settings/get specs :jobs.demo/wait-ms)))
        (fs/writeFileSync (:body-file files) "{:jobs.demo/wait-ms 30}")
        (settings/load! (assoc files :specs specs :body "Bob" :emit (fn [_])))
        (is (= 30 (settings/get specs :jobs.demo/wait-ms)))
        (fs/unlinkSync (:body-file files))
        (settings/load! (assoc files :specs specs :body "Bob" :emit (fn [_])))
        (is (= 5000 (settings/get specs :jobs.demo/wait-ms)))))))

(deftest a-second-body-in-the-process-throws
  (settings/with-settings {}
    (fn []
      (load-with nil nil "Bob")
      (is (thrown-with-msg? js/Error #"second body" (load-with nil nil "Amy"))))))

(deftest with-settings-restores-after-a-throw
  (settings/with-settings {}
    (fn []
      (is (thrown? js/Error (settings/with-settings {:jobs.demo/wait-ms 1}
                              (fn [] (is (= 1 (settings/get specs :jobs.demo/wait-ms))) (throw (js/Error. "boom"))))))
      (is (= 5000 (settings/get specs :jobs.demo/wait-ms))))))

(deftest with-settings-restores-after-a-promise-settles
  (async done
    (settings/with-settings {}
      (fn []
        (-> (settings/with-settings {:jobs.demo/wait-ms 1}
              (fn [] (-> (js/Promise.resolve 1) (.then (fn [_] (settings/get specs :jobs.demo/wait-ms))))))
            (.then (fn [v]
                     (is (= 1 v))
                     (is (= 5000 (settings/get specs :jobs.demo/wait-ms)))
                     (done))))))))

;; ---- the declared keys of the code base

(def all-specs (merge registry/settings settings/settings))

(defn key-problems
  "Strings naming each key of by-ns that is outside its declaring namespace, duplicated or has a default that does
  not fit its spec."
  [by-ns]
  (let [pairs (for [[sym s] by-ns [k v] s] [k sym v])
        misplaced (for [[k sym _] pairs
                        :let [ok (if (str/starts-with? (str sym) "engine.") (str/starts-with? (namespace k) "engine.") (= (str sym) (namespace k)))]
                        :when (not ok)]
                    (str k " declared in " sym))
        dups (for [[k n] (frequencies (map first pairs)) :when (> n 1)] (str k " declared twice"))
        bad-defaults (for [[k _ spec] pairs :when (a/problem k (:default spec))] (str k " default"))]
    (concat misplaced dups bad-defaults)))

(deftest declared-keys-are-in-place-unique-and-fit-their-spec
  (is (empty? (key-problems (merge registry/settings-by-ns settings-registry/settings-by-ns)))))

(def int-key {:default 1 :spec (a/int-in nil nil)})

(deftest key-problems-names-each-kind-of-problem
  (is (= [":jobs.a/x declared in jobs.b"] (key-problems {'jobs.b {:jobs.a/x int-key}})))
  (is (some #{":jobs.a/x declared twice"} (key-problems {'jobs.a {:jobs.a/x int-key} 'jobs.b {:jobs.a/x int-key}})))
  (is (= [":jobs.a/x default"] (key-problems {'jobs.a (a/declare-settings! {:jobs.a/x {:default "s" :spec (a/int-in nil nil)}})}))))

(def demo-by-ns (settings-registry "settings_demo"))

(deftest settings-registry-collects-a-real-declaring-namespace
  (is (= {'settings-demo.declares demo/settings} demo-by-ns))
  (is (empty? (key-problems demo-by-ns))))

(defn source-files [dir]
  (mapcat (fn [e] (let [p (path/join dir (.-name e))]
                    (if (.isDirectory e) (source-files p) [p])))
          (.readdirSync fs dir #js {:withFileTypes true})))

(defn settings-aliases
  "The names a source text calls engine.settings by: its :as alias and the full name."
  [text]
  (conj (set (map second (re-seq #"\[engine\.settings :as ([^\s\]]+)" text))) "engine.settings"))

(deftest settings-get-is-never-read-in-a-top-level-def
  (let [bad (for [f (source-files "src")
                  :when (re-find #"\.cljs$" f)
                  :let [text (fs/readFileSync f "utf8")
                        reads (re-pattern (str "(^|[^\\w.-])(" (str/join "|" (map #(str/replace % "." "\\.") (settings-aliases text))) ")/get[\\s)]"))]
                  form (str/split text #"\n(?=\()")
                  :when (and (re-find #"^\((def|defonce) " form) (re-find reads form))]
              f)]
    (is (empty? bad))))


(deftest every-engine-namespace-declaring-settings-is-gathered
  (let [declaring (set (for [f (source-files "src/engine")
                             :when (and (re-find #"\.cljs$" f) (re-find #"(?m)^\(def settings\b" (fs/readFileSync f "utf8")))]
                         (-> f (str/replace #"^src/" "") (str/replace #"\.cljs$" "") (str/replace "/" ".") (str/replace "_" "-") symbol)))
        gathered (set (keys settings-registry/settings-by-ns))]
    (is (seq declaring))
    (is (empty? (remove gathered (disj declaring 'engine.registry 'engine.settings-registry 'engine.main))))))

(deftest a-body-file-with-a-low-events-cap-is-bad-and-keeps-the-default
  (settings/with-settings {}
    (fn []
      (let [dir (tu/tmp-dir)
            events (atom [])
            specs (merge registry/settings settings-registry/settings)]
        (write! dir "body.edn" "{:engine.events/max-bytes 1023}")
        (settings/load! {:specs specs :world-file (path/join dir "none.edn") :body-file (path/join dir "body.edn")
                         :body "Bob" :emit #(swap! events conj %)})
        (is (= [:engine.events/max-bytes] (map :key @events)))
        (is (= 67108864 (settings/get specs :engine.events/max-bytes)))
        (write! dir "body.edn" "{:engine.events/max-bytes 4096}")
        (settings/load! {:specs specs :world-file (path/join dir "none.edn") :body-file (path/join dir "body.edn")
                         :body "Bob" :emit #(swap! events conj %)})
        (is (= 4096 (settings/get specs :engine.events/max-bytes)))))))

(deftest js-specs-and-reader-follow-a-reload
  (settings/with-settings {}
   (fn []
  (let [js (settings/js-specs #js {"prim/demo-ms" #js {:default 50 :spec #js ["int-in" 1 nil] :doc "A demo wait."}})
        dir (tu/tmp-dir)
        load! #(settings/load! {:specs js :body "Bob" :world-file (write! dir "world.edn" %) :body-file nil :emit (fn [_])})]
    (is (= {:default 50 :doc "A demo wait."} (select-keys (:prim/demo-ms js) [:default :doc])))
    (load! "{}")
    (let [read (settings/js-reader)]
      (is (= 50 (read "prim/demo-ms")))
      (is (nil? (read "prim/unknown")))
      (load! "{:prim/demo-ms 7}")
      (is (= 7 (read "prim/demo-ms")) "a reload reaches the reader at its next call")
      (load! "{:prim/demo-ms 0}")
      (is (= 50 (read "prim/demo-ms")) "a value outside the spec keeps the default")
      (is (= 9 (settings/scoped {:prim/demo-ms 9} #(read "prim/demo-ms"))) "a scoped override beats the kept answer")
      (is (= 50 (read "prim/demo-ms")) "and does not stay in it")
      (let [later (settings/js-reader)]
        (is (= 50 (later "prim/demo-ms")))
        (load! "{:prim/demo-ms 8}")
        (is (= 8 (read "prim/demo-ms")) "a reader built earlier still sees a reload after a later js-reader call")))))))

(defn load-args [dir]
  {:world-file (path/join dir "world.edn") :body-file (path/join dir "body.edn") :specs specs :body "Bob"})

(deftest reload-re-reads-the-files-load-was-given
  (settings/with-settings {}
    (fn []
      (let [[_ _ files] (load-with nil "{:jobs.demo/wait-ms 20}")
            events (atom [])]
        (fs/writeFileSync (:body-file files) "{:jobs.demo/wait-ms 30 :jobs.demo/typo 1}")
        (settings/reload! #(swap! events conj %))
        (is (= 30 (settings/get specs :jobs.demo/wait-ms)))
        (is (= 1 (count @events)))))))

(deftest set-writes-the-body-file-validates-and-reloads
  (settings/with-settings {}
    (fn []
      (let [[_ _ files] (load-with nil nil)
            emit (fn [_])]
        (is (= {:ok true} (settings/set-key! :jobs.demo/wait-ms 70 emit)))
        (is (= 70 (settings/get specs :jobs.demo/wait-ms)))
        (is (= {:jobs.demo/wait-ms 70} (cljs.reader/read-string (fs/readFileSync (:body-file files) "utf8"))))
        (is (= :body (get-in (settings/resolved) [:jobs.demo/wait-ms :layer])))
        (is (= {:ok true} (settings/set-key! :jobs.demo/label "x" emit)))
        (is (= {:jobs.demo/wait-ms 70 :jobs.demo/label "x"} (cljs.reader/read-string (fs/readFileSync (:body-file files) "utf8"))))))))

(deftest set-and-unset-keep-the-files-comments-and-write-no-temp-file
  (settings/with-settings {}
    (fn []
      (let [[_ _ files] (load-with nil (str ";; mine\n{:jobs.demo/wait-ms 20 ; slow\n :jobs.demo/label \"a;}\"\n ;; tail\n}\n"))
            emit (fn [_])
            read #(fs/readFileSync (:body-file files) "utf8")]
        (is (= {:ok true} (settings/set-key! :jobs.demo/wait-ms 70 emit)))
        (is (= 70 (settings/get specs :jobs.demo/wait-ms)))
        (is (= ";; mine\n{:jobs.demo/wait-ms 70 ; slow\n :jobs.demo/label \"a;}\"\n ;; tail\n}\n" (read)))
        (is (= {:ok true} (settings/unset-key! :jobs.demo/label emit)))
        (is (= ";; mine\n{:jobs.demo/wait-ms 70 ; slow\n ;; tail\n}\n" (read)))
        (is (= {:ok true} (settings/set-key! :jobs.demo/label "x" emit)))
        (is (= ";; mine\n{:jobs.demo/wait-ms 70 :jobs.demo/label \"x\" ; slow\n ;; tail\n}\n" (read)))
        (is (= ["body.edn"] (js->clj (fs/readdirSync (path/dirname (:body-file files)))))
            "the temp file is gone")))))

(deftest set-refuses-a-bad-key-or-value-and-leaves-the-file
  (settings/with-settings {}
    (fn []
      (let [[_ _ files] (load-with nil "{:jobs.demo/wait-ms 20}")
            emit (fn [_])
            before (fs/readFileSync (:body-file files) "utf8")
            unknown (settings/set-key! :jobs.demo/typo 1 emit)
            wrong (settings/set-key! :jobs.demo/wait-ms -5 emit)]
        (is (= false (:ok unknown)))
        (is (re-find #"unknown" (:reason unknown)))
        (is (= false (:ok wrong)))
        (is (re-find #"-5" (:reason wrong)))
        (is (= before (fs/readFileSync (:body-file files) "utf8")))
        (is (= 20 (settings/get specs :jobs.demo/wait-ms)))))))

(deftest set-refuses-an-unreadable-body-file
  (settings/with-settings {}
    (fn []
      (let [[_ _ files] (load-with nil "{:jobs.demo/wait-ms")
            r (settings/set-key! :jobs.demo/wait-ms 5 (fn [_]))]
        (is (= false (:ok r)))
        (is (= "{:jobs.demo/wait-ms" (fs/readFileSync (:body-file files) "utf8")))))))

(deftest unset-removes-the-key-from-the-body-file
  (settings/with-settings {}
    (fn []
      (let [[_ _ files] (load-with nil "{:jobs.demo/wait-ms 20 :jobs.demo/label \"x\"}")
            emit (fn [_])]
        (is (= {:ok true} (settings/unset-key! :jobs.demo/wait-ms emit)))
        (is (= 5000 (settings/get specs :jobs.demo/wait-ms)))
        (is (= {:jobs.demo/label "x"} (cljs.reader/read-string (fs/readFileSync (:body-file files) "utf8"))))
        (is (= false (:ok (settings/unset-key! :jobs.demo/typo emit))))))))

(deftest settings-view-carries-the-rates
  (settings/with-settings {}
    (fn []
      (load-with nil nil)
      (settings/set-clock! 40 false :packet)
      (let [v (settings/view #js {:physicsMs (fn [] 25)})]
        (is (= 40 (:game-rate v)))
        (is (= :packet (:rate-source v)))
        (is (= false (:frozen? v)))
        (is (= 25 (:physics-ms v)))
        (is (= 5000 (get-in v [:settings :jobs.demo/wait-ms :value]))))
      (settings/set-clock! 20 false :assumed))))

(def job-specs
  (a/declare-settings! {:engine.settings/job-args {:default {} :doc "d" :spec (a/map-of symbol? map?)}}))

(deftest job-arg-overrides-layer-by-job-and-arg-and-bad-ones-are-dropped
  (settings/with-settings {}
    (fn []
      (let [dir (tu/tmp-dir)
            events (atom [])
            world (write! dir "world.edn" "{:engine.settings/job-args {jobs.explore.look {:radius 20 :max-blocks 99 :max-entities 3} jobs.nowhere {:x 1} jobs.farm.find-spot {:range 3}}}")
            body (write! dir "body.edn" "{:engine.settings/job-args {jobs.explore.look {:radius 24 :typo 1 :useful? nil}}}")]
        (settings/load! {:specs (merge specs job-specs) :jobs registry/jobs :world-file world :body-file body :body "Bob"
                         :emit #(swap! events conj %)})
        (is (= {:radius 24 :max-entities 3} (settings/job-overlay 'jobs.explore.look)) "bad args dropped, the body wins per arg")
        (is (= {:range 3} (settings/job-overlay 'jobs.farm.find-spot)))
        (is (= [:body :world :code] (map #(settings/job-arg-layer 'jobs.explore.look %) [:radius :max-entities :useful?])))
        (is (= 4 (count @events)) "unknown job, unknown arg, nil value, out-of-range value")
        (is (every? #(= {:kind (keyword "settings.bad") :level :info} (select-keys % [:kind :level])) @events))))))

(deftest an-override-changes-the-default-an-explicit-arg-beats-it-and-a-frozen-instance-keeps-it
  (let [leaf-args #(second (expr/leaf registry/jobs 'jobs.explore.look %))
        overlay {:engine.settings/job-args {'jobs.explore.look {:radius 24}}}
        frozen (settings/with-settings overlay #(leaf-args {}))]
    (is (= 16 (:radius (settings/with-settings {} #(leaf-args {})))))
    (is (= 24 (:radius frozen)))
    (is (= 8 (:radius (settings/with-settings overlay #(leaf-args {:radius 8})))))
    (is (= 24 (:radius frozen)) "the args built before a reload stay as they were")
    (is (= 16 (:radius (settings/with-settings {} #(leaf-args {})))) "a job built after the override is gone has the code default")))
