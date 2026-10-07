(ns engine.settings-test
  (:require-macros [engine.registry :refer [settings-registry]])
  (:require [cljs.test :refer [deftest is async]]
            [clojure.string :as str]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.settings-registry :as settings-registry]
            [engine.test-util :as tu]
            [settings-demo.declares :as demo]
            ["fs" :as fs]
            ["path" :as path]))

(def specs
  {:jobs.demo/wait-ms {:default 5000 :doc "How long to wait." :type :int :min 0}
   :jobs.demo/label {:default "a" :doc "A label." :type :string}})

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

(deftest no-files-leave-the-code-defaults
  (settings/with-settings {}
    (fn []
      (let [[events res] (load-with nil nil)]
        (is (empty? events))
        (is (= 5000 (settings/get specs :jobs.demo/wait-ms)))
        (is (= :code (get-in res [:jobs.demo/wait-ms :layer])))))))

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
        bad-defaults (for [[k _ spec] pairs :when (settings/spec-problem spec (:default spec))] (str k " default"))]
    (concat misplaced dups bad-defaults)))

(deftest declared-keys-are-in-place-unique-and-fit-their-spec
  (is (empty? (key-problems (merge registry/settings-by-ns settings-registry/settings-by-ns)))))

(def int-key {:default 1 :type :int})

(deftest key-problems-names-each-kind-of-problem
  (is (= [":jobs.a/x declared in jobs.b"] (key-problems {'jobs.b {:jobs.a/x int-key}})))
  (is (some #{":jobs.a/x declared twice"} (key-problems {'jobs.a {:jobs.a/x int-key} 'jobs.b {:jobs.a/x int-key}})))
  (is (= [":jobs.a/x default"] (key-problems {'jobs.a {:jobs.a/x {:default "s" :type :int}}}))))

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
