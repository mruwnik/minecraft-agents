(ns dashboard.blueprint-library-test
  (:require [cljs.test :refer [deftest is testing]]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [dashboard.blueprint-library :as lib]
            [dashboard.blueprint :as view]
            [dashboard.blueprint-draft :as draft]))

(def tiny {:id "tiny" :front :south
           :key {"L" {:block "oak_log" :axis :y} "." :clear " " :clear
                 "=" {:block "oak_slab" :type :top :waterlogged true}}
           :layers [["L. " "=L."] ["..." " L."]]
           :spots {"bed" [0 0 0]} :note "Native source"})

(deftest native-source-detail-and-display
  (let [source (str "; source comment\n" (pr-str tiny))
        result (lib/detail source "tiny")]
    (is (= tiny (:bp result)))
    (is (= source (:source result)))
    (is (empty? (:errors result)))
    (is (= {"oak_log" 3 "oak_slab" 1} (get-in result [:bill :total])))
    (is (= 4 (count (:preview result))))
    (is (= "true" (get-in result [:preview 1 :states :waterlogged])))
    (is (= [[0 0.5 0 1 1 1]] (get-in result [:preview 1 :shapes])))
    (is (= "3x2x2" (view/footprint (:bp result))))
    (is (= [0 1] (vec (view/layer-ys tiny))))
    (is (= 6 (count (view/layer-cells tiny 0))))
    (is (= 3 (count (filter :air (view/layer-cells tiny 0)))))
    (is (= ["L" "="] (mapv :token (view/legend-rows tiny))))
    (is (= "oak_log[axis=y]" (:label (first (view/layer-cells tiny 0)))))
    (is (= source (draft/editable-source result)))
    (is (= {:filename "tiny.edn" :text (str source "\n")} (draft/download source)))))

(deftest invalid-native-palettes-and-unsupported-wants
  (doseq [[bp needle] [[(assoc-in tiny [:key "L"] {:crop "wheat"}) #"letter stands for"]
                       [(assoc-in tiny [:key "L"] {:tree "oak"}) #"letter stands for"]
                       [(assoc-in tiny [:key "L"] [:any "oak_log" "birch_log"]) #"letter stands for"]
                       [(update tiny :key dissoc " ") #"not in the key"]
                       [(assoc tiny :materials {:wood {}}) #"unknown key :materials"]
                       [(assoc tiny :id "other") #":id must equal"]]]
    (let [detail (lib/detail (pr-str bp) "tiny")]
      (is (nil? (:bp detail)))
      (is (some #(re-find needle %) (:errors detail)))
      (is (= (:errors detail) (mapv :error (:validation-errors detail))))
      (is (empty? (:preview detail)))))
  (is (seq (:errors (lib/preview "{" nil))))
  (is (seq (:errors (lib/preview (pr-str (dissoc tiny :id)) nil)))))

(deftest bounds-and-declared-stock
  (let [large (assoc tiny :layers [(vec (repeat 1001 (apply str (repeat 100 "L"))))] :spots {})]
    (is (re-find #"100000 cells" (first (:errors (lib/detail (pr-str large) "tiny"))))))
  (is (re-find #"2 MiB" (first (:errors (lib/detail (apply str (repeat (inc lib/max-source-bytes) "a")) "tiny")))))
  (let [result (lib/preview (pr-str tiny) "{\"oak_log\" 2 \"oak_slab\" 1}")]
    (is (= {"oak_log" 1} (get-in result [:stock :shortage])))
    (is (= tiny (:bp result))))
  (is (seq (:errors (lib/preview (pr-str tiny) "{:oak_log 3}"))))
  (is (seq (:errors (lib/preview (pr-str tiny) "{\"oak_log\" -1}")))))

(deftest canonical-library-ignores-json
  (let [dir (.mkdtempSync fs (.join path (.tmpdir os) "blueprints-edn-test-"))]
    (try
      (.writeFileSync fs (.join path dir "tiny.edn") (pr-str tiny))
      (.writeFileSync fs (.join path dir "tiny.blueprint.json") "invalid legacy JSON")
      (.writeFileSync fs (.join path dir "broken.edn") "{")
      (let [{:keys [blueprints errors]} (lib/library dir)]
        (is (= ["broken" "tiny"] (mapv :name blueprints)))
        (is (= tiny (:bp (last blueprints))))
        (is (= ["broken"] (mapv :name errors))))
      (finally (.rmSync fs dir #js {:recursive true :force true})))))

(deftest edn-draft-behaviour
  (is (= {:source (pr-str tiny) :stock nil} (draft/preview-body (pr-str tiny) " ")))
  (is (= "{bad" (:source (draft/preview-body "{bad" nil))))
  (is (thrown? js/Error (draft/download "{bad")))
  (is (thrown? js/Error (draft/download (pr-str (assoc-in tiny [:key "L"] {:crop "wheat"}))))))

(deftest strict-single-form-edn
  (doseq [source [(str (pr-str tiny) " {}") (str (pr-str tiny) " nil") ""]]
    (is (seq (:errors (lib/detail source "tiny"))))
    (is (seq (:errors (lib/preview source nil))))
    (is (thrown? js/Error (draft/download source))))
  (is (seq (:errors (lib/preview (pr-str tiny) "{\"oak_log\" 3} {}"))))
  (is (empty? (:errors (lib/detail (str (pr-str tiny) " ; trailing comment\n") "tiny")))))

(deftest aggregate-library-bounds
  (let [dir (.mkdtempSync fs (.join path (.tmpdir os) "blueprints-budget-test-"))
        big (assoc tiny :layers [(vec (repeat 600 (apply str (repeat 100 "L"))))] :spots {})]
    (try
      (.writeFileSync fs (.join path dir "first.edn") (pr-str (assoc big :id "first")))
      (.writeFileSync fs (.join path dir "second.edn") (pr-str (assoc big :id "second")))
      (let [result (lib/library dir)]
        (is (some? (:bp (first (:blueprints result)))))
        (is (nil? (:bp (second (:blueprints result)))))
        (is (re-find #"aggregate.*100000 cells" (first (:errors (second (:blueprints result)))))))
      (with-redefs [lib/max-library-files 1]
        (is (thrown? js/Error (lib/library dir))))
      (with-redefs [lib/max-library-bytes 100]
        (is (every? #(re-find #"aggregate.*32 MiB" (first (:errors %))) (:blueprints (lib/library dir)))))
      (finally (.rmSync fs dir #js {:recursive true :force true})))))

(deftest malformed-structure-is-reported
  (doseq [value [(assoc tiny :layers 1) (assoc tiny :layers [nil]) (assoc tiny :key {1 "stone"})]]
    (is (seq (:errors (lib/detail (pr-str value) "tiny"))))))
