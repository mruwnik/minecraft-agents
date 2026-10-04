(ns engine.planner-fixture
  "Shared helpers for tests of the ClojureScript planner (engine.path.planner-tuned) over the cljs fixtures
  (engine.path.fixture, engine.path.courses): worlds from fills, plan and create-search taking and returning plain
  cljs data with the JS planner's result field names, and the small builders the JS planner tests use."
  (:require [clojure.string :as str]
            [engine.path.courses :as courses]
            [engine.path.fixture :as fx]
            [engine.path.planner-tuned :as planner]
            [engine.test-util :as tu]))

(def blocks (delay (tu/require-here "./js/path/blocks.mjs")))
(def space (delay (tu/require-here "./js/path/space.mjs")))
(def table (delay (.defaultStateTable ^js @blocks)))

(defn kebab [s] (keyword (str/lower-case (str/replace s "_" "-"))))

(def MOVE
  "the move codes of the planner's MOVE by lower-kebab name: :start :walk :diagonal :jump :climb-up ..."
  (into {} (map (fn [k] [(kebab k) (aget planner/MOVE k)])) (js/Object.keys planner/MOVE)))

(def default-costs
  "the planner's DEFAULT_COSTS as a cljs map with the JS key names (:climbUp, :openRedstone ...)"
  (js->clj planner/DEFAULT-COSTS :keywordize-keys true))

(defn snapshot
  "fixtureSnapshot over cljs fills [x0 y0 z0 x1 y1 z1 name props] and single blocks [x y z name props]."
  [{:keys [fill blocks]}]
  (fx/fixture-snapshot {:fill (or fill []) :blocks (or blocks [])}))

(def stone-floor [-2 60 -2 40 63 40 "stone"])

(defn world
  "The JS tests' world: a stone floor whose top face is y=64 over x,z -2..40, plus :fill and :blocks."
  [{:keys [fill blocks]}]
  (snapshot {:fill (into [stone-floor] fill) :blocks blocks}))

(defn options-js
  "options as the JS plan() takes them, with the table and the free-space module a pathWorld carries."
  [options]
  (let [o (clj->js options)]
    (js/Object.assign #js {:table @table :space @space} o)))

(defn plan
  "planner-tuned/plan over a snapshot: query {:from {:x :y :z} :goal {:kind ...}} and options are cljs maps (options
  default to the table and space modules); the result is cljs data, key for key the JS planner's result."
  ([snapshot query] (plan snapshot query {}))
  ([snapshot query options]
   (-> (planner/plan snapshot (clj->js query) (options-js options))
       (js->clj :keywordize-keys true))))

(defn create-search
  "{:step (fn [max-expansions]) :result (fn [] cljs-result) :nearest (fn [] cljs)} over planner-tuned/create-search."
  ([snapshot query] (create-search snapshot query {}))
  ([snapshot query options]
   (let [^js s (planner/create-search snapshot (clj->js query) (options-js options))]
     {:step (fn [n] (.step s n))
      :result (fn [] (js->clj (.result s) :keywordize-keys true))
      :nearest (fn [] (js->clj (.nearest s) :keywordize-keys true))})))

(defn course-plan
  "Plan a named course (engine.path.courses) from its start to its go-to goal; options as plan."
  ([name] (course-plan name {}))
  ([name options]
   (let [{:keys [snapshot from goal]} (courses/course-snapshot name)]
     (plan snapshot {:from from :goal goal} options))))

(defn near ([x y z] (near x y z 0)) ([x y z range] {:kind "near" :x x :y y :z z :range range}))
(defn xz [x z range] {:kind "xz" :x x :z z :range range})

(def start {:x 2 :y 64 :z 2})

(defn run
  "plan from `from` (default start) to goal."
  ([snapshot goal] (run snapshot goal {} start))
  ([snapshot goal options] (run snapshot goal options start))
  ([snapshot goal options from] (plan snapshot {:from from :goal goal} options)))

(defn search
  "run with the goal flood off, to test what the main search does."
  ([snapshot goal] (search snapshot goal {} start))
  ([snapshot goal options] (search snapshot goal options start))
  ([snapshot goal options from] (run snapshot goal (assoc options :goalFlood 0) from)))

(defn cells [r] (mapv (juxt :x :y :z) (get-in r [:path :steps])))
(defn moves [r] (mapv :move (get-in r [:path :steps])))
(defn last-cell [r] (peek (cells r)))
