(ns agent-tools.goal-test
  (:require [cljs.test :refer [deftest is]]
            [cljs.reader :as reader]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [agent-tools.goal :as goal]))

(defn worlds-with-body []
  (let [worlds (fs/mkdtempSync (path/join (os/tmpdir) "goal-tool-"))]
    (fs/mkdirSync (path/join worlds "w" "agents" "Wren") #js {:recursive true})
    worlds))

(defn run [argv]
  (let [out (atom [])
        write (.-write (.-stdout js/process))]
    (set! (.-write (.-stdout js/process)) (fn [s] (swap! out conj s) true))
    (let [code (try (goal/main! argv) (finally (set! (.-write (.-stdout js/process)) write)))]
      [code (reader/read-string (apply str @out))])))

(deftest the-goal-tool-sets-shows-and-clears-a-body-goal
  (let [worlds (worlds-with-body)
        base ["Wren" "--world" "w" "--worlds" worlds]]
    (is (= [0 {:ok true :goal nil}] (run base)))
    (let [[code {:keys [ok goal]}] (run (conj base "fence the north pen"))]
      (is (= [0 true] [code ok]))
      (is (= "fence the north pen" (:text goal)))
      (is (= "Wren" (:by goal)))
      (is (number? (:since goal))))
    (is (= "fence the north pen" (get-in (run base) [1 :goal :text])))
    (is (= "operator" (get-in (run (into base ["--by" "operator" "check the farm"])) [1 :goal :by])))
    (is (= [0 {:ok true :goal nil :cleared true}] (run (conj base "--clear"))))
    (is (not (fs/existsSync (path/join worlds "w" "agents" "Wren" "goal.edn"))))
    (fs/rmSync worlds #js {:recursive true})))

(deftest the-goal-tool-refuses-bad-arguments
  (let [worlds (worlds-with-body)]
    (doseq [[argv reason] [[["Nobody" "--world" "w" "--worlds" worlds "x"] :no-body]
                           [["Wren" "--world" "w" "--worlds" worlds "   "] :bad-args]
                           [["Wren" "--world" "w" "--worlds" worlds "a" "b"] :bad-args]
                           [["Wren" "--world" "w" "--worlds" worlds "--clear" "x"] :bad-args]
                           [["Wren" "--worlds" worlds "x"] :bad-args]]]
      (let [[code out] (run argv)]
        (is (= 2 code) (pr-str argv))
        (is (= {:ok false :reason reason} (select-keys out [:ok :reason])) (pr-str argv))))
    (fs/rmSync worlds #js {:recursive true})))
