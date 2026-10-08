(ns dashboard.goal-test
  (:require [cljs.test :refer [deftest is are]]
            ["fs" :as fs]
            ["os" :as os]
            ["path" :as path]
            [dashboard.goal :as goal]))

(defn temp-dir [] (fs/mkdtempSync (path/join (os/tmpdir) "goal-test-")))

(deftest a-goal-is-written-read-back-and-cleared
  (let [dir (temp-dir)]
    (is (nil? (goal/read-goal dir)))
    (is (= {:text "fence the pen" :by "Wren" :since 1000} (goal/write-goal! dir "fence the pen" "Wren" 1000)))
    (is (= {:text "fence the pen" :by "Wren" :since 1000} (goal/read-goal dir)))
    (is (= (path/join dir "goal.edn") (goal/goal-file dir)))
    (goal/clear-goal! dir)
    (is (nil? (goal/read-goal dir)))
    (goal/clear-goal! dir)
    (fs/rmSync dir #js {:recursive true})))

(deftest goal-text-is-one-short-line
  (are [in out] (= out (goal/clean-text in))
    "  dig a well \n now " "dig a well   now"
    "" nil
    "   " nil
    nil nil
    (apply str (repeat 300 "x")) (apply str (repeat goal/max-text "x"))))

(deftest a-blank-goal-is-refused
  (let [dir (temp-dir)]
    (is (thrown-with-msg? js/Error #"empty" (goal/write-goal! dir " " "Wren" 1)))
    (is (nil? (goal/read-goal dir)))
    (fs/rmSync dir #js {:recursive true})))

(deftest an-unreadable-or-foreign-goal-file-reads-as-none
  (let [dir (temp-dir)]
    (doseq [text ["{:text" "[1 2]" "{:text 5}" "{:by \"x\"}"]]
      (fs/writeFileSync (goal/goal-file dir) text)
      (is (nil? (goal/read-goal dir)) text))
    (fs/writeFileSync (goal/goal-file dir) "{:text \"ok\"}")
    (is (= {:text "ok"} (goal/read-goal dir)))
    (fs/rmSync dir #js {:recursive true})))

(deftest a-goal-keeps-its-optional-wait-text
  (let [dir (temp-dir)]
    (is (= {:text "t" :by "w" :since 5 :wait "waiting: reflex.ended"} (goal/write-goal! dir "t" "w" 5 "waiting: reflex.ended")))
    (is (= "waiting: reflex.ended" (:wait (goal/read-goal dir))))
    (is (= {:text "t" :since 5} (goal/write-goal! dir "t" nil 5 "  ")))))
