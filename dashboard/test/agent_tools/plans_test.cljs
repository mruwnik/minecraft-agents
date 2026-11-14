(ns agent-tools.plans-test
  (:require [cljs.test :refer [deftest are]]
            [agent-tools.plans :as plans]))

(deftest raw-fragments-keep-dollar-sequences-verbatim
  (are [text] (= (str "{:doc " text "\n}\n") (plans/print-value {:doc (plans/raw-edn text)} false))
    "\"cost $& more\""
    "\"$1 and $$ and $`\""
    "; note $' here\n\"x\""))

(deftest check-route-and-adopt-need-a-body-as-the-help-says
  (are [argv] (re-find #"need --body" (str (:error (plans/request-for :plan (clj->js argv)))))
    ["check" "p1" "--world" "w"]
    ["route" "p1" "--world" "w" "--from=0,0,0" "--to=1,0,0" "--by" "x"]
    ["adopt" "p1" "--world" "w" "--box=0,0,0:1,1,1" "--by" "x"]))
