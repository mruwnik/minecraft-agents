(ns agent-tools.plans-test
  (:require [cljs.test :refer [deftest are]]
            [agent-tools.plans :as plans]))

(deftest raw-fragments-keep-dollar-sequences-verbatim
  (are [text] (= (str "{:doc " text "\n}\n") (plans/print-value {:doc (plans/raw-edn text)} false))
    "\"cost $& more\""
    "\"$1 and $$ and $`\""
    "; note $' here\n\"x\""))
