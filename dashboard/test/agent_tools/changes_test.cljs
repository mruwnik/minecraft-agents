(ns agent-tools.changes-test
  (:require [cljs.test :refer [deftest are]]
            [agent-tools.changes :as changes]))

(deftest exit-code-is-one-only-for-a-refused-answer
  (are [result expected] (= expected (changes/exit-code-for result))
    {:ok false :reason :cursor-gap} 1
    {:cursor {:seq 3} :items []} 0
    {:ok true} 0
    nil 0))
