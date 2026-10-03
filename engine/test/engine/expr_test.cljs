(ns engine.expr-test
  (:require [cljs.test :refer [deftest is are]]
            [cljs.reader :as reader]
            [engine.expr :as expr]))

(defn ^:async noop-round [_] :done)

(def reg
  {'jobs.a {:check (constantly true) :round noop-round
            :args {:n {:doc "n" :default 1} :m {:doc "m" :default 2}}}
   'jobs.b {:check (constantly true) :round noop-round}})

(defn problem [form]
  (try (expr/parse-spec reg form) nil
       (catch :default e (ex-message e))))

(deftest a-leaf-merges-its-args-over-the-defaults
  (is (= {:op :leaf :job 'jobs.a :args {:n 1 :m 2}} (expr/parse reg '(jobs.a))))
  (is (= {:op :leaf :job 'jobs.a :args {:n 5 :m 2 :x 0}} (expr/parse reg '(jobs.a {:n 5 :x 0}))))
  (is (= {:op :leaf :job 'jobs.b :args {}} (expr/parse reg '(jobs.b)))))

(deftest combinators-nest
  (is (= {:op :seq :children [{:op :leaf :job 'jobs.b :args {}}
                              {:op :repeat :child {:op :any :children [{:op :leaf :job 'jobs.b :args {}}]}}]}
         (expr/parse reg '(seq (jobs.b) (repeat (any (jobs.b))))))))

(deftest hold-is-a-flag-on-the-top
  (is (= {:node {:op :leaf :job 'jobs.b :args {}} :hold? true} (expr/parse-spec reg '(hold (jobs.b)))))
  (is (= {:node {:op :leaf :job 'jobs.b :args {}} :hold? false} (expr/parse-spec reg '(jobs.b)))))

(deftest backoff-wraps-the-top-in-either-order-with-hold
  (let [node {:op :leaf :job 'jobs.b :args {}}]
    (are [form expected] (= expected (expr/parse-spec reg form))
      '(backoff {:after 2} (jobs.b)) {:node node :hold? false :backoff {:after 2}}
      '(backoff false (jobs.b)) {:node node :hold? false :backoff false}
      '(hold (backoff {:after 2} (jobs.b))) {:node node :hold? true :backoff {:after 2}}
      '(backoff {:after 2} (hold (jobs.b))) {:node node :hold? true :backoff {:after 2}})))

(deftest specs-read-from-edn
  (is (= {:op :seq :children [{:op :leaf :job 'jobs.a :args {:n 3 :m 2}}]}
         (expr/parse reg (reader/read-string "(seq (jobs.a {:n 3}))")))))

(deftest bad-specs-are-refused-with-a-message
  (is (re-find #"unknown job or combinator jobs.nope" (problem '(jobs.nope))))
  (is (re-find #"unknown job or combinator map" (problem '(map (jobs.a)))))
  (is (re-find #"hold is only allowed" (problem '(seq (hold (jobs.a))))))
  (is (re-find #"repeat takes exactly one" (problem '(repeat (jobs.a) (jobs.b)))))
  (is (re-find #"hold takes exactly one" (problem '(hold))))
  (is (re-find #"backoff is only allowed" (problem '(seq (backoff {} (jobs.a))))))
  (is (re-find #"backoff takes a config map" (problem '(backoff (jobs.a)))))
  (is (re-find #":backoff must be" (problem '(backoff {:after 0} (jobs.a)))))
  (is (re-find #"backoff is not allowed here" (try (expr/parse reg '(backoff {} (jobs.a))) (catch :default e (ex-message e)))))
  (is (re-find #"seq takes at least one" (problem '(seq))))
  (is (re-find #"any takes at least one" (problem '(any))))
  (is (re-find #"args of jobs.a must be a map" (problem '(jobs.a 5))))
  (is (re-find #"jobs.a takes at most one args map" (problem '(jobs.a {} {}))))
  (is (re-find #"a job spec is a list" (problem 'jobs.a)))
  (is (re-find #"a job spec is a list" (problem '[jobs.a])))
  (is (re-find #"a job spec is a list" (problem '())))
  (is (re-find #"in \(seq \(jobs.nope\)\)" (problem '(seq (jobs.nope)))) "the message names the whole spec"))

(deftest labels-print-the-spec
  (is (= "jobs.a" (expr/label (expr/parse reg '(jobs.a {:n 4})))))
  (is (= "(seq jobs.a (repeat jobs.b))" (expr/label (expr/parse reg '(seq (jobs.a) (repeat (jobs.b))))))))

(deftest a-leaf-by-symbol-gets-its-defaults
  (is (= [(get reg 'jobs.a) {:n 1 :m 7}] (expr/leaf reg 'jobs.a {:m 7}))))
