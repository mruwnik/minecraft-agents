(ns engine.expr-test
  (:require [cljs.test :refer [deftest is are]]
            [cljs.reader :as reader]
            [engine.expr :as expr]))

(defn ^:async noop-round [_] :done)

(def reg
  {'jobs.a {:check (constantly true) :round noop-round
            :args {:n {:doc "n" :default 1} :m {:doc "m" :default 2}}}
   'jobs.b {:check (constantly true) :round noop-round :args nil}
   'jobs.free {:check (constantly true) :round noop-round}})

(defn problem [form]
  (try (expr/parse-spec reg form) nil
       (catch :default e (ex-message e))))

(deftest a-leaf-merges-its-args-over-the-defaults
  (is (= {:op :leaf :job 'jobs.a :args {:n 1 :m 2}} (expr/parse reg '(jobs.a))))
  (is (= {:op :leaf :job 'jobs.a :args {:n 5 :m 0}} (expr/parse reg '(jobs.a {:n 5 :m 0}))))
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

(deftest unknown-arg-keys-are-refused-naming-each-and-listing-the-known
  (is (re-find #"jobs.a has no arg :x, :y; its args are :m, :n" (problem '(jobs.a {:x 1 :n 2 :y 3}))))
  (is (re-find #"jobs.b has no arg :n; it takes no args" (problem '(jobs.b {:n 1}))))
  (is (re-find #"in \(seq \(jobs.a \{:z 1\}\)\)" (problem '(seq (jobs.a {:z 1})))))
  (is (nil? (problem '(jobs.a {:n 1 :m 2}))))
  (is (nil? (problem '(jobs.b {}))))
  (is (nil? (problem '(jobs.free {:anything 1}))) "an entry without :args is not checked"))

(deftest labels-print-the-spec
  (is (= "jobs.a" (expr/label (expr/parse reg '(jobs.a {:n 4})))))
  (is (= "(seq jobs.a (repeat jobs.b))" (expr/label (expr/parse reg '(seq (jobs.a) (repeat (jobs.b))))))))

(deftest a-leaf-by-symbol-gets-its-defaults
  (is (= [(get reg 'jobs.a) {:n 1 :m 7}] (expr/leaf reg 'jobs.a {:m 7}))))
