(ns engine.expr-test
  (:require [cljs.test :refer [deftest is are]]
            [cljs.reader :as reader]
            [engine.expr :as expr]))

(defn ^:async noop-round [_] :done)

(def reg
  {'jobs.a {:check (constantly true) :round noop-round
            :args {:n {:doc "n" :default 1} :m {:doc "m" :default 2}}}
   'jobs.p {:check (constantly true) :round noop-round
            :args {:at {:doc "cell" :type :pos :default nil} :n {:doc "n" :default 1}}}
   'jobs.t {:check (constantly true) :round noop-round
            :args {:k {:type :keyword} :i {:type :int} :x {:type :number :min 0 :max 10} :b {:type :bool}
                   :s {:type :string} :it {:type :item} :e {:type :enum :values [:a :b]} :u {:doc "untyped"}}}
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

(deftest position-args-take-a-vector-or-a-map-and-reach-the-job-as-a-map
  (are [at expected] (= expected (:args (expr/parse reg (list 'jobs.p {:at at}))))
    [1 64 -3] {:at {:x 1 :y 64 :z -3} :n 1}
    {:x 1 :y 64 :z -3} {:at {:x 1 :y 64 :z -3} :n 1}
    [1.5 64 2] {:at {:x 1.5 :y 64 :z 2} :n 1}
    nil {:at nil :n 1}))

(deftest malformed-position-args-are-refused-at-parse
  (are [at] (re-find #"jobs.p :at must be \[x y z\] or \{:x :y :z\}" (problem (list 'jobs.p {:at at})))
    [1 2]
    [1 2 "3"]
    {:x 1 :y 2}
    {:x 1 :y nil :z 3}
    "1 2 3"
    5))

(deftest typed-args-that-fit-pass
  (are [k v] (nil? (problem (list 'jobs.t {k v})))
    :k :wood :i 3 :i -2 :x 0 :x 2.5 :x 10 :b false :b true :s "" :s "hi" :it "oak_log" :e :a :u {:any 1}
    :k nil :i nil :e nil))

(deftest typed-args-that-do-not-fit-are-refused-naming-job-arg-type-and-value
  (are [k v re] (re-find re (problem (list 'jobs.t {k v})))
    :k "wood" #"jobs.t :k must be a keyword, got \"wood\""
    :i 2.5 #"jobs.t :i must be a whole number, got 2.5"
    :i "3" #"jobs.t :i must be a whole number, got \"3\""
    :x "5" #"jobs.t :x must be a number, got \"5\""
    :x -1 #"jobs.t :x must be a number >= 0, got -1"
    :x 11 #"jobs.t :x must be a number <= 10, got 11"
    :b 1 #"jobs.t :b must be true or false, got 1"
    :s :x #"jobs.t :s must be a string, got :x"
    :it "" #"jobs.t :it must be an item name \(non-empty string\), got \"\""
    :it :oak_log #"jobs.t :it must be an item name"
    :e :c #"jobs.t :e must be one of \[:a :b\], got :c"))

(deftest a-default-of-the-wrong-type-is-refused-too
  (let [r {'jobs.d {:check (constantly true) :round noop-round :args {:n {:type :int :default "x"}}}}]
    (is (re-find #"jobs.d :n must be a whole number" (try (expr/parse-spec r '(jobs.d)) nil (catch :default e (ex-message e)))))))
