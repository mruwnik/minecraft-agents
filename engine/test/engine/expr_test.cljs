(ns engine.expr-test
  (:require [cljs.test :refer [deftest is are]]
            [cljs.reader :as reader]
            [engine.expr :as expr]
            [engine.registry :as registry]))

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

(deftest repeat-takes-a-count-and-until-a-guard
  (is (= {:op :repeat :times 3 :child {:op :leaf :job 'jobs.b :args {}}} (expr/parse reg '(repeat 3 (jobs.b)))))
  (is (= {:op :until :children [{:op :leaf :job 'jobs.a :args {:n 1 :m 2}} {:op :leaf :job 'jobs.b :args {}}]}
         (expr/parse reg '(until (jobs.a) (jobs.b)))))
  (is (= "(repeat 3 jobs.b)" (expr/label (expr/parse reg '(repeat 3 (jobs.b))))))
  (is (= "(until jobs.a jobs.b)" (expr/label (expr/parse reg '(until (jobs.a) (jobs.b)))))))

(deftest hold-is-a-flag-on-the-top
  (is (= {:node {:op :leaf :job 'jobs.b :args {}} :hold? true} (expr/parse-spec reg '(hold (jobs.b)))))
  (is (= {:node {:op :leaf :job 'jobs.b :args {}} :hold? false} (expr/parse-spec reg '(jobs.b)))))

(deftest specs-read-from-edn
  (is (= {:op :seq :children [{:op :leaf :job 'jobs.a :args {:n 3 :m 2}}]}
         (expr/parse reg (reader/read-string "(seq (jobs.a {:n 3}))")))))

(deftest bad-specs-are-refused-with-a-message
  (is (re-find #"unknown job or combinator jobs.nope" (problem '(jobs.nope))))
  (is (re-find #"unknown job or combinator map" (problem '(map (jobs.a)))))
  (is (re-find #"hold is only allowed" (problem '(seq (hold (jobs.a))))))
  (is (re-find #"repeat takes an optional count" (problem (quote (repeat (jobs.a) (jobs.b))))))
  (is (re-find #"repeat takes exactly one" (problem (quote (repeat 2 (jobs.a) (jobs.b))))))
  (is (re-find #"repeat takes an optional count" (problem '(repeat 0 (jobs.a)))))
  (is (re-find #"repeat takes an optional count" (problem '(repeat :x (jobs.a)))))
  (is (re-find #"until takes a guard" (problem '(until (jobs.a)))))
  (is (re-find #"hold takes exactly one" (problem '(hold))))
  (is (re-find #"unknown job or combinator backoff" (problem '(backoff {} (jobs.a)))) "listed jobs have no backoff")
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

(def pos-args
  "The position-valued args of real jobs: [job arg]."
  [['jobs.movement.go-to :pos] ['jobs.blocks.dig :pos] ['jobs.blocks.place :pos] ['jobs.blocks.use-on :pos]
   ['jobs.access.toggle :pos] ['jobs.movement.leave-vehicle :toward]
   ['jobs.storage.withdraw :chest] ['jobs.storage.deposit :chest] ['jobs.items.smelt :furnace] ['jobs.items.bake :chest]
   ['jobs.build.clear-box :from] ['jobs.build.clear-box :to] ['jobs.access.tunnel :target] ['jobs.debug.walk-plan :to]
   ['jobs.animals.pen-check :at] ['jobs.memory.set-place :pos]])

(deftest real-jobs-take-position-args-as-a-vector-or-a-map-and-refuse-junk-at-submit
  (doseq [[job k] pos-args]
    (is (= :pos (get-in registry/jobs [job :args k :type])) (str job " " k))
    (let [args #(:args (expr/parse registry/jobs (list job {k %})))]
      (is (= {:x 1 :y 64 :z -3} (get (args [1 64 -3]) k)) (str job " vector"))
      (is (= (args [1 64 -3]) (args {:x 1 :y 64 :z -3})) (str job " map"))
      (is (re-find (re-pattern (str job " " k " must be \\[x y z\\]"))
                   (try (expr/parse registry/jobs (list job {k [1 2]})) nil (catch :default e (ex-message e))))
          (str job " junk")))))
