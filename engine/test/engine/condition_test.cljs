(ns engine.condition-test
  (:require [cljs.test :refer [deftest is are]]
            [engine.condition :as c]
            [engine.condition.facts :as facts]
            [engine.memory :as mem]
            [engine.test-util :as tu]))

(def ? c/unknown)

(defn node [form]
  (let [r (c/compile form)]
    (is (:ok r) (pr-str r))
    (:node r)))

(defn view
  "A memory view at now with the given {kind [data ...]} entries, all written at t 0."
  ([now] (view now {}))
  ([now kinds]
   {:now now
    :data {:entries (into {} (map (fn [[k ds]] [k (mapv (fn [d] {:t 0 :data d}) ds)])) kinds)
           :policies (into {} (map (fn [k] [k {:cap 50 :ttl :forever}])) (keys kinds))}}))

(defn env
  ([] (env (tu/fake)))
  ([p] (env p (view 0)))
  ([p memory] {:world p :memory memory}))

(defn value
  ([form] (value form (env)))
  ([form e] (:value (c/evaluate (node form) e {}))))

;; ------------------------------------------------------------------ reader

(deftest a-string-and-a-form-read-the-same
  (are [s form] (= {:ok true :form form} (c/read-condition s))
    "(< (health) 7)" '(< (health) 7)
    "(and (< (inventory \"bread\") 8) (< (distance-to (place :home)) 16))"
    '(and (< (inventory "bread") 8) (< (distance-to (place :home)) 16))
    "  (daytime) " '(daytime))
  (is (= (c/read-condition "(held-for 5 (burning))") (c/read-condition '(held-for 5 (burning))))))

(deftest a-leading-quote-is-refused
  (are [x] (= :quoted (:reason (c/read-condition x)))
    "'(< (health) 7)"
    " '(daytime)"
    '(quote (< (health) 7))))

(deftest unreadable-or-trailing-text-is-refused
  (are [s reason] (= reason (:reason (c/read-condition s)))
    "(< (health) 7" :unreadable
    "(daytime) (burning)" :trailing
    "" :unreadable))

;; ------------------------------------------------------------------ validator

(defn refusal [form] (c/compile form))

(deftest refusals-name-the-offending-sub-form
  (are [form reason at] (let [r (refusal form)]
                          (and (false? (:ok r)) (= reason (:reason r)) (= at (:at r))))
    '(let [x (health)] (< x 7)) :unknown-symbol '(let [x (health)] (< x 7))
    '(and (daytime) (< (+ (health) 1) 7)) :unknown-symbol '(+ (health) 1)
    '(> (count (inventory "bread")) 2) :unknown-symbol '(count (inventory "bread"))
    '(< (health) 7 8) :arity '(< (health) 7 8)
    '(and) :arity '(and)
    '(not (daytime) (burning)) :arity '(not (daytime) (burning))
    '(and (inventory "bread") (daytime)) :type '(inventory "bread")
    '(< (inventory :bread) 8) :type :bread
    '(distance-to :home) :type :home
    '(not 3) :type 3
    '(< (daytime) 1) :type '(daytime)
    '(health) :not-boolean '(health)
    '(place :home) :not-boolean '(place :home)
    '(= (health) "bread") :incomparable '(= (health) "bread")
    '(= (place :home) (place :bed)) :incomparable '(= (place :home) (place :bed))
    '(held-for (health) (daytime)) :bad-duration '(held-for (health) (daytime))
    '(held-for -1 (daytime)) :bad-duration '(held-for -1 (daytime))
    '(and health (daytime)) :not-a-form 'health
    '(and [1] (daytime)) :not-a-form [1]
    '(< nil 2) :not-a-form nil
    '() :not-a-form '()
    '(quote (daytime)) :quoted '(quote (daytime))))

(deftest an-unknown-symbol-refusal-lists-the-whole-vocabulary
  (let [{:keys [allowed]} (refusal '(some (daytime)))]
    (is (some #{"(and boolean ...)"} allowed))
    (is (some #{"(inventory string) -> number"} allowed))
    (is (some #{"(held-for seconds boolean)"} allowed))))

(deftest an-arity-or-type-refusal-lists-the-signature
  (is (= ["(inventory string) -> number"] (:allowed (refusal '(< (inventory) 3)))))
  (is (= ["(distance-to position) -> number"] (:allowed (refusal '(< (distance-to "home") 3))))))

(deftest literals-and-nesting-compile
  (are [form] (:ok (c/compile form))
    true
    '(and (< (inventory "bread") 8) (< (distance-to (place :home)) 16))
    '(or (burning) (suffocating) (not (daytime)))
    '(= (daytime) false)
    '(held-for 0 (held-for 2.5 (hostile-near 10)))
    '(> (blocks-near "oak_log" 16) 3)))

;; ------------------------------------------------------------------ evaluator

(def f-env (env (tu/fake {:self {:health 5 :food 20}})))

(deftest kleene-truth-table
  ;; (place :nowhere) is unknown, so (< (distance-to (place :nowhere)) 1) is the unknown boolean u
  (let [u '(< (distance-to (place :nowhere)) 1)
        t '(< (health) 7)
        f '(> (health) 7)]
    (are [form expected] (= expected (value form f-env))
      t true
      f false
      u ?
      (list 'not u) ?
      (list 'not t) false
      (list 'and f u) false
      (list 'and u f) false
      (list 'and t u) ?
      (list 'and t t) true
      (list 'or t u) true
      (list 'or u t) true
      (list 'or f u) ?
      (list 'or f f) false
      (list '= u t) ?
      (list '= t true) true)))

(deftest comparisons
  (are [form expected] (= expected (value form f-env))
    '(< (health) 5) false
    '(<= (health) 5) true
    '(> (health) 4) true
    '(>= (health) 6) false
    '(= (health) 5) true
    '(= "a" "a") true
    '(= :a :b) false))

(deftest held-for-holds-after-the-condition-was-true-long-enough
  (let [n (node '(held-for 5 (< (health) 7)))
        hurt (tu/fake {:self {:health 5}})
        well (tu/fake {:self {:health 20}})
        step (fn [state p now] (c/evaluate n (env p (view now)) state))
        r0 (step {} hurt 1000)
        r1 (step (:state r0) hurt 5999)
        r2 (step (:state r1) hurt 6000)
        r3 (step (:state r2) well 6500)
        r4 (step (:state r3) hurt 7000)
        r5 (step (:state r4) hurt 11999)]
    (is (= [false false true false false false] (mapv :value [r0 r1 r2 r3 r4 r5])))
    (is (= true (:value (step (:state r5) hurt 12000))))))

(deftest held-for-is-unknown-and-restarts-when-its-condition-is-unknown
  (let [n (node '(held-for 1 (< (distance-to (place :home)) 5)))
        home (view 0 {:home [{:pos {:x 0 :y 64 :z 0}}]})
        step (fn [state memory] (c/evaluate n (env (tu/fake) memory) state))
        r0 (step {} home)
        r1 (step (:state r0) (assoc (view 0) :now 2000))
        r2 (step (:state r1) (assoc home :now 2500))
        r3 (step (:state r2) (assoc home :now 3500))]
    (is (= [false ? false true] (mapv :value [r0 r1 r2 r3])))))

(deftest held-for-timers-do-not-depend-on-siblings
  (let [n (node '(or (daytime) (held-for 2 (< (health) 7))))
        p (tu/fake {:self {:health 5} :time 1000})
        r0 (c/evaluate n (env p (view 0)) {})
        r1 (c/evaluate n (env p (view 2000)) (:state r0))]
    (is (= true (:value r0)))
    (is (seq (:since (:state r1))))))

(deftest explain-lists-every-sub-term-with-its-value
  (is (= [{:form '(and (< (health) 7) (daytime)) :value true}
          {:form '(< (health) 7) :value true}
          {:form '(health) :value 5}
          {:form 7 :value 7}
          {:form '(daytime) :value true}]
         (c/explain (node '(and (< (health) 7) (daytime))) f-env {}))))

(deftest explain-reports-active-held-for-time-and-clears-it-when-reset
  (let [n (node '(held-for 5 (< (health) 7)))
        hurt (tu/fake {:self {:health 5}})
        well (tu/fake {:self {:health 20}})
        step (fn [state p now] (c/evaluate n (env p (view now)) state))
        first-step (step {} hurt 1000)
        explain-at (fn [state p now] (c/explain n (env p (view now)) state))
        active (explain-at (:state first-step) hurt 3000)
        elapsed (explain-at (:state first-step) hurt 6000)
        reset-step (step (:state first-step) well 6500)
        reset (explain-at (:state reset-step) well 6500)
        restarted (step (:state reset-step) hurt 7000)
        restart-explain (explain-at (:state restarted) hurt 7001)]
    (is (= 3000 (:remaining-ms (first active))))
    (is (= 0 (:remaining-ms (first elapsed))))
    (is (= false (:value (first reset))))
    (is (nil? (:remaining-ms (first reset))))
    (is (= 4999 (:remaining-ms (first restart-explain))))))

(deftest a-scanning-fact-is-served-from-the-cache-until-its-refresh
  (let [calls (atom 0)
        p (tu/fake {:blocks {"1,64,0" "oak_log" "2,64,0" "oak_log"}})
        counting (js/Object.assign #js {} p #js {:blocks (fn [o] (swap! calls inc) (.blocks p o))})
        n (node '(> (blocks-near "oak_log" 16) 1))
        step (fn [state now] (c/evaluate n (env counting (view now)) state))
        r0 (step {} 0)
        r1 (step (:state r0) 4999)
        r2 (step (:state r1) 5000)]
    (is (= [true true true] (mapv :value [r0 r1 r2])))
    (is (= 2 @calls))))

(deftest the-adapter-has-the-register-shape-and-its-own-state
  (let [pred-a (c/when-fn (node '(held-for 1 (< (health) 7))))
        pred-b (c/when-fn (node '(held-for 1 (< (health) 7))))
        p (tu/fake {:self {:health 5}})]
    (is (false? (pred-a p (view 0) {} nil)))
    (is (true? (pred-a p (view 1000) {} nil)))
    (is (false? (pred-b p (view 1000) {} nil)))
    (is (false? ((c/when-fn (node '(< (distance-to (place :x)) 1))) p (view 0) {})))))

;; ------------------------------------------------------------------ facts

(def offline #js {:self (fn [] #js {:status "offline"})
                  :entities (fn [_] #js [])
                  :blocks (fn [_] #js [])
                  :blockAt (fn [_] nil)})

;; ------------------------------------------------------------------ known?

(def home (view 0 {:home [{:pos {:x 0 :y 64 :z 0}}]}))

(deftest known-turns-unknown-into-a-definite-boolean
  (let [step (fn [form p memory] (value form (env p memory)))
        no-home '(not (known? (place :home)))
        near-home '(and (known? (place :home)) (< (distance-to (place :home)) 16))
        p (tu/fake {:self {:health 5}})]
    (are [form p memory expected] (= expected (step form p memory))
      no-home p (view 0) true
      no-home p home false
      '(known? (place :home)) p home true
      '(known? (place :home)) p (view 0) false
      '(known? (health)) p (view 0) true
      '(known? (health)) offline (view 0) false
      near-home p (view 0) false
      near-home p home true)))

(deftest known-over-a-boolean-expression-is-true-exactly-when-it-is-definite
  (let [u '(< (distance-to (place :nowhere)) 1)
        t '(< (health) 7)
        f '(> (health) 7)]
    (are [form expected] (= expected (value form f-env))
      (list 'known? u) false
      (list 'known? t) true
      (list 'known? (list 'and t u)) false
      (list 'known? (list 'and f u)) true
      (list 'known? (list 'or t u)) true
      (list 'known? (list 'or f u)) false
      (list 'known? (list 'not u)) false
      (list 'known? (list 'known? u)) true)))

(deftest held-for-over-known-times-the-absence-and-is-evaluated-every-tick
  (let [n (node '(held-for 5 (not (known? (place :bed)))))
        step (fn [state memory] (c/evaluate n (env (tu/fake) memory) state))
        bed (fn [now] (assoc (view 0 {:bed [{:pos {:x 0 :y 64 :z 0}}]}) :now now))
        r0 (step {} (view 1000))
        r1 (step (:state r0) (view 5999))
        r2 (step (:state r1) (view 6000))
        r3 (step (:state r2) (bed 6500))
        r4 (step (:state r3) (view 7000))]
    (is (= [false false true false false] (mapv :value [r0 r1 r2 r3 r4])))))

(deftest known-over-held-for-is-unknown-only-while-its-condition-is
  (let [n (node '(known? (held-for 5 (< (distance-to (place :home)) 5))))
        step (fn [state memory] (c/evaluate n (env (tu/fake) memory) state))
        r0 (step {} (view 0))
        r1 (step (:state r0) home)
        r2 (step (:state r1) (assoc home :now 100))]
    (is (= [false true true] (mapv :value [r0 r1 r2])))
    (is (seq (:since (:state r1))) "the timer inside advanced while the outer form was a plain boolean")))

(deftest known-evaluates-its-sub-form-with-no-short-circuit
  (let [n (node '(and (known? (place :home)) (held-for 2 (< (health) 7))))
        p (tu/fake {:self {:health 5}})
        r0 (c/evaluate n (env p (view 0)) {})
        r1 (c/evaluate n (env p (assoc home :now 2000)) (:state r0))]
    (is (= [false true] (mapv :value [r0 r1])))))

(deftest known-takes-one-fact-or-expression
  (are [form reason at] (let [r (refusal form)]
                          (and (false? (:ok r)) (= reason (:reason r)) (= at (:at r))
                               (= ["(known? form)"] (:allowed r))))
    '(known?) :arity '(known?)
    '(known? (place :home) (place :bed)) :arity '(known? (place :home) (place :bed))
    '(known? 3) :type 3
    '(known? :home) :type :home
    '(known? "x") :type "x")
  (are [form reason at] (let [r (refusal form)] (and (= reason (:reason r)) (= at (:at r))))
    '(known? health) :not-a-form 'health
    '(known? (nope)) :unknown-symbol '(nope)
    '(known? (inventory :bread)) :type :bread))

(deftest known-is-in-the-vocabulary-and-a-condition-by-itself
  (is (some #{"(known? form)"} (:allowed (refusal '(some (daytime))))))
  (is (some #{"(known? form)"} (:allowed (refusal '(health)))))
  (is (:ok (c/compile '(known? (place :home)))))
  (is (:ok (c/compile '(and (known? (health)) (not (known? (place :bed))))))))

(deftest explain-shows-known-and-its-inner-term-with-their-values
  (is (= [{:form '(not (known? (place :home))) :value true}
          {:form '(known? (place :home)) :value false}
          {:form '(place :home) :value ?}
          {:form :home :value :home}]
         (c/explain (node '(not (known? (place :home)))) (env) {})))
  (is (= [{:form '(known? (health)) :value true}
          {:form '(health) :value 5}]
         (c/explain (node '(known? (health))) f-env {}))))

(deftest facts-read-the-fake-world
  (let [p (tu/fake {:self {:held "iron_sword" :health 12 :food 9 :pos {:x 3 :y 64 :z 4} :inWater true}
                    :time 13000
                    :inventory [{:name "bread" :count 5} {:name "bread" :count 2} {:name "stick" :count 1}]
                    :equipment {:head {:name "iron_helmet"} :offHand {:name "shield"}}
                    :entities [{:kind "hostile" :name "zombie" :pos {:x 6 :y 64 :z 4} :visible true}
                               {:kind "hostile" :name "zombie" :pos {:x 3 :y 64 :z 20} :visible true}
                               {:kind "hostile" :name "skeleton" :pos {:x 3 :y 64 :z 6} :visible false}]
                    :blocks {"3,66,4" "stone"}})
        e (env p (view 0 {:home [{:pos {:x 0 :y 64 :z 0}}]}))]
    (are [form expected] (= expected (value form e))
      '(= (health) 12) true
      '(= (food) 9) true
      '(= (inventory "bread") 7) true
      '(= (inventory "apple") 0) true
      '(= (free-slots) 33) true
      '(wearing "iron_helmet") true
      '(wearing "shield") true
      '(wearing "iron_boots") false
      '(wearing "bread") false
      '(wearing "iron_sword") false
      '(= (distance-to (place :home)) 5) true
      '(daytime) false
      '(hostile-near 4) true
      '(hostile-near 2) false
      '(in-water) true
      '(burning) false
      '(suffocating) false
      '(night-unsafe) false
      '(stuck) false
      '(= (blocks-near "stone" 8) 1) true)))

(deftest facts-are-unknown-when-the-body-is-offline
  (are [form] (= ? (value form (env offline)))
    '(< (health) 7)
    '(< (food) 7)
    '(< (inventory "bread") 7)
    '(< (free-slots) 2)
    '(wearing "shield")
    '(< (distance-to (place :home)) 2)
    '(daytime)
    '(hostile-near 8)
    '(burning)
    '(in-water)
    '(suffocating)
    '(night-unsafe)
    '(< (blocks-near "stone" 8) 1)))

(deftest the-fact-table-declares-a-cost-for-every-fact
  (is (every? #{:cheap :scan} (map :cost (vals facts/table))))
  (is (every? pos? (keep :refresh-s (vals facts/table)))))

;; ------------------------------------------------------------------ since

(defn memory-at
  "A memory view at now (ms) holding entries of kind written at each t, under policy."
  [now kind ts policy]
  {:now now
   :data (reduce (fn [d t] (mem/add-entry d kind {:t t :data {}} policy)) mem/empty-data ts)})

(def forever {:cap 50 :ttl :forever})

(deftest since-is-the-seconds-since-the-latest-entry-of-the-kind
  (are [now ts seconds] (true? (value (list '= '(since :slept) seconds) (env (tu/fake) (memory-at now :slept ts forever))))
    10000 [4000] 6
    10000 [1000 4000] 6
    10000 [4000 1000] 9
    1500 [1000] 0.5
    1000 [1000] 0))

(deftest since-is-unknown-without-an-entry-and-known-says-never
  (let [e (env (tu/fake) (memory-at 10000 :looked [1000] forever))]
    (are [form expected] (= expected (value form e))
      '(< (since :slept) 5) ?
      '(= (since :slept) 0) ?
      '(known? (since :slept)) false
      '(not (known? (since :slept))) true
      '(known? (since :looked)) true)))

(deftest since-never-or-too-long-ago
  (let [form '(or (not (known? (since :slept))) (> (since :slept) 3600))
        at (fn [now ts] (value form (env (tu/fake) (memory-at now :slept ts forever))))]
    (is (true? (value form (env))) "never")
    (is (false? (at 3600000 [1000])) "3599 s ago")
    (is (true? (at 3602000 [1000])) "3601 s ago")))

(deftest since-reads-a-namespaced-kind
  (let [form '(or (not (known? (since :bred/cow))) (> (since :bred/cow) 1200))
        at (fn [now ts] (value form (env (tu/fake) (memory-at now :bred/cow ts forever))))]
    (is (true? (value form (env))) "never")
    (is (false? (at 1000000 [1000])) "999 s ago")
    (is (true? (at 1202000 [1000])) "1201 s ago")))

(deftest since-does-not-see-an-expired-entry
  (is (= ? (value '(> (since :looked) 0) (env (tu/fake) (memory-at 7200000 :looked [1000] {:cap 5 :ttl 3600000}))))))

(deftest since-takes-one-keyword-literal
  (are [form reason at] (let [r (refusal form)]
                          (and (false? (:ok r)) (= reason (:reason r)) (= at (:at r))
                               (= ["(since keyword) -> number"] (:allowed r))))
    '(< (since "slept") 5) :type "slept"
    '(< (since 3) 5) :type 3
    '(< (since (place :home)) 5) :type '(place :home)
    '(< (since) 5) :arity '(since)
    '(< (since :a :b) 5) :arity '(since :a :b)))

(deftest since-is-in-the-vocabulary-and-compiles-with-any-kind
  (is (some #{"(since keyword) -> number"} (:allowed (refusal '(nope)))))
  (is (:ok (c/compile '(> (since :never-written-kind) 5)))))

(deftest explain-shows-since-with-its-value-and-unknown-as-unknown
  (is (= [{:form '(> (since :slept) 5) :value true}
          {:form '(since :slept) :value 9}
          {:form :slept :value :slept}
          {:form 5 :value 5}]
         (c/explain (node '(> (since :slept) 5)) (env (tu/fake) (memory-at 10000 :slept [1000] forever)) {})))
  (is (= [{:form '(known? (since :slept)) :value false}
          {:form '(since :slept) :value ?}
          {:form :slept :value :slept}]
         (c/explain (node '(known? (since :slept))) (env) {}))))
