(ns engine.condition-test
  (:require [cljs.test :refer [deftest is are]]
            [engine.condition :as c]
            [engine.condition.facts :as facts]
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

(deftest facts-read-the-fake-world
  (let [p (tu/fake {:self {:health 12 :food 9 :pos {:x 3 :y 64 :z 4} :inWater true}
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
