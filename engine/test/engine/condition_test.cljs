(ns engine.condition-test
  (:require [cljs.test :refer [deftest is are]]
            [engine.condition :as c]
            [engine.contract :as contract]
            [engine.triggers :as triggers]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.lib.cond :as cond]
            [jobs.lib.ready :as ready]))

(defn view
  "A memory view at now with the given {kind [data ...]} entries, all written at t 0."
  ([now] (view now {}))
  ([now kinds]
   {:now now
    :data {:entries (into {} (map (fn [[k ds]] [k (mapv (fn [d] {:t 0 :data d}) ds)])) kinds)
           :policies (into {} (map (fn [k] [k {:cap 50 :ttl :forever}])) (keys kinds))}}))

(defn ctx-of
  ([] (ctx-of (tu/fake)))
  ([p] (ctx-of p (view 0)))
  ([p memory] {:primitives p :view (constantly memory)}))

(defn check [form]
  (let [r (c/compile form)]
    (is (:ok r) (pr-str r))
    (:check r)))

(defn status
  ([form] (status form (ctx-of)))
  ([form ctx] ((check form) ctx)))

(defn kind
  ([form] (first (status form)))
  ([form ctx] (first (status form ctx))))

;; ------------------------------------------------------------------ reader

(deftest a-string-and-a-form-read-the-same
  (are [s form] (= {:ok true :form form} (c/read-condition s))
    "(< (health) 7)" '(< (health) 7)
    "(and (< (inventory \"bread\") 8) (< (distance-to (place :home)) 16))"
    '(and (< (inventory "bread") 8) (< (distance-to (place :home)) 16))
    "  (daytime) " '(daytime)))

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

;; ------------------------------------------------------------------ the compiler

(defn refusal [form] (c/compile form))

(deftest refusals-name-the-offending-sub-form
  (are [form reason at] (let [r (refusal form)]
                          (and (false? (:ok r)) (= reason (:reason r)) (= at (:at r))))
    '(let [x (health)] (< x 7)) :unknown-symbol '(let [x (health)] (< x 7))
    '(and (daytime) (< (+ (health) 1) 7)) :unknown-symbol '(+ (health) 1)
    '(> (count (inventory "bread")) 2) :unknown-symbol '(count (inventory "bread"))
    '(held-for 5 (daytime)) :unknown-symbol '(held-for 5 (daytime))
    '(js/process.exit 1) :unknown-symbol '(js/process.exit 1)
    '(< (health) 7 8) :arity '(< (health) 7 8)
    '(and) :arity '(and)
    '(not (daytime) (burning)) :arity '(not (daytime) (burning))
    '(and (inventory "bread") (daytime)) :type '(inventory "bread")
    '(< (inventory :bread) 8) :type :bread
    '(distance-to :home) :type :home
    '(not 3) :type 3
    '(< (daytime) 1) :type '(daytime)
    '(because "x" (daytime)) :type "x"
    '(because :x 3 (daytime)) :type 3
    '(because :x (daytime) (daytime)) :type '(daytime)
    '(health) :not-a-status '(health)
    '(place :home) :not-a-status '(place :home)
    true :not-a-status true
    '(= (health) "bread") :incomparable '(= (health) "bread")
    '(= (place :home) (place :bed)) :incomparable '(= (place :home) (place :bed))
    '(= (daytime) false) :incomparable '(= (daytime) false)
    '(and health (daytime)) :not-a-form 'health
    '(and [1] (daytime)) :not-a-form [1]
    '(< nil 2) :not-a-form nil
    '() :not-a-form '()
    '(quote (daytime)) :quoted '(quote (daytime))))

(deftest an-unknown-symbol-refusal-lists-the-whole-vocabulary
  (let [{:keys [allowed]} (refusal '(some (daytime)))]
    (is (some #{"(and status ...)"} allowed))
    (is (some #{"(because keyword string? status)"} allowed))
    (is (some #{"(inventory string) -> number"} allowed))
    (is (some #{"(mounted) -> status"} allowed))
    (is (not-any? #(re-find #"held-for" %) allowed))))

(deftest an-arity-or-type-refusal-lists-the-signature
  (is (= ["(inventory string) -> number"] (:allowed (refusal '(< (inventory) 3)))))
  (is (= ["(distance-to position) -> number"] (:allowed (refusal '(< (distance-to "home") 3))))))

(deftest nesting-compiles
  (are [form] (:ok (c/compile form))
    '(and (< (inventory "bread") 8) (< (distance-to (place :home)) 16))
    '(or (burning) (suffocating) (not (daytime)))
    '(because :day-not-come "waiting for day" (daytime))
    '(and (not (in-water)) (< (inventory "bread") 8) (because :day-not-come (daytime)))
    '(> (blocks-near "oak_log" 16) 3)))

(deftest the-library-is-what-the-default-trigger-set-names
  (is (= cond/library triggers/library))
  (is (every? #(contains? triggers/library %) '[health food inventory time-of-day place since distance-to free-slots
                                                 blocks-near daytime mounted in-water wearing hostile-near burning
                                                 suffocating night-unsafe])))

;; ------------------------------------------------------------------ statuses of compiled forms

(def hurt (ctx-of (tu/fake {:self {:health 5 :food 20}})))

(deftest a-comparison-is-a-status-showing-the-relation-that-holds
  (are [form expected] (= expected (status form hurt))
    '(< (health) 7) [:ok "health 5 < 7" {:reason :health :have 5 :want 7}]
    '(> (health) 7) [:later "health 5 <= 7" {:reason :health :have 5 :want 7}]
    '(<= (health) 5) [:ok "health 5 <= 5" {:reason :health :have 5 :want 5}]
    '(>= (health) 6) [:later "health 5 < 6" {:reason :health :have 5 :want 6}]
    '(= (health) 5) [:ok "health 5 = 5" {:reason :health :have 5 :want 5}])
  (is (= :ok (kind '(= "a" "a"))))
  (is (= :later (kind '(= :a :b)))))

(deftest a-comparison-names-the-fact-on-either-side
  (is (= :food (:reason (nth (status '(> 30 (food)) hurt) 2)))))

(deftest an-unknown-value-makes-a-comparison-an-unknown-later
  (is (= [:later "distance-to (place nowhere) not known" {:reason :distance-to :unknown true}]
         (status '(< (distance-to (place :nowhere)) 1) hurt))))

(deftest because-replaces-a-failing-reason-and-keeps-the-kind
  (are [form expected] (= expected (status form hurt))
    '(because :too-healthy (> (health) 7)) [:later "health 5 <= 7" {:reason :too-healthy :have 5 :want 7}]
    '(because :too-healthy "not hurt enough" (> (health) 7)) [:later "not hurt enough" {:reason :too-healthy :have 5 :want 7}]
    '(because :hurt "hurt" (< (health) 7)) [:ok "health 5 < 7" {:reason :health :have 5 :want 7}]))

(deftest a-trigger-condition-holds-on-ok
  (let [f (check '(and (< (health) 7) (daytime)))]
    (is (= :ok (first (f hurt))))
    (is (= :later (first (f (ctx-of (tu/fake {:self {:health 20}}))))))))

(deftest every-part-is-in-the-status
  (let [[k text {:keys [parts]}] (status '(and (< (health) 7) (daytime)) hurt)]
    (is (= :ok k))
    (is (= "health 5 < 7; day" text))
    (is (= [[:ok "health 5 < 7" {:reason :health :have 5 :want 7}] [:ok "day" {:reason :daytime}]] parts))))

(deftest a-scanning-entry-is-read-once-a-tick
  (let [calls (atom 0)
        p (tu/seeing-all (tu/fake {:blocks {"1,64,0" "oak_log" "2,64,0" "oak_log"}}))
        counting (js/Object.assign #js {} p #js {:seenBlocks (fn [o] (swap! calls inc) (.seenBlocks p o))})
        f (check '(and (> (blocks-near "oak_log" 16) 1) (< (blocks-near "oak_log" 16) 5)))
        at (fn [now] (first (f (ctx-of counting (view now)))))]
    (is (= [:ok :ok :ok] [(at 0) (at 0) (at 50)]))
    (is (= 2 @calls))))

;; ------------------------------------------------------------------ operators (trusted code)

(defn const [s] (let [asked (atom 0)] [(fn [_] (swap! asked inc) s) asked]))

(def ok-a [:ok "a" {:reason :a}])
(def ok-b [:ok "b" {:reason :b}])
(def later-a [:later "la" {:reason :la}])
(def later-b [:later "lb" {:reason :lb}])
(def unknown-l [:later "u not known" {:reason :u :unknown true}])
(def never-a [:never "na" {:reason :na}])
(def look-a [:look "ka" {:reason :ka}])

(defn op [f & ss] (let [r ((apply f (map (comp first const) ss)) {})] (update r 2 dissoc :parts)))

(deftest and-is-any-never-else-the-first-later-else-the-first-look-else-ok
  (are [ss expected] (= expected (apply op cond/and ss))
    [ok-a ok-b] [:ok "a; b" {:reason :a}]
    [ok-a later-a later-b] later-a
    [look-a later-b] later-b
    [later-a never-a] never-a
    [ok-a look-a] look-a))

(deftest or-is-the-first-ok-else-the-first-look-else-the-first-later-else-never
  (are [ss expected] (= expected (apply op cond/or ss))
    [later-a ok-a ok-b] ok-a
    [later-a look-a] look-a
    [never-a later-b] later-b
    [later-a unknown-l] unknown-l
    [unknown-l later-a] unknown-l
    [never-a never-a] never-a))

(deftest not-turns-definite-statuses-round-and-keeps-unknown-and-look
  (are [s expected] (= expected (op cond/not s))
    ok-a [:later "a" {:reason :a}]
    later-a [:ok "la" {:reason :la}]
    never-a [:ok "na" {:reason :na}]
    unknown-l unknown-l
    look-a look-a))

(deftest and-and-or-ask-every-part
  (let [parts (map const [ok-a later-a never-a ok-b])
        [_ _ data] ((apply cond/and (map first parts)) {})
        _ ((apply cond/or (map first parts)) {})]
    (is (= [2 2 2 2] (map (comp deref second) parts)))
    (is (= [ok-a later-a never-a ok-b] (:parts data)))))

(deftest an-ending-cause-drops-the-parts
  (let [s ((apply cond/and (map (comp first const) [ok-a later-a])) {})]
    (is (some? (:parts (nth s 2))))
    (is (nil? (:parts (contract/status-cause s))))))

(deftest a-check-that-returns-no-status-is-an-error-naming-it
  (is (thrown-with-msg? js/Error #"not a status" ((cond/and (fn bad-check [_] nil)) {}))))

(deftest trusted-code-builds-the-same-checks-from-the-fns
  (is (= (status '(and (< (health) 7) (not (mounted))) hurt)
         ((cond/and (cond/< (cond/health) 7) (cond/not (cond/mounted))) hurt))))

;; ------------------------------------------------------------------ entries

(def offline #js {:self (fn [] #js {:status "offline"})
                  :entities (fn [_] #js [])
                  :blocks (fn [_] #js [])
                  :blockAt (fn [_] nil)})

(deftest entries-read-the-fake-world
  (let [p (tu/fake {:self {:held "iron_sword" :health 12 :food 9 :pos {:x 3 :y 64 :z 4} :inWater true}
                    :time 13000
                    :inventory [{:name "bread" :count 5} {:name "bread" :count 2} {:name "stick" :count 1}]
                    :equipment {:head {:name "iron_helmet"} :offHand {:name "shield"}}
                    :entities [{:kind "hostile" :name "zombie" :pos {:x 6 :y 64 :z 4} :visible true}
                               {:kind "hostile" :name "zombie" :pos {:x 3 :y 64 :z 20} :visible true}
                               {:kind "hostile" :name "skeleton" :pos {:x 3 :y 64 :z 6} :visible false}]
                    :blocks {"3,66,4" "stone"}})
        e (ctx-of (tu/seeing-all p) (view 0 {:home [{:pos {:x 0 :y 64 :z 0}}]}))]
    (are [form expected] (= expected (kind form e))
      '(= (health) 12) :ok
      '(= (food) 9) :ok
      '(= (inventory "bread") 7) :ok
      '(= (inventory "apple") 0) :ok
      '(= (free-slots) 33) :ok
      '(wearing "iron_helmet") :ok
      '(wearing "shield") :ok
      '(wearing "iron_boots") :later
      '(wearing "bread") :later
      '(wearing "iron_sword") :later
      '(= (distance-to (place :home)) 5) :ok
      '(daytime) :later
      '(hostile-near 4) :ok
      '(hostile-near 2) :later ; the skeleton is only heard: its band (about 4 blocks), not its exact place, is judged
      '(in-water) :ok
      '(burning) :later
      '(suffocating) :later
      '(night-unsafe) :ok ; afloat at night is no shelter
      '(= (blocks-near "stone" 8) 1) :ok
      '(= (time-of-day) 13000) :ok
      '(mounted) :later)))

(deftest a-status-entry-says-why-it-holds-or-not
  (let [riding (ctx-of (tu/seeing-all (tu/fake {:self {:pos {:x 0 :y 64 :z 0} :vehicle 9}
                                                 :entities [{:id 9 :kind "passive" :name "boat" :pos {:x 0 :y 64 :z 0} :visible true}]})))]
    (is (= [:ok "aboard" {:reason :mounted}] (status '(mounted) riding)))
    (is (= [:later "on foot" {:reason :not-aboard}] (status '(mounted) hurt)))
    (is (= [:later "aboard" {:reason :mounted}] (status '(not (mounted)) riding)) "not keeps the text and reason of an ok")
    (is (= [:ok "day" {:reason :daytime}] (status '(daytime) hurt)))))

(deftest hostile-near-is-a-real-danger-as-the-trigger-defines-it
  (let [wall (into {} (for [[x z] [[4 0] [6 0] [5 1] [5 -1]] y [64 65]] [(str x "," y "," z) "stone"]))
        zombie {:id 1 :kind "hostile" :name "zombie" :pos {:x 5 :y 64 :z 0} :visible true}
        skeleton {:id 2 :kind "hostile" :name "skeleton" :pos {:x 0 :y 64 :z 10} :visible true}
        near (fn [form world] (kind form (ctx-of (tu/seeing-all (tu/fake-on-floor (assoc world :self {:pos {:x 0 :y 64 :z 0}}))))))]
    (is (= :ok (near '(hostile-near 8) {:entities [zombie]})))
    (is (= :later (near '(hostile-near 8) {:blocks wall :entities [zombie]})) "a melee mob walled off is no danger")
    (is (= :later (near '(hostile-near 8) {:entities [skeleton]})) "a ranged mob beyond the written radius is not")
    (is (= :ok (near '(hostile-near 12) {:entities [skeleton]})) "a ranged mob with a line of fire within the radius is")))

(deftest blocks-near-counts-only-blocks-the-body-has-seen
  (let [fresh #(tu/fake {:blocks {"3,64,4" "diamond_ore" "3,60,4" "diamond_ore"}})
        only-top (doto (fresh)
                   (aset "seenBlocks" (fn [q] (.filter (.blocks (fresh) q) (fn [b] (= 64 (.-y (.-pos b))))))))]
    (are [p n] (= n (:have (nth (status '(= (blocks-near "diamond_ore" 8) 0) (ctx-of p)) 2)))
      (tu/blind (fresh)) 0
      (tu/seeing-all (fresh)) 2
      only-top 1)))

(deftest entries-are-unknown-laters-while-the-body-is-offline
  (are [form] (= [:later true] ((juxt first (comp :unknown #(nth % 2))) (status form (ctx-of offline))))
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

(deftest not-of-an-unknown-never-passes-on-an-offline-body
  (is (= :later (kind '(not (hostile-near 8)) (ctx-of offline)))))

;; ------------------------------------------------------------------ known?

(def home (view 0 {:home [{:pos {:x 0 :y 64 :z 0}}]}))

(deftest known-turns-unknown-into-a-definite-status
  (let [p (tu/fake {:self {:health 5}})]
    (are [form p memory expected] (= expected (kind form (ctx-of p memory)))
      '(not (known? (place :home))) p (view 0) :ok
      '(not (known? (place :home))) p home :later
      '(known? (place :home)) p home :ok
      '(known? (place :home)) p (view 0) :later
      '(known? (health)) p (view 0) :ok
      '(known? (health)) offline (view 0) :later
      '(known? (daytime)) offline (view 0) :later
      '(not (known? (daytime))) offline (view 0) :ok
      '(and (known? (place :home)) (< (distance-to (place :home)) 16)) p (view 0) :later
      '(and (known? (place :home)) (< (distance-to (place :home)) 16)) p home :ok)))

(deftest known-takes-one-entry-or-expression
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

;; ------------------------------------------------------------------ since

(defn memory-at
  "A memory view at now (ms) holding entries of kind written at each t, under policy."
  [now kind ts policy]
  {:now now
   :data (reduce (fn [d t] (mem/add-entry d kind {:t t :data {}} policy)) mem/empty-data ts)})

(def forever {:cap 50 :ttl :forever})

(deftest since-is-the-seconds-since-the-latest-entry-of-the-kind
  (are [now ts seconds] (= :ok (kind (list '= '(since :slept) seconds) (ctx-of (tu/fake) (memory-at now :slept ts forever))))
    10000 [4000] 6
    10000 [1000 4000] 6
    10000 [4000 1000] 9
    1500 [1000] 0.5
    1000 [1000] 0))

(deftest since-never-or-too-long-ago
  (let [form '(or (not (known? (since :slept))) (> (since :slept) 3600))
        at (fn [now ts] (kind form (ctx-of (tu/fake) (memory-at now :slept ts forever))))]
    (is (= :ok (kind form)) "never")
    (is (= :later (at 3600000 [1000])) "3599 s ago")
    (is (= :ok (at 3602000 [1000])) "3601 s ago")))

(deftest since-reads-a-namespaced-kind
  (is (= :ok (kind '(> (since :bred/cow) 1200) (ctx-of (tu/fake) (memory-at 1202000 :bred/cow [1000] forever))))))

(deftest since-does-not-see-an-expired-entry
  (is (= :ok (kind '(not (known? (since :looked))) (ctx-of (tu/fake) (memory-at 7200000 :looked [1000] {:cap 5 :ttl 3600000}))))))

(deftest since-takes-one-keyword-literal
  (are [form reason at] (let [r (refusal form)]
                          (and (false? (:ok r)) (= reason (:reason r)) (= at (:at r))
                               (= ["(since keyword) -> number"] (:allowed r))))
    '(< (since "slept") 5) :type "slept"
    '(< (since 3) 5) :type 3
    '(< (since (place :home)) 5) :type '(place :home)
    '(< (since) 5) :arity '(since)
    '(< (since :a :b) 5) :arity '(since :a :b)))

(def later-exposed (with-meta (fn [_] (contract/later :x "x")) {::ready/exposure (fn [_] (contract/later :exposed "dark"))}))

(deftest job-only-entries-compile-only-with-the-vocabulary
  (doseq [form '[(exposed) (job-safe) (and (job-fit) (not (threatened))) (always) (starving)]]
    (is (= :unknown-symbol (:reason (c/compile form))) (str form " refused for a trigger"))
    (is (:ok (c/compile form ready/vocabulary)) (str form " accepted for a job")))
  (is (= :unknown-symbol (:reason (c/compile '(exposed) {'other {:args [] :type :status :fn identity}}))))
  (is (some #(re-find #"\(exposed\)" %) (c/allowed ready/vocabulary)) "a refusal lists them"))

(deftest job-only-entries-read-the-frames-def
  (let [run (fn [form def] ((:check (c/compile form ready/vocabulary)) (assoc (ctx-of) :def def)))
        ok? #(= :ok (first %))]
    (is (not (ok? (run '(job-safe) {:safe? (fn [_] (contract/later :x "x"))}))))
    (is (ok? (run '(job-safe) {:safe? (fn [_] (contract/ok :x "x"))})))
    (is (ok? (run '(job-fit) {})) "no export holds")
    (is (ok? (run '(exposed) {:safe? later-exposed})))
    (is (not (ok? (run '(exposed) {:safe? (fn [_] (contract/ok :x "x"))}))))
    (is (ok? (run '(not (exposed)) {:safe? (fn [_] (contract/ok :x "x"))})))
    (is (ok? (run '(always) {})))))
