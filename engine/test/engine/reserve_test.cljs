(ns engine.reserve-test
  "jobs.lib.reserve: the register of reserved carried items, its built-in rules and the price of using a reserved unit."
  (:require [cljs.test :refer [deftest is are]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [jobs.lib.cost :as cost]
            [jobs.lib.reserve :as reserve]))

(defn item [name count] {:name name :count count})

(defn body
  "A ctx holding items, with register entries (maps) at time 1000 and live job instances (ids)."
  [items entries & {:keys [jobs overrides home-bed]}]
  (let [add (fn [d kind data policy] (mem/add-entry d kind {:t 0 :data data} policy))
        data (as-> mem/empty-data d
               (reduce #(add %1 :reserve %2 reserve/policy) d entries)
               (reduce #(add %1 (mem/job-kind (first %2)) (second %2) mem/job-policy) d (map #(if (string? %) [% {}] %) jobs))
               (reduce #(add %1 :choose/override %2 {:cap 20 :ttl :forever}) d overrides)
               (cond-> d home-bed (add :bed {:pos home-bed} mem/place-policy)))]
    {:primitives (tu/fake {:inventory items}) :view (fn [] {:data data :now 1000})}))

(deftest reserve-cost-is-value-times-odds
  (are [s cost-of] (= cost-of (Math/round (cost/reserve-cost 10 s)))
    0 0
    0.5 10
    0.9 90
    1 ##Inf))

(deftest an-item-entry-holds-up-to-its-count-of-what-is-carried
  (are [carried count held] (= held (reserve/held (body [(item "diamond" carried)] [{:item "diamond" :count count :strength 1}]) :tools))
    5 3 {"diamond" 3}
    2 3 {"diamond" 2}
    0 3 {}))

(deftest an-entry-spares-the-choosers-it-names-in-not
  (are [chooser held] (= held (reserve/held (body [(item "cobblestone" 3)] [{:item "cobblestone" :count 3 :not #{:craft}}]) chooser))
    :fillers {"cobblestone" 3}
    :craft {}))

(deftest a-tag-entry-holds-the-carried-names-of-the-tag
  (is (= {"raw_iron" 2 "iron_ore" 1}
         (reserve/held (body [(item "raw_iron" 4) (item "iron_ore" 1) (item "stick" 9)] [{:tag :ore :count 3}]) :tools))))

(deftest entries-expire-by-time-and-by-dead-job
  (are [entry jobs live] (= live (boolean (seq (reserve/held (body [(item "diamond" 1)] [(merge {:item "diamond"} entry)] :jobs jobs) :tools))))
    {:until 2000} [] true
    {:until 1000} [] false
    {:by "agent"} [] true
    {:by "j1"} ["j1"] true
    {:by "j1/bill"} [["j1" {:children {:bill {:args {}}}}]] true
    {:by "j1/bill"} [["j1" {:children {}}]] false
    {:by "j1/bill"} ["j1"] false
    {:by "j1/a/b"} [["j1" {:children {:a {:children {:b {}}}}}]] true
    {:by "j1"} [] false))

(deftest the-built-in-rules-hold-through-the-same-answer
  (let [c (body [(item "golden_apple" 2) (item "stone_pickaxe" 1) (item "bread" 4)]
                [])]
    (is (= {"golden_apple" 2 "bread" 4} (reserve/held c :tools)))))

(deftest one-carried-bed-is-held-until-a-home-bed-is-placed
  (let [items [(item "red_bed" 2) (item "white_bed" 1)]]
    (is (= {"red_bed" 1} (reserve/held (body items []) :deposit)))
    (is (= {} (reserve/held (body items [] :home-bed [1 0 1]) :deposit)))))

(deftest rows-are-strongest-first-and-capped-by-what-is-carried
  (let [c (body [(item "diamond" 4)] [{:item "diamond" :count 3 :strength 0.5} {:item "diamond" :count 3 :strength 0.8}])]
    (is (= [{:name "diamond" :count 3 :strength 0.8} {:name "diamond" :count 1 :strength 0.5}] (reserve/rows c :tools)))))

(deftest price-is-for-the-units-over-the-free-surplus-lowest-strength-first
  (let [c (body [(item "diamond" 5)] [{:item "diamond" :count 2 :strength 0.5} {:item "diamond" :count 1 :strength 0.9}])
        each (:value (cost/item-value [(item "diamond" 1)]))]
    (are [n expected] (= expected (reserve/price c :tools "diamond" n))
      2 0
      3 (cost/reserve-cost each 0.5)
      4 (* 2 (cost/reserve-cost each 0.5))
      5 (+ (* 2 (cost/reserve-cost each 0.5)) (cost/reserve-cost each 0.9)))))

(deftest a-strength-one-unit-is-never-for-sale
  (is (= ##Inf (reserve/price (body [(item "diamond" 3)] [{:item "diamond" :count 3 :strength 1}]) :tools "diamond" 1))))

(deftest an-avoid-override-holds-its-names-for-its-chooser-only
  (let [c (body [(item "diamond" 2)] [] :overrides [{:chooser :tools :avoid #{"diamond"}}])]
    (is (= {"diamond" 2} (reserve/held c :tools)))
    (is (= {} (reserve/held c :fillers)))))

(deftest a-register-entry-on-a-built-in-name-does-not-add-to-it
  (let [c (body [(item "golden_apple" 2)] [{:item "golden_apple" :count 1 :strength 0.5}])]
    (is (= {"golden_apple" 2} (reserve/held c :tools)))
    (is (= [{:name "golden_apple" :count 2 :strength 1}] (reserve/rows c :tools)))))

(deftest a-missing-or-all-count-holds-everything-carried
  (are [entry] (= {"diamond" 4} (reserve/held (body [(item "diamond" 4)] [(merge {:item "diamond"} entry)]) :tools))
    {}
    {:count :all}))

(deftest a-built-in-unit-is-never-for-sale
  (is (= ##Inf (reserve/price (body [(item "golden_apple" 2)] []) :tools "golden_apple" 1))))

(deftest a-strength-zero-entry-is-free
  (let [c (body [(item "diamond" 3)] [{:item "diamond" :count 3 :strength 0}])]
    (is (= {} (reserve/held c :tools)))
    (is (= 0 (reserve/price c :tools "diamond" 3)))))

(deftest free-is-the-inventory-less-the-held-units-across-stacks
  (is (= [(item "diamond" 1) (item "stick" 4)]
         (map #(dissoc % :slot) (reserve/free (body [(item "diamond" 2) (item "diamond" 3) (item "stick" 4)] [{:item "diamond" :count 4}]) :craft)))))
