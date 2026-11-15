(ns engine.reserve-test
  "jobs.lib.reserve: the register of reserved carried items, its built-in rules and the price of using a reserved unit."
  (:require [cljs.test :refer [deftest is are]]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            [engine.fake :as fake]
            [jobs.lib.cost :as cost]
            [jobs.lib.home-food :as home-food]
            [jobs.lib.world-files :as ew]
            [jobs.lib.reserve :as reserve]))

(defn item [name count] {:name name :count count})

(defn body
  "A ctx holding items, with register entries (maps) at time 1000 and live job instances (ids); left-s the job-time its call chain has left."
  [items entries & {:keys [jobs overrides home-bed stock left-s]}]
  (let [add (fn [d kind data policy] (mem/add-entry d kind {:t 0 :data data} policy))
        data (as-> mem/empty-data d
               (reduce #(add %1 :reserve %2 reserve/policy) d entries)
               (reduce #(add %1 (mem/job-kind (first %2)) (second %2) mem/job-policy) d (map #(if (string? %) [% {}] %) jobs))
               (reduce #(add %1 :choose/override %2 {:cap 20 :ttl :forever}) d overrides)
               (cond-> d home-bed (add :bed {:pos home-bed} mem/place-policy))
               (reduce #(add %1 :fetch/stock %2 {:cap 64 :ttl :forever}) d stock))]
    (cond-> {:primitives (tu/fake {:inventory items}) :view (fn [] {:data data :now 1000})}
      left-s (assoc :left (fn [] {:spend {:job-time left-s}})))))

(deftest reserve-cost-is-value-times-odds
  (are [s cost-of] (= cost-of (Math/round (cost/reserve-cost 10 s)))
    0 0
    0.5 10
    0.9 90
    1 ##Inf))

(deftest a-replace-strength-prices-a-spent-unit-above-the-item-and-below-never
  (are [name] (let [v (:value (cost/item-value [(item name 1)]))
                    price (cost/reserve-cost v (reserve/replace-strength name))]
                (and (< (reserve/replace-strength name) 1) (js/isFinite price) (< v price)))
    "oak_log" "cobblestone" "white_bed"))

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

(deftest entries-expire-by-time-and-not-by-a-dead-job
  (are [entry live] (= live (boolean (seq (reserve/held (body [(item "diamond" 1)] [(merge {:item "diamond"} entry)]) :tools))))
    {:until 2000} true
    {:until 1000} false
    {:by "agent"} true
    {:by "gone-job" :until 2000} true
    {:by "gone-job" :until 900} false
    {:by "j2265"} false))

(defn writer
  "A ctx for job j1 over a register in an atom; t the clock in ms."
  [t]
  (let [d (atom mem/empty-data)]
    {:id "j1" :token 1 :t t :d d
     :view (fn [] {:data @d :now @t})
     :remember (fn [kind data policy]
                 (swap! d #(-> % (mem/remove-entries kind (fn [e] (= (:id data) (:id (:data e)))))
                               (mem/add-entry kind {:t @t :data data} policy))))
     :forget (fn [kind pred] (swap! d mem/remove-entries kind (fn [e] (pred (:data e)))))}))

(deftest a-jobs-entries-are-stamped-renewed-while-it-runs-and-lapse-after-it
  (let [c (writer (atom 0))
        want [{:id "j1-a" :item "diamond" :count 1 :strength 1 :by "j1"}]
        ttl-ms #(- (:until (first (reserve/entries ((:view c))))) @(:t c))
        write! #(reserve/set-job-entries! c want)]
    (write!)
    (let [ttl (ttl-ms)]
      (is (pos? ttl) "stamped with an expiry")
      (reset! (:t c) (quot ttl 4))
      (write!)
      (is (= ttl (+ (quot ttl 4) (ttl-ms))) "still fresh: not rewritten")
      (reset! (:t c) (* 3 (quot ttl 4)))
      (write!)
      (is (= ttl (ttl-ms)) "past half its ttl: stamped anew")
      (reset! (:t c) (* 5 ttl))
      (is (empty? (reserve/entries ((:view c)))) "the job never renewed: it lapsed"))))

(deftest a-check-without-a-token-reads-a-due-reserve-without-writing-it
  (let [c (assoc (writer (atom 0)) :primitives (tu/fake {:inventory [(item "diamond" 1)]}))]
    (reserve/set-job-entries! c [{:id "j1/c0-a" :item "diamond" :count 1 :strength 1 :by "j1/c0"}])
    (let [ttl (:until (first (reserve/entries ((:view c)))))
          check (assoc c :token nil :remember #(throw (js/Error. "cut: the ownership token changed")))]
      (reset! (:t c) (* 3 (quot ttl 4)))
      (is (= {"diamond" 1} (reserve/held check :tools)))
      (is (= ttl (:until (first (reserve/entries ((:view c))))))))))

(deftest renewing-leaves-one-copy-of-the-entry
  (let [t (atom 0)
        c (assoc (writer t) :primitives (tu/fake {:inventory [(item "diamond" 1)]}))
        add! #(swap! (:d c) mem/add-entry :reserve {:t 0 :data %} reserve/policy)
        c (assoc c :remember (fn [kind data policy] (add! data)))]
    (add! {:id "j1-a" :item "diamond" :count 1 :strength 1 :by "j1" :until 3600000})
    (reset! t 3000000)
    (reserve/rows c :tools)
    (is (= 1 (count (reserve/entries ((:view c))))))))

(deftest rows-renew-a-running-jobs-entries-and-release-drops-them
  (let [c (assoc (writer (atom 0)) :primitives (tu/fake {:inventory [(item "diamond" 1)]}))]
    (reserve/set-job-entries! c [{:id "j1-a" :item "diamond" :count 1 :strength 1 :by "j1"}])
    (let [ttl (:until (first (reserve/entries ((:view c)))))]
      (reset! (:t c) (* 3 (quot ttl 4)))
      (reserve/rows c :tools)
      (is (> (:until (first (reserve/entries ((:view c))))) ttl)))
    (reserve/release! c)
    (is (empty? (reserve/entries ((:view c)))))))

(deftest the-built-in-rules-hold-through-the-same-answer
  (let [c (body [(item "golden_apple" 2) (item "stone_pickaxe" 1) (item "bread" 4)]
                [])]
    (is (= {"golden_apple" 2 "bread" 4} (reserve/held c :tools)))))

(deftest the-food-reserve-carries-the-job-time-left-and-counts-a-booked-chest-of-food-for-the-rest
  (are [stock left-s held] (= held (reserve/held (body [(item "bread" 20)] [] :stock stock :left-s left-s) :deposit))
    [] 600 {"bread" 12}
    [] 7200 {"bread" 20}
    [{:pos [3 64 0] :items {"bread" 64}}] 1200 {"bread" 5}
    [{:pos [3 64 0] :items {"bread" 64}}] 3600 {"bread" 13}
    [{:pos [3 64 0] :items {"wheat" 90}}] 1200 {"bread" 5}
    [{:pos [3 64 0] :items {"cobblestone" 64}}] 1200 {"bread" 12}
    [{:pos [900 64 0] :items {"bread" 64}}] 1200 {"bread" 6}))

(deftest own-zone-crops-count-at-any-distance-and-are-read-again-once-memory-moves
  (let [crops {"200,64,0" "wheat" "201,64,0" "wheat" "202,64,0" "wheat" "204,64,0" "melon" "205,64,0" "melon_stem"}
        p (tu/seeing-all (tu/fake {:inventory [(item "bread" 20)] :blocks crops :ages {"200,64,0" 7 "201,64,0" 7 "202,64,0" 7}}))
        st #js {:writes 0}
        reads (atom 0)
        at (aget p "seenBlockAt")
        _ (set! (.-mobSource st) p)
        _ (aset p "perception" {:st st})
        _ (aset p "seenBlockAt" (fn [pos] (swap! reads inc) (.call at p pos)))
        c {:primitives p :view (fn [] {:data mem/empty-data :now 1000})
           :engine {:world (ew/of-data {} {} [{:name "farm" :min [200 64 0] :max [205 64 0] :owner "Fake"}])}}
        crop-points #(reduce + (map :points (filter (comp #{:crop} :kind) (home-food/field-sources c))))]
    (is (< (abs (- 15 (crop-points))) 1e-9) "three ripe wheat 200 blocks off, a loaf, and a melon beside its stem, 5 slices of 2: past the seen-block query's 64")
    (let [first-reads @reads]
      (crop-points)
      (is (= first-reads @reads) "memory unchanged: the crops are not read again")
      (swap! (fake/state p) fake/drop-block [200 64 0])
      (set! (.-writes st) 1)
      (is (< (abs (- (+ 10 (/ 10 3)) (crop-points))) 1e-9) "a crop dug since (the memory's writes moved) no longer counts"))))

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
