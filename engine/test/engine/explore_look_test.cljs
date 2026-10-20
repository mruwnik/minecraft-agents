(ns engine.explore-look-test
  (:require [cljs.test :refer [deftest is are async]] [engine.core :as core] [engine.expr :as expr]
            [engine.events :as events] [engine.registry :as registry]
            [engine.test-util :as tu] [jobs.explore.look :as look]))
(deftest limits-and-types-are-the-arg-specs
  (are [form] (some? (expr/problem registry/jobs form))
    '(jobs.explore.look {:radius 1000})
    '(jobs.explore.look {:max-blocks 100})
    '(jobs.explore.look {:max-entities -1})
    '(jobs.explore.look {:entity-names ["sheep" 3]})
    '(jobs.explore.look {:properties? "true"})
    '(jobs.explore.look {:bogus 1})))
(deftest name-filters-are-validated-before-sensing
  (is (= ["stone"] (:block-names (look/options {:block-names "stone"}))))
  (is (:error (look/options {:block-names []})))
  (is (:error (look/options {:block-names ["Stone Brick"]})))
  (is (:error (look/options {:useful? true :block-names ["stone"]})))
  (is (:error (look/options {:at [1 nil 3]}))))
(deftest block-names-are-refused-by-name
  (is (re-find #"stone_bricks_x" (:error (look/options {:block-names ["stone" "stone_bricks_x"]} (fn [n] (= n "stone"))))))
  (is (nil? (:error (look/options {:block-names ["stone"]} (fn [n] (= n "stone"))))))
  (is (nil? (:error (look/options {:block-names ["nonsense"]})))))
(deftest a-queued-look-with-a-bad-key-or-block-emits-bad-args
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/seeing-all (tu/fake {:blocks (tu/box 1 64 0 3 64 0 "stone")}))
              [seen sink] (tu/capture-sink)
              eng (core/create {:primitives p :jobs registry/jobs :dir (tu/tmp-dir)
                                :events (events/make {:sinks [sink]})})]
          (core/submit! eng '(jobs.explore.look {:block-names ["no_such_block"]}) {})
          (await (core/tick! eng))
          (await (core/tick! eng))
          (let [event (first (filter #(= :stopped (:kind %)) @seen))]
            (is (= :bad-args (get-in event [:data :reason])))
            (is (re-find #"no_such_block" (pr-str (:data event))))
            (is (empty? (filter #(= :look.refused (:kind %)) @seen)))
            (is (= :impossible (get-in event [:data :kind])))
            (is (empty? (:list (core/state eng))) "impossible: it leaves the queue")))))))
(deftest loaded-air-unloaded-cell-and-exact-properties-have-distinct-evidence
  (let [p (tu/seeing-all (tu/fake {:blocks {"1,64,0" "wheat"} :ages {"1,64,0" 7} :states {"1,64,0" {:age 7}} :unloaded ["4,64,0"]}))]
    (is (= {:name "wheat" :pos [1 64 0] :properties {:age "7"} :seen? true}
           (:at (look/observe p (look/options {:at [1 64 0]})))))
    (is (= {:name "air" :pos [2 64 0] :seen? true}
           (:at (look/observe p (look/options {:at [2 64 0]})))))))
(deftest a-block-never-seen-is-unknown-not-reported-through-a-wall
  (let [p (tu/fake {:blocks {"1,64,0" "diamond_ore" "5,64,0" "stone"}
                    :entities [{:id 1 :name "Bob" :kind "player" :username "Bob" :pos {:x 6 :y 64 :z 0}}]})]
    (aset p "seenBlocks" (fn [q] (.filter (.blocks p q) (fn [b] (< (aget b "pos" "x") 3)))))
    (aset p "seenBlockAt" (fn [pos] (if (< (aget pos "x") 3)
                                      #js {:name "diamond_ore" :pos pos :age-ms 0}
                                      #js {:unknown true :pos pos})))
    (let [r (look/observe p (look/options {:max-entities 4 :at [5 64 0]}))]
      (is (= [[1 64 0]] (mapv :pos (:blocks r))) "the block in view is listed, not the one behind the wall")
      (is (= {:pos [5 64 0] :unknown true} (:at r)))
      (is (= ["Bob"] (mapv :name (:entities r))) "a player is listed even behind the wall"))))
(deftest a-queued-look-emits-useful-bounded-observations-without-acting
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (tu/seeing-all (tu/fake {:blocks (tu/box 1 64 0 10 64 0 "stone")
                          :entities [{:id 1 :name "sheep" :kind "passive" :uuid "sheep-uuid" :pos {:x 2 :y 64 :z 0}}]}))
              [seen sink] (tu/capture-sink)
              eng (core/create {:primitives p :jobs registry/jobs :dir (tu/tmp-dir)
                                :events (events/make {:sinks [sink]})})]
          (core/submit! eng '(jobs.explore.look {:max-blocks 2 :radius 16}) {})
          (await (core/tick! eng))
          (let [event (first (filter #(= :completed (:kind %)) @seen))]
            (is (empty? (filter #(= :look.observed (:kind %)) @seen)))
            (is (= [[1 64 0] [2 64 0]] (mapv :pos (get-in event [:data :blocks]))))
            (is (get-in event [:data :more-blocks?]))
            (is (= "sheep-uuid" (get-in event [:data :entities 0 :uuid])))
            (is (= :seen (get-in event [:data :scope]))))
          (is (empty? (:list (core/state eng))))
          (is (empty? (.-calls (.-world p)))))))))
(deftest a-refused-look-ends-stopped-with-the-reason-not-completed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [[seen sink] (tu/capture-sink)
              eng (core/create {:primitives (tu/fake {}) :jobs registry/jobs :dir (tu/tmp-dir)
                                :events (events/make {:sinks [sink]})})]
          (core/submit! eng '(jobs.explore.look {:max-blocks 32}) {})
          (await (core/tick! eng))
          (let [ended (first (filter #(#{:completed :stopped} (:kind %)) @seen))]
            (is (= :stopped (:kind ended)))
            (is (= :bad-args (get-in ended [:data :reason])))
            (is (re-find #"max-blocks" (str (get-in ended [:data :text])))))))))
(deftest block-name-checker-rejects-prototype-pollution-names
  (let [p (tu/fake {})
        checker (look/block-name-checker p)]
    (is (false? (checker "constructor")) "constructor should not be a valid block name")
    (is (false? (checker "__proto__")) "__proto__ should not be a valid block name")))
(deftest block-names-with-constructor-and-proto-are-rejected
  (let [p (tu/fake {})
        checker (look/block-name-checker p)]
    (is (re-find #"unknown block names" (:error (look/options {:block-names ["constructor"]} checker))))
    (is (re-find #"unknown block names" (:error (look/options {:block-names ["__proto__"]} checker)))))))

(deftest useful-lists-seen-workstations-storage-beds-and-ore-nearest-first-with-distance
  (let [p (tu/seeing-all (tu/fake {:blocks {"3,64,0" "crafting_table" "1,64,0" "stone" "2,64,0" "oak_log"
                                            "6,64,0" "iron_ore" "9,64,0" "red_bed" "5,64,4" "chest" "4,64,0" "furnace"}
                                   :self {:pos [0 64 0]}}))
        r (look/observe p (look/options {:useful? true :max-blocks 16}))]
    (is (= ["crafting_table" "furnace" "iron_ore" "chest" "red_bed"] (mapv :name (:blocks r))))
    (is (= [30 40 60 64 90] (mapv #(js/Math.round (* 10 (:distance %))) (:blocks r))) "distance in blocks, rounded"))
  (is (:error (look/options {:useful? true :block-names ["stone"]})) "useful? and block-names are exclusive"))
