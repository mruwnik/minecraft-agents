(ns dashboard.entity-observations-test
  (:require [cljs.test :refer [deftest is]]
            [dashboard.entity-observations :as e]))

(defn observation [world uuid dimension t]
  {:key (str "villager/" uuid) :uuid uuid :type "villager" :world world :dimension dimension
   :pos {:x t :y 65 :z 0} :observed-at t :expires-at (+ t e/ttl-ms)})

(defn source [name world observations now]
  (e/body-snapshot {:name name :world world}
                   {:ok true :body name :world world :online? true :entities observations} now))

(deftest actual-observation-time-controls-expiry
  (let [entry (source "Bob" "a" [(assoc (observation "a" "v" "overworld" 10) :expires-at 9999999)] 20)
        cache {[:a :Bob] entry}]
    (is (= 120010 (get-in entry [:entities 0 :expires-at])))
    (is (= 1 (:count (e/merge-world cache "a" nil 120009))))
    (is (= 0 (:count (e/merge-world cache "a" nil 120010))))
    (is (= 0 (:count (e/merge-world (assoc-in cache [[:a :Bob] :status] :unavailable) "a" nil 120010))))))

(deftest newest-stable-uuid-wins-before-dimension-filter
  (let [cache {1 (source "Old" "a" [(observation "a" "v" "overworld" 10)] 40)
               2 (source "New" "a" [(observation "a" "v" "the_nether" 20)] 40)
               3 (source "OtherWorld" "b" [(observation "b" "v" "overworld" 30)] 40)}]
    (is (= [] (:entities (e/merge-world cache "a" "overworld" 40))))
    (is (= ["New"] (mapv :seen-by (:entities (e/merge-world cache "a" "the_nether" 40)))))
    (is (= ["OtherWorld"] (mapv :seen-by (:entities (e/merge-world cache "b" nil 40)))))))

(deftest equal-times-are-deterministic-and-ephemeral-identities-stay-local
  (let [v (observation "a" "v" "overworld" 10)
        ephemeral (dissoc v :uuid)
        cache {2 (source "Z" "a" [v ephemeral] 20)
               1 (source "A" "a" [v ephemeral] 20)}
        snapshot (e/merge-world cache "a" nil 20)]
    (is (= 3 (:count snapshot)))
    (is (= "A" (:seen-by (first (filter :uuid (:entities snapshot))))))))

(deftest all-moving-types-are-kept-and-villager-count-is-filtered
  (let [v (observation "a" "v" "overworld" 10)
        observations (mapv #(assoc v :key % :uuid % :type %) ["villager" "player" "item" "boat"])
        snapshot (e/merge-world {1 (source "A" "a" observations 20)} "a" nil 20)]
    (is (= 4 (:count snapshot)))
    (is (= #{"villager" "player" "item" "boat"} (set (map :type (:entities snapshot)))))
    (is (= 1 (:count (e/villagers snapshot))))))

(deftest failed-sources-remain-visible-without-renewing-observations
  (let [entry (assoc (source "A" "a" [(observation "a" "v" "overworld" 10)] 20)
                     :status :unsupported :online? nil :error "restart required")
        snapshot (e/merge-world {1 entry} "a" nil 30)]
    (is (= 10 (get-in snapshot [:entities 0 :observed-at])))
    (is (= :unsupported (get-in snapshot [:sources 0 :status])))
    (is (= "restart required" (get-in snapshot [:sources 0 :error])))))

(deftest cache-bounds-sources-and-records-with-explicit-truncation
  (with-redefs [e/max-sources 2 e/max-cached 1 e/max-output 1]
    (let [cache (into {} (for [i (range 3)]
                           [i (assoc (source (str i) "a" [(observation "a" (str i) "overworld" i)] 5)
                                     :requested-at i)]))
          bounded (e/bound cache 5)
          snapshot (e/merge-world bounded "a" nil 5)]
      (is (= 2 (count bounded)))
      (is (= 1 (:count snapshot)))
      (is (true? (:truncated? snapshot))))))

(deftest aggregate-byte-cap-includes-source-status-overhead
  (with-redefs [e/max-output-bytes 8192]
    (let [observations (mapv #(observation "a" (str %) "overworld" 10) (range 100))
          entry (assoc (source "A" "a" observations 20) :error (apply str (repeat 384 "\n")))
          snapshot (e/merge-world {1 entry} "a" nil 20)
          bytes (.-length (.encode (js/TextEncoder.) (pr-str snapshot)))]
      (is (<= bytes 8192))
      (is (true? (:truncated? snapshot)))
      (is (< (count (:entities snapshot)) 100)))))
