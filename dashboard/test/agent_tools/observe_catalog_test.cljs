(ns agent-tools.observe-catalog-test
  (:require [cljs.test :refer [deftest is async]]
            [agent-tools.observe.catalog :as catalog]
            [agent-tools.world-data :as data]
            [clojure.string :as str]))

(def jobs
  {"jobs.items.get-kit" {:tag "goal" :doc "Get :items in order (the iron kit).\n  One call is the whole kit."}
   "jobs.survival.get-food" {:tag "goal" :category "reflex" :doc "Keep the body fed. More."}
   "jobs.farm.till" {}
   "jobs.farm.plant" {}
   "jobs.items.craft" {}
   "jobs.access.clear-path" {:category "phase"}
   "jobs.survival.unwedge" {:category "reflex"}
   "jobs.debug.notify" {:category "debug"}
   "jobs.primitive.dig" {:category "phase"}
   "jobs.primitive.place" {:category "phase"}})

(defn fake-get
  "The catalog endpoint over jobs, paging 2 names at a time whatever the limit asked."
  [_ request-path _]
  (let [params (js/URLSearchParams. (second (str/split request-path #"\?")))
        p #(.get params %)]
    (js/Promise.resolve
     (if (= "job" (p "kind"))
       {:status 200 :text (data/write-edn {:ok true :doc (get-in jobs [(p "name") :doc])})}
       (let [names (sort (keep (fn [[n m]] (when (and (or (nil? (p "category")) (= (p "category") (:category m)))
                                                      (or (nil? (p "tag")) (= (p "tag") (:tag m))))
                                             n))
                               jobs))
             offset (js/Number (p "offset"))
             items (take 2 (drop offset names))]
         {:status 200 :text (data/write-edn {:ok true :items (vec items) :total (count names)
                                             :next-offset (when (< (+ offset 2) (count names)) (+ offset 2))})})))))

(deftest the-catalog-by-level-gives-goal-jobs-a-doc-line-steps-by-family-and-counts-the-rest
  (async done
    (.then (catalog/read! fake-get "sock" {:all false})
           (fn [answer]
             (is (= [['jobs.items.get-kit "Get :items in order (the iron kit)."]
                     ['jobs.survival.get-food "Keep the body fed."]]
                    (:goal answer)))
             (is (= {:farm '[plant till] :items '[craft]} (:step answer)))
             (is (= [1 1 2] ((juxt :phase :reflex :primitive) answer)))
             (done)))))

(deftest all-names-the-phase-reflex-and-primitive-jobs
  (async done
    (.then (catalog/read! fake-get "sock" {:all true})
           (fn [answer]
             (is (= ['[jobs.access.clear-path] '[jobs.survival.unwedge] '[jobs.primitive.dig jobs.primitive.place]]
                    ((juxt :phase :reflex :primitive) answer)))
             (done)))))

(deftest a-failed-catalog-page-is-an-error-answer
  (async done
    (.then (catalog/read! (fn [_ _ _] (js/Promise.resolve {:status 400 :text "{:ok false}"})) "sock" {:all false})
           (fn [answer]
             (is (= {:ok false :reason :catalog-failed :status 400} answer))
             (done)))))
