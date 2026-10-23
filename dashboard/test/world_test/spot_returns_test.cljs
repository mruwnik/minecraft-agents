(ns world-test.spot-returns-test
  (:require [cljs.test :refer [deftest is are]]
            [world-test.spot-returns :as s]))

(defn line [t kind data] (pr-str {:time-ms t :source (if (#{:died :hurt} kind) :body :job) :kind kind :data data}))
(defn at [x] {:x x :y 64 :z 0})
(def died (line 0 :died {:attacker {:name "zombie"} :pos (at 0)}))

(deftest a-line-gives-its-kind-of-record
  (are [l k] (= k (:k (s/scan-line l)))
    died :spot
    (line 0 :died {:damageType "fall" :pos (at 0)}) :pos
    (line 0 :hurt {:health 4 :causes ["skeleton"] :pos (at 0)}) :spot
    (line 0 :hurt {:health 14 :causes ["skeleton"] :pos (at 0)}) :met
    (line 0 :hurt {:health 4 :causes ["wolf"] :pos (at 0)}) :pos
    (line 0 :planned {:pos (at 0)}) :pos
    (line 0 :hostile.decided {:mob "spider" :pos (at 0)}) :met))

(deftest a-return-counts-after-five-minutes-between-32-and-128-blocks-off
  (let [counts #(s/returns (keep s/scan-line (cons died %)))]
    (are [lines want] (= want (select-keys (counts lines) [:n :k]))
      [(line 10000 :respawned {:pos (at 60)}) (line 400000 :planned {:pos (at 5)}) (line 420000 :hostile.decided {:mob "zombie" :pos (at 5)})] {:n 1 :k 1}
      [(line 10000 :respawned {:pos (at 60)}) (line 400000 :planned {:pos (at 5)})] {:n 1 :k 0}
      [(line 10000 :respawned {:pos (at 60)}) (line 400000 :planned {:pos (at 5)}) (line 420000 :hostile.decided {:mob "zombie" :pos (at 40)})] {:n 1 :k 0}
      [(line 10000 :respawned {:pos (at 60)}) (line 400000 :planned {:pos (at 5)}) (line 420000 :hurt {:health 3 :causes ["skeleton"] :pos (at 4)})] {:n 1 :k 1}
      [(line 10000 :respawned {:pos (at 60)}) (line 400000 :died {:attacker {:name "zombie"} :pos (at 2)})] {:n 1 :k 1}
      [(line 10000 :respawned {:pos (at 60)}) (line 100000 :planned {:pos (at 5)})] {:n 0 :k 0}
      [(line 10000 :respawned {:pos (at 300)}) (line 400000 :planned {:pos (at 5)})] {:n 0 :k 0}
      [(line 10000 :respawned {:pos (at 20)}) (line 400000 :planned {:pos (at 5)})] {:n 0 :k 0})))
