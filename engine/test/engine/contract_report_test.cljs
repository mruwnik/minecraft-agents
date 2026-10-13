(ns engine.contract-report-test
  "The tally of what a job's acts used and yielded, kept on every frame of the chain, and the report it ends with."
  (:require [cljs.test :refer [deftest is are async]]
            [cljs.reader :as reader]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.test-util :as tu]
            ["fs" :as fs]
            ["path" :as path]))

(defn snap
  ([inventory] (snap inventory {} {}))
  ([inventory equipment hurt] {:inventory inventory :equipment equipment :hurt hurt}))

(deftest an-act-uses-what-left-the-bag-and-yields-what-came-in
  (are [before after change expected] (= expected (contract/act-delta before after change))
    (snap [{:name "oak_log" :count 2 :slot 0}])
    (snap [{:name "oak_log" :count 1 :slot 0} {:name "oak_planks" :count 4 :slot 1}])
    nil
    {:uses {:items {"oak_log" 1}} :yields {:items {"oak_planks" 4}}}

    (snap [{:name "stone_pickaxe" :count 1 :slot 0 :durability 131}])
    (snap [{:name "stone_pickaxe" :count 1 :slot 3 :durability 129}])
    nil
    {:uses {:wear {"stone_pickaxe" 2}}}

    (snap [{:name "stone_pickaxe" :count 1 :slot 0 :durability 1}])
    (snap [])
    nil
    {:uses {:items {"stone_pickaxe" 1}}}

    (snap [{:name "iron_helmet" :count 1 :slot 0}])
    (snap [] {:head {:name "iron_helmet" :count 1} :mainHand nil} {})
    nil
    {}

    (snap [{:name "emerald" :count 5 :slot 0}])
    (snap [{:name "emerald" :count 5 :slot 0}])
    {"emerald" -3 "bread" 6}
    {:uses {:items {"emerald" 3}} :yields {:items {"bread" 6}}}

    (snap [] {} {"zombie" 4 "fall" 2})
    (snap [] {} {"zombie" 9 "fall" 5 "berry bush" 1 "drowning" 2 "lava" 4})
    nil
    {:uses {:health 6}}

    {:inventory nil}
    (snap [{:name "dirt" :count 1 :slot 0}])
    nil
    {}))

(deftest the-report-adds-the-round-time-to-the-uses
  (is (= {:uses {:items {"bread" 2} :time 1.5} :yields {:items {"dirt" 1}}}
         (contract/report {:tally {:uses {:items {"bread" 2}} :yields {:items {"dirt" 1}}} :ran-ms 1500})))
  (is (= {:uses {:time 0}} (contract/report nil))))

;; ---------------------------------------------------------------- the engine

(defn ^:async eat! [c] (await (ctx/act c :eat #js {:item "bread"})))

(def leaf
  {:name :leaf :check (constantly true)
   :round (fn ^:async leaf-round [c] (await (eat! c)) (ctx/update-mem! c assoc :ate true) :continue)})

(def mid
  {:name :mid :check (constantly true)
   :round (fn ^:async mid-round [c]
            (await (eat! c))
            (await (ctx/call-child c :b leaf {}))
            :continue)})

(defn setup []
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        dir (tu/tmp-dir)
        p (tu/fake {:self (assoc fake/default-self :food 2)
                    :inventory [{:name "bread" :count 9} {:name "oak_log" :count 3}]})
        eng (core/create {:primitives p :dir dir :now #(deref clock)
                          :jobs {'top {:check (constantly true)
                                       :round (fn ^:async top-round [c]
                                                (let [n (:n (ctx/mem c) 0)]
                                                  (ctx/update-mem! c assoc :n (inc n))
                                                  (if (zero? n)
                                                    (await (ctx/call-child c :a mid {}))
                                                    (await (eat! c)))
                                                  :continue))}
                                 'eats {:check (constantly true)
                                        :round (fn ^:async eats-round [c] (await (eat! c)) :continue)}
                                 'timed {:check (constantly true)
                                         :round (fn ^:async timed-round [c]
                                                  (swap! clock + 100)
                                                  (await (ctx/call-child c :t {:name :slow :check (constantly true)
                                                                               :round (fn [_] (swap! clock + 300) :done)}
                                                                         {}))
                                                  (swap! clock + 50)
                                                  :done)}
                                 'craft {:check (constantly true)
                                         :round (fn ^:async craft-round [c]
                                                  (await (ctx/act c :craft #js {:item "oak_planks" :count 4}))
                                                  :done)}}
                          :triggers {}
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :clock clock :seen seen :dir dir}))

(defn raw-mem [eng root slots] (mem/job-mem (mem/view (:store eng)) root slots))
(defn tally [eng root slots] (:tally (:contract (raw-mem eng root slots))))

(deftest every-frame-on-the-chain-counts-each-act
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup)
              id (core/submit! eng '(top) {})]
          (await (core/tick! eng))
          (is (= {:uses {:items {"bread" 2}}} (tally eng id [])))
          (is (= {:uses {:items {"bread" 2}}} (tally eng id [:a])))
          (is (= {:uses {:items {"bread" 1}}} (tally eng id [:a :b])))
          (await (core/tick! eng))
          (is (= {:uses {:items {"bread" 3}}} (tally eng id [])) "the root's own act")
          (is (= {:uses {:items {"bread" 2}}} (tally eng id [:a])) "is not the child's")
          (is (= {:ate true} (dissoc (ctx/mem (core/make-ctx eng {:root id :slots [:a :b] :chain [] :token nil
                                                                  :args {} :round 1}))
                                     :args :children))
              "a job does not see the tally in its memory"))))))

(deftest the-tally-survives-a-cut-and-is-on-disk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p dir]} (setup)
              id (core/submit! eng '(top) {})]
          (.override (.-world p) "eat"
                     (fn [token a impl]
                       (-> (impl token a)
                           (.then (fn [_] (.setOwner p "other") (js/Promise.reject (core/cut-error)))))))
          (await (core/tick! eng))
          (is (= [id] (:list (core/state eng))) "the cut job stays listed")
          (is (= {:uses {:items {"bread" 1}}} (tally eng id [])) "the cut act's bread is counted")
          (is (= {:uses {:items {"bread" 1}}} (tally eng id [:a])))
          (is (= {:uses {:items {"bread" 1}}}
                 (-> (reader/read-string (fs/readFileSync (path/join dir "memory.edn") "utf8"))
                     (get-in [:entries (mem/job-kind id) 0 :data :children :a :contract :tally])))))))))

(deftest health-counts-only-the-chosen-causes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup)
              id (core/submit! eng '(eats) {})]
          (.override (.-world p) "eat"
                     (fn [token a impl]
                       (.emit (.-world p) #js {:kind "hurt" :food 20 :health 15 :amount 3 :damageType "fall"})
                       (.emit (.-world p) (clj->js {:kind "hurt" :food 20 :health 13 :amount 2
                                                    :attacker {:id 9 :name "zombie"}}))
                       (impl token a)))
          (await (core/tick! eng))
          (is (= {:uses {:items {"bread" 1} :health 3}} (tally eng id []))))))))

(deftest round-time-is-booked-once-per-frame
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(timed) {})
          (await (core/tick! eng))
          (let [ended (first (filter #(= :child_ended (:kind %)) @seen))
                completed (first (filter #(= :completed (:kind %)) @seen))]
            (is (= {:uses {:time 0.3}} (:report ended)) "the child's own round")
            (is (= {:uses {:time 0.45}} (:report completed)) "the parent's round holds the child's once")))))))

(deftest a-finished-job-reports-and-is-remembered
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup)]
          (core/submit! eng '(craft) {})
          (await (core/tick! eng))
          (let [report {:uses {:items {"oak_log" 1} :time 0} :yields {:items {"oak_planks" 4}}}]
            (is (= report (:report (first (filter #(= :completed (:kind %)) @seen)))))
            (is (= [{:job 'craft :report report}]
                   (mapv :data (mem/entries (mem/view (:store eng)) :job-report))))))))))
