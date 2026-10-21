(ns engine.fell-contract-test
  "jobs.forestry.fell-tree as a contract job: it exports the contract and ends with a kind and a cause (0 of N is never
  done); the pillar is a way of the felling."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.event-api :as event-api]
            [engine.fake :as fake]
            [engine.core :as core]
            [engine.farm-contract-test :refer [ending-of]]
            [engine.fell-tree-test :as ft]
            [engine.forest-maintain-test :as fm]
            [engine.library-test :as lt]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [jobs.lib.access.approach :as approach]
            [jobs.lib.ledger :as ledger]
            [jobs.lib.look :as look]
            [jobs.lib.trees :as trees]
            [jobs.forestry.fell-tree :as fell-tree]
            [jobs.forestry.fell-tree.log :as log]
            [jobs.forestry.fell-tree.pillar :as pillar]
            [jobs.lib.access :as access]
            [engine.core.activity :as activity]
            [engine.memory :as mem]))

(defn ^:async quote-of [eng sym args]
  (:quote (await (event-api/quote-view eng (str sym) (pr-str args)))))

(deftest fell-tree-exports-the-contract-and-quotes-validly
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (fm/start ft/tall {})
              sym 'jobs.forestry.fell-tree]
          (is (vector? (:target (get registry/jobs sym))))
          (is (nil? (contract/quote-problem (await (quote-of (:eng s) sym {:radius 10}))))))))))

(deftest fell-trees-walk-is-go-tos-quote
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (fm/start ft/tall {})
              walk (await (quote-of eng 'jobs.movement.go-to {:pos [6 64 0] :range 2}))
              fell (await (quote-of eng 'jobs.forestry.fell-tree {:radius 10}))]
          (is (js/isFinite (first (get-in walk [:uses :job-time]))))
          (is (= (+ 2 (first (get-in walk [:uses :job-time]))) (first (get-in fell [:uses :job-time])))))))))

(defn ^:async fell-ending
  "The ending of job with args on a fresh world of spec, in at most n ticks (nil when it does not end)."
  ([spec job args n] (fell-ending spec job args n nil))
  ([spec job args n after-start]
   (let [s (fm/start spec {})]
     (ft/eye-reach! (:p s) (:lose-logs? spec))
     (when after-start (after-start s))
     (assoc (select-keys s [:p :seen :eng]) :end (await (ending-of s job args n))))))

(defn kc [e] {:kind (:kind e) :cause (contract/cause-key e)})

(deftest a-felled-tree-is-done-and-a-missing-one-is-condition-failed-no-tree
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end]} (await (fell-ending ft/tall 'jobs.forestry.fell-tree {:radius 10} 200))]
          (is (= :done (:kind end)))
          (is (= {:base {:x 6 :y 64 :z 0} :logs 8 :axe :none-found} (dissoc (:data end) :text))))
        (let [{:keys [end]} (await (fell-ending {:blocks {}} 'jobs.forestry.fell-tree {:radius 10} 40))]
          (is (= {:kind :condition-failed :cause :no-tree} (kc end)))
          (is (= 10 (get-in end [:cause :radius]))))))))

(deftest no-tree-is-a-look-until-the-body-has-looked-round
  (let [c {:primitives (tu/fake {:blocks {}}) :view (constantly {:now 0}) :args {:radius 10}}
        status (fn [surveyed?] (with-redefs [ctx/mem (constantly {}) look/surveyed? (fn ([_] surveyed?) ([_ _] surveyed?))] (fell-tree/no-tree c)))]
    (is (= [:look :no-tree] ((juxt first (comp :reason last)) (status false))))
    (is (= [:later :no-tree] ((juxt first (comp :reason last)) (status true))))))

(deftest an-at-column-with-no-log-ends-impossible-no-tree-naming-the-column
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [at {:x 6 :y 64 :z 0}
              {:keys [end]} (await (fell-ending {:blocks {}} 'jobs.forestry.fell-tree {:at at} 40))]
          (is (= {:kind :impossible :cause :no-tree} (kc end)))
          (is (= at (get-in end [:cause :at]))))))))

(deftest an-at-column-with-no-log-is-never-not-later
  (let [at {:x 6 :y 64 :z 0}
        c {:primitives (tu/fake {:blocks {}}) :view (constantly {:now 0}) :args {:at at}}
        status (with-redefs [ctx/mem (constantly {}) look/surveyed? (fn ([_] true) ([_ _] true))] (fell-tree/no-tree c))]
    (is (= [:never :no-tree at] [(first status) (:reason (last status)) (:at (last status))]))))

(deftest a-column-not-yet-seen-is-walked-to-not-looked-for
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (fm/start {:blocks {}} {})]
          (core/submit! eng '(jobs.forestry.fell-tree {:at {:x 30 :y 64 :z 0}}) {})
          (await (core/tick! eng))
          (is (empty? (filterv #(= :waiting (:kind %)) @seen)) "not held back by a look-first no-tree"))))))

(deftest every-tree-out-of-reach-is-nothing-possible-tree-unreachable-with-the-columns
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end]} (await (fell-ending {:blocks (lt/tree 8 0 "oak" 3) :unreachable ["8,64,0"]}
                                                'jobs.forestry.fell-tree {:radius 20} 100))]
          (is (= {:kind :nothing-possible :cause :tree-unreachable} (kc end)))
          (is (= 1 (get-in end [:data :unreachable])))
          (is (= [[8 0]] (get-in end [:data :columns]))))))))

(deftest a-pillar-that-cannot-be-had-ends-with-the-fetch-ending-and-fetch-false-with-the-need
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end p]} (await (fell-ending (assoc ft/tall :inventory [] :lose-logs? true) 'jobs.forestry.fell-tree {:radius 10 :ways #{:carried :pillar}} 100))]
          (is (= {:kind :condition-failed :cause :need} (kc end)))
          (is (= ["dirt"] (get-in end [:cause :any-of])))
          (is (pos? (get-in end [:cause :count])))
          (is (= [] (filter #(= "jumpPlace" (.-name %)) (.-calls (.-world p))))))))))

(deftest a-call-without-the-pillar-way-leaves-the-tall-tree
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end p]} (await (fell-ending ft/tall 'jobs.forestry.fell-tree {:radius 10 :ways #{:carried :chest :craft :gather}} 200))]
          (is (= {:kind :nothing-possible :cause :tree-unreachable} (kc end)))
          (is (= [] (filter #(= "jumpPlace" (.-name %)) (.-calls (.-world p))))))))))

(deftest a-refused-pillar-leaves-the-tree-and-the-call-ends-tree-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [zones [{:name "grove" :min [-6 64 -9] :max [16 80 9] :allow #{:dig}}]
              {:keys [end]} (await (fm/with-zones zones #(fell-ending ft/tall 'jobs.forestry.fell-tree {:radius 10} 120)))]
          (is (= {:kind :nothing-possible :cause :tree-unreachable} (kc end))))))))

(deftest a-tall-tree-is-felled-from-a-pillar-taken-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end eng]} (await (fell-ending ft/tall 'jobs.forestry.fell-tree {:radius 10} 200))]
          (is (= :done (:kind end)))
          (is (= [] (ledger/open-entries (mem/view (:store eng)))) "the pillar is taken back"))
        (let [zones [{:name "grove" :min [-6 64 -9] :max [16 80 9] :allow #{:dig}}]
              {:keys [end]} (await (fm/with-zones zones #(fell-ending ft/tall 'jobs.forestry.fell-tree {:radius 10} 120)))]
          (is (= {:kind :nothing-possible :cause :tree-unreachable} (kc end)) "a refused pillar leaves the tree"))))))

(deftest a-pillar-with-no-log-in-reach-is-taken-back-and-the-tree-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [end eng]} (with-redefs [approach/plan (fn [_] {:stand [[5 64 0]]})]
                                  (await (fell-ending ft/tall 'jobs.forestry.fell-tree {:radius 10} 200)))]
          (is (= {:kind :nothing-possible :cause :tree-unreachable} (kc end)))
          (is (= [] (ledger/open-entries (mem/view (:store eng))))))))))

(deftest a-felling-cut-on-its-pillar-takes-it-back-in-its-cleanup
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (fm/start ft/tall {})
              eng (:eng s)
              open #(ledger/open-entries (mem/view (:store eng)))
              dug (atom nil)
              dig! log/dig-log!]
          (ft/eye-reach! (:p s) (:lose-logs? ft/tall))
          (with-redefs [log/dig-log! (fn ^:async cut-first [& args]
                                       (when (and (not @dug) (seq (open)))
                                         (reset! dug (seq (open)))
                                         (core/cancel! eng (first (:list (core/state eng)))))
                                       (await (apply dig! args)))]
            (core/submit! eng '(jobs.forestry.fell-tree {:radius 10}) {})
            (await (tu/run-until-empty eng 200)))
          (is (seq @dug) "the cut came while the pillar stood")
          (is (= [] (open)) "the cleanup took the pillar back"))))))

(defn fake-drop [x y z]
  #js {:id 99 :pos #js {:x x :y y :z z} :item #js {:name "oak_log"}})

(deftest logs-in-reach-of-the-pillar-top-are-collected-before-the-pillar-comes-down
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (fm/start ft/tall {})
              eng (:eng s)
              open-at-collect (atom [])
              call! ctx/call]
          (ft/eye-reach! (:p s) (:lose-logs? ft/tall))
          (with-redefs [pillar/own-log-drop-entities (fn [_ _ r] (if (= r trees/dig-reach) [(fake-drop 8.5 71 2.5)] []))
                        ctx/call (fn [c k job & more]
                                   (when (= :tcollect0 k)
                                     (swap! open-at-collect conj (boolean (seq (ledger/open-entries (mem/view (:store eng)))))))
                                   (apply call! c k job more))]
            (core/submit! eng '(jobs.forestry.fell-tree {:radius 10}) {})
            (await (tu/run-until-empty eng 200)))
          (is (= [true] @open-at-collect) "one collect, made while the pillar still stood"))))))

(deftest a-leaf-between-the-eye-and-a-log-in-reach-is-dug-before-the-log
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [spec {:blocks (merge (lt/tree 3 0 "oak" 3) {"2,64,0" "oak_leaves" "2,65,0" "oak_leaves"})}
              s (fm/start spec {})]
          (ft/eye-reach! (:p s) (:lose-logs? spec))
          (core/submit! (:eng s) '(jobs.forestry.fell-tree {:radius 10}) {})
          (await (tu/run-until-empty (:eng s) 200))
          (is (some #{[2 64 0] [2 65 0]} (take 2 (fm/digs (:p s)))) "a leaf in the line is the first dig")
          (is (empty? (lt/calls (:p s) "walk")) "no walk round the tree to a sight of the log"))))))

(deftest a-drop-resting-on-a-leaf-out-of-reach-is-lowered-by-digging-the-leaf
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (fm/start ft/tall {})
              eng (:eng s)
              lowered (atom [])
              call! ctx/call]
          (ft/eye-reach! (:p s) (:lose-logs? ft/tall))
          (fake/set-block! (:p s) [8 70 2] "oak_leaves")
          (with-redefs [pillar/own-log-drop-entities (fn [_ _ r] (if (= r trees/dig-reach) [(fake-drop 8.5 71 2.5)] []))
                        ctx/call (fn [c k job & [args :as more]]
                                   (when (= :lower0 k) (swap! lowered conj (:pos args)))
                                   (if (= :tcollect0 k)
                                     {:kind :nothing-possible :data {:left [{:pos {:x 8 :y 71 :z 2} :reason :unreachable}]}}
                                     (apply call! c k job more)))]
            (core/submit! eng '(jobs.forestry.fell-tree {:radius 10}) {})
            (await (tu/run-until-empty eng 200)))
          (is (= [{:x 8 :y 70 :z 2}] (distinct @lowered)) "the leaf under the drop was dug"))))))

(deftest the-pillar-take-back-runs-in-an-urgent-section
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [s (fm/start ft/tall {})
              eng (:eng s)
              levels (atom [])
              dig! log/dig-log!
              call! ctx/call]
          (ft/eye-reach! (:p s) (:lose-logs? ft/tall))
          (with-redefs [log/dig-log! (fn ^:async cut-first [& args]
                                       (when (and (empty? @levels) (seq (ledger/open-entries (mem/view (:store eng)))))
                                         (swap! levels conj :cut)
                                         (core/cancel! eng (first (:list (core/state eng)))))
                                       (await (apply dig! args)))
                        ctx/call (fn [c k job & more]
                                   (when (= 'jobs.access.cleanup job)
                                     (swap! levels conj (activity/section-level eng (:token c))))
                                   (apply call! c k job more))]
            (core/submit! eng '(jobs.forestry.fell-tree {:radius 10}) {})
            (await (tu/run-until-empty eng 200)))
          (is (= [:cut :urgent] (take 2 @levels))))))))

(deftest a-limit-or-need-ending-for-the-pillar-blocks-ends-the-call-and-another-leaves-the-tree
  (with-redefs [ctx/mem (constantly {:dug 2})
                log/mark-unreachable! (fn ([_] nil) ([_ _] nil))]
    (are [e k cause] (let [r (pillar/pass-up! {} e)]
                       (and (= k (:kind r)) (= cause (:cause r)) (= {:logs 2} (:data r))))
      (contract/ending :limit-reached :job-time {}) :limit-reached :job-time
      (contract/ending :condition-failed :need {}) :condition-failed :need)
    (is (= :again (pillar/pass-up! {} (contract/ending :nothing-possible :no-source {}))))))

(deftest the-pillar-plan-aims-at-fewer-logs-past-a-zone-refusal
  (let [logs (mapv (fn [y] {:pos {:x 0 :y y :z 0}}) [64 65 66 67 68])
        asked (atom [])]
    (with-redefs [access/rules-input (fn ([_] {}) ([_ _] {}))
                  approach/plan (fn [in]
                                  (swap! asked conj (count (:targets in)))
                                  (if (> (count (:targets in)) 2) {:reason :zone} {:pillar {:height 3}}))]
      (is (= {:pillar {:height 3}} (pillar/pillar-plan {:args {}} logs #{})))
      (is (= [5 3 1] @asked)))))

(deftest the-pillar-plan-searches-at-most-three-times-however-tall-the-tree
  (let [logs (mapv (fn [y] {:pos {:x 0 :y y :z 0}}) (range 64 94))
        asked (atom 0)]
    (with-redefs [access/rules-input (fn ([_] {}) ([_ _] {}))
                  approach/plan (fn [_] (swap! asked inc) {:reason :no-stand})]
      (is (= {:reason :no-stand} (pillar/pillar-plan {:args {}} logs #{})))
      (is (= 3 @asked)))))

(defn ^:async step-with-scaffold
  "pillar/round! on a call with no pillar yet and an open scaffold; the cleanup child leaves it open-after."
  [open-after cleanup-result]
  (let [open (atom true) started (atom false)]
    (with-redefs [ctx/mem (constantly {})
                  log/open-scaffold? (fn [_] @open)
                  ctx/atomic (fn [_ _ _ f] (f))
                  ctx/call (fn [& _] (reset! open open-after) cleanup-result)
                  ctx/end (fn ([_ kind cause] [kind cause]) ([_ kind cause _] [kind cause]))
                  log/blocked! (fn [_ text] [:blocked text])
                  pillar/start-pillar! (fn [_] (reset! started true) :started)]
      {:r (await (pillar/round! {:id "j"})) :started @started})))

(deftest a-call-starting-on-an-open-scaffold-takes-it-back-before-it-plans
  (async done
    (tu/run-async done
      (fn ^:async t []
        (is (= {:r :started :started true} (await (step-with-scaffold false :done))) "closed ledger: the call goes on to plan")
        (is (= {:r [:blocked "cannot take the pillar back"] :started false} (await (step-with-scaffold true :done))) "still open: ends, plans nothing")
        (is (= {:r :continue :started false} (await (step-with-scaffold true :continue))) "the cleanup child waits")))))

(deftest an-open-scaffold-cell-a-cleanup-held-does-not-keep-the-felling-open
  (with-redefs [ctx/view (constantly nil)
                ledger/open-entries (constantly [{:cell [1 5 1] :owner "j"} {:cell [1 6 1] :owner "j"}])
                ledger/of-instance? (constantly true)
                ledger/held-cells (constantly #{[1 6 1]})]
    (is (true? (log/open-scaffold? {:id "j"})) "one cell is not held")
    (with-redefs [ledger/held-cells (constantly #{[1 5 1] [1 6 1]})]
      (is (false? (log/open-scaffold? {:id "j"})) "all held: left to the scaffold-left trigger"))))

(deftest fell-tree-quotes-axe-wear-and-the-logs-it-picks-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (fm/start (assoc ft/tall :inventory [{:name "iron_axe" :count 1}]) {})
              q (await (quote-of eng 'jobs.forestry.fell-tree {:radius 10}))]
          (is (= {"iron_axe" [8 8 8]} (get-in q [:uses :wear])))
          (is (= {:items {"oak_log" [0 8 8]}} (:yields q)) "the logs are picked up"))))))
