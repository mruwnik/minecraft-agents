(ns engine.withdraw-test
  "jobs.storage.withdraw against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.memory :as mem]
            [engine.fake :as fake]
            [engine.test-util :as tu :refer [run-until-empty child-outcome]]
            [engine.triggers :as triggers]
            [jobs.storage.withdraw :as wd]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake-on-floor world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn inv [p] (into {} (map (juxt #(.-name %) #(.-count %))) (.-inventory (.self p))))
(defn chest-items [p] (get-in @(fake/state p) [:containers [10 64 0]]))
(defn know-place! [eng kind pos] (mem/write! (:store eng) kind {:pos pos} mem/place-policy))

(def chest {:x 10 :y 64 :z 0})
(def job 'jobs.storage.withdraw)

(defn ^:async ending-of
  "Run job with args as a ctx/call child of a recording parent, at most n ticks: the child's {:kind :cause :data}
  (nil when it does not end); a nil cause is left out."
  [eng job args n]
  (let [out (atom nil)
        parent {:round (fn ^:async recording-round [c]
                         (let [e (await (ctx/call c :kid job args))]
                           (if (= :continue e) :continue (do (reset! out (into {} (remove (comp nil? val)) (select-keys e [:kind :cause :data]))) (ctx/end c :done nil)))))}
        eng (assoc-in eng [:jobs 'recording-parent] parent)]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng n))
    @out))

(def carried-text "withdraw: everything wanted is carried")

(deftest shortfall-lists-what-is-still-needed-in-order
  (let [items [{:name "bread" :count 3} {:name "dirt" :count 2} {:name "bread" :count 4}]]
    (are [wanted expected] (= expected (vec (wd/shortfall items wanted)))
      (array-map "bread" 10 "stone_hoe" 2) [["bread" 3] ["stone_hoe" 2]]
      (array-map "bread" 7) []
      (array-map "bread" 5) []
      (array-map "gravel" 1 "dirt" 5 "bread" 8) [["gravel" 1] ["dirt" 3] ["bread" 1]]
      {} [])))

(deftest withdraw-tops-up-to-the-target
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "bread" :count 4}] :containers {"10,64,0" [{:name "bread" :count 32}]}})
              result (await (ending-of eng job {:chest chest :items {"bread" 16}} 8))]
          (is (= {"bread" 16} (inv p)))
          (is (= [{:name "bread" :count 20}] (chest-items p)))
          (is (= {:kind :done :data {:text carried-text}} result)))))))

(deftest withdraw-with-enough-carried-moves-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [{:name "bread" :count 20}] :containers {"10,64,0" [{:name "bread" :count 32}]}})
              result (await (ending-of eng job {:chest chest :items {"bread" 16}} 4))]
          (is (zero? (count (calls p "transfer"))))
          (is (= {:kind :done :cause :already :data {:text carried-text}} result)))))))

(deftest withdraw-reports-what-the-chest-lacks
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers {"10,64,0" [{:name "bread" :count 5}]}})
              result (await (ending-of eng job {:chest chest :items (array-map "bread" 16 "stone_hoe" 2)} 8))]
          (is (= {"bread" 5} (inv p)))
          (is (= {:kind :condition-failed :cause :chest-lacks
                  :data {:gave-up false :held {} :short {"bread" 11 "stone_hoe" 2}
                         :text "took bread 5; no more bread, stone_hoe in the chests (the last holds nothing)"}} result)))))))

(deftest withdraw-with-a-least-met-by-what-the-chest-held-is-done
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[least kind] [[3 :done] [:all :condition-failed]]]
          (let [{:keys [eng]} (setup {:containers {"10,64,0" [{:name "bread" :count 5}]}})
                result (await (ending-of eng job {:chest chest :items {"bread" 16} :least least} 8))]
            (is (= kind (:kind result)) (str ":least " least))))))))

(deftest withdraw-names-the-reason-and-the-held-items-when-the-chest-lacks-them
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:containers {"10,64,0" [{:name "bread" :count 5} {:name "dirt" :count 2}]}})
              text (:text (:data (await (ending-of eng job {:chest chest :items {"shears" 1}} 8))))]
          (is (re-find #"no shears in this chest" text))
          (is (re-find #"bread 5" text))
          (is (re-find #"dirt 2" text)))))))

(deftest withdraw-finds-the-chest-in-places
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers {"10,64,0" [{:name "bread" :count 5}]}})]
          (core/submit! eng (list job {:items {"bread" 3}}) {})
          (is (nil? (core/tick! eng)) "no chest known: blocked")
          (is (empty? (calls p "transfer")))
          (know-place! eng :chest chest)
          (await (run-until-empty eng 6))
          (is (= {"bread" 3} (inv p))))))))

(deftest withdraw-gives-up-on-an-unreachable-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:containers {"10,64,0" [{:name "bread" :count 5}]} :unreachable ["10,64,0"]})
              result (await (ending-of eng job {:chest chest :items {"bread" 3}} 8))]
          (is (= {:kind :nothing-possible :cause :unreachable :data {:gave-up true :short {"bread" 3}}} result))
          (is (= [] (:list (core/state eng)))))))))

(deftest withdraw-gives-up-with-the-transfer-failure
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[forced reason] [[{:status "full" :moved 0} :full] [{:status "ok" :moved 0} :nothing-moved]]]
          (let [{:keys [eng p]} (setup {:containers {"10,64,0" [{:name "bread" :count 5}]}})]
            (.override (.-world p) "transfer" (fn ^:async f [_ _ _] (clj->js forced)))
            (is (= {:kind :nothing-possible :cause reason :data {:gave-up true :short {"bread" 3}}}
                   (await (ending-of eng job {:chest chest :items {"bread" 3}} 8))))))))))

(deftest withdraw-gives-up-on-a-missing-chest
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {})]
          (is (= {:kind :condition-failed :cause :missing :data {:gave-up true :short {"bread" 3}}}
                 (await (ending-of eng job {:chest chest :items {"bread" 3}} 8)))))))))

(deftest withdraw-gives-up-when-the-known-chest-is-gone
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {})]
          (know-place! eng :chest chest)
          (is (= {:kind :condition-failed :cause :missing :data {:gave-up true :short {"bread" 3}}}
                 (await (ending-of eng job {:items {"bread" 3}} 8))))
          (is (some #(= :chest_missing (:kind %)) @seen))
          (is (= [] (:list (core/state eng)))))))))

(deftest withdraw-waits-with-a-reason-when-no-chest-is-known
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen]} (setup {})]
          (core/submit! eng (list job {:items {"bread" 3}}) {})
          (core/tick! eng)
          (is (some #(and (= :waiting (:kind %)) (= :no-chest (:reason %))) @seen)))))))


(deftest withdraw-walks-and-takes-every-name-in-one-call
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:containers {"20,64,0" [{:name "bread" :count 9} {:name "stone_hoe" :count 3}]}})
              result (await (ending-of eng job {:chest {:x 20 :y 64 :z 0} :items (array-map "bread" 4 "stone_hoe" 2)} 1))]
          (is (= {"bread" 4 "stone_hoe" 2} (inv p)))
          (is (= {:kind :done :data {:text carried-text}} result))
          (is (= 2 (count (calls p "transfer"))))
          (is (= 1 (count (tu/walked-to eng)))))))))

(deftest withdraw-short-of-what-the-chest-holds-is-stopped
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:containers {"10,64,0" [{:name "bread" :count 5}]}})
              result (await (ending-of eng job {:chest chest :items (array-map "bread" 16)} 1))]
          (is (= {:kind :condition-failed :cause :chest-lacks :data {:gave-up false :held {} :short {"bread" 11}
                                                                :text "took bread 5; no more bread in the chests (the last holds nothing)"}} result)))))))

(deftest withdraw-counts-what-moved-before-a-transfer-timed-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:inventory [] :containers {"10,64,0" [{:name "bread" :count 32}]}})]
          (.override (.-world p) "transfer"
                     (fn ^:async f [token args impl]
                       (await (impl token args))
                       #js {:status "timeout" :inventoryChange #js {"bread" 16}}))
          (let [result (await (ending-of eng job {:chest chest :items {"bread" 16}} 8))]
            (is (= {:kind :done :data {:text carried-text}} result))
            (is (= {"bread" 16} (inv p)))
            (is (= 1 (count (calls p "transfer"))))))))))

(deftest withdraw-looks-in-the-next-chest-of-the-group
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"10,64,0" "chest" "11,64,0" "chest"}
                                      :containers {"10,64,0" [{:name "dirt" :count 3}] "11,64,0" [{:name "bread" :count 9}]}})]
          (know-place! eng :chest chest)
          (mem/write! (:store eng) :storage/chests {:pos {:x 11 :y 64 :z 0}} {:cap 16 :ttl :forever})
          (let [result (await (ending-of eng job {:items {"bread" 5}} 12))]
            (is (= {"bread" 5} (inv p)))
            (is (= {:kind :done :data {:text carried-text}} result))))))))

(deftest withdraw-reports-the-carried-counts-and-lists-the-need-as-items
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng]} (setup {:inventory [{:name "bread" :count 4}] :containers {"10,64,0" [{:name "bread" :count 5}]}})
              result (await (child-outcome eng job {:chest chest :items {"bread" 16}} 8))]
          (is (= :stopped (:status result)))
          (is (= :chest-lacks (:reason result)))
          (is (= {:units :items :done 9 :left 7} (select-keys (:report (:ending result)) [:units :done :left]))
              "a partial take reports what it carried, not done"))))))

(deftest withdraw-message-counts-what-earlier-chests-gave
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup {:blocks {"10,64,0" "chest" "11,64,0" "chest"}
                                           :containers {"10,64,0" [{:name "bread" :count 3}] "11,64,0" [{:name "dirt" :count 2}]}})]
          (mem/write! (:store eng) :storage/chests {:pos {:x 11 :y 64 :z 0}} {:cap 16 :ttl :forever})
          (let [result (await (ending-of eng job {:chest chest :items {"bread" 10}} 16))
                text (:text (:data result))]
            (is (= {"bread" 3} (inv p)))
            (is (= :chest-lacks (:cause result)))
            (is (re-find #"took bread 3" text))
            (is (not (re-find #"^no bread in this chest" text)))))))))
