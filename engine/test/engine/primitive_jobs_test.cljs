(ns engine.primitive-jobs-test
  "The primitive table (jobs.primitive) and the act check: before an act runs its most is checked against what is left
  of every frame of the chain; enforced, a crossing act is refused and ends the outermost crossed frame :limit-reached,
  in shadow mode job.limit_would_end is told and the act runs."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.primitive :as primitive]))

(def world (atom {}))
(def clock (atom 0))

(defn setup [jobs]
  (reset! world {})
  (reset! clock 1000000)
  (let [[seen sink] (tu/capture-sink)
        p (tu/fake {})
        eng (core/create {:primitives p :dir (tu/tmp-dir) :now #(deref clock) :jobs jobs :triggers {}
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :seen seen :p p}))

(defn job-ev [seen kind] (filterv #(= [:job kind] [(:source %) (:kind %)]) @seen))
(defn stopped [seen] (select-keys (:data (first (job-ev seen :stopped))) [:kind :cause]))
(defn calls [p method] (count (filter #(= method (.-name %)) (.-calls (.-world p)))))
(defn enforced [f] (settings/with-settings {:engine.contract/enforce-limits true} f))

(defn quote-of [k p args] ((get-in primitive/table [k :quote-of]) p args))

(deftest every-primitive-has-a-target-a-quote-and-an-ending
  (let [p (tu/fake {})]
    (is (= 29 (count primitive/table)))
    (doseq [[k {:keys [target quote-of ending]}] primitive/table]
      (is (vector? target) k)
      (is (nil? (contract/quote-problem (quote-of p {}))) k)
      (is (every? #(< (nth % 2) ##Inf) (keep #(get-in (quote-of p {}) [:uses %]) [:time :health :food]))
          (str k ": an act cannot be stopped midway, so its most is finite"))
      (is (contract/ending? (ending #js {:status "failed"})) k))))

(deftest a-drop-is-quoted-as-the-fall-straight-to-the-goal
  (let [p (tu/fake {})]
    (fake/swap-self! p assoc :pos [0 70 0])
    (is (= [0 0 3] (get-in (quote-of :moveTo p {:pos {:x 0 :y 64 :z 0}}) [:uses :health])))
    (is (= [0 0 0] (get-in (quote-of :moveTo p {:pos {:x 0 :y 68 :z 9}}) [:uses :health])) "3 blocks hurt nothing")
    (is (= [0 1 20] (get-in (quote-of :moveTo p {:pos {:x 0 :y 64 :z 0} :timeoutS 20}) [:uses :time])))))

(deftest endings-read-the-primitives-status
  (let [end (get-in primitive/table [:dig :ending])]
    (is (= {:kind :done :cause :dug} (end #js {:status "dug"})))
    (is (= {:kind :limit-reached :cause {:line :time}} (end #js {:status "timeout"})))
    (is (= {:kind :nothing-possible :cause :failed} (end #js {:status "failed"})))
    (is (= {:kind :condition-failed :cause :unreachable} (end #js {:status "unreachable"})))))

(defn drops-to [y]
  {:conditions [] :defaults {:floor {:health 15}}
   :round (fn ^:async drop-round [c]
            (swap! world assoc :moved (.-status (await (ctx/act c :moveTo #js {:pos #js {:x 0 :y y :z 0}}))))
            :continue)})

(defn high-up [p]
  (fake/swap-self! p assoc :pos [0 70 0] :health 17))

(deftest enforced-a-drop-past-the-floor-is-refused-before-it-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (enforced
                (fn ^:async enforced-drop []
                  (let [{:keys [eng seen p]} (setup {'down (drops-to 64)})]
                    (high-up p)
                    (core/submit! eng '(down) {})
                    (await (core/tick! eng))
                    (is (zero? (calls p "moveTo")) "the act did not run")
                    (is (= {:kind :limit-reached :cause {:line :health :floor 15 :act :moveTo}} (stopped seen)))))))))))

(deftest enforced-a-drop-within-the-floor-runs
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (enforced
                (fn ^:async enforced-small-drop []
                  (let [{:keys [eng seen p]} (setup {'down (drops-to 66)})]
                    (high-up p)
                    (core/submit! eng '(down) {})
                    (await (core/tick! eng))
                    (is (= 1 (calls p "moveTo")))
                    (is (empty? (job-ev seen :stopped)))))))))))

(deftest shadow-mode-tells-the-refusal-and-runs-the-act
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen p]} (setup {'down (drops-to 64)})]
          (high-up p)
          (core/submit! eng '(down) {})
          (await (core/tick! eng))
          (is (= 1 (calls p "moveTo")))
          (is (= [{:cause {:line :health :floor 15 :act :moveTo} :most 3 :left 2}]
                 (mapv #(select-keys (:data %) [:cause :most :left]) (job-ev seen :limit_would_end))))
          (await (core/tick! eng))
          (is (= 1 (count (job-ev seen :limit_would_end))) "once per frame, line and act"))))))

(defn places-in-a-section [n]
  {:conditions [] :defaults {:spend {:items {"cobblestone" 1}}}
   :round (fn ^:async place-round [c]
            (await (ctx/atomic c :comfort :wall
                               (fn ^:async wall []
                                 (dotimes [i n]
                                   (await (ctx/act c :place #js {:pos #js {:x i :y 64 :z 2} :item "cobblestone"}))))))
            :continue)})

(deftest enforced-the-refusal-is-not-held-by-an-atomic-section
  (async done
    (tu/run-async done
      (fn ^:async t []
        (await (enforced
                (fn ^:async enforced-section []
                  (let [{:keys [eng seen p]} (setup {'wall (places-in-a-section 2)})]
                    (fake/swap-self! p assoc :pos [0 64 0])
                    (swap! (fake/state p) assoc :inventory [{:name "cobblestone" :count 5 :slot 36}])
                    (core/submit! eng '(wall) {})
                    (await (core/tick! eng))
                    (is (= 1 (calls p "place")) "the second place would use a second cobblestone")
                    (is (= {:kind :limit-reached :cause {:line :items :item "cobblestone" :spend 1 :act :place}}
                           (stopped seen)))))))))))
