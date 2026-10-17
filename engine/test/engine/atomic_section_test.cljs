(ns engine.atomic-section-test
  "Interrupt levels and atomic sections: a reflex at or below the level of the running round's open section
  (ctx/atomic) waits for the section to end, up to its level's wait cap; danger reflexes and do-now cut at once."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.triggers :as default-triggers]
            [engine.test-util :as tu]
            [engine.trigger-api :as trigger-api]))

(defn ^:async walk! [c]
  (await (ctx/act c :moveTo #js {:pos #js {:x 5 :y 64 :z 0}})))

(defn ^:async section-then-walk
  "A walk inside a section at the args' :level, then a walk outside it."
  [c]
  (await (ctx/atomic c (:level (:args c)) :tree #(walk! c)))
  (await (walk! c))
  (ctx/end c :done nil))

(defn ^:async nested
  "A comfort tree section holding an urgent pillar section around one walk, then a walk in the tree section only."
  [c]
  (await (ctx/atomic c :comfort :tree
                     (fn ^:async tree []
                       (await (ctx/atomic c :urgent :pillar #(walk! c)))
                       (await (walk! c)))))
  (ctx/end c :done nil))

(defn ^:async two-sections
  "Two comfort sections one after the other, a walk in each."
  [c]
  (await (ctx/atomic c :comfort :first #(walk! c)))
  (await (ctx/atomic c :comfort :second #(walk! c)))
  (ctx/end c :done nil))

(def always (constantly true))

(def jobs
  {'eat {:round (fn ^:async eat [c] (await (ctx/act c :eat #js {})) (ctx/end c :done nil))}
   'section-then-walk {:round section-then-walk}
   'nested {:round nested}
   'two-sections {:round two-sections}})

(defn food [w] (.-food (.self w)))

(def triggers
  {:comfy {:name :comfy :job '(eat) :level :comfort :persistence :retry :when (fn [w _ _] (< (food w) 10))}
   :urgy {:name :urgy :job '(eat) :level :urgent :persistence :retry :when (fn [w _ _] (<= (.-health (.self w)) 8))}
   :scary {:name :scary :job '(eat) :level :danger :persistence :retry
           :when (fn [w _ _] (seq (.entities w #js {:kind "hostile" :radius 8})))}
   :promo {:name :promo :job '(eat) :level :comfort :persistence :retry
           :when (fn [w _ _] (when (< (food w) 10) (if (<= (food w) 2) :urgent true)))}
   :untagged {:name :untagged :job '(eat) :when (constantly false)}
   :plain {:name :plain :job '(eat) :persistence :retry :when (fn [w _ _] (< (food w) 10))}})

(defn setup []
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake {})
        eng (core/create {:primitives p :jobs jobs :triggers triggers :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :clock clock :seen seen}))

(defn settle! [] (js/Promise. (fn [resolve] (js/setTimeout resolve 0))))

(defn kinds [seen] (->> @seen (map :kind) (filter #{:fired :cut :deferred :section_overran})))

(defn self! [p k v] (swap! (.. (.-world p) -state) assoc-in [:self k] v))

(defn ^:async start!
  "Register reflex id, submit job spec, hold the first two walks and start its round; [release-1 release-2 round]."
  [{:keys [eng p]} id spec]
  (core/register-reflex! eng {:trigger id})
  (core/submit! eng spec {})
  (let [r1 (.hold (.-world p) "moveTo")
        r2 (.hold (.-world p) "moveTo")
        round (core/tick! eng)]
    (await (settle!))
    [r1 r2 round]))

(deftest a-comfort-reflex-waits-for-a-comfort-section-then-cuts-once-it-ends
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup)
              [r1 _ round] (await (start! s :comfy '(section-then-walk {:level :comfort})))]
          (self! p :food 5)
          (is (nil? (core/tick! eng)) "held off: the section is open")
          (is (= [:deferred] (kinds seen)))
          (r1)
          (await (settle!))
          (is (some? (core/tick! eng)) "the section ended: it fires")
          (await round)
          (is (= [:deferred :fired :cut] (kinds seen))))))))

(deftest a-comfort-reflex-past-its-cap-cuts-the-section-with-a-warn
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen clock] :as s} (setup)
              [_ _ round] (await (start! s :comfy '(section-then-walk {:level :comfort})))]
          (self! p :food 5)
          (core/tick! eng)
          (swap! clock + 89000)
          (is (nil? (core/tick! eng)) "within the 90 s comfort cap")
          (swap! clock + 2000)
          (is (some? (core/tick! eng)) "past the cap: it cuts")
          (await round)
          (is (= [:deferred :section_overran :fired :cut] (kinds seen))))))))

(deftest an-urgent-reflex-waits-at-most-its-cap-in-an-urgent-section
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p clock] :as s} (setup)
              [_ _ round] (await (start! s :urgy '(section-then-walk {:level :urgent})))]
          (self! p :health 6)
          (is (nil? (core/tick! eng)))
          (swap! clock + 14000)
          (is (nil? (core/tick! eng)) "within the 15 s urgent cap")
          (swap! clock + 2000)
          (is (some? (core/tick! eng)) "past the cap")
          (await round))))))

(deftest an-urgent-reflex-cuts-a-comfort-section-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup)
              [_ _ round] (await (start! s :urgy '(section-then-walk {:level :comfort})))]
          (self! p :health 6)
          (is (some? (core/tick! eng)))
          (await round)
          (is (= [:fired :cut] (kinds seen))))))))

(deftest a-danger-reflex-cuts-an-urgent-section-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup)
              [_ _ round] (await (start! s :scary '(section-then-walk {:level :urgent})))]
          (swap! (.. (.-world p) -state) assoc :entities [{:id 1 :name "zombie" :kind "hostile" :pos [3 64 0] :health 20}])
          (is (some? (core/tick! eng)))
          (await round)
          (is (= [:fired :cut] (kinds seen))))))))

(deftest a-trigger-that-reports-urgent-mid-wait-cuts-a-comfort-section
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup)
              [_ _ round] (await (start! s :promo '(section-then-walk {:level :comfort})))]
          (self! p :food 5)
          (is (nil? (core/tick! eng)) "food 5: comfort, held off")
          (self! p :food 2)
          (is (some? (core/tick! eng)) "food 2: urgent, above the section")
          (await round)
          (is (= [:deferred :fired :cut] (kinds seen))))))))

(deftest an-entrys-level-is-a-floor-a-lower-reported-level-does-not-demote-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup)]
          (core/register-reflex! eng {:trigger :promo :level :danger})
          (core/submit! eng '(section-then-walk {:level :urgent}) {})
          (.hold (.-world p) "moveTo")
          (.hold (.-world p) "moveTo")
          (let [round (core/tick! eng)]
            (await (settle!))
            (self! p :food 2)
            (is (some? (core/tick! eng)) "reports :urgent but the entry is :danger: cuts the urgent section at once")
            (await round)
            (is (= [:fired :cut] (kinds seen)))))))))

(deftest a-trigger-with-no-level-is-danger-and-cuts-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup)
              [_ _ round] (await (start! s :plain '(section-then-walk {:level :urgent})))]
          (self! p :food 5)
          (is (some? (core/tick! eng)))
          (await round)
          (is (= [:fired :cut] (kinds seen))))))))

(deftest do-now-cuts-a-section-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as s} (setup)
              [_ _ round] (await (start! s :untagged '(section-then-walk {:level :urgent})))]
          (core/do-now! eng '(eat))
          (await round)
          (is (= [:cut] (kinds seen))))))))

(deftest an-inner-urgent-section-holds-urgent-reflexes-until-it-closes
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup)
              [r1 _ round] (await (start! s :urgy '(nested)))]
          (self! p :health 6)
          (is (nil? (core/tick! eng)) "the pillar section holds it")
          (r1)
          (await (settle!))
          (is (some? (core/tick! eng)) "only the comfort tree section is open: it cuts")
          (await round)
          (is (= [:deferred :fired :cut] (kinds seen))))))))

(deftest a-section-opened-while-a-reflex-waits-is-not-opened
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen] :as s} (setup)
              [r1 _ round] (await (start! s :comfy '(two-sections)))]
          (self! p :food 5)
          (is (nil? (core/tick! eng)))
          (r1)
          (await (settle!))
          (is (some? (core/tick! eng)) "the second section did not open: it fires in the gap")
          (await round)
          (is (= [:deferred :fired :cut] (kinds seen))))))))

(deftest an-entry-may-override-its-triggers-level
  (let [{:keys [eng]} (setup)]
    (is (nil? (:level (core/entry-from eng {:trigger :untagged}))) "the trigger's level is read at each tick")
    (is (= :comfort (:level (core/entry-from eng {:trigger :untagged :level :comfort}))))
    (is (nil? (trigger-api/entry-problem (:jobs eng) (:triggers eng) {:trigger :untagged :level :urgent})))
    (is (= :bad-level (:reason (trigger-api/entry-problem (:jobs eng) (:triggers eng) {:trigger :untagged :level :soon}))))))

(deftest the-default-triggers-declare-their-levels
  (is (= {:suffocating :danger :burning :danger :wedged :danger :hostile-near :danger :died :danger
          :night :urgent :dusk-home :urgent :poisoned :urgent
          :hungry :comfort :door-left :comfort :inventory-nearly-full :comfort :scaffold-left :comfort :needs-bed :comfort :needs-weapon :comfort
          :tidy-pending :comfort :pen-gate :comfort :mounted :comfort :loose-items :comfort}
         (update-vals default-triggers/all :level))))
