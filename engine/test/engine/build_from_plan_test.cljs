(ns engine.build-from-plan-test
  "jobs.build.from-plan placing what a plan wants against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.events :as events]
            [engine.harvest-test :as h]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.world :as world]
            [jobs.build.from-plan :as build]))

(def job 'jobs.build.from-plan)

(defn start
  "An engine over the fake world spec with the plans {id plan} as its world data."
  [spec plans]
  (let [[seen sink] (tu/legacy-capture-sink)
        p (tu/fake spec)
        w (world/of-data plans {})
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref h/clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref h/clock)})
                          :world w})]
    {:eng eng :p p :seen seen :w w}))

(defn pen-plan
  "A 3x3 fence ring at y 64 over x 2..4, z 2..4, a gate at 3 64 2 and a torch on the post at 2 64 2."
  [status]
  {:id "pen" :status status
   :parts [{:id "fence" :outline [[2 64 2] [4 64 4]] :want "oak_fence"}
           {:id "gate" :cells [[3 64 2]] :want {:block "oak_fence_gate" :facing :north}}
           {:id "light" :cells [[2 65 2]] :want "torch"}]})

(def kit [{:name "oak_fence" :count 16} {:name "oak_fence_gate" :count 1} {:name "torch" :count 4}])

(def ring [[2 64 2] [4 64 2] [2 64 3] [4 64 3] [2 64 4] [3 64 4] [4 64 4]])

(defn places [p] (mapv #(let [a (.-args %)] [[(.-x (.-pos a)) (.-y (.-pos a)) (.-z (.-pos a))] (.-item a)]) (h/calls p "place")))

(defn block [p [x y z]] (h/block-at p x y z))

(deftest an-empty-site-is-built-post-before-torch
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start {:inventory kit} {"pen" (pen-plan :active)})
              result (await (h/child-outcome eng job {:plan "pen"} 200))
              order (mapv first (places p))]
          (is (every? #(= "oak_fence" (block p %)) ring))
          (is (= "oak_fence_gate" (block p [3 64 2])))
          (is (= "torch" (block p [2 65 2])))
          (is (< (.indexOf order [2 64 2]) (.indexOf order [2 65 2])))
          (is (= {:placed 9 :missing [] :short {} :given-up {} :wrong []} result))
          (is (= 1 (count (h/events-of seen :build.done))))
          (is (empty? (h/events-of seen :build.short))))))))

(deftest a-gate-with-a-facing-is-placed-from-the-side-it-faces-away-from
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:inventory kit} {"pen" (pen-plan :active)})
              body-z (atom nil)]
          (.override (.-world p) "place"
                     (fn ^:async f [token args impl]
                       (when (= "oak_fence_gate" (.-item args)) (reset! body-z (.-z (.-pos (.self p)))))
                       (await (impl token args))))
          (await (h/child-outcome eng job {:plan "pen"} 200))
          (is (> @body-z 2.5)))))))

(deftest a-short-material-builds-what-it-can-and-says-what-is-missing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start {:inventory [{:name "oak_fence" :count 4} {:name "oak_fence_gate" :count 1}
                                                       {:name "torch" :count 1}]}
                                          {"pen" (pen-plan :active)})
              result (await (h/child-outcome eng job {:plan "pen"} 200))]
          (is (= 4 (count (filter #(= "oak_fence" (block p %)) ring))))
          (is (= {"oak_fence" 3} (:short result)))
          (is (= 3 (count (filter (set ring) (:missing result)))))
          (is (= [{"oak_fence" 3}] (mapv :short (h/events-of seen :build.short)))))))))

(deftest a-removed-block-is-put-back-and-nothing-else-is-touched
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [built (into {"3,64,2" "oak_fence_gate" "2,65,2" "torch"}
                          (map (fn [[x y z]] [(h/cell-key x y z) "oak_fence"]) (remove #{[4 64 3]} ring)))
              {:keys [eng p]} (start {:blocks built :inventory kit} {"pen" (pen-plan :active)})
              result (await (h/child-outcome eng job {:plan "pen"} 200))]
          (is (= [[[4 64 3] "oak_fence"]] (places p)))
          (is (= 1 (:placed result))))))))

(deftest a-wrong-block-is-reported-not-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (start {:blocks {"4,64,3" "stone"} :inventory kit} {"pen" (pen-plan :active)})
              result (await (h/child-outcome eng job {:plan "pen"} 200))]
          (is (empty? (h/calls p "dig")))
          (is (= "stone" (block p [4 64 3])))
          (is (= [{:pos [4 64 3] :found "stone" :want "oak_fence"}] (:wrong result))))))))

(deftest a-cell-refused-three-times-is-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (start {:inventory kit} {"pen" (pen-plan :active)})]
          (.override (.-world p) "place"
                     (fn ^:async f [token args impl]
                       (if (= [4 3] [(.-x (.-pos args)) (.-z (.-pos args))])
                         #js {:status "no-support"}
                         (await (impl token args)))))
          (let [result (await (h/child-outcome eng job {:plan "pen"} 200))]
            (is (= 3 (count (filter #(= [4 64 3] (first %)) (places p)))))
            (is (= {[4 64 3] :refused} (:given-up result)))
            (is (= 8 (:placed result)))
            (is (= 1 (count (h/events-of seen :build.gave-up))))))))))

(deftest a-cell-it-cannot-walk-to-is-given-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :status :active :parts [{:id "post" :cells [[40 64 0]] :want "oak_fence"}]}
              {:keys [eng]} (start {:inventory kit :unreachable (map #(apply h/cell-key %) (build/stand-cells [40 64 0] 64 nil #{}))} {"far" plan})
              result (await (h/child-outcome eng job {:plan "far"} 200))]
          (is (= {[40 64 0] :unreachable} (:given-up result)))
          (is (= 0 (:placed result))))))))

(deftest an-unloaded-cell-is-walked-to-not-taken-as-built
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan {:id "far" :status :active :parts [{:id "post" :cells [[40 64 0]] :want "oak_fence"}]}
              {:keys [eng p]} (start {:inventory kit :unloaded ["40,64,0"]} {"far" plan})
              result (await (h/child-outcome eng job {:plan "far"} 200))]
          (is (seq (h/calls p "moveTo")))
          (is (= {[40 64 0] :unloaded} (:given-up result)))
          (is (= [[40 64 0]] (:missing result))))))))

(defn declines
  "Submit the job with args over the world spec and plans, tick a few times: [places, build.declined warns]."
  [spec plans args]
  (let [{:keys [eng p seen]} (start spec plans)]
    (core/submit! eng (list job args) {})
    (dotimes [_ 4] (swap! h/clock + 700) (core/tick! eng))
    [(count (h/calls p "place")) (mapv #(select-keys % [:plan :reason]) (h/events-of seen :build.declined))]))

(deftest the-check-declines-a-plan-it-cannot-build-with-one-warn
  (let [kit-spec {:inventory kit}]
    (is (= [0 [{:plan "nope" :reason "no such plan"}]] (declines kit-spec {} {:plan "nope"})))
    (is (= [0 [{:plan "pen" :reason "the plan is :retired"}]] (declines kit-spec {"pen" (pen-plan :retired)} {:plan "pen"})))
    (is (= [0 [{:plan "pen" :reason "the plan is :proposed"}]] (declines kit-spec {"pen" (pen-plan :proposed)} {:plan "pen"})))
    (is (= [0 [{:plan "pen" :reason "no cells to build"}]] (declines kit-spec {"pen" (pen-plan :active)} {:plan "pen" :part "nope"})))
    (is (= [0 [{:plan "pen" :reason "nothing carried to build with: oak_fence 7, oak_fence_gate 1, torch 1"}]]
           (declines {:inventory []} {"pen" (pen-plan :active)} {:plan "pen"})))))

(deftest a-broken-plan-declines-naming-the-error
  (let [{:keys [eng p seen w]} (start {:inventory kit} {})]
    (reset! (:state w) (assoc @(:state w) :plans {"pen" {:error "unreadable EDN: eof"}}))
    (core/submit! eng (list job {:plan "pen"}) {})
    (dotimes [_ 3] (swap! h/clock + 700) (core/tick! eng))
    (is (empty? (h/calls p "place")))
    (is (= [{:plan "pen" :reason "the plan cannot be read: unreadable EDN: eof"}]
           (mapv #(select-keys % [:plan :reason]) (h/events-of seen :build.declined))))))

(deftest item-for-picks-the-block-to-place
  (are [want carried item] (= item (build/item-for want carried))
    "oak_fence" #{} "oak_fence"
    {:block "oak_fence_gate" :facing :north} #{} "oak_fence_gate"
    [:any "oak_fence" "spruce_fence"] #{"spruce_fence"} "spruce_fence"
    [:any "oak_fence" {:block "spruce_fence"}] #{} "oak_fence"
    {:crop "wheat"} #{"wheat_seeds"} nil
    :clear #{} nil))

(deftest facing-ok-wants-the-body-on-the-far-side
  (are [facing body ok] (= ok (build/facing-ok? facing [3 64 2] body))
    nil {:x 0.5 :y 64 :z 0.5} true
    "north" {:x 3.5 :y 64 :z 4.5} true
    "north" {:x 3.5 :y 64 :z 0.5} false
    "south" {:x 3.5 :y 64 :z 0.5} true
    "east" {:x 0.5 :y 64 :z 2.5} true
    "east" {:x 3.5 :y 64 :z 4.5} false))

(deftest a-corner-can-be-placed-from-inside-the-ring
  (let [planned (set (map first (map vector (conj ring [3 64 2]))))]
    (is (some #{[3 64 3]} (build/stand-cells [2 64 2] 64 nil planned)))))
