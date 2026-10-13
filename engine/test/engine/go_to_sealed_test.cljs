(ns engine.go-to-sealed-test
  "A body sealed in its own retreat plug (ledger purpose :retreat-plug) with a known hostile near: a walk that may not
  dig out waits with :sealed-hostile-near instead of failing, and walks on once the hostile is gone."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.triggers :as triggers]
            [engine.shelter-test :as st]
            [engine.test-util :as tu]
            [engine.memory :as mem]
            [jobs.lib.body :as body]
            [jobs.lib.child :as child]
            [jobs.lib.ledger :as ledger]
            [jobs.movement.go-to :as go-to]
            [jobs.movement.go-to.holds :as gholds]))

(def pit
  (merge (into {} (for [[dx dz] [[1 0] [-1 0] [0 1] [0 -1]] y [64 65]] [(str dx "," y "," dz) "dirt"]))
         {"0,66,0" "dirt"}))

(def parent
  {:check (constantly true)
   :round (fn ^:async walker [c] (await (child/call-walk c :walk {:pos [15 64 0] :escalate false})))})

(def hopper
  "Like parent, but each round walks in a fresh child slot, the way a stair or tunnel parent issues a go-to per step."
  (let [n (atom 0)]
    {:check (constantly true)
     :round (fn ^:async hop [c] (await (child/call-walk c (keyword (str "step" (swap! n inc))) {:pos [15 64 0] :escalate false})))}))

(defn sealed-holds
  "The :sealed-hostile-near holds; a walk with the way open may also hold :searching, as long as its search takes."
  [seen]
  (filterv #(= :sealed-hostile-near (:reason %)) (st/emitted seen :holding)))

(defn ^:async sealed
  "A noon world with the body in a 1-cell pit whose east side is a retreat plug, a skeleton outside; ticks n."
  ([n] (sealed n nil))
  ([n self] (sealed n self nil))
  ([n self setup] (sealed n self setup 'walker))
  ([n self setup job]
  (reset! gholds/plug-since {})
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:floor tu/walk-floor :time st/noon :self (merge {:pos {:x 0 :y 64 :z 0}} self) :blocks pit}))
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'walker parent 'hopper hopper) :triggers triggers/all
                          :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
        waits (atom [])
        s {:eng eng :p p :seen seen :clock clock :waits waits}]
    ((.-override (.-world p)) "wait" (fn [_ a impl]
                             (let [act @(:activity eng)]
                               (swap! waits conj (:reason (or (:hold act) (:wait-hold act)))))
                             (impl nil a)))
    (ledger/write! (:store eng) [{:cell [1 64 0] :item "dirt" :before "air" :job "j9" :purpose :retreat-plug :state :placed}])
    (fake/add-entity! p {:id 9 :name "skeleton" :kind "hostile" :pos {:x -6 :y 64 :z 0} :health 20})
    (when setup (setup eng p))
    (core/submit! eng (list job) {})
    (await (st/tick-n eng n))
    s)))

(deftest a-walk-from-the-sealed-plug-with-a-hostile-near-holds-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (sealed 6))]
          (is (= {:x 0 :y 64 :z 0} (select-keys (st/pos-of p) [:x :y :z])))
          (is (= [:sealed-hostile-near] (distinct (map :reason (st/emitted seen :holding)))))
          (is (empty? (st/emitted seen :unreachable))))))))

(deftest the-hold-ends-when-the-hostile-is-gone-with-the-plug-still-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (await (sealed 6))
              held (count (sealed-holds seen))]
          (swap! (fake/state p) update :entities #(vec (remove (comp #{9} :id) %)))
          (await (st/tick-n eng 8))
          (is (pos? held))
          (is (= held (count (sealed-holds seen))) "no further :sealed-hostile-near hold")
          (is (seq (st/emitted seen :unreachable)) "the walk ends in its own give-up, not an endless wait"))))))

(deftest the-hold-ends-at-its-deadline-with-the-hostile-still-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (await (sealed 6))]
          (is (empty? (st/emitted seen :unreachable)))
          (swap! clock + (* 10 60 1000))
          (await (st/tick-n eng 8))
          (is (seq (st/emitted seen :unreachable)) "after the deadline the walk goes on to its own give-up"))))))

(deftest a-starving-or-hurt-body-is-not-held
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [self [{:food 2} {:health 4}]]
          (let [{:keys [seen]} (await (sealed 6 self))]
            (is (empty? (sealed-holds seen)) (str self))
            (is (seq (st/emitted seen :unreachable)) (str self))))))))

(deftest a-muted-heard-only-hostile-does-not-hold-the-sealed-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[muted? held?] [[true false] [false true]]]
          (let [{:keys [seen]} (await (sealed 6 nil
                                              (fn [eng _]
                                                (when muted? (mem/write! (:store eng) :heard-mute {:until 9999999999} body/mute-policy)))))]
            (is (= held? (boolean (seq (sealed-holds seen)))) (str "muted " muted?))))))))

(deftest every-wait-of-the-held-walk-names-its-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [waits]} (await (sealed 6))]
          (is (seq @waits))
          (is (= [:sealed-hostile-near] (distinct @waits))))))))

(deftest a-heard-only-hostile-in-the-far-band-does-not-hold-the-sealed-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[x held?] [[-6 true] [-20 false]]]
          (let [{:keys [seen]} (await (sealed 6 nil (fn [_ p] (swap! (fake/state p) update :entities (partial mapv #(assoc-in % [:pos 0] x))))))]
            (is (= held? (boolean (seq (sealed-holds seen)))) (str "skeleton at x " x))))))))

(deftest the-hold-deadline-counts-across-a-parents-fresh-go-to-children
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen clock]} (await (sealed 6 nil nil 'hopper))]
          (is (empty? (st/emitted seen :unreachable)))
          (doseq [_ (range 4)]
            (swap! clock + (* 60 1000))
            (await (st/tick-n eng 3)))
          (is (seq (st/emitted seen :unreachable)) "4 minutes of holds over several children pass the 3 minute limit"))))))

(deftest the-hold-event-names-the-hostile-it-waits-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen]} (await (sealed 6))]
          (is (= [{:mob "skeleton" :band :near :sensed :heard}]
                 (map #(select-keys % [:mob :band :sensed]) (st/emitted seen :go-to.sealed-hostile-near)))))))))
