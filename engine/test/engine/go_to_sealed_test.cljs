(ns engine.go-to-sealed-test
  "A body sealed in its own retreat plug (ledger purpose :retreat-plug) with a known hostile near: a walk that may not
  dig out waits with :sealed-hostile-near instead of failing, and walks on once the hostile is gone."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.triggers :as triggers]
            [engine.shelter-support :as st]
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
  {:round (fn ^:async walker [c] (await (child/call-walk* c :walk {:pos [15 64 0] :ways #{:walk :clear-seal}})))})

(def escaper
  "Like parent, with the escape defaults: its walks carry no safe? gate."
  (assoc parent :defaults {:checks {:safe? '(always) :fit? '(always)}}))

(def hopper
  "Like parent, but each round walks in a fresh child slot, the way a stair or tunnel parent issues a go-to per step."
  (let [n (atom 0)]
    {:round (fn ^:async hop [c] (await (child/call-walk* c (keyword (str "step" (swap! n inc))) {:pos [15 64 0] :ways #{:walk :clear-seal}})))}))

(defn sealed-holds
  "The ticks the walker waited :sealed-hostile-near."
  [{:keys [waits]}]
  (filterv #{:sealed-hostile-near} @waits))

(defn ^:async tick!
  "Tick s's engine n times, noting the walker's waiting reason after each."
  [{:keys [eng waits id]} n]
  (dotimes [_ n]
    (await (core/tick! eng))
    (swap! waits conj (:reason (core/waiting eng id)))))

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
        eng (core/create {:primitives p :jobs (assoc registry/jobs 'walker parent 'hopper hopper 'escaper escaper) :triggers triggers/all
                          :dir (tu/tmp-dir) :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})
        waits (atom [])
        s {:eng eng :p p :seen seen :clock clock :waits waits}]
    (ledger/write! (:store eng) [{:cell [1 64 0] :item "dirt" :before "air" :job "j9" :purpose :retreat-plug :state :placed}])
    (fake/add-entity! p {:id 9 :name "skeleton" :kind "hostile" :pos {:x -6 :y 64 :z 0} :health 20})
    (when setup (setup eng p))
    (let [s (assoc s :id (core/submit! eng (list job) {}))]
      (await (tick! s n))
      s))))

(deftest a-walk-from-the-sealed-plug-with-a-hostile-near-waits-with-the-reason
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen waits] :as s} (await (sealed 6))]
          (is (= {:x 0 :y 64 :z 0} (select-keys (st/pos-of p) [:x :y :z])))
          (is (= [:sealed-hostile-near] (distinct @waits)))
          (is (empty? (st/emitted seen :unreachable))))))))

(deftest the-hold-ends-when-the-hostile-is-gone-with-the-plug-still-shut
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (await (sealed 6))
              held (count (sealed-holds s))]
          (swap! (fake/state p) update :entities #(vec (remove (comp #{9} :id) %)))
          (await (tick! s 8))
          (is (pos? held))
          (is (= held (count (sealed-holds s))) "no further :sealed-hostile-near wait")
          (is (seq (st/emitted seen :unreachable)) "the walk ends in its own give-up, not an endless wait"))))))

(deftest the-hold-ends-at-its-deadline-with-the-hostile-still-near
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen clock] :as s} (await (sealed 6))]
          (is (empty? (st/emitted seen :unreachable)))
          (swap! clock + (* 10 60 1000))
          (await (tick! s 8))
          (is (seq (st/emitted seen :unreachable)) "after the deadline the walk goes on to its own give-up"))))))

(deftest the-plug-clock-is-gone-once-the-walk-has-ended
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen clock] :as s} (await (sealed 6))]
          (is (seq @gholds/plug-since) "the wait started a clock under the root job")
          (swap! clock + (* 10 60 1000))
          (await (tick! s 8))
          (is (seq (st/emitted seen :unreachable)))
          (is (empty? @gholds/plug-since) "cleanup of the child go-to removes the root's clock"))))))

(deftest a-starving-or-hurt-body-is-not-held
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [self [{:food 2} {:health 4}]]
          (let [{:keys [seen] :as s} (await (sealed 6 self))]
            (is (empty? (sealed-holds s)) (str self))
            (is (seq (st/emitted seen :unreachable)) (str self))))))))

(deftest a-muted-heard-only-hostile-does-not-hold-the-sealed-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[muted? held?] [[true false] [false true]]]
          (let [s (await (sealed 6 nil
                                 (fn [eng _]
                                   (when muted? (mem/write! (:store eng) :heard-mute {:until 9999999999} body/mute-policy)))))]
            (is (= held? (boolean (seq (sealed-holds s)))) (str "muted " muted?))))))))

(deftest a-heard-only-hostile-in-the-far-band-does-not-hold-the-sealed-walk
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[x held?] [[-6 true] [-20 false]]]
          (let [s (await (sealed 6 nil (fn [_ p] (swap! (fake/state p) update :entities (partial mapv #(assoc-in % [:pos 0] x))))))]
            (is (= held? (boolean (seq (sealed-holds s)))) (str "skeleton at x " x))))))))

(deftest the-hold-deadline-counts-across-a-parents-fresh-go-to-children
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen clock] :as s} (await (sealed 6 nil nil 'hopper))]
          (is (empty? (st/emitted seen :unreachable)))
          (doseq [_ (range 4)]
            (swap! clock + (* 60 1000))
            (await (tick! s 3)))
          (is (seq (st/emitted seen :unreachable)) "4 minutes of waits over several children pass the 3 minute limit"))))))

(deftest the-wait-names-the-hostile-it-waits-on
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng id] :as s} (await (sealed 6))
              w (core/waiting eng id)]
          (is (= {:mob "skeleton" :band :near :sensed :heard} (select-keys w [:mob :band :sensed]))))))))

(deftest a-reflex-escape-default-walks-out-of-the-sealed-plug-unheld
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen] :as s} (await (sealed 6 nil nil 'escaper))]
          (is (empty? (sealed-holds s)))
          (is (seq (st/emitted seen :unreachable))))))))
