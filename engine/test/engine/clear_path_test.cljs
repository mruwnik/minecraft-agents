(ns engine.clear-path-test
  "jobs.access.clear-path against the fake world: how a dig child's wait or a refilled cell ends the door."
  (:require [cljs.test :refer [deftest is async]]
            [engine.contract :as contract]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.registry :as registry]
            [engine.test-util :as tu :refer [box]]
            [engine.triggers :as triggers]
            [jobs.lib.cost :as cost]
            [jobs.lib.tools :as tools]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]))

(def job 'jobs.access.clear-path)

(def wall
  "A stone wall at x 1..2 (y 64..65, z -1..1) on a stone floor, the body at the origin facing east."
  {:self {:pos {:x 0 :y 64 :z 0}}
   :blocks (merge (box -2 63 -2 8 63 2 "stone") (box 1 64 -1 2 65 1 "stone"))})

(defn ^:async run-door
  "Run clear-path over world (stub: a replacement round for jobs.blocks.dig, or nil) as a child, :east
  unless args says otherwise; its result."
  ([world stub] (run-door world stub {:heading :east}))
  ([world stub args] (run-door world stub args nil))
  ([world stub args rounds] (run-door world stub args rounds tu/outcome))
  ([world stub args rounds f]
   (let [clock (atom 1000000)
         [_ sink] (tu/legacy-capture-sink)
         p (cond-> (tu/fake (dissoc world :plans :sight :p-out)) (:sight world) (tu/seeing-within (:sight world)))
         _ (some-> (:p-out world) (reset! p))
         out (atom :not-done)
         parent (tu/recording-parent job args out f)
         jobs (cond-> (assoc registry/jobs 'recording-parent parent)
                stub (assoc 'jobs.blocks.dig (assoc (get registry/jobs 'jobs.blocks.dig) :round stub))
                rounds (as-> js (reduce-kv (fn [m k round] (assoc m k (assoc (get m k) :round round))) js rounds)))
         eng (core/create {:primitives p :jobs jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                           :world (ew/of-data (:plans world {}) {} [])
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
     (core/submit! eng '(recording-parent) {})
     (loop [i 0]
       (when (and (< i 60) (seq (:list (core/state eng))))
         (swap! clock + 700)
         (await (core/tick! eng))
         (recur (inc i))))
     @out)))

(deftest a-dig-child-that-waits-ends-the-door-dig-waits
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-door wall nil {:heading :east :ways #{:carried} :need-drop true}))]
          (is (= [:stopped :dig-waits] [(:status r) (:reason r)]) (pr-str r))
          (is (= [1 65 0] (:cell r)) "the top layer first"))))))

(deftest with-need-drop-unset-and-no-fetch-allowed-the-door-is-dug-by-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-door wall nil {:heading :east :ways #{:carried}}))]
          (is (= :done (:status r)) (pr-str r)))))))

(deftest the-dig-child-gets-the-call-ways
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen (atom [])
              stub (fn ^:async spy-dig [c]
                     (swap! seen conj (:ways (:args c)))
                     (ctx/end c :nothing-possible :failed {:dug false}))
              w (assoc wall :inventory [{:name "wooden_pickaxe" :count 1}])
              _ (await (run-door w stub))
              _ (await (run-door w stub {:heading :east :ways #{:carried}}))]
          (is (= [#{:carried :chest :craft :gather} #{:carried}] @seen)))))))

(deftest by-hand-digs-the-door-with-no-pickaxe
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-door wall nil {:heading :east :need-drop false}))]
          (is (= [:done [3 64 0]] [(:status r) (:through r)]) (pr-str r))
          (is (= 4 (count (:dug r))) "two blocks high, two thick, bare-handed"))))))

(deftest a-cell-that-is-dug-but-never-clears-ends-refills
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [digs (atom 0)
              stub (fn ^:async refilling-dig [c]
                     (swap! digs inc)
                     (ctx/end c :done nil {:dug true :reason :dug}))
              r (await (run-door (assoc wall :inventory [{:name "wooden_pickaxe" :count 1}]) stub))]
          (is (= [:stopped :refills] [(:status r) (:reason r)]) (pr-str r))
          (is (= 3 @digs) "a stone cell is dug max-cell-digs times"))))))

(deftest a-dig-child-that-yields-continue-counts-as-one-dig-of-the-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [calls (atom 0)
              stub (fn ^:async slow-dig [c]
                     (if (zero? (mod (swap! calls inc) 4))
                       (ctx/end c :done nil {:dug true :reason :dug})
                       :continue))
              r (await (run-door (assoc wall :inventory [{:name "wooden_pickaxe" :count 1}]) stub))]
          (is (= [:refills 3] [(:reason r) (count (:dug r))]) "a cell is dug max-cell-digs times, not max-cell-digs calls"))))))

(deftest a-dig-that-fails-ends-dig-failed-without-copying-its-ending
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [stub (fn ^:async failing-dig [c]
                     (ctx/end c :nothing-possible :failed {:dug false :primitive "timeout"}))
              r (await (run-door (assoc wall :inventory [{:name "wooden_pickaxe" :count 1}]) stub))]
          (is (= [:stopped :dig-failed] [(:status r) (:reason r)]) (pr-str r))
          (is (nil? (:dig r)) "the dig child's ending is the because, not copied"))))))

(deftest a-wall-with-a-floor-beyond-is-dug-and-walked-through
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [r (await (run-door (assoc wall :inventory [{:name "wooden_pickaxe" :count 1}]) nil))]
          (is (= [:done [3 64 0]] [(:status r) (:through r)]) (pr-str r))
          (is (= 4 (count (:dug r))) "two blocks high, two thick")
          (is (= [3 64 0] (:at r))))))))

(deftest a-wall-thicker-than-the-body-sees-is-dug-on-as-each-row-shows-the-next
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [thick (assoc wall :sight 3 :inventory [{:name "wooden_pickaxe" :count 1}]
                           :blocks (merge (box -2 63 -2 8 63 2 "stone") (box 1 64 -1 4 65 1 "stone")))
              r (await (run-door thick nil))]
          (is (= [:done [5 64 0]] [(:status r) (:through r)]) (pr-str r))
          (is (= 8 (count (:dug r))) "four thick, more than it could see"))))))

(deftest digging-on-past-what-it-saw-stops-when-the-next-plan-costs-over-max-dig-s
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [thick (assoc wall :sight 3 :inventory [{:name "wooden_pickaxe" :count 1}]
                           :blocks (merge (box -2 63 -2 40 63 2 "stone") (box 1 64 -1 30 65 1 "stone")))
              cell-s (cost/dig-cells-s [(tools/clear-ms (tu/fake (dissoc thick :sight)) "stone")])
              r (await (run-door thick nil {:heading :east :max-dig-s (* 10 cell-s)}))]
          (is (= [:stopped :too-dear] [(:status r) (:reason r)]) (pr-str r))
          (is (= 6 (count (:dug r))) "three rows priced 8 cells (the far side it could not see as rock); the next plan, 14, is over")
          (is (= [0 64 0] (:at r)) "it walked back to where it began"))))))

(deftest a-wall-seen-thick-is-priced-by-its-rows-not-refused-by-their-count
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [seen-thick (assoc wall :inventory [{:name "wooden_pickaxe" :count 1}]
                                :blocks (merge (box -2 63 -2 8 63 2 "stone") (box 1 64 -1 5 65 1 "stone")))
              cell-s (cost/dig-cells-s [(tools/clear-ms (tu/fake seen-thick) "stone")])
              through (await (run-door seen-thick nil {:heading :east :max-dig-s (* 10.5 cell-s)}))
              dear (await (run-door seen-thick nil {:heading :east :max-dig-s (* 9.5 cell-s)}))]
          (is (= [:done [6 64 0] 10] [(:status through) (:through through) (count (:dug through))]) (pr-str through))
          (is (= [:stopped :too-dear 0] [(:status dear) (:reason dear) (count (:dug dear))]) (pr-str dear)))))))

(deftest a-walk-back-that-fails-still-ends-the-door-where-the-body-stands
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [thick (assoc wall :sight 3 :inventory [{:name "wooden_pickaxe" :count 1}]
                           :blocks (merge (box -2 63 -2 40 63 2 "stone") (box 1 64 -1 30 65 1 "stone")))
              cell-s (cost/dig-cells-s [(tools/clear-ms (tu/fake (dissoc thick :sight)) "stone")])
              walk (get-in registry/jobs ['jobs.movement.go-to :round])
              no-way-back (fn ^:async no-way-back-walk [c] (if (= [0 64 0] (:pos (:args c)))
                                            (ctx/end c :nothing-possible {:reason :unreachable} {:arrived false})
                                            (await (walk c))))
              r (await (run-door thick nil {:heading :east :max-dig-s (* 10 cell-s)} {'jobs.movement.go-to no-way-back}))]
          (is (= [:stopped :too-dear [3 64 0]] [(:status r) (:reason r) (:at r)]) (pr-str r)))))))

(deftest the-step-through-is-a-go-to-child
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [asked (atom nil)
              go-to (fn ^:async refusing-go-to [c]
                      (reset! asked (:args c))
                      (ctx/end c :nothing-possible :unreachable {:arrived false}))
              r (await (run-door (assoc wall :inventory [{:name "wooden_pickaxe" :count 1}]) nil
                                 {:heading :east} {'jobs.movement.go-to go-to} identity))]
          (is (= [:nothing-possible :step-failed] [(:kind r) (contract/cause-key r)]) (pr-str r))
          (is (= [[3 64 0] 0 #{:walk :clear-seal}] ((juxt :pos :range :ways) @asked)) (pr-str @asked)))))))

(deftest a-missing-heading-is-impossible
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [e (await (run-door wall nil {} nil identity))]
          (is (= [:impossible :bad-args] [(:kind e) (contract/cause-key e)]) (pr-str e)))))))

(deftest the-job-exports-the-contract-and-quotes-validly
  (let [def (get registry/jobs job)
        q ((:quote-of def) {:primitives (tu/fake wall) :args {:heading :east}})]
    (is (= [:heading] (:target def)))
    (is (some? (:can-start? def)))
    (is (nil? (contract/quote-problem q)) (pr-str q))))

(deftest a-door-ends-with-a-kind-and-a-cause
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [pick {:inventory [{:name "wooden_pickaxe" :count 1}]}
              end (fn ^:async end* [world stub args]
                    (let [e (await (run-door world stub args nil identity))]
                      [(:kind e) (contract/cause-key e) (:kind (:because e))]))
              refilling (fn ^:async refilling [c] (ctx/end c :done nil {:dug true :reason :dug}))
              failing (fn ^:async failing [c] (ctx/end c :nothing-possible :failed {:dug false :primitive "timeout"}))
              east {:heading :east}]
          (is (= [:done :done nil] (await (end (merge wall pick) nil east))))
          (is (= [:condition-failed :dig-waits :condition-failed] (await (end wall nil (assoc east :need-drop true)))) "the dig child's wait is its because")
          (is (= [:nothing-possible :refills nil] (await (end (merge wall pick) refilling east))))
          (is (= [:nothing-possible :dig-failed :nothing-possible] (await (end (merge wall pick) failing east))))
          (is (= [:condition-failed :no-door nil] (await (end (dissoc wall :blocks) nil east))))
          (is (= [:condition-failed :too-dear nil] (await (end (merge wall pick) nil (assoc east :max-dig-s 1)))) "a wall dearer than it may spend")
          (is (= [:condition-failed :footprint nil]
                 (await (end (assoc (merge wall pick) :plans {"hut" (shape/with-author {:id "hut" :parts [{:id "w" :cells [[1 64 0]] :want "stone"}]} "Fake")})
                             nil east)))))))))

(deftest a-wall-of-the-bodys-own-plan-is-not-dug-unless-told-to
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (shape/with-author {:id "hut" :parts [{:id "w" :cells [[1 64 0] [1 65 0] [2 64 0] [2 65 0]] :want "stone"}]} "Fake")
              world (assoc wall :plans {"hut" plan})
              r (await (run-door world nil {:heading :east :need-drop false}))
              r2 (await (run-door world nil {:heading :east :settings {:jobs.lib.access/spare-own-builds false} :need-drop false}))]
          (is (= [:stopped :footprint "hut"] [(:status r) (:reason r) (:plan r)]) (pr-str r))
          (is (= :done (:status r2)) (pr-str r2)))))))

(defn done-of
  "The kind and reason of clear-path's done? for a body at pos over world with the call's memory mem."
  [world pos mem]
  (let [c {:primitives (tu/fake (assoc-in world [:self :pos] pos)) :root "j1" :slots [] :args {:heading :east}
           :view (fn [] {:now 0 :data {:entries {:job/j1 [{:t 0 :data mem}]}}})}]
    (tu/kind-reason ((:done? (get registry/jobs job)) c))))

(def plan-mem {:cells [[1 64 0] [1 65 0] [2 64 0] [2 65 0]] :through [3 64 0]})

(deftest done-reads-the-calls-own-plan-against-the-world
  (let [open (dissoc wall :blocks)
        floor {:blocks (box -2 63 -2 8 63 2 "stone")}
        dug (merge wall floor {:blocks (merge (box -2 63 -2 8 63 2 "stone") (box 2 64 -1 2 65 1 "stone"))})]
    (is (= [:later :digging] (done-of wall {:x 0 :y 64 :z 0} {})) "no plan yet")
    (is (= [:later :digging] (done-of wall {:x 0 :y 64 :z 0} plan-mem)) "planned cells still solid")
    (is (= [:later :digging] (done-of dug {:x 3 :y 64 :z 0} plan-mem)) "one cell still solid")
    (is (= [:later :digging] (done-of (merge open floor) {:x 0 :y 64 :z 0} plan-mem)) "open but not yet through")
    (is (= [:ok :done] (done-of (merge open floor) {:x 3 :y 64 :z 0} plan-mem)) "open and stood through")))

(deftest close-behind-shuts-the-mouth-from-inside-before-the-far-side-opens
  ;; go-to's break-out of its own pen: an animal must not follow the body into the door and out through it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (atom nil)
              thick (update wall :blocks merge (box 3 64 -1 3 65 1 "stone"))
              w (assoc thick :inventory [{:name "stone_pickaxe" :count 1}] :drops {"stone" "cobblestone"} :p-out p)
              r (await (run-door w nil {:heading :east :close-behind true}))
              block (fn [cell] (get-in @(fake/state @p) [:blocks cell] "air"))
              acts (keep (fn [a] (when (#{"dig" "place"} (.-name a)) [(.-name a) (let [{:keys [x y z]} (js->clj (.-pos (.-args a)) :keywordize-keys true)] [x y z])]))
                         (.-calls (.-world @p)))
              at (fn [act] (.indexOf (vec acts) act))]
          (is (= [:done [4 64 0] [[1 64 0] [1 65 0]]] [(:status r) (:through r) (:shut r)]) (pr-str (dissoc r :ending)))
          (is (= ["cobblestone" "cobblestone" "air" "air" "air" "air"] (mapv block [[1 64 0] [1 65 0] [2 64 0] [2 65 0] [3 64 0] [3 65 0]]))
              "the mouth is shut, the rest of the door left to go-to's put-back")
          (is (< (at ["place" [1 65 0]]) (at ["dig" [3 64 0]])) (str "shut before the far side opens: " (pr-str acts))))))))

(deftest a-door-shut-behind-that-turns-too-dear-opens-its-mouth-walks-back-and-ends-too-dear
  ;; the walk back first digs the shut mouth open again (a dead end): the ending is the price's, not :no-door's
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [p (atom nil)
              thick (assoc wall :sight 3 :inventory [{:name "wooden_pickaxe" :count 1}] :drops {"stone" "cobblestone"} :p-out p
                           :blocks (merge (box -2 63 -2 40 63 2 "stone") (box 1 64 -1 30 65 1 "stone")))
              cell-s (cost/dig-cells-s [(tools/clear-ms (tu/fake (dissoc thick :sight :p-out)) "stone")])
              r (await (run-door thick nil {:heading :east :close-behind true :max-dig-s (* 10 cell-s)} nil identity))
              block (fn [cell] (get-in @(fake/state @p) [:blocks cell] "air"))
              placed (keep #(when (= "place" (.-name %)) (let [{:keys [x y z]} (js->clj (.-pos (.-args %)) :keywordize-keys true)] [x y z]))
                           (.-calls (.-world @p)))]
          (is (= [:condition-failed :too-dear] [(:kind r) (contract/cause-key r)]) (pr-str r))
          (is (some #{[1 64 0]} placed) (str "the mouth was shut: " (pr-str placed)))
          (is (= [0 64 0] (:at (:data r))) "it walked back to where it began")
          (is (= ["air" "air"] (mapv block [[1 64 0] [1 65 0]])) "the mouth it shut is open again"))))))

(deftest a-door-one-column-deep-is-walked-out-of-with-close-behind
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [thin (update wall :blocks #(apply dissoc % (for [y [64 65] z [-1 0 1]] (str "2," y "," z))))
              r (await (run-door (assoc thin :inventory [{:name "stone_pickaxe" :count 1}]) nil {:heading :east :close-behind true}))]
          (is (= [:done [2 64 0] nil] [(:status r) (:through r) (:shut r)]) (pr-str (dissoc r :ending))))))))
