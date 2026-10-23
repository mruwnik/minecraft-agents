(ns engine.dig-pocket-test
  "jobs.survival.dig-pocket: its choice of wall (a solid face with rock all round, never flat ground, fluid or sand) and the job:
  dig the pocket, wall the way in, refuse what is another's."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.registry :as registry]
            [engine.settings :as settings]
            [engine.test-util :as tu]
            [jobs.lib.world-files :as ew]
            [plan.shape :as shape]
            [jobs.survival.dig-pocket :as pocket]
            [jobs.survival.shut-in :as shut-in]
            [jobs.survival.pit-cells :as pit]))

(defn blocks [name [x0 x1] [y0 y1] [z0 z1]]
  (into {} (for [x (range x0 (inc x1)) y (range y0 (inc y1)) z (range z0 (inc z1))] [(str x "," y "," z) name])))

(def ground (blocks "dirt" [-2 8] [60 63] [-3 3]))
(def feet {:x 0 :y 64 :z 0})
(def iron [{:name "iron_pickaxe" :count 1} {:name "cobblestone" :count 4}])

(defn ctx-of [p] {:primitives p :view (constantly {:data {} :now 0}) :root 1 :slots [] :args {}})

(defn world [extra inventory]
  (ctx-of (tu/seeing-all (tu/fake {:blocks (merge ground extra) :inventory inventory}))))

(defn stand
  ([extra] (stand extra iron))
  ([extra inventory] (:stand (pocket/site (assoc (world extra inventory) :args {:ways #{:carried}})))))

(deftest a-hill-with-rock-all-round-gives-a-stand-cell-facing-it
  (is (= {:x 3 :y 64 :z 0} (stand (blocks "stone" [4 8] [64 66] [-3 3])))))

(deftest flat-ground-has-no-stand-cell
  (is (nil? (stand {}))))

(deftest a-face-too-thin-or-low-for-the-shell-has-no-stand-cell
  (is (nil? (stand (blocks "stone" [4 8] [64 65] [-3 3]))) "no ceiling over the pocket")
  (is (nil? (stand (blocks "stone" [4 8] [64 66] [0 0]))) "no side walls"))

(deftest a-face-ten-blocks-off-is-found-once-the-walk-radius-reaches-it
  (let [far (blocks "stone" [13 17] [64 66] [-3 3])
        c (world (merge (blocks "dirt" [9 17] [60 63] [-3 3]) far) iron)]
    (is (nil? (:stand (pocket/site c))))
    (is (= {:x 12 :y 64 :z 0} (settings/with-settings {:jobs.survival.dig-pocket/walk-radius 16} #(:stand (pocket/site c)))))))

(deftest a-face-above-or-below-the-feet-is-found
  (is (= {:x 3 :y 66 :z 0} (stand (merge (blocks "dirt" [-2 8] [64 65] [-3 3]) (blocks "stone" [4 8] [66 68] [-3 3]))))))

(defn reads
  "The cells (f c) reads of c's world."
  [c f]
  (let [p (:primitives c)
        at (.-sensedAt p)
        n (atom 0)]
    (aset p "sensedAt" (fn [q] (swap! n inc) (at q)))
    (f c)
    (aset p "sensedAt" at)
    @n))

(deftest the-pocket-cover-in-the-open-tries-each-standing-cell-once
  (let [c (world {} [{:name "dirt" :count 4}])
        one-pass (reads c (fn [c] (doseq [cell (cons feet (pocket/stand-cells c feet))] (pit/pocket-site c cell true))))]
    (is (< (reads c #(shut-in/covers % [:pocket])) (* 1.5 one-pass))
        "one search of the stand cells serves both kinds of wall and the quote and the seconds in the open")))

(defn fits? [extra]
  (boolean (pit/pocket-cells (world (merge (blocks "stone" [4 8] [64 66] [-3 3]) extra) iron) {:x 3 :y 64 :z 0} [1 0])))

(deftest sand-gravel-or-water-in-or-round-the-pocket-rules-it-out
  (is (true? (fits? {})))
  (is (false? (fits? (blocks "sand" [4 8] [64 66] [-3 3]))) "sand falls")
  (is (false? (fits? {"4,66,0" "gravel"})) "gravel over the pocket falls into it")
  (is (false? (fits? {"4,64,1" "water"})) "water beside it")
  (is (false? (fits? {"5,64,0" "water"})) "water behind it"))

(deftest a-body-standing-in-water-digs-a-wall-with-water-on-the-way-in
  (is (true? (fits? {"3,64,0" "water" "3,65,0" "water"}))))

(deftest stone-needs-a-pickaxe-the-body-carries
  (let [cobble [{:name "cobblestone" :count 4}]]
    (is (nil? (stand (blocks "stone" [4 8] [64 66] [-3 3]) cobble)))
    (is (some? (stand (blocks "dirt" [4 8] [64 66] [-3 3]) cobble)))))

;; ------------------------------------------------------------------ the job: dig, wall in, refusals

(def hill (blocks "stone" [4 8] [64 66] [-3 3]))

(defn setup
  ([zones] (setup zones {}))
  ([zones spec]
   (let [clock (atom 1000000)
         [seen sink] (tu/legacy-capture-sink)
         p ((:seeing spec tu/seeing-all) (tu/fake (merge {:offlineScale 0.0001 :time 14000 :blocks (merge ground hill)
                                           :drops {"stone" "cobblestone"}
                                           :inventory [{:name "iron_pickaxe" :count 1} {:name "cobblestone" :count 4}]}
                                          (dissoc spec :plans :seeing))))
         eng (core/create {:primitives p :jobs registry/jobs :triggers {} :dir (tu/tmp-dir) :now #(deref clock)
                           :world (ew/of-data (:plans spec {}) {} zones)
                           :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
     {:eng eng :p p :seen seen :clock clock})))

(defn ^:async run-job! [{:keys [eng]} args]
  (core/submit! eng (list 'jobs.survival.dig-pocket args) {})
  (loop [i 0]
    (when (and (< i 300) (seq (:list (core/state eng))))
      (await (core/tick! eng))
      (recur (inc i)))))

(defn block-at [p [x y z]] (.-name (.blockAt p #js {:x x :y y :z z})))
(defn failed [seen] (some #(when (= :stopped (:kind %)) %) @seen))
(defn completed [seen] (some #(when (= :completed (:kind %)) %) @seen))

(def hill-zone {:name "keep" :owner "Miles" :min [3 60 -3] :max [9 70 3]})

(deftest the-pocket-is-dug-and-its-way-in-walled-from-inside
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [])]
          (await (run-job! s {}))
          (is (nil? (failed seen)))
          (is (= {"cobblestone" 2} (:spent (completed seen))) "the way in only: the body stands in the far pair, rock beside it")
          (is (= ["air" "air"] (mapv #(block-at p %) [[5 64 0] [5 65 0]])) "far pair dug and left open for the body")
          (is (= ["cobblestone" "cobblestone"] (mapv #(block-at p %) [[4 64 0] [4 65 0]])) "near pair walled with the dropped blocks"))))))

(deftest no-block-to-wall-with-is-no-site
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen] :as s} (setup [] {:drops {}})]
          (await (run-job! s {:use {:blocks ["dirt"]}}))
          (is (= :no-site (get-in (failed seen) [:reason]))))))))

(deftest a-face-in-another-zone-is-left-and-a-free-face-used
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [hill-zone]
                                            {:blocks (merge ground hill (blocks "dirt" [-10 -3] [60 63] [-3 3]) (blocks "stone" [-8 -4] [64 66] [-3 3]))})]
          (await (run-job! s {}))
          (is (nil? (failed seen)))
          (is (= "stone" (block-at p [4 64 0])) "the owned wall is untouched")
          (is (= "air" (block-at p [-5 64 0])) "the pocket is cut in the free hill"))))))

(deftest with-trespass-false-no-pocket-is-cut-in-another-zone-though-no-free-face-exists
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [hill-zone])]
          (await (run-job! s {:trespass? false}))
          (is (= :no-site (get-in (failed seen) [:reason])))
          (is (= "stone" (block-at p [4 64 0]))))))))

(deftest a-pocket-is-not-cut-in-the-bodys-own-plan
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (shape/with-author {:id "hut" :parts [{:id "w" :want "stone" :cells (vec (for [x (range 4 9) y [64 65 66] z (range -3 4)] [x y z]))}]} "Fake")
              {:keys [p seen] :as s} (setup [] {:plans {"hut" plan}})]
          (await (run-job! s {}))
          (is (= :no-site (get-in (failed seen) [:reason])))
          (is (= "stone" (block-at p [4 64 0]))))))))

;; ------------------------------------------------------------------ failure reasons

(deftest a-walk-that-never-gets-there-fails-walk-failed
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen] :as s} (setup [] {:blocks (merge ground (blocks "dirt" [9 14] [60 63] [-3 3]) (blocks "stone" [4 8] [64 66] [-3 3]))})]
          (.override (.-world (:p s)) "steer"
                     (fn [_ _ _] (js/Promise.resolve #js {:status "timeout" :pose #js {}})))
          (await (run-job! s {}))
          (is (= :walk-failed (get-in (failed seen) [:reason]))))))))

(deftest a-dig-the-world-refuses-fails-saying-so
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen] :as s} (setup [] {})]
          (.override (.-world (:p s)) "dig" (fn ^:async f [_ _ _] #js {:status "failed"}))
          (await (run-job! s {}))
          (is (re-find #"cannot dig the pocket" (:text (failed seen)))))))))

(deftest a-wall-the-world-refuses-fails-without-completing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {})]
          (.override (.-world p) "place" (fn ^:async f [_ _ _] #js {:status "failed"}))
          (await (run-job! s {}))
          (is (some? (failed seen)))
          (is (nil? (completed seen))))))))

(deftest stone-without-a-pickaxe-and-no-fetch-waits-no-tool
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {:inventory [{:name "cobblestone" :count 4}]})]
          (await (run-job! s {}))
          (is (= :no-tool (some #(when (= :waiting (:kind %)) (:reason %)) @seen)) "can-start? holds the call")
          (is (= "stone" (block-at p [4 64 0]))))))))

(defn ^:async run-wall! [s args]
  (core/submit! (:eng s) (list 'jobs.survival.wall-in args) {})
  (loop [i 0]
    (when (and (< i 30) (seq (:list (core/state (:eng s)))))
      (await (core/tick! (:eng s)))
      (recur (inc i)))))

(deftest the-night-walls-keep-out-of-the-bodys-own-plan-cells
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [plan (shape/with-author {:id "field" :parts [{:id "wheat" :want "wheat" :cells [[1 64 0] [0 64 1] [-1 64 0] [0 64 -1]]}]} "Fake")
              {:keys [p seen] :as s} (setup [] {:plans {"field" plan} :inventory [{:name "dirt" :count 8}]})]
          (await (run-wall! s {:for :night}))
          (is (= "air" (block-at p [1 64 0])) "no dirt in a planned crop cell")
          (is (= :own-plan (get-in (failed seen) [:reason]))))))))

(deftest wall-in-holds-for-want-of-a-block-of-the-calls-use-even-with-others-carried
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {:inventory [{:name "cobblestone" :count 8}]})]
          (await (run-wall! s {:use {:blocks ["glass"]}}))
          (is (= :no-blocks (some #(when (= :waiting (:kind %)) (:reason %)) @seen)) "can-start? reads the call's :use")
          (is (= "air" (block-at p [1 64 0]))))))))

(deftest wall-in-starts-with-a-block-only-the-calls-use-names
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {:inventory [{:name "oak_log" :count 12}]})]
          (await (run-wall! s {:use {:blocks ["oak_log"]}}))
          (is (nil? (some #(when (= :waiting (:kind %)) %) @seen)))
          (is (= "oak_log" (block-at p [1 64 0]))))))))

(deftest stone-without-a-pickaxe-tries-a-fetch-then-cuts-the-pocket-by-hand
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {:inventory [{:name "cobblestone" :count 4}]})]
          (await (run-job! s {:ways #{:carried :chest :craft :gather}}))
          (is (some #(and (= :child_started (:kind %)) (= :need (:slot %))) @seen) "a tool is asked for")
          (is (nil? (failed seen)))
          (is (= ["cobblestone" "cobblestone"] (mapv #(block-at p %) [[4 64 0] [4 65 0]]))))))))

(deftest a-hill-not-yet-seen-is-looked-for-before-no-site-then-dug
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen] :as s} (setup [] {:seeing tu/seeing-after-look})]
          (await (run-job! s {}))
          (is (nil? (failed seen)))
          (is (some? (completed seen)))
          (is (= "air" (block-at p [5 64 0]))))))))

(deftest flat-ground-without-a-pickaxe-is-still-no-site
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [seen] :as s} (setup [] {:inventory [] :blocks ground})]
          (await (run-job! s {}))
          (is (= :no-site (get-in (failed seen) [:reason]))))))))

(deftest a-waiting-fetch-child-makes-the-pocket-yield-not-fail
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng seen] :as s} (setup [] {:inventory []})
              orig ctx/call
              n (atom 0)]
          (core/submit! eng (list 'jobs.survival.dig-pocket {:ways #{:carried :chest :craft :gather}}) {})
          (set! ctx/call (fn [c slot job args]
                           (if (= :need slot)
                             (do (swap! n inc) (js/Promise.resolve :continue))
                             (orig c slot job args))))
          (try (await (core/tick! eng))
               (finally (set! ctx/call orig)))
          (is (= 1 @n) "one fetch round per scheduler round, no busy loop")
          (is (nil? (failed seen)))
          (is (= 1 (count (:list (core/state eng)))) "the job yielded and is still there"))))))

(deftest the-quote-prices-the-digs-and-the-seal-not-a-flat-guess
  (let [hill (blocks "stone" [1 8] [64 66] [-3 3])
        seconds #(get-in (pocket/quote-of (world hill %)) [:uses :job-time 1])
        with-pick (seconds iron)
        by-hand (seconds [{:name "cobblestone" :count 4}])]
    (is (< 3 with-pick 10) "two stone cells with a pickaxe and one placement")
    (is (> by-hand with-pick) "the hand digs stone slower")))
