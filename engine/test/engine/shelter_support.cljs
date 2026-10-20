(ns engine.shelter-support
  "Shared helpers of the night and shelter test namespaces."
  (:require [engine.core :as core]
            [engine.settings :as settings]
            [engine.events :as events]
            [engine.fake :as fake]
            [engine.memory :as mem]
            [engine.registry :as registry]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [jobs.lib.shelter :as sh]
            [jobs.lib.shelter.sense :as sh-sense]))
(def day-ms 1200000)
(def night 14000)
(def noon 1000)

;; the night is one round until morning: a test ends it by setting the time at a hold
(defn on-wait!
  "Call (f n) on the nth hold of the night (a wait act of hold-ms, from 1) before it runs."
  [p f]
  (let [n (atom 0)]
    (.override (.-world p) "wait" (fn ^:async g [token a impl]
                                    (when (= 5000 (.-ms a)) (f (swap! n inc)))
                                    (await (impl token a))))))

(defn dawn-after!
  "Day comes at the kth hold; (f n) runs at every hold before it."
  ([p k] (dawn-after! p k (fn [_])))
  ([p k f] (on-wait! p (fn [n] (if (>= n k) (.setTime (.-world p) noon) (f n))))))

(defn mid-night!
  "Day comes at hold n+1; at hold n, (f) is kept in the atom it returns: a look at the night while it holds."
  [p n f]
  (let [a (atom nil)]
    (dawn-after! p (inc n) (fn [i] (when (= i n) (reset! a (f)))))
    a))

(def default-dawn
  "Holds after which setup's night turns to day, so a test that does not care when the night ends still ends."
  12)

(defn walls-cheap
  "Run f (a test body) with the night walls priced low enough that the quote picks them over a pit: these tests are about the walls."
  [f]
  (settings/with-settings {:jobs.survival.plug-way/night-place-s 0.1} f))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake (merge {:offlineScale 0.0001 :floor tu/walk-floor} world))
        _ (dawn-after! p default-dawn)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir)
                          :now #(deref clock)
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen :clock clock}))

(defn ^:async tick-n [eng n]
  (loop [i 0]
    (when (< i n)
      (await (core/tick! eng))
      (recur (inc i)))))

;; a body that is safe is not held: the night ends, and a test brings the day itself
(defn ^:async night-then-day
  "Tick eng n times of night, keep (f) from then, set the day and tick m more times; the kept value."
  [eng p n f m]
  (await (tick-n eng n))
  (let [kept (f)]
    (.setTime (.-world p) noon)
    (await (tick-n eng m))
    kept))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))

(defn arg-pos [call] (js->clj (.-pos (.-args call)) :keywordize-keys true))

(defn entries [eng kind] (mapv :data (mem/entries (mem/view (:store eng)) kind)))

(defn know-bed! [eng pos] (mem/write! (:store eng) :bed {:pos pos} mem/place-policy))

(defn looked-round!
  "The night's look pass from p's cell, as the body remembers it: a trigger check after it is not held by the look."
  [eng p]
  (mem/write! (:store eng) :night-looked {:cell (sh-sense/feet p)} sh/night-looked-policy))

(defn pos-of [p] (js->clj (.-pos (.self p)) :keywordize-keys true))

(defn teleport! [p x y z] (fake/swap-self! p assoc :pos [x y z]))

(defn emitted [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn no-way-stops
  "The :no-way endings of shut-in seen so far, each with its :text: a root call's :stopped event, a night's child_ended."
  [seen]
  (vec (concat (for [e (emitted seen :stopped) :when (= :no-way (:reason e))] (assoc e :text (or (:text e) (:message e))))
               (for [e (emitted seen :child_ended) :when (= :no-way (get-in e [:ending :cause]))]
                 (assoc e :text (get-in e [:ending :data :text]))))))

(defn ended-because
  "The ending events (a job's own, or a child's child_ended) whose cause is cause."
  [seen cause]
  (filterv #(or (= cause (:reason %)) (= cause (get-in % [:ending :cause]))) @seen))

(defn ending-text [e] (or (:text e) (get-in e [:ending :data :text])))

(defn child-ended-because
  "The child_ended events of slot whose ending cause is cause."
  [seen slot cause]
  (filterv #(and (= :child_ended (:kind %)) (= slot (:slot %)) (= cause (get-in % [:ending :cause]))) @seen))

(defn emitted-by [seen source reflex] (filterv #(= [source reflex] [(:source %) (:reflex %)]) @seen))

(def floor {"0,63,0" "dirt" "0,62,0" "stone" "0,61,0" "stone" "0,60,0" "stone"})
(def bed-room
  "A closed stone room (inside x -1..7, y 64..65, z -1..1, roof at 66) around the body's start and a bed at 6,64,0."
  (assoc (apply dissoc (tu/box -2 64 -2 8 66 2 "stone") (keys (tu/box -1 64 -1 7 65 1 "stone"))) "6,64,0" "red_bed"))

(def dirt-stack [{:name "dirt" :count 12}])
(def wall-stack "Blocks enough to wall the body in on flat ground (18 cells)." [{:name "dirt" :count 20}])
(def sleeper {:id 5 :name "Alex" :kind "player" :sleeping true :pos {:x 10 :y 64 :z 0}})
(def awake {:id 6 :name "Sam" :kind "player" :pos {:x 10 :y 64 :z 0}})

(defn declined-events [seen reflex]
  (filterv #(= [:reflex :declined reflex] [(:source %) (:kind %) (:reflex %)]) @seen))

(def always-shelter
  {:name :always-shelter :job '(jobs.survival.night) :args {:roof-height 4}
   :persistence :cooldown :cooldown-s 10 :when (constantly true)})

(defn refuse-placing! [p]
  (.override (.-world p) "place" (fn ^:async f [_ _ _] #js {:status "no-support"})))

(defn after-first!
  "Run (f) once, right after the first act named act-name returns."
  [p act-name f]
  (let [done? (atom false)]
    (.override (.-world p) act-name (fn ^:async g [token a impl]
                                      (let [r (await (impl token a))]
                                        (when-not @done? (reset! done? true) (f))
                                        r)))))


(defn fires? [world args]
  (boolean ((:when (get triggers/all :night)) (tu/fake-on-floor world) {} args)))

(def ground
  "Dirt (no tool needed) from y 60 to 63 over x and z -4..4: room for a pit and a stair out of it."
  (into {} (for [x (range -4 5) z (range -4 5) y (range 60 64)] [(str x "," y "," z) "dirt"])))

(def stone-ground (into {} (map (fn [[k _]] [k "stone"])) ground))

(def hard-ground
  "Cobblestone where ground is dirt: nothing to dig without a pickaxe, so a held night has no pit or pocket to try."
  (into {} (map (fn [[k _]] [k "cobblestone"])) ground))

(def ms-of-offline (fn [p] (mapv #(.-ms (.-args %)) (calls p "offline"))))

(defn notified [seen] (count (emitted seen :job.notify)))

(defn refuse-cell!
  "Make place at cell (a [x y z] vector) fail the way the server's \"still air\" refusal does; others work as usual."
  [p [x y z]]
  (.override (.-world p) "place"
             (fn ^:async f [token a impl]
               (let [pos (.-pos a)]
                 (if (and (= x (.-x pos)) (= y (.-y pos)) (= z (.-z pos)))
                   #js {:status "failed" :reason "Server refused to place dirt: the block is still air"}
                   (await (impl token a)))))))
