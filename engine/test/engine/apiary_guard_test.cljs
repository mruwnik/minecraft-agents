(ns engine.apiary-guard-test
  "jobs.apiary.guard against the fake world."
  (:require [cljs.test :refer [deftest is are async]]
            [engine.registry :as registry]
            [engine.core :as core]
            [engine.ctx :as ctx]
            [engine.events :as events]
            [engine.jobs.apiary :as apiary]
            [engine.test-util :as tu]
            [engine.triggers :as triggers]
            [engine.world :as ew]
            [jobs.apiary.guard :as guard]))

(defn setup [world]
  (let [clock (atom 1000000)
        [seen sink] (tu/legacy-capture-sink)
        p (tu/fake world)
        eng (core/create {:primitives p :jobs registry/jobs :triggers triggers/all :dir (tu/tmp-dir) :now #(deref clock)
                          :world (ew/of-data {} {} (:zones world []))
                          :events (events/make {:body "Fake" :sinks [sink] :now #(deref clock)})})]
    {:eng eng :p p :seen seen}))

(defn ^:async run-until-empty [eng n]
  (loop [i 0]
    (if (or (>= i n) (empty? (:list (core/state eng))))
      i
      (do (await (core/tick! eng))
          (recur (inc i))))))

(defn ^:async child-outcome [eng job args n]
  (let [out (atom :not-done)
        parent {:check (constantly true)
                :round (fn ^:async recording-round [c]
                         (let [r (await (ctx/call-child c :kid job args))]
                           (when (= :done r) (reset! out (ctx/child-result c :kid)))
                           r))}
        eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent parent))]
    (core/submit! eng '(recording-parent) {})
    (await (run-until-empty eng n))
    @out))

(defn calls [p name] (filterv #(= name (.-name %)) (.-calls (.-world p))))
(defn kinds [seen kind] (filterv #(= kind (:kind %)) @seen))
(defn inv [& pairs] (mapv (fn [[n c]] {:name n :count c}) (partition 2 pairs)))
(defn block-name [p x y z] (.-name (.blockAt p #js {:x x :y y :z z})))
(defn lit? [p x y z] (true? (some-> (.blockAt p #js {:x x :y y :z z}) .-properties .-lit)))

(def job 'jobs.apiary.guard)

(defn around
  "The four horizontal neighbours of x,y,z as stone cells."
  [x y z]
  (into {} (map (fn [[dx dz]] [(str (+ x dx) "," y "," (+ z dz)) "stone"]) [[1 0] [-1 0] [0 1] [0 -1]])))

(defn fire-world
  "A lit campfire at x,y,z walled on four sides, on ground walled on four sides: the standard open fire."
  [{:keys [inventory x y z] :or {x 2 y 64 z 0}}]
  (let [k (str x "," y "," z)]
    {:inventory inventory
     :self {:pos {:x 0 :y 64 :z 0}}
     :blocks (merge (around x y z) (around x (dec y) z) {k "campfire" (str x "," (dec y) "," z) "stone"})
     :states {k {:lit true}}}))

(defn open-side [w k] (update w :blocks dissoc k))

(defn check-of [p args]
  ((:check (get registry/jobs job))
   {:primitives p :args (merge {:radius 16 :max 12 :box nil :center nil} args) :view (fn [] {:data {} :now 0}) :engine {:world (ew/of-data {} {} [])}}))

(deftest an-open-fire-gets-a-carpet
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p seen]} (setup (fire-world {:inventory (inv "white_carpet" 2)}))
              result (await (child-outcome eng job {} 40))]
          (is (= 1 (:carpeted result)))
          (is (= 0 (:sunk result)))
          (is (= :guarded (:reason result)))
          (is (= "white_carpet" (block-name p 2 65 0)))
          (is (not (apiary/open-fire? (apiary/block-at-fn p) {:x 2 :y 64 :z 0})))
          (is (= 1 (count (kinds seen :apiary.guard-done))))
          (is (empty? (kinds seen :apiary.guard-gave-up))))))))

(deftest a-raised-fire-is-sunk-and-carpeted
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (-> (fire-world {:inventory (inv "campfire" 1 "white_carpet" 1)}) (open-side "3,64,0"))
              {:keys [eng p]} (setup w)
              result (await (child-outcome eng job {} 60))]
          (is (= 1 (:sunk result)))
          (is (= 1 (:carpeted result)))
          (is (= :guarded (:reason result)))
          (is (= "campfire" (block-name p 2 63 0)))
          (is (lit? p 2 63 0))
          (is (= "white_carpet" (block-name p 2 64 0))))))))

(deftest a-covered-raised-fire-is-sunk-and-its-carpet-put-back
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (-> (fire-world {:inventory (inv "campfire" 1)}) (open-side "3,64,0") (update :blocks assoc "2,65,0" "red_carpet"))
              {:keys [eng p]} (setup w)
              result (await (child-outcome eng job {} 80))]
          (is (= 1 (:sunk result)))
          (is (= 1 (:carpeted result)))
          (is (= "campfire" (block-name p 2 63 0)))
          (is (= "red_carpet" (block-name p 2 64 0))))))))

(deftest a-raised-fire-without-a-campfire-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (-> (fire-world {:inventory (inv "stick" 1)}) (open-side "3,64,0") (update :blocks assoc "2,65,0" "white_carpet"))
              {:keys [eng p seen]} (setup w)
              result (await (child-outcome eng job {} 40))]
          (is (= :no-campfire (:reason result)))
          (is (= {"2,64,0" :no-campfire} (:left result)))
          (is (empty? (calls p "dig")))
          (is (empty? (calls p "place")))
          (is (= 1 (count (kinds seen :apiary.guard-gave-up)))))))))

(deftest an-open-fire-without-a-carpet-is-left
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (fire-world {:inventory (inv "stick" 1)}))
              result (await (child-outcome eng job {} 40))]
          (is (= :no-carpet (:reason result)))
          (is (= {"2,64,0" :no-carpet} (:left result)))
          (is (empty? (calls p "place"))))))))

(deftest moss-carpet-is-not-a-carpet
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [eng p]} (setup (fire-world {:inventory (inv "moss_carpet" 3)}))
              result (await (child-outcome eng job {} 40))]
          (is (= :no-carpet (:reason result)))
          (is (empty? (calls p "place"))))))))

(deftest a-raised-fire-over-unsound-ground-only-gets-its-carpet
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (-> (fire-world {:inventory (inv "campfire" 1 "white_carpet" 1)}) (open-side "3,64,0") (open-side "3,63,0"))
              {:keys [eng p]} (setup w)
              result (await (child-outcome eng job {} 40))]
          (is (= 0 (:sunk result)))
          (is (= 1 (:carpeted result)))
          (is (empty? (calls p "dig")))
          (is (= "campfire" (block-name p 2 64 0))))))))

(deftest a-safe-fire-is-not-the-jobs-business
  (are [w] (false? (check-of (:p (setup w)) {}))
    (assoc (fire-world {}) :blocks (assoc (:blocks (fire-world {})) "2,65,0" "white_carpet"))
    (-> (fire-world {}) (open-side "3,64,0") (open-side "3,63,0") (update :blocks assoc "2,65,0" "white_carpet"))
    (update (fire-world {}) :states dissoc "2,64,0")
    {}))

(deftest an-open-fire-or-a-started-job-passes-the-check
  (let [p (:p (setup (fire-world {})))]
    (is (true? (check-of p {})))
    (is (false? (check-of p {:box {:from {:x 10 :y 60 :z -5} :to {:x 14 :y 70 :z 5}}})))))

(deftest a-job-with-nothing-to-guard-does-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc-in (fire-world {:inventory (inv "white_carpet" 1)}) [:blocks "2,65,0"] "white_carpet")
              {:keys [eng p]} (setup w)
              _ (core/submit! eng (list job) {})
              _ (await (run-until-empty eng 5))]
          (is (empty? (calls p "moveTo")))
          (is (empty? (calls p "place")))
          (is (empty? (calls p "dig"))))))))

(deftest a-fire-the-body-cannot-walk-to-is-unreachable
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc (fire-world {:inventory (inv "white_carpet" 1)}) :self {:pos {:x 20 :y 64 :z 0}} :unreachable ["2,64,0"])
              {:keys [eng p]} (setup w)
              result (await (child-outcome eng job {:radius 30} 40))]
          (is (= :unreachable (:reason result)))
          (is (= {"2,64,0" :unreachable} (:skipped result)))
          (is (empty? (calls p "place"))))))))

(deftest the-count-is-bounded-by-max
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (-> (fire-world {:inventory (inv "white_carpet" 2)})
                    (update :blocks merge (:blocks (fire-world {:x 2 :z 5})))
                    (update :states merge (:states (fire-world {:x 2 :z 5}))))
              {:keys [eng p]} (setup w)
              result (await (child-outcome eng job {:max 1} 60))]
          (is (= :limit (:reason result)))
          (is (= 1 (:carpeted result)))
          (is (= 1 (count (calls p "place")))))))))

(deftest a-box-keeps-the-job-to-its-fires
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (-> (fire-world {:inventory (inv "white_carpet" 2)})
                    (update :blocks merge (:blocks (fire-world {:x 2 :z 5})))
                    (update :states merge (:states (fire-world {:x 2 :z 5}))))
              {:keys [eng p]} (setup w)
              result (await (child-outcome eng job {:box {:from {:x 0 :y 60 :z -2} :to {:x 4 :y 70 :z 2}}} 60))]
          (is (= 1 (:carpeted result)))
          (is (= 1 (:fires result)))
          (is (= "white_carpet" (block-name p 2 65 0)))
          (is (= "air" (block-name p 2 65 5))))))))

(deftest the-body-never-stands-in-the-fire-cell
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (assoc (fire-world {:inventory (inv "white_carpet" 1)}) :self {:pos {:x 2.5 :y 65 :z 0.5}})
              {:keys [eng p]} (setup w)
              result (await (child-outcome eng job {} 40))
              targets (map #(let [pos (.-pos (.-args %))] [(.-x pos) (.-y pos) (.-z pos)]) (calls p "moveTo"))]
          (is (= 1 (:carpeted result)))
          (is (not-any? #{[2 64 0] [2 65 0]} targets))
          (is (= 1 (count targets))))))))

(deftest the-end-reason
  (are [m seen expected] (= expected (guard/end-reason m seen))
    {:carpeted 1} {:left {} :fires 1} :guarded
    {:carpeted 1} {:left {"1,1,1" :no-carpet} :fires 2} :no-carpet
    {:carpeted 1 :skipped {"2,2,2" :unreachable}} {:left {"1,1,1" :no-carpet} :fires 2} :unreachable
    {} {:left {"1,1,1" :no-campfire "2,2,2" :no-carpet} :fires 2} :no-campfire
    {} {:left {} :fires 0} :no-fire
    {} {:left {} :fires 3} :safe
    {:skipped {"2,2,2" :on-fire}} {:left {} :fires 1} :on-fire))

;; ------------------------------------------------------------------ zones and claims

(def zone-over-fire {:name "bees" :min [2 60 0] :max [2 70 0]})

(defn ^:async run-guard
  "Submit the job with args over world w, tick n times; the setup map."
  [w args n]
  (let [s (setup w)]
    (core/submit! (:eng s) (list job args) {})
    (dotimes [_ n]
      (await (core/tick! (:eng s))))
    s))

(deftest a-fire-follows-its-zone-owner-and-the-opt-out
  (async done
    (tu/run-async done
      (fn ^:async t []
        (doseq [[owner extra block declined] [["Fake" {} "white_carpet" []] ["FAKE" {} "white_carpet" []]
                                              ["Miles" {} "air" [{:reason :refused :zones ["bees"]}]]
                                              ["Miles" {:ignore-zones? true} "white_carpet" []]]]
          (let [w (assoc (fire-world {:inventory (inv "white_carpet" 2)}) :zones [(assoc zone-over-fire :owner owner)])
                {:keys [p seen]} (await (run-guard w extra 12))]
            (is (= block (block-name p 2 65 0)) (pr-str [owner extra]))
            (is (= declined (mapv #(select-keys % [:reason :zones]) (kinds seen :apiary.guard-declined))) (pr-str [owner extra]))))))))

(deftest a-sink-in-a-foreign-zone-digs-and-places-nothing
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [w (-> (fire-world {:inventory (inv "campfire" 1 "white_carpet" 1)}) (open-side "3,64,0")
                    (assoc :zones [(assoc zone-over-fire :owner "Miles")]))
              {:keys [p]} (await (run-guard w {} 12))]
          (is (empty? (calls p "dig")))
          (is (empty? (calls p "place"))))))))

(deftest no-zone-list-declines-the-guard
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [p seen]} (await (run-guard (assoc (fire-world {:inventory (inv "white_carpet" 2)}) :zones nil) {} 12))]
          (is (empty? (calls p "place")))
          (is (= [:no-zones] (mapv :reason (kinds seen :apiary.guard-declined)))))))))
