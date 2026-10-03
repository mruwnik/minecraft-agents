(ns engine.jobs.samples
  "Three tiny jobs that prove the job contract end to end. The real job
  library lives in its own namespaces under engine.jobs."
  (:require [engine.ctx :as ctx]))

(defn distance [a b]
  (js/Math.hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b))))

(defn self-pos [c]
  (let [p (.-pos (.self (:primitives c)))]
    {:x (.-x p) :y (.-y p) :z (.-z p)}))

(def max-blocked 3)

(defn ^:async go-to-round
  "args {:pos {:x :y :z} :range 1}. Walks; a partial walk continues next
  round; three blocked walks give up with an unreachable warn."
  [c]
  (let [{:keys [pos range] :or {range 1}} (:args c)]
    (if (<= (distance (self-pos c) pos) range)
      :done
      (let [r (await (ctx/act c :moveTo (clj->js {:pos pos :range range})))]
        (case (.-status r)
          "arrived" :done
          "partial" :continue
          (let [tries (inc (:blocked (ctx/mem c) 0))]
            (ctx/update-mem! c assoc :blocked tries)
            (if (< tries max-blocked)
              :continue
              (do (ctx/emit! c :unreachable :warn {:target pos :tries tries
                                                    :text (str "gave up walking to " pos)})
                  :done))))))))

(def go-to {:name :go-to :check (constantly true) :round go-to-round})

(defn day? [c] (.-isDay (.self (:primitives c))))

(defn ^:async wait-for-day-round
  "Runs only by day (its check declines at night), so it is done at once."
  [_c]
  :done)

(def wait-for-day {:name :wait-for-day :check day? :round wait-for-day-round})

(defn ^:async eat-round
  "Eat the best food carried, once. args {:item name} picks one."
  [c]
  (let [r (await (ctx/act c :eat (clj->js (select-keys (:args c) [:item]))))]
    (when (= "no-food" (.-status r))
      (ctx/emit! c :no_food :info {:text "nothing to eat"}))
    :done))

(def eat {:name :eat :check (constantly true) :round eat-round})

(def look-ahead 3)

(def looked-policy {:cap 1 :ttl :forever})

(defn ^:async look-around-round
  "Face a point a few blocks ahead of the body, once, then write a :looked
  entry to body memory, which the :every-interval trigger reads."
  [c]
  (let [{:keys [x y z]} (self-pos c)]
    (await (ctx/act c :look (clj->js {:pos {:x (+ x look-ahead) :y (inc y) :z z}})))
    (ctx/remember! c :looked {} looked-policy)
    :done))

(def look-around {:name :look-around :check (constantly true) :round look-around-round})

(defn ^:async pace-round
  "args {:a pos :b pos :laps 3 :rounds 8 :range 1}. One round walks a, b, a, b
  (:laps times each) with a moveTo per leg, stopping early on any status but
  \"arrived\". Continues until :rounds rounds have run. A harmless long round
  for showing a reflex cut a running job; a cut throws out of ctx/act."
  [c]
  (let [{:keys [a b laps rounds range] :or {laps 3 rounds 8 range 1}} (:args c)
        legs (take (* 2 laps) (cycle [a b]))]
    (loop [legs legs]
      (when-let [pos (first legs)]
        (let [r (await (ctx/act c :moveTo (clj->js {:pos pos :range range})))]
          (when (= "arrived" (.-status r))
            (recur (rest legs))))))
    (let [done-rounds (inc (:rounds-run (ctx/mem c) 0))]
      (ctx/update-mem! c assoc :rounds-run done-rounds)
      (if (>= done-rounds rounds) :done :continue))))

(def pace {:name :pace :check (constantly true) :round pace-round})
