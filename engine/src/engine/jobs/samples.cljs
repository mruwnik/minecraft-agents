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
            (ctx/commit! c {:blocked tries})
            (if (< tries max-blocked)
              :not-ready
              (do (ctx/emit! c :unreachable :warn {:target pos :tries tries
                                                    :text (str "gave up walking to " pos)})
                  :done))))))))

(def go-to {:name :go-to :round go-to-round})

(defn ^:async wait-for-day-round
  "Done once it is day; otherwise yields until day."
  [c]
  (if (.-isDay (.self (:primitives c)))
    :done
    {:status :continue :wake [:day]}))

(def wait-for-day {:name :wait-for-day :round wait-for-day-round})

(defn ^:async eat-round
  "Eat the best food carried, once. args {:item name} picks one."
  [c]
  (let [r (await (ctx/act c :eat (clj->js (select-keys (:args c) [:item]))))]
    (when (= "no-food" (.-status r))
      (ctx/emit! c :no_food :info {:text "nothing to eat"}))
    :done))

(def eat {:name :eat :round eat-round})

(def look-ahead 3)

(defn ^:async look-around-round
  "Face a point a few blocks ahead of the body, once, then record the time in
  body memory as :every-interval-last, which the :every-interval trigger reads."
  [c]
  (let [{:keys [x y z]} (self-pos c)]
    (await (ctx/act c :look (clj->js {:pos {:x (+ x look-ahead) :y (inc y) :z z}})))
    (ctx/commit! c :body #(assoc % :every-interval-last ((:now (:engine c)))))
    :done))

(def look-around {:name :look-around :round look-around-round})
