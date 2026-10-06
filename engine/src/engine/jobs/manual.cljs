(ns engine.jobs.manual
  "What manual takeover (engine.takeover) does as the jobs do, called through engine.hooks: a move-to walks as go-to
  walks (engine.path.near), a dig holds the best carried tool, a wear wears as the wear job does."
  (:require [engine.armour :as armour]
            [engine.jobs.tools :as tools]
            [engine.path.near :as near]
            [engine.path.walk :as walk]))

(defn plannable?
  "Whether the body of primitives p can sense the world for path planning."
  [p]
  (some? (walk/path-world p)))

(def cell-of near/cell-of)

(def walk-round! near/walk-round!)

(def dig-floor-s "A short dig keeps the common 10 s lease." 10)
(def dig-margin-s "Slack over the expected dig time: the look, the equip, the drop wait and latency." 8)
(def dig-cap-s "The longest lease a dig is given, whatever digTime says." 60)

(defn dig-need
  "The tool name a dig at pos lacks: no carried tool can harvest the block there (nil when one can, or no block)."
  [p pos]
  (let [block (some-> (.blockAt p pos) .-name)
        names (map :name (js->clj (.-inventory (.self p)) :keywordize-keys true))]
    (when block (tools/harvest-need names (some-> (.harvestTools p block) (js->clj))))))

(defn dig-timeout-s
  "The lease seconds a world dig needs: the expected dig time of the tool the dig will hold (primitives digTime, the
  carried best tool for the block) plus dig-margin-s, at least dig-floor-s and at most dig-cap-s. A dig that answers
  no-tool at once digs nothing, so it needs only dig-floor-s."
  [p args]
  (let [pos (clj->js (:pos args))
        block (some-> (.blockAt p pos) .-name)
        names (map :name (js->clj (.-inventory (.self p)) :keywordize-keys true))
        ms (if (dig-need p pos) 0 (or (.digTime p pos (when block (tools/best-tool names block))) 0))]
    (-> (+ (/ ms 1000) dig-margin-s) (max dig-floor-s) (min dig-cap-s) js/Math.ceil)))

(defn ^:async dig-with-tool!
  "A manual dig as a job digs: the best carried tool for the block is held first. A block no carried tool can harvest is
  not dug (the dig would lose its drop): {:status \"no-tool\" :block :needed :reason}. Resolves to a JS result."
  [p token args]
  (let [block (some-> (.blockAt p (clj->js (:pos args))) .-name)
        names (map :name (js->clj (.-inventory (.self p)) :keywordize-keys true))
        needed (when block (tools/harvest-need names (some-> (.harvestTools p block) (js->clj))))
        tool (when block (tools/best-tool names block))]
    (if needed
      #js {:status "no-tool" :block block :needed needed
           :reason (str block " needs " needed (when (not= "pickaxe" needed) " or better") "; no carried tool can harvest it")}
      (do
        (when (and tool (not= tool (.-held (.self p))))
          (await (.equip p token #js {:item tool :dest "hand"})))
        (await (.dig p token (clj->js args)))))))

(defn ^:async wear!
  "A manual wear as the wear job does it: the named carried piece, or the best carried piece for each slot that is
  empty or worn weaker. Resolves to a JS result: {:status \"worn\" :worn [{:item :slot}]}, or {:status \"cannot\" :reason
  \"not-armour\"|\"no-item\"}, or {:status \"failed\" ...} when the server did not take a piece."
  [p token args]
  (let [self (.self p)
        names (map :name (js->clj (.-inventory self) :keywordize-keys true))
        r (await (armour/wear! (fn [item slot] (.equip p token (clj->js {:item item :dest slot})))
                               (armour/worn-of (.-equipment self)) names (:item args)))]
    (clj->js (cond
               (= :failed (:reason r)) (assoc (select-keys r [:worn :item :status]) :status "failed" :reason (:status r))
               (:ok r) {:status "worn" :worn (:worn r)}
               :else {:status "cannot" :reason (name (:reason r)) :item (:item r)}))))
