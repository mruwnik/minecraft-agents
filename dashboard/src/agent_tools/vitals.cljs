(ns agent-tools.vitals
  "What a waiting agent is told besides the wake: health, food, hostiles near, a hit taken meanwhile, and that reflexes
  are off during an exclusive run. Best effort: a silent engine adds nothing."
  (:require [agent-tools.snapshot :as snapshot]
            [jobs.lib.cost.threat :as threat]
            ["node:fs" :as fs]
            ["node:path" :as path]
            [agent-tools.world-data :as data]))

(def reflexes-off "exclusive run: reflexes are off (no breathe, hostile, night); go-to still keeps air; hits are still reported (observe --wait wakes on a hit, :vitals carries health)")

(def hostile-radius 24)

(defn dist [a b]
  (js/Math.round (js/Math.hypot (- (:x a) (:x b)) (- (:y a) (:y b)) (- (:z a) (:z b)))))

(defn hostiles-near
  "{:hostiles [\"zombie 5\"] :heard [\"skeleton north\"]} from the /entities rows: hostile mobs seen within
  hostile-radius of pos (name and blocks away, nearest first), and the hostile ones only heard."
  [rows version pos]
  (let [{:keys [entities heard]} (snapshot/perceived rows version)
        hostile? #(and version (threat/hostile? version {:name (:name %)}))]
    {:hostiles (->> entities
                    (filter #(= "hostile" (:type %)))
                    (map (fn [{:keys [name] p :pos}] [name (dist pos p)]))
                    (filter #(<= (second %) hostile-radius))
                    (sort-by second)
                    (mapv (fn [[n d]] (str n " " d))))
     :heard (->> heard (filter hostile?) (mapv #(str (:name %) " " (some-> (:direction %) name))))}))

(defn view
  "The report of a wait: before and after are /status maps, seen is hostiles-near's answer. :hurt comes first."
  [before after seen]
  (let [lost (- (or (:health before) 0) (or (:health after) 0))]
    (into (array-map)
          (concat
           (when (and (:health before) (:health after) (pos? lost))
             [[:hurt (str "lost " lost " health during the wait (" (:health before) " -> " (:health after) ")")]])
           (keep (fn [[k v]] (when (some? v) [k v]))
                 [[:health (:health after)] [:food (:food after)]
                  [:hostiles (not-empty (:hostiles seen))] [:heard (not-empty (:heard seen))]
                  [:reflexes-off (when (:exclusive after) reflexes-off)]])))))

(defn pose-version
  "The minecraft version in the body's view/pose.json next to its engine socket, or nil."
  [socket-path]
  (let [file (.join path (.dirname path socket-path) ".." "view" "pose.json")]
    (try (some-> (when (.existsSync fs file) (js/JSON.parse (.readFileSync fs file "utf8"))) (.-mcVersion))
         (catch :default _ nil))))

(defn read!
  "A promise of the body's /status map plus :seen (hostiles-near's answer); {} when the engine does not answer."
  [socket-path get!]
  (-> (get! socket-path "/status" {})
      (.then (fn [{:keys [text]}]
               (let [status (data/read-edn text)
                     control (.join path (.dirname path socket-path) "control.sock")]
                 (-> (get! control "/entities" {})
                     (.then (fn [{:keys [text]}]
                              (assoc status :seen (hostiles-near (:entities (data/read-edn text)) (pose-version socket-path) (:position status)))))
                     (.catch (fn [_] status))))))
      (.catch (fn [_] {}))))

(defn around!
  "Run thunk (a promise of the wait result) between two reads: a promise of [result view]."
  [socket-path get! thunk]
  (-> (read! socket-path get!)
      (.then (fn [before]
               (-> (thunk)
                   (.then (fn [result]
                            (-> (read! socket-path get!)
                                (.then (fn [after] [result (view before after (:seen after))]))))))))))
