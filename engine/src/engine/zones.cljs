(ns engine.zones
  "The world's zone file (worlds/<world>/zones.edn) checked and parsed; no IO. A zone is an inclusive box
  {:name \"farm\" :min [x y z] :max [x y z] :owner \"name\" :allow #{:dig :place :harvest :take :put} :note \"text\"}.
  :allow is what OTHERS may do inside it (none when absent); the owner may always act. :note is optional. engine.access.zones reads them."
  (:require [cljs.reader :as reader]
            [clojure.string :as str]))

(def actions #{:dig :place :harvest :take :put})

(def zone-keys #{:name :min :max :owner :allow :note})

(defn coords? [v] (and (vector? v) (= 3 (count v)) (every? integer? v)))

(defn text? [v] (and (string? v) (not (str/blank? v))))

(defn zone-problems
  "Every problem of one zone map, as texts without the zone's label."
  [{:keys [name min max owner allow note] :as zone}]
  (let [corners? (and (coords? min) (coords? max))]
    (filterv some?
             (concat
              [(when-not (text? name) ":name must be a non-empty string")]
              (for [[k v] [[:min min] [:max max]] :when (not (coords? v))]
                (str k " must be [x y z] integers"))
              (when corners?
                (for [[axis a b] (map vector ["x" "y" "z"] min max) :when (> a b)]
                  (str ":min is above :max on " axis)))
              [(when-not (text? owner) ":owner must be a non-empty string")
               (when-not (or (not (contains? zone :allow)) (and (set? allow) (every? actions allow)))
                 ":allow must be a set of :dig :place :harvest :take :put")
               (when-not (or (nil? note) (string? note)) ":note must be a string")]
              (for [k (keys zone) :when (not (zone-keys k))] (str "unknown key " k))))))

(defn label [i zone] (if (text? (:name zone)) (:name zone) (str i)))

(defn zone-errors
  "Every problem of a parsed zone list as texts naming the zone (its name, else its index); empty when valid."
  [zones]
  (if-not (vector? zones)
    ["the zone file must hold a vector of zones"]
    (let [earlier? (fn [i name] (and (text? name) (some #(and (map? %) (= name (:name %))) (take i zones))))]
      (vec (mapcat (fn [i zone]
                     (let [where (str "zone " (label i zone) ": ")]
                       (if-not (map? zone)
                         [(str where "not a map")]
                         (map #(str where %)
                              (cond-> (zone-problems zone)
                                (earlier? i (:name zone)) (conj "the name is used twice"))))))
                   (range) zones)))))

(defn parse
  "Text of the zone file -> {:value zones} or {:errors [text ...]}; never throws."
  [text]
  (let [read (try {:value (reader/read-string text)} (catch :default e {:error (str "unreadable EDN: " (ex-message e))}))]
    (if (:error read)
      {:errors [(:error read)]}
      (let [errors (zone-errors (:value read))]
        (if (seq errors) {:errors errors} {:value (:value read)})))))

(def claim-keys #{:id :owner :min :max :until :status :note})

(defn claim-problems [i {:keys [id owner min max until status] :as claim}]
  (let [where (str "claim " (if (text? id) id i) ": ")]
    (map #(str where %)
         (filterv some?
                  (concat
                   [(when-not (text? id) ":id must be a non-empty string")
                    (when-not (text? owner) ":owner must be a non-empty string")
                    (when-not (and (coords? min) (coords? max)) ":min and :max must be [x y z] integers")
                    (when-not (integer? until) ":until must be an integer (ms)")
                    (when-not (#{"active" "released"} (some-> status name)) ":status must be :active or :released")]
                   (for [k (keys claim) :when (not (claim-keys k))] (str "unknown key " k)))))))

(defn parse-claims
  "Text of the claims file -> {:value claims} or {:errors [text ...]}; never throws."
  [text]
  (let [read (try {:value (reader/read-string text)} (catch :default e {:error (str "unreadable EDN: " (ex-message e))}))
        claims (:value read)]
    (cond
      (:error read) {:errors [(:error read)]}
      (not (vector? claims)) {:errors ["the claims file must hold a vector of claims"]}
      :else (let [errors (vec (mapcat (fn [i c] (if (map? c) (claim-problems i c) [(str "claim " i ": not a map")]))
                                      (range) claims))]
              (if (seq errors) {:errors errors} {:value claims})))))
