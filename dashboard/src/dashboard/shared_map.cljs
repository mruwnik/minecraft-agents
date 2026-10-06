(ns dashboard.shared-map
  "Canonical protection zones and authored social claim overlays. Claims are
  labels on the common map; the engine does not enforce them, jobs consult them (jobs.lib.access) and may be told to ignore them."
  (:require ["fs" :as fs]
            ["path" :as path]
            [cljs.reader :as reader]
            [jobs.lib.zone-file :as zones]))

(defn display-zone [{:keys [name min max owner allow note] :as zone}]
  {:name name :x1 (nth min 0) :y1 (nth min 1) :z1 (nth min 2)
   :x2 (nth max 0) :y2 (nth max 1) :z2 (nth max 2)
   :owner owner :allow allow :note note :kind :zone})

(defn claim-overlay [claim now]
  (when (and (map? claim) (= :active (:status claim))
             (number? (:until claim)) (> (:until claim) now)
             (string? (:id claim))
             (empty? (zones/zone-errors [(assoc (select-keys claim [:min :max :owner :note]) :name (:id claim))])))
    (assoc (display-zone (assoc claim :name (str "claim: " (:id claim) " (" (:owner claim) ")")))
           :kind :claim :id (:id claim) :until (:until claim))))

(defn read-overlays
  "Prefer zones.edn when present; legacy JSON zones are a display-only fallback.
  Invalid canonical zones are reported instead of showing conflicting JSON.
  Only active, unexpired social claims are projected into the common map."
  ([dir legacy-zones] (read-overlays dir legacy-zones (js/Date.now)))
  ([dir legacy-zones now]
   (let [zone-file (.join path dir "zones.edn")
         claim-file (.join path dir "claims.edn")
         read-file (fn [file]
                     (when (.existsSync fs file)
                       (try
                         (if (> (.-size (.statSync fs file)) 8388608)
                           {:error "map overlay file exceeds 8 MiB"}
                           {:text (.readFileSync fs file "utf8")})
                         (catch :default e {:error (ex-message e)}))))
         zone-input (read-file zone-file)
         parsed (when (:text zone-input) (zones/parse (:text zone-input)))
         zone-values (if zone-input
                       (mapv display-zone (:value parsed))
                       legacy-zones)
         claim-input (read-file claim-file)
         claims (when (:text claim-input)
                  (try {:value (reader/read-string (:text claim-input))}
                       (catch :default e {:error (ex-message e)})))
         claim-values (when (vector? (:value claims)) (:value claims))]
     {:zones (into (vec zone-values) (keep #(claim-overlay % now) claim-values))
      :errors (vec (concat
                    (when (:error zone-input) [{:file "zones.edn" :message (:error zone-input)}])
                    (for [message (:errors parsed)] {:file "zones.edn" :message message})
                    (when (:error claim-input) [{:file "claims.edn" :message (:error claim-input)}])
                    (when (:error claims) [{:file "claims.edn" :message (:error claims)}])
                    (when (and claims (not (:error claims)) (not (vector? (:value claims))))
                      [{:file "claims.edn" :message "claims must be a vector"}])))})))
