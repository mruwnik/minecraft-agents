(ns engine.job-api
  "Bounded agent job commands. Scheduling stays in core; command dedupe is saved with engine state."
  (:require [engine.backoff :as backoff]
            [engine.core :as core]
            [engine.expr :as expr]
            [clojure.walk :as walk]
            ["crypto" :as crypto]))

(def max-requests 128)
(defn text [s n] (let [s (str s)] (subs s 0 (min n (count s)))))
(defn status [eng id]
  (let [s (core/state eng)]
    (cond (contains? (:failed s) id) :failed
          (= id (:id (core/holder eng))) :running
          (= id (:resume s)) :resuming
          (some #{id} (:list s)) :queued
          :else :absent)))
(defn summary [eng id]
  (when-let [inst (get-in (core/state eng) [:instances id])]
    {:id id :name (text (expr/label (:spec inst)) 160) :status (status eng id)
     :round (:round inst)}))
(defn list-jobs [eng offset limit]
  (let [ids (:list (core/state eng))
        items (mapv #(summary eng %) (take limit (drop offset ids)))
        next (+ offset (count items))]
    (cond-> {:total (count ids) :items items}
      (< next (count ids)) (assoc :next-offset next))))
(defn fail [reason & [message]]
  (cond-> {:ok false :reason reason} message (assoc :message (text message 240))))
(defn valid-request-id? [id]
  (and (string? id) (boolean (re-matches #"[A-Za-z0-9_.:-]{1,80}" id))))
(defn valid-job-id? [id] (and (string? id) (boolean (re-matches #"j[0-9]+" id))))
(defn valid-by?
  "Who asks: a short string or keyword."
  [by]
  (or (keyword? by) (and (string? by) (<= 1 (count by) 40))))
(defn backoff-problem [cfg] (try (backoff/validate! cfg) nil (catch :default e (ex-message e))))
(defn bounded-spec? [spec]
  (let [budget (volatile! 256)]
    (letfn [(walk [v depth]
              (vswap! budget dec)
              (and (not (neg? @budget)) (<= depth 24)
                   (cond (map? v) (every? #(walk % (inc depth)) (mapcat identity v))
                         (coll? v) (every? #(walk % (inc depth)) v)
                         (string? v) (<= (count v) 2048)
                         :else true)))]
      (walk spec 0))))
(defn fingerprint [request]
  (let [canonical (walk/postwalk #(if (map? %) (into (sorted-map-by (fn [a b] (compare (pr-str a) (pr-str b)))) %) %)
                                (select-keys request [:op :id :spec :generation-id :front? :hold? :backoff :by]))]
    (.digest (.update (.createHash crypto "sha256") (pr-str canonical)) "hex")))
(defn remember! [eng id record]
  (swap! (:state eng)
         (fn [s]
           (let [ledger (:job-requests s)
                 order (if (contains? (:records ledger) id) (:order ledger) (conj (vec (:order ledger)) id))
                 order (vec (take-last max-requests order))]
             (assoc s :job-requests {:order order :records (select-keys (assoc (:records ledger) id record) order)})))))
(defn submit-opts
  "core/submit! opts from a request's :front? :hold? :backoff :by."
  [request by]
  (merge {:by by} (select-keys request [:front? :hold? :backoff])))
(defn mutate! [eng {:keys [op id spec request-id generation-id by] :or {by :agent} :as request}]
  (let [prior (get-in (core/state eng) [:job-requests :records request-id])
        signature (fingerprint request)
        spec-error (when (#{:submit :interrupt} op)
                     (if (bounded-spec? spec) (expr/problem (:jobs eng) spec) "job expression exceeds size/depth limits"))]
    (cond
      (not (map? request)) (fail :bad-request)
      (not (contains? #{:submit :interrupt :cancel :retry} op)) (fail :bad-op)
      (seq (remove #{:op :id :spec :request-id :generation-id :front? :hold? :backoff :by} (keys request))) (fail :unknown-field)
      (not (valid-request-id? request-id)) (fail :bad-request-id)
      (not= generation-id (:generation-id (core/state eng))) (fail :generation-mismatch)
      prior (if (= signature (:signature prior))
              (if-let [result (:result prior)]
                (cond-> (assoc result :duplicate true)
                  (get-in result [:job :id]) (assoc :job (or (summary eng (get-in result [:job :id]))
                                                          {:id (get-in result [:job :id]) :status :absent})))
                  (fail :request-uncertain "A previous attempt was interrupted; it will not be executed again."))
              (fail :request-id-conflict))
      spec-error (fail :bad-spec spec-error)
      (some #(and (contains? request %) (not (boolean? (get request %)))) [:front? :hold?]) (fail :bad-field ":front? and :hold? are true or false")
      (and (contains? request :backoff) (backoff-problem (:backoff request))) (fail :bad-backoff (backoff-problem (:backoff request)))
      (not (valid-by? by)) (fail :bad-by ":by is a short string or keyword naming who asks")
      (and (#{:cancel :retry} op) (not (valid-job-id? id))) (fail :bad-job-id)
      (and (#{:cancel :retry} op) (nil? (get-in (core/state eng) [:instances id]))) (fail :job-not-found)
      (and (= op :interrupt) (core/manual? eng)) (fail :manual-control "Release the exclusive body lease before interrupting; submit can still queue work.")
      :else
      (do
        ;; This synchronous state swap is durably saved by core's existing state watch before any job change.
        (remember! eng request-id {:signature signature :status :pending})
        (let [result (try
                       (case op
                         :submit (let [job-id (core/submit! eng spec (submit-opts request by))]
                                   (swap! (:state eng) assoc-in [:instances job-id :by] by)
                                   {:ok true :job (summary eng job-id)})
                         :interrupt (let [job-id (core/do-now! eng spec)]
                                      (swap! (:state eng) assoc-in [:instances job-id :by] by)
                                      {:ok true :job (summary eng job-id)})
                         :cancel (do (core/cancel! eng id) {:ok true :id id :status :cancelled})
                         :retry (if (core/retry! eng id) {:ok true :job (summary eng id)}
                                    (fail :not-failed)))
                       (catch :default e (fail :request-uncertain (ex-message e))))]
          (when-not (= :request-uncertain (:reason result))
            (remember! eng request-id {:signature signature :status :finished :result result}))
          result)))))
