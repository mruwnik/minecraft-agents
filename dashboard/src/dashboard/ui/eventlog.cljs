(ns dashboard.ui.eventlog
  "The body popup's event stream: categories, attention state and one-line summaries. Pure."
  (:require [clojure.string :as str]))

(def chips [[:all "all"] [:jobs "jobs"] [:movement "movement"] [:combat "combat"] [:chat "chat"]
            [:system "system"] [:notices "notices"] [:required "required"]])

(def combat-kinds #{"hurt" "died" "attack" "attacked" "killed"})
(def combat-reflexes #{"flee" "fight" "hunt" "defend" "attack"})
(def movement-kinds #{"unreachable" "stuck" "unstick" "unstick.failed" "moved"})
(def movement-reflexes #{"stuck" "unstick"})
(def movement-action #"(?i)go[-_]?to|walk|move|path|flee|follow|swim|jump|approach|come")

(defn field-name [x]
  (cond (keyword? x) (name x) (string? x) x :else (when (some? x) (str x))))
(defn source [e] (field-name (:source e)))
(defn kind [e] (field-name (:kind e)))
(defn context [e] (or (:context e) {}))
(defn data [e] (or (:data e) {}))
(defn event-time [e] (or (:time-ms e) (:t e)))
(defn event-name [e]
  (let [ctx (context e) d (data e)]
    (or (:name d) (:job-name d) (:action d) (:job d) (:name ctx) (:job-id ctx) (:reflex-id ctx))))
(defn event-message [e]
  (let [d (data e)]
    (or (:message e) (:error d) (:reason d)
        (when-let [name (event-name e)] (str name))
        (str/replace (or (kind e) "event") "_" " "))))

(defn category [e]
  (let [s (source e) k (kind e) d (data e)
        reflex (or (:reflex-id (context e)) (:reflex d))
        action (or (:action d) (:name d))]
    (cond
      (or (= "chat" s) (contains? #{"chat" "said" "whisper"} k)) :chat
      (or (and (= "body" s) (combat-kinds k)) (= "combat" s) (and (= "reflex" s) (combat-reflexes reflex))) :combat
      (or (= "movement" s) (movement-kinds k) (and (= "reflex" s) (movement-reflexes reflex))
          (and (= "action" s) (re-find movement-action (str action)))) :movement
      (contains? #{"job" "action" "reflex"} s) :jobs
      :else :system)))

(defn attention [e outstanding]
  (let [a (field-name (or (:attention e) :none))
        request-id (:request-id e)]
    (cond
      (and (= "attention" (source e)) (= "resolved" (kind e))) :resolved
      (= "notice" a) :notice
      (= "required" a) (if (contains? outstanding request-id) :required :resolved)
      :else :routine)))

(defn summary [e]
  (or (:message e)
      (let [d (data e)
            reason (or (:reason d) (:error d))
            name (event-name e)
            args (:args d)
            ms (:ms d)
            parts (remove nil? [name
                                (when reason (field-name reason))
                                (when (seq args) (pr-str args))
                                (when (number? ms) (str (js/Math.round ms) "ms"))])]
        (if (seq parts) (str/join " " parts) (str/replace (or (kind e) "event") "_" " ")))))

(defn matches-chip? [chip row]
  (case chip
    (nil :all) true
    :notices (= :notice (:attention row))
    :required (= :required (:attention row))
    (= chip (:category row))))

(defn row [e outstanding]
  {:seq (:seq e) :generation-id (:generation-id e) :t (event-time e)
   :attention (attention e outstanding) :category (category e)
   :source-kind (str (source e) "." (kind e)) :text (summary e)})

(defn rows
  "Newest first, after the chip and text filter; events arrive oldest first."
  ([events opts] (rows events {} opts))
  ([events outstanding {:keys [chip text]}]
   (let [needle (str/lower-case (or text ""))
         hit? (fn [r] (or (str/blank? needle) (str/includes? (str/lower-case (str (:source-kind r) " " (:text r))) needle)))]
     (->> (rseq (vec events))
          (map #(row % outstanding))
          (filter #(matches-chip? chip %))
          (filter hit?)
          vec))))

(defn current-action
  "The action still running (its start was the last action event), else nil."
  [events]
  (let [last-action (last (filter #(= "action" (source %)) events))]
    (when (= "started" (kind last-action)) (event-name last-action))))

(defn request-text [request]
  (let [e (:event request)
        ctx (context e)
        d (data e)
        reason (or (:reason request) (:reason d))
        job (or (:job-id request) (:job-id ctx))]
    (str (event-message e)
         (when job (str " · " job))
         (when (and reason (not= reason (:reason d))) (str " · " (field-name reason))))))
