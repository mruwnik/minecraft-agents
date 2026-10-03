(ns dashboard.ui.eventlog
  "The action log of the body popup: category chips, the text filter and one-line summaries. Pure."
  (:require [clojure.string :as str]))

(def chips [[:all "all"] [:jobs "jobs"] [:movement "movement"] [:combat "combat"] [:chat "chat"] [:system "system"] [:errors "errors"]])

(def combat-kinds #{"hurt" "died" "attack" "attacked" "killed"})
(def combat-reflexes #{"flee" "fight" "hunt" "defend" "attack"})
(def movement-kinds #{"unreachable" "stuck" "unstick" "unstick.failed" "moved"})
(def movement-reflexes #{"stuck" "unstick"})
(def movement-action #"(?i)go[-_]?to|walk|move|path|flee|follow|swim|jump|approach|come")

(defn category [{:keys [source kind name reflex]}]
  (cond
    (or (= "chat" source) (#{"chat" "said" "whisper"} kind)) :chat
    (or (and (= "body" source) (combat-kinds kind)) (= "combat" source) (and (= "reflex" source) (combat-reflexes reflex))) :combat
    (or (= "movement" source) (movement-kinds kind) (and (= "reflex" source) (movement-reflexes reflex))
        (and (= "action" source) (re-find movement-action (str name)))) :movement
    (#{"job" "action" "reflex"} source) :jobs
    :else :system))

(defn summary [{:keys [source kind name text error args ms]}]
  (cond
    text text
    error error
    :else (let [parts (remove nil? [name
                                    (when (seq args) (js/JSON.stringify (clj->js args)))
                                    (when (number? ms) (str (js/Math.round ms) "ms"))])]
            (if (empty? parts) (str/replace (str kind) "_" " ") (str/join " " parts)))))

(defn matches-chip? [chip e]
  (case chip
    (nil :all) true
    :errors (boolean (#{"warn" "error"} (:level e)))
    (= chip (category e))))

(defn row [e]
  {:seq (:seq e) :t (:t e) :level (:level e) :category (category e)
   :source-kind (str (:source e) "." (:kind e)) :text (summary e)})

(defn rows
  "Newest first, after the chip and the text filter ({:chip :all|... :text \"\"}); events arrive oldest first."
  [events {:keys [chip text]}]
  (let [needle (str/lower-case (or text ""))
        hit? (fn [r] (or (str/blank? needle) (str/includes? (str/lower-case (str (:source-kind r) " " (:text r))) needle)))]
    (->> (rseq (vec events))
         (filter #(matches-chip? chip %))
         (map row)
         (filter hit?)
         vec)))

(defn current-action
  "The action still running (its start was the last action event), else nil."
  [events]
  (let [last-action (last (filter #(= "action" (:source %)) events))]
    (when (= "started" (:kind last-action)) (:name last-action))))
