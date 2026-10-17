(ns jobs.primitive
  "The acting primitives of engine.primitive as registry jobs: `job` wraps a table entry, which engine.registry lists
  as jobs.primitive.<kebab-case name> so agents and jobs call and quote each by name."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.primitive :as primitive]))

(def position-keys #{:pos})

(def also-required
  "Args beside the table's target that the act cannot go without."
  {:place [:item] :steer [:decide] :jumpPlace [:item] :trade [:id :op]})

(defn missing-args
  "The can-start? of primitive k: :ok when the args hold what the act needs, else :never :bad-args naming what is
  missing."
  [k]
  (let [need (distinct (concat (get-in primitive/table [k :target]) (also-required k)))]
    (fn [c]
      (if-let [missing (seq (remove #(some? (get (:args c) %)) need))]
        (contract/never :bad-args (str "missing " (str/join ", " (map name missing))) :missing (vec missing))
        (contract/ok :args "args given")))))

(a/defargs settings
  {::job-time-s {:default 300 :doc "Seconds one primitive act may take before it gives up (:limit-reached); an offline act is the longest." :spec (a/num-in 0 nil)}})

(def precondition-statuses
  "Statuses that say a precondition was false before the act ran (a failed condition); any other status the table
  reads as a failed condition is an act that ran and did not reach its end."
  #{:missing :occupied :gone :not-night :monsters-near :no-item :no-room :no-support :no-water :no-rod :no-food
    :no-lapis :no-levels :not-mounted :not-a-boat :not-mountable :hand-full :full :empty :busy :refused-mounted :already-mounted})

(defn read-ending
  "The table's ending e with the kind read by when the act failed: :unsupported impossible, a false precondition a
  failed condition, anything else nothing possible."
  [e]
  (cond
    (not= :condition-failed (:kind e)) e
    (= :unsupported (:cause e)) (assoc e :kind :impossible)
    (precondition-statuses (:cause e)) e
    :else (assoc e :kind :nothing-possible)))

(defn failure-text
  "What a failed act of primitive k met, as the ending's words: its status, the block that holds the cell, its reason."
  [k {:keys [status block reason]}]
  (str (name k) ": " status (some->> block (str " by ")) (some->> reason (#(str " (" % ")")))))

(defn args-data [k]
  (into {} (map (fn [arg] [arg (if (position-keys arg)
                                  {:doc "where, [x y z] or {:x :y :z}" :spec ::a/pos}
                                  {:doc (str "the act's " (name arg) ", as the primitive reads it")
                                   :spec (a/pred "a value" some?) :form 'some?})]))
        (get-in primitive/table [k :args])))

(defn act-args
  "The act's args as JavaScript: the job's own args, unset ones and the call keys left out."
  [args]
  (clj->js (apply dissoc (into {} (remove (comp nil? val)) args) (keys contract/call-keys))))

(defn nothing-to-check "The safe? and fit? of a primitive: the act is bounded and the caller judges it." [_] (contract/ok :ready "nothing to check"))

(defn job
  "The registry entry of primitive k, the job sym (engine.registry names it): one round is one act, ended by the table's
  ending of its result (the result's fields are the ending's data)."
  [k sym]
  (let [{:keys [target quote-of ending]} (get primitive/table k)]
    {:can-start? (missing-args k) :safe? nothing-to-check :fit? nothing-to-check :target target :category :phase :part-of [] :args (a/declare! sym (args-data k))
     :doc (str "The primitive " k " as a job: one act, which cannot be stopped midway. Takes the act's args (engine/README,"
               " Acting). Ends done when the act's status is a success, else with the ending its status maps to; the result"
               " fields are the ending's data.")
     :defaults {:spend {:job-time ::job-time-s}} :settings settings
     :quote-of (fn [c] (quote-of (:primitives c) (:args c)))
     :round (fn ^:async primitive-round [c]
              (let [r (try (await (ctx/act c k (act-args (:args c))))
                           (catch :default err
                             (if (.-badArgs err) err (throw err))))]
                (if (instance? js/Error r)
                  (ctx/end c :impossible {:reason :bad-args :why (.-message r)})
                  (let [e (read-ending (ending r))]
                    (let [data (js->clj r :keywordize-keys true)]
                      (ctx/end c (:kind e) (:cause e) (cond-> data (not= :done (:kind e)) (assoc :text (failure-text k data)))))))))}))
