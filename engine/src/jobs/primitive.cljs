(ns jobs.primitive
  "The acting primitives of engine.primitive as registry jobs: `job` wraps a table entry, which engine.registry lists
  as jobs.primitive.<kebab-case name> so agents and jobs call and quote each by name."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.primitive :as primitive]))

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

(defn failure-text
  "What a failed act of primitive k met, as the ending's words: its status, the block that holds the cell, its reason."
  [k {:keys [status block reason]}]
  (str (name k) ": " status (some->> block (str " by ")) (some->> reason (#(str " (" % ")")))))

(defn spec-entry
  "An arg entry: doc, spec pred and the quoted form the catalog shows."
  [doc spec form]
  {:doc doc :spec spec :form form})

(def arg-specs
  "The args spec of each arg any primitive takes, as the primitive reads it (engine/js/prim-*.mjs)."
  (let [text (a/pred "a string" string?)
        number (a/pred "a number" a/finite?)
        id (a/or-of (a/pred "a number" a/finite?) (a/pred "a string" string?))
        map-arg (a/pred "a map" map?)]
    {:pos (spec-entry "where, [x y z] or {:x :y :z}" ::a/pos '::a/pos)
     :range (spec-entry "how near to get" (a/num-in 0 nil) '(a/num-in 0 nil))
     :timeoutS (spec-entry "seconds before the act gives up" (a/num-in 0 nil) '(a/num-in 0 nil))
     :maxDistance (spec-entry "the longest walk" (a/num-in 0 nil) '(a/num-in 0 nil))
     :maxDropDown (spec-entry "the longest drop a walk may take, in blocks" (a/int-in 0 nil) '(a/int-in 0 nil))
     :item (spec-entry "an item name" text 'string?)
     :click (spec-entry "the click chosen by the caller" map-arg 'map?)
     :count (spec-entry "how many" (a/int-in 0 nil) '(a/int-in 0 nil))
     :id (spec-entry "an entity id" id '(a/or-of number string))
     :entity (spec-entry "an entity id" id '(a/or-of number string))
     :direction (spec-entry "deposit or withdraw" #{"deposit" "withdraw"} #{"deposit" "withdraw"})
     :slot (spec-entry "an inventory slot" (a/int-in 0 nil) '(a/int-in 0 nil))
     :watchS (spec-entry "seconds to watch the drop" (a/num-in 0 nil) '(a/num-in 0 nil))
     :table (spec-entry "a crafting table cell {:x :y :z}" a/point? 'a/point?)
     :dest (spec-entry "where an item goes" #{"hand" "off-hand" "head" "torso" "legs" "feet"} #{"hand" "off-hand" "head" "torso" "legs" "feet"})
     :op (spec-entry "what to do at the block or villager" text 'string?)
     :input (spec-entry "what to load, {:item :count}" map-arg 'map?)
     :fuel (spec-entry "what to load, {:item :count}" map-arg 'map?)
     :output (spec-entry "false to leave the output" boolean? 'boolean?)
     :choice (spec-entry "the offer, 0 to 2" #{0 1 2} #{0 1 2})
     :levelCost (spec-entry "the levels the offer costs" (a/int-in 0 nil) '(a/int-in 0 nil))
     :message (spec-entry "the chat line" text 'string?)
     :to (spec-entry "the player to whisper to" text 'string?)
     :offer (spec-entry "the index of the trade" (a/int-in 0 nil) '(a/int-in 0 nil))
     :times (spec-entry "how many times" (a/int-in 1 nil) '(a/int-in 1 nil))
     :yaw (spec-entry "degrees" number '(a/num-in nil nil))
     :pitch (spec-entry "degrees" number '(a/num-in nil nil))
     :face (spec-entry "the face to click" #{"up" "down" "north" "south" "west" "east"} #{"up" "down" "north" "south" "west" "east"})
     :ms (spec-entry "milliseconds" (a/num-in 0 nil) '(a/num-in 0 nil))
     :toward (spec-entry "where to swim to" ::a/pos '::a/pos)
     :decide (spec-entry "the steering function" (a/pred "a function" fn?) 'fn?)
     :ticks (spec-entry "ticks of paddling" (a/int-in 0 nil) '(a/int-in 0 nil))
     :turn (spec-entry "left or right" #{"left" "right"} #{"left" "right"})
     :forward (spec-entry "paddle forward" boolean? 'boolean?)
     :why (spec-entry "why the act is done" text 'string?)}))

(defn args-data [k]
  (into {} (map (fn [arg] [arg (get arg-specs arg)])) (get-in primitive/table [k :args])))

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
                  (let [e (ending r)
                        data (js->clj r :keywordize-keys true)]
                    (ctx/end c (:kind e) (:cause e) (cond-> data (not= :done (:kind e)) (assoc :text (failure-text k data))))))))}))
