(ns plan.drop-status
  "One-off migration for the removed plan :status field: every submitted plan is active, so a :retired plan is deleted
  and every other plan only loses the key. Pure: plans in, plans out (migrate), and the same on a plan file's text
  (strip-status-text, which keeps formatting and comments). The IO is plan.drop-status-cli."
  (:require [clojure.string :as str]))

(defn migrate
  "{id plan} -> {:plans {id plan} :deleted [ids of retired plans] :stripped [ids that had the key]}, both sorted."
  [plans]
  (let [{retired true kept false} (group-by #(= :retired (:status (val %))) plans)]
    {:plans (into {} (map (fn [[id plan]] [id (dissoc plan :status)])) kept)
     :deleted (vec (sort (map key retired)))
     :stripped (vec (sort (for [[id plan] kept :when (contains? plan :status)] id)))}))

(def delimiters #{\{ \} \[ \] \( \) \" \; \space \newline \tab \return \,})
(def blanks #{\space \newline \tab \return \,})

(defn token-end
  "Index after the token that starts at i in text."
  [text i]
  (or (first (filter #(contains? delimiters (nth text %)) (range i (count text)))) (count text)))

(defn skip-string [text i]
  (loop [j (inc i)]
    (cond
      (>= j (count text)) (count text)
      (= \\ (nth text j)) (recur (+ j 2))
      (= \" (nth text j)) (inc j)
      :else (recur (inc j)))))

(defn skip-blanks [text i]
  (or (first (remove #(contains? blanks (nth text %)) (range i (count text)))) (count text)))

(defn status-pair-end
  "End index of the `:status :value` pair that starts at i, or nil when text does not have one there."
  [text i]
  (let [key-end (token-end text i)]
    (when (= ":status" (subs text i key-end))
      (let [v (skip-blanks text key-end)]
        (when (and (< v (count text)) (= \: (nth text v)))
          (token-end text v))))))

(defn strip-status-text
  "The text of a plan file without its top-level :status pair (the pair, the blanks before it, and when it came first
  the blanks after it). Strings, comments and nested maps are left alone."
  [text]
  (let [n (count text)]
    (loop [i 0, depth 0, out []]
      (if (>= i n)
        (apply str out)
        (let [c (nth text i)]
          (cond
            (= \" c) (let [e (skip-string text i)] (recur e depth (conj out (subs text i e))))
            (= \; c) (let [e (or (str/index-of text "\n" i) n)] (recur e depth (conj out (subs text i e))))
            (contains? #{\{ \[ \(} c) (recur (inc i) (inc depth) (conj out c))
            (contains? #{\} \] \)} c) (recur (inc i) (dec depth) (conj out c))
            (and (= 1 depth) (= \: c) (status-pair-end text i))
            (let [e (status-pair-end text i)
                  kept (apply str out)
                  trimmed (str/replace kept #"[ \n\t\r,]+$" "")
                  first-key? (str/ends-with? trimmed "{")
                  next-i (if first-key? (skip-blanks text e) e)]
              (recur next-i depth [trimmed]))
            :else (recur (inc i) depth (conj out c))))))))
