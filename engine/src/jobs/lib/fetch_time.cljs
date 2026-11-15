(ns jobs.lib.fetch-time
  "The seconds fetching may take: the default :spend :job-time of a job that fetches (items.obtain, animals.leash,
  items.anvil-combine, items.enchant)."
  (:require [engine.args :as a]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]))

(a/defargs settings
  {::one-unit-s {:default 600 :doc "Seconds a fetch of one experience level may run: what a level of experience is priced at." :spec (a/num-in 0 nil)}})

(defn one-unit-s [] (settings/get settings ::one-unit-s))

(defn items-fig
  "{:value :sd} of the fetch (cost/fetch-value) of n (default 1) of the one of names that adds up least; zeros for no names."
  ([names] (items-fig names 1))
  ([names n]
   (if (seq names)
     (apply min-key #(+ (:value %) (:sd %)) (map #(select-keys (cost/fetch-value [{:name % :count (or n 1)}]) [:value :sd]) names))
     {:value 0 :sd 0})))

(defn items-s
  "Seconds a fetch of n (default 1) of any one of names may take: its items-fig value plus one sd (Q184), so an anvil counts its 31 iron."
  ([names] (items-s names 1))
  ([names n]
   (let [{:keys [value sd]} (items-fig names n)
         s (+ value sd)]
     (if (js/isFinite s) s 0))))
