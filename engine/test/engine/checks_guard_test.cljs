(ns engine.checks-guard-test
  "A job with :checks defaults that names a child job carrying a different :checks default lets the child's own form win
  over the parent's intent (the child's default beats the inherited one). Each such pair is listed on purpose."
  (:require [cljs.test :refer [deftest is]]
            [clojure.string :as str]
            [engine.layering-test :as layering]
            [engine.registry :as registry]
            ["fs" :as fs]))

(def allowed
  {['jobs.survival.night 'jobs.survival.sleep] "sleep is only called once night has chosen the bed, and asks (not (threatened))"
   ['jobs.survival.get-food 'jobs.combat.hunt] "get-food passes its own :checks (job-safe-or-starving) at the hunt call, so a starving body still hunts"
   ['jobs.survival.extinguish 'jobs.survival.get-food] "extinguish only reaches get-food by quoting eat's helper source; its eat call passes :get-food? false"
   ['jobs.survival.respond-to-hostile 'jobs.combat.attack] "the :hostiles fight call passes :checks (always) itself: a reflex fight is not refused for the danger it answers"
   ['jobs.survival.night 'jobs.survival.bed-down] "bed-down is only called once night has chosen the bed, and asks (not (threatened))"
   ['jobs.survival.rest 'jobs.survival.get-food] "rest fetches food (through eat) only when starving or calm, so get-food's own job-safe-or-starving form is the intent"
   ['jobs.survival.night 'jobs.survival.get-food] "the night hold calls it only when starving with no danger near, so its starving form is the intent"
   ['jobs.survival.leave-shelter 'jobs.access.cleanup] "the take-back call passes :checks itself: it asks (not (threatened)) and needs no fitness"
   ['jobs.survival.leave-shelter 'jobs.survival.get-food] "the dawn hold shares night's hold, which calls get-food only when starving with no danger near"
   ['jobs.survival.extinguish 'jobs.access.cleanup] "the scoop call passes :checks (always) itself: putting the fire out is not refused for the danger it answers"
   ['jobs.survival.breathe 'jobs.combat.attack] "the fight call passes :checks (always) itself: a reflex fight is not refused for the danger it answers"
   ['jobs.survival.stand-in 'jobs.combat.attack] "the cornered fight call (respond options) passes :checks (always) itself: a reflex fight is not refused for the danger it answers"
   ['jobs.survival.night 'jobs.combat.attack] "night only reaches the cornered fight through enter-room's respond options, which pass :checks (always) itself: a reflex fight is not refused for the danger it answers"
   ['jobs.survival.go-home 'jobs.survival.night] "go-home only names night in a :next hint for the caller; it never runs it"})

(defn source-of [ns-sym]
  (.readFileSync fs (str "src/" (-> (str ns-sym) (str/replace "-" "_") (str/replace "." "/")) ".cljs") "utf8"))

(defn helpers-of
  "ns-sym and the non-job, non-lib jobs namespaces it requires, transitively."
  [files ns-sym]
  (loop [todo [ns-sym] seen #{}]
    (if-let [n (first todo)]
      (recur (into (rest todo)
                   (for [r (get-in files [n :requires])
                         :when (and (= :helper (layering/kind files r)) (not (seen r)))]
                     r))
             (conj seen n))
      seen)))

(defn named-jobs
  "The registered jobs a namespace's source quotes ('jobs.x.y), but itself."
  [files ns-sym]
  (set (for [n (helpers-of files ns-sym)
             [_ sym] (re-seq #"'(jobs\.[a-z0-9.-]+)" (source-of n))
             :let [child (symbol sym)]
             :when (and (not= child ns-sym) (contains? registry/jobs child))]
         child)))

(deftest a-child-with-its-own-checks-default-is-listed-against-its-parent
  (let [files (layering/source-files)
        checks-of #(:checks (:defaults (get registry/jobs %)))
        parents (filter checks-of (keys registry/jobs))
        odd (set (for [p parents
                       c (named-jobs files p)
                       :when (and (checks-of c) (not= (checks-of c) (checks-of p)))]
                   [p c]))]
    (is (seq parents))
    (is (= #{} (apply disj odd (keys allowed)))
        "a child with another :checks default under a parent that has one: make the parent pass the form at the call, or list the pair in allowed with a reason")
    (is (= #{} (apply disj (set (keys allowed)) odd)) "allowed pairs that are no longer different: delete them")))
