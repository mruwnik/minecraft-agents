(ns jobs.items.fetch-limits
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [jobs.lib.fetch :as fetch]))

(def doc
  "Set the body's own default fetch limits (body memory :fetch/limits, kept until changed), in one round. Without :job
  they hold for every job with :fetch; with :job (a job symbol such as jobs.blocks.dig) for that job only, over the
  all-jobs ones. Given keys replace the old ones, the rest stay; :clear true drops the defaults (of :job, or all).
  Precedence of a fetch's limits, later wins: built-in {:depth 7 :minutes 10 :fail-minutes 10 :what all :how all}, the
  job's code default, these all-jobs defaults, these per-job defaults, the job's own :fetch arg.
  Refused (warn fetch-limits.refused, result {:ok false :reason :bad-args :text}) for a bad value. Success: info
  fetch-limits.set, result {:ok true :limits {:all {..} :jobs {..}}}.")

(a/defargs args
  {:job {:doc "a job symbol these limits are for; nil: every job" :spec symbol? :default nil}
   :depth {:doc "nested fetches allowed" :spec (a/int-in 0 nil) :default nil}
   :minutes {:doc "time budget of one fetch, minutes" :spec (a/num-in 0 nil) :default nil}
   :fail-minutes {:doc "how long a failed fetch is remembered and not tried again, minutes" :spec (a/num-in 0 nil) :default nil}
   :what {:doc "kinds fetched, a subset of #{:tool :item}" :spec (a/set-of #{:tool :item}) :default nil}
   :how {:doc "sources used, a subset of #{:chest :craft :gather}" :spec (a/set-of #{:chest :craft :gather}) :default nil}
   :clear {:doc "drop the defaults (of :job, or all) instead" :spec boolean? :default false}})

(defn check [_c] true)

(defn given [a]
  (into {} (remove (comp nil? val)) (select-keys a fetch/limit-keys)))

(defn next-limits
  "The body limits after args a, or {:error text}."
  [body a]
  (let [job (:job a)
        new (given a)
        path (if job [:jobs job] [:all])]
    (cond
      (and (some? job) (not (symbol? job))) {:error ":job is a job symbol"}
      (fetch/limit-error new) {:error (fetch/limit-error new)}
      (and (:clear a) job) (let [b (update body :jobs dissoc job)] (if (empty? (:jobs b)) (dissoc b :jobs) b))
      (:clear a) {}
      :else (update-in body path merge new))))

(defn round [c]
  (let [b (next-limits (fetch/body-limits (ctx/view c)) (:args c))]
    (if-let [e (:error b)]
      (do (ctx/emit! c :fetch-limits.refused :warn {:reason :bad-args :text (str "fetch-limits: " e)})
          (ctx/result! c {:ok false :reason :bad-args :text e}))
      (do (ctx/remember! c fetch/limits-kind b {:cap 1 :ttl :forever})
          (ctx/emit! c :fetch-limits.set :info {:limits b :text (str "fetch limits now " (pr-str b))})
          (ctx/result! c {:ok true :limits b})))
    :done))
