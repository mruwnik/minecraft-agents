(ns jobs.farm.tend-decide
  "The steps of jobs.farm.tend, the child job of each, and the decision per step over the facts: skip it with a
  reason, or call its child.")

(def steps [:harvest :irrigate :till :plant :fertilize])

(def jobs
  {:harvest 'jobs.farm.harvest
   :irrigate 'jobs.farm.irrigate
   :till 'jobs.farm.till
   :plant 'jobs.farm.plant
   :fertilize 'jobs.farm.fertilize})

(defn decide-harvest
  [{:keys [area]} {:keys [ripe]}]
  (if (zero? ripe)
    {:skip :no-ripe}
    {:call {:slot :harvest :job (jobs :harvest)
            :args (if (:plan area)
                    {:area (select-keys area [:plan :part]) :replant true :replant-bare false}
                    {:area area :replant true})}}))

(defn decide-irrigate
  [{:keys [irrigate area]} {:keys [dry ground-box]}]
  (cond
    (not irrigate) {:skip :irrigate-off}
    (not (pos? (or dry 0))) {:skip :not-dry}
    :else {:call {:slot :irrigate :job (jobs :irrigate)
                  :args (cond-> {:area {:min (:min ground-box) :max (:max ground-box)}} (:plan area) (assoc :for-plan (:plan area)))}}))

(defn decide-till
  [{:keys [till area]} {:keys [hoe untilled bare seeds till-short till-held]}]
  (cond
    (not till) {:skip :till-off}
    (not hoe) {:skip :no-hoe}
    (and (empty? untilled) till-held) {:skip :food-reserve :detail till-held}
    (empty? untilled) {:skip (if (pos? (or till-short 0)) :no-seed :nothing-to-till)}
    (and (not (:plan area)) (<= seeds bare)) {:skip :no-seed}
    :else {:call {:slot :till :job (jobs :till)
                  :args (cond-> {:area {:min (first untilled) :max (first untilled)}} (:plan area) (assoc :for-plan (:plan area)))}}))

(defn decide-plant
  [{:keys [area]} {:keys [bare seed]}]
  (cond
    (zero? bare) {:skip :no-bare}
    (not seed) {:skip :no-seed}
    :else {:call {:slot :plant :job (jobs :plant) :args {:area (if (:plan area) (select-keys area [:plan :part]) area) :ways #{:carried}}}}))

(defn decide-fertilize
  [{:keys [fertilize]} {:keys [meal bones unripe field]}]
  (cond
    (not fertilize) {:skip :fertilize-off}
    (not (or meal bones)) {:skip :no-bone-meal}
    (zero? unripe) {:skip :none-unripe}
    :else {:call {:slot :fertilize :job (jobs :fertilize) :args {:area field}}}))

(defn decide
  "{:skip reason} or {:call {:slot :job :args}} for a step."
  [step args facts]
  (case step
    :harvest (decide-harvest args facts)
    :irrigate (decide-irrigate args facts)
    :till (decide-till args facts)
    :plant (decide-plant args facts)
    :fertilize (decide-fertilize args facts)
    {:skip :todo}))
