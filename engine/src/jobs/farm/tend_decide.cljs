(ns jobs.farm.tend-decide
  "The steps of jobs.farm.tend, the child job of each, and the decision per step over the facts: skip it with a
  reason, or call its child.")

(def steps [:harvest :till :plant :fertilize :compost :deposit])

(def jobs
  {:harvest 'jobs.farm.harvest
   :till 'jobs.farm.till
   :plant 'jobs.farm.plant
   :fertilize 'jobs.farm.fertilize
   :compost 'jobs.farm.compost
   :deposit 'jobs.storage.deposit})

(defn decide-harvest
  [{:keys [plan part]} {:keys [ripe mid radius]}]
  (if (zero? ripe)
    {:skip :no-ripe}
    {:call {:slot :harvest :job (jobs :harvest)
            :args (if plan
                    {:plan plan :part part :replant true :replant-bare false}
                    {:center mid :radius radius :replant true})}}))

(defn decide-till
  [{:keys [till plan]} {:keys [hoe untilled bare seeds till-short]}]
  (cond
    (not till) {:skip :till-off}
    (not hoe) {:skip :no-hoe}
    (empty? untilled) {:skip (if (pos? (or till-short 0)) :no-seed :nothing-to-till)}
    (and (not plan) (<= seeds bare)) {:skip :no-seed}
    :else {:call {:slot :till :job (jobs :till)
                  :args (cond-> {:from (first untilled) :to (first untilled)} plan (assoc :for-plan plan))}}))

(defn decide-plant
  [{:keys [box plan part]} {:keys [bare seed]}]
  (cond
    (zero? bare) {:skip :no-bare}
    (not seed) {:skip :no-seed}
    :else {:call {:slot :plant :job (jobs :plant) :args (if plan {:plan plan :part part :fetch false} {:box box :fetch false})}}))

(defn decide-fertilize
  [{:keys [fertilize]} {:keys [meal unripe mid radius]}]
  (cond
    (not fertilize) {:skip :fertilize-off}
    (not meal) {:skip :no-bone-meal}
    (zero? unripe) {:skip :none-unripe}
    :else {:call {:slot :fertilize :job (jobs :fertilize) :args {:center mid :radius radius}}}))

(defn decide-compost
  [{:keys [composter]} {:keys [waste keep]}]
  (cond
    (nil? composter) {:skip :no-composter}
    (empty? waste) {:skip :no-surplus-seed}
    :else {:call {:slot :compost :job (jobs :compost) :args {:at composter :items waste :keep keep}}}))

(defn decide-deposit
  [{:keys [chest]} {:keys [stored keep]}]
  (cond
    (nil? chest) {:skip :no-chest}
    (empty? stored) {:skip :nothing-to-store}
    :else {:call {:slot :deposit :job (jobs :deposit) :args {:chest chest :items stored :keep keep}}}))

(defn decide
  "{:skip reason} or {:call {:slot :job :args}} for a step."
  [step args facts]
  (case step
    :harvest (decide-harvest args facts)
    :till (decide-till args facts)
    :plant (decide-plant args facts)
    :fertilize (decide-fertilize args facts)
    :compost (decide-compost args facts)
    :deposit (decide-deposit args facts)
    {:skip :todo}))
