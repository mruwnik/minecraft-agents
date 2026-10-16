(ns jobs.village.maintain
  (:require [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [engine.settings :as settings]
            [engine.args :as a]
            [clojure.string :as str]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]))

(def doc
  "Keep one village in order. One call is one pass over its steps in a fixed order. Each is a child job, called until
  it ends and never run twice in a pass (:continue only while a child waits on the world):
  - :repair (jobs.build.from-plan, :plan): the village plan is built again where it is missing or broken. The plan's
    huts and beds are the housing, so this is the house step too. Skipped :no-plan without a :plan.
  - :breed (jobs.village.breed, :target and :radius): villagers (babies count) are below :target and 2 adults are
    near. Skipped :no-target, :at-target, :too-few-adults.
  - :roll-N (jobs.village.roll, one per entry of :roles, with its :profession, :trade, :pos and :radius): no adult villager
    near holds the role's profession, so one is rolled to it (the lock). :pos is the workstation cell. Skipped
    :filled when a villager holds the profession, :no-villager when none is near, :no-pos without one (the pass
    ends stopped, the role cannot be rolled). A roll
    already begun goes on while a villager is near.
  Villagers are those the body sees within :radius. Every call re-reads the villagers: a step the world no longer needs is skipped.
  A step whose child declines is booked {:skipped :declined :reason why} (the child's wait reason); the pass goes on.
  A child that throws fails the pass (parked failed, like any job whose round throws).
  A village where no step would run is in order: done at once (:already). A started run always continues.
  It ends :done with {:villagers n :target :steps {step summary}} when some step ran to its end
  and none was incomplete. A summary is the child's :reason, :placed, :missing, :fed, :rolled, :born and :count (a
  child that ended other than :done carries :stopped kind). A child that ended other than :done, or declined,
  or a role without :pos, ends the pass nothing-possible :incomplete; a started pass where, by its end, no step ran
  (the world changed under it) ends condition-failed :nothing-to-do; a child that reached a limit ends the pass limit-reached with it.
  The job names of breed and roll are used, not their internals: their args are as listed above.
  :ignore-zones? is passed to the repair.")

(a/defargs args
  {:plan {:doc "id of the village plan to repair (jobs.build.from-plan); nil: no repair" :spec a/name? :default nil}
   :target {:doc "villagers wanted (babies count); nil: no breeding" :spec (a/int-in 2 nil) :default nil}
   :roles {:doc "profession roles to hold: [{:profession name :pos workstation-cell :trade name?}], each rolled when nobody holds it" :spec (a/coll-of map?) :default []}
   :radius {:doc "villagers within this many blocks of the body count" :spec (a/int-in 1 96) :default 48}
   :ignore-zones? :shared})

(def jobs
  {:repair 'jobs.build.from-plan
   :breed 'jobs.village.breed
   :roll 'jobs.village.roll})

(defn job-of
  "The job of a step slot (:roll-N is jobs.village.roll)."
  [slot]
  (jobs (if (str/starts-with? (name slot) "roll-") :roll slot)))

(defn slots
  "The steps of a pass for these args, in order."
  [{:keys [roles]}]
  (vec (concat [:repair :breed]
               (map #(keyword (str "roll-" %)) (range (count roles))))))

(defn role-of [args slot]
  (nth (:roles args) (js/parseInt (subs (name slot) 5))))

;; ------------------------------------------------------------------ what is read

(defn facts
  "The villagers the body sees: {:villagers [{:baby :profession}]}."
  [c]
  (let [v (array-seq (.entities (:primitives c) #js {:radius (:radius (:args c)) :names #js ["villager"]}))]
    {:villagers (mapv (fn [e] {:baby (true? (.-baby e)) :profession (.-profession e)}) v)}))

;; ------------------------------------------------------------------ what is decided

(defn unlooked?
  "A role or a target needs villagers, none is in view and no look round yet: villagers count only while in view."
  [c]
  (and (or (seq (:roles (:args c))) (:target (:args c)))
       (empty? (:villagers (facts c)))
       (not (look/surveyed? c))))

(defn decide
  "{:skip reason} or {:call args} for a step slot."
  [slot {:keys [plan target radius ignore-zones?] :as args} {:keys [villagers]}]
  (let [adults (remove :baby villagers)]
    (case slot
      :repair (if plan
                {:call (cond-> {:plan plan} ignore-zones? (assoc :ignore-zones? true))}
                {:skip :no-plan})
      :breed (cond
               (nil? target) {:skip :no-target}
               (>= (count villagers) target) {:skip :at-target}
               (< (count adults) 2) {:skip :too-few-adults}
               :else {:call {:target target :radius radius}})
      (let [{:keys [profession trade pos]} (role-of args slot)]
        (cond
          (empty? villagers) {:skip :no-villager}
          (some #(= profession (:profession %)) adults) {:skip :filled}
          (nil? pos) {:skip :no-pos}
          :else {:call (cond-> {:profession profession :pos pos :radius radius} trade (assoc :trade trade))})))))

(defn plan
  "Walk todo, booking the steps that decide skips in report; {:todo :report :call}, :call nil when none is left."
  [todo args facts report]
  (loop [todo todo report report]
    (if (empty? todo)
      {:todo [] :report report :call nil}
      (let [{:keys [skip call]} (decide (first todo) args facts)]
        (if skip
          (recur (rest todo) (assoc report (first todo) {:skipped skip}))
          {:todo (vec todo) :report report :call call})))))

;; ------------------------------------------------------------------ the run

(defn summary
  "What the report keeps of a child's result."
  [r]
  (select-keys r [:reason :placed :missing :fed :rolled :born :count]))

(defn ran?
  "Whether a step's report entry is one that ran to its end."
  [entry]
  (not (or (:skipped entry) (:stopped entry))))

(defn incomplete?
  [entry]
  (or (some? (:stopped entry)) (contains? #{:declined :no-pos} (:skipped entry))))

(defn finish!
  "End the pass: nothing-possible :incomplete when a step was incomplete, done when some step ran, else
  condition-failed :nothing-to-do."
  [c report]
  (let [bad (filterv #(incomplete? (report %)) (slots (:args c)))
        out {:villagers (count (:villagers (facts c))) :target (:target (:args c)) :steps report}
        text (if (seq bad)
               (str "maintain done, incomplete: "
                    (str/join ", " (map #(str (name %) (some->> (or (some-> (:reason (report %)) name) (when (= :no-pos (:skipped (report %))) "no-pos")) (str " "))) bad)))
               (str "maintain done: " (count (filter :skipped (vals report))) " steps skipped"))]
    (cond
      (seq bad) (ctx/end c :nothing-possible :incomplete (assoc out :text text))
      (some ran? (vals report)) (ctx/end c :done nil (assoc out :text text))
      :else (ctx/end c :condition-failed {:reason :nothing-to-do} out))))

(defn booked
  "Memory after the step's child ended: the step leaves todo, its summary goes in the report."
  [m slot entry]
  (-> m (update :todo rest) (dissoc :call-args) (assoc-in [:report slot] entry)))

(defn entry-of
  "The report entry of a child's ending e: its summary; a child that did not start (no data) is {:skipped :declined
  :reason cause}, one that stopped carries :stopped kind."
  [e]
  (cond
    (= :done (:kind e)) (summary (:data e))
    (empty? (:data e)) {:skipped :declined :reason (contract/cause-key e)}
    :else (assoc (summary (merge {:reason (contract/cause-key e)} (:data e))) :stopped (:kind e))))

(defn roll? [slot] (str/starts-with? (name slot) "roll-"))

(defn resume
  "The step to go on with: a roll begun goes on while its villager holds the profession (its saved call); any other
  step is decided again from what the body sees."
  [c m]
  (let [saved (:call-args m)
        f (facts c)
        fresh (plan (:todo m) (:args c) f (:report m))]
    (if (and saved (= (:call-slot m) (first (:todo m))) (roll? (first (:todo m))) (seq (:villagers f)))
      (assoc fresh :todo (:todo m) :call (or (:call fresh) saved))
      fresh)))

(defn ^:async step
  "One piece of the pass: :again after a step ended, :continue while its child waits on the world, :done."
  [c]
  (when-not (:todo (ctx/mem c))
    (ctx/update-mem! c assoc :todo (slots (:args c)) :report {}))
  (if (unlooked? c)
    (await (look/survey! c))
  (let [m (ctx/mem c)
        {:keys [call] :as p} (resume c m)]
    (ctx/update-mem! c assoc :todo (:todo p) :report (:report p))
    (if-not call
      (finish! c (:report p))
      (let [slot (first (:todo p))
            _ (ctx/update-mem! c assoc :call-args call :call-slot slot)
            r (await (ctx/call c slot (job-of slot) call))]
        (case r
          :continue :continue
          (do (ctx/update-mem! c booked slot (entry-of r))
              (if (= :limit-reached (:kind r))
                (ctx/end c :limit-reached (:cause r) {:steps (:report (ctx/mem c))})
                :again))))))))

(a/defargs settings
  {::max-steps {:default 400 :doc "Steps of one call before it gives the round back with :continue." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 1800 :doc "Seconds a maintain pass may take: all its steps." :spec (a/num-in 0 nil)}})
(defn max-steps [] (settings/get settings ::max-steps))

(defn ^:async round [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) (max-steps)) (step c) :continue)))))

;; ------------------------------------------------------------------ the contract

(def target [:plan :roles])

(def defaults {:spend {:job-time ::job-time-s}})

(def safe? (ready/safe-at-by-night work-place/spot))
(def fit? ready/fit?)

(defn done?
  "Done once no step is left to run (decided from the facts, as each step is), none is incomplete and no child is under
  way; the data is the pass's report."
  [c]
  (let [m (ctx/mem c)
        {:keys [call report todo]} (plan (or (:todo m) (slots (:args c))) (:args c) (facts c) (:report m {}))
        ran (count (filter ran? (vals report)))
        data [:units :steps :done ran :left (count todo)]]
    (if (or call (:call-args m) (some incomplete? (vals report)) (unlooked? c))
      (apply contract/later :steps (str ran " steps run") data)
      (apply contract/ok :in-order (str "the village is in order: " (count (filter :skipped (vals report))) " steps skipped")
             (conj data :result {:villagers (count (:villagers (facts c))) :target (:target (:args c)) :steps report})))))

(defn quote-of [_c]
  {:uses {:job-time [1 60 (settings/get settings ::job-time-s)]}})
