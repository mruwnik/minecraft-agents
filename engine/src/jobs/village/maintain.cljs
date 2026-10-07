(ns jobs.village.maintain
  (:require [engine.args :as a]
            [clojure.string :as str]
            [engine.ctx :as ctx]
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
  - :import: bringing villagers in needs passenger transport, which does not exist yet; always skipped :no-transport.
  Villagers are those the body sees within :radius. Every call re-reads the villagers: a step the world no longer needs is skipped.
  A step whose child declines is booked {:skipped :declined :reason why} (the child's wait reason), one that
  throws {:skipped :failed :error text}; the pass goes on either way.
  The job declines (does nothing) unless some step would run. A started run always continues.
  It ends :done with {:villagers n :target :steps {step summary}} (info maintain.done, also when every step was
  skipped). A summary is the child's :status, :reason, :placed, :missing, :fed, :rolled, :born and :count. A child that
  ended stopped, declined or failed, or a role without :pos, makes the pass :stopped with :reason \"incomplete\" (warn maintain.done).
  The job names of breed and roll are used, not their internals: their args are as listed above.
  :ignore-zones? is passed to the repair.")

(a/defargs args
  {:plan {:doc "id of the village plan to repair (jobs.build.from-plan); nil: no repair" :spec a/name? :default nil}
   :target {:doc "villagers wanted (babies count); nil: no breeding" :spec (a/int-in 2 nil) :default nil}
   :roles {:doc "profession roles to hold: [{:profession name :pos workstation-cell :trade name?}], each rolled when nobody holds it" :spec (a/coll-of map?) :default []}
   :radius {:doc "villagers within this many blocks of the body count" :spec (a/int-in 1 96) :default 48}
   :ignore-zones? {:doc "act regardless of zones and claims (passed to the repair); the rules of the game allow it" :spec boolean? :default false}})

(def jobs
  {:repair 'jobs.build.from-plan
   :breed 'jobs.village.breed
   :roll 'jobs.village.roll})

(defn job-of
  "The job of a step slot (:roll-N is jobs.village.roll), nil for :import."
  [slot]
  (jobs (if (str/starts-with? (name slot) "roll-") :roll slot)))

(defn slots
  "The steps of a pass for these args, in order."
  [{:keys [roles]}]
  (vec (concat [:repair :breed]
               (map #(keyword (str "roll-" %)) (range (count roles)))
               [:import])))

(defn role-of [args slot]
  (nth (:roles args) (js/parseInt (subs (name slot) 5))))

;; ------------------------------------------------------------------ what is read

(defn facts
  "The villagers the body sees: {:villagers [{:baby :profession}]}."
  [c]
  (let [v (array-seq (.entities (:primitives c) #js {:radius (:radius (:args c)) :names #js ["villager"]}))]
    {:villagers (mapv (fn [e] {:baby (true? (.-baby e)) :profession (.-profession e)}) v)}))

;; ------------------------------------------------------------------ what is decided

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
      :import {:skip :no-transport}
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

(defn check [c]
  (or (boolean (or (:todo (ctx/mem c))
                   (let [f (facts c)]
                     (some #(let [d (decide % (:args c) f)] (or (:call d) (= :no-pos (:skip d)))) (slots (:args c))))))
      (ctx/wait c {:reason :nothing-to-do})))

;; ------------------------------------------------------------------ the run

(defn summary
  "What the report keeps of a child's result."
  [r]
  (select-keys r [:status :reason :placed :missing :fed :rolled :born :count]))

(defn incomplete?
  [entry]
  (or (= :stopped (:status entry)) (contains? #{:failed :declined :no-pos} (:skipped entry))))

(defn finish!
  [c report]
  (let [bad (filterv #(incomplete? (report %)) (slots (:args c)))
        out (cond-> {:villagers (count (:villagers (facts c))) :target (:target (:args c)) :steps report}
              (seq bad) (assoc :status :stopped :reason "incomplete"))
        text (if (seq bad)
               (str "maintain done, incomplete: "
                    (str/join ", " (map #(str (name %) (some->> (or (:reason (report %)) (when (= :no-pos (:skipped (report %))) "no-pos")) (str " "))) bad)))
               (str "maintain done: " (count (filter :skipped (vals report))) " steps skipped"))]
    (ctx/emit! c :maintain.done (if (seq bad) :warn :info) (assoc out :text text))
    (ctx/result! c out)
    :done))

(defn booked
  "Memory after the step's child ended: the step leaves todo, its summary goes in the report."
  [m slot entry]
  (-> m (update :todo rest) (dissoc :call-args) (assoc-in [:report slot] entry)))

(defn ^:async run-child!
  "One round of the child in slot. A child that throws is booked failed instead of failing the pass; a cut passes through."
  [c slot call-args]
  (try
    (await (ctx/call-child c slot (job-of slot) call-args))
    (catch :default e
      (when (= "cut" (.-code e)) (throw e))
      (ctx/update-mem! c booked slot {:skipped :failed :error (str e)})
      :failed)))

(defn declined-reason
  "Why the child of slot declines: the reason of its check's wait, nil when its check passes."
  [c slot call-args]
  (let [w (atom nil)]
    (ctx/check-child (assoc c :wait w) slot (job-of slot) call-args)
    (if (map? @w) (:reason @w) @w)))

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
  (let [m (ctx/mem c)
        {:keys [call] :as p} (resume c m)]
    (ctx/update-mem! c assoc :todo (:todo p) :report (:report p))
    (if-not call
      (finish! c (:report p))
      (let [slot (first (:todo p))
            _ (ctx/update-mem! c assoc :call-args call :call-slot slot)
            r (await (run-child! c slot call))]
        (case r
          :continue :continue
          :failed :again
          :done (do (ctx/update-mem! c booked slot (summary (ctx/child-result c slot))) :again)
          (do (ctx/update-mem! c booked slot {:skipped :declined :reason (declined-reason c slot call)}) :again))))))

(def max-steps "Steps of one call before it gives the round back with :continue." 400)

(defn ^:async round [c]
  (let [n (atom 0)]
    (await (pace/steps! c #(if (< (swap! n inc) max-steps) (step c) :continue)))))
