(ns jobs.lib.trip
  "Trips start with room: a gathering job that leaves home calls ready! first. See ready!."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.memory :as mem]
            [engine.settings :as settings]
            [jobs.lib.cost :as cost]
            [jobs.lib.util :as u]))

(a/defargs settings
  {::want-free {:default 12 :doc "Free slots a trip wants at its start: with fewer free and a known chest near, the body deposits first." :spec (a/int-in 0 36)}
   ::max-cost {:default 30 :doc "Walk cost (jobs.lib.cost/walk-cost, about 100 blocks) up to which a known chest is near enough to deposit at before a trip." :spec (a/num-in 0 nil)}})

(defn want-free [] (settings/get settings ::want-free))
(defn max-cost [] (settings/get settings ::max-cost))

(def child :trip-deposit)

(defn done! [c outcome fields]
  (ctx/update-mem! c assoc :trip outcome)
  (ctx/emit! c (keyword (str "trip." (name outcome))) :info fields))

(defn plan
  "What a trip start does: :off, :roomy, :no-chest, :far or :deposit, with the chest and its walk cost."
  [c]
  (let [chest (mem/place (ctx/view c) :chest)
        free (u/free-slots (:primitives c))
        cost (when chest (:cost (cost/walk-cost {:distance (u/dist (u/self-pos c) chest)})))]
    (cond
      (false? (:deposit-first? (:args c))) {:do :off}
      (>= free (want-free)) {:do :roomy :free free}
      (nil? chest) {:do :no-chest :free free}
      (> cost (max-cost)) {:do :far :free free :cost cost}
      :else {:do :deposit :chest chest :free free :cost cost})))

(defn ^:async ready!
  "Before a gathering job sets out: when fewer than ::want-free slots are free and the known :chest place is within
  ::max-cost of walking, run jobs.storage.deposit as a child (:free ::want-free; it keeps tools, armour and the food
  reserve). Resolves nil to go on, :continue while the child waits on the world. Done once per job (:trip in the job's
  memory), so a restart does not repeat it. No chest known or a far one: info trip.no-chest or trip.far and the trip
  goes on. The job's :deposit-first? false skips it. A deposit that stops (its own warn says why) does not stop the trip."
  [c]
  (if (:trip (ctx/mem c))
    nil
    (let [p (plan c)]
      (case (:do p)
        :off (do (ctx/update-mem! c assoc :trip :off) nil)
        :roomy (do (ctx/update-mem! c assoc :trip :roomy) nil)
        (:no-chest :far) (do (done! c (case (:do p) :no-chest :no-chest :far) (select-keys p [:free :cost])) nil)
        :deposit
        (let [r (await (ctx/call-child c child 'jobs.storage.deposit {:free (want-free)}))]
          (if (= :continue r)
            :continue
            (do (done! c :deposited (assoc (select-keys p [:free :cost]) :result (ctx/child-result c child))) nil)))))))
