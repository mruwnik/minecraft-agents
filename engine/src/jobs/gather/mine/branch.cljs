(ns jobs.gather.mine.branch
  "Branch mining of a :stay-level strip tunnel (:branches): every ::branch-every blocks of the main tunnel the body cuts a side
  branch to each side, one jobs.gather.dig-for child per branch, and walks back to the junction between them."
  (:require [engine.ctx :as ctx]
            [engine.settings :as settings]
            [engine.contract :as contract]
            [jobs.gather.mine-stay :as stay]
            [jobs.gather.mine.end :refer [add-cut cut-of stop!]]
            [jobs.lib.waits :as waits]
            [jobs.gather.mine.targets :refer [step-cell work-s]]
            [jobs.lib.child :as child]
            [jobs.lib.look :refer [cell-of]]
            [jobs.lib.util :as u]))

(def halting "Branch endings that end the whole mine as they would in the main tunnel." #{:no-tool :no-torches :flooded})

(def sides {"north" ["west" "east"] "south" ["east" "west"] "east" ["north" "south"] "west" ["south" "north"]})

(defn every [] (settings/get stay/settings ::stay/branch-every))

(defn segment-limit
  "The most steps the next stretch of the main tunnel cuts: up to the next junction, or limit."
  [c steps limit]
  (if (:branches (:args c))
    (min limit (* (every) (inc (quot steps (every)))))
    limit))

(defn due?
  "Whether the main tunnel, at steps, stands at a junction whose branches are not cut yet."
  [c steps]
  (and (:branches (:args c)) (pos? steps) (zero? (mod steps (every))) (not (contains? (set (:branched (ctx/mem c))) steps))))

(defn begin!
  "Start the branches of the junction at steps (memory :branch)."
  [c {:keys [heading] :as tunnel}]
  (ctx/update-mem! c assoc :branch {:at (:steps tunnel) :todo (sides heading) :tunnel (select-keys tunnel [:origin :heading :steps])}))

(defn ^:async branch-round!
  "Cut the first branch left to do by a child run, then walk back to the junction; the junction is booked once every side is done.
  :continue while a child runs, else :again."
  [c]
  (let [{:keys [at todo tunnel]} (:branch (ctx/mem c))
        side (first todo)
        junction (step-cell tunnel at)
        args (-> (:args c)
                 (dissoc :direction :submitted-at :mend)
                 (assoc :heading side :tunnel-length (settings/get stay/settings ::stay/branch-length) :branches false :ways #{:seen :strip-tunnel} :track-ground false))
        r (or (:branch-end (ctx/mem c))
              (let [r (waits/waiting-out c (await (ctx/call c :branch 'jobs.gather.dig-for args)))]
                (when-not (= :continue r) (ctx/update-mem! c #(-> % (assoc :branch-end true) (update :cut add-cut (cut-of r)))))
                r))
        why (when (map? r) (contract/cause-key r))
        back (when-not (or (= :continue r) (halting why))
               (await (child/call-walk* c :branch-back {:pos {:x (+ (:x junction) 0.5) :y (:y junction) :z (+ (:z junction) 0.5)} :range 0 :stay-s (work-s c)
                                                        :ignore-zones? (boolean (:ignore-zones? (:args c)))})))]
    (cond
      (= :continue r) :continue
      (halting why) (stop! c why (select-keys (merge (:data r) (:cause r)) [:tool :why :text]))
      (= :continue back) :continue
      :else (do
              (ctx/update-mem! c #(let [m (-> % (dissoc :branch-end) (update-in [:branch :todo] rest))]
                                    (if (seq (get-in m [:branch :todo]))
                                      m
                                      (-> m (dissoc :branch) (update :branched (fnil conj []) at)))))
              (ctx/emit! c :mine.branch :info {:at (u/cell-vec junction) :heading side :text (str "mine cut a branch " side " from the tunnel at " at " blocks")})
              :again))))
