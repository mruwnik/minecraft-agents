(ns engine.core.cleanup
  "A job's cleanup (exports cleanup and cleanup-defaults; contract-conditions design 2.4): a second phase run once
  however a started frame ends, before its caller sees the ending. A frame whose ending is fixed closes: the frames
  below it whose job has a cleanup are cleaned first, deepest first (each with the ending :cut-off :ended), then the
  frame itself. The ending waits in the frame's :contract :closing; one cleanup round runs per round of the frame, so a
  cleanup may :continue. A cleaning frame's :call is its cleanup's (the frames outside it are not checked, see
  engine.core.limits/frames); the job's call comes back when the cleanup ends, its time added. A cleanup that does not
  end :done, overruns or throws leaves the ending as it is, with :cleanup {:kind :cause} in its data and a warn
  job.cleanup_failed; a done one's data merges into the ending's."
  (:require [engine.args :as a]
            [engine.contract :as contract]
            [engine.core.base :refer [cut-off! emit! now owner?]]
            [engine.core.limits :as limits]
            [engine.core.tally :as tally]
            [engine.memory :as mem]
            [engine.settings :as settings]))

(a/defargs settings
  {::job-time-s {:default 30 :spec (a/num-in 0 nil) :doc "The :spend :job-time of a cleanup whose job has no cleanup-defaults."}
   ::wall-time-s {:default 60 :spec (a/num-in 0 nil) :doc "The :spend :wall-time of a cleanup whose job has no cleanup-defaults."}})

(def default-defaults {:spend {:job-time ::job-time-s :wall-time ::wall-time-s}})

(defn frame [eng root slots] (mem/job-mem (mem/view (:store eng)) root slots))

(defn state-of [c] (:contract (frame (:engine c) (:root c) (:slots c))))

(defn closing? [eng root slots] (some? (get-in (frame eng root slots) [:contract :closing])))

(defn cleanup-def
  "The registered def of the job a frame's :contract state names, when it exports cleanup."
  [eng state]
  (let [d (get (:jobs eng) (:job state))]
    (when (:cleanup d) d)))

(defn pending
  "The slots, relative to frame m, of the started frames below it whose job has a cleanup, deepest first. Below a
  frame that is cleaning nothing counts: its children are its cleanup's."
  [eng m]
  (mapcat (fn [[slot child]]
            (let [s (:contract child)]
              (concat (when-not (:cleanup s) (map #(into [slot] %) (pending eng child)))
                      (when (and (:call s) (cleanup-def eng s)) [[slot]]))))
          (:children m)))

(defn owes?
  "Whether frame m of job def has a cleanup to run: one below it, or its own once started."
  [eng def m]
  (boolean (or (seq (pending eng m)) (and (:cleanup def) (get-in m [:contract :call])))))

(defn mark-closing!
  "Fix ending e as the frame's, to be handed on once its cleanups have run."
  [eng root slots e]
  (mem/update-job! (:store eng) root slots assoc-in [:contract :closing] e))

(defn cleanup-call [def args]
  (contract/freeze {:defaults (contract/resolve-settings (or (:cleanup-defaults def) default-defaults)
                                                         #(settings/get (merge settings (:settings def)) %) args)}
                   {} nil))

(defn begin!
  "Put the frame of c into its cleanup with ending e: the cleanup's call in place of the job's, counted from now (the
  part of this round run so far stays the job's)."
  [def c e]
  (let [eng (:engine c)
        t (now eng)
        pre (- t (nth (:t0s c) (count (:slots c)) t))]
    (mem/update-job! (:store eng) (:root c) (:slots c) update :contract
                     (fn [s] (-> (dissoc s :limit)
                                 (assoc :closing e
                                        :cleanup {:call (:call s) :started-ms (:started-ms s) :ran-ms (+ (or (:ran-ms s) 0) pre)}
                                        :call (cleanup-call def (:args c)) :started-ms t :ran-ms (- pre)))))))

(defn finish!
  "End the cleanup of the frame of c with ce (its ending, or the failure): the job's call back, the cleanup's time
  added; the frame's ending, with ce in it, stays in :closing (:cleaned) until step hands it on, so a cancel that cut
  this round cannot lose it."
  [c ce]
  (let [eng (:engine c)
        e (:closing (state-of c))
        ending (if (= :done (:kind ce))
                 (cond-> e (seq (:data ce)) (update :data merge (:data ce)))
                 (let [failure (select-keys ce [:kind :cause])]
                   (emit! eng {:source :job :kind :cleanup_failed :level :warn :job (:id c) :chain (:chain c) :round (:round c)
                               :reflex (:reflex c) :data failure
                               :text (str "cleanup ended " (name (:kind ce)) " (" (contract/cause-key ce) "); the job's ending stands")})
                   (assoc-in e [:data :cleanup] failure)))]
    (mem/update-job! (:store eng) (:root c) (:slots c) update :contract
                     (fn [{:keys [cleanup ran-ms] :as s}]
                       (-> (dissoc s :cleanup :limit)
                           (assoc :closing ending :cleaned true
                                  :call (:call cleanup) :started-ms (:started-ms cleanup)
                                  :ran-ms (+ (:ran-ms cleanup) (or ran-ms 0))))))))

(defn ^:async run-once
  "One round of def's cleanup in the frame of c: :continue, or what it ended with. Its own call bounds it; an overrun
  or a throw is its ending."
  [def c]
  (let [eng (:engine c)
        overrun #(when-let [l (contract/limit-of %)]
                   (when (= (:frame l) (:id c)) (contract/ending :limit-reached (:cause l) nil)))]
    (try
      (limits/check! eng c)
      (let [r (await ((:cleanup def) c (:closing (state-of c))))]
        (cond
          (= :continue r) (do (limits/check! eng c) :continue)
          (contract/ending? r) r
          :else (contract/ending :nothing-possible {:reason :bad-return :value (pr-str r)} nil)))
      (catch :default e
        (cond
          (overrun e) (overrun e)
          (contract/passes-through? e) (throw e)
          :else (contract/ending :nothing-possible {:reason :error :error (str e)} nil))))))

(defn drop-frame!
  "Remove the frame of c, cleaned on behalf of a frame above: it and the frames below it are cut off."
  [c]
  (let [eng (:engine c)
        slots (:slots c)]
    (cut-off! eng (:root c) (pop slots) {:children {(peek slots) (frame eng (:root c) slots)}} :ended)
    (mem/update-job! (:store eng) (:root c) (pop slots) update :children dissoc (peek slots))))

(defn ^:async step
  "One round of the closing frame of c (job def): the next cleanup owed, the next one taken in the same round when one
  ends. :continue while one continues, else the frame's ending. ctx-below: (fn [c rel-slots]) the ctx of a frame below."
  [eng def c ctx-below]
  (let [m (frame eng (:root c) (:slots c))
        rel (when-not (get-in m [:contract :cleanup]) (first (pending eng m)))]
    (cond
      rel
      (let [dc (ctx-below c rel)
            ddef (cleanup-def eng (state-of dc))
            _ (when-not (:cleanup (state-of dc)) (begin! ddef dc (contract/ending :cut-off :ended nil)))
            t0 (now eng)
            r (try (await (run-once ddef dc))
                   (finally (tally/add-time! eng (:root dc) (:slots dc) (- (now eng) t0))))]
        (if (= :continue r)
          :continue
          (do (finish! dc r)
              (drop-frame! dc)
              (await (step eng def c ctx-below)))))

      (and (:cleanup def) (get-in m [:contract :call]) (not (get-in m [:contract :cleaned])))
      (do (when-not (get-in m [:contract :cleanup]) (begin! def c (get-in m [:contract :closing])))
          (let [r (await (run-once def c))]
            (if (= :continue r) :continue (do (finish! c r) (await (step eng def c ctx-below))))))

      :else
      (let [e (get-in m [:contract :closing])]
        (when (owner? eng (:token c))
          (mem/update-job! (:store eng) (:root c) (:slots c) update :contract dissoc :closing :cleaned))
        e))))

(defn ^:async close
  "The frame of c (job def) has ended with e: e at once when no cleanup is owed, else its first closing round (step)."
  [eng def c e ctx-below]
  (if (owes? eng def (frame eng (:root c) (:slots c)))
    (do (mark-closing! eng (:root c) (:slots c) e)
        (await (step eng def c ctx-below)))
    e))

(defn next-tick
  "One physics step of wall clock: a macrotask, so packets and timers run (the world can change) between steps."
  [eng]
  (js/Promise. (fn [ok] (js/setTimeout ok (settings/physics-ms (:primitives eng))))))

(defn ^:async drain-on
  "After a step of the closing frame of c gave r: while r is :continue and the round holds the body, a tick, then the
  next step. For a frame whose caller cannot come back for the rest (a reflex root, the driver's slot job)."
  [eng def c r ctx-below]
  (if (and (= :continue r) (owner? eng (:token c)))
    (do (await (next-tick eng))
        (if (owner? eng (:token c)) (await (drain-on eng def c (await (step eng def c ctx-below)) ctx-below)) r))
    r))

(defn ^:async drain
  "Step the closing frame of c until its cleanups have run or the round is cut (drain-on)."
  [eng def c ctx-below]
  (await (drain-on eng def c (await (step eng def c ctx-below)) ctx-below)))

(defn ^:async close-now
  "close, run to its end within this round (drain-on)."
  [eng def c e ctx-below]
  (await (drain-on eng def c (await (close eng def c e ctx-below)) ctx-below)))

(defn ^:async run-out
  "For a frame of c (job def) whose caller does not come back: r, what its round gave, with its cleanups run to their
  end in this round. A frame leaving on :continue closes with :cut-off :ended and still gives :continue."
  [eng def c r ctx-below]
  (cond
    (closing? eng (:root c) (:slots c)) (await (drain-on eng def c r ctx-below))
    (contract/ending? r) r
    :else (do (await (close-now eng def c (contract/ending :cut-off :ended nil) ctx-below)) r)))
