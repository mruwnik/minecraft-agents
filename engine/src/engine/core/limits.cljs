(ns engine.core.limits
  "A call's limits (engine.contract) as the engine checks them: before every act and at each ctx/call and root round
  boundary, over the started contract frames of the chain. The outermost crossed frame is the one a crossing ends.
  With :engine.contract/enforce-limits on it is tagged (its :contract :limit) and a limit error unwinds to its call
  boundary, which ends it :limit-reached; on the way frames may act but not call, and a crossing inside ctx/atomic is
  held until the section closes. Off, job.limit_would_end is told once per crossed frame and line, and the job goes on."
  (:require [engine.contract :as contract]
            [engine.core.base :refer [emit! now]]
            [engine.expr :as expr]
            [engine.memory :as mem]
            [engine.settings :as settings]))

(defn enforce? [] (settings/get contract/settings ::contract/enforce-limits))

(defn levels
  "The body's {:health :food}."
  [p]
  (let [s (.self p)] {:health (.-health s) :food (.-food s)}))

(defn frames
  "[slots state] of each started contract frame from root down to slots."
  [eng root slots]
  (let [m (mem/job-mem (mem/view (:store eng)) root [])]
    (when (seq m)
      (keep (fn [i]
              (let [s (subvec slots 0 i)
                    state (:contract (get-in m (mem/slot-path s)))]
                (when (:call state) [s state])))
            (range (inc (count slots)))))))

(defn crossings
  "{:slots :cause :amount :state :extra-ms} of each line frames cross, outermost frame first (engine.contract/crossings).
  t0s: when each frame's round in flight started."
  [eng frames t0s]
  (let [lv (levels (:primitives eng))
        t (now eng)]
    (for [[s state] frames
          :let [extra-ms (- t (nth t0s (count s) t))]
          [cause amount] (contract/crossings state lv t extra-ms)]
      {:slots s :cause cause :amount amount :state state :extra-ms extra-ms})))

(defn tag!
  "Tag the frame at slots of root with cause; the limit error that ends it."
  [eng root {:keys [slots cause]}]
  (let [id (mem/path->id root slots)]
    (mem/update-job! (:store eng) root slots assoc-in [:contract :limit] {:frame id :cause cause})
    (contract/limit-error id cause)))

(defn chain-of [root slots] (mapv #(mem/path->id root (subvec slots 0 %)) (range (inc (count slots)))))

(defn would-end!
  "Tell job.limit_would_end for the crossing x, once per frame and line."
  [eng root {:keys [slots cause amount state extra-ms]}]
  (when-not (contains? (:would-end state) cause)
    (mem/update-job! (:store eng) root slots update-in [:contract :would-end] (fnil conj #{}) cause)
    (emit! eng (merge {:source :job :kind :limit_would_end :level :info :job (mem/path->id root slots)
                       :chain (chain-of root slots)
                       :name (or (:job state)
                                 (when (empty? slots) (some-> (get-in @(:state eng) [:instances root :spec]) expr/label)))
                       :cause cause :call (:call state) :report (contract/report state extra-ms)}
                      amount))))

(defn refuse!
  "Throw the limit error of the frame of base's chain that is unwinding, when enforced: no new call in an unwinding
  chain."
  [eng {:keys [root slots]}]
  (when (enforce?)
    (when-let [{:keys [frame cause]} (some (comp :limit second) (frames eng root slots))]
      (throw (contract/limit-error frame cause)))))

(defn check!
  "Check the chain of base ({:root :slots :t0s :section-depth :held}) against its limits (see the ns doc). Throws the
  limit error that ends a frame; returns nil."
  [eng {:keys [root slots t0s section-depth held]}]
  (let [fs (frames eng root slots)]
    (cond
      (empty? fs) nil
      (not (enforce?)) (run! #(would-end! eng root %) (crossings eng fs t0s))
      (some (comp :limit second) fs) nil
      :else (when-let [x (first (crossings eng fs t0s))]
              (if (and section-depth (pos? @section-depth))
                (swap! held #(or % x))
                (throw (tag! eng root x)))))
    nil))

(defn section-closed!
  "After a ctx/atomic section of base's round closed: throw the crossing it held, once no section is open."
  [eng {:keys [root section-depth held]}]
  (when (and held (zero? @section-depth))
    (when-let [x @held]
      (reset! held nil)
      (throw (tag! eng root x)))))

(defn ending
  "The :limit-reached ending of the frame at slots of root when limit error e ends it (no e: when it is tagged), else
  nil."
  [eng root slots e]
  (let [id (mem/path->id root slots)
        l (if e (contract/limit-of e) (get-in (mem/job-mem (mem/view (:store eng)) root slots) [:contract :limit]))]
    (when (= id (:frame l))
      (contract/ending :limit-reached (:cause l) nil))))

(defn floor-cause
  "Why the call of contract ctx c, (call-fn), may not start or go on now, when enforced: {:floor :min}."
  [c call-fn]
  (when (enforce?) (contract/below-floor (call-fn) (levels (:primitives c)))))
