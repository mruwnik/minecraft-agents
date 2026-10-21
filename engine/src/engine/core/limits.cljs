(ns engine.core.limits
  "A call's limits (engine.contract) as the engine checks them: before every act and at each ctx/call and root round
  boundary, over the started contract frames of the chain. The outermost crossed frame is the one a crossing ends: it
  is tagged (its :contract :limit), job.limit_reached tells the line and its amount, and a limit error unwinds to its
  call boundary, which ends it :limit-reached; on the way frames may act unchecked but not call (frames further out
  are still checked), and a crossing inside ctx/atomic is held until the section closes. Before an act, its most
  (engine.primitive) is checked against what each frame has left: a crossing refuses the act at once."
  (:require [engine.contract :as contract]
            [engine.core.base :refer [emit! now]]
            [engine.expr :as expr]
            [engine.memory :as mem]
            [engine.primitive :as primitive]))

(defn levels
  "The body's {:health :food}."
  [p]
  (let [s (.self p)] {:health (.-health s) :food (.-food s)}))

(defn frames
  "[slots state] of each started contract frame from root down to slots, from the innermost one in its cleanup
  (engine.core.cleanup) on when there is one: a cleanup's own call bounds it, not its callers'."
  [eng root slots]
  (let [m (mem/job-mem (mem/view (:store eng)) root [])]
    (when (seq m)
      (let [fs (vec (keep (fn [i]
                            (let [s (subvec slots 0 i)
                                  state (:contract (get-in m (mem/slot-path s)))]
                              (when (:call state) [s state])))
                          (range (inc (count slots)))))
            i (last (keep-indexed #(when (:cleanup (second %2)) %1) fs))]
        (cond->> fs i (drop i))))))

(defn crossings
  "{:slots :cause :amount :state :now :extra-ms} of each line frames cross, outermost frame first (engine.contract/crossings).
  t0s: when each frame's round in flight started."
  [eng frames t0s]
  (let [lv (levels (:primitives eng))
        t (now eng)]
    (for [[s state] frames
          :let [extra-ms (- t (nth t0s (count s) t))]
          [cause amount] (contract/crossings state lv t extra-ms)]
      {:slots s :cause cause :amount amount :state state :now t :extra-ms extra-ms})))

(defn chain-of [root slots] (mapv #(mem/path->id root (subvec slots 0 %)) (range (inc (count slots)))))

(defn tag!
  "Tag the frame the crossing x ends, tell job.limit_reached (the line, its amount and the frame's report); the limit
  error that ends it."
  [eng root {:keys [slots cause amount state extra-ms] t :now}]
  (let [id (mem/path->id root slots)]
    (mem/update-job! (:store eng) root slots assoc-in [:contract :limit] {:frame id :cause cause})
    (emit! eng (merge {:source :job :kind :limit_reached :level :info :job id :chain (chain-of root slots)
                       :name (or (:job state)
                                 (when (empty? slots) (some-> (get-in @(:state eng) [:instances root :spec]) expr/label)))
                       :cause cause :call (:call state) :report (contract/report state t extra-ms)}
                      amount))
    (contract/limit-error id cause)))

(defn refuse!
  "Throw the limit error of the frame of base's chain that is unwinding: no new call in an unwinding chain."
  [eng {:keys [root slots]}]
  (when-let [{:keys [frame cause]} (some (comp :limit second) (frames eng root slots))]
    (throw (contract/limit-error frame cause))))

(defn outside-unwinding
  "The frames of fs (root first) outside the outermost one unwinding (tagged): an unwinding frame and those within it
  may act; a frame further out still holds its lines."
  [fs]
  (take-while (comp not :limit second) fs))

(defn check-frames!
  "check! over fs, the chain's started frames."
  [eng {:keys [root t0s section-depth held]} fs]
  (when-let [x (first (crossings eng (outside-unwinding fs) t0s))]
    (if (and section-depth (pos? @section-depth))
      (swap! held #(or % x))
      (throw (tag! eng root x))))
  nil)

(defn check!
  "Check the chain of base ({:root :slots :t0s :section-depth :held}) against its limits (see the ns doc). Throws the
  limit error that ends a frame; returns nil."
  [eng {:keys [root slots] :as base}]
  (check-frames! eng base (frames eng root slots)))

(defn refusals
  "As crossings, each line of frames act k with args could cross before it can be stopped (its engine.primitive quote's
  most, engine.contract/most-crossings), the cause naming the act."
  [eng frames t0s k args]
  (when-let [quote-of (get-in primitive/table [k :quote-of])]
    (let [p (:primitives eng)
          q (quote-of p (js->clj args :keywordize-keys true))
          lv (levels p)
          t (now eng)]
      (for [[s state] frames
            :let [extra-ms (- t (nth t0s (count s) t))]
            [cause amount] (contract/most-crossings state q lv t extra-ms)]
        {:slots s :cause (assoc cause :act k) :amount amount :state state :now t :extra-ms extra-ms}))))

(defn check-act!
  "check! before act k with args, then the act's most against what every frame has left. A crossing refuses the act,
  inside ctx/atomic too, and ends the outermost frame it crosses; frames unwinding are not checked (cleanup acts)."
  [eng {:keys [root slots t0s] :as base} k args]
  (let [fs (frames eng root slots)]
    (check-frames! eng base fs)
    (when-let [x (first (refusals eng (outside-unwinding fs) t0s k args))]
      (throw (tag! eng root x)))))

(defn section-closed!
  "After a ctx/atomic section of base's round closed: throw the crossing it held, once no section is open. A crossing
  of a frame inside the section (a child call that has returned) is dropped: no call boundary is left to end."
  [eng {:keys [root slots section-depth held]}]
  (when (and held (zero? @section-depth))
    (when-let [x @held]
      (reset! held nil)
      (when (= (:slots x) (subvec slots 0 (min (count slots) (count (:slots x)))))
        (throw (tag! eng root x))))))

(defn ending
  "The :limit-reached ending of the frame at slots of root when limit error e ends it (no e: when it is tagged), else
  nil."
  [eng root slots e]
  (let [id (mem/path->id root slots)
        l (if e (contract/limit-of e) (get-in (mem/job-mem (mem/view (:store eng)) root slots) [:contract :limit]))]
    (when (= id (:frame l))
      (contract/ending :limit-reached (:cause l) nil))))

(defn floor-cause
  "Why the call of contract ctx c, (call-fn), may not start or go on now: {:floor :min}."
  [c call-fn]
  (contract/below-floor (call-fn) (levels (:primitives c))))

(defn floor-wait "floor-cause {:floor line :min f} as a wait reason {:reason line :cause}." [cause]
  {:reason (:floor cause) :cause cause})
