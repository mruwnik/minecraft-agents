(ns engine.primitive
  "The acting primitives as the job contract sees them (docs/goals-and-actions.md): each is one act, which cannot be
  stopped midway. An entry is {:args [arg keys] :target [arg keys] :quote-of (fn [p args] quote)
  :ending (fn [result] ending)}: p the primitives, args the act's args (keyword keys), result the primitive's JS result. engine.core.limits checks a
  quote's most against what every frame has left before the act runs, so a most is finite and never short of the
  real cost (moveTo's drops excepted, see fall): time is the primitive's own bound (js/prim-*.mjs). jobs.primitive wraps each entry as a registry job."
  (:require [engine.contract :as contract]
            [engine.settings :as settings]))

(defn js-setting
  "The live value of js/settings.mjs key k, else fallback (no JavaScript settings loaded, as in tests)."
  [k fallback]
  (if-some [v ((settings/js-reader) k)] v fallback))

(defn upto "A figure of 0 up to most, mean a guess." [mean most] [0 (min mean most) most])

(defn timed "A quote of time only, bounded by most s." [most] {:uses {:job-time (upto 1 most)}})

(defn use-items
  "q using up to n of item (least 0: an act that fails uses none); unchanged without an item."
  [q item n]
  (cond-> q (and (string? item) (pos? n)) (assoc-in [:uses :items item] [0 n n])))

(defn held [p] (let [h (.-held (.self p))] (if (string? h) h (some-> h .-name))))

(defn carried [p item]
  (reduce + 0 (keep #(when (= item (.-name %)) (.-count %)) (.-inventory (.self p)))))

(def own-max-drop "The pathfinder's own drop cap in blocks fallen, the cap of a walk that sets none." 3)

(defn fall
  "The health the drops of a walk from where the body stands to pos could cost, each drop at most max-drop blocks and
  1 hp per block past 3. Every drop and every 1-block climb takes a step, so a path along the straight line holds at
  most (steps + descent) / (max-drop + 1) full drops; a path that strays further is not foreseen."
  [p pos max-drop]
  (let [from (some-> (.self p) .-pos)
        hurt (- max-drop 3)]
    (if (and (pos? hurt) from (every? number? [(:x pos) (:y pos) (:z pos)]))
      (let [cell #(js/Math.floor %)
            steps (js/Math.ceil (js/Math.hypot (- (cell (:x pos)) (cell (.-x from))) (- (cell (:z pos)) (cell (.-z from)))))
            descent (- (cell (.-y from)) (cell (:y pos)))]
        (* hurt (max 0 (quot (+ steps descent) (inc max-drop)))))
      0)))

(def any-act-fails
  "The failure statuses every act can return, with their kinds."
  {"refused-mounted" :condition-failed "unsupported" :impossible "error" :nothing-possible})

(defn ending-of
  "The ending fn of a primitive whose done statuses are done and whose own failures are fails ({status kind}): timeout is
  the time line, a cut or a lost connection cuts off, failed is nothing possible, any other status has the kind fails or
  any-act-fails gives it, else nothing possible."
  [done fails]
  (fn [r]
    (let [s (some-> r .-status)
          k (keyword s)]
      (cond
        (contains? done s) (contract/ending :done k nil)
        (= "timeout" s) (contract/ending :limit-reached {:line :job-time} nil)
        (#{"cut" "offline" "closed" "disconnected"} s) (contract/ending :cut-off k nil)
        (= "failed" s) (contract/ending :nothing-possible :failed nil)
        :else (contract/ending (get fails s (get any-act-fails s :nothing-possible)) (or k :no-status) nil)))))

(def spawn-s "connect.mjs SPAWN_TIMEOUT_MS: how long one connect waits for the spawn." 60)

(defn reconnect-s
  "The most a reconnect after offline takes (primitives.mjs reconnectBot): each try a spawn and a world load, a retry
  wait between tries."
  []
  (let [tries (js-setting "prim/reconnect-tries" 3)]
    (+ (* tries (+ spawn-s (/ (js-setting "prim/world-timeout-ms" 10000) 1000)))
       (* (dec tries) (/ (js-setting "prim/reconnect-retry-ms" 5000) 1000)))))

(defn use-on-s
  "The most a useOn takes (use-on.mjs): a second for the click, then its waits for the block change and the count."
  []
  (+ 1 (/ (+ (js-setting "use-on/wait-ms" 1000) (js-setting "use-on/consume-wait-ms" 300)) 1000)))

(defn fails
  "The failure statuses of a primitive as {status kind}: condition-failed ones name a false precondition, nothing-possible
  ones an act that ran and did not reach its end. Statuses every act can return are in any-act-fails."
  [condition-failed nothing-possible]
  (merge (zipmap condition-failed (repeat :condition-failed)) (zipmap nothing-possible (repeat :nothing-possible))))

(defn entry [args target done fails quote-of]
  {:args args :target target :quote-of quote-of :fails fails :ending (ending-of done fails)})

(defn fixed [args target done fails most] (entry args target done fails (fn [_ _] (timed most))))

(def table
  {:moveTo (entry [:pos :range :timeoutS :maxDistance :maxDropDown] [:pos] #{"arrived"} (fails #{} #{"unreachable" "blocked" "partial" "out-of-reach"})
                  (fn [p {:keys [pos timeoutS maxDropDown]}]
                    (assoc-in (timed (min (or timeoutS 20) 60)) [:uses :health]
                              [0 0 (fall p pos (or maxDropDown own-max-drop))])))
   :dig (entry [:pos :timeoutS] [:pos] #{"dug"} (fails #{"missing" "gone"} #{"cannot" "unreachable"})
               (fn [p {:keys [pos timeoutS]}]
                 (let [ms (or (when pos (.digTime p (clj->js pos))) 0)
                       bound (+ (/ (if (js/Number.isFinite ms) ms 0) 1000)
                                (js-setting "prim/dig-margin-s" 5) (js-setting "prim/drop-wait-s" 1))
                       tool (held p)]
                   (cond-> (timed (min bound (or timeoutS ##Inf)))
                     tool (assoc-in [:uses :wear tool] [0 1 1])))))
   :place (entry [:pos :item :click] [:pos] #{"placed"} (fails #{"missing" "occupied" "no-item" "no-support"} #{"cannot" "unreachable"}) (fn [_ {:keys [item]}] (use-items (timed 5) item 1)))
   :jumpPlace (entry [:item :count] [] #{"done"} (fails #{"no-item" "no-support" "no-headroom"} #{"not-raised" "partial"})
                     (fn [_ {:keys [item count]}]
                       (let [n (min (or count 1) (js-setting "prim/jump-place-max" 8))]
                         (use-items (timed (* n (js-setting "prim/jump-place-block-s" 2))) item n))))
   :collect (entry [:id :timeoutS :maxDropDown] [:id] #{"collected"} (fails #{"gone"} #{"unreachable"}) (fn [_ {:keys [timeoutS]}] (timed (min (or timeoutS 10) 20))))
   :inspectContainer (fixed [:pos :entity] [:pos] #{"ok"} (fails #{"missing"} #{"unreachable" "out-of-reach"}) 5)
   :transfer (entry [:pos :entity :direction :item :count] [:pos :item] #{"ok"} (fails #{"missing" "no-item" "full"} #{"cannot" "unreachable" "out-of-reach"})
                    (fn [_ {:keys [direction item count]}]
                      (let [q (timed 5)]
                        (cond
                          (not (number? count)) q
                          (= "deposit" direction) (use-items q item count)
                          (string? item) (assoc-in q [:yields :items item] [0 count count])
                          :else q))))
   :equip (fixed [:item :dest] [:item] #{"equipped"} (fails #{"no-item"} #{}) 2)
   :toss (entry [:item :count :slot :watchS] [:item] #{"tossed"} (fails #{"no-item"} #{})
                (fn [p {:keys [item count watchS]}]
                  (use-items (timed (+ 2 (min (or watchS 0) 5))) item (or count (carried p item)))))
   :craft (entry [:item :count :table] [:item] #{"crafted"} (fails #{"no-item" "full"} #{"cannot" "partial" "unreachable" "out-of-reach"}) (fn [_ {:keys [count]}] (timed (min (js-setting "prim/craft-max-s" 60) (+ 4 (* 6 (or count 1)))))))
   :furnace (fixed [:pos :op :input :fuel :output] [:pos] #{"ok"} (fails #{"missing" "no-item" "busy"} #{"cannot" "rejected" "unreachable"}) 5)
   :enchant (fixed [:pos :op :item :choice :levelCost] [:pos] #{"ok" "enchanted"} (fails #{"missing" "no-item" "no-lapis" "no-levels" "full"} #{"cannot" "unreachable"}) 15)
   :chat (fixed [:message :to] [] #{"sent"} (fails #{"gone"} #{}) 3)
   :eat (entry [:item] [:item] #{"ate"} (fails #{"no-item" "no-food"} #{}) (fn [_ {:keys [item]}] (use-items (timed 5) item 1)))
   :fish (entry [:pos :timeoutS] [:pos] #{"caught"} (fails #{"missing" "no-rod" "no-water"} #{"nothing" "unreachable"})
                (fn [_ {:keys [timeoutS]}] (timed (+ 5 (or timeoutS (js-setting "prim/fish-wait-s" 45))))))
   :attack (fixed [:id] [:id] #{"hit" "killed"} (fails #{"gone"} #{"out-of-reach"}) 1)
   :interact (entry [:id :item] [:id] #{"used"} (fails #{"gone" "no-item" "full"} #{"cannot" "out-of-reach" "no-effect"}) (fn [_ {:keys [item]}] (use-items (timed 2) item 1)))
   :trade (fixed [:id :op :offer :times] [:id] #{"bought" "ok"} (fails #{"gone" "no-item" "full"} #{"cannot" "out-of-reach"}) 8)
   :unequip (fixed [:dest] [] #{"ok" "empty"} (fails #{"full"} #{}) 2)
   :sleep (fixed [:pos] [:pos] #{"sleeping"} (fails #{"missing" "occupied" "not-night" "monsters-near"} #{"unreachable"}) 5)
   :look (fixed [:pos :yaw :pitch] [:pos] #{"ok"} (fails #{} #{}) 1)
   :swim (entry [:ms :toward :tread] [] #{"surfaced" "landed" "timeout"} (fails #{} #{})
                (fn [_ {:keys [ms]}]
                  (timed (/ (min (or ms (js-setting "prim/swim-default-ms" 3000)) (js-setting "prim/swim-max-ms" 10000))
                            1000))))
   :useOn (entry [:pos :item :face] [:pos] #{"used"} (fails #{"missing" "no-item" "no-room"} #{"cannot" "unreachable" "unchanged"}) (fn [_ {:keys [item]}] (use-items (timed (use-on-s)) (when (not= "none" item) item) 1)))
   :steer (entry [:decide :timeoutS] [] #{"done"} (fails #{} #{}) (fn [_ {:keys [timeoutS]}] (timed (or timeoutS (js-setting "steer/default-timeout-s" 60)))))
   :mount (fixed [:id] [:id] #{"mounted" "already-mounted"} (fails #{"gone" "not-mountable" "hand-full"} #{"blocked" "out-of-reach"}) 2)
   :dismount (fixed [:yaw :pitch] [] #{"dismounted"} (fails #{"not-mounted"} #{}) 2)
   :paddle (fixed [:ticks :turn :forward] [] #{"ok"} (fails #{"not-mounted" "not-a-boat"} #{"blocked"}) 4)
   :wait (entry [:ms] [] #{"ok"} (fails #{} #{})
                (fn [_ {:keys [ms]}]
                  (let [s (/ (min (max (if (number? ms) ms 0) 0) (js-setting "prim/wait-max-ms" 10000)) 1000)]
                    {:uses {:job-time [s s s]}})))
   :offline (entry [:ms :why] [] #{"ok"} (fails #{} #{})
                   (fn [_ {:keys [ms]}]
                     (let [s (/ (min (or ms (js-setting "prim/offline-default-ms" 300000))
                                     (js-setting "prim/offline-max-ms" 600000))
                                1000)]
                       {:uses {:job-time [0 s (+ s (reconnect-s))]}})))})
