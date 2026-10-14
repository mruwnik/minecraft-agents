(ns jobs.primitive
  "The acting primitives as the job contract sees them (docs/goals-and-actions.md): each is one act, which cannot be
  stopped midway. An entry is {:target [arg keys] :quote-of (fn [p args] quote) :ending (fn [result] ending)}: p the
  primitives, args the act's args (keyword keys), result the primitive's JS result. engine.core.limits checks a
  quote's most against what every frame has left before the act runs, so a most is finite and never short of the
  real cost: time is the primitive's own bound (js/prim-*.mjs). `job` wraps an entry as a registry job, which
  engine.registry lists as jobs.primitive.<kebab-case name> so agents and jobs call and quote each by name."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]))

(defn js-setting
  "The live value of js/settings.mjs key k, else fallback (no JavaScript settings loaded, as in tests)."
  [k fallback]
  (if-some [v ((settings/js-reader) k)] v fallback))

(defn upto "A figure of 0 up to most, mean a guess." [mean most] [0 (min mean most) most])

(defn timed "A quote of time only, bounded by most s." [most] {:uses {:time (upto 1 most)}})

(defn use-items
  "q using up to n of item (least 0: an act that fails uses none); unchanged without an item."
  [q item n]
  (cond-> q (and (string? item) (pos? n)) (assoc-in [:uses :items item] [0 n n])))

(defn held [p] (let [h (.-held (.self p))] (if (string? h) h (some-> h .-name))))

(defn carried [p item]
  (reduce + 0 (keep #(when (= item (.-name %)) (.-count %)) (.-inventory (.self p)))))

(defn fall
  "The health a fall straight from where the body stands down to pos costs: each block past 3 is 1 hp."
  [p pos]
  (let [from (some-> (.self p) .-pos .-y)]
    (if (and (number? from) (number? (:y pos)))
      (max 0 (- (js/Math.floor from) (js/Math.floor (:y pos)) 3))
      0)))

(defn ending-of
  "The ending fn of a primitive whose done statuses are done: timeout is the time line, a cut or a lost connection
  cuts off, failed is nothing possible, any other status a failed condition named by it."
  [done]
  (fn [r]
    (let [s (some-> r .-status)
          k (keyword s)]
      (cond
        (contains? done s) (contract/ending :done k nil)
        (= "timeout" s) (contract/ending :limit-reached {:line :time} nil)
        (#{"cut" "offline" "closed" "disconnected"} s) (contract/ending :cut-off k nil)
        (= "failed" s) (contract/ending :nothing-possible :failed nil)
        :else (contract/ending :condition-failed (or k :no-status) nil)))))

(defn entry [target done quote-of] {:target target :quote-of quote-of :ending (ending-of done)})

(defn fixed [target done most] (entry target done (fn [_ _] (timed most))))

(def table
  {:moveTo (entry [:pos] #{"arrived"}
                  (fn [p {:keys [pos timeoutS]}]
                    (assoc-in (timed (min (or timeoutS 20) 60)) [:uses :health] [0 0 (fall p pos)])))
   :dig (entry [:pos] #{"dug"}
               (fn [p {:keys [pos timeoutS]}]
                 (let [ms (or (when pos (.digTime p (clj->js pos))) 0)
                       bound (+ (/ (if (js/Number.isFinite ms) ms 0) 1000)
                                (js-setting "prim/dig-margin-s" 5) (js-setting "prim/drop-wait-s" 1))
                       tool (held p)]
                   (cond-> (timed (min bound (or timeoutS ##Inf)))
                     tool (assoc-in [:uses :wear tool] [0 1 1])))))
   :place (entry [:pos] #{"placed"} (fn [_ {:keys [item]}] (use-items (timed 5) item 1)))
   :jumpPlace (entry [] #{"done"}
                     (fn [_ {:keys [item count]}]
                       (let [n (min (or count 1) (js-setting "prim/jump-place-max" 8))]
                         (use-items (timed (* n (js-setting "prim/jump-place-block-s" 2))) item n))))
   :collect (entry [:id] #{"collected"} (fn [_ {:keys [timeoutS]}] (timed (min (or timeoutS 10) 20))))
   :inspectContainer (fixed [:pos] #{"ok"} 5)
   :transfer (entry [:pos :item] #{"ok"}
                    (fn [_ {:keys [direction item count]}]
                      (let [q (timed 5)]
                        (cond
                          (not (number? count)) q
                          (= "deposit" direction) (use-items q item count)
                          (string? item) (assoc-in q [:yields :items item] [0 count count])
                          :else q))))
   :equip (fixed [:item] #{"equipped"} 2)
   :toss (entry [:item] #{"tossed"}
                (fn [p {:keys [item count watchS]}]
                  (use-items (timed (+ 2 (min (or watchS 0) 5))) item (or count (carried p item)))))
   :craft (entry [:item] #{"crafted"} (fn [_ {:keys [count]}] (timed (min 60 (+ 4 (* 6 (or count 1)))))))
   :furnace (fixed [:pos] #{"ok"} 5)
   :enchant (fixed [:pos] #{"ok" "enchanted"} 15)
   :chat (fixed [] #{"sent"} 3)
   :eat (entry [:item] #{"ate"} (fn [_ {:keys [item]}] (use-items (timed 5) item 1)))
   :fish (entry [:pos] #{"caught"}
                (fn [_ {:keys [timeoutS]}] (timed (+ 5 (or timeoutS (js-setting "prim/fish-wait-s" 45))))))
   :attack (fixed [:id] #{"hit" "killed"} 1)
   :interact (entry [:id] #{"used"} (fn [_ {:keys [item]}] (use-items (timed 2) item 1)))
   :trade (fixed [:id] #{"bought" "ok"} 8)
   :unequip (fixed [] #{"ok" "empty"} 2)
   :sleep (fixed [:pos] #{"sleeping"} 5)
   :look (fixed [:pos] #{"ok"} 1)
   :swim (entry [] #{"surfaced" "landed" "timeout"}
                (fn [_ {:keys [ms]}]
                  (timed (/ (min (or ms (js-setting "prim/swim-default-ms" 3000)) (js-setting "prim/swim-max-ms" 10000))
                            1000))))
   :useOn (entry [:pos] #{"used"} (fn [_ {:keys [item]}] (use-items (timed 5) (when (not= "none" item) item) 1)))
   :steer (entry [] #{"done"} (fn [_ {:keys [timeoutS]}] (timed (or timeoutS (js-setting "steer/default-timeout-s" 60)))))
   :mount (fixed [:id] #{"mounted" "already-mounted"} 2)
   :dismount (fixed [] #{"dismounted"} 2)
   :paddle (fixed [] #{"ok"} 4)
   :wait (entry [] #{"ok"}
                (fn [_ {:keys [ms]}]
                  (let [s (/ (min (max (if (number? ms) ms 0) 0) (js-setting "prim/wait-max-ms" 10000)) 1000)]
                    {:uses {:time [s s s]}})))
   :offline (entry [] #{"ok"}
                   (fn [_ {:keys [ms]}]
                     (let [s (/ (min (or ms (js-setting "prim/offline-default-ms" 300000))
                                     (js-setting "prim/offline-max-ms" 600000))
                                1000)]
                       {:uses {:time [0 s s]}})))})

(def arg-keys
  "The keys of each primitive's args (engine/README, Acting); a job of it takes these and the call keys."
  {:moveTo [:pos :range :timeoutS :maxDistance] :dig [:pos :timeoutS] :place [:pos :item :click]
   :jumpPlace [:item :count] :collect [:id :timeoutS :maxDropDown] :inspectContainer [:pos :entity]
   :transfer [:pos :entity :direction :item :count] :equip [:item :dest] :toss [:item :count :slot :watchS]
   :craft [:item :count :table] :furnace [:pos :op :input :fuel :output] :enchant [:pos :op :item :choice :levelCost]
   :fish [:pos :timeoutS] :chat [:message :to] :eat [:item] :attack [:id] :interact [:id :item]
   :trade [:id :op :offer :times] :unequip [:dest] :sleep [:pos] :look [:pos :yaw :pitch] :swim [:ms :toward]
   :useOn [:pos :item :face] :steer [:decide :timeoutS] :mount [:id] :dismount [:yaw :pitch]
   :paddle [:ticks :turn :forward] :wait [:ms] :offline [:ms :why]})

(def position-keys #{:pos})

(def also-required
  "Args beside the table's target that the act cannot go without."
  {:place [:item] :steer [:decide] :jumpPlace [:item] :trade [:id :op]})

(defn missing-args
  "The condition of primitive k: nil when the args hold what the act needs, else the cause naming what is missing."
  [k]
  (let [need (distinct (concat (get-in table [k :target]) (also-required k)))]
    (fn [c]
      (when-let [missing (seq (remove #(some? (get (:args c) %)) need))]
        {:reason :bad-args :missing (vec missing)}))))

(defn kebab [k] (str/replace (name k) #"[A-Z]" #(str "-" (str/lower-case %))))

(defn args-data [k]
  (into {} (map (fn [arg] [arg (if (position-keys arg)
                                  {:doc "where, [x y z] or {:x :y :z}" :spec ::a/pos}
                                  {:doc (str "the act's " (name arg) ", as the primitive reads it")
                                   :spec (a/pred "a value" some?) :form 'some?})]))
        (arg-keys k)))

(defn act-args
  "The act's args as JavaScript: the job's own args, unset ones and the call keys left out."
  [args]
  (clj->js (apply dissoc (into {} (remove (comp nil? val)) args) (keys contract/call-keys))))

(defn job
  "The registry entry of primitive k: one round is one act, ended by the table's ending of its result (the result's
  fields are the ending's data)."
  [k]
  (let [{:keys [target quote-of ending]} (get table k)
        sym (symbol (str "jobs.primitive." (kebab k)))]
    {:conditions [[:fn (missing-args k)]] :target target :args (a/declare! sym (args-data k))
     :doc (str "The primitive " k " as a job: one act, which cannot be stopped midway. Takes the act's args (engine/README,"
               " Acting). Ends done when the act's status is a success, else with the ending its status maps to; the result"
               " fields are the ending's data.")
     :quote-of (fn [c] (quote-of (:primitives c) (:args c)))
     :round (fn ^:async primitive-round [c]
              (let [r (try (await (ctx/act c k (act-args (:args c))))
                           (catch :default err
                             (if (.-badArgs err) err (throw err))))]
                (if (instance? js/Error r)
                  (ctx/end c :condition-failed {:reason :bad-args :why (.-message r)})
                  (let [e (ending r)]
                    (ctx/end c (:kind e) (:cause e) (js->clj r :keywordize-keys true))))))}))
