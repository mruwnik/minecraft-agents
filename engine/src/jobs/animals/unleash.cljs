(ns jobs.animals.unleash
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.animals :as animals]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look]
            [jobs.lib.pace :as pace]
            [jobs.lib.util :as u]))

(def doc
  "Take the lead off the animals within :radius that are on this body's lead or tied to a fence knot, and pick
  the leads up again. A one-shot order that starts and ends itself; a cut job resumes.

  :mob limits it to one kind (any when nil), :animal to one animal by uuid (any when nil).

  One call is the whole run. It takes the nearest such animal not given up on. The body walks to within 3 blocks of what it
  clicks (doors :shut) and clicks with an empty hand:
  - An animal on this body's lead is clicked itself. It counts when the sensing then shows it off the lead
    (checked for up to 1.5 s).
  - An animal tied to a knot: the knot is clicked. That removes the knot and hands every animal tied to it to
    this body's lead, and the next step takes the lead off. An animal tied to a knot the sensing cannot see is
    given up on (:no-knot).

  An animal is given up on when its walk is blocked or two clicks were out of reach (:unreachable), the click
  freed nothing (:no-effect), it vanished (:gone), the server said it cannot (:cannot) or the use failed
  (:failed).

  Unless :collect is false the job then waits 0.8 s for the drops and runs jobs.items.collect-drops for the
  leads in :radius.

  The data {:reason :freed
  [keys] :given-up {key reason} :collected n :leads n} (:collected is the leads gained since the call began, :leads
  the leads carried at the end) and the ending by the reason:
  - :done :unleashed: some animal was freed; :already when none within :radius was on a lead at the start, after one look around.
  - :condition-failed :refused or :no-zones (no zone list was read).
  - :nothing-possible with nothing freed: :unreachable if one was given up as unreachable, :failed if others were given up, else :none (0 freed is
    never done; a run that began with an animal on a lead). It also ends after three fruitless animals in a row.

  Zones: an animal standing in another owner's zone or claim, or in a plan's footprint, is left on its lead (warn
  unleash.declined once, :reason :refused, or :no-zones when no zone list was read). :ignore-zones? true skips the check.")

(a/defargs args
  {:mob {:doc "the animal's name, such as \"cow\"; any animal when nil" :spec a/name? :default nil}
   :animal {:doc "uuid (or id) of the one animal to free; any when nil" :spec (a/or-of string? number?) :default nil}
   :radius {:doc "animals within this many blocks count" :spec (a/num-in 0 nil) :default 8}
   :collect {:doc "pick up the leads afterwards" :spec boolean? :default true}
   :ignore-zones? :shared})

(a/defargs settings
  {::job-time-s {:default 60 :doc "Seconds from the start of the call before unleashing gives up; a walk to the animal and the collect of the lead (10 s at most) fit." :spec (a/num-in 0 nil)}
   ::max-in-row {:default 3 :doc "Fruitless animals in a row that end the run." :spec (a/int-in 0 nil)}
   ::knot-margin {:default 12 :doc "Blocks around the knot searched for the animal." :spec (a/int-in 0 nil)}
   ::settle-ms {:default 250 :doc "How long the body waits for an animal to settle, in ms." :spec (a/int-in 0 nil)}
   ::settle-tries {:default 6 :doc "Looks at the animal before it counts as settled." :spec (a/int-in 0 nil)}
   ::drop-wait-ms {:default 800 :doc "How long the body waits for a dropped item, in ms." :spec (a/int-in 0 nil)}})
(defn max-in-row [] (settings/get settings ::max-in-row))
(def knot-name "leash_knot")
(defn knot-margin [] (settings/get settings ::knot-margin))
(defn settle-ms [] (settings/get settings ::settle-ms))
(defn settle-tries [] (settings/get settings ::settle-tries))
(defn drop-wait-ms [] (settings/get settings ::drop-wait-ms))

(def target [:mob :animal])

(def safe? ready/safe-here-by-night)
(def fit? ready/ok)


(def defaults {:spend {:job-time ::job-time-s}})

(def can-start?
  "Nothing waits: an order with nobody on a lead ends the first round."
  (constantly (contract/ok :no-wait "no start condition")))

(defn quote-of [c]
  {:uses {:job-time [0 10 ##Inf]}})

(defn leads-carried [c]
  (reduce + (map :count (filter #(= "lead" (:name %)) (u/inventory (:primitives c))))))

(def ending-of
  "The kind and cause of a finish by reason."
  {:unleashed [:done nil]
   :refused [:condition-failed :refused]
   :no-zones [:condition-failed :no-zones]})

(defn finish!
  "Emit the outcome and end the call (ending-of, else nothing-possible). :collected is the leads gained since the call
  began, so a lead picked up the moment it dropped counts too."
  ([c reason] (finish! c reason nil))
  ([c reason cause]
  (let [m (ctx/mem c)
        leads (leads-carried c)
        result {:reason reason
                :freed (vec (:freed m))
                :given-up (:given-up m {})
                :collected (max 0 (- leads (:leads-at-start m leads)))
                :leads leads
                :text (str "unleash done: " (name reason) ", freed " (count (:freed m)))}]
    (let [[kind ending-cause] (get ending-of reason [:nothing-possible reason])]
      (ctx/end c kind (or cause ending-cause) result)))))

(defn go-collect! [c reason]
  (if-not (:collect (:args c))
    (finish! c reason)
    (do (ctx/update-mem! c assoc :phase :collect :reason reason)
        :again)))

(defn animals-in-radius [c]
  (let [{:keys [mob radius]} (:args c)]
    (if mob
      (animals/herd (:primitives c) mob radius)
      (vec (look/seen-objs (:primitives c) {:radius radius :kind "passive" :max 64})))))

(defn on-a-lead
  "The animals in radius on a lead (this body's or a knot's) that the call is about, nearest first."
  [c]
  (let [{:keys [animal]} (:args c)]
    (filterv #(and (animals/leashed? %) (or (nil? animal) (= animal (animals/key-of %)))) (animals-in-radius c))))

(defn candidates
  "The animals on this body's lead or tied to a knot, not freed or given up on, nearest first."
  [c]
  (let [{:keys [freed given-up]} (ctx/mem c)
        skip (into (set freed) (keys given-up))]
    (->> (on-a-lead c)
         (filterv #(not (contains? skip (animals/key-of %))))
         (animals/allowed c :unleash.declined "unleash" :take))))

(defn done?
  "Ok before any work when no animal in reach is on a lead; later with the animals freed and those still on a lead:
  the run ends itself with its reason (a run that freed or gave up on one is not done by finding none)."
  [c]
  (let [{:keys [freed given-up phase]} (ctx/mem c)]
    (if (and (empty? freed) (empty? given-up) (nil? phase) (empty? (on-a-lead c)) (look/surveyed? c))
      (contract/ok :unleashed "no animal in reach is on a lead"
                   :result {:reason :unleashed :freed [] :given-up {} :collected 0 :leads (leads-carried c)})
      (contract/later :unleashing (str (count freed) " animals freed") :units :animals
                      :done (count freed) :left (count (candidates c))))))

(defn knot-of
  "The leash_knot entity the animal is tied to, or nil when it is not seen."
  [c a]
  (let [holder (.-leashHolder a)]
    (when (number? holder)
      (->> (look/seen-objs (:primitives c) {:radius (+ (knot-margin) (:radius (:args c))) :max 64})
           (filter #(and (= holder (.-id %)) (= knot-name (.-name %))))
           first))))

(defn none-reason [c]
  (let [given-up (vals (:given-up (ctx/mem c)))]
    (cond
      (seq (:freed (ctx/mem c))) :unleashed
      (some #{:unreachable} given-up) :unreachable
      (seq given-up) :failed
      :else :none)))

(defn now-of [c k]
  (first (filter #(= k (animals/key-of %)) (animals-in-radius c))))

(defn freed? [c k]
  (let [now (now-of c k)]
    (and now (not (animals/leashed? now)))))

(defn handed-over?
  "True when the animal is on this body's lead now."
  [c k]
  (some-> (now-of c k) animals/led-by-me?))

(defn ^:async await-state!
  "The state of animal k after a click once the sensing settles, polling tries times: :freed (off its lead), or
  :handed when knot? and it is on this body's lead, else :unchanged. A removed knot lets go a tick or two after
  the click."
  [c k knot? tries]
  (cond
    (freed? c k) :freed
    (and knot? (handed-over? c k)) :handed
    (zero? tries) :unchanged
    :else (do (await (ctx/act c :wait #js {:ms (settle-ms)}))
              (await (await-state! c k knot? (dec tries))))))

(defn ^:async click!
  "Click target with an empty hand and book what became of animal."
  [c animal target]
  (let [k (animals/key-of animal)
        r (await (animals/use-on! c k target nil {:doors :shut}))]
    (cond
      (= :continue r) :continue
      (#{:used :no-effect} (:status r)) (case (await (await-state! c k (not (animals/led-by-me? animal)) (settle-tries)))
                                          :freed (ctx/update-mem! c #(-> % (update :freed (fnil conj []) k) (assoc :in-row 0)))
                                          :handed (ctx/update-mem! c assoc :in-row 0)
                                          (do (animals/give-up! c k :no-effect) (animals/bump-row! c)))
      :else (animals/book-failed! c k (:status r)))))

(defn ^:async engage!
  "Click what holds the animal's lead; end when fruitless :max-in-row times in a row."
  [c a]
  (let [k (animals/key-of a)
        target (if (animals/led-by-me? a) a (knot-of c a))
        r (if target
            (await (click! c a target))
            (do (animals/give-up! c k :no-knot) (animals/bump-row! c)))]
    (cond
      (= :continue r) :continue
      (>= (:in-row (ctx/mem c) 0) (max-in-row)) (go-collect! c (none-reason c))
      :else :again)))

(defn ^:async collect! [c]
  (let [waited (if (:dropped (ctx/mem c))
                 {:kind :done}
                 (await (child/pause! c (/ (drop-wait-ms) 1000) :lead-dropping)))]
    (if (= :continue waited)
      :continue
      (do (ctx/update-mem! c assoc :dropped true)
          (let [e (await (ctx/call c :collect 'jobs.items.collect-drops {:radius (:radius (:args c)) :items ["lead"]}))]
            (if (= :continue e)
              :continue
              (finish! c (:reason (ctx/mem c)))))))))

(defn ^:async step [c]
  (ctx/update-mem! c update :leads-at-start (fn [n] (or n (leads-carried c))))
  (let [m (ctx/mem c)
        cands (candidates c)]
    (cond
      (= :collect (:phase m)) (await (collect! c))
      (and (empty? cands) (not (look/surveyed? c))) (do (await (look/survey! c)) (if (look/surveyed? c) (await (step c)) :again)) ; animals count only while in view; no done? between the look and its verdict, which keeps the :already cause
      (empty? cands) (cond (seq (:freed m)) (go-collect! c :unleashed)
                           (and (empty? (:given-up m)) (empty? (on-a-lead c))) (finish! c :unleashed :already)
                           :else (finish! c (none-reason c)))
      :else (await (engage! c (animals/pick c cands))))))

(defn ^:async round
  "The whole attempt: steps (a pace between; done? and can-run? are asked between them) until one ends or yields."
  [c]
  (await (pace/steps! c #(step c))))
