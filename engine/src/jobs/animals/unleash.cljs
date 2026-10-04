(ns jobs.animals.unleash
  (:require [engine.ctx :as ctx]
            [engine.jobs.animals :as animals]
            [engine.jobs.util :as u]))

(def doc
  "Take the lead off the animals within :radius that are on this body's lead
  or tied to a fence knot, and pick the leads up again: a one-shot order that
  starts and ends itself. :mob limits it to one kind of animal (any when nil),
  :animal to one animal by its uuid (any when nil). Each round takes the nearest
  such animal not given up on, walks to within 3 blocks of what is clicked
  (moveTo range 2, :walk-timeout-s) and clicks it with an empty hand: the
  animal itself when it is on this body's lead, else the leash_knot it is tied
  to (the click removes the knot and hands every animal tied to it to this body's lead, a second click on the animal then takes the lead off, as on 26.1). An animal tied to a
  knot the sensing cannot see is given up on (:no-knot). A click on the animal counts only
  when the sensing then shows it off its lead; a click on the knot when it then shows it on
  this body's lead (the next round takes it off). An animal is given up on when
  its walk is blocked (:unreachable), when two clicks were out of reach
  (:unreachable), when the click freed nothing within 1.5 s (:no-effect), when it vanished
  (:gone) or when the server said it cannot (:cannot) or the use failed
  (:failed). Unless :collect is false, it then waits 0.8 s for the drops to show and runs jobs.forestry.collect-drops
  for the leads in radius and ends. Done (info unleash.done, and a warn
  unleash.gave-up with the reason unless it is :unleashed; hands over {:reason
  :freed [keys] :given-up {key reason} :collected n :leads carried-at-the-end}) with :reason :unleashed
  when some animal was freed, :timeout after :timeout-s from the first round
  (without collecting), and, with nothing freed, :unreachable when one was
  given up on as unreachable, :refused when others were given up on, else
  :none. It also ends after three fruitless rounds in a row. The check always
  passes, so a cut job resumes and ends itself.")

(def args
  {:mob {:doc "the animal's name, such as \"cow\"; any animal when nil" :default nil}
   :animal {:doc "uuid (or id) of the one animal to free; any when nil" :default nil}
   :radius {:doc "animals within this many blocks count" :default 8}
   :walk-timeout-s {:doc "bound of one walk towards the click" :default 5}
   :timeout-s {:doc "seconds from the first round before the job gives up" :default 30}
   :collect {:doc "pick up the leads afterwards" :default true}})

(def reach 3)
(def max-in-row 3)
(def knot-name "leash_knot")
(def knot-margin 12)
(def settle-ms 250)
(def settle-tries 6)
(def drop-wait-ms 800)

(defn check [_c] true)

(defn finish!
  "Emit the outcome, hand it to the parent and end the job."
  [c reason]
  (let [m (ctx/mem c)
        result {:reason reason
                :freed (vec (:freed m))
                :given-up (:given-up m {})
                :collected (:collected m 0)
                :leads (reduce + (map :count (filter #(= "lead" (:name %)) (u/inventory (:primitives c)))))}]
    (ctx/emit! c :unleash.done :info (assoc result :text (str "unleash done: " (name reason) ", freed " (count (:freed m)))))
    (when (not= :unleashed reason)
      (ctx/emit! c :unleash.gave-up :warn {:reason reason :text (str "unleashing stopped: " (name reason))}))
    (ctx/result! c result)
    :done))

(defn go-collect! [c reason]
  (if-not (:collect (:args c))
    (finish! c reason)
    (do (ctx/update-mem! c assoc :phase :collect :reason reason)
        :continue)))

(defn animals-in-radius [c]
  (let [{:keys [mob radius]} (:args c)]
    (if mob
      (animals/herd (:primitives c) mob radius)
      (vec (array-seq (.entities (:primitives c) #js {:radius radius :kind "passive" :max 64}))))))

(defn candidates
  "The animals on this body's lead or tied to a knot, not freed or given up on, nearest first."
  [c]
  (let [{:keys [animal]} (:args c)
        {:keys [freed given-up]} (ctx/mem c)
        skip (into (set freed) (keys given-up))]
    (filterv #(and (animals/leashed? %)
                   (or (nil? animal) (= animal (animals/key-of %)))
                   (not (contains? skip (animals/key-of %))))
             (animals-in-radius c))))

(defn knot-of
  "The leash_knot entity the animal is tied to, or nil when it is not seen."
  [c a]
  (let [holder (.-leashHolder a)]
    (when (number? holder)
      (->> (array-seq (.entities (:primitives c) #js {:radius (+ knot-margin (:radius (:args c))) :max 64}))
           (filter #(and (= holder (.-id %)) (= knot-name (.-name %))))
           first))))

(defn none-reason [c]
  (let [given-up (vals (:given-up (ctx/mem c)))]
    (cond
      (seq (:freed (ctx/mem c))) :unleashed
      (some #{:unreachable} given-up) :unreachable
      (seq given-up) :refused
      :else :none)))

(defn give-up! [c k reason]
  (ctx/update-mem! c assoc-in [:given-up k] reason))

(defn bump-row! [c]
  (ctx/update-mem! c update :in-row (fnil inc 0)))

(defn ^:async walk!
  "Walk within reach of what is clicked. Resolves to :there, :partial or :blocked; a blocked walk gives the animal up."
  [c k target]
  (let [tpos (u/pos-of (.-pos target))]
    (if (<= (u/dist (u/self-pos c) tpos) reach)
      :there
      (let [r (await (ctx/act c :moveTo (clj->js {:pos tpos :range 2 :timeoutS (:walk-timeout-s (:args c))})))]
        (case (.-status r)
          "arrived" :there
          "partial" :partial
          (do (give-up! c k :unreachable)
              (bump-row! c)
              :blocked))))))

(defn book-out-of-reach! [c k]
  (let [n (inc (get-in (ctx/mem c) [:fails k] 0))]
    (ctx/update-mem! c assoc-in [:fails k] n)
    (when (>= n 2) (give-up! c k :unreachable))
    (bump-row! c)))

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
    :else (do (await (ctx/act c :wait #js {:ms settle-ms}))
              (await (await-state! c k knot? (dec tries))))))

(defn ^:async click!
  "Click target with an empty hand and book what became of animal."
  [c animal target]
  (let [k (animals/key-of animal)
        r (await (ctx/act c :interact #js {:id (.-id target)}))]
    (case (.-status r)
      ("used" "no-effect") (case (await (await-state! c k (not (animals/led-by-me? animal)) settle-tries))
                            :freed (ctx/update-mem! c #(-> % (update :freed (fnil conj []) k) (assoc :in-row 0)))
                            :handed (ctx/update-mem! c assoc :in-row 0)
                            (do (give-up! c k :no-effect) (bump-row! c)))
      "gone" (give-up! c k :gone)
      "out-of-reach" (book-out-of-reach! c k)
      "cannot" (do (give-up! c k :cannot) (bump-row! c))
      (do (give-up! c k :failed) (bump-row! c)))))

(defn ^:async engage!
  "Walk to what holds the animal's lead and click it; end when fruitless three times in a row."
  [c a]
  (let [k (animals/key-of a)
        target (if (animals/led-by-me? a) a (knot-of c a))]
    (if-not target
      (do (give-up! c k :no-knot) (bump-row! c))
      (when (= :there (await (walk! c k target)))
        (await (click! c a target))))
    (if (>= (:in-row (ctx/mem c) 0) max-in-row)
      (go-collect! c (none-reason c))
      :continue)))

(defn ^:async collect! [c]
  (when-not (:dropped (ctx/mem c))
    (await (ctx/act c :wait #js {:ms drop-wait-ms}))
    (ctx/update-mem! c assoc :dropped true))
  (let [r (await (ctx/call-child c :collect 'jobs.forestry.collect-drops {:radius (:radius (:args c)) :filter ["lead"]}))]
    (if-not (= :done r)
      :continue
      (do (ctx/update-mem! c assoc :collected (:collected (ctx/child-result c :collect) 0))
          (finish! c (:reason (ctx/mem c)))))))

(defn ^:async round [c]
  (let [now (ctx/now c)
        {:keys [timeout-s]} (:args c)]
    (ctx/update-mem! c update :started #(or % now))
    (let [m (ctx/mem c)
          cands (candidates c)]
      (cond
        (>= (- now (:started m)) (* 1000 timeout-s)) (finish! c :timeout)
        (= :collect (:phase m)) (await (collect! c))
        (empty? cands) (if (seq (:freed m)) (go-collect! c :unleashed) (finish! c (none-reason c)))
        :else (await (engage! c (first cands)))))))
