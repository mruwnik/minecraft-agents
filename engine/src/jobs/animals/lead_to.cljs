(ns jobs.animals.lead-to
  (:require [engine.ctx :as ctx]
            [engine.jobs.animals :as animals]
            [engine.jobs.util :as u]))

(def doc
  "Put a lead on one animal of the mob type :mob, walk to :pos with it
  following and there either tie it to the fence post at :fence or let it go:
  a one-shot order that starts and ends itself. Phases :leash, :walk, :gather
  and :arrive, each by a child job: jobs.animals.leash (radius :radius),
  jobs.movement.go-to (to :pos, or to the :fence cell when no :pos is given,
  range :range), without :fence a :gather (see below) and then, with :fence,
  a click with an empty hand on that post (useOn) after walking to within 2 of
  it, else jobs.animals.unleash for that animal (which picks the lead up
  again). Before every walking round and again on arrival the animal is looked
  up (within :watch-radius): when it is seen off this body's lead the job ends
  at once with :lead-broke, when it is not seen at all with :lost, never with
  success. A tie counts only when the sensing then shows the animal held by
  something else than this body (leashed, not leashedToMe); one click is
  retried once. With :fence the block there must be a fence (name ending
  _fence) or the job ends :no-fence before leashing anything. Done (info
  lead-to.done, and a warn lead-to.gave-up with the reason unless it is
  :tied or :unleashed; hands over {:reason :animal key :still-led bool :at
  pos}) with :reason :tied or :unleashed on success, :lead-broke or :lost
  as above, :unreachable when the walk gave up (the animal is then still on
  the lead: :still-led true), :tie-failed when the post did not take the
  animal (still on the lead), :timeout after :timeout-s from the first round
  (a cut walk leaves the animal on the lead), :no-fence, and the reason of
  jobs.animals.leash (:no-lead, :none, :unreachable, :refused, :all-leashed,
  :timeout) when no animal got on the lead, or of jobs.animals.unleash
  (:refused, :unreachable, :none, :timeout) when the lead would not come off.
  :gather (no :fence only): a body outwalks a led animal, which trails about
  a lead length behind it, so on arrival the animal is looked up and, when it
  is farther than :gather-radius from :pos, the body walks on towards :pos by
  the excess (range 1) so the lead pulls the animal that much nearer, then
  looks again; at most :gather-tries pulls. A pull that did not arrive, or
  running out of pulls, is no failure: the animal is let go where it is.
  The check always passes, so a cut job resumes and ends itself.")

(def args
  {:mob {:doc "the animal's name, such as \"cow\"" :default nil}
   :pos {:doc "where to lead it {:x :y :z}; the :fence cell when nil" :default nil}
   :fence {:doc "the fence post {:x :y :z} to tie it to; unleash it at :pos when nil" :default nil}
   :range {:doc "how close to :pos counts as there" :default 2}
   :radius {:doc "animals within this many blocks are leashed from where the job starts" :default 8}
   :gather-radius {:doc "without :fence the animal is let go once it is within this many blocks of :pos" :default 3}
   :gather-tries {:doc "without :fence how many times the body walks on to pull a trailing animal nearer" :default 3}
   :watch-radius {:doc "how far from the body the led animal is looked for" :default 64}
   :timeout-s {:doc "seconds from the first round before the job gives up" :default 180}})

(def max-ties 2)

(defn check [_c] true)

(defn finish!
  "Emit the outcome, hand it to the parent and end the job."
  [c reason]
  (let [m (ctx/mem c)
        result {:reason reason
                :animal (:animal m)
                :still-led (boolean (:still-led m))
                :at (u/self-pos c)}]
    (ctx/emit! c :lead-to.done :info (assoc result :text (str "lead-to done: " (name reason))))
    (when-not (#{:tied :unleashed} reason)
      (ctx/emit! c :lead-to.gave-up :warn {:reason reason :text (str "leading stopped: " (name reason))}))
    (ctx/result! c result)
    :done))

(defn destination [c]
  (let [{:keys [pos fence]} (:args c)]
    (or pos fence)))

(defn fence-block? [c]
  (some-> (u/block-name (:primitives c) (:fence (:args c))) (.endsWith "_fence")))

(defn animal-now
  "The led animal as the sensing shows it, or nil when it is not seen."
  [c]
  (animals/find-by-key (:primitives c) (:mob (:args c)) (:watch-radius (:args c)) (:animal (ctx/mem c))))

(defn set-phase! [c phase]
  (ctx/update-mem! c assoc :phase phase))

(defn ^:async leash! [c]
  (let [{:keys [mob radius]} (:args c)
        r (await (ctx/call-child c :leash 'jobs.animals.leash {:mob mob :radius radius}))]
    (if-not (= :done r)
      :continue
      (let [res (ctx/child-result c :leash)]
        (if-not (= :leashed (:reason res))
          (finish! c (:reason res))
          (do (ctx/update-mem! c assoc :animal (:animal res) :still-led true :phase :walk)
              :continue))))))

(defn ^:async walk! [c]
  (let [r (await (ctx/call-child c :walk 'jobs.movement.go-to {:pos (destination c) :range (:range (:args c)) :doors :leave-open}))]
    (if-not (= :done r)
      :continue
      (if (:arrived (ctx/child-result c :walk))
        (do (set-phase! c (if (:fence (:args c)) :arrive :gather)) :continue)
        (finish! c :unreachable)))))

(defn flat-dist [a b]
  (js/Math.hypot (- (:x a) (:x b)) (- (:z a) (:z b))))

(defn pull-point
  "Where the body walks to pull animal-pos nearer to pos by its excess over radius: the body's own
  position moved along the line from the animal to pos (flat, the body's y kept), or nil when the
  animal is close enough."
  [body animal-pos pos radius]
  (let [d (flat-dist animal-pos pos)
        excess (- d radius)]
    (when (pos? excess)
      (let [k (/ excess d)]
        (assoc body
               :x (+ (:x body) (* k (- (:x pos) (:x animal-pos))))
               :z (+ (:z body) (* k (- (:z pos) (:z animal-pos)))))))))

(defn ^:async pull! [c target]
  (let [r (await (ctx/call-child c :pull 'jobs.movement.go-to {:pos target :range 1 :doors :leave-open}))]
    (if-not (= :done r)
      :continue
      (do (ctx/update-mem! c dissoc :pull-target)
          (ctx/update-mem! c update :pulls (fnil inc 0))
          (when-not (:arrived (ctx/child-result c :pull)) (set-phase! c :arrive))
          :continue))))

(defn ^:async gather!
  "Without :fence: let the animal catch up. A pull in progress is carried on with its stored target;
  otherwise the animal is looked at and either is near enough (or the pulls are spent: :arrive) or
  starts a pull."
  [c a]
  (let [{:keys [pos gather-radius gather-tries]} (:args c)
        {:keys [pull-target pulls] :or {pulls 0}} (ctx/mem c)
        target (or pull-target
                   (when (< pulls gather-tries)
                     (pull-point (u/self-pos c) (u/pos-of (.-pos a)) pos gather-radius)))]
    (if-not target
      (do (set-phase! c :arrive) :continue)
      (do (ctx/update-mem! c assoc :pull-target target)
          (await (pull! c target))))))

(defn ^:async let-go! [c]
  (set-phase! c :release)
  (let [r (await (ctx/call-child c :unleash 'jobs.animals.unleash {:mob (:mob (:args c)) :animal (:animal (ctx/mem c))
                                                                    :radius (:watch-radius (:args c))}))]
    (if-not (= :done r)
      :continue
      (let [reason (:reason (ctx/child-result c :unleash))]
        (when (= :unleashed reason) (ctx/update-mem! c assoc :still-led false))
        (finish! c reason)))))

(defn tied?
  "True when the animal is held by something else than this body."
  [c]
  (let [a (animal-now c)]
    (and a (animals/leashed? a) (not (animals/led-by-me? a)))))

(defn ^:async tie! [c]
  (let [fence (:fence (:args c))
        near (await (u/walk-near! c fence 2))]
    (if-not (= :there near)
      :continue
      (let [r (await (ctx/act c :useOn (clj->js {:pos fence})))
            tries (inc (:ties (ctx/mem c) 0))]
        (ctx/update-mem! c assoc :ties tries)
        (cond
          (tied? c) (do (ctx/update-mem! c assoc :still-led false) (finish! c :tied))
          (>= tries max-ties) (finish! c :tie-failed)
          :else (do (ctx/emit! c :lead-to.tie-retry :info {:status (.-status r) :text "the post did not take the animal, trying once more"})
                    :continue))))))

(defn ^:async arrive! [c]
  (if (:fence (:args c)) (await (tie! c)) (await (let-go! c))))

(defn ^:async round [c]
  (let [now (ctx/now c)
        {:keys [timeout-s fence]} (:args c)]
    (ctx/update-mem! c update :started #(or % now))
    (let [{:keys [phase started animal]} (ctx/mem c)
          a (when animal (animal-now c))]
      (cond
        (and (nil? phase) fence (not (fence-block? c))) (finish! c :no-fence)
        (>= (- now started) (* 1000 timeout-s)) (finish! c :timeout)
        (nil? phase) (do (set-phase! c :leash) :continue)
        (= :leash phase) (await (leash! c))
        (and (#{:walk :gather :arrive} phase) (nil? a)) (do (ctx/update-mem! c assoc :still-led false) (finish! c :lost))
        (and (#{:walk :gather :arrive} phase) (not (animals/led-by-me? a))) (do (ctx/update-mem! c assoc :still-led false) (finish! c :lead-broke))
        (= :walk phase) (await (walk! c))
        (= :gather phase) (await (gather! c a))
        (= :arrive phase) (await (arrive! c))
        (= :release phase) (await (let-go! c))
        :else (finish! c :lost)))))
