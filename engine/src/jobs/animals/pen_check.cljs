(ns jobs.animals.pen-check
  (:require [plan.pen-fit :as pen-fit]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.work-place :as work-place]
            [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.need :as need]
            [jobs.lib.animals :as animals]
            [jobs.lib.dig-look :as dig-look]
            [jobs.lib.near :as near]
            [jobs.lib.pen :as pen]
            [jobs.lib.pen-patch :as pen-patch]
            [jobs.lib.pen-read :as pen-read]
            [jobs.lib.storage :as storage]
            [jobs.lib.util :as u]))

(def doc
  "Make a pen secure: say whether an animal can walk out of a pen, and patch what it can. Flood-fills what a cow can walk (rules in
  jobs.lib.pen: fences, walls and closed gates are 1.5 high, an open gate is a way out, a drop of more than 3
  is not taken) from the feet cell :at [x y z], or from every surface inside :box (or the columns a :plan's fence, wall and gate cells close in), where a step out of the box
  is a leak. Reads what the body has seen; a pen it has not seen closed is checked again after it looks at the
  cells it has not seen (jobs.lib.sense/decide!); a cell that stays unseen (hidden behind a block) is walked to, at
  most 3 times, until the eye sees it, unless the fill already leaves through a seen gap: that is answered at once (an open gate may divide the pen, so it waits for the look). Ends impossible :no-pen without :at, :box and :plan; waits (:plan-trouble) while :plan is missing or unreadable and ends impossible :plan-trouble when it has no fence, wall or gate cells.

  The fill stops after ::max-cells cells (a setting): a bigger pen is not closed, reason :unbounded. A fill without a box
  that finds no wall either side of a gap says that too.

  Patching (way :patch, on by default; :ways #{:check} only reads; the fence comes from the carried items, and from :chest :craft :gather when the call's ways list them: :ways #{:check :patch} patches with what is carried): a leak that is a :gap gets one fence (a jobs.blocks.place child; the kind is :item, else the fence or wall seen
  beside the gap) and one on top of each :climb or :perch-in fence; an :open-gate is shut (a jobs.items.use child). Then the pen is read again, in at most ::max-passes passes
  (a pass that put nothing right is the last). Nothing else is placed or dug. The result gains :patched {:placed :shut :failed}.

  Ends with the data {:closed? :reason :cells :leaks :gates}: :done when closed, or when a leak was found and :patch is not allowed (a leak is
  then the answer, not a failure); with :patch allowed a leak left after the patches is nothing-possible :leak (the leaks name what remains). Else condition-failed (:no-start) or nothing-possible (:unbounded, :unloaded: the check gave up) with the :reason as the cause:
  - :herdable? false (and the text says why) when the pen is closed but too shallow for herd (plan.pen-fit).
  - :reason is nil when closed, else :leak, :unbounded, :unloaded (a cell, or the start's floor, was never seen) or :no-start (:at is no floor to stand on).
  - :leaks lists {:pos {:x :y :z} :why}, why one of :open-gate :gap :climb :perch-in :open :unloaded (only when :reason is not :leak; :gap also names the cell under a fence hung over lower ground (a plan box as high as its fence starts the fill from the ground below); :perch-in: open ground within a block of a fence top outside the pen, :stand names the ground cell; :climb with a box names the fence, wall or gate top first stood on, reached from a block or raised floor beside it). At most 12 are
    listed, :leaks-total gives the count when more.
  - :cells counts the inside cells. For a leaky pen without a box it counts the cells the pen would enclose with
    its leaks shut, and 0 when shutting up to 4 rounds of them does not close it (a low wall all round).
  - :remembered lists the cells the answer rests on that are remembered, not in view even after a look (at most 12,
    :remembered-total when more).
  - :gate-drops (only when found) lists {:gate :from :to :drop}: a gate whose side of the pen is cut off by a drop of more than 1 block (animals will not step down 2).
  - :gates lists every fence gate in or beside those cells with :open?.
  - :animals lists the livestock on those cells (or up to 3 above one: a ledge inside the fence) as {:kind :id :pos [x y z]}, :animals-in counts them, so a loss can be dated from the log. Only animals in view are listed: :unseen-cells counts the inside cells known from memory only, :animals-complete? is false while any.")

(a/defargs args
  {:at {:doc "feet cell [x y z] or {:x :y :z} of a spot inside the pen (the floor's top, where the animal's feet are)" :spec ::a/pos :default nil}
   :box {:doc "the pen: {:min :max}, each corner {:x :y :z} or [x y z], inclusive; a step out of it is a leak" :spec pen-read/box-arg :default nil}
   :plan {:doc "id of a plan of the body's world: the pen is the box round its fence, gate and wall cells (instead of :box)" :spec a/name? :default nil}
   :part {:doc "with :plan, only the cells of this part" :spec a/name? :default nil}
   :item {:doc "the fence or wall item that patches a gap; nil: the kind seen beside the gap" :spec a/item? :default nil}
   :ignore-zones? :shared})

(def ways [:check :patch :chest :craft :gather])

(defn pen-box
  "{:box} for the args (:box, else the box of :plan), {:trouble text}, or {} with only :at."
  [c]
  (let [{:keys [box plan part]} (:args c)]
    (cond
      box {:box (pen-read/norm-box box)}
      plan (pen-read/plan-box c plan part)
      :else {})))

(def target [:at :box :plan])

(def safe?
  "Not exposed at the pen: :at, else the middle of its :box or of the cells of :plan."
  (price/staying :work (ready/safe-at-by-night work-place/spot)))
(def fit? ready/ok)


(a/defargs settings
  {::job-time-s {:default 30 :doc "Seconds a pen check may take besides the look at its cells; the default time spend is this plus ::cell-s a cell the fill may visit." :spec (a/num-in 0 nil)}
   ::cell-s {:default 0.03 :doc "Seconds of the default time spend per cell the fill may visit (::max-cells, or the footprint of a given :box when smaller)." :spec (a/num-in 0 nil)}
   ::max-cells {:default pen/default-max-cells :doc "Most cells the fill visits before it gives up with :unbounded." :spec (a/int-in 1 nil)}
   ::max-walks {:default 3 :doc "Walks to the same unseen cell (no progress) before the answer is given." :spec (a/int-in 0 nil)}
   ::max-passes {:default 3 :doc "Patch passes (patch, then read again) before the leaks that remain are reported." :spec (a/int-in 1 nil)}
   ::patch-s {:default 120 :doc "Seconds of the default time spend for patching." :spec (a/num-in 0 nil)}
   ::animal-radius {:default 48 :doc "Blocks round the body within which animals on the pen's cells are listed." :spec (a/num-in 1 nil)}})

(defn default-time
  "The call's default :spend :job-time: ::job-time-s plus ::cell-s for each cell the fill may visit."
  [{:keys [box]}]
  (let [max-cells (settings/get settings ::max-cells)
        {:keys [min max]} (when box (pen-read/norm-box box))
        area (when box (* (inc (- (:x max) (:x min))) (inc (- (:z max) (:z min)))))]
    (+ (settings/get settings ::job-time-s) (settings/get settings ::patch-s)
       (* (settings/get settings ::cell-s) (if area (clojure.core/min max-cells area) max-cells)))))

(def defaults {:spend {:job-time default-time}})

(defn no-pen
  "The cause :no-pen before the call starts: none of :at, :box, :plan."
  [c]
  (let [{:keys [at box plan]} (:args c)]
    (when-not (or at box plan) {:reason :no-pen :why "needs :at, :box or :plan"})))

(defn can-run?
  "Later :plan-trouble while :plan is missing or unreadable (it may be saved or fixed later); never when it has no
  fence, wall or gate cells."
  [c]
  (let [{:keys [trouble later?]} (pen-box c)]
    (cond
      (nil? trouble) (contract/ok :pen "a pen to read")
      later? (contract/later :plan-trouble trouble :why trouble)
      :else (contract/never :plan-trouble trouble :why trouble))))

(defn can-start?
  "Never :no-pen without :at, :box and :plan."
  [c]
  (if (no-pen c)
    (contract/never :no-pen "pen-check needs :at, :box or :plan" :why "needs :at, :box or :plan")
    (can-run? c)))

(defn done?
  "Later with the cells of the pen read: the round ends the call with the answer."
  [c]
  (contract/later :reading (str (:cells (ctx/mem c) 0) " cells read") :units :cells :done (:cells (ctx/mem c) 0) :left 0))

(defn quote-of [_c]
  {:uses {:job-time [1 5 30]}})

(defn unseen-leak
  "The nearest-listed leak cell that is unseen and not more than one layer below the start cell at, as {:x :y :z}: an
  animal cannot leave through ground under the pen floor, and those cells can never be seen."
  [checked at]
  (let [floor-y (dec (if (map? at) (:y at) (second at)))]
    (:pos (first (filter #(and (= :unloaded (:why %)) (>= (:y (:pos %)) floor-y)) (:leaks checked))))))

(defn seen-leak?
  "Whether the final read of a pen still waiting on unseen cells names a leak, other than the start cell at, at a cell the
  body has seen: the way out is known and the unseen cells lie past it. An open gate is no proof: it may divide the
  pen, with more of the pen unseen beyond it."
  [p final at]
  (boolean (some (fn [{:keys [pos why]}]
                   (let [start (if (map? at) [(:x at) (:y at) (:z at)] (vec at))
                         cell [(:x pos) (:y pos) (:z pos)]]
                     (and (not (#{:unloaded :open-gate} why)) (not= cell start) (not (dig-look/unknown? p cell)))))
                 (:leaks final))))

(defn answered?
  "Whether the read answer needs no more looks: closed, or a leak at a cell the body has seen (the unseen ground beyond it
  does not matter)."
  [p opts answer at]
  (boolean (or (:closed? answer)
               (seen-leak? p (if (= :unloaded (:reason answer)) (pen-read/read p (assoc opts :final? true)) answer) at))))

(defn leak-text
  "\": why at [x y z]\" listing the first leaks of result, with how many more."
  [{:keys [leaks leaks-total]}]
  (let [shown (take 3 leaks)
        more (- (or leaks-total (count leaks)) (count shown))]
    (when (seq shown)
      (str ": " (str/join ", " (map (fn [{:keys [pos why stand]}]
                                           (str (name why) " at [" (:x pos) " " (:y pos) " " (:z pos) "]"
                                                (when stand (str " (an animal steps onto the top from the ground at [" (:x stand) " " (:y stand) " " (:z stand) "])"))))
                                         shown))
           (when (pos? more) (str " and " more " more"))))))

(defn gate-drop-text
  [{:keys [gate-drops]}]
  (when-let [{:keys [gate drop]} (first gate-drops)]
    (str "; gate at [" (:x gate) " " (:y gate) " " (:z gate) "] has a drop of " drop " inside, animals will not step down")))

(defn herd-note
  "Why herd could not fill this closed pen (too shallow for any gate axis), or nil."
  [{:keys [closed? inside]}]
  (when (and closed? (seq inside))
    (let [[xs zs] (apply map vector (map (juxt first #(nth % 2)) inside))]
      (pen-fit/shallow-text {:min {:x (apply min xs) :z (apply min zs)} :max {:x (apply max xs) :z (apply max zs)}} nil))))

(defn animal-coverage
  "{:unseen-cells :animals-complete?}: the inside cells the answer reads from memory only, where an animal would not be listed."
  [{:keys [inside remembered]}]
  (let [n (count (filter inside remembered))]
    {:unseen-cells n :animals-complete? (zero? n)}))

(defn start-near
  "[x y z] the unloaded leaks are listed nearest to: the pen's spot, else the body."
  [c]
  (u/cell-vec (or (work-place/spot c) (u/self-pos c))))

(defn barrier-name? [n]
  (boolean (and n (or (str/ends-with? n "_fence") (str/ends-with? n "_wall")))))

(defn fence-near
  "The name of the nearest fence or wall seen within 3 cells of [x y z], else the first one carried, else nil."
  [p [x y z]]
  (let [near (sort-by (fn [[dx dz]] (+ (* dx dx) (* dz dz)))
                      (for [dx (range -3 4) dz (range -3 4)] [dx dz]))]
    (or (->> near
             (map (fn [[dx dz]] (some-> (u/sensed p [(+ x dx) y (+ z dz)]) .-name)))
             (filter barrier-name?)
             first)
        (first (filter barrier-name? (keys (storage/carried-totals (u/inventory p))))))))

(defn patch-plan
  "[{:op :place|:shut :pos [x y z] :item}] for the patchable leaks of result: a fence in each :gap cell and on top of each :climb or :perch-in fence, each open gate shut."
  [c result]
  (let [p (:primitives c)
        item (:item (:args c))]
    (vec (keep (fn [{:keys [pos why] :as leak}]
                 (if (= :open-gate why)
                   {:op :shut :pos [(:x pos) (:y pos) (:z pos)]}
                   (when-let [cell (pen-patch/patch-cell leak)]
                     (when-let [i (or item (fence-near p cell))] {:op :place :pos cell :item i}))))
               (:leaks result)))))

(defn ^:async patch-step!
  "One patch of the queue by a child (a pending child yields the round); the outcome is counted in the memory."
  [c]
  (let [{:keys [op pos item]} (first (:patch-queue (ctx/mem c)))
        shared (select-keys (:args c) [:ignore-zones?])
        r (await (if (= :shut op)
                   (ctx/call c :patch 'jobs.items.use (assoc shared :on pos :item "none"))
                   (pen-patch/place-fence! c pos item {:ways (into #{:carried} (filter #(ctx/way? c %)) [:chest :craft :gather])})))]
    (if (= :continue r)
      :continue
      (let [ok? (= :done (:kind r))
            k (cond (not ok?) :failed (= :shut op) :shut :else :placed)]
        (ctx/update-mem! c #(-> % (update :patch-queue rest) (update-in [:patched k] (fnil inc 0)) (update :pass-ok (fnil + 0) (if ok? 1 0))))
        :continue))))

(defn ^:async read-pen! [c]
  (let [{:keys [at]} (:args c)
        max-cells (settings/get settings ::max-cells)
        box (:box (pen-box c))
        near (or (:near (ctx/mem c)) (start-near c))
        _ (ctx/update-mem! c assoc :near near)
        p (:primitives c)
        opts {:at at :box box :max-cells max-cells :near near}
        checked (await (pen-read/read! c (assoc opts :early? true) #(not (answered? p opts % at))))
        final (when (= :unloaded (:reason checked))
                (pen-read/read p (assoc opts :final? true)))
        unseen (when-not (seen-leak? p (or final checked) at) (unseen-leak checked at))
        {:keys [walks last-unseen] :or {walks 0}} (ctx/mem c)
        stuck (if (= unseen last-unseen) walks 0)
        walk (when (and unseen (< stuck (settings/get settings ::max-walks)))
               (await (near/go-near! c unseen 1 {:doors :never :ways #{:walk :clear-seal}})))]
    (case walk
      :partial :continue
      :there (do (ctx/update-mem! c assoc :walks (inc stuck) :last-unseen unseen) :continue)
      (let [checked (or final checked)
            ;; a seen leak is the answer: the unseen ground beyond it is no extra finding
            checked (cond-> checked (= :leak (:reason checked)) (update :leaks #(vec (remove (comp #{:unloaded} :why) %))))
            rows (animals/in-pen-rows p checked (settings/get settings ::animal-radius))
            result (merge (pen/summary checked) (pen-read/listed checked (u/self-pos c)) {:animals rows :animals-in (count rows)} (animal-coverage checked)
                          (when (herd-note checked) {:herdable? false}))
            result (assoc result :text (if (:closed? result)
                                         (str "pen closed, " (:cells result) " cells" (gate-drop-text result)
                                              (when-let [note (herd-note checked)] (str "; but " note)))
                                         (str "pen not closed: " (name (:reason result)) (leak-text result) (gate-drop-text result))))]
        (ctx/update-mem! c assoc :cells (:cells result))
        (let [{:keys [patched pass pass-ok] :or {pass 0}} (ctx/mem c)
              queue (when (and (= :leak (:reason result)) (ctx/way? c :patch)
                               (pen-patch/another-pass? pass pass-ok (settings/get settings ::max-passes)))
                      (patch-plan c result))
              result (cond-> result patched (assoc :patched (merge {:placed 0 :shut 0 :failed 0} patched)))]
          (cond
            (seq queue) (do (ctx/update-mem! c assoc :patch-queue queue :pass (inc pass) :pass-ok 0) :continue)
            (or (nil? (:reason result)) (and (= :leak (:reason result)) (not (ctx/way? c :patch)))) (ctx/end c :done nil result)
            (= :no-start (:reason result)) (ctx/end c :condition-failed :no-start result)
            :else (ctx/end c :nothing-possible (:reason result) result)))))))

(defn ^:async round [c]
  (cond
    (:trouble (pen-box c)) :continue
    (seq (:patch-queue (ctx/mem c))) (await (patch-step! c))
    :else (await (read-pen! c))))
