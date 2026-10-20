(ns jobs.lib.dig-in-leave
  "Leaving the shelter jobs.survival.shut-in built for the night (leave!): jobs.survival.night calls it by day."
  (:require [jobs.lib.access :as access]
            [jobs.lib.waits :as waits]
            [jobs.lib.tidy :as tidy]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.lib.child :as child]
            [jobs.lib.ready :as ready]
            [jobs.lib.shelter.inside :as sh-inside]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]))

(def max-climb
  "Stair steps cut one at a time out of a roofed shaft with no recorded :start before leave! gives up."
  32)

(def access-reasons #{:zone :claim :footprint :no-zones})

(def shut-in? sh-inside/shut-in?)

(def sheltered-in sh-inside/sheltered-in)

(defn leave-unsafe
  "Why the shelter must stay shut now: :night; nil when the body may leave. Hostiles do not keep it shut: by day the
  body leaves whatever is around, and a real danger is the hostile reflex's to deal with, as anywhere else."
  [p]
  (when (sh-sense/dark-outside? p) :night))

(def headings {:north [0 -1] :east [1 0] :south [0 1] :west [-1 0]})

(defn heading-order
  "The four headings, the one nearest the direction from feet to toward first (north, east, south, west without one)."
  [feet toward]
  (if-not toward
    [:north :east :south :west]
    (let [dx (- (:x toward) (:x feet))
          dz (- (:z toward) (:z feet))]
      (vec (sort-by (fn [h] (let [[hx hz] (headings h)] (- (+ (* hx dx) (* hz dz)))))
                    [:north :east :south :west])))))

(defn exit-attempts
  "The stairs to try: every heading respecting zones, then every heading with :ignore-zones? (the last resort, taken
  only when one of the first stopped for an access reason)."
  [order]
  (into (mapv (fn [h] {:heading h :ignore-zones? false}) order)
        (mapv (fn [h] {:heading h :ignore-zones? true}) order)))

(defn leave-mem [c] (:dig-out (ctx/mem c)))
(defn update-leave! [c f & args] (ctx/update-mem! c #(apply update % :dig-out f args)))

(defn leave-result!
  "End leave!: clear its memory, emit the event of its reason, resolve to the result map."
  [c status reason detail]
  (let [{:keys [x y z]} (sh-sense/feet (:primitives c))
        result (merge {:status status :reason reason :at [x y z]} detail)]
    (ctx/update-mem! c dissoc :dig-out)
    (case reason
      :out (ctx/emit! c :dig-in.left :info (assoc result :text "out of the shelter"))
      :unsafe (ctx/emit! c :dig-in.staying :info (assoc result :text (str "keeping the shelter shut: " (name (:why detail)))))
      (ctx/emit! c :dig-in.trapped :warn (assoc result :text "no way out of the shelter")))
    result))

(defn note-trespass!
  "Note the cells a last-resort stair dug in another's zone or claim (its :dug [{:cell :block}]) as :tidy entries,
  so jobs.survival.restore-broken puts them back."
  [c dug]
  (doseq [{:keys [cell block]} dug
          :let [pos (zipmap [:x :y :z] cell)
                v (tidy/refusal c :dig pos)]
          :when v]
    (tidy/record! c (merge {:cell cell :action :dig :was block} (select-keys v [:zone :claim :plan])) "air")))

(defn ^:async open-door!
  "Dig the first solid :door cell of a walled shelter (its own block); once both are open, step through them."
  [c door]
  (let [p (:primitives c)
        cell (first (filter #(sh-sense/solid-at? p %) door))]
    (if cell
      (let [_ (await (tools/equip-for! c (u/seen-name p cell) {:fast true}))
            r (await (tidy/dig! c cell))]
        (if (= "dug" (.-status r))
          :continue
          (leave-result! c :stopped :no-way-out {:tries [{:cell cell :reason (keyword (.-status r))}]})))
      (let [{:keys [x y z]} (sh-sense/feet p)
            [d] door
            beyond {:x (+ (:x d) (- (:x d) x)) :y y :z (+ (:z d) (- (:z d) z))}]
        ;; raw moveTo kept: a step into the cell the job is digging, range 0.5, inside its own pit; the planner has no standable goal there.
        (await (ctx/act c :moveTo (clj->js {:pos (if (sh-sense/solid-at? p (update beyond :y dec)) beyond d) :range 0.5})))
        (update-leave! c assoc :stepped true)
        :continue))))

;; Taking the shelter's valuable blocks back (the :take-back cells of the latest :shelter entry).

(def take-back-reach
  "Blocks from the feet within which a wall block is dug without a walk."
  4)

(def take-back-radius
  "Blocks from the shelter cell within which the dug blocks' drops are collected."
  8)

(defn take-back-state
  "The take-back memory of the latest :shelter entry: a newer entry (a shelter sealed since) starts afresh."
  [c]
  (let [tb (:take-back (ctx/mem c))]
    (if (= (:for tb) (:t (ctx/latest c :shelter))) tb {})))

(defn update-take-back! [c f & args]
  (ctx/update-mem! c assoc :take-back (assoc (apply f (take-back-state c) args) :for (:t (ctx/latest c :shelter)))))

(defn take-back-skip
  "Why the placed block at cell must stay: :floor (the cell under the feet), a refusal reason (another's zone, claim or
  plan: :zone :claim :footprint, or any other the rules give), :hazard (lava or fluid beside it, or nothing solid under
  it), :no-tool (no carried tool harvests item: a dig would lose it); nil when it may be dug."
  [c cell item]
  (let [p (:primitives c)
        {:keys [x y z]} (sh-sense/feet p)
        v (access/may? (access/rules-input c {:ignore-zones? false}) :dig cell)]
    (cond
      (= [(:x cell) (:y cell) (:z cell)] [x (dec y) z]) :floor
      (not (tools/can-harvest? p item)) :no-tool
      (and (not (:ok v)) (not= :no-zones (:reason v))) (:reason v)
      (seq (:hazards v)) :hazard)))

(defn take-back-cells
  "The :take-back cells of the latest :shelter entry that still hold the placed item and were not given up on, nearest first."
  [c]
  (let [p (:primitives c)
        entry (:data (ctx/latest c :shelter))
        skipped (:skipped (take-back-state c))]
    (->> (:take-back entry)
         (remove #(contains? skipped (:cell %)))
         (filter #(= (:item %) (u/seen-name p (:cell %))))
         (sort-by #(u/dist (sh-sense/feet p) (:cell %))))))

(defn skip-cell! [c cell reason]
  (update-take-back! c update :skipped (fnil assoc {}) cell reason)
  (ctx/emit! c :dig-in.take-back-skipped :debug {:cell cell :reason reason :text (str "left the shelter block at " (pr-str cell) ": " (name reason))}))

(defn ^:async take-back!
  "One round of digging the shelter's valuable blocks again: the nearest one still standing (a walk when out of reach, a
  cell the walk or the dig fails on is skipped with a dig-in.take-back-skipped debug), then one collect-drops call.
  Resolves to :continue while working, else nil."
  [c]
  (let [p (:primitives c)
        tb (take-back-state c)
        {:keys [cell item] :as target} (first (take-back-cells c))]
    (cond
      (:done tb) nil
      (not target)
      (if (and (pos? (:dug tb 0)) (not (:collected tb)))
        (let [r (waits/waiting-out c (await (ctx/call c :take-back-collect 'jobs.items.collect-drops
                                       {:radius take-back-radius :near (:pos (:data (ctx/latest c :shelter))) :items (vec (distinct (map :item (:take-back (:data (ctx/latest c :shelter))))))})))]
          (if (= :continue r)
            :continue
            (do (update-take-back! c assoc :collected true :done true)
                (ctx/emit! c :dig-in.took-back :info {:dug (:dug tb) :text (str "took back " (:dug tb) " shelter blocks")})
                nil)))
        (do (update-take-back! c assoc :done true) nil))
      (> (u/dist (sh-sense/feet p) cell) take-back-reach)
      (let [r (await (child/call-walk* c :take-back-walk {:pos cell :range 3 :ways #{:walk :clear-seal} :spend {:tries 1}}))]
        (cond
          (= :continue r) :continue
          (= :done (:kind r)) :continue
          :else (do (skip-cell! c cell :unreachable) :continue)))
      (take-back-skip c cell item)
      (do (skip-cell! c cell (take-back-skip c cell item)) :continue)
      :else
      (let [_ (await (tools/equip-for! c item {:fast true}))
            r (await (tidy/dig! c cell))]
        (if (= "dug" (.-status r))
          (update-take-back! c update :dug (fnil inc 0))
          (skip-cell! c cell (keyword (.-status r))))
        :continue))))

(defn ^:async climb!
  "One stair attempt out of a pit (jobs.access.stair :up, a child of the caller): to the :start height, or with no
  :start one step at a time until nothing solid is within sh-sense/default-roof-height above, at most max-climb steps. A
  stopped stair books its reason and the next heading is tried; so does one that cannot start (it lacks a tool or a slot),
  instead of waiting, because the caller (jobs.survival.night) must end failed, not wait, when trapped."
  [c {:keys [start]} toward]
  (let [{:keys [i order tries climbed] :or {i 0 tries [] climbed 0}} (leave-mem c)
        order (or order (heading-order (sh-sense/feet (:primitives c)) toward))
        _ (update-leave! c assoc :order order)
        attempt (get (exit-attempts order) i)]
    (cond
      (or (nil? attempt) (and (:ignore-zones? attempt) (not (some #(access-reasons (:reason %)) tries))))
      (leave-result! c :stopped :no-way-out {:tries tries})
      (>= climbed max-climb)
      (leave-result! c :stopped :no-way-out {:tries (conj tries {:reason :too-deep :heading (:heading attempt)})})
      :else
      (let [slot (keyword (str "dig-out-" i))
            _ (update-leave! c assoc :heading (:heading attempt))
            e (waits/waiting-out c (await (ctx/call c slot 'jobs.access.stair
                                     (merge {:dir :up :checks (ready/unforced c {:fit? '(always)}) :heading (:heading attempt) :ways #{:carried} :ignore-zones? (or (:ignore-zones? attempt) (:ignore-zones? (:args c)))}
                                            (if start {:y (:y start)} {:steps 1})))))
            data (:data e)]
        (when (and (not= :continue e) (:ignore-zones? attempt)) (note-trespass! c (:dug data)))
        (cond
          (= :continue e) :continue
          (= :done (:kind e)) (do (update-leave! c update :climbed (fnil + 0) (:steps data 0)) :continue)
          :else (do (update-leave! c #(-> % (assoc :i (inc i))
                                          (update :tries (fnil conj [])
                                                  (merge {:heading (:heading attempt)} (select-keys data [:cell :heading])
                                                         {:reason (contract/cause-key e)}))))
                    :continue))))))

(defn ^:async leave!
  "One round of getting the body out of the shelter shut-in built. The shelter job calls it by day.
  Progress is in the caller's job memory under :dig-out. The stair is its child :dig-out-<i>.
  Resolves to :continue while working, else {:status :done|:stopped :reason :out|:unsafe|:no-way-out :at [x y z]}
  plus :why (:unsafe), :heading (the stair that got it out) or :tries ({:heading :reason :cell} per stopped stair).
  The shelter is the latest :shelter entry when the body stands in its :pos and is shut in. Without one: done, :out.
  At night nothing is dug: stopped, :unsafe, :why :night. By day it leaves whatever hostiles are around.
  - A walled shelter: digs its :door cells (feet, then head), then steps through to the cell beyond if it has a floor;
    still shut in by a pit :start with the door open, it climbs.
  - A pit (:start) or a roofed shaft (no :start, no :door): a stair up (jobs.access.stair :up) to the start height.
    A shaft goes one step at a time until nothing solid is within 4 above (at most 32 steps, else :too-deep).
    The heading nearest :toward goes first. The stair refuses lava, water, falling blocks and a missing floor,
    and then the next heading is tried.
  - Only when a heading stopped for an access reason (:zone :claim :footprint :no-zones),
    all four are tried again with :ignore-zones?, the survival last resort.
    The cells dug in another's zone or claim are noted as :tidy entries.
  The pit and the stair are left dug. Out of it, with :take-back? (default true), the valuable blocks the shelter placed
  (the :take-back cells of its entry, still the placed block) are dug and their drops collected (jobs.items.collect-drops);
  a cell it cannot reach or dig is left, with a debug dig-in.take-back-skipped. Events: dig-in.took-back (info).
  Events: dig-in.left (info), dig-in.staying (info, unsafe), dig-in.trapped (warn, no way out)."
  ([c] (leave! c {}))
  ([c {:keys [toward take-back?] :or {take-back? true}}]
   (let [p (:primitives c)
         entry (or (:entry (leave-mem c)) (sheltered-in c))
         _ (when entry (update-leave! c assoc :entry entry))
         shut? (and entry (shut-in? p entry))
         step? (and entry (:door entry) (not shut?) (not (:stepped (leave-mem c))))
         why (when shut? (leave-unsafe p))]
     (cond
       step? (await (open-door! c (:door entry)))
       (and (not shut?) take-back? (not (leave-unsafe p)) (= :continue (await (take-back! c)))) :continue
       (not shut?) (leave-result! c :done :out (select-keys (leave-mem c) [:heading]))
       why (leave-result! c :stopped :unsafe {:why why})
       (some #(sh-sense/solid-at? p %) (:door entry)) (await (open-door! c (:door entry)))
       :else (await (climb! c entry toward))))))
