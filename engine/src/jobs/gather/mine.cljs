(ns jobs.gather.mine
  (:require [clojure.string :as str]
            [engine.args :as a]
            [jobs.lib.work-place :as work-place]
            [jobs.lib.waits :as waits]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.gather.dig-for :as dig]
            [jobs.gather.mine.descent :refer [descent-wanted? shaft-top-here?]]
            [jobs.gather.shaft-descent :refer [pick-shaft]]
            [jobs.gather.mine.end :as end :refer [end-run wanted]]
            [jobs.gather.mine.mend :as mend]
            [jobs.gather.mine.way-set :as mw]
            [jobs.gather.mine.targets :refer [owed carried got had zone-margin within-keep-clear scan items main-block]]
                        [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.child :as child]
            [jobs.lib.look :refer [cell-of heading-name]]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.price :as price]
            [jobs.lib.ready :as ready]
            [jobs.lib.torch :as torch]
            [jobs.lib.tools :as tools]
            [jobs.lib.torch-stock :as stock]
            [jobs.lib.trip :as trip]
            [jobs.lib.util :as u]
            [jobs.lib.access.zones :as zones]))

(def doc
  "Mine like a player: a trip that gets what the dig needs, calls jobs.gather.dig-for, and comes back. It remembers the cell it
  starts on, deposits when the bag is nearly full (jobs.lib.trip), gets a pickaxe when none is carried that harvests the block, a spare
  for a worn last one, and torches before a descent or tunnel (all through jobs.lib.need; ways #{:carried} skips them), walks out of an own
  zone it stands near, walks back to the start cell, and calls jobs.gather.dig-for with the same args until it ends. Flooded
  at the start, or a pickaxe or torches that cannot be had, end it before the dig. A dig that stops for a pickaxe or torches (:no-tool,
  :no-torches) gets them again and goes on from where it stood (at most ::max-equip-rounds rounds); any other ending is passed on.
  Then, with :mend, it fills the ground under the start that the dig opened (mine.mend; a mend with nothing to fill with or six failed
  fills warns mine.mend-short / mine.mend-failed, and never changes the ending). The body stays where the dig ends: walking home is the
  caller's, or the :stuck-underground trigger's.

  Needs and impossible: those of jobs.gather.dig-for; its can-start? checks stand here too. The run's own area is :center / :radius.

  Ends :done when :got meets :count (or the call's :least); else the dig's ending (condition-failed for the facts
  :none :wet :refused :own-zone :no-shaft :flooded :no-tool :no-torches, nothing-possible for
  failed tries). Data {:got n :reason r :mended n} plus the dig's data; text says reason, :got, :mended, the stair and the
  tunnel. Quote: about ::item-s s an item, open-ended.")

(a/defargs args
  {:block :shared
   :item :shared
   :count :shared
   :radius :shared
   :center :shared
   :wet :shared
   :skip {:doc "cells [x y z] never targeted (a caller passes the ones an earlier call gave up on: :skipped of its result)" :spec (a/coll-of (a/pred "a cell [x y z]" #(and (sequential? %) (= 3 (count %)) (every? number? %)))) :default nil}
   :ignore-zones? :shared
   :mend {:doc "fill the ground under the start again once the dig is over" :spec boolean? :default false}
   :heading :shared
   :direction :shared
   :tunnel-length :shared
   :branches {:doc "on a level-keeping tunnel (no descending way): cut a side branch to each side of the tunnel every few blocks (mine-stay ::branch-every, ::branch-length), more rock in view per block walked" :spec boolean? :default false}
   :level :shared
   :shaft :shared
   :descend-limit :shared
   :torches :shared
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent :lava-adjacent :falling-block), a set or list; nil: the dig takes :fluid-adjacent and :falling-block, its stair only :falling-block (it stops or plugs beside water); the lava and :wet rules still hold"
            :spec (a/coll-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default nil}})


(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds a mine call may take by default (its :spend :job-time)." :spec (a/num-in 0 nil)}
   ::item-s {:default 6 :doc "Mean seconds one item takes (walk, dig, collect), for the quote." :spec (a/num-in 0 nil)}
   ::max-equip-rounds {:default 3 :doc "Times the trip gets a pickaxe or torches again for a dig that stopped without them before it ends with the dig's ending." :spec (a/int-in 0 nil)}})

(def target [:item :block :heading :center])

(def ways
  "The ways the dig may use (the call's :ways, default all): :seen (dig what is in view), :descend-stair (stair down to the ore's level, or through soil to stone; :descend-limit steps), :descend-shaft (down a mineshaft plan, :shaft names it; with no :descend-stair and no usable shaft the run ends :no-shaft before anything is dug), :strip-tunnel (a tunnel at the start level, :tunnel-length blocks). With :strip-tunnel and no descent the tunnel keeps its level: seen ore only beside it (or by a short side cut), and a cut that opens into air beyond it is sealed with a carried block."
  (into mw/ordered need/fetch-ways))

(def defaults {:least :all :spend {:job-time ::job-time-s}})

(def safe? (price/staying :long (ready/safe-at-or work-place/dig-spot ready/safe-if-lit true)))

(def fit? ready/fit?)

;; ------------------------------------------------------------------ the contract

(def can-start? dig/can-start?)

(defn done?
  "Progress in items: got against :count carried (as the dig left it: a mend that spends some does not change it). :ok at once when :count is carried before the trip began, else once it has finished
  (:finished in memory) with the wanted items carried; else :later, the round ends the run."
  [c]
  (let [n (:count (:args c))
        m (ctx/mem c)
        got (if (:goal m) (or (:dug-got m) (got c)) 0)
        extra {:units :items :done got :left (max 0 (- (owed c) got)) :result {:got got}}]
    (if (or (and (:finished m) (>= got (wanted c)))
            (and (not (:goal m)) (>= (carried c) n)))
      (apply contract/ok :mined (str "mine: " got " of " n) (mapcat identity extra))
      (apply contract/later :mining (str "mine: " got " of " n ", the round ends the run") (mapcat identity extra)))))

(defn quote-of
  "Finding out: about ::item-s per item, open-ended (a tunnel may find none); yields up to :count of the item."
  [c]
  (let [{:keys [count] :as a} (:args c)
        mean (* count (settings/get settings ::item-s))]
    {:uses {:job-time [(min count mean) mean ##Inf]}
     :yields {:items {(first (items a)) [0 count count]}}
     :done [0 count count]}))

;; ------------------------------------------------------------------ equip

(defn stair-start?
  "Whether the dig starts with a stair of its own (not by a mineshaft, and no wanted block in sight to walk to instead): the start keeps clear of buildings."
  [c]
  (and (descent-wanted? c) (or (not (mw/shaft? (:args c))) (boolean (:none (pick-shaft c)))) (empty? (:targets (scan c)))))

(defn ^:async leave-zone!
  "Before the first dig: a go-to child to a cell zone-margin outside the own zone (or, for a stair, building) zn the body stands near (the start moves with it).
  :continue while it walks, :again once out, else the trip ends :zone-exit."
  [c zn]
  (let [m (zone-margin)
        goal (zones/exit-goal (cell-of (u/self-pos c)) zn m)
        from (:start (ctx/mem c))
        r (await (child/call-walk* c :leave
                                   {:pos {:x (+ (:x goal) 0.5) :y (:y goal) :z (+ (:z goal) 0.5)} :range 2 :settings {:jobs.movement.go-to.tuning/y-slack 8}}))
        here (cell-of (u/self-pos c))]
    (cond
      (= :continue r) :continue
      (not (within-keep-clear c (dec m) (access/cell here) (stair-start? c)))
      (do (ctx/emit! c :mine.left-zone :info {:zone (:name zn) :from (access/cell from) :to (access/cell here)
                                              :text (str "mine walked out of zone " (:name zn) " to " (str/join "," (access/cell here)) " before it digs")})
          (ctx/update-mem! c assoc :start here :resume-at here)
          :again)
      :else (end-run c {:got 0 :reason :zone-exit :zone (:name zn) :cell (access/cell here) :mended 0
                        :text (str "mine could not get out of zone " (:name zn) " before it digs")}))))

(defn ^:async back-to-resume!
  "After a fetch walked the body off: a go-to child back to the cell the dig goes on from (mem :resume-at). :continue while it walks,
  :again once there (go-to's own :arrived counts: water can push the body into the next cell after it arrived), else the trip ends :not-back."
  [c]
  (let [{:keys [x y z] :as cell} (:resume-at (ctx/mem c))
        r (await (child/call-walk* c :back
                                   {:pos {:x (+ x 0.5) :y y :z (+ z 0.5)} :range 0
                                    :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (cond
      (= :continue r) :continue
      (= (access/cell cell) (access/cell (cell-of (u/self-pos c)))) :again
      (get-in r [:data :arrived]) (do (ctx/update-mem! c assoc :resume-at (cell-of (u/self-pos c))) :again)
      :else (end-run c {:got 0 :reason :not-back :cell (access/cell cell) :mended 0
                        :text (str "mine could not get back to " (str/join "," (access/cell cell)) " after getting its tools")}))))

(defn ^:async fetch-pickaxe!
  "A pickaxe is owed: jobs.items.get-tool for the block through jobs.lib.need (the ways' sources apply), once per equip. :continue
  while it runs, else nil; a failed fetch is kept as the dig's :why (mem :tool-why)."
  [c]
  (let [block (main-block (:args c))
        o (need/fetch-opts c)]
    (when (and o (not (tools/can-harvest? (:primitives c) block)) (not (:tool-why (ctx/mem c))))
      (let [e (await (need/tool! c {:block block} o))]
        (cond
          (= :continue e) :continue
          (tools/can-harvest? (:primitives c) block) nil
          :else (do (ctx/update-mem! c assoc :tool-why (or (when (map? e) (contract/cause-key (or (:because e) e))) :unknown)
                                          :tool-ending (when (map? e) (dissoc (or (:because e) e) :report)))
                    nil))))))

(defn digs-left
  "The digs the call still has to make: items short of the goal, else :count (an ore drops at least one item per dig)."
  [c]
  (max 1 (- (:count (:args c)) (carried c))))

(defn ^:async equip!
  "What the dig needs, in order: room in the bag, a pickaxe, a spare, torches. :continue while one waits on the world, else nil."
  [c]
  (or (await (trip/ready! c))
      (await (trip/shed! c))
      (await (fetch-pickaxe! c))
      (await (trip/tool-ready! c (main-block (:args c)) (digs-left c)))
      (await (when (and (:torches (:args c)) (nil? (:tunnel (ctx/mem c))) (or (mw/tunnel? (:args c)) (descent-wanted? c)))
               (stock/torches! c)))))

(defn stop-cause
  "Why the dig cannot begin or go on after the equip, as {:reason :text ..} (jobs.gather.dig-for/stop-cause), or for a dig that stopped
  for torches while none are carried."
  [c]
  (or (dig/stop-cause c)
      (when (and (= :no-torches (:wanted (ctx/mem c))) (zero? (torch/torches-held (:primitives c))))
        {:reason :no-torches :text "dig-for stopped: no torches left and none could be had; the trip got no more"})))

;; ------------------------------------------------------------------ the end

(defn to-end!
  "The dig is over with reason (cause: fields of a stop before a dig, else nil): the mend next with :mend, else the end."
  [c reason cause]
  (ctx/update-mem! c assoc :phase (if (:mend (:args c)) :mend :end) :reason reason :end-cause cause :dug-got (got c))
  :again)

(defn dig-text
  "The dig's end text as the trip's: its leading \"dig-for\" is \"mine\"."
  [text]
  (str "mine" (str/replace-first text #"^dig-for" "")))

(defn finish!
  "End the trip: the dig's ending in memory and the mend's count."
  [c]
  (let [{:keys [dig-end dig-ending tool-ending end-cause reason mended tool-why dug-got]} (ctx/mem c)
        dug (let [d (merge dig-end end-cause)]
              (if (seq (:left d))
                (let [left (end/still-lying c (:left d))]
                  (if (seq left) (assoc d :left left) (dissoc d :left)))
                d))
        n (or dug-got (got c))
        mended (or mended 0)
        data (cond-> (merge (dissoc dug :text) {:got n :reason reason :mended mended})
               (and tool-why (= :no-tool reason)) (assoc :why tool-why))
        text (str (dig-text (or (:text dug) (str "dig-for got " n " of " (owed c) ": " (name reason))))
                  (when (pos? mended) (str "; mended " mended)))
        because (if (and tool-ending (= :no-tool reason)) tool-ending dig-ending)
        e (end-run c (assoc data :text text))]
    (if (and because (not= :done (:kind e)) (not= :done (:kind because)))
      (contract/with-because e because)
      e)))

;; ------------------------------------------------------------------ the steps

(defn dig-args
  "The args of the dig-for call: the trip's, without :mend, tracking the ground when it mends, a tunnel keeping its heading."
  [c]
  (let [{:keys [dug-heading]} (ctx/mem c)]
    (cond-> (-> (:args c)
                (dissoc :mend :submitted-at)
                (assoc :track-ground (boolean (:mend (:args c))) :least (:least (ctx/frozen-call c) :all)
                       :shed (not (:shed-failed (ctx/mem c)))))
      (and dug-heading (nil? (:heading (:args c))) (nil? (:direction (:args c)))) (assoc :heading dug-heading))))

(defn booked
  "The memory m after a dig ending e: its data kept, its ground and stair steps added to what earlier calls opened."
  [m e]
  (let [d (:data e)
        known (into #{} (map :pos) (:ground m))]
    (-> m
        (assoc :dig-end d)
        (update :ground (fnil into []) (remove #(known (:pos %))) (:ground d))
        (update-in [:descent :steps] #(max (or % 0) (get-in d [:descent :steps] 0)))
        (cond-> (get-in d [:tunnel :heading]) (assoc :dug-heading (get-in d [:tunnel :heading]))))))

(defn gettable?
  "Whether the trip may get again what a dig that stopped for reason lacked."
  [c reason]
  (and (< (:rounds (ctx/mem c) 0) (settings/get settings ::max-equip-rounds))
       (case reason
         :no-tool (some? (need/fetch-opts c))
         :no-torches (and (:stocked? (ctx/mem c)) (stock/fetch-allows? c))
         false)))

(defn shed-again?
  "Whether a dig that stopped for reason :surplus goes on after the pack is cleared (not after a clearing that freed nothing)."
  [c reason]
  (and (= :surplus reason) (not (:shed-failed (ctx/mem c)))))

(defn ^:async dig-step!
  "One round of the dig-for child; its ending is kept. A stop for a pickaxe or torches that the trip may get again goes back to the
  equip, to dig on from where the dig stopped; any other ending goes to the mend or the end."
  [c]
  (let [e (waits/waiting-out c (await (ctx/call c :dig 'jobs.gather.dig-for (dig-args c))))]
    (if (= :continue e)
      :continue
      (let [reason (or (:reason (:data e)) (contract/cause-key e))]
        (ctx/update-mem! c booked e)
        (ctx/update-mem! c assoc :dig-ending (dissoc e :report))
        (if (shed-again? c reason)
          (do (ctx/update-mem! c #(-> % (assoc :phase nil :resume-at (cell-of (u/self-pos c)) :wanted reason)
                                      (dissoc :equipped?)))
              :again)
        (if (gettable? c reason)
          (do (ctx/update-mem! c #(-> % (update :rounds (fnil inc 0))
                                      (assoc :phase nil :resume-at (cell-of (u/self-pos c)) :wanted reason)
                                      (dissoc :equipped? :torches-tried :tool-why :tool-ending)))
              :again)
          (to-end! c reason nil)))))))

(defn ^:async equip-step!
  "The phase before each dig: equip once, end on a stop the equip did not cure, walk back to where the dig goes on from, out of an own
  zone near the first start, then begin the dig."
  [c]
  (let [m (ctx/mem c)
        e (when-not (:equipped? m) (await (equip! c)))
        stop (when (:equipped? m) (stop-cause c))
        at-resume? (= (access/cell (:resume-at m)) (access/cell (cell-of (u/self-pos c))))
        own (when-not (:zoned? m) (within-keep-clear c (dec (zone-margin)) (access/cell (cell-of (u/self-pos c))) (stair-start? c)))]
    (cond
      (= :continue e) :continue
      (not (:equipped? m)) (do (ctx/update-mem! c #(cond-> (assoc % :equipped? true) (pos? (torch/torches-held (:primitives c))) (assoc :stocked? true))) :again)
      stop (to-end! c (:reason stop) (select-keys stop [:tool :why :text]))
      (not at-resume?) (await (back-to-resume! c))
      (and own (not (shaft-top-here? c))) (await (leave-zone! c own))
      :else (do (ctx/update-mem! c #(cond-> (assoc % :zoned? true :phase :dig) (not (:goal %)) (assoc :goal (:count (:args c)) :had (carried c))))
                :again))))

(defn ^:async mend-step!
  "The mend of the ground the dig opened, then the end."
  [c]
  (let [r (await (mend/mend-round! c))]
    (if (nil? r)
      (finish! c)
      r)))

(defn ^:async step [c]
  (case (:phase (ctx/mem c))
    :dig (await (dig-step! c))
    :mend (await (mend-step! c))
    :end (finish! c)
    (await (equip-step! c))))

(defn ^:async round
  [c]
  (when-not (:start (ctx/mem c))
    (let [start (cell-of (u/self-pos c))]
      (ctx/update-mem! c assoc :start start :resume-at start)))
  (await (pace/steps! c #(step c))))

(defn ^:async cleanup
  "A run cut by the limit, a cancel or a parent's end stays where it is."
  [c _]
  (ctx/end c :done nil {}))

(def meta {:tags [:mine :dig :ore :light] :position-bound? true})
