(ns jobs.gather.mine
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.gather.dig-for :as dig]
            [jobs.gather.mine.descent :refer [descent-wanted? shaft-top-here?]]
            [jobs.gather.mine.end :as end :refer [end-run wanted]]
            [jobs.gather.mine.home :as home]
            [jobs.gather.mine.mend :as mend]
            [jobs.gather.mine.targets :refer [owed carried got had zone-margin within-own-zone]]
                        [jobs.lib.access :as access]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.child :as child]
            [jobs.lib.look :refer [cell-of heading-name]]
            [jobs.lib.need :as need]
            [jobs.lib.pace :as pace]
            [jobs.lib.ready :as ready]
            [jobs.lib.torch :as torch]
            [jobs.lib.tools :as tools]
            [jobs.lib.trip :as trip]
            [jobs.lib.util :as u]
            [jobs.lib.access.zones :as zones]))

(def doc
  "Mine like a player: a trip that gets what the dig needs, calls jobs.gather.dig-for, and comes back. It remembers the cell it
  starts on, deposits when the bag is nearly full (jobs.lib.trip), gets a pickaxe when none is carried that harvests the block, a spare
  for a worn last one, and torches before a descent or tunnel (all through jobs.lib.need; :fetch false skips them), walks out of an own
  zone it stands near, walks back to the start cell, and calls jobs.gather.dig-for with the same args until it ends. Hungry or flooded
  at the start, or a pickaxe or torches that cannot be had, end it before the dig. A dig that stops for a pickaxe or torches (:no-tool,
  :no-torches) gets them again and goes on from where it stood (at most ::max-equip-rounds rounds); any other ending is passed on.
  Then, with :mend, it fills the ground under the start that the dig opened (mine.mend), and always walks back to the start cell (a go-to
  child with every way but a pillar first); the walk is the cleanup for a run cut by the limit or a cancel. Not arriving ends :not-home
  (warn mine.not-home); a mend with nothing to fill with or six failed fills warn mine.mend-short / mine.mend-failed.

  Needs and impossible: those of jobs.gather.dig-for; its can-start? checks stand here too. The run's own area is :center / :radius.

  Ends :done when :got meets :count (or the call's :least) and the body is home; else the dig's ending (condition-failed for the facts
  :none :wet :refused :own-zone :no-shaft :flooded :no-tool :no-torches, limit-reached {:line :food} for :hungry, nothing-possible for
  failed tries) or :not-home. Data {:got n :reason r :mended n} plus the dig's data; text says reason, :got, :mended, the stair and the
  tunnel. Quote: about ::item-s s an item, open-ended.")

(a/defargs args
  {:block {:doc "name of the block to mine (required)" :spec a/name? :default nil}
   :item {:doc "the item the block drops; nil: the drop-item table, else the block name" :spec a/item? :default nil}
   :count {:doc "how many to carry at least (carried at the start count)" :spec (a/int-in 0 nil) :default 8}
   :radius {:doc "seen blocks within this many blocks of the body count" :spec (a/num-in 0 nil) :default 16}
   :center {:doc "the centre of the area the job works, a cell [x y z] or {:x :y :z}; nil: where the body stands when the job first runs (jobs.lib.area)" :spec a/position? :default nil}
   :wet {:doc "dig blocks that touch water, and tunnel beside water" :spec boolean? :default false}
   :pick-up-ores {:doc "true: dig the ores of jobs.lib.ore-pick's ::ores setting that are exposed within reach of where it stands; false: none; or the ore block names to dig" :spec (a/or-of boolean? (a/coll-of a/name?)) :default true}
   :fetch {:doc "get a pickaxe when none is carried (jobs.items.get-tool through jobs.lib.need), a spare for a worn one, and torches: true, a set of kinds or a map of which only :how and :explore apply; false: the dig ends with what is carried" :spec need/option? :default true}
   :ignore-zones? :shared
   :mend {:doc "fill the ground under the start again once the dig is over" :spec boolean? :default false}
   :heading {:doc "the strip tunnel's heading, over :direction: north, south, east or west (n/s/e/w), or a cell [x y z] (the dominant axis from the start toward it); nil: :direction" :spec (a/pred "a heading name or a cell [x y z]" #(or (and (or (string? %) (keyword? %)) (heading-name %)) (and (sequential? %) (= 3 (count %)) (every? number? %)))) :default nil}
   :direction {:doc "the strip tunnel's heading: north, south, east or west (n/s/e/w); nil: the way the body faces when the job starts" :spec (a/pred "north, south, east or west (n/s/e/w), a string or keyword" #(and (or (string? %) (keyword? %)) (heading-name %))) :default nil}
   :tunnel-length {:doc "the most blocks the strip tunnel runs in this job, at the body's level; 0: no tunnel, seen blocks only" :spec (a/int-in 0 nil) :default 32}
   :stay-level {:doc "caution for caves: dig only at the level the body starts at (the two-high tunnel, no stair down, no :level), mine only seen ore beside the body (:ore-reach: or a short side cut out), and seal a dug cell that opens into air beyond the tunnel with a carried block (tunnel ends :breach-sealed, or :breach-open when it cannot seal)" :spec boolean? :default false}
   :side-cut? {:doc "set by a run's own side cut (a child mine run starting inside its tunnel): skips the check that :stay-level starts under cover" :spec boolean? :default false}
   :ore-reach {:doc "with :stay-level: seen ore this many blocks into the wall beside the tunnel is taken by a short side cut (a child mine run), shut behind with a carried block; 0 or 1: only ore beside the body" :spec (a/int-in 0 nil) :default 2}
   :variants {:doc "an ore's stone and deepslate kinds both count as the block (iron_ore and deepslate_iron_ore, either named); false: only :block" :spec boolean? :default true}
   :level {:doc "the feet height the strip tunnel runs at (a whole number, or a cell [x y z] / {:x :y :z} whose y it takes): with no target seen the body first stairs down to it (jobs.access.stair) when higher; nil: the ore's level (setting mine.descent/ore-levels), none for other blocks; false: where the body stands"
           :spec (a/pred "a whole number, a cell [x y z] or {:x :y :z}, nil or false" #(or (nil? %) (false? %) (integer? %) (and (sequential? %) (= 3 (count %)) (integer? (second %))) (and (map? %) (integer? (:y %))))) :default nil}
   :shaft {:doc "a mineshaft plan id to descend by instead of digging a stair (jobs.lib.shafts); nil: the nearest usable one within reach, when a descent is due; false: none; :required: the nearest usable one, and no shaft usable ends :no-shaft before anything is dug" :spec (a/pred "a shaft plan id, nil, false or :required" #(or (nil? %) (false? %) (= :required %) (string? %))) :default nil}
   :descend-limit {:doc "the most steps of stair down through soil to find stone, when the block is stone-type and none is in sight; 0: never descend" :spec (a/int-in 0 nil) :default 12}
   :torch-interval {:doc "the strip tunnel hangs a torch every this many steps; 0: none" :spec (a/int-in 0 nil) :default 10}
   :accept {:doc "dig hazards of jobs.lib.access.rules taken (:fluid-adjacent :falling-block); the lava and :wet rules above still hold"
            :spec (a/set-of #{:fluid-adjacent :lava-adjacent :falling-block}) :default #{:fluid-adjacent :falling-block}}})


(a/defargs settings
  {::job-time-s {:default 1800 :doc "Seconds a mine call may take by default (its :spend :job-time)." :spec (a/num-in 0 nil)}
   ::item-s {:default 6 :doc "Mean seconds one item takes (walk, dig, collect), for the quote." :spec (a/num-in 0 nil)}
   ::max-equip-rounds {:default 3 :doc "Times the trip gets a pickaxe or torches again for a dig that stopped without them before it ends with the dig's ending." :spec (a/int-in 0 nil)}})

(def target [:block :heading :center])

(def defaults {:least :all :spend {:job-time ::job-time-s}})

(def safe? ready/safe-if-lit)

(def fit? ready/fit?)

;; ------------------------------------------------------------------ the contract

(def can-start? dig/can-start?)

(defn done?
  "Progress in items: got against :count carried. :ok at once when :count is carried before the trip began, else once it has finished
  (:finished in memory) with the wanted items carried and the body home; else :later, the round ends the run."
  [c]
  (let [n (:count (:args c))
        m (ctx/mem c)
        got (if (:goal m) (got c) 0)
        extra {:units :items :done got :left (max 0 (- (owed c) got)) :result {:got got}}]
    (if (or (and (:finished m) (not= :not-home (:reason m)) (>= got (wanted c)))
            (and (not (:goal m)) (>= (carried c) n)))
      (apply contract/ok :mined (str "mine: " got " of " n ", home") (mapcat identity extra))
      (apply contract/later :mining (str "mine: " got " of " n ", the round ends the run") (mapcat identity extra)))))

(defn quote-of
  "Finding out: about ::item-s per item, open-ended (a tunnel may find none); yields up to :count of the item."
  [c]
  (let [{:keys [count] :as a} (:args c)
        mean (* count (settings/get settings ::item-s))]
    {:uses {:job-time [(min count mean) mean ##Inf]}
     :yields {:items {(blocks/item-name a) [0 count count]}}
     :done [0 count count]}))

;; ------------------------------------------------------------------ equip

(defn allows?
  "Whether :fetch allows getting things of kind (:tool or :item)."
  [c kind]
  (let [f (need/parse-arg (:fetch (:args c)))]
    (boolean (and (map? f) (not (:error f)) (or (nil? (:what f)) (contains? (:what f) kind))))))

(defn ^:async leave-zone!
  "Before the first dig: a go-to child to a cell zone-margin outside the own zone zn the body stands near (the start moves with it).
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
      (not (within-own-zone c (dec m) (access/cell here)))
      (do (ctx/emit! c :mine.left-zone :info {:zone (:name zn) :from (access/cell from) :to (access/cell here)
                                              :text (str "mine walked out of zone " (:name zn) " to " (str/join "," (access/cell here)) " before it digs")})
          (ctx/update-mem! c assoc :start here :resume-at here)
          :again)
      :else (end-run c {:got 0 :reason :zone-exit :mended 0
                        :text (str "mine could not get out of zone " (:name zn) " before it digs")}))))

(defn ^:async back-to-resume!
  "After a fetch walked the body off: a go-to child back to the cell the dig goes on from (mem :resume-at). :continue while it walks,
  :again once there, else the trip ends :not-back."
  [c]
  (let [{:keys [x y z] :as cell} (:resume-at (ctx/mem c))
        r (await (child/call-walk* c :back
                                   {:pos {:x (+ x 0.5) :y y :z (+ z 0.5)} :range 0
                                    :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
    (cond
      (= :continue r) :continue
      (= (access/cell cell) (access/cell (cell-of (u/self-pos c)))) :again
      :else (end-run c {:got 0 :reason :not-back :mended 0
                        :text (str "mine could not get back to " (str/join "," (access/cell cell)) " after getting its tools")}))))

(defn ^:async fetch-pickaxe!
  "A pickaxe is owed: jobs.items.get-tool for the block through jobs.lib.need (:fetch's :how and :explore apply), once per equip. :continue
  while it runs, else nil; a failed fetch is kept as the dig's :why (mem :tool-why)."
  [c]
  (let [{:keys [block]} (:args c)
        o (need/opts c 'jobs.gather.mine)
        o (when (and o (contains? (:what o) :tool)) o)]
    (when (and o (not (tools/can-harvest? (:primitives c) block)) (not (:tool-why (ctx/mem c))))
      (let [e (await (need/tool! c {:block block} (select-keys o [:how :explore])))]
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
      (await (fetch-pickaxe! c))
      (await (trip/tool-ready! c (:block (:args c)) (digs-left c)))
      (await (when (and (pos? (or (:torch-interval (:args c)) 0)) (nil? (:tunnel (ctx/mem c))) (or (pos? (:tunnel-length (:args c))) (descent-wanted? c)))
               (trip/torches-ready! c)))))

(defn stop-cause
  "Why the dig cannot begin or go on after the equip, as {:reason :text ..} (jobs.gather.dig-for/stop-cause), or for a dig that stopped
  for torches while none are carried."
  [c]
  (or (dig/stop-cause c)
      (when (and (= :no-torches (:wanted (ctx/mem c))) (zero? (torch/torches-held (:primitives c))))
        {:reason :no-torches :text "dig-for stopped: no torches left and none could be had; the trip got no more"})))

;; ------------------------------------------------------------------ the end

(defn to-home!
  "The dig is over with reason (cause: fields of a stop before a dig, else nil): the mend next with :mend, else the walk home."
  [c reason cause]
  (ctx/update-mem! c assoc :phase (if (:mend (:args c)) :mend :home) :reason reason :end-cause cause :at-mend (carried c))
  :again)

(defn dig-text
  "The dig's end text as the trip's: its leading \"dig-for\" is \"mine\"."
  [text]
  (str "mine" (str/replace-first text #"^dig-for" "")))

(defn spent-on-mend?
  "Whether the mend ate into what the dig brought, leaving fewer than the goal carried."
  [c]
  (let [{:keys [at-mend]} (ctx/mem c)
        now (carried c)]
    (and (:mend (:args c)) at-mend (< now (:count (:args c))) (< now at-mend) (> at-mend (had c)))))

(defn finish!
  "End the trip: the dig's ending in memory, whether the body arrived home, the mend's count."
  [c arrived?]
  (let [{:keys [dig-end dig-ending tool-ending end-cause reason mended start tool-why]} (ctx/mem c)
        dug (let [d (merge dig-end end-cause)]
              (if (seq (:left d))
                (let [left (end/still-lying c (:left d))]
                  (if (seq left) (assoc d :left left) (dissoc d :left)))
                d))
        n (got c)
        mended (or mended 0)
        spent? (and arrived? (spent-on-mend? c))
        end-reason (cond (not arrived?) :not-home spent? :spent-on-mend :else reason)
        tunnel (:tunnel dug)
        data (cond-> (merge (dissoc dug :text) {:got n :reason end-reason :mended mended})
               (or spent? (not arrived?)) (assoc :dig-reason reason)
               tunnel (assoc :tunnel (assoc tunnel :walked-back? (boolean arrived?) :back-at (access/cell (cell-of (u/self-pos c)))))
               (and tool-why (= :no-tool reason)) (assoc :why tool-why))
        home (str/join "," (access/cell start))
        text (str (dig-text (or (:text dug) (str "dig-for got " n " of " (owed c) ": " (name end-reason))))
                  (when (pos? mended) (str "; mended " mended))
                  (when tunnel (if arrived? (str ", walked back to " home) ", did not get back to its origin"))
                  (when-not arrived? (str "; did not get back to " home)))
        because (if (and tool-ending (= :no-tool reason)) tool-ending dig-ending)
        e (end-run c (assoc data :text text))]
    (if (and because (not= :done (:kind e)) (not= :done (:kind because)))
      (contract/with-because e because)
      e)))

(defn not-home!
  "A walk home that did not arrive: warn mine.not-home."
  [c]
  (ctx/emit! c :mine.not-home :warn {:to (access/cell (:start (ctx/mem c))) :at (access/cell (cell-of (u/self-pos c)))
                                     :text (str "mine did not get back to " (str/join "," (access/cell (:start (ctx/mem c)))))}))

;; ------------------------------------------------------------------ the steps

(defn dig-args
  "The args of the dig-for call: the trip's, without :mend, tracking the ground when it mends, a tunnel keeping its heading."
  [c]
  (let [{:keys [dug-heading]} (ctx/mem c)]
    (cond-> (-> (:args c)
                (dissoc :mend :submitted-at)
                (assoc :track-ground (boolean (:mend (:args c))) :least (:least (ctx/frozen-call c) :all)))
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
         :no-tool (allows? c :tool)
         :no-torches (and (:stocked? (ctx/mem c)) (allows? c :item))
         false)))

(defn ^:async dig-step!
  "One round of the dig-for child; its ending is kept. A stop for a pickaxe or torches that the trip may get again goes back to the
  equip, to dig on from where the dig stopped; any other ending goes to the mend or the walk home."
  [c]
  (let [e (await (ctx/call c :dig 'jobs.gather.dig-for (dig-args c)))]
    (if (= :continue e)
      :continue
      (let [reason (or (:reason (:data e)) (contract/cause-key e))]
        (ctx/update-mem! c booked e)
        (ctx/update-mem! c assoc :dig-ending (dissoc e :report))
        (if (gettable? c reason)
          (do (ctx/update-mem! c #(-> % (update :rounds (fnil inc 0))
                                      (assoc :phase nil :resume-at (cell-of (u/self-pos c)) :wanted reason)
                                      (dissoc :equipped? :torches-tried :tool-why :tool-ending)))
              :again)
          (to-home! c reason nil))))))

(defn ^:async equip-step!
  "The phase before each dig: equip once, end on a stop the equip did not cure, walk back to where the dig goes on from, out of an own
  zone near the first start, then begin the dig."
  [c]
  (let [m (ctx/mem c)
        e (when-not (:equipped? m) (await (equip! c)))
        stop (when (:equipped? m) (stop-cause c))
        at-resume? (= (access/cell (:resume-at m)) (access/cell (cell-of (u/self-pos c))))
        own (when-not (:zoned? m) (within-own-zone c (dec (zone-margin)) (access/cell (cell-of (u/self-pos c)))))]
    (cond
      (= :continue e) :continue
      (not (:equipped? m)) (do (ctx/update-mem! c #(cond-> (assoc % :equipped? true) (pos? (torch/torches-held (:primitives c))) (assoc :stocked? true))) :again)
      stop (to-home! c (:reason stop) (select-keys stop [:tool :why :text]))
      (not at-resume?) (await (back-to-resume! c))
      (and own (not (shaft-top-here? c))) (await (leave-zone! c own))
      :else (do (ctx/update-mem! c #(cond-> (assoc % :zoned? true :phase :dig) (not (:goal %)) (assoc :goal (:count (:args c)) :had (carried c))))
                :again))))

(defn ^:async mend-step!
  "The mend of the ground the dig opened, then the walk home."
  [c]
  (let [r (await (mend/mend-round! c))]
    (if (nil? r)
      (do (ctx/update-mem! c assoc :phase :home) :again)
      r)))

(defn ^:async home-step!
  "The walk back to the start cell; the trip ends when it is over."
  [c]
  (let [r (await (home/home-round! c))]
    (if (= :continue r)
      :continue
      (do (when-not r (not-home! c))
          (finish! c r)))))

(defn ^:async step [c]
  (case (:phase (ctx/mem c))
    :dig (await (dig-step! c))
    :mend (await (mend-step! c))
    :home (await (home-step! c))
    (await (equip-step! c))))

(defn ^:async round
  [c]
  (when-not (:start (ctx/mem c))
    (let [start (cell-of (u/self-pos c))]
      (ctx/update-mem! c assoc :start start :resume-at start
                       :start-floor (u/feel-name (:primitives c) {:x (:x start) :y (dec (:y start)) :z (:z start)}))))
  (await (pace/steps! c #(step c))))

(defn ^:async cleanup
  "A trip that ended by itself has walked home already. Cut by the limit, a cancel or a parent's end after the dig began: the walk back
  to the start cell, a step per round, done when it ends (arrived or not; not-home warns)."
  [c _]
  (let [m (ctx/mem c)]
    (if (or (:finished m) (not (:goal m)))
      (ctx/end c :done nil {})
      (let [r (await (home/home-round! c))]
        (if (= :continue r)
          :continue
          (do (when-not r (not-home! c))
              (ctx/end c :done nil {:walked-back? (boolean r)})))))))

(def meta {:tags [:mine :dig :ore :light] :position-bound? true})
