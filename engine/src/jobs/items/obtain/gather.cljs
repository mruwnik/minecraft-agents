(ns jobs.items.obtain.gather
  "The gather source: the child for a raw need, its tool, and the search for what is not seen."
  (:require [clojure.string :as str]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [engine.game :as game]
            [jobs.items.obtain.base :refer [animal-radius barren-ore explore-far-setting explore-rounds search-scan-radius site-radius tried! ways-of]]
            [jobs.items.obtain.need :refer [explore-opts gather-viable? pickaxe-first? light-settling? search-names tools-wanted torch-due trees-unreachable?]]
            [jobs.items.obtain.plan :refer [gather-plan]]
            [jobs.items.obtain.seen :refer [gather-carried gather-slot seen-of shears-source with-avoid with-block with-count]]
            [jobs.lib.breath :as breath]
            [jobs.lib.cost :as cost]
            [jobs.lib.foods :as foods]
            [jobs.lib.look :as look]
            [jobs.lib.shelter.sense :as sh-sense]
            [jobs.lib.surface-up :as surface]
            [jobs.items.obtain.table :refer [animal? gather-need searchable?]]
            [jobs.lib.waits :as waits]
            [jobs.lib.child :as child]
            [jobs.lib.need :as fetch-need]
            [jobs.lib.torch-stock :as torch-stock]
            [jobs.lib.reserve :as reserve]
            [jobs.lib.util :as u]))

(defn keep-crafting-items!
  "Reserve the carried units the craft chain will take, against every chooser but the crafts, while a gather child runs:
  a sapling fetch must not pillar up with the cobblestone of the axe."
  [c uses]
  (doseq [[item n] uses]
    (ctx/remember! c reserve/kind {:id (str (:id c) "-craft-" item) :item item :count n :strength 1 :by (:id c)
                                   :why "kept for the craft" :not #{:craft :smelt}}
                   reserve/policy)))

(defn release-crafting-items! [c]
  (ctx/forget-where! c reserve/kind #(= (:id c) (:by %))))

(defn declined?
  "Whether child ending e is a refusal before it ran: a failed condition with no result of its own."
  [e]
  (and (= :condition-failed (:kind e)) (empty? (:data e))))

(defn spared-why
  "The text of a hunt ending that left animals alone (its :spared-by, a count per rule: pen, keep ..), else nil."
  [res]
  (when-let [by (not-empty (:spared-by res))]
    (str "left alone: " (str/join ", " (map (fn [[k n]] (str n " " (name k))) by)))))

(defn fruitless-limit
  "Fruitless runs that end the gather source: the need's :max-runs, but only for a run that happened; a child that
  was declined (it could not start) broke nothing and counts against the default."
  [need e]
  (if (declined? e) (child/max-fruitless) (:max-runs need (child/max-fruitless))))

(defn note-skipped
  "Memory m with the cells a mine gave up on: one given up on in two runs is left out of the next (a single give-up may be a passing fault)."
  [m cells]
  (let [cells (map vec cells)
        again (filter (:skipped-once m #{}) cells)]
    (-> m
        (update :skipped-once (fnil into #{}) cells)
        (update :skip-cells (fnil into #{}) again))))

(defn gather-failed!
  "A gather run that brought nothing (a mine that ended :wet or :flooded leaves its ore's column out of the next sight). Trees seen but unreachable (:no-tree) with :explore on send the body to look
  elsewhere (bounded by [:explored key]); a mine that lost its pickaxe (:no-tool) that the pickaxe step can replace
  goes to that step; neither counts as a fruitless run. A stair that found no stone while stone is seen (behind
  glass, bedrock) is :unreachable, not a missing source."
  [c names need why e]
  (cond
    (and (= :no-tree why) (explore-opts c) (< (get-in (ctx/mem c) [:explored (:key need)] 0) (explore-rounds)))
    (do (ctx/update-mem! c assoc-in [:gather :unreachable (:key need)] true) :again)

    (and (= :no-tool why) (pickaxe-first? c names need)) :again

    :else
    (let [why (if (and (= :no-stone-found why) (seen-of c need)) :unreachable why)]
      (child/fruitless! c :gather e)
      (when (child/exhausted? c :gather (fruitless-limit need e)) (tried! c :gather (or why :failed)))
      :again)))

(defn pickaxe-args
  "The args of the obtain child that gets a tool for need."
  [c names need]
  (let [a (:args c)]
    (into {} (remove (comp nil? val))
          {:any-of (tools-wanted c names need) :count 1 :ways (ways-of c) :explore (:explore a)})))

(defn ^:async pickaxe-step!
  "Get the cheapest tool that harvests the need's block (a jobs.items.obtain child): :again once it is carried, :continue
  while it runs, else the gather source fails."
  [c names need]
  (let [args (pickaxe-args c names need)
        _ (when-not (:said-pickaxe (ctx/mem c))
            (ctx/update-mem! c assoc :said-pickaxe true)
            (ctx/emit! c :obtain.pickaxe :info
                       {:message (str "getting a " (str/join "/" (:any-of args)) " to harvest " (:key need))
                        :item (:key need) :tools (:any-of args)}))
        e (await (fetch-need/call! c :pickaxe 'jobs.items.obtain args))]
    (when-not (= :continue e) (ctx/update-mem! c dissoc :said-pickaxe))
    (cond
      (= :continue e) :continue
      (pickaxe-first? c names need)
      (do (when-let [w (and (map? (:cause e)) (:why (:cause e)))] (ctx/update-mem! c assoc-in [:gather :why] w))
          (tried! c :gather {:pickaxe (contract/cause-key e)}))
      :else :again)))

(defn tree?
  "Whether a gather need is a log felled from a tree: trees stand at the surface."
  [need]
  (= 'jobs.forestry.harvest-wood (:job need)))

(defn surfacing?
  "surface/surfacing? for a need not seen: an animal, a tree or a water source in sight is walked to, not climbed toward. A tree
  need not in sight is climbed toward from under rock before any search, as a player would."
  [c need]
  (and (not (and (or (animal? need) (tree? need) (= "water_bucket" (:key need))) (seen-of c need)))
       (or (surface/surfacing? c need)
           (and (tree? need) (not (seen-of c need)) (not (surface/climbed? c)) (sh-sense/buried? (:primitives c))))))

(defn ^:async surface-step!
  "One round of the way up (jobs.lib.surface-up): :continue while it goes on, else the gather source fails (:no-way-up)."
  [c]
  (case (await (surface/step! c))
    :continue :continue
    :out :again
    (do (ctx/update-mem! c dissoc :surface) (tried! c :gather :no-way-up))))

(def max-approaches "Walks toward an animal in sight whose herd is not all in sight, before the hunt's keep rule counts it alone." 2)

(defn approach-due?
  "Whether the hunt for an animal need seen declines :too-few (the pair it spares is not all in sight): the body walks closer to count the herd."
  [c need]
  (and (animal? need) (seen-of c need)
       (< (get-in (ctx/mem c) [:gather :approaches (:key need)] 0) max-approaches)
       (let [s (ctx/child-status c (gather-slot need) (:job need) (:args need))]
         (and (= :later (first s)) (= :too-few (:reason (contract/status-cause s)))))))

(defn approach!
  "Set the lead to the animal in sight; :again."
  [c need]
  (let [{:keys [x y z]} (u/pos-of (.-pos (seen-of c need)))]
    (ctx/update-mem! c update-in [:gather :approaches (:key need)] (fnil inc 0))
    (ctx/update-mem! c assoc-in [:gather :lead] {:key (:key need) :pos [x y z]})
    :again))

(defn ^:async lead-step!
  "An animal the search found but the body does not see now (the look that found it faced elsewhere): walk to within 6 blocks
  of where it was found. :continue while it walks, else :again."
  [c]
  (let [{:keys [pos]} (get-in (ctx/mem c) [:gather :lead])
        r (await (child/call-walk* c :lead-walk {:pos (zipmap [:x :y :z] pos) :range 6 :ways #{:walk} :spend {:tries 1}}))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c update :gather dissoc :lead) :again))))

(def max-search-timeouts 3)

(def heading-ring [:north :north-east :east :south-east :south :south-west :west :north-west])

(defn far-heading
  "The heading of search number k (1 up): the first runs on away from where the first began (north when the body is still
  there), each later one turns 135 degrees, so eight searches take eight ways."
  [start [x _ z] k]
  (let [[sx _ sz] start
        angle (js/Math.atan2 (- x sx) (- (- z sz)))
        base (if (< (js/Math.hypot (- x sx) (- z sz)) 16)
               0
               (mod (js/Math.round (/ angle (/ js/Math.PI 4))) 8))]
    (heading-ring (mod (+ base (* 3 (dec k))) 8))))

(defn within-site
  "Search options o kept within the site-radius setting of center (nil: unchanged)."
  [o center]
  (cond-> o center (update :max-distance min (site-radius))))

(defn round-opts
  "The search options for search number k of a need: the first rings round the start, the rest outward along a heading."
  [c k]
  (let [center (:center (:args c))]
    (within-site
     (if (zero? k)
       (explore-opts c)
       (let [start (or center (:explore-start (ctx/mem c)) [0 0 0])
             p (.-pos (.self (:primitives c)))]
         (assoc (explore-far-setting) :pattern :outward
                :heading (far-heading start [(.-x p) (.-y p) (.-z p)] k))))
     center)))

(defn ^:async explore-walk!
  "Walk and look for need (a jobs.explore.search child): :again once it is seen; a search that finds none stops the
  gather source, as does the last of the explore-rounds searches for one need. Also :again when none is found and the body is still under rock (the next round stairs up), and after a search that ran out of its own time (no verdict: not counted, the next round looks from where the body stands)."
  [c need]
  (let [targets (search-names need)
        k0 (get-in (ctx/mem c) [:explored (:key need)] 0)
        here (.-pos (.self (:primitives c)))
        _ (when (and (zero? k0) (not (:explore-start (ctx/mem c))))
            (ctx/update-mem! c assoc :explore-start [(.-x here) (.-y here) (.-z here)]))
        o (round-opts c k0)
        said [(:key need) k0]
        _ (when-not (= said (:explore-said (ctx/mem c)))
            (ctx/update-mem! c assoc :explore-said said)
            (ctx/emit! c :obtain.exploring :info
                     {:message (str "nothing seen to get " (str/join "/" targets) ": walking to look for it, up to " (:max-distance o)
                                    " blocks out and " (:max-legs o) " legs, from " (js/Math.floor (.-x here)) " "
                                    (js/Math.floor (.-y here)) " " (js/Math.floor (.-z here)))
                      :attention :notice
                      :for targets :max-distance (:max-distance o) :max-legs (:max-legs o) :heading (:heading o)
                      :from (mapv #(js/Math.floor %) [(.-x here) (.-y here) (.-z here)])}))
        e (waits/waiting-out c (await (ctx/call c :explore 'jobs.explore.search
                           (child/search-args (merge o (when-let [at (:center (:args c))] {:center at}) {:target targets :count 1 :scan-radius (search-scan-radius) :use-notes false :takeable true :avoid (vec (:cut-off (ctx/mem c)))}
                                                     (when (animal? need) {:forage (boolean (foods/edible? (:key need)))}))))))]
    (if (= :continue e)
      :continue
      (let [res (:data e)
            timed-out? (boolean (some #(= :limit-reached (:kind %)) (take-while some? (iterate :because e))))
            timeouts (if timed-out? (inc (get-in (ctx/mem c) [:explore-timeouts (:key need)] 0)) 0)
            ;; a search that ran out of its own time is not counted, unless that keeps happening
            out-of-time? (and timed-out? (<= timeouts max-search-timeouts))
            k (cond-> (get-in (ctx/mem c) [:explored (:key need)] 0) (not out-of-time?) inc)]
        (ctx/update-mem! c dissoc :explore-said)
        (ctx/update-mem! c assoc-in [:explore-timeouts (:key need)] (if out-of-time? timeouts 0))
        (ctx/update-mem! c assoc-in [:explored (:key need)] k)
        (ctx/update-mem! c update :gather update :unreachable dissoc (:key need))
        (let [found? (= :found (:reason res))
              [x y z :as pos] (when found? (:pos (first (:found res))))
              lead? (and pos (animal? need))]
          (when (and pos (not lead?) (< 12 (let [now (.-pos (.self (:primitives c)))] (js/Math.hypot (- x (.-x now)) (- z (.-z now))))))
            (ctx/update-mem! c assoc :approach {:x x :y y :z z}))
          (cond
            (or (and found? (seen-of c need)) (surfacing? c need) out-of-time?) :again
            lead? (do (ctx/emit! c :obtain.lead :info {:message (str "found " (str/join "/" targets) " at " (str/join " " pos) " out of sight: walking to it") :pos pos})
                      (ctx/update-mem! c assoc-in [:gather :lead] {:key (:key need) :pos pos})
                      :again)
            (< k (explore-rounds)) :again
            :else (tried! c :gather (cond-> {:searched (:key need) :for targets :result (or (:reason res) (contract/cause-key e))}
                                      (:why res) (assoc :why (:why res))
                                      (:coverage res) (assoc :legs (:legs (:coverage res)) :farthest (:farthest (:coverage res)))))))))))

(defn ^:async approach-step!
  "Walk up to the log a search found far off (the felling child works within its radius of the body): :continue while it walks, else :again."
  [c]
  (let [r (await (child/call-walk* c :approach {:pos (:approach (ctx/mem c)) :range 8}))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c dissoc :approach) :again))))

(defn ^:async torch-step!
  "Before a mine that must stair or tunnel down to an ore not seen starts (its own ways lack :gather, so it cannot stock torches itself): get the
  torches here with an obtain child, as jobs.lib.torch-stock does for a mine called alone. :continue while it runs, else nil."
  [c need]
  (when (and (:tunnel? need) (= 'jobs.gather.mine (:job need)) (not (seen-of c need)) (not (get-in (ctx/mem c) [:gather :before])))
    (or (await (fetch-need/return! c))
        (await (torch-stock/torches! c)))))

(defn start-cause
  "The cause of the need's child's start check now."
  [c need]
  (contract/status-cause (ctx/child-status c (gather-slot need) (:job need) (:args need))))

(defn book-light!
  "Book the cell for the light step of the next round; :again."
  [c cell]
  (ctx/update-mem! c assoc-in [:gather :light] cell)
  :again)

(defn ^:async light-step!
  "Hang a torch at cell (a jobs.build.light-area child, which fetches one when none is carried): :continue while it runs, else :again
  with the cell booked as tried, and when."
  [c cell]
  (let [r (await (ctx/call c :light 'jobs.build.light-area {:cells [cell]}))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c (fn [m] (-> m (update :gather dissoc :light) (assoc-in [:gather :lit cell] (ctx/now c))))) :again))))

(defn dug-blocks
  "The new ground a mine run cut (data res): its tunnel legs (the last one included), else its tunnel, and its stair down."
  [{:keys [legs tunnel descent]}]
  (+ (if (seq legs) (reduce + 0 (map :length legs)) (:steps tunnel 0)) (:steps descent 0)))

(def faces-per-block "Rock faces a 1x2 tunnel lays bare per block it cuts: two walls two high, the roof and the floor." 6)

(defn ore-resource
  "The jobs.lib.cost base-rates resource of need's ore block (\"iron\" for deepslate_iron_ore), nil for none."
  [need]
  (some-> (:block (:args need)) (str/replace #"^deepslate_" "") (str/replace #"_ore$" "")))

(defn barren?
  "Whether n blocks cut in a row with no find of need's ore would, at the body's height, have laid bare ::barren-ore of it
  (cost/expected-density over faces-per-block faces a block): the ground there is too poor to mine on. Always for a block with no ore placement."
  [c need n]
  (let [d (cost/expected-density (ore-resource need) (update (u/self-pos c) :y js/Math.floor) nil)]
    (or (not (pos? d)) (>= (* n faces-per-block d) (barren-ore)))))

(defn dug-total
  "Blocks cut in a row without a find, counting this run's n: the count of the ore key in mem's [:gather :dug] ({:n :at},
  where the last run ended) while pos is within n blocks of that end (the run went on from there), else n (a new site)."
  [mem key pos n]
  (let [{:keys [n0 at]} (get-in mem [:gather :dug key])]
    (if (and at (<= (u/dist at pos) n)) (+ n n0) n)))

(defn strip-on!
  "A mine for an ore not seen that cut n blocks of new ground and found none looked at new rock: the next run mines on
  from where it stands, not counted fruitless, until the blocks cut in a row without a find make the ground barren?; info obtain.strip-on. :again."
  [c need n]
  (let [pos (u/self-pos c)
        total (dug-total (ctx/mem c) (:key need) pos n)
        y (js/Math.floor (:y pos))]
    (ctx/update-mem! c assoc-in [:gather :dug (:key need)] {:n0 total :at pos})
    (if (barren? c need total)
      (do (ctx/update-mem! c assoc-in [:gather :why] (str "cut " total " blocks down to y " y " and saw no " (:key need) ": too little there to mine on"))
          false)
      (do (child/progress! c :gather)
          (ctx/emit! c :obtain.strip-on :info
                     {:message (str "mine cut " n " blocks at y " y " and saw no " (:key need) ": mining on (" total " blocks so far)")
                      :item (:key need) :blocks n :total total :y y})
          :again))))

(defn ^:async gather-step!
  "One round of the gather source: the child for the first raw item the chain lacks, until it ends."
  [c names have target]
  (let [p (:primitives c)
        pl (gather-plan c names (- target have))
        need (some->> (some-> pl :gather (gather-need (game/version-of p))) (with-block c) (with-count p) (with-avoid c))
        stocking (await (torch-step! c need))
        wet (when (and need (not stocking) (not (get-in (ctx/mem c) [:gather :before])) (not= "water_bucket" (:key need)) (not (animal? need)) (breath/in-water? p))
              (#{:continue :again} (await (surface/leave-water! c))))]
    (cond
      stocking stocking
      (:approach (ctx/mem c)) (await (approach-step! c))
      wet wet
      (get-in (ctx/mem c) [:gather :light])
      (await (light-step! c (get-in (ctx/mem c) [:gather :light])))
      (nil? need) (tried! c :gather :no-plan)
      (= (:key need) (get-in (ctx/mem c) [:gather :lead :key]))
      (await (lead-step! c))
      (and (animal? need) (not (get-in (ctx/mem c) [:gather :before])) (not (look/surveyed? c :near)) (not (seen-of c need)))
      (do (await (look/survey-until! c :near #(seen-of c need) (look/glance-effort) look/hazards-only)) :again)
      (and (not (get-in (ctx/mem c) [:gather :before])) (approach-due? c need))
      (approach! c need)
      (and (not (get-in (ctx/mem c) [:gather :before])) (pickaxe-first? c names need) (or (explore-opts c) (seen-of c need)))
      (await (pickaxe-step! c names need))
      (and (not (get-in (ctx/mem c) [:gather :before])) (explore-opts c) (searchable? need) (not (:tunnel? need))
           (surfacing? c need))
      (await (surface-step! c))
      (and (not (get-in (ctx/mem c) [:gather :before])) (explore-opts c) (searchable? need) (not (:tunnel? need))
           (or (not (seen-of c need)) (get-in (ctx/mem c) [:gather :unreachable (:key need)]) (trees-unreachable? c need))
           (< (get-in (ctx/mem c) [:explored (:key need)] 0) (explore-rounds)))
      (await (explore-walk! c need))
      (and (not (get-in (ctx/mem c) [:gather :before])) (torch-due c (start-cause c need)))
      (book-light! c (torch-due c (start-cause c need)))
      (and (not (get-in (ctx/mem c) [:gather :before])) (light-settling? c (start-cause c need)))
      :continue
      (and (not (get-in (ctx/mem c) [:gather :before]))
           (not (if (explore-opts c)
                  ;; :later (dark, night) is the child's to wait out, not a sign that nothing is there
                  (and (or (:tunnel? need) (seen-of c need))
                       (#{:ok :later} (first (ctx/child-status c (gather-slot need) (:job need) (:args need)))))
                  (gather-viable? c names (- target have)))))
      (tried! c :gather :none-seen)
      :else
      (do (when-not (get-in (ctx/mem c) [:gather :before])
            (ctx/update-mem! c assoc-in [:gather :before] (gather-carried p need))
            (ctx/update-mem! c assoc-in [:gather :item] (:key need))
            (keep-crafting-items! c (:uses pl))
            (when (tree? need)
              (ctx/update-mem! c assoc-in [:gather :tree] (select-keys need [:key :job :args])))
            (let [at (:pos (seen-of c need))]
              (when at (ctx/update-mem! c assoc-in [:gather :at] [(:x at) (:z at)]))
              (ctx/emit! c :obtain.gathering :info
                         {:message (str "getting " (:key need) " with " (:job need)
                                        (when at (str " from " (:x at) " " (:y at) " " (:z at))))
                          :item (:key need) :via (:job need) :at at}))
            (when (:sheep need)
              (let [how (shears-source c)]
                (ctx/emit! c :obtain.wool-source :info
                           {:message (case how
                                       :carried "wool by shearing: shears carried"
                                       :craft "wool by shearing: shears will be crafted from 2 iron ingots"
                                       :chest "wool by shearing: shears or iron fetched from a chest"
                                       "wool by killing sheep: no shears and no iron for shears (the last 2 adults of a colour live)")
                            :how (or how :kill)})))
            (when-let [{:keys [wanted taking]} (:wool-fallback pl)]
              (ctx/emit! c :obtain.wool-colour :info
                         {:message (str "no " wanted " sheep within " (animal-radius) " blocks; taking " taking " wool instead")
                          :wanted wanted :taking taking})))
          (let [e (waits/waiting-out c (await (ctx/call c (gather-slot need) (:job need) (:args need))))]
            (if (= :continue e)
              :continue
              (let [res (merge (when (map? (:cause e)) (:cause e)) (:data e))
                    before (get-in (ctx/mem c) [:gather :before])
                    at (get-in (ctx/mem c) [:gather :at])]
                (ctx/update-mem! c update :gather dissoc :before :item :tree :at)
                (release-crafting-items! c)
                (if (> (gather-carried p need) before)
                  (do (child/progress! c :gather) (ctx/update-mem! c update :gather dissoc :dug) :again)
                  (do (when-let [w (or (:why res) (spared-why res))] (ctx/update-mem! c assoc-in [:gather :why] w))
                      (when-let [cells (seq (:skipped res))] (ctx/update-mem! c note-skipped cells))
                      (when-let [cols (seq (:columns res))] (ctx/update-mem! c update :cut-off (fnil into #{}) (map vec) cols))
                      (cond
                        (torch-due c (:cause e)) (book-light! c (torch-due c (:cause e)))
                        (light-settling? c (:cause e)) :again
                        (and (#{:flooded :wet :exposed :hazard} (contract/cause-key e)) at)
                        (do (ctx/update-mem! c update :cut-off (fnil conj #{}) at) :again)
                        (and (:tunnel? need) (not at) (pos? (dug-blocks res)) (strip-on! c need (dug-blocks res))) :again
                        :else (do (when-not (and (:tunnel? need) (not at) (pos? (dug-blocks res))) ; else the barren verdict stands
                                    (ctx/update-mem! c update :gather dissoc :dug))
                                  (gather-failed! c names need (if (= :done (:kind e)) :nothing-gathered (contract/cause-key e)) e))))))))))))

(defn ^:async finish-tree!
  "The log child of an earlier round was cut before it ended: runs it to its end even when the logs carried are enough
  (a tree it started is felled whole). :continue while it waits, else :again."
  [c]
  (let [{:keys [key job args]} (get-in (ctx/mem c) [:gather :tree])
        slot (gather-slot {:key key})
        r (waits/waiting-out c (await (ctx/call c slot job args)))]
    (if (= :continue r)
      :continue
      (do (ctx/update-mem! c update :gather dissoc :before :item :tree) :again))))
