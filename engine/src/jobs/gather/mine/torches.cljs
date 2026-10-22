(ns jobs.gather.mine.torches
  "The torches of jobs.gather.dig-for: when one is owed on the tunnel, a branch or the stair down, where it hangs, and
  the craft of more."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :as look :refer [cell-of headings glance!]]
            [jobs.lib.need :as need]
            [jobs.lib.waits :as waits]
            [jobs.lib.placement :as placement]
            [jobs.lib.shelter.sense :as sense]
            [jobs.lib.solid :as solid]
            [jobs.lib.torch :as torch]
            [jobs.lib.torch-plan :as tp]
            [jobs.lib.trip :as trip]
            [jobs.lib.util :as u]
            [jobs.gather.mine.end :refer [stop!]]
            [jobs.gather.mine.targets :refer [step-cell]]))

(a/defargs settings
  {::torch-min {:default 2 :doc "A torch round crafts more when fewer torches than this are carried." :spec (a/int-in 0 nil)}
   ::torch-craft-count {:default 4 :doc "Torches one craft makes." :spec (a/int-in 1 nil)}})

(defn torch-min [] (settings/get settings ::torch-min))

(defn torch-craft-count [] (settings/get settings ::torch-craft-count))

(defn count-of [c names]
  (transduce (comp (filter #(contains? names (:name %))) (map :count)) + 0 (u/inventory (:primitives c))))

(defn wood?
  "Whether an item crafts into sticks: a log or planks."
  [n]
  (boolean (and n (re-find #"_(log|planks)$" n))))

(defn craftable?
  "Whether torches can be crafted from what is carried: coal or charcoal, and sticks or wood to make them from."
  [c]
  (let [names (map :name (u/inventory (:primitives c)))]
    (and (pos? (torch/fuel-held (:primitives c)))
         (or (pos? (count-of c #{"stick"})) (some wood? names)))))

(defn on-line?
  "Whether the body stands on the tunnel's last cell, past its first."
  [c]
  (let [{:keys [tunnel]} (ctx/mem c)]
    (and (pos? (:steps tunnel 0)) (= (cell-of (u/self-pos c)) (step-cell tunnel (:steps tunnel))))))

(defn on?
  "Whether the dig hangs torches at all (its :torches arg)."
  [c]
  (boolean (:torches (:args c))))

(defn due-spot
  "The site (a cell vector) a torch is owed at, or nil: jobs.lib.torch-plan over cells (seen, judged by how the body perceives
  their light) and ahead (not yet cut; dark when the light under the feet is 1 or less), light passing through open cells, the
  sites being only site, the torches already hung the sources."
  [c cells ahead site]
  (when (and site (not (some #{site} (:left-out (ctx/mem c)))))
    (let [p (:primitives c)
          feet-light (sense/light-at p (cell-of (u/self-pos c)))
          ahead? (set ahead)
          plan (tp/plan {:cells (concat cells ahead)
                         :dark? #(if (ahead? %) (or (nil? feet-light) (<= feet-light 1)) (torch/perceived-dark? p %))
                         :open? #(or (ahead? %) (let [n (u/seen-name p (zipmap [:x :y :z] %))] (and n (not (solid/solid? n)))))
                         :spots [site]
                         :sources (:hung (ctx/mem c))})]
      (first (:spots plan)))))

(defn vec-of [{:keys [x y z]}] [x y z])

(defn tunnel-spot
  "Where the tunnel owes a torch: the cell behind the body, when the body stands on the tunnel's last cell and a cell of the last
  few or the next is dark and no hung torch reaches it."
  [c]
  (let [{:keys [tunnel]} (ctx/mem c)
        n (:steps tunnel 0)
        cell #(vec-of (step-cell tunnel %))]
    (when (and (on? c) (on-line? c))
      (due-spot c (map cell (range (max 0 (- n tp/spread)) (inc n))) [(cell (inc n))] (cell (dec n))))))

(defn torch-due? [c] (some? (tunnel-spot c)))

(defn line-cells
  "The cells a branch runs off: the tunnel's, or the start cell while no tunnel is cut (the body mines a cave from where it began)."
  [{:keys [tunnel start]}]
  (cond tunnel (map #(step-cell tunnel %) (range (inc (:steps tunnel 0))))
        start [start]))

(defn branch-site
  "{:dir :site} for a torch off the line (line-cells): the run goes away from the line along the axis it is farthest off on, the
  site is the cell behind the body; nil when the body is straight above or below the line (no side to run along)."
  [m here]
  (when-let [cells (seq (line-cells m))]
    (let [near (apply min-key #(+ (Math/pow (- (:x here) (:x %)) 2) (Math/pow (- (:z here) (:z %)) 2)) cells)
          dx (- (:x here) (:x near))
          dz (- (:z here) (:z near))
          dir (if (>= (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0] [0 (js/Math.sign dz)])]
      (when (not= [0 0] dir)
        {:dir dir :site (-> here (update :x - (first dir)) (update :z - (second dir)))}))))

(defn branch-torch-due?
  "Whether the body, off the tunnel line (or the start cell, with no tunnel), stands in a dark cell no hung torch reaches."
  [c]
  (let [here (cell-of (u/self-pos c))
        site (some-> (branch-site (ctx/mem c) here) :site vec-of)]
    (boolean (and (on? c) site (due-spot c [(vec-of here)] [] site)))))

(defn stair-site
  "The cell the stair's torch hangs from: the body's cell one step back up the stair."
  [c]
  (let [[dx dz] (headings (:heading (ctx/mem c)))
        {:keys [x y z]} (cell-of (u/self-pos c))]
    [(- x dx) (inc y) (- z dz)]))

(defn stair-torch-due?
  "Whether the stair down to the ore level owes a torch: past its first step, a dark cell of the stair or the next is out of reach of every hung torch."
  [c]
  (let [[dx dz] (headings (:heading (ctx/mem c)))
        {:keys [x y z]} (cell-of (u/self-pos c))
        site (stair-site c)]
    (boolean (and (on? c) (pos? (:steps (:descent (ctx/mem c)) 0))
                  (due-spot c [site [x y z]] [[(+ x dx) (dec y) (+ z dz)]] site)))))

(defn mouth-torch-due?
  "Whether the stair this run cuts from its start owes the torch at its mouth: the first torch of the stair hangs after its first
  step whatever the light now, so the opening stays lit when the light fails."
  [c]
  (let [d (:descent (ctx/mem c))]
    (boolean (and (on? c) (pos? (:steps d 0)) (not (:mouth-torch d)) (not= :done (:state (:own-stair d)))))))

(defn booked!
  "Book a torch hung: its cell joins the planner's sources."
  [c site]
  (ctx/update-mem! c update :hung (fnil conj []) site))

(defn left-out!
  "Book the torch step done with no torch hung: the site is not asked again, but lights nothing. An info event of the reason."
  [c site cell reason]
  (ctx/update-mem! c update :left-out (fnil conj []) site)
  (ctx/emit! c :mine.torch-left-out :info {:cell cell :reason reason
                                           :text (str "mine hung no torch" (some->> cell (str/join ",") (str " at ")) ": " (name reason))}))

(defn ^:async hang-torch!
  "Hang a torch on the cell behind the body (a branch: behind it on its way off the tunnel), after a glance at it and
  its side walls when the support is not yet seen."
  [c branch]
  (let [p (:primitives c)
        {:keys [tunnel]} (ctx/mem c)
        block-at (fn [[x y z]] (u/seen-name p {:x x :y y :z z}))
        here (cell-of (u/self-pos c))
        {:keys [dir site]} (cond
                             (= :stair branch) {:dir (headings (:heading (ctx/mem c))) :site (zipmap [:x :y :z] (stair-site c))}
                             branch (branch-site (ctx/mem c) here)
                             :else {:dir (headings (:heading tunnel)) :site (step-cell tunnel (dec (:steps tunnel)))})
        pick (fn [] (when site (torch/torch-at dir [(:x site) (:y site) (:z site)] (placement/eye (u/self-pos c)) block-at)))
        choice (or (let [c0 (pick)] (when-not (:refused c0) c0))
                   (when site (let [[dx dz] dir] (await (glance! c [[(- dx) (- dz)] [(- dz) dx] [dz (- dx)]]))))
                   (pick))
        cell (:cell choice)
        booked (vec-of (or site here))
        reason (cond (nil? site) :no-site
                     (:refused choice) (:refused choice)
                     (empty? (:allowed (gate/look c :place [(zipmap [:x :y :z] cell)] {}))) :refused)]
    (if reason
      (left-out! c booked cell reason)
      (let [held? (await (torch/place! c choice block-at))]
        (if held? (booked! c (vec cell)) (left-out! c booked cell :place-failed))))))

(defn owed-site
  "The cell the torch of this round hangs from (the site due-spot asks about), as hang-torch! picks it."
  [c branch]
  (let [{:keys [tunnel]} (ctx/mem c)]
    (vec-of (cond (= :stair branch) (zipmap [:x :y :z] (stair-site c))
                  branch (:site (branch-site (ctx/mem c) (cell-of (u/self-pos c))))
                  :else (step-cell tunnel (dec (:steps tunnel)))))))

(defn lit-here?
  "Whether the body's cell is lit as the torch plan judges light (torch/perceived-dark?): a torch is not needed there."
  [c]
  (not (torch/perceived-dark? (:primitives c) (vec-of (cell-of (u/self-pos c))))))

(defn no-torches-text [branch]
  (str "dig-for stopped: no torches left for the " (cond (= :stair branch) "stair" branch "branch" :else "tunnel")
       " and nothing to craft them from; jobs.gather.mine stocks torches before it digs"))

(defn mouth-done!
  "The stair's first torch is owed no more once one is hung or the cell is lit (a round that only crafted owes it still)."
  [c branch]
  (when (= :stair branch) (ctx/update-mem! c assoc-in [:descent :mouth-torch] true)))

(defn ^:async torch-round!
  "One round of a torch (the tunnel's; a branch's when branch is true, the stair's when :stair): craft more when under 2 are carried and coal or
  charcoal and a stick are; with none to hang, leave the torch out where the cell is lit, else :no-torches; else hang one. :continue / :again."
  [c branch]
  (let [m (ctx/mem c)
        n (torch/torches-held (:primitives c))]
    (cond
      (and (< n (torch-min)) (craftable? c) (not (:craft-failed m)))
      (let [e (waits/waiting-out c (await (ctx/call c :torch-craft 'jobs.items.craft {:item "torch" :count (need/carry-count c :torch-craft ["torch"] (torch-craft-count)) :ways (need/pass-ways c) :avoid (trip/line-ahead c)})))]
        (when (and (not= :continue e) (not (and (= :done (:kind e)) (pos? (:made (:data e) 0)))))
          (ctx/update-mem! c assoc :craft-failed true))
        (if (= :continue e) :continue :again))

      (and (zero? n) (lit-here? c))
      (let [here (vec-of (cell-of (u/self-pos c)))] (left-out! c (owed-site c branch) here :lit) (mouth-done! c branch) :again)

      (zero? n) :no-torches

      :else (do (await (hang-torch! c branch)) (mouth-done! c branch) :again))))

(defn ^:async torch-step!
  "torch-round! for the dig itself: no torch left ends the dig :no-torches."
  [c branch]
  (let [r (await (torch-round! c branch))]
    (if (= :no-torches r) (stop! c :no-torches {:text (no-torches-text branch)}) r)))
