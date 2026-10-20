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
  (when site
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

(defn branch-site
  "{:dir :site} for a torch on a branch: the run goes away from the tunnel along the axis it is farthest off on, the
  site is the cell behind the body; nil when the body is straight above or below the line (no side to run along)."
  [tunnel here]
  (let [near (apply min-key #(+ (Math/pow (- (:x here) (:x %)) 2) (Math/pow (- (:z here) (:z %)) 2))
                    (map #(step-cell tunnel %) (range (inc (:steps tunnel 0)))))
        dx (- (:x here) (:x near))
        dz (- (:z here) (:z near))
        dir (if (>= (js/Math.abs dx) (js/Math.abs dz)) [(js/Math.sign dx) 0] [0 (js/Math.sign dz)])]
    (when (not= [0 0] dir)
      {:dir dir :site (-> here (update :x - (first dir)) (update :z - (second dir)))})))

(defn branch-torch-due?
  "Whether the body, off the tunnel line, stands in a dark cell no hung torch reaches."
  [c]
  (let [{:keys [tunnel]} (ctx/mem c)
        here (cell-of (u/self-pos c))
        site (some-> tunnel (branch-site here) :site vec-of)]
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

(defn booked!
  "Book the torch step done: the cell of the torch hung, or the site a torch was left out of, joins the planner's sources so it is not asked again."
  [c site]
  (ctx/update-mem! c update :hung (fnil conj []) site))

(defn left-out!
  "Book the torch step done, with an info event of the reason."
  [c site cell reason]
  (booked! c site)
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
                             branch (branch-site tunnel here)
                             :else {:dir (headings (:heading tunnel)) :site (step-cell tunnel (dec (:steps tunnel)))})
        pick (fn [] (when site (torch/torch-at dir [(:x site) (:y site) (:z site)] (placement/eye (u/self-pos c)) block-at)))
        choice (or (let [c0 (pick)] (when-not (:refused c0) c0))
                   (when site (let [[dx dz] dir] (await (glance! c [[(- dx) (- dz)] [(- dz) dx] [dz (- dx)]]))))
                   (pick))
        cell (:cell choice)
        booked (vec-of (or site here))
        reason (cond (nil? site) :no-site
                     (:refused choice) (:refused choice)
                     (not (gate/allowed? c :mine.declined "mine" :place (zipmap [:x :y :z] cell))) :refused)]
    (if reason
      (left-out! c booked cell reason)
      (let [held? (await (torch/place! c choice block-at))]
        (if held? (booked! c (vec cell)) (left-out! c booked cell :place-failed))))))

(defn lit-here?
  "Whether the body's cell is already lit (block light, or sky light less the darkening): a torch is not needed there."
  [c]
  (let [p (:primitives c)
        cell (cell-of (u/self-pos c))
        raw (look/raw-of p)
        block (sense/block-light-at p cell)
        sky (sense/sky-light-at p cell)]
    (boolean (and raw block sky (look/lit-light? block sky (look/sky-subtract raw))))))

(defn no-torches-text [branch]
  (str "dig-for stopped: no torches left for the " (cond (= :stair branch) "stair" branch "branch" :else "tunnel")
       " and nothing to craft them from; jobs.gather.mine stocks torches before it digs"))

(defn ^:async torch-round!
  "One round of a torch (the tunnel's; a branch's when branch is true, the stair's when :stair): craft more when under 2 are carried and coal or
  charcoal and a stick are; with none to hang, leave the torch out where the cell is lit, else :no-torches; else hang one. :continue / :again."
  [c branch]
  (let [m (ctx/mem c)
        n (torch/torches-held (:primitives c))]
    (cond
      (and (< n (torch-min)) (craftable? c) (not (:craft-failed m)))
      (let [e (await (ctx/call c :torch-craft 'jobs.items.craft {:item "torch" :count (need/carry-count c :torch-craft ["torch"] (torch-craft-count)) :fetch (:fetch (:args c)) :avoid (trip/line-ahead c)}))]
        (when (and (not= :continue e) (not (and (= :done (:kind e)) (pos? (:made (:data e) 0)))))
          (ctx/update-mem! c assoc :craft-failed true))
        (if (= :continue e) :continue :again))

      (and (zero? n) (lit-here? c))
      (let [{:keys [x y z]} (cell-of (u/self-pos c))] (left-out! c [x y z] [x y z] :lit) :again)

      (zero? n) :no-torches

      :else (do (await (hang-torch! c branch)) :again))))

(defn ^:async torch-step!
  "torch-round! for the dig itself: no torch left ends the dig :no-torches."
  [c branch]
  (let [r (await (torch-round! c branch))]
    (if (= :no-torches r) (stop! c :no-torches {:text (no-torches-text branch)}) r)))
