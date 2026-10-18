(ns jobs.gather.mine.torches
  "The torches of jobs.gather.mine: when one is owed on the tunnel, a branch or the stair down, where it hangs, and
  the craft of more."
  (:require [clojure.string :as str]
            [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.gate :as gate]
            [jobs.lib.look :refer [cell-of headings glance!]]
            [jobs.lib.need :as need]
            [jobs.lib.placement :as placement]
            [jobs.lib.torch :as torch]
            [jobs.lib.trip :as trip]
            [jobs.lib.util :as u]
            [jobs.gather.mine.targets :refer [step-cell]]))

(a/defargs settings
  {::end-torch-gap {:default 4 :doc "A cut's end gets a torch when the last hangs this many steps back or more." :spec (a/int-in 0 nil)}
   ::branch-gap {:default 4 :doc "A dig this far from the tunnel line (and from any torch hung on a branch) gets a torch of its own." :spec (a/int-in 0 nil)}
   ::torch-min {:default 2 :doc "A torch round crafts more when fewer torches than this are carried." :spec (a/int-in 0 nil)}
   ::torch-craft-count {:default 4 :doc "Torches one craft makes." :spec (a/int-in 1 nil)}})

(defn end-torch-gap [] (settings/get settings ::end-torch-gap))

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

(defn torch-gap
  "Steps since the last torch hung (the steps taken when none did yet)."
  [c]
  (let [m (ctx/mem c)] (- (get-in m [:tunnel :steps] 0) (or (:torch-at m) 0))))

(defn torch-due?
  "Whether the tunnel's next torch is owed: the first at step 1, then every :torch-interval."
  [c]
  (let [interval (:torch-interval (:args c))]
    (and (pos? (or interval 0)) (on-line? c)
         (or (nil? (:torch-at (ctx/mem c))) (>= (torch-gap c) interval)))))

(defn end-torch-due? [c]
  (and (pos? (or (:torch-interval (:args c)) 0)) (on-line? c) (>= (torch-gap c) (end-torch-gap))))

(defn branch-gap [] (settings/get settings ::branch-gap))

(defn line-dist
  "The distance from cell to the nearest cell of the tunnel dug so far."
  [tunnel cell]
  (apply min (map #(u/dist cell (step-cell tunnel %)) (range (inc (:steps tunnel 0))))))

(defn branch-torch-due?
  "Whether the body stands off the tunnel line by branch-gap or more with no branch torch within that of it."
  [c]
  (let [{:keys [tunnel branch-torches]} (ctx/mem c)
        here (cell-of (u/self-pos c))]
    (and (some? tunnel) (pos? (or (:torch-interval (:args c)) 0))
         (>= (line-dist tunnel here) (branch-gap))
         (every? #(>= (u/dist here %) (branch-gap)) branch-torches))))

(defn booked!
  "Book the torch step done: a tunnel torch at the tunnel's length, a stair one (branch :stair) at the descent's steps, a branch one
  (branch true) at the body's cell."
  [c branch]
  (cond
    (= :stair branch) (ctx/update-mem! c assoc-in [:descent :torch-at] (get-in (ctx/mem c) [:descent :steps] 0))
    branch (ctx/update-mem! c update :branch-torches (fnil conj []) (cell-of (u/self-pos c)))
    :else (ctx/update-mem! c assoc :torch-at (get-in (ctx/mem c) [:tunnel :steps]))))

(defn stair-torch-due?
  "Whether the stair down to the ore level owes a torch: the first after its first step, then every :torch-interval steps."
  [c]
  (let [interval (:torch-interval (:args c))
        {:keys [steps torch-at]} (:descent (ctx/mem c))]
    (and (pos? (or interval 0)) (pos? (or steps 0))
         (or (nil? torch-at) (>= (- steps torch-at) interval)))))

(defn left-out!
  "Book the torch step done, with an info event of the reason."
  [c cell reason branch]
  (booked! c branch)
  (ctx/emit! c :mine.torch-left-out :info {:cell cell :reason reason
                                           :text (str "mine hung no torch" (some->> cell (str/join ",") (str " at ")) ": " (name reason))}))

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

(defn ^:async hang-torch!
  "Hang a torch on the cell behind the body (a branch: behind it on its way off the tunnel), after a glance at it and
  its side walls when the support is not yet seen."
  [c branch]
  (let [p (:primitives c)
        {:keys [tunnel]} (ctx/mem c)
        block-at (fn [[x y z]] (u/seen-name p {:x x :y y :z z}))
        here (cell-of (u/self-pos c))
        {:keys [dir site]} (cond
                             (= :stair branch) (let [[dx dz :as d] (headings (:heading (ctx/mem c)))]
                                                 {:dir d :site (-> here (update :x - dx) (update :z - dz) (update :y inc))})
                             branch (branch-site tunnel here)
                             :else {:dir (headings (:heading tunnel)) :site (step-cell tunnel (dec (:steps tunnel)))})
        pick (fn [] (when site (torch/torch-at dir [(:x site) (:y site) (:z site)] (placement/eye (u/self-pos c)) block-at)))
        choice (or (let [c0 (pick)] (when-not (:refused c0) c0))
                   (when site (let [[dx dz] dir] (await (glance! c [[(- dx) (- dz)] [(- dz) dx] [dz (- dx)]]))))
                   (pick))
        cell (:cell choice)
        reason (cond (nil? site) :no-site
                     (:refused choice) (:refused choice)
                     (not (gate/allowed? c :mine.declined "mine" :place (zipmap [:x :y :z] cell))) :refused)]
    (if reason
      (left-out! c cell reason branch)
      (let [held? (await (torch/place! c choice block-at))]
        (booked! c branch)
        (when-not held? (left-out! c cell :place-failed branch))))))

(defn ^:async torch-step!
  "One round of a torch (the tunnel's; a branch's when branch is true, the stair's when :stair): craft more when under 2 are carried and coal or
  charcoal and a stick are, say so once with none to hang, else hang one. :continue."
  [c branch]
  (let [m (ctx/mem c)
        n (torch/torches-held (:primitives c))]
    (cond
      (and (< n (torch-min)) (craftable? c) (not (:craft-failed m)))
      (let [e (await (ctx/call c :torch-craft 'jobs.items.craft {:item "torch" :count (need/carry-count c :torch-craft ["torch"] (torch-craft-count)) :fetch (:fetch (:args c)) :avoid (trip/line-ahead c)}))]
        (when (and (not= :continue e) (not (and (= :done (:kind e)) (pos? (:made (:data e) 0)))))
          (ctx/update-mem! c assoc :craft-failed true))
        (if (= :continue e) :continue :again))

      (zero? n)
      (do (when-not (get-in m [:no-torches-said (if (= :stair branch) :stair (boolean branch))])
            (ctx/emit! c :mine.no-torches :info {:text (str "mine has no torches and nothing to craft them from: the "
                                                            (cond (= :stair branch) "stair" branch "branch" :else "tunnel") " stays dark")}))
          (ctx/update-mem! c assoc-in [:no-torches-said (if (= :stair branch) :stair (boolean branch))] true)
          (booked! c branch)
          :again)

      :else (do (await (hang-torch! c branch)) :again))))
