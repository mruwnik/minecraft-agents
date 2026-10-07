(ns jobs.survival.dig-niche
  (:require [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.child :as child]
            [jobs.lib.fetch :as fetch]
            [jobs.lib.look :as look]
            [jobs.lib.result :as result]
            [jobs.lib.shelter :as sh]
            [jobs.lib.solid :as solid]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in :as dig-in]
            [jobs.survival.dig-in-cells :as dig-cells]))

(def doc
  "Shut the body in for the night in a hillside or wall where no pit can be dug: a niche two blocks deep, one wide and
  two high, cut sideways from a standing cell beside a solid face, then the opening plugged from inside.
  It picks the nearest standing cell within :reach whose face has solid blocks all round the niche (floor, ceiling,
  both sides, the back) that the carried tools harvest, with no fluid or falling block (sand, gravel) in or round it,
  walks there (go-to), digs the four cells (opening first, head before feet), steps to the far cell and places one block
  at each opening cell (feet, then head) from the blocks the dig dropped or carried ones.
  Needs a tool that harvests the face (a pickaxe for stone): :fetch (default true; jobs.lib.fetch) runs jobs.items.get-tool
  for it, else, or when that fails, it stops :no-tool.
  Declines (waiting) with :day or :already-sealed. Ends done {:pos :door [feet head cells plugged]} with a :shelter entry {:pos :door} (jobs.survival.dig-in-leave/leave! digs
  the door out by day), or stopped :no-site, :no-tool, :refused (every site would dig or plug another's zone, claim or plan footprint), :unreachable, :dig-failed, :no-blocks, :place-failed or :no-progress (over max-steps rounds).
  Events: dig-niche.sealed (info).")

(def args
  {:reach {:doc "how far from the body to look for a face" :default 16}
   :blocks {:doc "names of the blocks it may place" :default dig-in/shelter-blocks}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :default false}
   :fetch {:doc "get a missing tool (jobs.lib.fetch): true, a set of kinds or a map of limits; false stops :no-tool" :default true}
   :roof-height {:doc "a solid block within this many blocks above counts as a roof" :default sh/default-roof-height}})

(defn check
  "Night and no roof over the body; a decline says why (ctx/wait): :day or :already-sealed."
  [c]
  (let [p (:primitives c)]
    (cond
      (not (sh/night? p)) (ctx/wait c {:reason :day})
      (sh/roofed? p (:roof-height (:args c))) (ctx/wait c {:reason :already-sealed :pos (sh/feet p)})
      :else true)))

(def max-steps "Rounds one call takes at most." 40)

(defn falling? [name] (boolean (re-find #"^(sand|red_sand|gravel)$|_concrete_powder$" (or name ""))))

(defn at
  "The cell k along dir from stand cell f, s to the side, h up."
  [{:keys [x y z]} [dx dz] k s h]
  {:x (+ x (* k dx) (* s (- dz))) :y (+ y h) :z (+ z (* k dz) (* s dx))})

(defn dug-cells
  "The niche's cells from stand cell f in dir: opening first, head before feet."
  [f dir]
  (for [k [1 2] h [1 0]] (at f dir k 0 h)))

(defn door-cells [f dir] (mapv #(at f dir 1 0 %) [0 1]))

(defn shell-cells
  "The cells that must stay solid round the niche: floor, ceiling, both sides of both cells, the back."
  [f dir]
  (concat (for [k [1 2] s [-1 1] h [0 1]] (at f dir k s h))
          (for [k [1 2] h [-1 2]] (at f dir k 0 h))
          (for [h [0 1]] (at f dir 3 0 h))))

(defn niche-ok?
  "Whether a niche can be cut from stand cell f in dir: standing room on dry ground with the approach (the cell behind) open, every cell to dig solid, harvestable
  and not falling, every shell cell solid, dry and not a falling block."
  ([p f dir] (niche-ok? p f dir true))
  ([p f dir need-tool?]
  (let [name #(u/block-name p %)
        solid? #(solid/solid? (name %))]
    (and (solid? (at f dir 0 0 -1)) (not (solid? f))
         (not (solid? (at f dir -1 0 0))) (not (solid? (at f dir -1 0 1))) (not (solid? (at f dir 0 0 1)))
         (not (dig-cells/wet? p f)) (not (dig-cells/wet? p (at f dir 0 0 1)))
         (every? #(and (solid? %) (not (falling? (name %))) (not (dig-cells/wet? p %))
                       (or (not need-tool?) (tools/can-harvest? p (name %))))
                 (dug-cells f dir))
         (every? #(and (solid? %) (not (dig-cells/wet? p %))) (shell-cells f dir))
         (not (falling? (name (at f dir 1 0 2))))
         (not (falling? (name (at f dir 2 0 2))))))))

(defn permitted?
  "Whether the zone rules (input in, see jobs.lib.access/rules-input) let the job dig the niche's cells and plug its opening."
  [in f dir]
  (not (or (some #(access/trespass-refusal in :dig %) (dug-cells f dir))
           (some #(access/trespass-refusal in :place %) (door-cells f dir)))))

(defn stand-ok?
  "Cheap per-cell part of niche-ok?: dry standing room the body has seen (no hollow it only knows from the data) on
  solid ground, whatever the direction."
  [p f]
  (let [solid? #(solid/solid? (u/block-name p %))
        up (assoc f :y (inc (:y f)))
        unseen? #(:unknown (look/seen-block p %))]
    (and (not (unseen? f)) (not (unseen? up)) (solid? (assoc f :y (dec (:y f)))) (not (solid? f)) (not (solid? up))
         (not (dig-cells/wet? p f)) (not (dig-cells/wet? p up)))))

(defn scan*
  "One nearest-first pass within reach of the feet cell, stopping at the first {:stand :dir} where a niche can be cut and
  (ok? stand dir) holds. Returns {:site that-or-nil :refused? whether a niche that would do was turned down by ok?}.
  need-tool? false ignores whether a carried tool harvests the face."
  [p reach ok? need-tool?]
  (let [{:keys [x y z]} (sh/feet p)
        offsets (sort-by (fn [[dx dz]] (+ (* dx dx) (* dz dz)))
                         (for [dx (range (- reach) (inc reach)) dz (range (- reach) (inc reach))
                               :when (<= (+ (* dx dx) (* dz dz)) (* reach reach))]
                           [dx dz]))
        refused (volatile! false)
        try-dir (fn [f dir]
                  (when (niche-ok? p f dir need-tool?)
                    (if (ok? f dir) {:stand f :dir dir} (do (vreset! refused true) nil))))
        site (some (fn [[dx dz]]
                     (some (fn [dy]
                             (let [f {:x (+ x dx) :y (+ y dy) :z (+ z dz)}]
                               (when (stand-ok? p f) (some #(try-dir f %) [[1 0] [-1 0] [0 1] [0 -1]]))))
                           [0 -1 1 -2 2 -3 3]))
                   offsets)]
    {:site site :refused? @refused}))

(defn scan
  "scan* that needs a carried tool to harvest the face."
  [p reach ok?]
  (scan* p reach ok? true))

(defn find-site
  "The nearest {:stand :dir} within reach of the feet cell where a niche can be cut (and (ok? stand dir) holds, default
  always), or nil."
  ([p reach] (find-site p reach (constantly true)))
  ([p reach ok?] (:site (scan p reach ok?))))

(def dig-reach "How far from a cell the body digs it without walking closer." 4)

(defn stage
  "What is left to do at the site, from the world: [:dig cell] (the first solid cell, cut from the stand cell or the
  opening), :walk-to-face, :walk-in, [:plug cell] or :sealed."
  [p {:keys [stand dir]}]
  (let [feet (sh/feet p)
        solid (first (filter #(sh/solid-at? p %) (dug-cells stand dir)))
        open (remove #(sh/solid-at? p %) (door-cells stand dir))]
    (cond
      (and (= feet (at stand dir 2 0 0)) (not (sh/solid-at? p (at stand dir 2 0 1))))
      (if (seq open) [:plug (first open)] [:sealed])
      solid (if (<= (u/dist feet solid) dig-reach) [:dig solid] [:walk-to-face])
      :else [:walk-in])))

(defn tool-wait
  "The :no-tool wait of the first cell of the site's niche no carried tool harvests, nil when none."
  [p {:keys [stand dir]}]
  (when-let [cell (first (remove #(tools/can-harvest? p (u/block-name p %)) (dug-cells stand dir)))]
    {:reason :no-tool :block (u/block-name p cell)}))

(defn fail! [c reason text]
  (let [site (:site (ctx/mem c))]
    (ctx/update-mem! c dissoc :site)
    (ctx/emit! c :dig_niche_failed :warn {:reason reason :site site :text text}))
  (result/stop! c reason text))

(defn ^:async go!
  "One go-to to cell; :continue while it waits, :arrived, or :failed."
  [c slot cell]
  (let [w (await (ctx/call-child c slot 'jobs.movement.go-to {:pos cell :range 0}))]
    (cond (= :continue w) :continue
          (:arrived (ctx/child-result c slot)) :arrived
          :else :failed)))

(defn ^:async dig-step! [c cell]
  (let [p (:primitives c)
        _ (await (tools/equip-for! c (u/block-name p cell) {:fast true}))
        r (await (tidy/dig! c cell))]
    (if (= "dug" (.-status r))
      (do (await (dig-cells/collect-drops! c (:blocks (:args c)) (.-drops r))) :again)
      (fail! c :dig-failed (str "cannot dig the niche: " (.-status r))))))

(defn ^:async plug-step! [c cell]
  (let [item (dig-cells/pick c (:blocks (:args c)))]
    (if (nil? item)
      (fail! c :no-blocks "nothing to plug the niche with")
      (let [r (await (tidy/place! c cell item))]
        (if (#{"placed" "occupied"} (.-status r))
          :again
          (fail! c :place-failed (str "cannot plug the niche: " (.-status r))))))))

(defn seal!
  "Plugged: the :shelter entry and the result."
  [c {:keys [stand dir]}]
  (let [p (:primitives c)
        feet (sh/feet p)
        door (door-cells stand dir)]
    (ctx/remember! c :shelter {:pos feet :state :built :door door} dig-in/shelter-policy)
    (ctx/emit! c :dig-niche.sealed :info {:pos feet :door door :text "shut in a niche in the hillside for the night"})
    (result/finish! c {:pos feet :door door})))

(defn ^:async step [c]
  (let [p (:primitives c)
        in (access/rules-input c)
        ok? (partial permitted? in)
        saved (:site (ctx/mem c))
        reach (:reach (:args c))
        kept (when (and saved (ok? (:stand saved) (:dir saved))) saved)
        {:keys [site refused?]} (if kept {:site kept} (scan p reach ok?))]
    (if (nil? site)
      (if refused?
        (fail! c :refused "every hillside or wall that would do is another's (zone, claim or plan)")
        (let [w (some->> (:site (scan* p reach ok? false)) (tool-wait p))
              r (when w (await (fetch/step! c 'jobs.survival.dig-niche w)))]
          (cond
            r r
            w (fail! c :no-tool (str "no tool for the " (:block w) " of the hillside"))
            :else (fail! c :no-site "no hillside or wall to cut a niche into"))))
      (let [_ (ctx/update-mem! c assoc :site site)
            [what cell] (let [s (stage p site)] (if (vector? s) s [s]))]
        (case what
          :walk-to-face (let [r (await (go! c :face (:stand site)))]
                          (case r
                            :failed (fail! c :unreachable "cannot walk to the hillside")
                            :continue :continue
                            :again))
          :dig (await (dig-step! c cell))
          :walk-in (let [r (await (go! c :in (at (:stand site) (:dir site) 2 0 0)))]
                     (case r
                       :failed (fail! c :unreachable "cannot step into the niche")
                       :continue :continue
                       :again))
          :plug (await (plug-step! c cell))
          :sealed (seal! c site))))))

(defn ^:async round
  "Cut the niche and plug it (see doc), one step at a time with a timer between; :continue (a child waits on the
  world) is yielded."
  [c]
  (loop [i 0]
    (let [r (if (< i max-steps) (await (step c)) (fail! c :no-progress "the niche took too many steps"))]
      (if (= :again r)
        (do (await (child/pace!)) (recur (inc i)))
        r))))
