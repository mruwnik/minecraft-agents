(ns jobs.survival.dig-niche
  (:require [jobs.lib.ready :as ready]
            [engine.settings :as settings]
            [engine.args :as a]
            [clojure.string]
            [jobs.lib.blocks :as lb]
            [engine.ctx :as ctx]
            [jobs.lib.access :as access]
            [jobs.lib.checks :as checks]
            [jobs.lib.child :as child]
            [jobs.lib.dig-look :as dl]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.look :as look]
            [jobs.lib.need :as need]
            [jobs.lib.reach :as reach]
            [jobs.lib.seal :as seal]
            [jobs.lib.shelter :as sh]
            [jobs.lib.solid :as solid]
            [jobs.lib.spent :as spent]
            [jobs.lib.tidy :as tidy]
            [jobs.lib.tools :as tools]
            [jobs.lib.util :as u]
            [jobs.survival.dig-in-cells :as dig-cells]
            [jobs.survival.dig-in.mode :as mode]))

(def doc
  "Shut the body in for the night in a hillside or wall where no pit can be dug: a niche two blocks deep, one wide and
  two high, cut sideways from a standing cell beside a solid face, then the opening plugged from inside.
  It picks the nearest standing cell within :reach whose face has solid blocks all round the niche (floor, ceiling,
  both sides, the back) that the carried tools harvest, with no fluid or falling block (sand, gravel) in or round it,
  walks there (go-to), digs the four cells (opening first, head before feet), steps to the far cell and places one block
  at each opening cell (feet, then head) from the blocks the dig dropped or carried ones (the call's :use :blocks,
  default the shelter fillers).
  Needs a tool that harvests the face (a pickaxe for stone): :fetch (default true; jobs.lib.need) runs jobs.items.get-tool
  for it, else, or when that fails, it ends :no-tool.
  Conditions (at the start only): night, else condition-failed :day. A body already shut in a closed room under a roof (any roof within :roof-height, a canopy too) is done. Done {:pos :door [feet head cells plugged]}
  with a :shelter entry {:pos :door} (jobs.lib.dig-in-leave/leave! digs the door out by day). Facts of the site are condition-failed:
  :no-site, :no-tool, :refused (every site would dig or plug another's zone, claim or plan footprint), :no-blocks, and what a dug cell
  laid open: :fluid (in or beside the niche), :open-shell (a shell cell that is not solid). Tries that failed are nothing-possible:
  :unreachable, :dig-failed, :place-failed (a mob in the door cell is waited out first, :dig-in-cells/mob-wait-ticks), :no-progress
  (over max-steps rounds); the data is {:text}. Rock the body has not looked into reads stone
  (dig-in-cells/rock-name); after each dig it looks at the cells laid open and waits a lava flow delay.
  Quote: time by the walk and six cells (four dug, two plugged); done 1 shelter.")

(a/defargs args
  {:reach {:doc "how far from the body to look for a face, 1 to 32" :spec (a/int-in 1 32) :default 16}
   :ignore-zones? {:doc "act regardless of zones and claims; the rules of the game allow it" :spec boolean? :default false}
   :fetch {:doc "get a missing tool (jobs.lib.need): true, a set of kinds or a map of limits; false stops :no-tool" :spec need/option? :default true}
   :roof-height {:doc "a solid block within this many blocks above counts as a roof, 1 to 32" :spec (a/int-in 1 32) :default sh/default-roof-height}})

(def can-start? (checks/night :day))

(def target [])

(a/defargs settings
  {::max-steps {:default 40 :doc "Rounds one call takes at most." :spec (a/int-in 0 nil)}
   ::dig-reach {:default 4 :doc "How far from a cell the body digs it without walking closer." :spec (a/int-in 0 nil)}
   ::job-time-s {:default 120 :doc "Seconds a niche may take." :spec (a/num-in 0 nil)}
   ::cell-s {:default 2 :doc "Seconds one cell takes to dig or plug, for the quote." :spec (a/num-in 0 nil)}
   ::niche-cells {:default 6 :doc "Cells one niche takes to dig and plug, for the quote." :spec (a/int-in 0 nil)}
   ::quote-spread {:default 3 :doc "How many times the mean the quote's upper time bound is, at least." :spec (a/num-in 1 nil)}
   ::walk-s {:default 5 :doc "Seconds the walk to the face takes, for the quote." :spec (a/num-in 0 nil)}})
(defn time-s [] (settings/get settings ::job-time-s))

(def defaults {:checks {:safe? '(always) :fit? '(always)} :spend {:job-time ::job-time-s}})

(def safe? ready/ok)

(def fit? ready/ok)


(defn done?
  "Shut in a closed room under a roof before this call cut anything, or once the niche is plugged."
  [c]
  (let [m (ctx/mem c)
        p (:primitives c)
        d (and (sh/roofed? p (:roof-height (:args c))) (reach/closed-room? p (sh/feet p))
               (or (:ended m) (and (nil? (:site m)) (not (checks/running? c)))))]
    (checks/goal d [:roomed "shut in a closed room under a roof"] [:exposed "not shut in a closed room"]
                 {:units :shelters :done (if d 1 0) :left (if d 0 1)})))

(defn quote-of [c]
  (let [roofed (= :ok (first (done? c)))
        mean (if roofed 0 (+ (settings/get settings ::walk-s) (* (settings/get settings ::niche-cells) (settings/get settings ::cell-s))))]
    {:uses {:job-time [0 mean (max (* (settings/get settings ::quote-spread) mean) (time-s))]} :done [0 1 1]}))
(defn max-steps [] (settings/get settings ::max-steps))


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
  (let [name #(dig-cells/rock-name p %)
        solid? #(solid/solid? (name %))]
    (and (solid? (at f dir 0 0 -1)) (not (solid? f))
         (not (solid? (at f dir -1 0 0))) (not (solid? (at f dir -1 0 1))) (not (solid? (at f dir 0 0 1)))
         (not (dig-cells/wet? p f)) (not (dig-cells/wet? p (at f dir 0 0 1)))
         (every? #(and (solid? %) (not (rules/falling? (name %))) (not (dig-cells/wet? p %))
                       (or (not need-tool?) (tools/can-harvest? p (name %))))
                 (dug-cells f dir))
         (every? #(and (solid? %) (not (dig-cells/wet? p %))) (shell-cells f dir))
         (not (rules/falling? (name (at f dir 1 0 2))))
         (not (rules/falling? (name (at f dir 2 0 2))))))))

(defn permitted?
  "Whether the zone rules (input in, see jobs.lib.access/rules-input, with :spare-own? for the body's own plans) let the job
  dig the niche's cells and plug its opening."
  [in f dir]
  (not (or (some #(or (access/own-keep in %) (access/trespass-refusal in :dig %)) (dug-cells f dir))
           (some #(access/trespass-refusal in :place %) (door-cells f dir)))))

(defn stand-ok?
  "Cheap per-cell part of niche-ok?: dry standing room the body has seen (no hollow it only knows from the data) on
  solid ground, whatever the direction."
  [p f]
  (let [solid? #(solid/solid? (u/seen-name p %))
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

(defn dig-reach [] (settings/get settings ::dig-reach))

(defn stage
  "What is left to do at the site, from the world: [:dig cell] (the first solid cell, cut from the stand cell or the
  opening), :walk-to-face, :walk-in, :plug or :sealed."
  [p {:keys [stand dir]}]
  (let [feet (sh/feet p)
        solid (first (filter #(dig-cells/rock-solid? p %) (dug-cells stand dir)))
        open (remove #(sh/solid-at? p %) (door-cells stand dir))]
    (cond
      (and (= feet (at stand dir 2 0 0)) (not (sh/solid-at? p (at stand dir 2 0 1))))
      (if (seq open) [:plug] [:sealed])
      solid (if (<= (u/dist feet solid) (dig-reach)) [:dig solid] [:walk-to-face])
      :else [:walk-in])))

(defn tool-wait
  "The :no-tool wait of the first cell of the site's niche no carried tool harvests, nil when none."
  [p {:keys [stand dir]}]
  (when-let [cell (first (remove #(tools/can-harvest? p (dig-cells/rock-name p %)) (dug-cells stand dir)))]
    {:reason :no-tool :block (dig-cells/rock-name p cell)}))

(defn shown-stop
  "{:reason :text} when what the body sees of the site rules the niche out now: fluid in or round it (a dug cell lays
  open what lay behind it), or a shell cell seen not solid; nil when nothing shows."
  [p {:keys [stand dir]}]
  (if-let [wet (first (filter #(dig-cells/wet? p %) (concat (dug-cells stand dir) (shell-cells stand dir))))]
    {:reason :fluid :text (str (u/seen-name p wet) " in or beside the niche")}
    (when-let [open (first (filter #(some-> (u/seen-name p %) (as-> n (not (solid/solid? n)))) (shell-cells stand dir)))]
      {:reason :open-shell :text (str (u/seen-name p open) " in the shell of the niche")})))

(def fact-stops
  "The stops that name a fact of the site or the body (condition-failed); the others are tries that failed (nothing-possible)."
  #{:no-site :no-tool :refused :no-blocks :fluid :open-shell})

(defn fail! [c reason text]
  (let [site (:site (ctx/mem c))]
    (ctx/update-mem! c dissoc :site :plug-since)
    (ctx/end c (if (fact-stops reason) :condition-failed :nothing-possible) reason {:text text :site site})))

(defn ^:async go!
  "One go-to to cell; :continue while it waits, :arrived, or :failed."
  [c slot cell]
  (let [e (await (child/call-walk* c slot {:pos cell :range 0}))]
    (cond (= :continue e) :continue
          (= :done (:kind e)) :arrived
          :else :failed)))

(defn ^:async dig-step! [c cell]
  (let [p (:primitives c)
        _ (await (tools/equip-for! c (dig-cells/rock-name p cell) {:fast true}))
        r (await (tidy/dig! c cell))]
    (if (= "dug" (.-status r))
      (do (ctx/update-mem! c assoc :dug-at (ctx/now c))
          (await (dl/see-round! c [(:x cell) (:y cell) (:z cell)]))
          (await (dl/wait-settled! c [[(:x cell) (:y cell) (:z cell)]]))
          (await (dig-cells/collect-drops! c (mode/blocks c) (.-drops r)))
          :again)
      (fail! c :dig-failed (str "cannot dig the niche: " (.-status r))))))

(defn ^:async plug-step!
  "Shut the opening with jobs.survival.plug-in (walls way, ledger :shelter): :again once it is shut, :continue while it works."
  [c]
  (if (nil? (lb/pick c (mode/blocks c)))
    (fail! c :no-blocks "nothing to plug the niche with")
    (let [e (await (ctx/call c :plug 'jobs.survival.plug-in
                             {:ways #{:walls} :purpose :shelter :roof-height (:roof-height (:args c)) :use {:blocks (mode/blocks c)}}))]
      (cond
        (= :continue e) :continue
        (= :done (:kind e)) (do (spent/absorb! c (:data e)) :again)
        :else (fail! c :place-failed (str "cannot plug the niche: " (or (get-in e [:data :text]) (name (:kind e)))))))))

(defn seal!
  "Plugged: the :shelter entry and the result."
  [c {:keys [stand dir]}]
  (let [p (:primitives c)
        feet (sh/feet p)
        door (door-cells stand dir)
        placed (spent/log c)]
    (ctx/remember! c :shelter {:pos feet :state :built :door door} sh/shelter-policy)
    (ctx/update-mem! c assoc :ended true)
    (ctx/end c :done nil (merge {:pos feet :door door} (spent/summary placed)))))

(defn ^:async fetch-tool!
  "Get the tool for the block of w (jobs.lib.need) when :fetch allows a tool: :continue while it works, :again once
  it got one, nil when it did not or may not."
  [c w]
  (let [o (need/opts c 'jobs.survival.dig-niche)]
    (when (and o (contains? (:what o) :tool))
      (let [e (await (need/tool! c {:block (:block w)} (select-keys o [:how :explore])))]
        (cond (= :continue e) :continue
              (= :done (:kind e)) :again)))))

(defn ^:async step [c]
  (let [p (:primitives c)
        in (access/rules-input c {:spare-own? true})
        ok? (partial permitted? in)
        saved (:site (ctx/mem c))
        reach (:reach (:args c))
        kept (when (and saved (ok? (:stand saved) (:dir saved))) saved)
        {:keys [site refused?]} (if kept {:site kept} (scan p reach ok?))]
    (if (nil? site)
      (if refused?
        (fail! c :refused "every hillside or wall that would do is another's (zone, claim or plan)")
        (let [w (some->> (:site (scan* p reach ok? false)) (tool-wait p))
              r (when w (await (fetch-tool! c w)))]
          (cond
            r r
            w (fail! c :no-tool (str "no tool for the " (:block w) " of the hillside"))
            (not (look/surveyed? c)) (await (look/look-around! c))
            :else (fail! c :no-site "no hillside or wall to cut a niche into"))))
      (let [_ (ctx/update-mem! c assoc :site site)
            shown (shown-stop p site)
            [what cell] (let [s (stage p site)] (if (vector? s) s [s]))]
        (if shown
          (fail! c (:reason shown) (:text shown))
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
            :plug (await (plug-step! c))
            :sealed (seal! c site)))))))

(defn ^:async round
  "Cut the niche and plug it (see doc), one step at a time with a timer between; :continue (a child waits on the
  world) is yielded."
  [c]
  (loop [i 0]
    (let [r (if (< i (max-steps)) (await (step c)) (fail! c :no-progress "the niche took too many steps"))]
      (if (= :again r)
        (do (await (child/pace!)) (recur (inc i)))
        r))))
