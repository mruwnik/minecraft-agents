(ns jobs.build.rail-spans
  "The tunnel and bridge spans of a rail line, cut and built before the line itself (a phase of jobs.build.rail-line)."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.block-sets :as bs]
                        [jobs.lib.child :as child]
            [jobs.lib.look :refer [cell-of]]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]
            [jobs.lib.world :as known]
            [plan.rail :as rail]
            [plan.shape :as shape]
            [plan.rail-route :as route]))

(a/defargs settings
  {::max-tries {:default 5 :doc "Walks to a span's stand and tunnel or bridge children begun for it in all before the line's own build is left to deal with it." :spec (a/int-in 1 nil)}})

(def open-names #{"air" "cave_air" "void_air" "water"})

(defn kind-fn
  "(fn [pos]) -> the kind of a declared span (the plan's :spans) that pos lies in and that is still to do there: a tunnel cell
  holding a block or never seen, a bridge cell with no floor seen under it."
  [c]
  (let [p (:primitives c)
        declared (into {} (for [{:keys [kind] :as span} (:spans (:plan (known/plan c (:plan (:args c)))))
                                pos (shape/span-cells span)]
                            [pos kind]))
        name-at (fn [[x y z]] (u/seen-name p {:x x :y y :z z}))]
    (fn [[x y z :as pos]]
      (case (declared pos)
        :tunnel (let [n (name-at pos)] (when (or (nil? n) (not (bs/passable? n))) :tunnel))
        :bridge (let [n (name-at [x (dec y) z])] (when (or (nil? n) (open-names n)) :bridge))
        nil))))

(defn due
  "The span to work on next: of the unfinished ones (route/built-spans over the line's chain) not begun max-tries times,
  the one whose stand cell is nearest the body at [x y z]."
  [c cells self]
  (let [chain (rail/line cells)
        tries (:span-tries (ctx/mem c))
        spans (when-not (:error chain) (route/built-spans (mapv :pos chain) (kind-fn c)))
        left (remove #(>= (get tries (:stand %) 0) (settings/get settings ::max-tries)) spans)]
    (first (sort-by (fn [{[x _ z] :stand}] (let [[sx _ sz] self] (+ (abs (- x sx)) (abs (- z sz))))) left))))

(def heading-names {[1 0] "east" [-1 0] "west" [0 1] "south" [0 -1] "north"})

(defn child-call
  "[job args] cutting or bridging span from where the body stands at its stand cell."
  [c {:keys [kind heading cells]}]
  (let [n (count cells)
        zones {:ignore-zones? (boolean (:ignore-zones? (:args c))) :ways (need/pass-ways c)}]
    (if (= :bridge kind)
      ['jobs.access.bridge (merge zones {:heading (keyword (heading-names heading)) :length n})]
      ['jobs.gather.strip-tunnel (merge zones {:heading (heading-names heading) :limit n :through (:plan (:args c))})])))

(defn ^:async step!
  "Work on the next span: walk to its stand cell, then cut the tunnel or build the bridge with a child. nil when no span is
  due, :continue while a child waits, else :again (each begun span is counted in :span-tries)."
  [c cells]
  (let [self (cell-of (u/self-pos c))
        {:keys [stand] :as span} (due c cells [(:x self) (:y self) (:z self)])]
    (when span
      (let [[x y z] stand
            there? (= [x y z] [(:x self) (:y self) (:z self)])
            count-try! #(ctx/update-mem! c update-in [:span-tries stand] (fnil inc 0))]
        (if-not there?
          (let [r (await (child/call-walk* c :span-walk {:pos {:x x :y y :z z} :range 0 :ways #{:walk :clear-seal}
                                                         :ignore-zones? (boolean (:ignore-zones? (:args c)))}))]
            (if (or (= :continue r) (= :cut-off (:kind r))) :continue (do (count-try!) :again)))
          (let [[job args] (child-call c span)
                r (await (ctx/call c :span job args))]
            (if (or (= :continue r) (= :cut-off (:kind r)))
              :continue
              (do (count-try!) :again))))))))
