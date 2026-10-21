(ns jobs.movement.go-to.unstick
  "go-to and a body the walker left stuck in water: before the next plan it swims at a bank by hand (the swim act: forward
  and jump held toward a cell), another bank than the one the walk was stuck at first. At most ::swims a call; each is an
  info go-to.unstuck {:toward :status :swim}."
  (:require [engine.args :as a]
            [engine.ctx :as ctx]
            [engine.settings :as settings]
            [jobs.lib.breath :as breath]
            [jobs.lib.util :as u]
            [jobs.lib.column-kinds :as kinds]
            [jobs.movement.go-to.column :as column]))

(a/defargs settings
  {::swims {:default 2 :doc "Swims at a bank a go-to call makes at most for a body its walk left stuck in water." :spec (a/int-in 0 nil)}
   ::swim-ms {:default 2500 :doc "Milliseconds one such swim holds forward and jump before it counts as failed." :spec (a/int-in 1 nil)}})

(defn banks
  "The dry stands a body in water at feet cell [x y z] can swim onto: cells beside it (the 8 round it), level with it or
  one up, feet and head open over a solid; those nearest goal first, the cell stuck (where the walk made no progress)
  last. kind-at is a fn [x y z] -> :open, :solid, ..."
  [kind-at [x y z] goal stuck]
  (let [open? #(keyword-identical? :open %)
        bank? (fn [[bx by bz]] (and (open? (kind-at bx by bz)) (open? (kind-at bx (inc by) bz))
                                    (keyword-identical? :solid (kind-at bx (dec by) bz))))
        far (fn [[bx by bz]] (u/dist goal {:x (+ bx 0.5) :y by :z (+ bz 0.5)}))]
    (->> (for [dx [-1 0 1] dz [-1 0 1] dy [0 1] :when (not (and (zero? dx) (zero? dz)))] [(+ x dx) (+ y dy) (+ z dz)])
         (filter bank?)
         (sort-by (juxt #(= stuck %) far)))))

(defn ^:async swim-out!
  "After a walk that ended walked ({:status :stuck :target ...}) toward goal: a body in water swims at the banks round it
  (banks) until one swim lands, within the call's ::swims (mem :unstuck). True when it stands on a bank."
  [c goal walked]
  (let [p (:primitives c)
        left (- (settings/get settings ::swims) (:unstuck (ctx/mem c) 0))]
    (when (and (= :stuck (:status walked)) (pos? left) (breath/in-water? p))
      (let [[x y z] (mapv #(js/Math.floor (% (u/self-pos c))) [:x :y :z])]
        (loop [[bank & more] (take left (banks (column/kind-at p false) [x y z] goal (:target walked)))]
          (when bank
            (let [n (inc (:unstuck (ctx/mem c) 0))
                  _ (ctx/update-mem! c assoc :unstuck n)
                  r (await (ctx/act c :swim (clj->js {:toward (zipmap [:x :y :z] bank) :ms (settings/get settings ::swim-ms)})))
                  status (.-status r)]
              (ctx/emit! c :go-to.unstuck :info
                         {:toward bank :status status :swim n
                          :text (str "stuck in water at " [x y z] ": swam at the bank " bank " (" status ")")})
              (or (= "landed" status) (recur more)))))))))
