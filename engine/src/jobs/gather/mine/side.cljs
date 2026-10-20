(ns jobs.gather.mine.side
  "The wall ore of a :stay-level jobs.gather.mine run: a side cut by a child mine run, and the holes dug in the wall
  shut again."
  (:require [clojure.string :as str]
            [engine.contract :as contract]
            [engine.ctx :as ctx]
            [jobs.gather.mine-stay :as stay]
            [jobs.gather.mine.targets :refer [rules-in skip! step-cell]]
            [jobs.gather.mine.tunnel :as tunnel]
            [jobs.lib.access :as access]
            [jobs.lib.access.rules :as rules]
            [jobs.lib.blocks :as blocks]
            [jobs.lib.child :as child]
            [jobs.lib.look :as look :refer [cell-of headings]]
            [jobs.lib.need :as need]
            [jobs.lib.util :as u]))

(defn ^:async side-round!
  "Take ore in the tunnel wall by a child dig-for run: a stay-level tunnel of the cut's steps to the side, which digs the ore beside it.
  Then the body walks back to the cell it cut from, the cut's mouth is shut with a carried block, and the ore is skipped."
  [c {:keys [pos heading steps] :as cut}]
  (ctx/update-mem! c #(-> % (assoc :side-cut cut) (update :side-from (fn [from] (or from (cell-of (u/self-pos c)))))))
  (let [from (:side-from (ctx/mem c))
        args (-> (:args c)
                 (dissoc :direction :submitted-at)
                 (assoc :heading heading :tunnel-length steps :count (need/carry-count c :side [(blocks/item-name (:args c))] 1) :shaft false :stay-level true :track-ground false))
        r (or (:side-end (ctx/mem c))
              (let [r (await (ctx/call c :side 'jobs.gather.dig-for (assoc args :settings {::stay/ore-reach 0})))]
                (when-not (= :continue r) (ctx/update-mem! c assoc :side-end (dissoc r :report)))
                r))
        back (when-not (= :continue r)
               (await (child/call-walk* c :side-back {:pos {:x (+ (:x from) 0.5) :y (:y from) :z (+ (:z from) 0.5)} :range 0 :ignore-zones? (boolean (:ignore-zones? (:args c)))})))]
    (if (or (= :continue r) (= :continue back))
      :continue
      (let [_ (ctx/update-mem! c dissoc :side-cut :side-from :side-end)
            [dx dz] (headings heading)
            mouth (-> from (update :x + dx) (update :z + dz))
            name-at #(u/seen-name (:primitives c) %)
            open (filterv #(not (rules/solid-floor? name-at %)) [mouth (update mouth :y inc)])
            took (= :done (:kind r))
            shut (or (empty? open) (await (stay/seal! c (rules-in c) open)))]
        (ctx/emit! c :mine.side (if took :info :warn)
                   {:pos (access/cell pos) :heading heading :steps steps :shut shut
                    :text (str (if took "mine took" "mine did not take") " the wall ore at " (str/join "," (access/cell pos)) " by a side cut " steps " " heading
                               (when-not took (str ": " (name (or (contract/cause-key r) :unknown))))
                               (when-not shut "; the cut stays open"))})
        (skip! c pos)
        :again))))

(defn ^:async shut-holes!
  "Fill each cell an ore was dug from in the wall (stay-level, after its drop was collected) with a carried block, so no cut
  stays open beside the tunnel. A body that went into a hole to fetch its drop first steps back to the tunnel line. A hole that
  cannot be filled warns mine.hole-open and is listed in the result."
  [c]
  (let [{:keys [holes tunnel start]} (ctx/mem c)
        feet (cell-of (u/self-pos c))
        in-hole? (some #(and (= (:x feet) (% 0)) (= (:z feet) (% 2)) (<= 0 (- (% 1) (:y feet)) 1)) holes)
        line (if tunnel (step-cell tunnel (:steps tunnel)) start)
        w (when in-hole? (await (tunnel/back-on-line! c line)))]
    (cond
      (= :continue w) :continue
      (some? w)
      (do (ctx/update-mem! c #(-> % (dissoc :holes) (update :holes-open into holes)))
          (ctx/emit! c :mine.hole-open :warn {:cells holes :text (str "mine could not leave the hole it dug at " (str/join " " (map #(str/join "," %) holes)) " to shut it")})
          :again)
      :else
      (loop [[[x y z] & more] holes open []]
        (if x
          (do (await (ctx/act c :look (clj->js {:pos {:x (+ x 0.5) :y (+ y 0.5) :z (+ z 0.5)}})))
              (look/see! c)
              (recur more (cond-> open (rules/air (u/seen-name (:primitives c) {:x x :y y :z z})) (conj {:x x :y y :z z}))))
          (let [shut (or (empty? open) (await (stay/seal! c (rules-in c) open)))]
            (ctx/update-mem! c #(cond-> (dissoc % :holes) (not shut) (update :holes-open into (map access/cell open))))
            (when-not shut
              (ctx/emit! c :mine.hole-open :warn {:cells (mapv access/cell open)
                                                  :text (str "mine could not shut the hole it dug at " (str/join " " (map #(str/join "," (access/cell %)) open)))}))
            :again))))))
