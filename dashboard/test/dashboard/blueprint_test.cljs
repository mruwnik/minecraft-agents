(ns dashboard.blueprint-test
  (:require [cljs.test :refer [deftest is]]
            [dashboard.blockcolour :as bc]
            [dashboard.blueprint :as b]))

(defn alt [name & [states]] {:name name :states (or states {})})

(def tiny-bp
  {:name "tiny-hut" :title "Tiny hut" :description "d" :tags ["shelter" "storage"] :front "south" :foundation "flat"
   :clearance 1 :params {:wood "oak"} :width 3 :depth 2
   :legend {:. {:token "." :alts [(alt "air")] :tags []}
            :P {:token "P" :alts [(alt "oak_planks")] :tags []}
            :L {:token "L" :alts [(alt "oak_log" {:axis "y"})] :tags []}
            :i {:token "i" :alts [(alt "torch")] :tags []}
            :S {:token "S" :alts [(alt "@solid")] :tags []}}
   :layers [{:y -1 :grid ["SSS" "S_S"]} {:y 0 :grid ["LPL" ".i."]}]})

(def tiny-bill {:total {:oak_planks 1 :oak_log 2 :torch 1}
                :layers [{:y -1 :items {}} {:y 0 :items {:oak_planks 1 :oak_log 2 :torch 1}}] :tools []})
(def clean {:errors [] :warnings []})
(def tiny-detail {:name "tiny-hut" :hash "abcd1234" :bp tiny-bp :bill tiny-bill :lint clean :errors [] :builds []})
(def unparsed {:name "broken" :hash "00000000" :bp nil :bill nil :lint nil
               :errors ["front matter: name is required" "no ## y<n> layer found"] :builds []})

(deftest footprint
  (is (= "3x2x2" (b/footprint tiny-bp))))

(deftest block-count
  (is (= 4 (b/block-count tiny-bill)))
  (is (= 0 (b/block-count nil))))

(deftest blueprint-row
  (is (= {:name "tiny-hut" :title "Tiny hut" :kind "shelter, storage" :footprint "3x2x2" :layers 2 :blocks 4 :status "ok" :builds 0}
         (b/blueprint-row tiny-detail)))
  (is (= {:name "broken" :title "" :kind "" :footprint "" :layers 0 :blocks 0 :status "does not parse" :builds 0}
         (b/blueprint-row unparsed))))

(deftest blueprint-row-status
  (doseq [[patch status] [[{:lint clean} "ok"]
                          [{:lint {:errors [] :warnings ["w"]}} "1 warning"]
                          [{:lint {:errors [] :warnings ["w" "w2"]}} "2 warnings"]
                          [{:lint {:errors ["e"] :warnings ["w"]}} "build refuses"]
                          [{:builds [{:place "a"} {:place "b"}]} "ok"]]]
    (is (= status (:status (b/blueprint-row (merge tiny-detail patch)))) (pr-str patch))))

(deftest blueprint-row-counts-builds
  (is (= 2 (:builds (b/blueprint-row (assoc tiny-detail :builds [{:place "a"} {:place "b"}]))))))

(deftest status-level
  (doseq [[status level] [["ok" "ok"] ["build refuses" "error"] ["does not parse" "parse"] ["2 warnings" "warning"]]]
    (is (= level (b/status-level status)))))

(deftest layer-cells
  (is (= ["0,0 S any solid block" "1,0 S any solid block" "2,0 S any solid block" "0,1 S any solid block" "2,1 S any solid block"]
         (map #(str (:dx %) "," (:dz %) " " (:token %) " " (:label %)) (b/layer-cells tiny-bp -1))))
  (is (= ["0,0 L oak_log[axis=y] false" "1,0 P oak_planks false" "2,0 L oak_log[axis=y] false"
          "0,1 . air true" "1,1 i torch false" "2,1 . air true"]
         (map #(str (:dx %) "," (:dz %) " " (:token %) " " (:label %) " " (:air %)) (b/layer-cells tiny-bp 0))))
  (is (= [] (b/layer-cells tiny-bp 7))))

(deftest layer-cells-colours
  (let [cells (b/layer-cells tiny-bp 0)]
    (is (= [(bc/block-colour "oak_log") (bc/block-colour "oak_planks") (bc/block-colour "oak_log") nil (bc/block-colour "torch") nil]
           (map :colour cells)))
    (is (= #{0} (set (map :y cells))))))

(deftest waterlogged-cells-tint-towards-water
  (let [wet (alt "oak_slab" {:type "top" :waterlogged "true"})
        bp (assoc tiny-bp :legend (assoc (:legend tiny-bp) :_w {:token "=" :alts [wet] :tags []}
                                         := {:token "=" :alts [wet] :tags []}
                                         :s {:token "s" :alts [(alt "oak_slab" {:type "top"})] :tags []})
                 :layers [{:y 0 :grid ["=s"]}])
        [w d] (b/layer-cells bp 0)]
    (is (= (bc/block-colour "oak_slab") (:colour d)))
    (is (= (bc/alt-colour wet) (:colour w)))
    (is (not= (:colour w) (:colour d)))
    (is (= (:colour w) (:colour (first (b/legend-rows bp)))))))

(deftest hover-text
  (doseq [[what cell text] [["states" (nth (b/layer-cells tiny-bp 0) 0) "x+0 y0 z+0 · oak_log[axis=y] (L)"]
                            ["plain" (nth (b/layer-cells tiny-bp 0) 4) "x+1 y0 z+1 · torch (i)"]
                            ["air" (nth (b/layer-cells tiny-bp 0) 3) "x+0 y0 z+1 · air (.)"]
                            ["solid" (nth (b/layer-cells tiny-bp -1) 4) "x+2 y-1 z+1 · any solid block (S)"]]]
    (is (= text (b/hover-text cell)) what)))

(deftest legend-rows
  (is (= [(str "S any solid block 5 " (bc/block-colour "@solid"))
          (str "L oak_log[axis=y] 2 " (bc/block-colour "oak_log"))
          (str "P oak_planks 1 " (bc/block-colour "oak_planks"))
          (str "i torch 1 " (bc/block-colour "torch"))]
         (map #(str (:token %) " " (:label %) " " (:count %) " " (:colour %)) (b/legend-rows tiny-bp))))
  (is (= ["S" "L" "P" "i"]
         (map :token (b/legend-rows (assoc-in tiny-bp [:legend :X] {:token "X" :alts [(alt "chest")] :tags []}))))))

(deftest bill-rows
  (is (= [{:item "cobblestone" :count 8 :share 1}
          {:item "oak_log" :count 2 :share 0.25}
          {:item "oak_planks" :count 1 :share 0.125}
          {:item "torch" :count 1 :share 0.125}]
         (b/bill-rows {:torch 1 :oak_log 2 :oak_planks 1 :cobblestone 8})))
  (is (= [] (b/bill-rows {})))
  (is (= [] (b/bill-rows nil))))

(deftest bill-note
  (is (= "4 items in all · per layer y-1: 0 · y0: 4" (b/bill-note tiny-detail)))
  (is (= "4 items in all · per layer y-1: 0 · y0: 4 · tools: pick, axe · scaffold: 3 dirt"
         (b/bill-note (-> tiny-detail (assoc-in [:bill :tools] ["pick" "axe"]) (assoc-in [:bill :scaffold] {:dirt 3}))))))

(deftest lint-lines
  (doseq [[what detail expected]
          [["unparsed" unparsed [{:level "parse" :text "front matter: name is required"} {:level "parse" :text "no ## y<n> layer found"}]]
           ["clean" tiny-detail [{:level "ok" :text "lint has nothing to say: build accepts it"}]]
           ["errors first" (assoc tiny-detail :lint {:errors ["e1"] :warnings ["w1"]})
            [{:level "error" :text "e1"} {:level "warning" :text "w1"}]]
           ["warnings alone" (assoc tiny-detail :lint {:errors [] :warnings ["w"]}) [{:level "warning" :text "w"}]]]]
    (is (= expected (b/lint-lines detail)) what)))

(deftest material-role-rows
  (let [detail {:materials {:shell {:kind "full_cube" :requires {:gravity false} :preferences ["oak_planks"]}}
                :relationships [{:members ["shell"] :relation "material" :strength "preferred"}]
                :materialObjects [{:id "wall" :material "shell"} {:id "light" :block "torch"}]
                :allocation {:assignments {:wall "birch_planks" :light "torch"}}}
        rows (b/material-role-rows detail)]
    (is (some #{"gravity: excluded"} (:required (first rows))))
    (is (some #(and (re-find #"oak_planks" %) (re-find #"other allowed" %)) (:preferences (first rows))))
    (is (some #(and (re-find #"throughout this role" %) (re-find #"mixing allowed" %)) (:preferences (first rows))))
    (is (= ["birch_planks"] (:palette (first rows))))
    (is (= ["must use: torch"] (:required (second rows))))
    (is (= [] (:palette (first (b/material-role-rows (dissoc detail :allocation))))))))

(deftest material-role-rows-requirements
  (let [detail {:materials {:roof {:kind "stairs_like" :familyClass "wood" :requires {:flammableOnly true}
                                   :candidates ["a" "b"] :acceptExisting ["c"] :preferences ["p"]}
                            :wall {:kind "full_cube"}}
                :relationships [{:members ["roof" "wall"] :relation "family" :strength "required"}]
                :materialObjects []}
        [roof wall] (b/material-role-rows detail)]
    (is (= "roof" (:role roof)))
    (is (= ["shape: stairs like" "family: wood" "flammable only: required" "allowed: a, b" "existing blocks also allowed: c"
            "matching family with wall required"]
           (:required roof)))
    (is (= ["preferred examples: p (other allowed blocks can be selected)"] (:preferences roof)))
    (is (= ["shape: full cube" "matching family with roof required"] (:required wall)))))

(deftest meta-facts
  (is (= [["tags" "shelter, storage"] ["front" "south"] ["foundation" "flat"] ["clearance" "1 air layer"] ["params" "wood=oak"] ["hash" "abcd1234"]]
         (b/meta-facts tiny-detail)))
  (is (= "2 air layers" (second (nth (b/meta-facts (assoc-in tiny-detail [:bp :clearance] 2)) 3)))))

(deftest build-text
  (doseq [[build expected] [[{:place "hut" :x 1 :y 2 :z 3 :params {}} "hut (1,2,3)"]
                            [{:place "hut" :x 1 :y 2 :z 3 :facing "north" :params {:wood "oak"} :by "Bob" :current false}
                             "hut (1,2,3, facing north, wood=oak, by Bob)"]]]
    (is (= expected (b/build-text build)))))

(deftest builds-stale?
  (is (true? (b/older-version? {:current false})))
  (is (false? (b/older-version? {:current true}))))

;; ---------------------------------------------------------------- the preview
(def unit [[0 0 0 1 1 1]])
(def preview-cells
  [{:x 2 :y 0 :z 0 :name "birch_planks" :states {} :shapes unit}
   {:x 2 :y 3 :z 0 :name "red_wool" :states {} :shapes unit}])

(deftest preview-faces
  (let [whole (b/preview-faces preview-cells {:angle 0})
        cut (b/preview-faces preview-cells {:angle 0 :max-y 0})]
    (is (= 6 (count whole)))
    (is (= 3 (count cut)))
    (is (every? #(identical? (first preview-cells) (:cell %)) cut))
    (is (not= (map :points (b/preview-faces preview-cells {:angle (/ js/Math.PI 2)})) (map :points whole)))
    (is (apply <= (map :depth whole)))
    (is (< (:y1 (b/preview-bounds whole)) (:y1 (b/preview-bounds cut))))
    (is (every? js/isFinite (for [f whole p (:points f) v (vals p)] v)))))

(deftest preview-faces-colours
  (is (not= (bc/alt-colour (first preview-cells)) (bc/alt-colour (second preview-cells))))
  (is (= 3 (count (set (map :colour (b/preview-faces [(first preview-cells)] {:angle 0})))))))

(deftest preview-bounds-empty
  (is (= {:x1 0 :x2 1 :y1 0 :y2 1} (b/preview-bounds []))))

(deftest fit-faces
  (let [faces (b/preview-faces preview-cells {:angle 0})
        fitted (b/fit-faces faces (b/preview-bounds faces) 400 300)]
    (is (= (count faces) (count fitted)))
    (is (every? #(<= 0 (:x %) 400) (for [f fitted p (:points f)] p)))
    (is (every? #(<= 0 (:y %) 300) (for [f fitted p (:points f)] p)))))

(deftest inside-polygon
  (let [square [{:x 0 :y 0} {:x 10 :y 0} {:x 10 :y 10} {:x 0 :y 10}]]
    (doseq [[x y expected] [[5 5 true] [11 5 false] [-1 5 false] [5 11 false]]]
      (is (= expected (b/inside-polygon? square x y))))))

(deftest layer-range
  (is (= {:min -1 :max 0} (b/layer-range tiny-bp))))

(deftest preview-layer-text
  (is (= " y0 (whole building)" (b/preview-layer-text 0 0)))
  (is (= " y-1" (b/preview-layer-text -1 0))))

(deftest face-hover-text
  (is (= "x+2 y0 z+0 · birch_planks" (b/face-hover-text {:cell (first preview-cells)}))))

(deftest palette-label
  (is (= "drag to rotate · allocated from declared stock; site/scaffolds not checked" (b/palette-label {:palette "resolved-declared-stock"})))
  (is (= "drag to rotate · illustrative material palette, not inventory selection" (b/palette-label {:palette "representative"}))))

(deftest find-detail
  (is (= tiny-detail (b/find-detail [unparsed tiny-detail] "tiny-hut")))
  (is (nil? (b/find-detail [tiny-detail] "nope"))))

(deftest plan-cells-keep-air-and-skip-underscores
  (let [cells (b/plan-cells tiny-bp)]
    (is (= 11 (count cells)))
    (is (= [{:dx 0 :dy 0 :dz 0 :air false :names ["oak_log"]} {:dx 0 :dy 0 :dz 1 :air true :names ["air"]}
            {:dx 1 :dy 0 :dz 1 :air false :names ["torch"]}]
           (filter #(and (zero? (:dy %)) (#{[0 0] [0 1] [1 1]} [(:dx %) (:dz %)])) cells)))
    (is (= {:dx 0 :dy -1 :dz 0 :air false :names ["@solid"]} (first cells)))))
