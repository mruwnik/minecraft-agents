(ns engine.fake.animals-test
  "The fake world's animals over cljs world data: the cases of js/fake-interact.test.mjs, fake-leash.test.mjs,
  fake-tempt.test.mjs and fake-lead-path.test.mjs. A world is {:blocks :states :inventory :entities :next-entity-id
  :self {:pos :held}}; a walk is the body's new position, then the lead's drag and the food's lure."
  (:require [cljs.test :refer [deftest is testing]]
            [engine.fake.animals :as animals]
            [engine.fake.use-on :as use-on]))

(defn world
  [{:keys [blocks states inventory entities pos held] :or {pos [0 64 0]}}]
  {:blocks (or blocks {}) :states (or states {}) :inventory (vec inventory) :entities (vec entities)
   :next-entity-id 10 :self {:pos pos :held held}})

(defn cow [& {:as extra}] (merge {:id 1 :name "cow" :kind "passive" :pos [1 64 0]} extra))
(defn stack [name count] {:name name :count count})
(defn entity [w id] (first (filter #(= id (:id %)) (:entities w))))
(defn knot [w] (first (filter #(= "leash_knot" (:name %)) (:entities w))))
(defn lead-items [w] (count (filter #(and (= "item" (:kind %)) (= "lead" (get-in % [:item :name]))) (:entities w))))
(defn close? [a b] (< (Math/abs (- a b)) 1e-6))
(defn flat [a b] (animals/flat a b))

(defn walk
  "The body walks to pos in one go: the lead's drag, then the food's lure."
  [w pos]
  (let [from (get-in w [:self :pos])]
    (-> w (assoc-in [:self :pos] pos) (animals/drag-leashed from) animals/tempt-follow)))

(defn equip [w item] (assoc-in w [:self :held] item))
(defn lead-on [w] (first (animals/interact w {:id 1 :item "lead"})))
(defn count-of [w n] (:count (first (filter #(= n (:name %)) (:inventory w)))))

;; ---- interact

(def wheat [(stack "wheat" 3)])
(def defaults {:status nil :reason nil :consumed 0 :worn 0 :love false :leash nil :changed {} :wheat nil :carrot nil
               :lead nil :field {} :spawned [] :held nil})

(def interact-table
  [{:label "gone" :ent [] :args {:id 9} :want {:status "gone"}}
   {:label "no-item" :ent [(cow)] :args {:id 1 :item "wheat"} :want {:status "no-item"}}
   {:label "out of reach" :ent [(cow :pos [5 64 0])] :inv wheat :args {:id 1 :item "wheat"} :want {:status "out-of-reach" :wheat 3}}
   {:label "feed adult" :held "wheat" :ent [(cow)] :inv wheat :args {:id 1 :item "wheat"}
    :want {:status "used" :consumed 1 :love true :wheat 2 :field {:in-love true} :held "wheat"}}
   {:label "feed in love" :held "wheat" :ent [(cow :in-love true)] :inv wheat :args {:id 1 :item "wheat"} :want {:status "no-effect" :wheat 3 :held "wheat"}}
   {:label "cooldown" :held "wheat" :ent [(cow :cooldown true)] :inv wheat :args {:id 1 :item "wheat"} :want {:status "no-effect" :wheat 3 :held "wheat"}}
   {:label "baby" :held "wheat" :ent [(cow :baby true)] :inv wheat :args {:id 1 :item "wheat"} :want {:status "used" :consumed 1 :wheat 2 :held "wheat"}}
   {:label "wrong food" :ent [(cow)] :inv [(stack "carrot" 1)] :args {:id 1 :item "carrot"} :want {:status "no-effect" :carrot 1 :held "carrot"}}
   {:label "shear sheep" :ent [(cow :name "sheep" :sheared false)] :inv [(stack "shears" 1)] :args {:id 1 :item "shears"}
    :want {:status "used" :worn 1 :changed {:sheared [false true]} :field {:sheared true} :spawned ["white_wool"] :held "shears"}}
   {:label "shear sheared" :ent [(cow :name "sheep" :sheared true)] :inv [(stack "shears" 1)] :args {:id 1 :item "shears"} :want {:status "no-effect" :held "shears"}}
   {:label "shear a baby sheep" :ent [(cow :name "sheep" :baby true)] :inv [(stack "shears" 1)] :args {:id 1 :item "shears"} :want {:status "no-effect" :held "shears"}}
   {:label "lead" :ent [(cow)] :inv [(stack "lead" 1)] :args {:id 1 :item "lead"}
    :want {:status "used" :consumed 1 :leash "attached" :field {:leashed true :leashed-to-me true} :held "lead"}}
   {:label "lead on a led animal" :ent [(cow :leashed true)] :inv [(stack "lead" 1)] :args {:id 1 :item "lead"} :want {:status "no-effect" :lead 1 :held "lead"}}
   {:label "empty hand unleashes" :ent [(cow :leashed true :leashed-to-me true)] :args {:id 1}
    :want {:status "used" :leash "detached" :field {:leashed false :leashed-to-me false} :spawned ["lead"]}}
   {:label "empty hand unleashes, the lead picked up at once" :ent [(cow :leashed true :leashed-to-me true :pickup true)] :args {:id 1}
    :want {:status "used" :leash "detached" :field {:leashed false} :lead 1}}
   {:label "empty hand with no room is full" :held "wheat" :start "wheat" :ent [(cow)]
    :inv (mapv #(stack (str "item_" %) 1) (range 36)) :args {:id 1} :want {:status "full" :held "wheat"}}
   {:label "refused villager" :ent [(cow :name "villager")] :args {:id 1} :want {:status "cannot" :reason "opens-window"}}
   {:label "refused horse before no-item" :ent [(cow :name "horse")] :args {:id 1 :item "saddle"} :want {:status "cannot" :reason "mounts"}}
   {:label "refused boat" :ent [(cow :name "oak_boat")] :args {:id 1} :want {:status "cannot" :reason "mounts"}}
   {:label "mounts" :ent [(cow :mounts true)] :inv wheat :args {:id 1 :item "wheat"} :want {:status "failed" :reason "mounted" :wheat 3 :held "wheat"}}
   {:label "opens" :ent [(cow :opens true)] :inv wheat :args {:id 1 :item "wheat"} :want {:status "failed" :reason "opened-window" :wheat 3 :held "wheat"}}
   {:label "accepts false" :ent [(cow :accepts false)] :inv wheat :args {:id 1 :item "wheat"} :want {:status "no-effect" :wheat 3 :held "wheat"}}])

(defn summary [w r field]
  (merge (select-keys r [:status :reason :consumed :worn :love :leash :changed])
         {:wheat (count-of w "wheat") :carrot (count-of w "carrot") :lead (count-of w "lead")
          :field (select-keys (entity w 1) (keys field))
          :spawned (mapv #(get-in % [:item :name]) (filter #(= "item" (:kind %)) (:entities w)))
          :held (get-in w [:self :held])}))

(deftest interact-cases
  (doseq [{:keys [label ent inv args want start]} interact-table]
    (testing label
      (let [w0 (world {:entities ent :inventory inv :held start})
            [w r] (animals/interact w0 args)]
        (is (= (merge defaults want) (merge defaults (summary w r (:field want)))))))))

(deftest interact-full-leaves-the-hand-as-it-was
  (let [w0 (world {:entities [(cow)] :inventory (mapv #(stack (str "item_" %) 1) (range 36)) :held "wheat"})
        [w r] (animals/interact w0 {:id 1})]
    (is (= [w0 "full"] [w (:status r)]))))

;; ---- the lead

(def fenced {:blocks {[5 64 0] "oak_fence"} :entities [(cow)] :inventory [(stack "lead" 1)]})

(deftest a-lead-on-a-cow-marks-it-leashed-to-the-body
  (is (= [true true] ((juxt :leashed :leashed-to-me) (entity (lead-on (world fenced)) 1)))))

(deftest a-led-animal-follows-the-body-when-it-walks
  (let [w (walk (lead-on (world fenced)) [-30 64 0])]
    (is (close? 2 (flat (:pos (entity w 1)) [-30 64 0])))
    (is (true? (:leashed-to-me (entity w 1))))))

(deftest a-trail-keeps-the-animal-put-within-it-and-drags-it-that-far-behind-beyond
  (let [w (lead-on (world (assoc fenced :blocks {} :entities [(cow :trail 6)])))
        near (walk w [4 64 0])
        far (walk near [30 64 0])]
    (is (= [1 64 0] (:pos (entity near 1))))
    (is (close? 24 (first (:pos (entity far 1)))))))

(deftest a-led-animal-is-dragged-in-a-straight-line-and-a-fence-or-gate-stops-it
  (doseq [[label block state] [["fence" "oak_fence" {}] ["shut gate" "oak_fence_gate" {:open false}] ["open gate" "oak_fence_gate" {:open true}]]]
    (testing label
      (let [w (walk (lead-on (world (assoc fenced :blocks {[5 64 0] block} :states {[5 64 0] state}))) [30 64 0])
            x (first (:pos (entity w 1)))]
        (is (and (< 4 x) (< x 5)) (str "at " x))
        (is (true? (:leashed-to-me (entity w 1))))))))

(deftest a-lead-on-a-snapping-animal-breaks-on-the-walk-and-drops-as-an-item
  (let [w (walk (lead-on (world (assoc fenced :entities [(cow :snaps true)]))) [30 64 0])]
    (is (false? (:leashed-to-me (entity w 1))))
    (is (= 1 (lead-items w)))
    (is (= [1 64 0] (:pos (first (filter #(= "item" (:kind %)) (:entities w))))))))

(deftest breaks-at-breaks-the-lead-when-the-body-passes-that-distance-during-a-walk
  (let [w (lead-on (world (assoc fenced :blocks {} :entities [(cow :trail 6 :breaks-at 8 :pace 0.5)])))
        short (walk w [4 64 0])
        long (walk short [30 64 0])]
    (is (true? (:leashed-to-me (entity short 1))) "a walk that stays within breaks-at of it")
    (is (false? (:leashed-to-me (entity long 1))) "a 26-block walk by a body twice as fast as the animal")
    (is (= 1 (lead-items long)))))

(deftest breaks-at-with-pace-1-survives-a-long-walk
  (let [w (lead-on (world (assoc fenced :blocks {} :entities [(cow :trail 6 :breaks-at 8)])))
        far (walk w [30 64 0])
        farther (walk far [60 64 0])]
    (is (= [true true] [(:leashed-to-me (entity far 1)) (:leashed-to-me (entity farther 1))]))))

(defn tied [item]
  (let [w (lead-on (world (assoc fenced :inventory [(stack "lead" 2)] :pos [4 64 0])))]
    (use-on/use-on w (merge {:pos [5 64 0]} (when item {:item item})))))

(deftest a-click-on-a-fence-ties-the-led-animal-to-a-knot-there
  (doseq [item [nil "lead"]]
    (testing (str item)
      (let [[w r] (tied item)]
        (is (= "used" (:status r)))
        (is (= [true false (:id (knot w))] ((juxt :leashed :leashed-to-me :leash-holder) (entity w 1))))
        (is (= [5 64 0] (:pos (knot w))))))))

(deftest a-click-on-a-fence-with-nothing-led-changes-nothing
  (let [[w r] (use-on/use-on (world (assoc fenced :pos [4 64 0])) {:pos [5 64 0]})]
    (is (= "unchanged" (:status r)))
    (is (nil? (knot w)))))

(deftest an-empty-hand-on-the-knot-hands-the-animal-back-and-a-click-then-drops-the-lead
  (let [[w] (tied nil)
        [w1 r] (animals/interact w {:id (:id (knot w))})
        [w2] (animals/interact w1 {:id 1})]
    (is (= "used" (:status r)))
    (is (= [true true nil] ((juxt :leashed :leashed-to-me :leash-holder) (entity w1 1))))
    (is (nil? (knot w1)))
    (is (zero? (lead-items w1)))
    (is (= [false 1] [(:leashed (entity w2 1)) (lead-items w2)]))))

(deftest an-empty-hand-on-a-cow-tied-to-a-fence-does-not-free-it
  (let [w (first (tied nil))
        [w' r] (animals/interact (assoc-in w [:self :pos] [2 64 0]) {:id 1})]
    (is (nil? (:leash r)))
    (is (true? (:leashed (entity w' 1))))))

;; ---- the food's lure

(def wall (into {} (map (fn [z] [[10 64 z] "oak_fence"]) [-5 -4 -3 -2 -1 1 2 3 4 5])))
(defn gated [open entities]
  {:pos [14 64 0] :blocks (assoc wall [10 64 0] "oak_fence_gate") :states {[10 64 0] {:open open}} :entities entities
   :inventory [(stack "wheat" 4) (stack "lead" 1)]})
(defn cow-at [id x z & {:as extra}] (merge {:id id :name "cow" :kind "passive" :pos [x 64 z]} extra))

(deftest an-animal-walks-to-the-body-holding-its-food-and-stops-short-of-it
  (let [w (animals/tempt-follow (world {:pos [0 64 0] :entities [(cow-at 1 6 0)] :held "wheat"}))]
    (is (close? 2.5 (flat (:pos (entity w 1)) [0 64 0])))))

(deftest an-animal-is-not-drawn-by-an-empty-hand-by-food-it-does-not-eat-or-from-beyond-the-range
  (doseq [[label held x] [["empty" nil 6] ["lead" "lead" 6] ["far" "wheat" 12]]]
    (testing label
      (let [w (animals/tempt-follow (world {:pos [0 64 0] :entities [(cow-at 1 x 0)] :held held}))]
        (is (= [x 64 0] (:pos (entity w 1))))))))

(deftest a-shut-gate-keeps-the-animal-on-its-side
  (let [w (animals/tempt-follow (equip (world (gated false [(cow-at 1 6 3)])) "wheat"))]
    (is (< (first (:pos (entity w 1))) 10))))

(deftest an-open-gate-is-walked-through-also-from-off-its-axis
  (let [w (animals/tempt-follow (equip (world (gated true [(cow-at 1 6 3)])) "wheat"))]
    (is (>= (first (:pos (entity w 1))) 11) (str (:pos (entity w 1))))))

(deftest a-walking-body-draws-the-animal-too-a-leashed-one-is-left-to-its-lead
  (let [w (walk (world {:pos [0 64 0] :held "wheat"
                        :entities [(cow-at 1 3 0) (cow-at 2 3 2 :leashed true :leashed-to-me true)]})
                [-6 64 0])]
    (is (close? 2.5 (flat (:pos (entity w 1)) [-6 64 0])))
    (is (= [-8 64 0] (:pos (entity w 2))) "the lead drags it as before")))

;; ---- the lead's path regime

(def ring
  (concat (for [x (range 10 17) z [-3 3]] [x 64 z])
          (for [z [-2 -1 1 2]] [10 64 z])
          (for [z [-2 -1 0 1 2]] [16 64 z])))

(defn pen [open & [extra cow-z]]
  (world {:pos [7.5 64 0.5]
          :blocks (assoc (zipmap ring (repeat "oak_fence")) [10 64 0] "oak_fence_gate")
          :states {[10 64 0] {:open open}}
          :entities [(merge {:id 1 :name "cow" :kind "passive" :leashed true :leashed-to-me true :pos [3.5 64 (or cow-z 0.5)]} extra)]}))

(defn step [w x] (walk w [x 64 0.5]))
(def xs [8.5 9.5 10.5 11.5 12.5 13.5 14.5 15.5])
(defn behind [w] (flat (:pos (entity w 1)) (get-in w [:self :pos])))

(deftest short-steps-through-an-open-gate-the-cow-paths-behind-the-body-and-ends-inside
  (let [trail (reductions step (pen true) xs)]
    (doseq [[x w] (map vector xs (rest trail))]
      (is (<= 3.29 (behind w) 3.5) (str "body " x ": " (behind w))))
    (is (> (first (:pos (entity (last trail) 1))) 11))))

(deftest short-steps-with-the-gate-shut-the-cow-stays-outside
  (is (< (first (:pos (entity (reduce step (pen false) xs) 1))) 10)))

(deftest one-long-walk-drags-the-cow-in-a-straight-line-stopping-outside-at-the-gate
  (let [x (first (:pos (entity (step (pen true) 15.5) 1)))]
    (is (and (< 9 x) (< x 10)) (str "at " x))))

(deftest a-pinned-cow-never-moves-in-short-steps
  (is (= [3.5 64 0.5] (:pos (entity (reduce step (pen true {:pin true}) (take 2 xs)) 1)))))

(deftest a-cow-two-blocks-off-the-axis-ends-on-the-axis-and-passes-the-gate
  (is (> (first (:pos (entity (reduce step (pen true {} 2.5) xs) 1))) 11)))
