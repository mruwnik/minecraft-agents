(ns engine.job-arg-types-test
  "Job args are checked against their specs at submit: a wrong list, map or scalar value is refused."
  (:require [cljs.test :refer [deftest is]]
            [engine.expr :as expr]
            [engine.registry :as registry]))

(deftest wrong-list-args-are-refused-at-submit
  (doseq [form ['(jobs.blocks.dig {:pos [1 64 1] :accept false})
                '(jobs.blocks.dig {:pos [1 64 1] :accept [1 2]})
                '(jobs.combat.attack {:targets false})
                '(jobs.combat.attack {:targets {:x "a"}})
                '(jobs.combat.attack {:targets [true] :weapons "sword"})
                '(jobs.farm.tend {:box {:min {:x 0 :y 63 :z 0} :max {:x 1 :y 64 :z 1}} :keep [1 2]})
                '(jobs.farm.tend {:box {:x "a"}})
                '(jobs.items.obtain {:any-of true})
                '(jobs.items.obtain {:item "stick" :how "chest"})
                '(jobs.storage.deposit {:items true})
                '(jobs.storage.deposit {:keep [1 2]})
                '(jobs.storage.withdraw {:items true})
                '(jobs.storage.withdraw {:items ["a"]})
                '(jobs.storage.kit {:tools false})
                '(jobs.storage.kit {:craft-tiers {:x "a"}})
                '(jobs.survival.dig-in {:blocks false})
                '(jobs.survival.retreat {:weapons true})
                '(jobs.survival.retreat {:blocks {:x "a"}})]]
    (is (string? (expr/problem registry/jobs form)) (pr-str form))))

(deftest well-formed-list-args-pass
  (doseq [form ['(jobs.combat.attack {:targets ["zombie"] :weapons ["sword"]})
                '(jobs.items.obtain {:item "stick" :how #{:chest :craft}})
                '(jobs.storage.deposit {:items ["stick"] :keep {"bread" 3}})]]
    (is (nil? (expr/problem registry/jobs form)) (pr-str form))))

(deftest scalar-args-are-typed-at-submit
  (doseq [form ['(jobs.blocks.dig {:pos [1 64 1] :collect "text"})
                '(jobs.blocks.dig {:pos [1 64 1] :on-fluid :sideways})
                '(jobs.combat.attack {:radius "text"})
                '(jobs.combat.attack {:absent :never})
                '(jobs.combat.attack {:max-hits -7})
                '(jobs.farm.tend {:till 3})
                '(jobs.movement.boat-drive {:pos [1 63 1] :range "text"})
                '(jobs.movement.boat-drive {:pos [1 63 1] :max-strokes "text"})
                '(jobs.movement.boat-drive {:pos [1 63 1] :max-strokes 0})
                '(jobs.movement.boat-drive {:pos [1 63 1] :max-s "120"})
                '(jobs.movement.boat-land {:radius "far"})
                '(jobs.movement.boat-land {:max-spots 0})
                '(jobs.items.obtain {:item 3})
                '(jobs.items.obtain {:item "stick" :count 0})
                '(jobs.storage.deposit {:free "text"})
                '(jobs.storage.deposit {:ignore-zones? 3})
                '(jobs.storage.kit {:spare -7})
                '(jobs.storage.kit {:craft :kw})
                '(jobs.storage.kit {:food "text"})
                '(jobs.survival.dig-in {:max-places 0})
                '(jobs.survival.dig-in {:enclose "text"})
                '(jobs.survival.retreat {:radius "text"})
                '(jobs.survival.retreat {:step -7})]]
    (is (string? (expr/problem registry/jobs form)) (pr-str form)))
  (doseq [form ['(jobs.blocks.dig {:pos [1 64 1] :on-fluid :fail})
                '(jobs.combat.attack {:radius 16 :absent :wait})
                '(jobs.movement.boat-drive {:pos [1 63 1] :range 2 :max-strokes 10 :max-s 30})
                '(jobs.items.obtain {:item "stick" :count 64})
                '(jobs.items.obtain {:item "stick" :count 100})
                '(jobs.storage.deposit {:free 2})
                '(jobs.storage.kit {:spare 0})
                '(jobs.survival.dig-in {:max-places 4 :enclose true})
                '(jobs.survival.retreat {:radius 8 :step 6})]]
    (is (nil? (expr/problem registry/jobs form)) (pr-str form))))

(deftest shape-errors-once-caught-in-a-round-are-refused-at-submit
  (doseq [form ['(jobs.items.get-tool {:block 5})
                '(jobs.items.get-tool {:kind 5})
                '(jobs.movement.go-to {:pos [24 0 16] :tolls [{:x 3 :y 64 :z 0}]})
                '(jobs.movement.go-to {:pos [24 0 16] :tolls [{:x 3 :y 64 :z 0 :factor -1}]})
                '(jobs.movement.go-to {:pos [24 0 16] :tolls {:x 3 :y 64 :z 0 :factor 2}})
                '(jobs.movement.go-to {:pos [22 0 16] :drop-cost -1})]]
    (is (string? (expr/problem registry/jobs form)) (pr-str form)))
  (is (nil? (expr/problem registry/jobs '(jobs.movement.go-to {:pos [24 0 16] :tolls [{:x 0 :y 0 :z 0 :factor 3}]})))))
