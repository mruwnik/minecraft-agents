(ns engine.job-arg-types-test
  "Job args the spec :type cannot describe: a wrong list or map value is declined :bad-args by the check, and scalar args are typed at submit."
  (:require [cljs.test :refer [deftest is are]]
            [engine.expr :as expr]
            [engine.registry :as registry]
            [engine.test-util :as tu]))

(defn check-wait
  "The wait reason map of job's check over args, or :passed."
  [job args]
  (let [wait (atom nil)
        c {:primitives (tu/fake {}) :args args :wait wait :view (constantly {}) :root "arg-types" :slots []}
        r ((:check (get registry/jobs job)) c)]
    (if (false? r) @wait :passed)))

(deftest wrong-list-args-decline-bad-args
  (are [job args] (let [w (check-wait job args)] (and (= :bad-args (:reason w)) (string? (:why w))))
    'jobs.blocks.dig {:pos [1 64 1] :accept false}
    'jobs.blocks.dig {:pos [1 64 1] :accept [1 2]}
    'jobs.combat.attack {:targets false}
    'jobs.combat.attack {:targets {:x "a"}}
    'jobs.combat.attack {:targets [true] :weapons "sword"}
    'jobs.farm.tend {:box {:min {:x 0 :y 63 :z 0} :max {:x 1 :y 64 :z 1}} :keep [1 2]}
    'jobs.farm.tend {:box {:x "a"}}
    'jobs.items.obtain {:any-of true}
    'jobs.items.obtain {:item "stick" :how "chest"}
    'jobs.storage.deposit {:items true}
    'jobs.storage.deposit {:keep [1 2]}
    'jobs.storage.withdraw {:items true}
    'jobs.storage.withdraw {:items ["a"]}
    'jobs.storage.kit {:tools false}
    'jobs.storage.kit {:craft-tiers {:x "a"}}
    'jobs.survival.dig-in {:blocks false}
    'jobs.survival.retreat {:weapons true}
    'jobs.survival.retreat {:blocks {:x "a"}}))

(deftest well-formed-list-args-pass-the-guard
  (are [job args] (= :passed (let [w (check-wait job args)] (if (= :bad-args (:reason w)) w :passed)))
    'jobs.combat.attack {:targets ["zombie"] :weapons ["sword"]}
    'jobs.items.obtain {:item "stick" :how #{:chest :craft}}
    'jobs.storage.deposit {:items ["stick"] :keep {"bread" 3}}))

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
