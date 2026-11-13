(ns engine.go-to-area-test
  "The body's work area (jobs.lib.walk.world ::area): a walk from inside it plans no step out of it."
  (:require [cljs.test :refer [deftest is async]]
            [engine.core :as core]
            [engine.fake :as fake]
            [engine.settings :as settings]
            [engine.go-to-test :as gt]
            [engine.test-util :as tu :refer [box floor]]))

;; a wall across x 8 (two high: no jump over it) with a gap at z -5, outside an area of z -3..10, and one at z 8, inside it
(def walled
  (merge (floor -2 -6 20 10)
         (box 8 64 -6 8 65 -6 "stone") (box 8 64 -4 8 65 7 "stone") (box 8 64 9 8 65 10 "stone")))

(def area {:jobs.lib.walk.world/area {:min [-2 60 -3] :max [20 70 10]}})

(defn ^:async go-in-area!
  "gt/go! over walled with settings (default: the area set); :zs every feet z the body stood at."
  [args & [overrides]]
  (await (settings/with-settings (or overrides area)
           (fn ^:async run []
             (let [{:keys [eng p] :as s} (gt/setup {:blocks walled})
                   out (atom :not-done)
                   zs (atom [])
                   eng (assoc eng :jobs (assoc (:jobs eng) 'recording-parent (gt/recording-parent out args)))]
               (add-watch (fake/state p) ::z (fn [_ _ _ w] (swap! zs conj (js/Math.floor (nth (get-in w [:self :pos]) 2)))))
               (core/submit! eng (tu/order '(recording-parent {})) {})
               (let [ticks (await (gt/tick-out! eng 30))]
                 (assoc s :out @out :zs @zs :ticks ticks)))))))

(deftest a-walk-inside-the-area-goes-round-by-the-gap-inside-it
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [args {:pos [16 64 0] :range 0 :ways #{:walk}}
              {:keys [out zs]} (await (go-in-area! args))
              free (await (go-in-area! args {}))]
          (is (some #(< % -3) (:zs free)) "without the area the gap at z -5 is the nearer way")
          (is (= {:arrived true} out))
          (is (every? #(>= % -3) zs) (str "left the area: " (pr-str (distinct zs)))))))))

(deftest a-goal-outside-the-area-ends-at-once
  (async done
    (tu/run-async done
      (fn ^:async t []
        (let [{:keys [out zs]} (await (go-in-area! {:pos [16 64 -5] :range 0}))]
          (is (= [false :outside-area] [(:arrived out) (:reason out)]) (pr-str out))
          (is (every? zero? zs) "no step taken"))))))

(deftest a-goal-inside-the-area-with-no-way-inside-it-gives-up
  (async done
    (tu/run-async done
      (fn ^:async t []
        ;; the gap at z 8 shut: only the one at z -5, outside the area, is left
        (let [{:keys [out]} (await (settings/with-settings area
                                        #(gt/go! {:blocks (merge walled (box 8 64 8 8 65 8 "stone"))} {:pos [16 64 0] :range 0 :ways #{:walk}})))]
          (is (= [false :unreachable] [(:arrived @out) (:reason @out)]) (pr-str @out)))))))
