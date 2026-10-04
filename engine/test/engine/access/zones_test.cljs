(ns engine.access.zones-test
  "engine.access.zones: the social verdict over zones, claims and plan footprints, as a table."
  (:require [cljs.test :refer [deftest is are]]
            [engine.access.zones :as zones]))

(def now 1000)
(def mine {:name "mine" :min [0 60 0] :max [9 70 9] :owner "Bot"})
(def theirs {:name "theirs" :min [20 60 0] :max [29 70 9] :owner "Miles"})
(def unknown {:name "lib" :min [40 60 0] :max [49 70 9] :owner "unknown"})
(def open-to-take (assoc theirs :allow #{:take}))
(def inside-theirs {:name "inner" :min [22 62 2] :max [24 64 4] :owner "Bot"})
(def my-claim {:id "c1" :owner "bot" :status :active :until 5000 :min [60 60 0] :max [69 70 9]})
(def their-claim {:id "c2" :owner "Miles" :status :active :until 5000 :min [80 60 0] :max [89 70 9]})
(def expired (assoc their-claim :until 900))
(def released (assoc their-claim :status :released))

(defn v [in] (select-keys (zones/verdict (merge {:zones [] :claims [] :footprints {} :self "Bot" :now now} in))
                          [:ok :why :reason :zone :owner :claim :plan]))

(deftest verdict-table
  (are [in expected] (= expected (v in))
    {:zones nil :action :dig :cell [0 64 0]} {:ok false :reason :no-zones}
    {:zones [] :action :dig :cell [0 64 0]} {:ok true :why :open}
    {:zones [mine] :action :dig :cell [1 64 1]} {:ok true :why :own-zone}
    {:zones [mine] :action :place :cell [1 64 1]} {:ok true :why :own-zone}
    {:zones [mine] :action :take :cell [1 64 1]} {:ok true :why :own-zone}
    {:zones [mine] :action :dig :cell [1 64 1] :self "BOT"} {:ok true :why :own-zone}
    {:zones [theirs] :action :dig :cell [21 64 1]} {:ok false :reason :zone :zone "theirs" :owner "Miles"}
    {:zones [theirs] :action :harvest :cell [21 64 1]} {:ok false :reason :zone :zone "theirs" :owner "Miles"}
    {:zones [theirs] :action :dig :cell [1 64 1]} {:ok true :why :open}
    {:zones [open-to-take] :action :take :cell [21 64 1]} {:ok true :why :open}
    {:zones [open-to-take] :action :dig :cell [21 64 1]} {:ok false :reason :zone :zone "theirs" :owner "Miles"}
    {:zones [unknown] :action :take :cell [41 64 1]} {:ok false :reason :zone :zone "lib" :owner "unknown"}
    {:zones [theirs inside-theirs] :action :dig :cell [23 63 3]} {:ok false :reason :zone :zone "theirs" :owner "Miles"}
    {:zones [theirs] :action :put :cell [21 64 1]} {:ok false :reason :zone :zone "theirs" :owner "Miles"}
    {:zones [(assoc theirs :allow #{:put})] :action :put :cell [21 64 1]} {:ok true :why :open}
    ;; footprints of other plans
    {:zones [mine] :footprints {[1 64 1] "hut"} :action :dig :cell [1 64 1]} {:ok false :reason :footprint :plan "hut"}
    {:zones [] :footprints {[1 64 1] "hut"} :action :place :cell [1 64 1]} {:ok false :reason :footprint :plan "hut"}
    {:zones [] :footprints {[1 64 1] "hut"} :action :place :cell [2 64 1]} {:ok true :why :open}
    ;; claims
    {:zones [] :claims [their-claim] :action :dig :cell [81 64 1]} {:ok false :reason :claim :claim "c2" :owner "Miles"}
    {:zones [] :claims [their-claim] :action :take :cell [81 64 1]} {:ok false :reason :claim :claim "c2" :owner "Miles"}
    {:zones [] :claims [expired] :action :dig :cell [81 64 1]} {:ok true :why :open}
    {:zones [] :claims [released] :action :dig :cell [81 64 1]} {:ok true :why :open}
    {:zones [] :claims [my-claim] :action :dig :cell [61 64 1]} {:ok true :why :own-claim}
    {:zones [] :claims [their-claim] :action :dig :cell [1 64 1]} {:ok true :why :open}))

(deftest the-three-open-decisions-are-named-defaults
  (is (false? zones/deposit-into-foreign-chest?))
  (is (true? zones/plan-footprint-beats-zone?))
  (is (true? zones/unknown-owner-foreign?)))
