(ns engine.zones-test
  "engine.zones: the zone file's validation and parse."
  (:require [cljs.test :refer [deftest is are]]
            [engine.zones :as zones]))

(def farm {:name "farm" :min [0 60 0] :max [9 70 9] :owner "Miles"})

(deftest a-good-zone-list-has-no-errors
  (are [zs] (= [] (zones/zone-errors zs))
    []
    [farm]
    [farm (assoc farm :name "pen" :allow #{:harvest})]
    [(assoc farm :allow #{:dig :place :harvest} :note "welcome")]
    [(assoc farm :min [5 64 5] :max [5 64 5])]))

(deftest each-problem-is-named-with-its-zone
  (are [zs error] (= [error] (zones/zone-errors zs))
    {} "the zone file must hold a vector of zones"
    '(1) "the zone file must hold a vector of zones"
    [3] "zone 0: not a map"
    [(dissoc farm :name)] "zone 0: :name must be a non-empty string"
    [(assoc farm :name "")] "zone 0: :name must be a non-empty string"
    [farm farm] "zone farm: the name is used twice"
    [(dissoc farm :min)] "zone farm: :min must be [x y z] integers"
    [(assoc farm :max [1 2])] "zone farm: :max must be [x y z] integers"
    [(assoc farm :max [1.5 2 3])] "zone farm: :max must be [x y z] integers"
    [(assoc farm :min [0 80 0])] "zone farm: :min is above :max on y"
    [(dissoc farm :owner)] "zone farm: :owner must be a non-empty string"
    [(assoc farm :allow [:dig])] "zone farm: :allow must be a set of :dig :place :harvest :take :put"
    [(assoc farm :allow #{:fly})] "zone farm: :allow must be a set of :dig :place :harvest :take :put"
    [(assoc farm :allows #{:dig})] "zone farm: unknown key :allows"
    [(assoc farm :note 3)] "zone farm: :note must be a string"))

(deftest a-zone-with-several-problems-names-each
  (is (= ["zone farm: :min is above :max on x" "zone farm: :owner must be a non-empty string"]
         (zones/zone-errors [(-> farm (assoc :min [20 60 0]) (dissoc :owner))]))))

(deftest parse-gives-the-zones-or-the-errors
  (is (= {:value [farm]} (zones/parse (pr-str [farm]))))
  (is (= {:value []} (zones/parse "[]")))
  (is (re-find #"^unreadable EDN" (first (:errors (zones/parse "[{:name ")))))
  (is (= {:errors ["zone 0: not a map"]} (zones/parse "[1]"))))

(deftest allow-accepts-take-and-put
  (are [zs] (= [] (zones/zone-errors zs))
    [(assoc farm :allow #{:take})]
    [(assoc farm :allow #{:take :put :dig})]))

(def a-claim {:id "c1" :owner "Miles" :status :active :until 5000 :min [0 60 0] :max [9 70 9]})

(deftest parse-claims-reads-good-files-and-names-each-problem
  (are [text expected] (= expected (zones/parse-claims text))
    (pr-str [a-claim]) {:value [a-claim]}
    "[]" {:value []}
    "{}" {:errors ["the claims file must hold a vector of claims"]}
    "[3]" {:errors ["claim 0: not a map"]}
    (pr-str [(dissoc a-claim :owner)]) {:errors ["claim c1: :owner must be a non-empty string"]}
    (pr-str [(assoc a-claim :status :gone)]) {:errors ["claim c1: :status must be :active or :released"]}
    (pr-str [(assoc a-claim :x 1)]) {:errors ["claim c1: unknown key :x"]}))

(deftest parse-claims-reports-unreadable-edn
  (is (re-find #"^unreadable EDN" (first (:errors (zones/parse-claims "[{:id "))))))
