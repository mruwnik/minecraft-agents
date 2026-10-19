(ns engine.sky-perception
  "A fake body's perception with the fake world's live sky but no light data of any cell: what sh-sense/sky-subtract-now reads."
  (:require [engine.fake.raw-world :as fake-raw]
            [engine.perception :as perception]))

(defn with-sky
  "p with a perception whose raw world knows the sky and no cell, returned."
  [p]
  (let [raw (fake-raw/create p)
        per (perception/create (doto raw (aset "stateAt" (fn [_ _ _] -1))) {:radius 16 :ray-deg 2})]
    (set! (.-mobSource ^js (:st per)) p)
    (aset p "perception" per)
    p))
