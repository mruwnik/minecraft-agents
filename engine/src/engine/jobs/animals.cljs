(ns engine.jobs.animals
  "Helpers for the animal jobs: what each mob breeds on, and the herd near the body."
  (:require [engine.jobs.util :as u]))

(def breeding-food
  "Mob name -> the foods that put it in love, in the order the body prefers them."
  {"cow" ["wheat"]
   "mooshroom" ["wheat"]
   "sheep" ["wheat"]
   "goat" ["wheat"]
   "pig" ["carrot" "potato" "beetroot"]
   "chicken" ["wheat_seeds" "melon_seeds" "pumpkin_seeds" "beetroot_seeds" "torchflower_seeds"]
   "rabbit" ["carrot" "golden_carrot" "dandelion"]
   "bee" ["dandelion" "poppy" "blue_orchid" "allium" "azure_bluet" "red_tulip" "orange_tulip"
          "white_tulip" "pink_tulip" "oxeye_daisy" "cornflower" "lily_of_the_valley" "sunflower"
          "lilac" "rose_bush" "peony" "torchflower" "pink_petals" "wildflowers"]})

(defn food-carried
  "The first food of mob's list the body carries, else nil."
  [p mob]
  (let [have (into #{} (map :name) (u/inventory p))]
    (first (filter have (get breeding-food mob)))))

(defn herd
  "The JS entities named mob within radius of the body, nearest first, as a vector."
  [p mob radius]
  (vec (array-seq (.entities p #js {:radius radius :names #js [mob] :max 64}))))

(defn adults
  "The herd's members that are not babies."
  [p mob radius]
  (filterv #(not (true? (.-baby %))) (herd p mob radius)))

(defn babies
  "The herd's babies."
  [p mob radius]
  (filterv #(true? (.-baby %)) (herd p mob radius)))

(defn key-of
  "The key an animal is tracked by: its uuid (entity ids change when chunks
  reload), else its id."
  [e]
  (or (.-uuid e) (.-id e)))

(defn leashed?
  "True when the sensing says the animal is on a lead (to anyone)."
  [e]
  (true? (.-leashed e)))

(defn led-by-me?
  "True when the animal is on this body's lead."
  [e]
  (true? (.-leashedToMe e)))

(defn find-by-key
  "The member of the herd tracked by key, or nil."
  [p mob radius k]
  (first (filter #(= k (key-of %)) (herd p mob radius))))
