(ns world-test.build
  "Pure part of the body-build freshness check (runner: rebuild engine/out/body-release.cjs when its recorded source hash is not the tree's)."
  (:require [clojure.string :as str]))

(def body-v8-flags
  "V8 flags of a body process; they only work on the command line (heap cap: RSS 335 -> ~255 MB). engine/package.json \"body\" keeps the one other copy."
  ["--max-old-space-size=256" "--heap-growing-percent=100" "--max-semi-space-size=8"])

(defn parse-src
  "[hash commit] of a tools/engine-src-hash line (or an engine/out/body-release.src file), nil when blank."
  [text]
  (let [[h c] (str/split (str/trim (str text)) #"\s+")]
    (when (seq h) [h c])))

(defn stale?
  "True when the bundle's recorded source hash (nil = no record) differs from the tree's: a compile that began before a
  change records the older hash, however late it finished."
  [recorded current]
  (or (nil? recorded) (not= (first recorded) (first current))))

(defn decide
  "What the runner does with the bundle: :unreadable (the tree hash is nil, so a rebuild could never be recorded as
  current), :current or :rebuild."
  [recorded current]
  (cond (nil? current) :unreadable
        (stale? recorded current) :rebuild
        :else :current))

(defn dirty? [[_ commit]] (boolean (some-> commit (str/ends-with? "-dirty"))))
