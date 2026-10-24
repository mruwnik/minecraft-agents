(ns world-test.build
  "Pure part of the body-build freshness check (runner: rebuild engine/out/body-release.cjs when a source is newer).")

(def body-v8-flags
  "V8 flags of a body process; they only work on the command line (heap cap: RSS 335 -> ~255 MB). engine/package.json \"body\" keeps the one other copy."
  ["--max-old-space-size=256" "--heap-growing-percent=100" "--max-semi-space-size=8"])

(defn stale?
  "True when the build (mtime in ms, nil = missing) is missing or older than the newest of the source mtimes."
  [build-mtime source-mtimes]
  (or (nil? build-mtime)
      (boolean (some #(> % build-mtime) source-mtimes))))
