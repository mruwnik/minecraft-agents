(ns world-test.build
  "Pure part of the body-build freshness check (runner: rebuild engine/out/body.cjs when a source is newer).")

(defn stale?
  "True when the build (mtime in ms, nil = missing) is missing or older than the newest of the source mtimes."
  [build-mtime source-mtimes]
  (or (nil? build-mtime)
      (boolean (some #(> % build-mtime) source-mtimes))))
