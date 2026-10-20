(ns engine.edn-fast
  "A fast reader for the EDN the event log writes: maps, vectors, sets, strings without escapes, keywords, plain numbers,
  nil, true and false. Anything else throws, so the caller falls back to cljs.reader.")

(defn fail [] (throw (js/Error. "edn-fast: unsupported")))

(defn delimiter? [c]
  (or (<= c 32) (== c 44) (== c 125) (== c 93) (== c 41) (== c 34) (== c 123) (== c 91) (== c 40)))

(def number-re #"^-?\d+(\.\d+)?([eE][+-]?\d+)?$")

(def escapes {"\"" "\"" "\\" "\\" "n" "\n" "t" "\t" "r" "\r"})

(defn read-escaped
  "[string next-index] of the string opening at i, with the simple escapes decoded."
  [^string s i]
  (let [n (.-length s)
        out (array)]
    (loop [j (inc i)]
      (when (>= j n) (fail))
      (let [ch (.charAt s j)]
        (cond
          (= ch "\"") [(.join out "") (inc j)]
          (= ch "\\") (let [e (get escapes (.charAt s (inc j)))]
                         (when-not e (fail))
                         (.push out e)
                         (recur (+ j 2)))
          :else (do (.push out ch) (recur (inc j))))))))

(defn read-form
  "[value next-index] of the form in s starting at or after i."
  [^string s i]
  (let [n (.-length s)
        skip (fn [i] (loop [i i] (if (and (< i n) (let [c (.charCodeAt s i)] (or (<= c 32) (== c 44)))) (recur (inc i)) i)))
        i (skip i)]
    (when (>= i n) (fail))
    (let [c (.charCodeAt s i)]
      (cond
        (== c 123) ; {
        (loop [i (skip (inc i)) m (transient {})]
          (when (>= i n) (fail))
          (if (== (.charCodeAt s i) 125)
            [(persistent! m) (inc i)]
            (let [[k i] (read-form s i)
                  [v i] (read-form s i)]
              (recur (skip i) (assoc! m k v)))))

        (== c 91) ; [
        (loop [i (skip (inc i)) v (transient [])]
          (when (>= i n) (fail))
          (if (== (.charCodeAt s i) 93)
            [(persistent! v) (inc i)]
            (let [[x i] (read-form s i)]
              (recur (skip i) (conj! v x)))))

        (and (== c 35) (< (inc i) n) (== (.charCodeAt s (inc i)) 123)) ; #{
        (loop [i (skip (+ i 2)) v (transient #{})]
          (when (>= i n) (fail))
          (if (== (.charCodeAt s i) 125)
            [(persistent! v) (inc i)]
            (let [[x i] (read-form s i)]
              (recur (skip i) (conj! v x)))))

        (== c 34) ; "
        (let [end (.indexOf s "\"" (inc i))
              plain (when (>= end 0) (.substring s (inc i) end))]
          (if (and plain (neg? (.indexOf plain "\\")))
            [plain (inc end)]
            (read-escaped s i)))

        :else
        (let [end (loop [j (inc i)] (if (and (< j n) (not (delimiter? (.charCodeAt s j)))) (recur (inc j)) j))
              token (.substring s i end)]
          (cond
            (== c 58) (if (> (.-length token) 1) [(keyword (.substring token 1)) end] (fail))
            (= token "nil") [nil end]
            (= token "true") [true end]
            (= token "false") [false end]
            (re-find number-re token) [(js/parseFloat token) end]
            :else (fail)))))))

(defn read-string
  "The form in text, which must be exactly one; throws when text holds anything this reader does not cover."
  [^string text]
  (let [[value end] (read-form text 0)]
    (when (not= end (.-length (.trimEnd text))) (fail))
    value))
