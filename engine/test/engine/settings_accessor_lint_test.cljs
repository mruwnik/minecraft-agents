(ns engine.settings-accessor-lint-test
  "Lint over src: a settings accessor (a zero-arg defn that reads settings/get) is always called. Using its name as a
  value (a number <= a fn is false in JS) is a bug the compiler does not see."
  (:require [cljs.test :refer [deftest is]]
            [clojure.string :as str]
            ["fs" :as fs]
            ["path" :as path]))

(defn scan [dir]
  (mapcat (fn [e]
            (let [p (path/join dir (.-name e))]
              (cond (.isDirectory e) (scan p)
                    (re-find #"\.cljs$" (.-name e)) [p])))
          (.readdirSync fs dir #js {:withFileTypes true})))

(defn strip
  "Text without string literals and line comments."
  [text]
  (-> text
      (str/replace #"\"(?:\\.|[^\"\\])*\"" "\"\"")
      (str/replace #"(?m);.*$" "")))

(defn ns-of [text] (second (re-find #"\(ns\s+([^\s\)]+)" text)))

(defn aliases-of
  "The aliases a file gives each required namespace: {ns [alias ...]}, plus the full ns name."
  [text]
  (reduce (fn [m [_ n a]] (update m n (fnil conj [n]) a))
          {}
          (re-seq #"\[([\w.\-]+)\s+:as\s+([\w.\-]+)" text)))

(defn accessors-of
  "The names of the settings accessors a file defines."
  [text]
  (map second (re-seq #"\(defn\s+([^\s\[]+)\s+\[\]\s+\(settings/get\s" text)))

(defn violations
  "Value uses of accessors in files: any occurrence of an accessor name not directly after an open paren, or as the
  name a defn gives it. A bare name a file also destructures with :keys is a local and skipped."
  [files]
  (let [accessors (into {} (keep (fn [[_ raw]]
                                   (let [t (strip raw)]
                                     (when-let [as (seq (accessors-of t))] [(ns-of t) (vec as)]))))
                         files)]
    (vec
     (for [[file raw] files
           :let [text (strip raw)
                 self (ns-of text)
                 by-alias (aliases-of text)
                 refs (distinct (concat
                                 (for [[n names] accessors, nm names, a (get by-alias n)] (str a "/" nm))
                                 (for [nm (get accessors self)
                                       :when (not (re-find (re-pattern (str ":keys \\[[^\\]]*\\b" nm "\\b")) text))]
                                   nm)))]
           ref refs
           :let [re (js/RegExp. (str "(?<![\\w./:*+!?'#-])" (str/replace ref "." "\\.") "(?![\\w*+!?-])") "g")]
           hit (loop [acc []]
                 (if-let [m (.exec re text)]
                   (recur (conj acc (subs text (max 0 (- (.-index m) 6)) (.-index m))))
                   acc))
           :when (not (re-find #"\($|defn\s+$|defn-\s+$" hit))]
       [file ref]))))

(def fabricated
  {"a.cljs" "(ns a (:require [jobs.lib.walk :as walk]))\n(defn centred? [px] (<= px walk/centre-tolerance))"
   "b.cljs" "(ns b (:require [jobs.lib.walk :as walk]))\n(defn ok [px] (<= px (walk/centre-tolerance)))"
   "walk.cljs" "(ns jobs.lib.walk)\n(defn centre-tolerance [] (settings/get settings ::centre-tolerance))\n(defn f [x] {:tries centre-tolerance})\n(defn g [x] (centre-tolerance))"})

(deftest finds-an-accessor-used-as-a-value
  (is (= [["a.cljs" "walk/centre-tolerance"] ["walk.cljs" "centre-tolerance"]]
         (sort-by first (violations fabricated)))))

(deftest no-settings-accessor-is-read-as-a-value
  (let [files (into {} (map (fn [f] [f (.readFileSync fs f "utf8")])) (scan "src"))]
    (is (seq (mapcat accessors-of (vals files))) "the lint sees the accessors")
    (is (= [] (violations files)))))
