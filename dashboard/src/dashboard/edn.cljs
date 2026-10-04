(ns dashboard.edn
  "Read exactly one EDN form in both the browser and Node."
  (:require [cljs.tools.reader.edn :as edn]
            [cljs.tools.reader.reader-types :as readers]))

(defn one-form [text]
  (let [r (readers/string-push-back-reader text) eof (js-obj)
        value (edn/read {:eof eof} r)]
    (when (identical? value eof) (throw (js/Error. "empty EDN file")))
    (when-not (identical? eof (edn/read {:eof eof} r))
      (throw (js/Error. "EDN file must contain exactly one form")))
    value))
