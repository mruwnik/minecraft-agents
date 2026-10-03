(ns engine.conditions
  "Data conditions for per-round yields: a vector [kind & args] that survives
  a restart, unlike a fn. Add kinds with (defmethod holds? :kind ...).")

(defmulti holds?
  "Whether condition holds now. world is the primitives object, memory is
  {:common :body :job}, now is ms since epoch."
  (fn [condition _world _memory _now] (first condition)))

(defmethod holds? :day [_ world _ _] (.-isDay (.self world)))

(defmethod holds? :night [_ world _ _] (not (.-isDay (.self world))))

(defmethod holds? :after [[_ t] _ _ now] (>= now t))

(defmethod holds? :default [_ _ _ _] false)
