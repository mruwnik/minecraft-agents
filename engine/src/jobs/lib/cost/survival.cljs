(ns jobs.lib.cost.survival
  "The Gamma-Poisson survival shape both memory fades share: the chance nothing has happened after x, under a rate known
  only as a Gamma(a, b) prior. Pure.")

(defn gamma-survival
  "(1 + x/b)^-a: the chance none of a events (the prior's count) per b (its span, same unit as x) has happened in x."
  [a b x]
  (js/Math.pow (+ 1 (/ x b)) (- a)))
