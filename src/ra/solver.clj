(ns ra.solver
  (:refer-clojure :exclude [= <= * +]))

(defprotocol Solver
  (variable [this name])
  (add-constraint! [this constraint])
  (=  [this args])
  (<= [this args])
  (*  [this args])
  (+  [this args])
  (solve! [this direction constraint])
  (value [this x]))