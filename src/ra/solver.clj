(ns ra.solver)

(defprotocol Solver
  (variable [this name])
  (add-constraint! [this constraint])
  (=  [this args])
  (<= [this args])
  (*  [this args])
  (-  [this args])
  (+  [this args])
  (solve [this direction constraint])
  (value [this x]))