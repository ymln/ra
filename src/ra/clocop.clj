(ns ra.clocop
  (:refer-clojure :exclude [= <= * - +])
  (:require [clocop.core :as clocop :refer [with-store store constrain! int-var]]
            [clocop.constraints :as c]
            [ra.solver :refer [Solver]]))

(defn negate [arg]
  (c/$- 0 arg))

(defn make-problem []
  (let [store (store)]
    (reify Solver
      (variable [this name] (with-store store
                              (int-var name 0 1)))
      (add-constraint! [this constraint]
        (with-store store
          (constrain! constraint)))
      (=  [this args] (with-store store (apply  c/$=  args)))
      (<= [this args] (with-store store (apply  c/$<= args)))
      (*  [this args] (with-store store (reduce c/$*  args)))
      (+  [this args] (with-store store (reduce c/$+  args)))
      (solve! [this direction constraint]
        (with-store store (clocop/solve! :minimize (case direction
                                                     :minimize constraint
                                                     :maximize (negate constraint)))))
      (value [this x] (.value x)))))
