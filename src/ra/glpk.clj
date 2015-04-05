(ns ra.glpk
  (:require [ra.solver :refer :all]
            [clojure.core :as c])
  (:refer-clojure :exclude [= <= * - +])
  (:import (org.gnu.glpk GLPK GLPKConstants glp_smcp)))

(defn normalize-sum [sum]
  (assert (c/= (first sum) :+))
  (into {} (map (fn [x]
                  (if (c/= (first x) :variable)
                    [(second x) 1]
                    (let [[op var val] x]
                      (assert (c/= op :*))
                      [(second var) val])))
                (rest sum))))

(defn add-objective! [problem direction constraint]
  (GLPK/glp_set_obj_dir problem (case direction
                                  :minimize GLPKConstants/GLP_MIN
                                  :maximize GLPKConstants/GLP_MAX))
  (let [nsum (normalize-sum constraint)
        len (GLPK/glp_get_num_cols problem)]
    (doseq [i (range len)]
      (GLPK/glp_set_obj_coef problem (inc i) (get nsum (inc i) 0)))))

(defn make-problem []
  (let [problem (GLPK/glp_create_prob)
        solution (atom {})]
    (reify Solver
      (variable [this name]
        (let [col-number (GLPK/glp_add_cols problem 1)]
          (GLPK/glp_set_col_name problem col-number name)
          (GLPK/glp_set_col_kind problem col-number GLPKConstants/GLP_BV)
          [:variable col-number]))
      (add-constraint! [this constraint]
        (let [[boundary-type sum bound] constraint
              row-num (GLPK/glp_add_rows problem 1)]
          (GLPK/glp_set_row_bnds problem row-num
                                 (case boundary-type
                                   :<= GLPKConstants/GLP_UP
                                   :=  GLPKConstants/GLP_FX)
                                 bound bound)
          (let [nsum (normalize-sum sum)
                len (GLPK/glp_get_num_cols problem)
                ind (GLPK/new_intArray (inc len))
                val (GLPK/new_doubleArray (inc len))]
            (doseq [i (range len)]
              (GLPK/intArray_setitem ind (inc i) (inc i))
              (GLPK/doubleArray_setitem val (inc i) (get nsum (inc i) 0)))
            (GLPK/glp_set_mat_row problem row-num len ind val)
            (GLPK/delete_intArray ind)
            (GLPK/delete_doubleArray val))))
      (=  [this args] (apply conj [] :=  args))
      (<= [this args] (apply conj [] :<= args))
      (*  [this args] (apply conj [] :*  args))
      (+  [this args] (apply conj [] :+  args))
      (solve! [this direction constraint]
        (add-objective! problem direction constraint)
        (let [parm (glp_smcp.)]
          (GLPK/glp_init_smcp parm)
          (if (not= (GLPK/glp_simplex problem parm) 0)
            (throw (Exception. "Could not solve the problem"))))
        (doseq [i (range (GLPK/glp_get_num_cols problem))]
          (swap! solution assoc (inc i) (GLPK/glp_get_col_prim problem (inc i))))
        (GLPK/glp_delete_prob problem)
        nil)
      (value [this x]
        (let [i (second x)]
          (int (get @solution i)))))))