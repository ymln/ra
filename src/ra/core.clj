(ns ra.core
  (:require [ra.glpk :refer [make-problem]]
            [ra.solver :as solver :refer [variable add-constraint! solve! value]])
  (:gen-class))

(defn graph-vertices [graph]
  (into #{} (apply concat graph)))

(defn graph-next [graph v]
  (map second (filter (fn [[v1 v2]] (= v1 v)) graph)))

(defn graph-last? [graph v]
  (empty? (graph-next graph v)))

(defn graph-seconds [graph]
  (map second graph))

(defn graph-is-start? [graph vertice]
  (not (some #{vertice} (graph-seconds graph))))

(defn graph-starts [graph]
  (filter (partial graph-is-start? graph) (graph-vertices graph)))

(defmacro defmemofn [name & rest]
  `(def ~name (memoize (fn ~@rest))))

(defmemofn paths-from [graph start]
  (if (graph-last? graph start)
    [[start]]
    (let [after-start (graph-next graph start)
          paths-from-after-start (mapcat (partial paths-from graph) after-start)]
      (map (partial cons start) paths-from-after-start))))

(defn paths [graph]
  (let [starts (graph-starts graph)]
    (mapcat (partial paths-from graph) starts)))

(defn matrix-rows [m]
  (count m))

(defn matrix-columns [m]
  (count (first m)))

(defn matrix-column [m col]
  (mapv #(nth % col) m))

(defn matrix-row [m row]
  (nth m row))

(defn matrix-ref [m row col]
  (-> (matrix-row m row)
      (nth col)))

(def oo 1000000) ;; infinity

(defn flatten-matrix [m]
  (apply concat m))

; example
; contractors columns       8
; bp          rows          5
(defn ra [w t f q F T graph]
  (let [problem (make-problem)
        operators {=   solver/=
                   <=  solver/<=
                   *   solver/*
                   +   solver/+}
        $ (fn [f & args]
            (if (every? number? args)
              (apply f args)
              ((get operators f) problem args)))
        bp-count (matrix-rows t)
        contractors-count (matrix-columns t)
        P (paths graph)
        x (for [i (range bp-count)]
            (for [j (range contractors-count)]
              (variable problem (str "x" i "_" j))))
        sum (fn [lst] (apply (partial $ +) lst))
        matrix-inner-product (fn [m1 m2]
                               (sum (map #($ * %1 %2) (flatten-matrix m1) (flatten-matrix m2))))
        time (fn [p x]
               (sum (for [i p, j (range contractors-count)]
                      ($ * (matrix-ref x (- i 1) j) (int (matrix-ref t (- i 1) j))))))
        finances (fn [x] (matrix-inner-product x f))
        quality (fn [x]
                  (sum (for [i (range bp-count)
                             j (range contractors-count)]
                         ($ * (matrix-ref x i j)
                            (* (nth w i)
                               (matrix-ref q i j))))))
        x-vals (fn [x] (mapv (fn [row] (mapv #(value problem %) row)) x))]
    ;; sum of x = 1
    (doseq [i (range bp-count)]
      (add-constraint! problem ($ = (sum (matrix-row x i)) 1)))
    ;; finances
    (add-constraint! problem ($ <= (finances x) F))
    ;; time
    (doseq [p P] (add-constraint! problem ($ <= (time p x) T)))
    ;; quality
    (solve! problem :maximize (quality x))
    (let [xvals (x-vals x)]
      {:x xvals
       :quality (quality xvals)
       :finances (finances xvals)
       :time (apply max (for [p P] (time p xvals)))})))