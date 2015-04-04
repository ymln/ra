(ns ra.core
  (:use clocop.core clocop.constraints)
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

(defn paths-from [graph start]
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

(defn x-vals [x]
  (mapv (fn [row] (mapv #(.value %) row)) x))

(def oo 1000000) ;; infinity

(defn make-problem []
  (store))

(defn variable [name]
  (int-var name 0 1))

(def operators-map
  {=   $=
   <=  $<=
   *   $*
   -   $-
   +   $+})
(defn $ [f & args]
  (if (every? number? args)
    (apply f args)
    (apply (get operators-map f) args)))

(defn sum [lst] (apply (partial $ +) lst))

(defn matrix-inner-product [m1 m2]
  (sum (map (partial $ *) (flatten m1) (flatten m2))))

(defn time [p contractors-count x t]
  (sum (for [i p, j (range contractors-count)]
           ($ * (matrix-ref x (- i 1) j) (int (matrix-ref t (- i 1) j))))))

(defn finances [x f]
  (matrix-inner-product x f))

(defn quality [bp-count contractors-count x w q]
  (sum (for [i (range bp-count)
               j (range contractors-count)]
           ($ * (matrix-ref x i j)
              (* (nth w i)
                 (matrix-ref q i j))))) )

; example
; contractors columns       8
; bp          rows          5
(defn ra [w t f q F T graph]
  (with-store (store)
    (let [problem (make-problem)
          bp-count (matrix-rows t)
          contractors-count (matrix-columns t)
          P (paths graph)
          x (for [i (range bp-count)]
              (for [j (range contractors-count)]
                (variable (str "x" i "_" j))))
          quality-var (int-var "quality" 0 oo)]
      ;; sum of x = 1
      (doseq [i (range bp-count)]
        (constrain! ($ = 1 (sum (matrix-row x i)))))
      ;; finances
      (constrain! ($ <= (finances x f) F))
      ;; time
      (doseq [p P] (constrain! ($ <= (time p contractors-count x t) T)))
      ;; quality
      (constrain! ($ = quality-var (quality bp-count contractors-count x w q)))
      (solve! :minimize ($ - 0 quality-var))
      {:x (x-vals x)
       :quality (.value quality-var)
       :finances (finances x f)
       :time (apply max (for [p P] (time p contractors-count (x-vals x) t)))})))