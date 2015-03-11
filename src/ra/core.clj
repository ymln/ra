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

(defn $matrix-inner-product [m1 m2]
  ($weighted-sum (flatten m1) (flatten m2)))

(defn matrix-rows [m]
  (count m))

(defn matrix-columns [m]
  (count (first m)))

(defn matrix-column [m col]
  (mapv #(nth % col) m))

(defn matrix-row [m row]
  (nth m row))

(defn $sum [lst]
  (apply $+ lst))

(defn matrix-ref [m row col]
  (-> (matrix-row m row)
      (nth col)))

(defn x-vals [x]
  (mapv (fn [row] (mapv #(.value %) row)) x))

; contractors columns       8
; bp          rows          5
(defn ra [w t f q F T graph]
  (with-store (store)
    (let [bp-count (matrix-rows t)
          contractors-count (matrix-columns t)
          P (paths graph)
          x (for [i (range bp-count)]
              (for [j (range contractors-count)]
                (int-var (str "x" i "_" j) 0 1)))
          quality (int-var "quality" 0 1000000000)]
      (doseq [i (range bp-count)]
        (constrain! ($= 1 ($sum (matrix-row x i)))))

      (constrain! ($<= ($matrix-inner-product x f) F))

      (doseq [p P]
        (constrain! ($<= ($sum (for [i p, j (range contractors-count)]
                                 ($* (matrix-ref x (- i 1) j) (matrix-ref t (- i 1) j))))
                         T)))
      (constrain! ($= quality ($sum (for [i (range bp-count)
                                          j (range contractors-count)]
                                      ($* (matrix-ref x i j)
                                          (* (nth w i)
                                             (matrix-ref q i j)))))))
      (solve! :minimize ($- 0 quality))
      [(x-vals x) (.value quality)])))

(def oo 10000000) ;; infinity

(def t
  [[5  3   oo  oo  oo]
   [4  5   oo  oo  oo]
   [4  3   oo  oo  oo]
   [10 8   3   5   oo]
   [15 6   oo  5   oo]
   [20 oo  3   5   oo]
   [20 oo  4   7   oo]
   [30 15  oo  15  20]])

(def f
  [[5000  10000 oo    oo    oo]
   [5000  5000  oo    oo    oo]
   [5000  5000  oo    oo    oo]
   [20000 15000 5000  10000 oo]
   [25000 15000 oo    15000 oo]
   [30000 oo    10000 15000 oo]
   [40000 oo    30000 25000 oo]
   [50000 15000 oo    15000 25000]])

(def q
  [[10 3 0 0 0]
   [10 4 0 0 0]
   [10 4 0 0 0]
   [10 6 2 5 0]
   [9  9 0 7 0]
   [5  0 8 9 0]
   [6  0 7 8 0]
   [8  7 0 7 9]])

(def w [30 20 10 20 10 5 3 2])

(def graph
  [[6 2]
   [5 2]
   [2 7]
   [2 8]
   [7 3]
   [8 3]
   [3 4]
   [4 1]])

(defn my-ra [F T]
  (ra w t f q F T graph))

(defn quality [q w x]
  (apply + (for [i (range (matrix-rows x))
                 j (range (matrix-columns x))]
             (* (matrix-ref x i j)
                (nth w i)
                (matrix-ref q i j)))))

#_(test :F 100000 :T 60 => :q 0.968
        [[1 0 0 0 0]
         [1 0 0 0 0]
         [1 0 0 0 0]
         [1 0 0 0 0]
         [0 1 0 0 0]
         [0 0 1 0 0]
         [0 0 0 1 0]
         [0 1 0 0 0]])

(defn -main
  [& args]
  (let [F (Integer/valueOf (nth args 0))
        T (Integer/valueOf (nth args 1))]
    (prn (my-ra F T))))

(defn round [x] (Math/round (double x)))
(def Fs (range 80000 120000 1000))
(def Ts (range 40 80 1))
(defn my-ra-quality [F T]
  (second (my-ra (round F) (round T))))
(def Qs (for [F Fs] (for [T Ts] (my-ra-quality F T))))
;(require '[incanter.charts :as c])
;(require '[incanter.core :as ic])
;(ic/view (c/heat-map my-ra-quality 80000 120000 40 80 :include-zero? false))