(ns ra.core-test
  (:require [clojure.test :refer :all]
            [ra.core :refer :all]))

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

(deftest ra-test
  (let [sol (my-ra 100000 60)]
    (is (= (dissoc sol :x) {:quality 968
                            :finances 100000
                            :time 44}))))

(defn round [x] (Math/round (double x)))
(def Fs (range 80000 120000 1000))
(def Ts (range 30 90 1))
(defn my-ra-quality [F T]
  (:quality (my-ra (round F) (round T))))
(def Qs (for [F Fs] (for [T Ts] (my-ra-quality F T))))
#_(deftest ra-multiple-test
  (is (= Qs
         '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 728 788 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808) (0 0 0 0 0 728 788 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808) (0 0 0 0 0 728 788 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808) (0 0 0 0 0 728 788 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808) (0 0 0 0 0 728 788 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808 808) (0 0 0 518 578 733 793 813 848 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868) (0 0 0 518 578 733 793 813 848 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868) (0 0 0 518 578 733 793 813 848 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868) (0 0 0 518 578 733 793 813 848 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868) (0 0 0 518 578 733 793 813 848 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868 868) (0 0 0 523 583 733 793 813 853 873 873 873 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888) (0 0 0 523 583 733 793 813 853 873 873 873 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888) (0 0 0 523 583 733 793 813 853 873 873 873 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888) (0 0 0 523 583 733 793 813 853 873 873 873 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888) (0 0 0 523 583 733 793 813 853 873 873 873 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888 888) (0 0 0 523 583 733 793 813 853 873 873 873 893 948 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968) (0 0 0 523 583 733 793 813 853 873 873 873 893 948 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968) (0 0 0 523 583 733 793 813 853 873 873 873 893 948 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968) (0 0 0 523 583 733 793 813 853 873 873 873 893 948 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968) (0 0 0 523 583 733 793 813 853 873 873 873 893 948 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968 968) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973 973) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977) (0 0 0 523 583 733 793 813 853 873 873 873 893 953 973 973 973 973 973 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977 977)))))

;(require '[incanter.charts :as c])
;(require '[incanter.core :as ic])
;(ic/view (c/heat-map my-ra-quality 80000 120000 40 80 :include-zero? false))
