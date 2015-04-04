(ns ra.task-list
  (:require [ra.core :as c]))

(defn tl-id       [task] (get task 0))
(defn tl-name     [task] (get task 1))
(defn tl-time     [task] (get task 2))
(defn tl-finances [task] (get task 3))
(defn tl-quality  [task] (get task 4))
(defn tl-deps     [task] (get task 5))

(defn make-graph [task-list]
  (apply concat (for [task task-list]
                  (let [id (tl-id task)
                        deps (tl-deps task)]
                    (for [dep deps]
                      [dep id])))))

(defn keys*
  "Like keys, but works on vectors too"
  [x]
  (cond (map? x) (keys x)
        (vector? x) (map (partial + 1) (range (count x)))))

(defn find-contractors [task-list]
  (let [time-keys (comp keys* tl-time)
        finances-keys (comp keys* tl-finances)
        quality-keys (comp keys* tl-quality)]
    (into [] (set (concat (apply concat (map time-keys task-list))
                          (apply concat (map finances-keys task-list))
                          (apply concat (map quality-keys task-list)))))))

(defmacro forv [seq body]
  `(into [] (for ~seq ~body)))

(defn to-matrix [task-list selector not-found]
  (forv [task task-list]
    (forv [c (find-contractors task-list)]
      (get (selector task) c not-found))))

(defn matrix-map [f mat]
  (mapv #(mapv f %) mat))

(defn vec-of-vecs? [v]
  (and (vector? v)
       (every? vector? v)))

(defn matrix? [v]
  (and (vec-of-vecs? v)
       (every? #(= (count (first v)) (count %)) v)))

(defn ra [w task-list F T]
  (assert (matrix? task-list))
  (assert (= (count (first task-list)) 6))
  (let [graph (make-graph task-list)
        t (to-matrix task-list tl-time c/oo)
        f (to-matrix task-list tl-finances c/oo)
        q (matrix-map #(int (* 10 %)) (to-matrix task-list tl-quality 0))]
    (c/ra w t f q F T graph)))