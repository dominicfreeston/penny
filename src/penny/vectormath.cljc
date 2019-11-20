(ns penny.vectormath
  (:require [quil.core :as q]))

;; Vectors

(defn add [v1 v2]
  (mapv + v1 v2))

(defn sub [v1 v2]
  (mapv - v1 v2))

(defn mult [v m]
  (mapv #(* % m) v))

(defn div [v m]
  (mapv #(/ % m) v))

(defn mag [v]
  (apply q/mag v))

(defn norm [v]
  (let [m (mag v)]
    (if (zero? m) v (div v m))))

(defn lim [v max]
  (if (> (mag v) max)
    (mult (norm v) max)
    v))

(defn dir [src dest]
  (norm (sub dest src)))

(defn dist [src dest]
  (mag (sub dest src)))

(defn rotate [[x y] angle]
  [(- (* x (q/cos angle)) (* y (q/sin angle)))
   (+ (* x (q/sin angle)) (* y (q/cos angle)))])
