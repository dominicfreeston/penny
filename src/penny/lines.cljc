(ns penny.lines
  (:require [penny.vectormath :as v]))

;; Helpers

(defn- det
  [a b c d]
  (- (* a d) (* b c)))

(defn- dot
  "Returns the dot product of the vectors of two line segments"
  [[p1 p2] [p3 p4]]
  (let [v1 (v/sub p2 p1)
        v2 (v/sub p4 p3)]
    (reduce + (map * v1 v2))))

(defn- contains-point?
  "Assumes point is on line, checks if line segment contains point"
  [segment point]
  ;; https://www.lucidar.me/en/mathematics/check-if-a-point-belongs-on-a-line-segment/
  (let [a (dot segment [(first segment) point])
        b (dot segment segment)]
    (not (or (< a 0) (> a b)))))

(defn- unit
  "Unit vector of a line/segment"
  [[p1 p2]]
  (v/norm (v/sub p2 p1)))

;; Splitting

(defn points-along-line
  "Returns an infinite sequence of all the points on a line"
  [start vec gap]
  (let [increment (v/mult (v/norm vec) gap)]
    (iterate #(v/add increment %) start)))

(defn points-in-segment
  "Return points in a segment that are gap apart; resulting segment may be shorter."
  [segment gap]
  (take-while (partial contains-point? segment)
              (points-along-line (first segment) (unit segment) gap)))

;; Overlapping

(defn crop-segments
  "Returns a segment that is the overlapping part of both segments,
  assuming both segments are on the same line"
  [segment1 segment2]
  (distinct (concat (filter (partial contains-point? segment1) segment2)
                    (filter (partial contains-point? segment2) segment1))))

;; Crossing

(defn cross-point-ll
  ;; http://mathworld.wolfram.com/Line-LineIntersection.html
  "Calculates the cross points of two (infinite) lines (if it exists)"
  [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (let [denom (det (- x1 x2) (- y1 y2)
                   (- x3 x4) (- y3 y4))]
    (if (zero? denom)
      nil
      [(/ (det (det x1 y1
                    x2 y2) (- x1 x2)
               (det x3 y3
                    x4 y4) (- x3 x4))
          denom)
       (/ (det (det x1 y1
                    x2 y2) (- y1 y2)
               (det x3 y3
                    x4 y4) (- y3 y4))
          denom)])))

(defn cross-point-ls
  "Calculates whether and where an infinite line crosses a segment"
  [line segment]
  (when-let [point (cross-point-ll line segment)]
    (when (contains-point? segment point) point)))

(defn cross-point-ss
  "Calculates whether and where two line segments cross"
  [segment1 segment2]
  (when-let [point (cross-point-ll segment1 segment2)]
    (when (and (contains-point? segment1 point)
               (contains-point? segment2 point))
      point)))



