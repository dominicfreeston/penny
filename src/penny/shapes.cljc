(ns penny.shapes
  (:require [penny.vectormath :as v]
            [penny.lines :as l]))

;; Shapes are simply defined as a vector of points
;; Where points are vectors of the form [x y]
;; With the assumptions that they are joined by straight lines through the sequence
;; And will be closed by joining the last point with the first

(def ^:private x first)
(def ^:private y second)

(defn shape-to-segments
  "Converts a series of points into a series of segments representing the closed shape"
  [shape]
  (partition 2 1 [(first shape)] shape))

(defn min-x [shape]
  (apply min (map x shape)))

(defn max-x [shape]
  (apply max (map x shape)))

(defn min-y [shape]
  (apply min (map y shape)))

(defn max-y [shape]
  (apply max (map y shape)))

(defn box
  "Returns the bounding box of the shape"
  ;; Once upon a time this was enlarged by 1px
  ;; but that seemed wrong an assumption to make everywhere
  ;; However there may be some edge-cases no longer handled
  [shape]
  (let [x1 (min-x shape)
        y1 (min-y shape)
        x2 (max-x shape)
        y2 (max-y shape)]
    [[x1 y1] [x2 y1] [x2 y2] [x1 y2]]))

(defn- segment-goes-through-shape? [shape segment]
  (boolean (some #(l/cross-point-ss segment %) (shape-to-segments shape))))

(defn- line-cross-points-through-shape
  "Returns a list of all the points where the line intersects with shape"
  [shape line]
  (->> (shape-to-segments shape)
       (map (partial l/cross-point-ls line))
       (remove nil?)
       distinct))

(defn- point-in-box?
  "Returns whether the point is inside the bounding box of the shape."
  [box p]
  (not (or (< (x p) (min-x box))
           (> (x p) (max-x box))
           (< (y p) (min-y box))
           (> (y p) (max-y box)))))

(defn point-in-shape?
  "Returns whether a point is inside the shape."
  ;; It does so by projecting segments in all four cardinal directions
  ;; within the bounding box and then checking they all intersect with the shape
  ;; For optimisation, it first checks whether the point is inside the bounding box
  ;; NB: Don't think this is quite valid for all shapes, might need to check it hits
  ;; the shape an odd number of times before hitting the box or something...
  [shape p]
  (if (point-in-box? shape p)
    (or (some #(l/point-on-segment? % p) (shape-to-segments shape))
        (let [cross-lines (map #(vector p (v/add p %))
                               [[0 1] [1 0]])
              cross-points (mapcat #(line-cross-points-through-shape (box shape) %)
                                   cross-lines)
              cross-segments (map #(vector p %)
                                  cross-points)]
          (every? (partial segment-goes-through-shape? shape) cross-segments)))
    false))

;; Cropping Line/Segment to fit inside a shape

(defn- segment-in-shape?
   "Returns whether a candidate segment is inside the shape."
  ;; It does so by taking the midpoint and checking that point is inside the shape
  ;; This assumes that the segment is built of cross-point pairs, hence private
  [shape [p1 p2]]
  (let [mid (v/mult (v/add p1 p2) 0.5)]
    (point-in-shape? shape mid)))

(defn segments-in-shape
  "Return a list of all the line segments inside shape"
  ;; It finds all the cross points, pairs them up, then checks
  ;; they are actually inside (which is necessary to suport convex polygons)
  [shape line]
  (->> (line-cross-points-through-shape shape line)
       (sort-by (partial v/dist (first line)))
       (partition 2 1)
       (filter (partial segment-in-shape? shape))))

(defn crop-segment-to-shape
  "Returns a list of segments that fit within the shape and
  overlap with the original segment. If the segment fits within
  the shape, the list contains only the original segment."
  [shape segment]
  (let [limits (segments-in-shape shape segment)]
    (->> limits
         (map (partial l/crop-segments segment))
         (filter #(= 2 (count %))))))

(defn crop-segments-to-shape
  "Return the list of segments cropped to fit within the shape.
  The returned list may contain more or fewer segments than the original."
  [shape segments]
  (mapcat (partial crop-segment-to-shape shape) segments))
