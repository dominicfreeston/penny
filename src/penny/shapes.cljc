(ns penny.shapes
  (:require [quil.core :as q]
            [penny.vectormath :as v]
            [penny.lines :as l]))

;; Shapes are simply defined as a vector of points
;; Where points are vectors of the form [x y]
;; With the assumptions that they are joined by straight lines through the sequence
;; And will be closed by joining the last point with the first

(def ^:private x first)
(def ^:private y second)

;; Polygon builders
;;;;;;;;;;;;;;;;;;;

(defn- circle-point
  "Returns point at angle a with radius r around the origin"
  [a r]
  [(* r (q/sin a))
   (* r (q/cos a))])

(defn cyclic-polygon
  "Returns a cyclic polygon made of the points at angles in a circle of radius r around the origin"
  [angles r]
  (map #(circle-point % r) angles))

(defn regular-polygon
  "Returns a polygon with n equal sides within a circle of radius r around the origin with optional rotation rot"
  ([n r rot]
   (cyclic-polygon (range rot (+ rot q/TWO-PI) (/ q/TWO-PI n)) r))
  ([n r]
   (regular-polygon n r 0))
  ([n]
   (regular-polygon n 1)))


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
  ([shape]
   (box shape [0 0]))
  ([shape insets]
   (let [x1 (+ (min-x shape) (x insets))
         y1 (+ (min-y shape) (y insets))
         x2 (- (max-x shape) (x insets))
         y2 (- (max-y shape) (y insets))]
     [[x1 y1] [x2 y1] [x2 y2] [x1 y2]])))

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

(defn- winding-number
  ;; http://geomalgorithms.com/a03-_inclusion.html#wn_PnPoly()
  [shape p]
  (reduce 
   (fn [wn [v1 v2]]
     (if (<= (y v1) (y p))
       (if (and (> (y v2) (y p))
                (> (l/point-on-line-side [v1 v2] p) 0))
         (inc wn)
         wn)
       (if (and (<= (y v2) (y p))
                (< (l/point-on-line-side [v1 v2] p) 0))
         (dec wn)
         wn)))
   0
   (shape-to-segments shape)))

(defn point-in-shape?
  "Returns whether a point is inside the shape.
  Being on an edge is considered to be inside the shape."
  [shape p] (or (not= 0 (winding-number shape p))
                (some #(l/point-on-segment? % p) (shape-to-segments shape))))

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

(defn- segment-goes-through-box?
  "Returns whether the segment is inside or crosses the bounding box"
  [shape segment]
  (or (some #(point-in-box? shape %) segment)
      ;; Can probably speed this up since we don't care about actual cross point
      ;; https://stackoverflow.com/questions/4977491/determining-if-two-line-segments-intersect/4977569#4977569
      (some #(l/cross-point-ss segment %) (shape-to-segments (box shape)))))

(defn crop-segment-to-shape
  "Returns a list of segments that fit within the shape and
  overlap with the original segment. If the segment fits within
  the shape, the list contains only the original segment."
  [shape segment]
  (if (segment-goes-through-box? shape segment)
    (let [limits (segments-in-shape shape segment)]
      (->> limits
           (map (partial l/crop-segments segment))
           (filter #(= 2 (count %)))))
    []))

(defn crop-segments-to-shape
  "Return the list of segments cropped to fit within the shape.
  The returned list may contain more or fewer segments than the original."
  [shape segments]
  (mapcat (partial crop-segment-to-shape shape) segments))

;; Fill Lines
;;;;;;;;;;;;;

(defn- parallel-lines
  "Find all lines that go through shape, at distance gap apart, in direction vec"
  [shape vec gap offset]
  (let [unit (v/norm vec)
        perp1 [(- (y unit)) (x unit)]
        perp2 [(y unit) (- (x unit))]
        box (box shape)
        origin (v/add (first box) offset)
        test-fn (fn [line] ;; test if lines goes through the box
                  (some #(l/cross-point-ls line %)
                        (shape-to-segments box)))
        lines-fn (fn [dir]
                   (take-while test-fn
                               (rest (map #(vector % (v/add % unit))
                                          (l/points-along-line origin dir gap)))))]
    (concat (reverse (lines-fn perp1))
            (filter test-fn [[origin (v/add origin unit)]])
            (lines-fn perp2))))

(defn fill-lines
  "Returns a list of fill-lines defined by vec going through shape, gap apart"
  ([shape vec gap]
   (fill-lines shape vec gap [0 0]))
  ([shape vec gap offset]
   (mapcat (partial segments-in-shape shape)
           (parallel-lines shape vec gap offset))))

(defn h-lines
  "Returns a list of horizontal fill-lines gap apart"
  [shape gap]
  (fill-lines shape [1 0] gap))

(defn v-lines
  "Returns a list of vertical fill-lines gap apart"
  [shape gap]
  (fill-lines shape [0 1] gap))
