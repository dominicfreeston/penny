(ns penny.hatchfill
  (:require [penny.vectormath :as v]
            [penny.lines :as l]))

(def ^:private x first)
(def ^:private y second)

(defn- shape-to-lines
  "converts a series of points into a series of lines representing a shape"
  [points]
  (partition 2 1 [(first points)] points))

(defn- top-left [shape]
  [(apply min (map x shape))
   (apply min (map y shape))])

(defn- bot-right [shape]
  [(apply max (map x shape))
   (apply max (map y shape))])

(defn- box
  "Returns the bounding box of the shape (slightly enlarged)"
  [shape]
  (let [tl (v/add [-1 -1] (top-left shape))
        br (v/add [1 1] (bot-right shape))]
    [tl [(x tl) (y br)] br [(x br) (y tl)]]))

(defn- segment-goes-through-shape? [shape segment]
  (boolean (some #(l/cross-point-ss segment %) (shape-to-lines shape))))

(defn- line-goes-through-shape? [shape line]
  (boolean (some #(l/cross-point-ls line %) (shape-to-lines shape))))

(defn- parallel-lines
  "Find all lines that go through shape, at distance gap apart, in direction vec"
  [shape vec gap offset]
  (let [unit (v/norm vec)
        perp1 [(- (y unit)) (x unit)]
        perp2 [(y unit) (- (x unit))]
        box (box shape)
        origin (v/add (first box) offset)
        test-fn (partial line-goes-through-shape? box)
        lines-fn (fn [dir]
                   (take-while test-fn
                               (rest (map #(vector % (v/add % unit))
                                          (l/points-along-line origin dir gap)))))]
    (concat (reverse (lines-fn perp1))
            (filter test-fn [[origin (v/add origin unit)]])
            (lines-fn perp2))))

(defn- line-cross-points-through-shape
  "Returns an ordered list of all the points where the line intersects with shape"
  [shape line]
  (->> (shape-to-lines shape)
       (map (partial l/cross-point-ls line))
       (remove nil?)
       (sort-by (partial v/dist (first line)))
       dedupe))

(defn- segment-in-shape?
  "Returns whether a candidate segment is inside the shape."
  ;; It does so by taking the midpoint then projecting segments
  ;; in all four cardinal directions within the bounding box
  ;; and then checking they all intersect with the shape
  [shape [p1 p2]]
  (let [box (box shape)
        mid (v/mult (v/add p1 p2) 0.5)
        cross-lines (map #(vector mid (v/add mid %)) [[0 1] [1 0]])
        cross-points (mapcat #(line-cross-points-through-shape box %) cross-lines)
        cross-segments (map #(vector mid %) cross-points)]
    (every? (partial segment-goes-through-shape? shape) cross-segments)))

(defn- segments-in-shape
  "Return a list of all the line segments inside shape"
  ;; It finds all the cross points, pairs them up, then checks
  ;; they are actually inside (which is necessary to suport convex polygons)
  [shape line]
  (->> (line-cross-points-through-shape shape line)
       (partition 2 1)
       (filter (partial segment-in-shape? shape))))

(defn- crop-segment-to-shape [shape segment]
  (let [limits (segments-in-shape shape segment)]
    (->> limits
         (map (partial l/crop-segments segment))
         (filter #(= 2 (count %))))))

;; They're called lines in the public API but they're really segments...
(defn crop-lines-to-shape
  "Return a list of lines cropped to fit within the shape"
  [shape lines]
  (mapcat (partial crop-segment-to-shape shape) lines))

;; Fill lines

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
