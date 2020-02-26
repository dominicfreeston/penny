(ns penny.hatchfill
  (:require [penny.vectormath :as v]
            [penny.lines :as l]
            [penny.shapes :as s]))

(def ^:private x first)
(def ^:private y second)

(defn- line-goes-through-shape? [shape line]
  (boolean (some #(l/cross-point-ls line %) (s/shape-to-segments shape))))

(defn- parallel-lines
  "Find all lines that go through shape, at distance gap apart, in direction vec"
  [shape vec gap offset]
  (let [unit (v/norm vec)
        perp1 [(- (y unit)) (x unit)]
        perp2 [(y unit) (- (x unit))]
        box (s/box shape)
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
  (->> (s/shape-to-segments shape)
       (map (partial l/cross-point-ls line))
       (remove nil?)
       (sort-by (partial v/dist (first line)))
       dedupe))

;; Fill lines

(defn fill-lines
  "Returns a list of fill-lines defined by vec going through shape, gap apart"
  ([shape vec gap]
   (fill-lines shape vec gap [0 0]))
  ([shape vec gap offset]
   (mapcat (partial s/segments-in-shape shape)
           (parallel-lines shape vec gap offset))))

(defn h-lines
  "Returns a list of horizontal fill-lines gap apart"
  [shape gap]
  (fill-lines shape [1 0] gap))

(defn v-lines
  "Returns a list of vertical fill-lines gap apart"
  [shape gap]
  (fill-lines shape [0 1] gap))
