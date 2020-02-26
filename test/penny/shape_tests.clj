(ns penny.shape-tests
  (:require  [clojure.test :refer :all]
             [penny.shapes :as s]))

(def concave-polygon
  ;; |--
  ;; | /
  ;; |/
  ;; |\
  ;; | \
  ;; |-- (like this but square)
  [[0 0] [10 0] [0 5] [10 10] [0 10]])

(deftest point-in-shape?
  (testing "square box"
    (let [shape [[0 0] [0 10] [10 10] [10 0]]]
      (are [point] (s/point-in-shape? shape point)
        [0 0]
        [0 10]
        [10 10]
        [10 0]
        [5 5])
      (are [point] (not (s/point-in-shape? shape point))
        [-1 5]
        [5 -1]
        [5 11]
        [11 5])))
  (testing "triangle"
    (let [shape [[0 0] [10 10] [10 0]]]
      (are [point] (s/point-in-shape? shape point)
        [0 0]
        [10 10]
        [10 0]
        [5 5]
        [5 3])
      (are [point] (not (s/point-in-shape? shape point))
        [0 10]
        [3 5]
        [5 6])))
  (testing "concave polygon"
    (let [shape concave-polygon]
      (are [point] (s/point-in-shape? shape point)
        [0 0]
        [10 10]
        [10 0]
        [0 10])
      (are [point] (not (s/point-in-shape? shape point))
        [5 5]
        [10 5]))))

(deftest segments-in-shape
  (testing "concave polygon"
    (let [shape concave-polygon]
      (are [line] (= [[[0 0] [10/3 10/3]]]
                     (s/segments-in-shape shape line))
        [[0 0] [10 10]]
        [[0 0] [1 1]]
        [[-10 -10] [-5 -5]])
      (are [line] (= [[[5 0] [5 5/2]] [[5 15/2] [5 10]]]
                     (s/segments-in-shape shape line))
        [[5 0] [5 10]]
        [[5 0] [5 1]]
        [[5 -10] [5 -20]]))))

(deftest crop-segment-to-shape
  (testing "concave polygon"
    (let [shape concave-polygon]
      (are [segment] (= [[[0 0] [10/3 10/3]]]
                        (s/crop-segment-to-shape shape segment))
        [[0 0] [10 10]]
        [[-10 -10] [20 20]])
      (are [segment] (= [[[5 0] [5 5/2]] [[5 15/2] [5 10]]]
                        (s/crop-segment-to-shape shape segment))
        [[5 0] [5 10]]
        [[5 -10] [5 20]])
      (are [segment] (= [segment]
                        (s/crop-segment-to-shape shape segment))
        [[0 0] [1 1]]
        [[1 1] [2 2]]
        [[0 8] [1 6]]))))
