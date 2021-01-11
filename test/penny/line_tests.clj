(ns penny.line-tests
  (:require  [clojure.test :refer :all]
             [penny.lines :as l]))

(deftest cross-point-ll
  (are [cross line1 line2]
        (= cross (l/cross-point-ll line1 line2))
      [0 0]
      [[0 0] [0 2]]
      [[1 0] [-1 0]]

      [0 0]
      [[1 1] [3 3]]
      [[0 0] [5 14]]

      [10 10]
      [[0 0] [20 20]]
      [[10 0] [10 -1]]

      nil
      [[0 0] [0 1]]
      [[1 0] [1 1]]
      ))

(deftest cross-point-ls
  (are [cross line segment]
      (= cross (l/cross-point-ls line segment))
    [0 0]
    [[0 0] [0 2]]
    [[1 0] [-1 0]]
    
    [0 0]
    [[1 1] [3 3]]
    [[0 0] [5 14]]
    
    nil
    [[0 0] [20 20]]
    [[10 0] [10 -1]]

    nil
    [[0 0] [0 1]]
    [[1 0] [1 1]]
    ))

(deftest cross-point-ss
  (are [cross segment1 segment2]
      (= cross (l/cross-point-ss segment1 segment2))
    [0 0]
    [[0 0] [0 2]]
    [[1 0] [-1 0]]
    
    nil
    [[1 1] [3 3]]
    [[0 0] [5 14]]
    
    nil
    [[0 0] [20 20]]
    [[10 0] [10 -1]]

    nil
    [[0 0] [0 1]]
    [[1 0] [1 1]]
    ))


(deftest point-to-line
  (are [point line result]
      (= result (l/point-to-line line point))
    [10 0]
    [[0 0] [0 10]]
    [[10 0] [0 0]]

    [10 -1]
    [[0 0] [0 10]]
    [[10 -1] [0 -1]]))

(deftest point-to-segment
  (are [point line result]
      (= result (l/point-to-segment line point))
    [10 0]
    [[0 0] [0 10]]
    [[10 0] [0 0]]

    [10 -1]
    [[0 0] [0 10]]
    [[10 -1] [0 0]]
    ))
