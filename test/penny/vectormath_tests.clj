(ns penny.vectormath-tests
  (:require [clojure.test :refer :all]
            [penny.vectormath :as v]))

(deftest add
  (are [v1 v2 r] (= (v/add v1 v2) r)
    [0 0]
    [2 3]
    [2 3]

    [10 5]
    [10 10]
    [20 15]

    [0 1 2]
    [3 4 5]
    [3 5 7]))

(deftest sub
  (are [v1 v2 r] (= (v/sub v1 v2) r)
    [0 0]
    [2 3]
    [-2 -3]

    [10 5]
    [10 10]
    [0 -5]

    [0 1 2]
    [3 4 5]
    [-3 -3 -3]))


(deftest dist
  (are [v1 v2 r] (= (v/dist v1 v2) r)
    [0 0]
    [0 10]
    10.0

    [0 0]
    [3 4]
    5.0

    [15 15]
    [40 90]
    (Math/sqrt 6250)

    [1 2 3]
    [4 5 6]
    (Math/sqrt 27)))

(deftest mag
  (are [v r] (= (v/mag v) r)
    [0 10]
    10.0

    [3 4]
    5.0

    [3 3 3]
    (Math/sqrt 27)))

