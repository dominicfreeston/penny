(ns penny.vectormath)

;; Math

(def PI
  #?(:clj Math/PI
     :cljs js/Math.PI))

(defn sin [x]
  #?(:clj  (Math/sin x)
     :cljs (js/Math.sin x)))

(defn cos [x]
  #?(:clj  (Math/cos x)
     :cljs (js/Math.cos x)))

(defn sqrt [x]
  #?(:clj  (Math/sqrt x)
     :cljs (js/Math.sqrt x)))

(defn sq [x]
  (* x x))

;; Vectors

(defn add [v1 v2]
  (mapv + v1 v2))

(defn sub [v1 v2]
  (mapv - v1 v2))

(defn mult [v m]
  (mapv #(* % m) v))

(defn div [v m]
  (mapv #(/ % m) v))

(defn dist [src dest]
  (->> (sub dest src)
       (map sq)
       (reduce +)
       sqrt))

(defn mag [v]
  (dist (repeat 0) v))

(defn norm [v]
  (let [m (mag v)]
    (if (zero? m) v (div v m))))

(defn lim [v max]
  (if (> (mag v) max)
    (mult (norm v) max)
    v))

(defn dir [src dest]
  (norm (sub dest src)))

(defn rotate [[x y] angle]
  [(- (* x (cos angle)) (* y (sin angle)))
   (+ (* x (sin angle)) (* y (cos angle)))])
