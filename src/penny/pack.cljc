(ns penny.pack
  (:require [penny.vectormath :as v]))

(defn- overlap? [state c1 c2]
  (> (+ (:rad c1) (:rad c2) (:min-gap state))
     (v/dist (:pos c1) (:pos c2))))

(defn- can-fit? [state cand]
  (and
   (or (not (:constrain state))
       (let [[cx cy cw ch] (:container state)
             [x y] (:pos cand)
             r (:rad cand)]
         (and (< cx (- x r))
              (< cy (- y r))
              (> (+ cx cw) (+ x r))
              (> (+ cy ch) (+ y r)))))
   (not-any? (partial overlap? state cand) (:circles state))))

(defn- find-cand [state]
  (let [rand-fn (fn [start size] (+ start (* size ((:rand-fn state)))))
        [x y w h] (:container state)]
    (loop [attempts 0]
      (if (< attempts (:max-tries state))
        (let [cand {:pos [(rand-fn x w) (rand-fn y h)]
                    :rad (:min-size state)}]
          (if (can-fit? state cand)
            cand
            (recur (inc attempts))))
        nil))))

(defn- grow-cand [state cand]
  (loop [cand cand]
    (let [new-cand (update cand :rad + (:growth-step state))]
      (if (and (>= (:max-size state) (:rad new-cand))
               (can-fit? state new-cand))
        (recur new-cand)
        cand))))

(defn- add-circle [state]
  (if-let [cand (find-cand state)]
    (update state :circles conj (grow-cand state cand))
    state))

(defn circles
  "Returns a list of circles (of form {:pos [x y] :rad r}) packed according
  to a parameter map passed in with the following values:
  {:container - a vector containing [x y w h]
   :min-size - minimum circle size
   :max-size - maximum circle-size
   :min-gap - minimum gap between circles, defaults to 0
   :growth-step - min step between circle sizes, defaults to 1
   :constrain - a bool whether to ensure the whole circle fits inside the container, defaults to true
   :max-tries - number of times before it stops trying to add new circles, defaults to 1000
   :rand-fn - a function that returns a random number between 0 and 1
   }"
  [params]
  (:circles
   (loop [state (into {:min-gap 0
                       :growth-step 1
                       :constrain true
                       :max-tries 1000}
                      params)]
     (if-let [cand (find-cand state)]
       (recur (update state :circles conj (grow-cand state cand)))
       state))))
