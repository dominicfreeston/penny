(ns penny.utils
  (:require [quil.core :as q]))

(defn ^:private zp
  "Zero-pad a string representing number n with minimum digits c"
  [n c]
  (loop [s (str n)]
    (if (< (.length s) c)
      (recur (str "0" s))
      s)))

(defn timestamp
  "Returns a string representing the current date/time to the nearest second - definitely not ISO anything, but useful as a sketch name and can also be used to seed the random generators using penny.random-seed/set-timestamp-as-random-seed!"
  []
  (str (q/year)
       "-"
       (zp (q/month) 2)
       "-"
       (zp (q/day) 2)
       "-"
       (zp (q/hour) 2)
       (zp (q/minute) 2)
       "-"
       (zp (q/seconds) 2)))
