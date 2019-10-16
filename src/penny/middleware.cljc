(ns penny.middleware
  (:require [quil.core :as q]))

(defn ^:private zp
  "Zero-pad a string representing number n"
  [n c]
  (loop [s (str n)]
    (if (< (.length s) c)
      (recur (str "0" s))
      s)))

(defn ^:private current-time []
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

(def ^:private  mpressed (atom false))

(defmacro ^:private do-on-click [action]
  `(if (q/mouse-pressed?)
     (when (not @mpressed)
       (reset! mpressed true)
       ~action)
     (reset! mpressed false)))

(defmacro export [draw]
  `(let [time# (current-time)
         gr# (q/create-graphics (q/width) (q/height) :svg (str "output/svg/" time# ".svg"))]
     (q/save (str "output/png/" time# ".png"))
     (q/with-graphics gr#
       (q/no-fill)
       ~draw
       (.dispose gr#))))

(defmacro ^:private export-on-click [draw]
  `(do-on-click (export ~draw)))

(defn on-click-exporter [options]
  (let [draw (:draw options)]
    (assoc options
           :draw (fn [& args]
                   (q/background 255)
                   (q/no-fill)
                   (apply draw args)
                   (export-on-click (apply draw args))))))
