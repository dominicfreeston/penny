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

(defn ^:private  mm-to-px [mm]
  (/ (* mm 90) 25.4))

(def ^:private a4-w (mm-to-px 297))
(def ^:private a4-h (mm-to-px 210))

(def ^:private  mpressed (atom false))

(defmacro ^:private do-on-click [action]
  `(if (q/mouse-pressed?)
     (when (not @mpressed)
       (reset! mpressed true)
       ~action)
     (reset! mpressed false)))

(defmacro ^:private export [draw]
  `(let [time# (current-time)
         gr# (q/create-graphics a4-w a4-h :svg (str "output/svg/" time# ".svg"))]
     (q/save-frame (str "output/png/" time# ".png"))
     (q/with-graphics gr#
       (q/with-translation [(/ (- a4-w (q/width)) 2) (/ (- a4-h (q/height)) 2)]
         ~draw)
       (.dispose gr#))))

(defmacro ^:private export-on-click [draw]
  `(do-on-click (export ~draw)))

(defn on-click-exporter [options]
  (let [draw (:draw options)]
    (assoc options
           :draw (fn [& args]
                   (apply draw args)
                   (export-on-click (apply draw args))))))
