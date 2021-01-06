(ns penny.middleware
  (:require [quil.core :as q]
            [penny.utils :as u]))

(def ^:private  mpressed (atom false))

(defmacro ^:private do-on-click [action]
  `(if (q/mouse-pressed?)
     (when (not @mpressed)
       (reset! mpressed true)
       ~action)
     (reset! mpressed false)))

(defmacro export [draw]
  `(let [time# (u/timestamp)
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
                   (q/stroke 0)
                   (q/no-fill)
                   (apply draw args)
                   (export-on-click (apply draw args))))))

(defn- update-pause-meta [meta]
  (if (q/key-pressed?)
    (let [key (q/key-as-keyword)]
      (if (and (not (:key-pressed meta)) (= :space key))
        (assoc meta
               :key-pressed true
               :is-paused? (not (:is-paused? meta)))
        (assoc meta
               :rewind (= :left key)
               :forward (= :right key))))
    (assoc meta
           :key-pressed false
           :rewind false
           :forward false)))

(defn- draw-time-travel-UI [state]
  (when (get-in state [:meta :is-paused?])
    (let [hist (count (:history state))
          total (+ hist (count (:future state)))]
      (q/fill 190 30 30 100)
      (q/stroke nil)
      (q/rect 0 0 (* (/ hist total) (q/width)) 12)
      (q/fill 0)
      (q/text (str hist "/" total) 0 10))))

(defn- time-travel-update [options]
  (fn [state]
    (let [new-meta (update-pause-meta (:meta state))
          history (:history state)
          future (:future state)]
      (cond
        (:rewind new-meta)
        (if (= (count history) 1)
          (assoc state
                 :meta new-meta)
          (assoc state
                 :meta new-meta
                 :history (pop history)
                 :future (conj future (peek history))))
        (:forward new-meta)
        (assoc state
             :meta new-meta
             :history (if-let [s (peek future)]
                        (conj history s)
                        history)
             :future (if (empty? future)
                       future
                       (pop future)))
        (:is-paused? new-meta)
        (assoc state
             :meta new-meta)
        :else
        (assoc state
             :meta new-meta
             :history (conj history
                            ((:update options) (peek history)))
             :future [])))))

(defn time-travel [options]
  (assoc options
         :setup (fn []
                  {:meta {:key key}
                   :history [((:setup options))]
                   :future []})
         :draw (fn [state]
                 (q/background 255)
                 (q/stroke 0)
                 (q/no-fill)
                 (q/push-matrix)
                 ((:draw options) (last (:history state)))
                 (q/pop-matrix)
                 (draw-time-travel-UI state))
         :update (time-travel-update options)))
