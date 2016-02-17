(ns acm-bar-graph.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

; data structure for a bar
(defn bar [mean std condition]
  {:mean mean
   :std std
   :condition condition})

(defn question
  [name
   [non-norm-mean non-norm-std]       ; non-adversarial normal
   [non-el-mean non-el-std]           ; non-adversarial elevated
   [ad-norm-mean ad-norm-std]        ; adversarial normal
   [ad-el-mean ad-el-std]]             ; adversarial elevated
  {:name name
   :non-adversarial [(bar non-norm-mean non-norm-std :normal)
                     (bar non-el-mean non-el-std :elevated)]
   :adversarial [(bar ad-norm-mean ad-norm-std :normal)
                 (bar ad-el-mean ad-el-std :elevated)]})

(def emotion-attribution-data
  [(question "calm" [5.43 1.034] [3.23 1.03] [4.88 0.971] [3.21 1.141])
   (question "upset" [2.93 1.086] [4 1.2] [3.36 0.952] [3.79 1.021])
   (question "anxious" [2.89 1.397] [4.81 0.981] [2.88 1.201] [5.04 0.859])
   (question "emotional" [3.39 1.315] [3.65 0.977] [3.6 0.957] [4.17 1.049])])

(def trust-attribution-data
  [(question "rely" [3.96 1.427] [3.62 1.203] [2.8 1] [3.5 1.063])
   (question "depend" [3.75 1.175] [3.69 1.123] [2.72 1.061] [3.83 0.963])
   (question "trustworthy" [4.04 0.962] [4.04 0.999] [3.08 1.077] [4.08 0.717])]) 

(defn color-from [condition]
  (if (= condition :elevated) 0 100))

(defn maprange [[a1 a2] [b1 b2] s]
	(+ b1 (/ (* (- s a1) (- b2 b1)) (- a2 a1))))

(defn scale [y]
  (maprange [0 5] [0 (q/height)] y))

(defn setup []
  (q/color-mode :hsb) ; Set color mode to HSB (HSV) instead of default RGB.
  (q/no-stroke)
  trust-attribution-data) ; setup function returns initial state.

(defn draw-error-bar [bar x w y]
  (let [width 2
        x (-> (/ w 2) (+ x) (- width)) 
        h (scale (:std bar))
        y (- y (/ h 2))]
    (q/fill 0 0 0)
    (q/rect x y width h)
  ))

(defn draw-bar [offset idx bar]
  (let [w 40
        x (+ offset (* idx w))
        y 0
        h (scale (:mean bar))
        c (color-from (:condition bar))]
    (q/fill c 255 255)
    (q/rect x y w h)
    (draw-error-bar bar x w h)
    ))

(defn draw-group [idx bars]
  (let [offset (* 100 idx)
        d (partial draw-bar offset)]
    (doall (map-indexed d bars))))

(defn draw-condition [idx cond]
  (draw-group idx (:adversarial cond))
  (draw-group (inc idx) (:non-adversarial cond)))

(defn draw [graphs]
  (doall (map-indexed draw-condition trust-attribution-data)))
;        (-> trust-attribution-data
;            first)]
;  (let )
;  (doall (map-indexed draw-group trust-attribution-data)
;    (q/background 255)
;;    (draw-group 1 (:adversarial groups))
;;    (draw-group 0 (:non-adversarial groups))
;  ))

(q/defsketch acm-bar-graph
  :size [1000 300]
  :setup setup
  :draw draw
  ;:features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
