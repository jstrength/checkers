(ns checkers.utils
  (:import (java.util Date)))

(def SCREEN_SIZE 500)
(def MARGIN 50)
(def BOARD_WIDTH (- SCREEN_SIZE (* 2 MARGIN)))
(def SQUARE_WIDTH (/ BOARD_WIDTH 8))
(def PIECE_WIDTH (* 0.75 SQUARE_WIDTH))

(def EMPTY 0)
(def BLACK 1)
(def RED 2)
(def BLACK_KING 3)
(def RED_KING 4)

(def TURN_TIMER_LIMIT 10)

(def position-coordinates
  (vec (for [y (range 8)
             x (range 8)]
         {:raw-x x :raw-y y
          :x (+ (* x SQUARE_WIDTH) MARGIN)
          :y (+ (* y SQUARE_WIDTH) MARGIN)})))

(defn get-nth [board n]
  (when (>= 63 n 0)
    (nth board n)))

(defn get-turn-timer [state]
  (- TURN_TIMER_LIMIT (int (/ (- (.getTime (Date.)) (:turn-timer state)) 1000))))

