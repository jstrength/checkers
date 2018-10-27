(ns checkers.utils)

;todo don't upper case _ constants, just regular cabab case
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

(def TURN_TIMER_LIMIT 5)

(defn get-next-menu [current-state menu-states]
  (nth menu-states (mod (inc (.indexOf menu-states current-state)) (count menu-states))))

(defn get-previous-menu [current-state menu-states]
  (nth menu-states (mod (dec (.indexOf menu-states current-state)) (count menu-states))))

(def main-menu-states
  [:start :multiplayer :settings :quit])
(def settings-menu-states
  [:sound :dark :light :board])

(def position-coordinates
  (vec (for [y (range 8)
             x (range 8)]
         {:raw-x x :raw-y y
          :x (+ (* x SQUARE_WIDTH) MARGIN)
          :y (+ (* y SQUARE_WIDTH) MARGIN)})))

(defn get-nth [board n]
  (when (>= 63 n 0)
    (nth board n)))
