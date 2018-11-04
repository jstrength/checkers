(ns checkers.utils)

;todo don't upper case _ constants, just regular cabab case
(def SCREEN_SIZE 500)
(def MARGIN 50)
(def BOARD_WIDTH (- SCREEN_SIZE (* 2 MARGIN)))
(def ^:dynamic SQUARE_WIDTH (/ BOARD_WIDTH 8))
(def PIECE_WIDTH (* 0.75 SQUARE_WIDTH))

(def EMPTY 0)
(def BLACK 1)
(def RED 2)
(def BLACK_KING 3)
(def RED_KING 4)

(def TURN_TIMER_LIMIT 5)

(def dark-colors {:black [0]
                  :olive [128 128 0]
                  :navy [0 0 128]
                  :brown [180 120 70]
                  :maroon [128 0 0]})

(def light-colors {:red [255 0 0]
                   :white [245 245 245]
                   :yellow [255 255 0]
                   :lime [0 255 0]
                   :magenta [255 0 255]})

(def up-key 38)
(def down-key 40)
(def right-key 39)
(def left-key 37)
(def backspace-key 8)
(def enter-key 10)

(def port 1337)

(def position-coordinates
  (vec (for [y (range 8)
             x (range 8)]
         {:raw-x x :raw-y y
          :x (+ (* x SQUARE_WIDTH) MARGIN)
          :y (+ (* y SQUARE_WIDTH) MARGIN)})))

(defn get-nth [board n]
  (when (>= 63 n 0)
    (nth board n)))

(defn get-alpha [player n]
  (str (nth (if (= :red player) "abcdefgh" "hgfedcba") n)))

(defn get-numeric [player n]
  (str (if (= :red player) (inc (- 7 n)) (inc n))))

(defn get-algebraic-notation [player square]
  (let [{x :raw-x y :raw-y} (position-coordinates square)]
    (str (get-alpha player x) (get-numeric player y))))

(def default-game-state
  {:board (vec (for [n (range 0 64)]
                 (let [{x :raw-x y :raw-y} (position-coordinates n)]
                   (if (if (odd? y) (even? x) (odd? x))
                     (cond (< n (* 3 8)) BLACK
                           (> n (- 63 (* 3 8))) RED
                           :else EMPTY)
                     EMPTY))))
   :start-time 0
   :last-turn-seconds 0
   :total-seconds 0
   :multiplayer false
   :turn :red
   :player :red
   ;:current-menu nil ;;added in setup
   ;:background-img nil ;;added in setup
   :game-state :menu})

(def default-settings-state
  {:dark-color :black
   :light-color :red
   :dark-square-color :brown
   :light-square-color :white
   :sound-on? true
   :timer 0
   :ip ""})
