(ns checkers.draw
  (:require [quil.core :as q]
            [checkers.utils :refer :all]
            [checkers.logic :as logic]))

(def menu-text-size 40)

(declare draw-static-piece)

(defn game-text! [{:keys [turn] :as state}]
  (q/text-size 20)
  (q/fill 0)

  (q/text (str "Turn:" ) 10 20)
  (with-redefs [PIECE_WIDTH 25]
    (draw-static-piece (+ 25 (q/text-width "Turn:")) 15
                       (if (= turn :black)
                         (:dark-color state)
                         (:light-color state))))
  (q/fill 0)

  (if (= :in-progress (:game-state state))
    (when (:multiplayer state)
      (if (:waiting state)
        (q/text (str "Waiting for opponent to move") 150 30)
        (q/text (str "Your turn") 150 30)))
    (q/text (name (:game-state state)) (/ SCREEN_SIZE 2) 30))

  (q/text (str "Time elapsed: " (let [total-seconds (:total-seconds state)
                                      minutes (int (/ total-seconds 60))
                                      seconds (int (mod total-seconds 60))]
                                  (format "%02d:%02d" minutes seconds)))
          10 (- SCREEN_SIZE 25))

  (when-not (zero? (:timer state))
    (q/text (str "Turn timer: " (- TURN_TIMER_LIMIT (:last-turn-seconds state)))
            (/ SCREEN_SIZE 2) (- SCREEN_SIZE 25)))

  (q/text-size 10)
  (q/text "Hotkeys: p - pause game, q - quit to main menu" 10 (- SCREEN_SIZE 10))

  (q/text-size 30))

(defn ^:private  get-quil-color [color]
  (apply q/color (get dark-colors color (get light-colors color))))

(defn draw-square [x y color]
  (q/fill (get-quil-color color))
  (q/rect x y SQUARE_WIDTH SQUARE_WIDTH))

(defn draw-static-piece
  ([x y color]
   (q/fill (get-quil-color color))
   (q/ellipse x y PIECE_WIDTH PIECE_WIDTH))
  ([x y color king]
   (draw-static-piece x y color)
   (when king
     (q/fill (get-quil-color :white))
     (q/text "K" (+ x -10) (+ y 10)))))

(defn static-pieces! [{:keys [board selected] :as state}]
  (doall
    (map-indexed
      (fn [pos b]
        (when (and (not= b EMPTY) (not= pos (:square selected)))
          (let [{:keys [x y]} (get-nth position-coordinates pos)]
            (draw-static-piece
              (+ x (/ SQUARE_WIDTH 2))
              (+ y (/ SQUARE_WIDTH 2))
              (if (#{BLACK BLACK_KING} b) (:dark-color state) (:light-color state))
              (#{BLACK_KING RED_KING} b)))))
      board)))

(defn draw-board! [{:keys [dark-square-color light-square-color player] :as state}]
  (q/background 255)
  (q/stroke 0)

  (let [text-size 20]
    (q/text-size text-size)
    (doseq [n (range 8)]
      (q/text (str (get-numeric player n)) (- MARGIN 15) (+ (* n SQUARE_WIDTH) (+ (* 1.5 SQUARE_WIDTH) (/ text-size 4))))
      (q/text (get-alpha player n) (+ (* n SQUARE_WIDTH) (- (* SQUARE_WIDTH 1.5) (/ text-size 4))) (- MARGIN 5))))

  (doseq [n (range 9)]
    (q/line MARGIN (+ MARGIN (* SQUARE_WIDTH n)) (+ BOARD_WIDTH MARGIN) (+ MARGIN (* SQUARE_WIDTH n)))
    (q/line (+ MARGIN (* SQUARE_WIDTH n)) MARGIN (+ MARGIN (* SQUARE_WIDTH n)) (+ BOARD_WIDTH MARGIN)))

  (doseq [{:keys [x y raw-x raw-y]} position-coordinates]
    (if (if (odd? raw-y) (even? raw-x) (odd? raw-x))
      (do
        (q/fill (get-quil-color dark-square-color))
        (q/rect x y SQUARE_WIDTH SQUARE_WIDTH))
      (do
        (q/fill (get-quil-color light-square-color))
        (q/rect x y SQUARE_WIDTH SQUARE_WIDTH)))))

(defn draw-selected-peice! [{:keys [board selected] :as state}]
  (when-let [{:keys [x y piece square]} selected]
    ;; highlight valid moves
    (q/no-fill)
    (q/stroke-weight 5)
    (q/with-stroke
      [255 240 0]
      (doseq [s (or (logic/get-valid-jumps state square) (logic/get-valid-moves board square))]
        (let [{:keys [x y]} (position-coordinates s)]
          (q/rect x y SQUARE_WIDTH SQUARE_WIDTH))))
    (q/stroke-weight 1)
    (draw-static-piece x y (if (#{BLACK BLACK_KING} piece)
                             (:dark-color state)
                             (:light-color state))
                       (#{BLACK_KING RED_KING} piece))))


(defn draw-menu-item [x y text selected?]
  (q/fill (get-quil-color :black))
  (when selected?
    (q/fill (get-quil-color :red)))
  (q/text text x y))

(defn menu [{:keys [background-img current-menu] :as state}]
  (q/image background-img 0 0 SCREEN_SIZE SCREEN_SIZE) ;;todo rotate background or something cool
  (q/text-size menu-text-size)
  (q/fill (get-quil-color :white))
  (q/rect 50 40 375 410)
  (doall (map-indexed
           (fn [idx display-item]
             (let [draw-fn (get-in current-menu [:items display-item :draw])]
               (draw-fn state 75 (+ 100 (* (/ 400 (count (:items current-menu))) idx)))))
           (:display-order current-menu))))

(defn game [state]
  (doto state
    (draw-board!)
    (static-pieces!)
    (draw-selected-peice!)
    (game-text!)))
