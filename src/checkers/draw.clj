(ns checkers.draw
  (:require [quil.core :as q]
            [checkers.utils :refer :all]
            [checkers.logic :as logic])
  (:import (java.util Date)))

(defn game-text! [{:keys [turn] :as state}]
  (q/text-size 20)
  (q/fill 0)
  (q/text (str "Turn: " (name turn)) 10 30)
  (when (:player state)
    (if (:waiting state)
      (q/text (str "Waiting for oppoent to move") 150 30)
      (q/text (str "Your turn") 150 30)))
  (when (not= :in-progress (:game-state state))
    (q/text (name (:game-state state)) (/ SCREEN_SIZE 2) 30))
  (q/text (str "Time elapsed: " (let [total-seconds (/ (- (.getTime (Date.)) (:start-time state)) 1000.0)
                                      minutes (int (/ total-seconds 60))
                                      seconds (int (mod total-seconds 60))]
                                  (format "%02d:%02d" minutes seconds))) 10 (- SCREEN_SIZE 10))
  (q/text (str "Turn timer: " (get-turn-timer state)) (/ SCREEN_SIZE 2) (- SCREEN_SIZE 10))
  (q/text-size 30))

(defn static-pieces! [{:keys [board selected] :as state}]
  (doall
    (map-indexed
      (fn [pos b]
        (when (and (not= b EMPTY) (not= pos (:square selected)))
          (let [{:keys [x y]} (get-nth position-coordinates pos)]
            (if (#{BLACK BLACK_KING} b)
              (q/fill 0)
              (q/fill 255 0 0))
            (q/ellipse (+ x (/ SQUARE_WIDTH 2)) (+ y (/ SQUARE_WIDTH 2))
                       PIECE_WIDTH PIECE_WIDTH)
            (condp = b
              BLACK_KING (do (q/fill 255 0 0) (q/text "K" (+ x -10 (/ SQUARE_WIDTH 2)) (+ y 10 (/ SQUARE_WIDTH 2))) (q/fill 0))
              RED_KING (do (q/fill 0) (q/text "K" (+ x -10 (/ SQUARE_WIDTH 2)) (+ y 10 (/ SQUARE_WIDTH 2))) (q/fill 255 0 0))
              nil))))
      board)))

(defn draw-board! [state]
  (q/background 255)
  (q/stroke 0)

  (doseq [n (range 9)]
    (q/line MARGIN (+ MARGIN (* SQUARE_WIDTH n)) (+ BOARD_WIDTH MARGIN) (+ MARGIN (* SQUARE_WIDTH n)))
    (q/line (+ MARGIN (* SQUARE_WIDTH n)) MARGIN (+ MARGIN (* SQUARE_WIDTH n)) (+ BOARD_WIDTH MARGIN)))
  (q/fill 180 120 70)
  (doseq [{:keys [x y raw-x raw-y]} position-coordinates]
    (when (if (odd? raw-y) (even? raw-x) (odd? raw-x))
      (q/rect x y SQUARE_WIDTH SQUARE_WIDTH))))

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
    ;; draw selected piece
    (if (#{BLACK BLACK_KING} piece)
      (q/fill 0)
      (q/fill 255 0 0))
    (q/ellipse x y PIECE_WIDTH PIECE_WIDTH)

    (condp = piece
      BLACK_KING (do (q/fill 255 0 0) (q/text "K" (+ x -10) (+ y 10)) (q/fill 0))
      RED_KING (do (q/fill 0) (q/text "K" (+ x -10) (+ y 10)) (q/fill 255 0 0))
      nil)))