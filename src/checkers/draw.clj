(ns checkers.draw
  (:require [quil.core :as q]
            [checkers.utils :refer :all]
            [checkers.logic :as logic]))

(def dark-colors {:black [0]
                  :olive [128 128 0]
                  :navy [0 0 128]
                  :maroon [128 0 0]})

(def light-colors {:red [255 0 0]
                   :yellow [255 255 0]
                   :lime [0 255 0]
                   :magenta [255 0 255]})

(defn game-text! [{:keys [turn] :as state}]
  (q/text-size 20)
  (q/fill 0)
  (q/text (str "Turn: " (name turn)) 10 30)
  (when (:player state)
    (if (:waiting state)
      (q/text (str "Waiting for opponent to move") 150 30)
      (q/text (str "Your turn") 150 30)))
  (case (:game-state state)
    :paused (do (q/text-size 70)
                (q/fill 100 100 100)
                (q/text "Game Paused" 20 275)
                (q/fill 0)
                (q/text-size 20))
    (q/text (name (:game-state state)) (/ SCREEN_SIZE 2) 30))
  (q/text (str "Time elapsed: " (let [total-seconds (:total-seconds state)
                                      minutes (int (/ total-seconds 60))
                                      seconds (int (mod total-seconds 60))]
                                  (format "%02d:%02d" minutes seconds)))
          10 (- SCREEN_SIZE 25))
  (q/text (str "Turn timer: " (- TURN_TIMER_LIMIT (:last-turn-seconds state)))
          (/ SCREEN_SIZE 2) (- SCREEN_SIZE 25))
  (q/text-size 10)
  (q/text "Hotkeys: r - resume game, p - pause game, q - quit to main menu" 10 (- SCREEN_SIZE 10))
  (q/text-size 30))

(defn ^:private  get-quil-color [color]
  (apply q/color (get dark-colors color (get light-colors color))))

(defn ^:private draw-static-piece
  ([x y color]
   (q/fill (get-quil-color color))
   (q/ellipse x y PIECE_WIDTH PIECE_WIDTH))
  ([x y color king]
   (draw-static-piece x y color)
   (condp = king
     BLACK_KING (do (q/fill 255 255 255)
                    (q/text "K" (+ x -10) (+ y 10)) (q/fill 0))
     RED_KING (do (q/fill 0)
                  (q/text "K" (+ x -10) (+ y 10)))
     nil)))

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
              b))))
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
      (q/fill (apply q/color (get dark-colors (:dark-color state))))
      (q/fill (apply q/color (get light-colors (:light-color state)))))
    (q/ellipse x y PIECE_WIDTH PIECE_WIDTH)

    (condp = piece
      BLACK_KING (do (q/fill 255 0 0) (q/text "K" (+ x -10) (+ y 10)) (q/fill 0))
      RED_KING (do (q/fill 0) (q/text "K" (+ x -10) (+ y 10)) (q/fill 255 0 0))
      nil)))


(defn draw-menu-item [x y text selected?]
  (q/fill 0)
  (when selected?
    (q/fill 255 0 0))
  (q/text text x y)
  (q/text text (inc x) (inc y))
  (q/fill 0))

(defn menu [{:keys [menu-item-selected] :as state}]
  (q/background 255)
  (q/text-size 50)
  (draw-menu-item 100 100 "Start Game" (= :start menu-item-selected))
  (draw-menu-item 100 200 "Multiplayer" (= :multiplayer menu-item-selected))
  (draw-menu-item 100 300 "Settings" (= :settings menu-item-selected))
  (draw-menu-item 100 400 "Quit" (= :quit menu-item-selected)))

(defn settings-menu [{:keys [menu-item-selected] :as state}]
  ;piece colors, board style
  (q/background 255)
  (draw-menu-item 100 100 (format "Sound: %s" (if (:sound-on? state) "ON" "OFF")) (= :sound menu-item-selected))
  (draw-menu-item 100 200 "Dark Color:" (= :dark menu-item-selected))
  (draw-static-piece (+ 100 (q/text-width "Dark Color:  ")) 185 (get dark-colors (:dark-color state)))
  (draw-menu-item 100 300 "Light Color:" (= :light menu-item-selected))
  (draw-static-piece (+ 100 (q/text-width "Light Color:  ")) 285 (get light-colors (:light-color state)))
  (draw-menu-item 100 400 "Board Style" (= :board menu-item-selected))
  )

(defn multiplayer-menu [{:keys [menu-item-selected] :as state}]
  ;todo another menu
  (q/background 255)
  (draw-menu-item 100 100 "Host Game" (= :start menu-item-selected))
  (draw-menu-item 100 200 "Join Game" (= :start menu-item-selected))
  )
