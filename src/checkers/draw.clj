(ns checkers.draw
  (:require [quil.core :as q]
            [checkers.utils :refer :all]
            [checkers.logic :as logic]
            [clojure.string :as string]))

(def menu-text-size 40)

(declare draw-static-piece)

(defn game-text! [{:keys [player turn] :as state}]
  (q/text-size 20)
  (q/fill 0)

  (q/text (str "Turn:" ) 10 20)
  (with-redefs [PIECE_WIDTH 25]
    (draw-static-piece (+ 25 (q/text-width "Turn:")) 15
                       (if (= turn :black)
                         (:dark-color state)
                         (:light-color state))))
  (q/fill 0)

  (case (:game-state state)
    :in-progress
    (when (:multiplayer state)
      (if (= player turn)
        (q/text (str "Your turn") 150 20)
        (q/text (str "Waiting for opponent to move") 150 20)))
    :waiting-for-opponent
    (q/text "Waiting for opponent to connect" (/ SCREEN_SIZE_WIDTH 4) 20)
    :red-wins
    (q/text "Red wins!" (/ SCREEN_SIZE_WIDTH 2) 20)
    :black-wins
    (q/text "Black wins!" (/ SCREEN_SIZE_WIDTH 2) 20)
    :tied
    (q/text "Game tied." (/ SCREEN_SIZE_WIDTH 2) 20)
    nil #_(q/text (name (:game-state state)) (/ SCREEN_SIZE_WIDTH 2) 20))

  (q/text (str "Time elapsed: " (let [total-seconds (:total-seconds state)
                                      minutes (int (/ total-seconds 60))
                                      seconds (int (mod total-seconds 60))]
                                  (format "%02d:%02d" minutes seconds)))
          10 (- SCREEN_SIZE_HEIGHT 25))

  (when-not (zero? (:timer state))
    (q/text (str "Turn timer: " (- TURN_TIMER_LIMIT (:last-turn-seconds state)))
            (/ SCREEN_SIZE_WIDTH 2) (- SCREEN_SIZE_HEIGHT 25)))


  (when (:hosting? state)
    (q/text-size 15)
    (q/text (str "Hosting:" your-ip) (/ SCREEN_SIZE_WIDTH 1.6) (- SCREEN_SIZE_HEIGHT 10)))

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
  (q/stroke (get-quil-color :black))
  (q/fill (get-quil-color :black))

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

(defn draw-button! [text x y width height selected?]
  (q/fill
    (if selected?
      (get-quil-color :yellow)
      (get-quil-color :white)))
  (q/rect x y width height 5)
  (q/fill (get-quil-color :black))
  (q/text text (+ 5 x) (+ y 22)))

(def ^:private get-button-specs
  (memoize
    (fn []
      (loop [[text & r] ["Resign" "Draw" "Pause" "Quit"]
             x BOARD_SIZE
             specs []]
        (if text
          (let [margin 5
                y (- BOARD_SIZE 40)
                width (+ (* 2 margin) (q/text-width text))
                height (+ (* 2 margin) 20)]
            (recur r (+ x (q/text-width text) 20)
                   (conj specs {:text text :x x :y y :width width :height height})))
          specs)))))

(defn ^:private within-button? [mx my x y width height]
  (and (> (+ x width) mx x) (< y my (+ y height))))

(defn selected-button [{mx :x my :y}]
  (loop [[{:keys [text x y width height]} & r] (get-button-specs)]
    (when text
      (if (within-button? mx my x y width height)
        (keyword (string/lower-case text))
        (recur r)))))

(defn draw-game-buttons! [state]
  (q/text-size 16)
  (loop [[{:keys [text x y width height]} & r] (get-button-specs)]
    (when text
      (let [selected? (within-button? (q/mouse-x) (q/mouse-y) x y width height)]
        (draw-button! text x y width height selected?)
        (recur r)))))

(defn draw-menu-item [x y text selected?]
  (q/fill (get-quil-color :black))
  (when selected?
    (q/fill (get-quil-color :red)))
  (q/text text x y))

(defn menu [{:keys [background-img current-menu] :as state}]
  (q/image background-img 0 0 SCREEN_SIZE_WIDTH SCREEN_SIZE_HEIGHT) ;;todo rotate background or something cool
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
    (game-text!)
    (draw-game-buttons!)))
