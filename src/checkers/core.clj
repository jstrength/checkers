(ns checkers.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [checkers.draw :as draw]
            [checkers.logic :as logic]
            [checkers.utils :refer :all]
            [checkers.navigation :as nav]
            [checkers.server :as server])
  (:import (java.util Date)))

(def starting-state
  (merge default-game-state default-settings-state))

(defn on-close [e]
  (server/close-connection!)
  (println e))

(defn setup [args]
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 30)
  (q/text-size 30)
  (q/smooth)

  ;(q/text-font (q/create-font "AllStarResort.ttf" 50 true))
  (q/text-font (q/create-font "AndaleMono" 50))

  ; setup function returns initial state. It contains
  ; circle color and position.
  ;todo move into it's own resource-state instead?
  (assoc starting-state
    :current-menu nav/main-menu
    :background-img (q/load-image "background.jpg")))

(defn mouse-pressed [{:keys [game-state turn board jumping-piece] :as state} {:keys [x y] :as event}]
  (if (and (= game-state :in-progress)
           (> (- SCREEN_SIZE MARGIN) x MARGIN)
           (> (- SCREEN_SIZE MARGIN) y MARGIN))
    (let [square (+ (quot (- x MARGIN) SQUARE_WIDTH)
                    (* 8 (quot (- y MARGIN) SQUARE_WIDTH)))]
      ;(println (is-playable-piece? state square))
      ;(println square)
      (if (and (if jumping-piece (= square jumping-piece) true)
               (= turn (if (#{BLACK BLACK_KING} (get board square)) :black :red))
               (logic/is-playable-piece? state square))
        (-> state
            ;(assoc-in [:board square] EMPTY)
            (assoc :selected {:piece (get board square) :square square :x x :y y}))
        state))
    state))

(defn mouse-released [{:keys [board selected] :as state} event]
  (if-let [{:keys [x y piece square]} selected]
    (let [release-square (logic/calc-square-at x y)]
      (if (and (= (get board release-square) EMPTY)
               (let [{x :raw-x y :raw-y} (position-coordinates release-square)]
                 (if (odd? y) (even? x) (odd? x)))
               (seq (filter #(= release-square %) (or (logic/get-valid-jumps state square)
                                                      (logic/get-valid-moves board square))))
               (< MARGIN x (+ BOARD_WIDTH MARGIN))
               (< MARGIN y (+ BOARD_WIDTH MARGIN)))
        ;; dropped piece is on valid square within bounds of board
        (-> state
            (update :actions conj [:move
                                   {:release-square release-square
                                    :square square :piece piece}]))
        ;; reset piece to original square
        (-> state
            ;(assoc-in [:board square] piece)
            (dissoc :selected))))
    state))

(defn mouse-dragged [{:keys [selected] :as state} {:keys [x y] :as event}]
  (if selected
    (update state :selected assoc :x x :y y)
    state))

(defn key-pressed [state event]
  ;(println event)
  ;(server/send-move! :red 10 2)
  (cond
    (#{:paused :menu} (:game-state state))
    (nav/handle-nav state event)

    (= :in-progress (:game-state state))
    (case (:key event)
      :p (assoc state :game-state :paused
                      :current-menu nav/pause-menu)
      :q (do (when (:multiplayer state)
               (server/write-msg ::server/quit))
             (update state :actions conj [:reset-game]))
      state)

    :else
    (assoc state :current-menu nav/play-again-menu
                 :game-state :menu)))

(defn handle-actions [{:keys [actions multiplayer turn player] :as state}]
  ;(println state)
  (reduce
    (fn [s action]
      (when (and multiplayer (= turn player))
        (server/perform-action s action))
      (logic/perform-action s action))
    (dissoc state :actions)
    (distinct actions)) ;todo why need distinct?/?
  )

(defn handle-turn-timer [state]
  (if (>= (- (.getTime (Date.)) (:start-time state)) 1000)
    (cond->
      (-> state
          (update :total-seconds inc)
          (assoc :start-time (.getTime (Date.))))

      (not (zero? (:timer state)))
      (update :last-turn-seconds inc)

      (zero? (- TURN_TIMER_LIMIT (:last-turn-seconds state)))
      (update :actions conj [:turn-switch]))
    state))

(defn update-state [{:keys [game-state multiplayer actions] :as state}]
  (cond->
    state

    (= :in-progress game-state)
    handle-turn-timer

    multiplayer
    server/update-state

    (and multiplayer (not= :waiting-for-opponent game-state) (not (server/is-connected?)))
    (update :actions conj [:opponent-disconnected])

    actions
    handle-actions))

(defn draw-state [state]
  (if (#{:paused :menu} (:game-state state))
    (doto state (draw/menu))
    (doto state (draw/game))))

(defn -main [& args]
  (q/defsketch checkers-quil
    :title "Checkers"
    :size [SCREEN_SIZE SCREEN_SIZE]
    ; setup function called only once, during sketch initialization.
    :setup (partial setup args)
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :mouse-pressed mouse-pressed
    :mouse-released mouse-released
    :mouse-dragged mouse-dragged
    :key-pressed key-pressed
    :on-close on-close
    :features [:keep-on-top #_:exit-on-close]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))
